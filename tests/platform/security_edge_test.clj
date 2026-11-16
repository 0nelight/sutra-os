(ns ^:unit platform.security-edge-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.feedback.places :as places]
            [sutra.http.security-log :as seclog]
            [sutra.kit.redact :as redact]
            [sutra.serve.platform :as platform]
            [sutra.serve.errors :as errors]
            [sutra.store.control :as ctl]))

(defn- err-of [f]
  (let [w (java.io.StringWriter.)]
    [(binding [*err* w] (f)) (str w)]))

(defn- security-lines [err]
  (->> (str/split-lines err)
       (filter #(str/starts-with? % "[security] "))
       (mapv #(json/read-str (subs % 11) :key-fn keyword))))

(deftest secrets-are-redacted-at-any-depth
  (let [d {:spec    {:host "db" :password "hunter2" :auth {:type :bearer :token "t0k"}
                     :url "jdbc:postgresql://sa:hunter2@db:5432/x"}
           :secret-cipher "abc" "clientSecret" "s" :headers {"Authorization" "Bearer abc"}
           :input-tokens 12 :rows [{:apiKey "k"}] :note "token=xyz; next"}
        r (redact/redact d)]
    (is (= "[redacted]" (get-in r [:spec :password]) (get-in r [:spec :auth :token])
           (:secret-cipher r) (get r "clientSecret") (get-in r [:headers "Authorization"])
           (get-in r [:rows 0 :apiKey])))
    (is (= :bearer (get-in r [:spec :auth :type])) "a map under :auth is walked, not dropped")
    (is (= "jdbc:postgresql://[redacted]@db:5432/x" (get-in r [:spec :url])))
    (is (= 12 (:input-tokens r)) "a count of tokens is no token")
    (is (= "token=[redacted]; next" (:note r)))
    (is (not (str/includes? (pr-str r) "hunter2")))))

(deftest log-values-stay-on-one-line
  (is (= "a\\nuser=admin\\r\\tx\\u0001" (seclog/encode "a\nuser=admin\r\tx\u0001")))
  (let [[_ err] (err-of #(seclog/security-event!
                          :login/failure
                          {:request-method :post :uri "/api/login" :remote-addr "127.0.0.1"
                           :headers {"x-forwarded-for" "10.0.0.1"}}
                          :failure {:username "eve\n[security] forged" :password "pw"}))
        lines (str/split-lines err)
        [e]   (security-lines err)]
    (is (= 1 (count lines)))
    (is (= "login/failure" (:event e)))
    (is (= "10.0.0.1" (:ip e)))
    (is (= "/api/login" (:route e)))
    (is (= "eve\n[security] forged" (:username e)) "escaped on the wire, intact when read")
    (is (= "[redacted]" (:password e)))))

(def ^:private json-req {:request-method :post :uri "/api/x"
                         :headers {"accept" "application/json"}})

(defn- answer [handler req]
  (err-of #((#'errors/wrap-exceptions handler) req)))

(deftest a-fault-names-only-its-incident
  (let [[res err] (answer (fn [_] (throw (ex-info "kaputt bei jdbc:postgresql://sa:hunter2@db"
                                                  {:spec {:password "hunter2"}
                                                   :schema {:store :internal}})))
                          json-req)
        body (json/read-str (:body res) :key-fn keyword)]
    (is (= 500 (:status res)))
    (is (= #{:error :incident} (set (keys body))))
    (is (str/includes? (:error body) (:incident body)))
    (is (str/includes? err (str "[500] " (:incident body))) "the log finds it by its id")
    (is (str/includes? err ":internal") "the log keeps the data")
    (is (not (str/includes? err "hunter2")) "but none of its secrets")))

(deftest a-refusal-keeps-its-data-without-secrets
  (let [[res _] (answer (fn [_] (throw (ex-info "nicht erlaubt"
                                                {:sutra.kit.anomaly/category :unprocessable
                                                 :field :x
                                                 :spec {:password "hunter2" :host "h"}})))
                        {:request-method :post :uri "/api/x"})]
    (is (= 422 (:status res)))
    (is (= :x (get-in res [:body :data :field])))
    (is (= "h" (get-in res [:body :data :spec :host])))
    (is (= "[redacted]" (get-in res [:body :data :spec :password])))))

(defn- refusing [_]
  (throw (ex-info "abgelehnt" {:sutra.kit.anomaly/category :unprocessable :action :a})))

(deftest a-feedback-header-alone-shows-no-origin
  (let [[res _] (answer refusing (assoc-in json-req [:headers "x-sutra-feedback"] "1"))]
    (is (= 422 (:status res)))
    (is (nil? (get-in res [:headers "X-Sutra-Herkunft"])))))

(deftest only-the-owner-of-the-session-is-answered
  (with-redefs [ctl/get (fn [_ t {:keys [id]}]
                          (when (and (= :begleitungen t) (= "s1" id)) {:id id :benutzer "anna"}))]
    (let [frage (fn [user id]
                  (places/admitted? (cond-> {:sutra/store ::store
                                                 :headers {"x-sutra-feedback" id}}
                                          user (assoc :principal {:username user}))))]
      (is (true? (frage "anna" "s1")))
      (is (false? (frage "bob" "s1")) "a session of another user")
      (is (false? (frage nil "s1")) "no one signed in")
      (is (false? (frage "anna" "s2")) "no such session"))))

(defn- routed [req id]
  (assoc req :sutra/endpoint id))

(deftest denials-and-admin-changes-are-security-events
  (testing "a 403 is logged with its reason"
    (let [[_ err] (answer (fn [_] {:status 403 :body {:error "admin only"}})
                          (routed {:request-method :post :uri "/api/protections"
                                   :remote-addr "10.1.1.1"}
                                  :protections))
          [e] (security-lines err)]
      (is (= "authz/write-denied" (:event e)))
      (is (= "denied" (:outcome e)))
      (is (= "admin only" (:error e)))
      (is (= "10.1.1.1" (:ip e)))))
  (testing "a write to an admin endpoint is logged"
    (let [[_ err] (answer (fn [_] {:status 200 :body {:ok true}})
                          (routed {:request-method :post :uri "/api/egress/policies/decide"}
                                  :egress-policy-decide))
          [e] (security-lines err)]
      (is (= "admin/change" (:event e)))
      (is (= "egress-policy-decide" (:endpoint e)))
      (is (= "success" (:outcome e)))))
  (testing "an install over roles names what it touched"
    (let [[_ err] (answer (fn [_] {:status 200 :body {:ok true}})
                          (routed {:request-method :post :uri "/api/install"
                                   :body-params {:schema {:roles {:editor {}}
                                                          :object-types {:a {}}}}}
                                  :install))
          [e] (security-lines err)]
      (is (= "admin/schema-change" (:event e)))
      (is (= {:roles ["editor"]} (:sections e)))))
  (testing "a read is no event"
    (let [[_ err] (answer (fn [_] {:status 200 :body {}})
                          (routed {:request-method :get :uri "/api/protections"}
                                  :protections))]
      (is (empty? (security-lines err))))))

(deftest a-client-log-entry-cannot-forge-a-line
  (let [f (java.io.File/createTempFile "client" ".log")]
    (try
      (with-redefs-fn {#'platform/client-log-path (str f)}
        (fn []
          (let [res (platform/client-log-handler
                     {:body-params {:entries (into [{:level "error"
                                                     :message "x\n2026 [error] user=admin"}]
                                                   (repeat 80 {:message "m"}))}})]
            (is (= 50 (get-in res [:body :logged])))
            (is (= 31 (get-in res [:body :dropped]))))))
      (let [lines (str/split-lines (slurp f))]
        (is (= 50 (count lines)))
        (is (str/includes? (first lines) "x\\n2026 [error] user=admin")))
      (finally (io/delete-file f true)))))
