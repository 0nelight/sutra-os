(ns ^:api platform.endpoint-authority-test
  (:require [sutra.serve.errors :as errors]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.serve.server :as server]
            [sutra.access.http.auth :as auth]
            [sutra.types.paths :as paths]
            [sutra.http.responses :as responses]))

(def ^:private non-admin {:principal {:username "definitely-not-the-admin"}})
(defn- bound-handlers
  []
  (for [id     (keys paths/admin-only)
        [m h]  (get (server/handler-bindings) id)]
    [id m h]))

(defn- invoke [h req]
  (try ((errors/wrap-exceptions h) req)
       (catch Throwable _ ::passed-gate)))

(deftest every-declared-endpoint-is-actually-bound
  (doseq [id (keys paths/admin-only)]
    (is (contains? (server/handler-bindings) id)
        (str id " is declared admin-only but no handler is bound to it — the"
             " declaration is inert, and a typo'd id looks exactly like this")))
  (is (seq (bound-handlers)) "the probe must actually exercise something"))

(deftest a-non-admin-is-refused-by-every-admin-endpoint
  (doseq [[id m h] (bound-handlers)]
    (testing (str id " " m)
      (let [r (invoke h non-admin)]
        (is (= 403 (:status r))
            "a declared admin-only endpoint must refuse a non-admin principal")
        (is (str/starts-with? (get-in r [:body :error] "") "admin only — ")
            "and say so in the shared wording")
        (is (str/includes? (get-in r [:body :error] "")
                           (get paths/admin-only id))
            "naming the reason from the table cell that made the decision")))))

(deftest gating-an-undeclared-endpoint-throws-at-wrap-time
  (is (thrown? clojure.lang.ExceptionInfo
               (auth/admin-gated :no-such-endpoint (fn [_] (responses/ok))))))

(defn- with-requirement [] (for [[id binds] (server/handler-bindings)
                                 method (keys binds)
                                 :let [rq (paths/requirement id method)]
                                 :when (:op rq)]
                             [id method rq]))

(def ^:private passed {:status 200 :body :passed})

(defn- ask-as [id method rq auth]
  (let [secs    (fn [[s _]] (cond (keyword? s) [s] (map? s) [:datasets] :else s))
        targets (remove nil? [(:on rq) (:also rq)])
        schema  (into {} (for [t targets s (secs t)]
                           [s {"rid-x" {:project "werft"}}]))
        params  (into {} (for [[_ p] targets]
                           [p "rid-x"]))
        params  (cond-> params
                  (map? (first (:on rq))) (assoc (:param (first (:on rq))) "datasets"))
        h       (auth/gated id method (fn [_] passed))]
    (try (h {:sutra/auth auth :sutra/schema schema :path-params params :query-params {}})
         (catch clojure.lang.ExceptionInfo e
           (when-not (:sutra/acl-denied (ex-data e)) (throw e))
           {:status 403}))))

(deftest every-declared-operation-holds-for-the-right-principal
  (is (> (count (with-requirement)) 100) "the walk must cover the table")
  (doseq [[id method {:keys [op on] :as rq}] (with-requirement)
          :let [held {"werft" #{op}}
                none {:operations #{} :project-operations {"anderes" #{op}}}
                role {:operations #{op} :project-operations {}}
                here {:operations #{} :project-operations held}]]
    (testing (str id " " method " " rq)
      (is (= 403 (:status (ask-as id method rq none))) "whoever holds nothing is refused")
      (is (= :passed (:body (ask-as id method rq role))) "the global role passes")
      (when on
        (is (= :passed (:body (ask-as id method rq here)))
            "the operation on the target's project passes")
        (is (= 403 (:status (ask-as id method rq
                                    (assoc here :blocked-resources #{"rid-x"}))))
            "a marking still refuses")))))

(deftest a-method-that-declares-nothing-stops-the-server
  (is (thrown? clojure.lang.ExceptionInfo
               (auth/gate-bindings {:schema {:patch (fn [_] passed)}}))))

(deftest the-column-is-data-and-covers-every-route
  (let [routes (for [[id binds] (server/handler-bindings) method (keys binds)] [id method])
        kind   (fn [[id method]]
                 (let [{:keys [auth]} (paths/endpoints id)
                       {:keys [op member custom]} (paths/requirement id method)]
                   (cond (paths/declared-auth auth) auth
                         op :op member :member custom :custom)))]
    (is (= (count routes) (reduce + (vals (frequencies (keep kind routes)))))
        "each route has exactly one kind of requirement")
    (is (>= (count (server/handler-bindings)) 390))
    (is (>= (count routes) 459))
    (println "[requires]" (count (server/handler-bindings)) "endpoints" (count routes) "routes"
             (frequencies (map kind routes))))
  (doseq [[id r] paths/endpoints
          [_ {:keys [requires]}] (:methods r)]
    (is (= requires (edn/read-string (pr-str requires)))
        (str id " must stay plain data, ready for resources/sutra/ops.edn"))))

(deftest no-requirement-leaves-a-reason-empty
  (doseq [[id binds] (server/handler-bindings)
          method (keys binds)
          :when (not (paths/declared-auth (:auth (paths/endpoints id))))
          :let [{:keys [op member custom]} (paths/requirement id method)]]
    (is (or op (seq member) (seq custom)) (str id " " method))))
