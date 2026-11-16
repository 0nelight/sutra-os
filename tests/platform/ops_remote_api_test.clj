(ns ^:api platform.ops-remote-api-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [org.httpkit.client :as http]
            [support.pod :as pod]
            [sutra.foundation.ops :as op]
            [sutra.store.access.scope :as ctx]))

(def ^:private secret "test-operations-secret")

(defn- all-services [] (into #{} (map (comp :service val)) (op/entries)))

(def ^:private served
  {:profile :split :self :pod
   :processes {:pod {:services :all} :here {:services #{}}}})

(defn- with-served-pod [f]
  (let [topology op/*topology*]
    (op/set-peer-secret! secret)
    (alter-var-root #'op/*topology* (constantly served))
    (try (f)
         (finally (alter-var-root #'op/*topology* (constantly topology))))))

(use-fixtures :each pod/with-mem-pod-auth with-served-pod)

(defn- elsewhere
  [svc]
  {:profile :split
   :self    :here
   :processes {:here {:services (disj (all-services) svc)}
               :pod  {:services #{svc} :url (pod/base-url)}}})

(deftest a-query-answers-the-same-across-the-process-boundary
  (doseq [[o svc] [[:automation/list :automation] [:health/by-node :health]]]
    (testing o
      (let [store (ctx/as-system (ctx/store))
            here  (op/query {:store store} o {:branch "main"})
            there (binding [op/*topology* (elsewhere svc)]
                    (is (map? (op/placement op/*topology* o)) "placed in the pod")
                    (op/query {:store store} o {:branch "main"}))]
        (is (= here there))))))

(defn- post [token body]
  @(http/post (str (pod/base-url) "/api/ops")
              (cond-> {:as :text
                       :headers {"Content-Type" "application/edn"}
                       :body (pr-str body)}
                token (assoc-in [:headers "Authorization"] (str "Bearer " token)))))

(def ^:private call {:op :automation/list :kind :query :in {:branch "main"}
                     :branch "main" :authority nil :schema? false})

(defn- signed [payload] (op/sign secret (System/currentTimeMillis) payload))

(defn- refused? [{:keys [status body]} re]
  (and (= 400 status) (re-find re (str (:error (edn/read-string body))))))

(deftest only-the-operations-token-is-answered
  (is (= 401 (:status (post nil (signed call)))))
  (is (= 401 (:status (post "wrong" (signed call)))))
  (is (= 200 (:status (post (op/peer-token) (signed call))))))

(deftest only-a-signed-fresh-unaltered-call-is-answered
  (let [token (op/peer-token)]
    (testing "unsigned"
      (is (refused? (post token call) #"unsigned")))
    (testing "altered: another authority under the same signature"
      (let [env (signed call)]
        (is (refused? (post token (assoc env :payload (pr-str (assoc call :authority
                                                                     {:user "admin"}))))
                      #"bad signature"))))
    (testing "signed with another secret"
      (is (refused? (post token (op/sign "other" (System/currentTimeMillis) call))
                    #"bad signature")))
    (testing "stale"
      (is (refused? (post token (op/sign secret (- (System/currentTimeMillis)
                                                    (* 2 op/signature-window-ms))
                                         call))
                    #"time window")))
    (testing "sent twice"
      (let [env (signed call)]
        (is (= 200 (:status (post token env))))
        (is (refused? (post token env) #"sent twice"))))
    (testing "an operation this process does not know"
      (is (= 400 (:status (post token (signed (assoc call :op :nope/nothing)))))))))

(deftest a-single-process-answers-nothing
  (alter-var-root #'op/*topology* (constantly op/solo))
  (is (= 404 (:status (post (op/peer-token) (signed call))))))
