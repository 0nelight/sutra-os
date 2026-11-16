(ns ^:unit platform.governance-test
  (:require [sutra.access.authority :as authority]
            [sutra.store.access.scope :as ctx]
            [clojure.test :refer [deftest testing is]]
            [sutra.access.http.auth :as auth]
            [sutra.store.edge :as edge]
            [sutra.catalog.grants :as grants]
            [sutra.store.pg.select :as select]))

(deftest fail-closed-values-are-actually-closed
  (testing "in-memory cleared? path"
    (is (false? (grants/cleared? nil #{:secret})) "nil names no clearance: holds nothing")
    (is (true?  (grants/cleared? grants/all #{:secret})) "all is said, never left out")
    (is (false? (grants/cleared? #{} #{:secret})) "NEW [#{} …]: holds nothing (CLOSED)")
    (is (true?  (grants/cleared? #{} #{}))        "unmarked resources stay public"))
  (testing "SQL marking-where path"
    (is (= "TRUE" (:sql (select/marking-where nil)))
        "OLD nil denied: no WHERE filter (OPEN)")
    (is (not= "TRUE" (:sql (select/marking-where #{:secret})))
        "NEW registry denied: rows filtered (CLOSED)")))

(deftest principal-context-computes-mac-rbac
  (testing "admin is the unrestricted superuser"
    (with-redefs [ctx/store (constantly {})
                  authority/read-authority (fn [& _] (throw (ex-info "should not be called" {})))
                  authority/usable-schema (fn [_ _] {})]
      (let [ctx (auth/principal-context {:user-id "a" :branch "main"
                                         :username @#'grants/admin-username})]
        (is (= "admin" (:role ctx)))
        (is (= grants/all (:clears ctx)) "cleared for every marking, said explicitly")
        (is (= #{} (:denied ctx)) "no read filter")
        (is (= grants/all (:ops ctx)) "superuser: every operation, said explicitly")
        (is (= grants/all (:proj-ops ctx)) "and in every project"))))
  (testing "a non-admin whose clearance calc throws is denied everything"
    (with-redefs [ctx/store (constantly {})
                  authority/read-authority (fn [& _] (throw (ex-info "boom" {})))
                  authority/usable-schema (fn [_ _] {})
                  authority/all-markings  (fn [_ _] #{:secret})]
      (let [ctx (auth/principal-context {:user-id "u" :branch "main" :username "alice"})]
        (is (= #{} (:clears ctx)))
        (is (= #{:secret} (:denied ctx)))))))

(deftest wrap-auth-fails-closed-when-clearance-calc-throws
  (let [registry #{:secret :topsecret}
        seen     (atom nil)
        token    "unit-failclosed-token"
        handler  (auth/wrap-auth
                  (fn [req]
                    (reset! seen {:clears (grants/clearances (:authority (:sutra/store req)))
                                  :denied (grants/denied-markings (:authority (:sutra/store req)))})
                    {:status 200}))]
    (with-redefs [ctx/store (fn ([] {}) ([_] {}) ([_ auth] {:authority auth}))
                  authority/read-authority (fn [& _] (throw (ex-info "clearance boom" {})))
                  authority/all-markings  (fn [_ _] registry)
                  authority/usable-schema (fn [_ _] {})
                  edge/read-edge          (fn [& _] {:exists? true :schema {}})
                  auth/session-for      (fn [_ t]
                                          (when (= t token)
                                            {:user-id "u1" :branch "main" :username "alice"}))]
      (handler {:uri     "/api/anything"
                :headers {"authorization" (str "Bearer " token)}})
      (is (= #{} (:clears @seen))
          "clearances closed to the empty set (holds nothing)")
      (is (= registry (:denied @seen))
          "denied-markings = the whole registry (every marked row hidden)"))))
