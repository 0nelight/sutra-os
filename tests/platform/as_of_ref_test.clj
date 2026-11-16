(ns ^:unit platform.as-of-ref-test
  (:require [sutra.serve.errors :as errors]
            [sutra.catalog.lookup :as lookup]
            [clojure.test :refer [deftest is testing]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.store.world :as world]
            [sutra.http.responses :as responses]
            [sutra.objects.http :as oss]
            [sutra.authoring.http.oms :as oms]))

(def ^:private prod-token "73464")
(def ^:private commit-hash "abcdefghijklmnopqrstuvwxyz012345")
(def ^:private type-rid "ri.sutra.main.object-type.71b7029b-e045-4623-8c2c-d14b7fd7e0a4")

(deftest a-store-fault-is-not-a-404
  (testing "a client's malformed ref is 400, carrying the store's own message"
    (let [e (ex-info "as-of '73464' is a bulk-store version token"
                     {:as-of prod-token :sutra.kit.anomaly/category :incorrect})]
      (is (= 400 (:status (responses/fault e {:commit prod-token}))))))

  (testing "a declared absence stays 404"
    (let [e (ex-info "nothing here" {:sutra.kit.anomaly/category :not-found})]
      (is (= 404 (:status (responses/fault e {}))))))

  (testing "a database fault is NOT swallowed as 404"
    (let [e (java.sql.SQLException.
             "ERROR: syntax error at or near \"'73464'\"\n  Position: 62")]
      (is (thrown? java.sql.SQLException (responses/fault e {:commit prod-token}))))))

(deftest handlers-do-not-report-a-store-fault-as-not-found
  (with-redefs [ctx/store                  (constantly {})
                lookup/->type-rid            (fn [_ _ _] type-rid)]
    (testing "GET /api/at/:commit/objects/:type — a SQL fault propagates"
      (with-redefs [sub/read-type
                    (fn [& _]
                      (throw (java.sql.SQLException.
                              "ERROR: syntax error at or near \"'73464'\"\n  Position: 62")))]
        (is (thrown? java.sql.SQLException
                     (oss/objects-at-commit
                      {:path-params {:commit prod-token :type "buchungssatz"}
                       :sutra/store (ctx/store)
                       :sutra/schema {}})))))

    (testing "GET /api/at/:commit/objects/:type — a bad ref is 400, not 404"
      (with-redefs [sub/read-type
                    (fn [& _]
                      (throw (ex-info "as-of '73464' is a bulk-store version token"
                                      {:as-of prod-token :sutra.kit.anomaly/category :incorrect})))]
        (let [{:keys [status body]}
              ((errors/wrap-exceptions oss/objects-at-commit)
               {:path-params {:commit prod-token :type "buchungssatz"} :sutra/store (ctx/store)
                :sutra/schema {}})]
          (is (= 400 status))
          (is (not (contains? body :hint))))))

    (testing "an unknown object type is still 404"
      (with-redefs [lookup/->type-rid (fn [_ _ _] nil)]
        (is (= 404 (:status ((errors/wrap-exceptions oss/objects-at-commit)
                             {:path-params {:commit commit-hash :type "nope"}
                              :sutra/store (ctx/store)
                              :sutra/schema {}}))))))

    (testing "GET /api/at/:commit/schema/object-type/:type — a SQL fault propagates"
      (with-redefs [world/as-of
                    (fn [& _]
                      (throw (java.sql.SQLException.
                              "ERROR: syntax error at or near \"'73464'\"\n  Position: 50")))]
        (is (thrown? java.sql.SQLException
                     (oms/object-type-spec-at-commit
                      {:path-params {:commit prod-token :type "buchungssatz"}
                       :sutra/store (ctx/store)
                       :sutra/schema {}})))))))
