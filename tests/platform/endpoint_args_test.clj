(ns ^:unit platform.endpoint-args-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.grants :as grants]
            [sutra.http.endpoint :as ep]
            [sutra.kit.anomaly :as anomaly]
            [sutra.types.paths :as paths]))

(defn- req [store] {:sutra/store store :sutra/schema {} :query-params {} :path-params {}})

(defn- refusal-of [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (anomaly/of e))))

(def public-id (first (paths/ids-with-auth :public)))

(deftest user-is-resolved-once
  (testing "the signed-in user"
    (is (= "anna" (:user (ep/args :health-checks {} (req {:authority {:username "anna"}}))))))
  (testing "no sign-in set up: the admin"
    (is (= grants/admin-username (:user (ep/args :health-checks {} (req {})))))
    (is (some? public-id))
    (testing "but never on a public endpoint"
      (is (nil? (:user (ep/args public-id {} (req {})))))))
  (testing "the system authority acts for nobody"
    (is (nil? (:user (ep/args :health-checks {} (req {:authority grants/system-authority})))))))

(deftest needs-user-refuses-without-one
  (let [h (ep/handler :health-checks {:needs-user true :call :user})]
    (is (= {:status 200 :body "anna"} (h (req {:authority {:username "anna"}}))))
    (let [r (refusal-of #(h (req {:authority grants/system-authority})))]
      (is (= :incorrect (anomaly/category r)))
      (is (= :refusal.http/no-user (:refusal/code (anomaly/data r)))))))

(deftest with-binds-lookups-or-refuses
  (let [h (ep/handler :health-checks
                      {:with {:thing [#(get {"a" 1} (:id %)) :refusal.schedules/no-such-schedule]}
                       :call :thing})]
    (is (= {:status 200 :body 1} (h (assoc (req {}) :path-params {:id "a"}))))
    (let [r (refusal-of #(h (assoc (req {}) :path-params {:id "b"})))]
      (is (= :not-found (anomaly/category r)))
      (is (= :refusal.schedules/no-such-schedule (:refusal/code (anomaly/data r))))
      (is (= "b" (:id (anomaly/data r)))))))

(deftest found-passes-values-and-refuses-nil
  (is (= false (ep/found false :refusal.http/no-such-blob {})))
  (is (= :not-found (anomaly/category (refusal-of #(ep/found nil :refusal.http/no-such-blob {}))))))

(deftest or-404-refuses-with-its-code
  (let [h (ep/handler :health-checks {:call (constantly nil) :or-404 :refusal.http/no-such-blob})
        r (refusal-of #(h (assoc (req {}) :path-params {:hash "h"})))]
    (is (= :refusal.http/no-such-blob (:refusal/code (anomaly/data r))))
    (is (= "h" (:hash (anomaly/data r))))))

(deftest user-accessors
  (is (= "anna" (ep/signed-in-user {:principal {:username "anna"}})))
  (is (nil? (ep/signed-in-user {:sutra/store {}})) "no fallback where it would grant")
  (is (= grants/admin-username (ep/acting-user {:sutra/store {}}))))
