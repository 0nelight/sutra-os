(ns ^:unit platform.asvs-authz-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.access.http.auth :as auth]
            [sutra.builds.http :as builds-http]
            [sutra.catalog.project-scope :as scope]
            [sutra.http.origin :as origin]
            [sutra.kit.rate-limit :as rate]
            [sutra.types.paths :as paths]))

(def ^:private hour {:capacity 3 :per-ms 3600000})

(deftest a-bucket-gives-its-capacity-then-waits-for-the-refill
  (let [step (fn [s now] (rate/spend s hour "u" now 1))
        r1   (step {} 0)
        r2   (step (:state r1) 0)
        r3   (step (:state r2) 0)
        r4   (step (:state r3) 0)]
    (is (every? :allowed? [r1 r2 r3]))
    (is (false? (:allowed? r4)))
    (is (= 1200000 (:retry-ms r4)) "one unit comes back after a third of the hour")
    (is (:allowed? (step (:state r3) 1200000)))
    (testing "another key has its own bucket"
      (is (:allowed? (rate/spend (:state r3) hour "v" 0 1))))))

(deftest a-charge-goes-below-zero-and-the-next-spend-waits-for-the-debt
  (let [s (rate/charge {} hour "u" 0 6)
        r (rate/spend s hour "u" 0 1)]
    (is (false? (:allowed? r)))
    (is (= (* 4 1200000) (:retry-ms r)))))

(deftest refilled-buckets-are-dropped-past-max-keys
  (let [policy (assoc hour :max-keys 2)
        s      (reduce #(:state (rate/spend %1 policy %2 0 1)) {} ["a" "b" "c"])]
    (is (= 3 (count s)) "nothing has refilled yet")
    (is (= 1 (count (:state (rate/spend s policy "d" 3600000 1)))))))

(deftest the-limiter-answers-nil-or-the-wait
  (let [lim (rate/limiter {:capacity 1 :per-ms 60000} #(System/currentTimeMillis))]
    (is (nil? (rate/spend! lim "u")))
    (is (pos? (rate/spend! lim "u")))
    (rate/reset! lim)
    (is (nil? (rate/spend! lim "u")))))

(deftest a-websocket-opens-from-the-instance-and-the-allowlist-only
  (let [allowed (origin/allowlist "https://desk.example.org, http://localhost:8280")]
    (is (origin/allowed? allowed "https://sutra.example.org" "sutra.example.org"))
    (is (origin/allowed? allowed "https://sutra.example.org" "sutra.example.org:443"))
    (is (origin/allowed? allowed "http://localhost:8080" "localhost:8080"))
    (is (origin/allowed? allowed "https://desk.example.org" "sutra.example.org"))
    (is (origin/allowed? allowed "http://localhost:8280" "localhost:8080"))
    (is (origin/allowed? allowed nil "sutra.example.org") "no Origin: no browser page")
    (is (not (origin/allowed? allowed "https://evil.example" "sutra.example.org")))
    (is (not (origin/allowed? allowed "https://sutra.example.org.evil" "sutra.example.org")))
    (is (not (origin/allowed? allowed "http://localhost:9999" "localhost:8080")))
    (is (not (origin/allowed? allowed "null" "sutra.example.org")))))

(deftest the-handshake-is-refused-before-the-upgrade
  (let [h (origin/wrap-ws-origin (fn [_] {:status 101}))]
    (is (= 403 (:status (h {:websocket? true
                            :headers {"origin" "https://evil.example" "host" "a.example"}}))))
    (is (= 101 (:status (h {:websocket? true
                            :headers {"origin" "https://a.example" "host" "a.example"}}))))
    (is (= 101 (:status (h {:headers {"origin" "https://evil.example" "host" "a.example"}})))
        "a plain request is not judged here")))

(defn- publish-as [auth]
  (let [h (auth/gated :stream-records :post (fn [_] {:status 200}))]
    (try (:status (h {:sutra/auth auth
                      :sutra/schema {:datasets {"rid-s" {:dataset-name :anruf :project "werft"}}}
                      :path-params {:name "anruf"} :query-params {}}))
         (catch clojure.lang.ExceptionInfo e
           (if (:sutra/acl-denied (ex-data e)) 403 (throw e))))))

(deftest publishing-to-a-stream-needs-update-on-its-project
  (is (= {:op :update :on [:datasets :name]} (paths/requirement :stream-records :post)))
  (is (= 403 (publish-as {:operations #{:read} :project-operations {"werft" #{:read}}})))
  (is (= 403 (publish-as {:operations #{} :project-operations {"anderes" #{:update}}})))
  (is (= 200 (publish-as {:operations #{} :project-operations {"werft" #{:read :update}}}))))

(deftest roles-and-markings-need-the-operation-that-governs-them
  (let [editor {:operations #{:read :create :update} :project-operations {}}
        owner  {:operations #{:read :create :update :manage-permissions :manage-markings}
                :project-operations {}}
        change (fn [auth section]
                 (try (scope/require-schema-change! auth [{:status "modified" :section section
                                                           :name "x"}])
                      :ok
                      (catch clojure.lang.ExceptionInfo e
                        (:operation (ex-data e)))))]
    (is (= :manage-permissions (change editor "roles")))
    (is (= :manage-markings (change editor "markings")))
    (is (= :ok (change owner "roles")))
    (is (= :ok (change owner "markings")))
    (is (= :ok (change editor "object-types")) "other sections keep their rule")))

(deftest a-run-shows-a-stranger-only-what-their-projects-allow
  (let [schema {:transforms {"t-a" {:transform-name :a :project "werft"}
                             "t-b" {:transform-name :b :project "geheim"}}}
        viewer {:user "u2" :operations #{} :project-operations {"werft" #{:read}}}
        run    {:actor "u1" :targets ["a" "b"] :error-message "geheim kaputt"
                :inputs {:x 1} :progress {:done ["b"]}}
        mine   (builds-http/run-for {:user "u1" :operations #{}} schema run)
        seen   (builds-http/run-for viewer schema run)]
    (is (= run mine) "its starter sees all of it")
    (is (= ["a"] (:targets seen)))
    (is (not-any? #(contains? seen %) [:inputs :error-message :progress]))
    (is (= ["t-a"] (map :transform_rid (builds-http/visible-jobs
                                        viewer schema [{:transform_rid "t-a"}
                                                       {:transform_rid "t-b"}]))))))
