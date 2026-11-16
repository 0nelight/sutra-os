(ns ^:unit platform.agent-token-scope-test
  (:require [sutra.serve.errors :as errors]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.authority :as authority]
            [sutra.access.http.auth :as auth]
            [sutra.access.sessions :as sessions]
            [sutra.branching.http.proposals :as proposals]
            [sutra.catalog.grants :as grants]
            [sutra.store.access.scope :as ctx]
            [sutra.store.edge :as edge]
            [sutra.types.paths :as paths]))

(def ^:private token "unit-agent-token")

(defn- served
  [routes uri]
  (let [seen    (atom nil)
        handler (errors/wrap-exceptions
                 (auth/wrap-auth (fn [req] (reset! seen req) {:status 200})))]
    (with-redefs [ctx/store                (fn ([] {}) ([_] {}) ([_ auth] {:authority auth}))
                  authority/read-authority (fn [& _] (throw (ex-info "no store here" {})))
                  authority/all-markings   (fn [_ _] #{})
                  authority/usable-schema  (fn [_ _] {})
                  edge/read-edge           (fn [& _] {:exists? true :schema {}})
                  sessions/seen!           (fn [& _] nil)
                  auth/session-for         (fn [_ t]
                                             (when (= t token)
                                               {:user-id "u1" :branch "main" :username "alice"
                                                :scope (sessions/agent-scope "builder" routes)}))]
      {:status (:status (handler {:uri uri :request-method :get
                                  :headers {"authorization" (str "Bearer " token)}}))
       :req    @seen})))

(deftest the-scope-names-its-routes
  (is (= #{:projection-files :proposals}
         (sessions/agent-routes {:scope (sessions/agent-scope "builder"
                                                              [:proposals :projection-files])})))
  (testing "a scope that names no route reaches nothing"
    (is (= #{} (sessions/agent-routes {:scope "agent:builder routes:"})))
    (is (= #{} (sessions/agent-routes {:scope "agent:builder"}))))
  (testing "another session is no agent's"
    (is (= #{} (sessions/agent-routes {:scope "ri.repo.1"})))
    (is (not (sessions/agent-scoped? {:scope "app:ri.app.1 ops:x"})))))

(deftest an-agent-token-reaches-the-routes-of-its-tools-only
  (testing "a route of its tools is served, marked as the agent's"
    (let [{:keys [status req]} (served [:projection-files] "/api/projections/ontologie/files")]
      (is (= 200 status))
      (is (true? (:sutra/agent? req)))))
  (testing "any other route is refused before a handler sees it"
    (let [{:keys [status req]} (served [:projection-files] (paths/url :branches))]
      (is (= 403 status))
      (is (nil? req)))))

(deftest an-agent-token-is-never-the-admin
  (let [id    (first (keys paths/admin-only))
        gated (errors/wrap-exceptions (auth/admin-gated id (fn [_] {:status 200})))
        admin {:username @#'grants/admin-username}]
    (is (= 200 (:status (gated {:principal admin}))) "control: the admin is served")
    (is (= 403 (:status (gated {:principal admin :sutra/agent? true})))
        "the admin's agent is not")
    (is (true? (#'proposals/admin? {:principal admin})))
    (is (false? (#'proposals/admin? {:principal admin :sutra/agent? true}))
        "a proposal an agent opens never lands with the admin's override")))
