(ns ^:unit platform.token-scope-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.apps.http.custom :as apps]
            [sutra.access.http.host :as host]
            [sutra.access.oauth :as oauth]
            [sutra.access.throttle :as throttle]
            [sutra.kit.system :as sys]
            [support.mem :as mem]))

(defn- store
  []
  (assoc (mem/store) ::sys/system (sys/configure (sys/assemble [throttle/module]) {})))

(def ^:private app-rid "ri.sutra.main.custom-application.test")

(def ^:private schema
  {:custom-applications
   {app-rid {:custom-application-name :ssh-config-sync
             :status :published
             :permissions {:roles [:admin]}
             :client {:grant-types [:client-credentials] :token-ttl 900}
             :scope {:object-types [:zugang]
                     :actions      [:zugang-erfassen :zugang-aendern]
                     :operations   [:actions :ontology :schema]}}}
   :object-types {"ri.ot.zugang" {:object-type-name :zugang :plural "Zugänge"}}
   :action-types {:zugang-erfassen {} :zugang-aendern {}}})

(defn- token!
  [store scope]
  (let [c (oauth/register-client! store {:app app-rid :app-name :ssh-config-sync
                                      :grant-types [:client-credentials]
                                      :created-by "admin"})]
    (apps/oauth-token {:body-params (cond-> {:grant_type "client_credentials"
                                             :client_id (:client-id c)
                                             :client_secret (:client-secret c)}
                                      scope (assoc :scope scope))
                       :sutra/store store
                       :sutra/schema schema
                       :sutra/branch "main"
                       :sutra/host host/capabilities})))

(defn- admits?
  [uri granted]
  (nil? (apps/guard schema app-rid {:uri uri}
                    {:username "admin" :role "admin" :granted-operations granted})))

(deftest a-request-without-a-scope-gets-the-whole-declaration
  (let [r (token! (store) nil)]
    (is (= 200 (:status r)))
    (is (= "actions ontology schema" (get-in r [:body :scope]))
        "no scope asked for means no restriction beyond the declaration")))

(deftest a-request-for-less-gets-exactly-that
  (let [r (token! (store) "actions")]
    (is (= 200 (:status r)))
    (is (= "actions" (get-in r [:body :scope])))))

(deftest a-request-for-more-than-the-declaration-is-refused
  (let [store (store)
        r  (token! store "actions admin")]
    (is (= 400 (:status r)))
    (is (= "invalid_scope" (get-in r [:body :error])))
    (is (re-find #"admin" (get-in r [:body :detail]))
        "the refusal names the namespace that is not declared")
    (is (empty? (filter oauth/app-scoped? (vals (:sessions (:db store)))))
        "and no session was minted")))

(deftest the-guard-enforces-the-token-not-just-the-declaration
  (testing "a namespace the application declares and the token asked for"
    (is (admits? "/api/actions" #{:actions})))
  (testing "a namespace the application declares but THIS token did not ask for"
    (is (not (admits? "/api/ontology/query" #{:actions}))
        "the application may read object sets; this token may not"))
  (testing "a namespace outside the declaration is refused whatever the token says"
    (is (not (admits? "/api/admin/users" #{:actions :admin}))))
  (testing "the token endpoint itself is always reachable"
    (is (admits? "/api/oauth/token" #{:actions}))))

(deftest the-principal-allowlist-reads-a-keyword-and-a-string-alike
  (let [ask (fn [role] (apps/guard schema app-rid {:uri "/api/actions"}
                                   {:username "admin" :role role
                                    :granted-operations #{:actions}}))]
    (is (nil? (ask "admin")) "the store hands out a string")
    (is (nil? (ask :admin)) "a keyword names the same role, it does not name nothing")
    (is (some? (ask "gast")) "and a role that is not listed is still refused")))

(deftest a-scope-without-the-marker-grants-nothing
  (testing "there is no spelling that means \"everything\" — absence is not a wide grant"
    (is (= #{} (oauth/granted-operations {:scope (str "app:" app-rid)})))
    (is (not (admits? "/api/ontology/query" #{})))
    (is (not (admits? "/api/actions" #{}))))
  (testing "so a truncated or never-written scope closes the door instead of opening it"
    (is (= #{} (oauth/granted-operations {:scope (str "app:" app-rid " ops:")})))
    (is (= #{} (oauth/granted-operations {:scope nil}))))
  (testing "and the token endpoint stays reachable, or a client could never recover"
    (is (admits? "/api/oauth/token" #{}))))

(deftest the-whole-declaration-is-written-out-not-implied
  (let [r (token! (store) nil)]
    (is (= 200 (:status r)))
    (is (= #{:actions :ontology :schema}
           (oauth/granted-operations {:scope (str "app:" app-rid " ops:actions,ontology,schema")}))
        "what the token got is readable from the scope itself, not from what is missing")))

(def ^:private cid-rid "ri.sutra.main.custom-application.cid")

(def ^:private cid-schema
  {:custom-applications
   {cid-rid {:custom-application-name :telefon-cid
             :status :published
             :scope {:functions [:cid-name] :operations [:function]}}}
   :functions {"ri.fn.cid" {:function-name :cid-name}
               "ri.fn.andere" {:function-name :andere}}})

(defn- cid-call-admitted? [body]
  (nil? (apps/guard cid-schema cid-rid {:uri "/api/function" :body-params body}
                    {:username "svc" :role "viewer" :granted-operations #{:function}})))

(deftest a-function-token-runs-only-its-declared-functions
  (is (cid-call-admitted? {:function-name :cid-name :inputs {:nummer "43"}}))
  (is (cid-call-admitted? {:function-name "cid-name"}) "the wire may carry a string")
  (is (not (cid-call-admitted? {:function-name :andere})) "a stored function outside the scope")
  (is (not (cid-call-admitted? {:language :clojure :source "(evil)"}))
      "code sent along is never in a declaration")
  (is (not (cid-call-admitted? {})) "no name names nothing in the declaration"))
