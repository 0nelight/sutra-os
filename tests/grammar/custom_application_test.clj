(ns ^:unit grammar.custom-application-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.authoring.install :as install]
            [sutra.apps.http.custom :as cust]
            [sutra.parser.project-emit :as pemit]
            [platform-sdk.scope :as scope]))

(def ^:private app
  {:custom-application-name :befund-erfassung
   :display-name "Befund-Erfassung"
   :description "Tablet-App der Linienwartung."
   :status :published
   :sdk {:namespace "befunde"}
   :client {:grant-types [:authorization-code :client-credentials]
            :redirect-uris ["https://befunde.example/cb"]
            :token-ttl 900}
   :scope {:object-types [:befund :befundbild]
           :actions [:befund-anlegen]
           :operations [:objects :actions]}
   :permissions {:roles [:admin]}
   :hosting {:subdomain "befunde" :root "dist"}
   :metrics true})

(def ^:private schema
  {:object-types
   {"ri.o.1" {:object-type-name :befund :match-keys [:nr]
              :attribute-list [{:attribute-name :nr :attribute-type :string}
                               {:attribute-name :kosten :attribute-type :decimal}]
              :plural "Befunde"}
    "ri.o.2" {:object-type-name :befundbild :match-keys [:bild-id]
              :attribute-list [{:attribute-name :bild-id :attribute-type :string}]
              :plural "Befundbilder"}
    "ri.o.3" {:object-type-name :personalakte :match-keys [:id]
              :attribute-list [{:attribute-name :id :attribute-type :string}]
              :plural "Personalakten"}}
   :link-types
   {"ri.l.1" {:link-type-name :bild-befund :from-object-type "ri.o.2"
              :to-object-type "ri.o.1" :cardinality :many-to-one}
    "ri.l.2" {:link-type-name :befund-personal :from-object-type "ri.o.1"
              :to-object-type "ri.o.3" :cardinality :many-to-one}}
   :actions {:befund-anlegen {:name :befund-anlegen
                               :params {:bez {:type :string :required true}}}
             :lizenz-entziehen   {:name :lizenz-entziehen :params {}}}
   :functions {}
   :custom-applications {"ri.ca.1" app}})

(deftest the-declaration-round-trips
  (let [{:keys [ent]} (pemit/export schema {:all-projects? true :inline? true})
        out (get-in ent [:schema :custom-applications :befund-erfassung])]
    (is (some? out) "export must emit the section, or authored intent is lost")
    (testing "every authored part survives"
      (is (= "Befund-Erfassung" (:display-name out)))
      (is (= :published (:status out)))
      (is (= {:namespace "befunde"} (:sdk out)))
      (is (= 900 (get-in out [:client :token-ttl])))
      (is (= [:befund :befundbild] (get-in out [:scope :object-types])))
      (is (= {:subdomain "befunde" :root "dist"} (:hosting out)))
      (is (true? (:metrics out))))))

(deftest narrowing-keeps-only-what-is-declared
  (let [n (scope/narrow schema app)]
    (is (= #{:befund :befundbild}
           (set (map :object-type-name (vals (:object-types n))))))
    (is (= #{:befund-anlegen} (set (keys (:actions n)))))
    (testing "a link is kept only when BOTH endpoints are in scope"
      (is (= #{:bild-befund}
             (set (map :link-type-name (vals (:link-types n)))))
          "befund-personal points at a type outside the scope and must not appear —
           an SDK traversal that always 403s is worse than its absence"))))

(deftest the-generated-sdk-carries-the-scope-and-nothing-else
  (let [{:keys [source ns fingerprint]} (scope/generate-sdk schema app {:ontology "main"})]
    (is (= "befunde" ns))
    (is (string? fingerprint))
    (is (str/includes? source "(defn befund\n"))
    (is (str/includes? source "(defn befund-anlegen!"))
    (is (not (str/includes? source "personalakte"))
        "a type outside the scope must not be nameable from the SDK")
    (is (not (str/includes? source "lizenz-entziehen")))))

(deftest the-docs-describe-exactly-the-sdk
  (let [md (scope/docs schema app {:ontology "main"})]
    (is (str/includes? md "# Befund-Erfassung — SDK reference"))
    (is (str/includes? md "### `befund`"))
    (is (str/includes? md "`kosten`"))
    (is (str/includes? md "### `befund-anlegen`"))
    (is (not (str/includes? md "personalakte"))
        "docs render from the narrowed schema, so they cannot describe what the SDK lacks")))

(deftest the-guard-and-the-sdk-agree
  (testing "in scope"
    (is (scope/permits? schema app :object-type :befund))
    (is (scope/permits? schema app :action :befund-anlegen))
    (is (scope/permits? schema app :operation :objects)))
  (testing "out of scope"
    (is (not (scope/permits? schema app :object-type :personalakte)))
    (is (not (scope/permits? schema app :action :lizenz-entziehen)))
    (is (not (scope/permits? schema app :operation :admin))
        "an undeclared API namespace is refused"))
  (testing "the two agree on every object type in the schema"
    (let [in-sdk (set (map (comp keyword name :object-type-name)
                           (vals (:object-types (scope/narrow schema app)))))]
      (doseq [ot (map (comp keyword name :object-type-name) (vals (:object-types schema)))]
        (is (= (contains? in-sdk ot) (scope/permits? schema app :object-type ot))
            (str "SDK and guard disagree about " ot
                 " — that is the drift this design exists to prevent"))))))

(deftest a-revoked-scope-narrows-to-nothing
  (let [empty-app (assoc app :scope {})
        n (scope/narrow schema empty-app)]
    (is (empty? (:object-types n)))
    (is (empty? (:actions n)))
    (is (empty? (:link-types n))
        "no endpoints in scope means no links, not all links")))

(deftest api-namespace-is-the-path
  (testing "so a declaration reads the
            way the URL does"
    (is (= :objects (cust/api-namespace "/api/objects/befund")))
    (is (= :actions (cust/api-namespace "/api/actions")))
    (is (= :admin   (cust/api-namespace "/api/admin/code-reload")))
    (is (nil? (cust/api-namespace "/hosted/befunde/index.html")))))

(def ^:private admin {:username "admin" :role "admin"
                      :granted-operations #{:objects :actions}})

(defn- guard [uri & [more]]
  (cust/guard schema "ri.ca.1" (merge {:uri uri} more) admin))

(deftest the-guard-refuses-an-undeclared-namespace
  (is (nil? (guard "/api/objects/befund" {:path-params {:type "befund"}}))
      "objects is declared and befund is in scope")
  (let [r (guard "/api/admin/code-reload")]
    (is (= 403 (anomaly/status r)))
    (is (str/includes? (anomaly/message r) ":operations"))))

(deftest the-guard-refuses-an-out-of-scope-object-type
  (let [r (guard "/api/objects/personalakte" {:path-params {:type "personalakte"}})]
    (is (= 403 (anomaly/status r)))
    (is (str/includes? (anomaly/message r) "personalakte"))))

(deftest a-query-function-of-a-code-repository-is-in-the-sdk
  (let [schema* (assoc-in schema [:transforms "ri.t.1"]
                          {:function-name :befund-suche :inputs {:suche {:kind :scalar}}})
        app*    {:scope {:functions [:befund-suche]}}]
    (is (= ["ri.t.1"] (keys (:transforms (scope/narrow schema* app*)))))
    (is (empty? (:transforms (scope/narrow schema* {:scope {}}))))))

(deftest the-guard-reads-the-types-of-a-query-from-its-body
  (let [admin* (update admin :granted-operations conj :ontology)
        query  (fn [body]
                 (cust/guard (assoc-in schema
                                       [:custom-applications "ri.ca.1" :scope :operations]
                                       [:objects :actions :ontology])
                             "ri.ca.1"
                             {:uri "/api/ontology/query" :body-params body}
                             admin*))]
    (is (nil? (query {:find :befund})) "befund is in scope")
    (is (= 403 (anomaly/status (query {:find :personalakte}))))))

(deftest the-guard-refuses-an-out-of-scope-action
  (is (nil? (guard "/api/actions" {:body-params {:action :befund-anlegen}})))
  (let [r (guard "/api/actions" {:body-params {:action :lizenz-entziehen}})]
    (is (= 403 (anomaly/status r)))
    (is (str/includes? (anomaly/message r) "lizenz-entziehen"))))

(deftest a-draft-application-reaches-nothing
  (doseq [status [:draft nil]]
    (let [sc (assoc-in schema [:custom-applications "ri.ca.1" :status] status)
          r  (cust/guard sc "ri.ca.1" {:uri "/api/objects/befund"} admin)]
      (is (= 403 (anomaly/status r)))
      (is (str/includes? (anomaly/message r) ":published")))))

(deftest an-undeclared-application-reaches-nothing
  (let [r (cust/guard {:custom-applications {}} "ri.ca.1" {:uri "/api/objects/befund"} admin)]
    (is (= 403 (anomaly/status r)))
    (is (str/includes? (anomaly/message r) "no longer declared")
        "a token outlives its declaration — that window must fail closed")))

(deftest an-application-can-always-describe-itself
  (is (nil? (guard "/api/custom-apps/befund-erfassung"))
      "refusing this would make a token unable to read its own scope")
  (is (nil? (guard "/api/oauth/token"))))

(deftest hosting-needs-a-repository
  (let [r (cust/hosting-refusal (dissoc app :repository))]
    (is (= 503 (:status r)))
    (is (str/includes? (:body r) ":repository"))))

(deftest hosting-refuses-a-blank-root
  (testing "a blank :root would publish the whole repository — sources included"
    (doseq [root [nil "" "   "]]
      (let [r (cust/hosting-refusal (-> app (assoc :repository "ri.r.1")
                                        (assoc-in [:hosting :root] root)))]
        (is (= 503 (:status r)) (str "root " (pr-str root) " must be refused"))
        (is (str/includes? (:body r) ":root"))))))

(deftest hosting-serves-a-named-build-directory
  (is (nil? (cust/hosting-refusal (assoc app :repository "ri.r.1")))
      "declared :repository plus :root \"dist\" is the servable case"))

(deftest permissions-admit-a-declared-role
  (is (cust/permits-principal? app {:username "edda" :role "admin"})))

(deftest permissions-refuse-an-undeclared-principal
  (is (not (cust/permits-principal? app {:username "edda" :role "consumer"})))
  (let [r (cust/guard schema "ri.ca.1" {:uri "/api/objects/befund"}
                      {:username "edda" :role "consumer"})]
    (is (= 403 (anomaly/status r)))
    (is (str/includes? (anomaly/message r) ":permissions"))))

(deftest permissions-admit-a-named-user-whatever-the-role
  (let [a (assoc app :permissions {:users [:edda]})]
    (is (cust/permits-principal? a {:username "edda" :role "consumer"}))
    (is (not (cust/permits-principal? a {:username "milo" :role "consumer"})))))

(deftest absent-permissions-narrow-nothing
  (testing "an empty declaration admits everyone the PLATFORM already admits —
            it is a narrowing, not a gate you must remember to open"
    (doseq [p [nil {} {:users [] :roles []}]]
      (is (cust/permits-principal? (assoc app :permissions p)
                                   {:username "wer-auch-immer" :role "consumer"})))))

(deftest the-install-refuses-an-operation-that-names-no-api-namespace
  (let [bad {:custom-applications
             {:x {:custom-application-name :x :scope {:operations [:objekte]}}}}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"name no API namespace"
         (install/plan-schema bad {} {}))
        "otherwise the declaration looks right and every call 403s")))

(deftest the-install-accepts-real-api-namespaces
  (let [good {:custom-applications
              {:x {:custom-application-name :x
                   :scope {:operations [:objects :object-types :actions :function]}}}}]
    (is (= 1 (count (install/plan-schema good {} {}))))))
