(ns ^:api platform.instance-push-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.protection :as protection]
            [sutra.governance.protection :as gov-protection]
            [sutra.catalog.grants :as grants]
            [sutra.store.world :as world]
            [sutra.authoring.instance-change :as instance]
            [sutra.actions.landing :as landing]
            [sutra.authoring.stage :as stage]
            [sutra.authoring.projections :as projections]
            [sutra.store.seat :as seat]
            [sutra.actions.staging :as stg]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private schema
  {:schema {:projects {:buero {:description "alt"}}
            :sources  {:erp {:project :buero :type :postgres
                             :jdbc-url "jdbc:postgresql://db:5432/erp" :egress ["db:5432"]}}
            :datasets {:roh {:project :buero :kind :standalone
                             :columns [{:attribute-name :id :attribute-type :string}]
                             :external-source {:datasource :erp :table "ROH" :primary-key :id}}}
            :objects  {:auftrag {:primary-key :auf-nr :attributes [[:auf-nr :string]]
                                 :match-keys [:auf-nr]
                                 :project :buero
                                 :plural "Aufträge"}}}})

(defn- c
  [store b]
  {:store (world/at store b) :schema (seat/branch-schema (world/at store b)) :branch b
   :edge (protection/guard-reads store)})

(defn- edited
  [store b d od & [table]]
  (let [p  (projections/pull (c store b) "ontologie" nil false)
        fs (update-keys (:files p) name)
        ed (fn [fs f]
             (into {} (for [[k t] fs] [k (pr-str (update (edn/read-string t) :schema f))])))]
    (assoc p :files (ed fs (fn [s]
                             (cond-> s
                               (get-in s
                                       [:projects
                                        :buero]) (assoc-in [:projects :buero :description] d)
                               (get-in s
                                       [:objects
                                        :auftrag]) (assoc-in [:objects :auftrag :description] od)
                               (and table (get-in s [:datasets :roh :external-source]))
                               (assoc-in [:datasets :roh :external-source :table] table)))))))

(defn- sync-table [store b]
  (some #(when (= :roh-sync (:transform-name %)) (get-in % [:source :table] (:table %)))
        (vals (:transforms (world/authored (world/at store b))))))

(defn- project-description [store b]
  (some #(when (= :buero (:project-name %)) (:description %))
        (vals (:projects (world/authored (world/at store b))))))

(deftest a-push-from-a-branch-lands-the-instance-part-on-main
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [admin (ctx/system-store "main")
        b     (str "feat-" (random-uuid))]
    (world/fork! admin "main" b {:kind :feature})
    (testing "the admin's push: the project change lands on main, the object change stages on the branch"
      (let [r (projections/push! (c admin b) "ontologie" (edited admin b "neu" "auf dem Zweig"))]
        (is (get-in r [:instance :landed]) (pr-str (:instance r)))
        (is (= "neu" (project-description admin "main")))
        (is (= "neu" (project-description admin b)) "the branch reads it from main")
        (is (seq (stg/list-staged-enriched (world/at admin b)))
            "the object change is staged on the branch")
        (is (not-any? #(= "project" (str (:type %)))
                      (stg/list-staged-enriched (world/at admin b))))))
    (testing "a sync is configuration, not branched: its change lands on main"
      (let [before (sync-table admin "main")
            r      (projections/push! (c admin b) "ontologie"
                                      (edited admin b "neu" "auf dem Zweig" "ROH2"))]
        (is (some? before) "the sync exists on main")
        (is (get-in r [:instance :landed]) (pr-str (:instance r)))
        (is (= "ROH2" (sync-table admin "main")))
        (is (= "ROH2" (sync-table admin b)) "the branch reads it from main")))
    (testing "on a protected main, anyone else's push becomes a request the admin decides"
      (gov-protection/set-policy! admin "main" {:protected true})
      (try
        (let [user (ctx/store b {:user "u1" :username "u1" :operations grants/all
                                :project-operations {}})
              r    (projections/push! (c user b) "ontologie" (edited admin b "vom benutzer" "x"))
              req  (get-in r [:instance :requested])]
          (is (= :instance-change (:kind req)) (pr-str (:instance r)))
          (is (= "neu" (project-description admin "main")) "nothing lands before the decision")
          (instance/apply-request! (ctx/system-store "main")
                                   (protection/guard-reads (ctx/system-store "main")) req)
          (is (= "vom benutzer" (project-description admin "main")) "approved, it lands"))
        (finally (gov-protection/set-policy! admin "main" {:protected false}))))))

(def ^:private auftrag
  {:primary-key :auf-nr :attributes [[:auf-nr :string]] :match-keys [:auf-nr]
   :project :buero})

(deftest a-branched-row-on-main-is-named-and-nothing-is-left-behind
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [admin (ctx/system-store "main")
        b     (str "feat-" (random-uuid))
        d     (str "neu-" (random-uuid))]
    (world/fork! admin "main" b {:kind :feature})
    (gov-protection/set-policy! admin "main" {:protected true})
    (try
      (stage/stage! (c admin "main")
                  {:schema
                     {:objects {:auftrag (assoc auftrag :description "auf main liegengeblieben")}}}
                  nil)
      (is (= 1 (count (instance/in-the-way admin (protection/guard-reads admin) "main")))
          "a branched row stages on main — only its commit is refused there — and it
           stands in every instance landing's way")
      (testing "the push says what is in the way, not 'open a proposal'"
        (let [why (get-in (projections/push! (c admin b) "ontologie" (edited admin b d "x"))
                          [:instance :land-refused])]
          (is (some? why))
          (is (str/includes? why "changeset drop") "it names the way off main")
          (is (not (str/includes? why "proposal from another branch"))
              "a proposal cannot carry what the instance holds")))
      (is (= 1 (count (stg/list-staged-enriched (world/at admin "main"))))
          "and the refused landing staged nothing of its own beside it")
      (is (not= d (project-description admin "main")) "nothing landed")
      (finally
        (let [s (world/at admin "main")] (stg/clear-staged! s (stg/staging-view s)))
        (gov-protection/set-policy! admin "main" {:protected false})))))

(deftest a-refused-landing-takes-back-what-it-staged
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [admin (ctx/system-store "main")
        part  {:schema {:projects {:buero {:description "will nicht landen"}}} :opts {}}]
    (with-redefs [landing/land (fn [_ _ _] {:land-refused "nope" :land-status 403})]
      (is (:land-refused (instance/land! admin (protection/guard-reads admin) "main" part false))))
    (is (empty? (stg/list-staged-enriched (world/at admin "main")))
        "a row left here would decide the next landing's guard with it")))
