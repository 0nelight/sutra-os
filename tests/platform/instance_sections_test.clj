(ns ^:integration platform.instance-sections-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.branch-policy :as policy]
            [sutra.store.world :as world]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.types.entities :as ents]
            [sutra.governance.merge :as bm]
            [sutra.actions.apply :as action-apply]
            [sutra.authoring.install.world :as install]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private model {:in 1 :out 2})

(defn- with-models!
  [store & ids]
  (world/install! store (assoc (world/authored store) :llm-models (zipmap ids (repeat model)))))

(deftest the-instance-sections-are-data
  (is (= #{:sources :llm-models :organizations :spaces :projects :roles :markings}
         ents/instance-sections))
  (is (= {:transforms #{} :datasets #{}}
         (select-keys (ents/instance-keys {}) [:transforms :datasets]))
      "of the branched sections, only the entries a rule names")
  (is (not (ents/instance-section? :object-types)) "the ontology stays on the branch"))

(deftest where-a-branch-reads-what-the-instance-holds
  (is (nil? (branch-kinds/instance-home "main" "main")) "main holds it")
  (is (nil? (branch-kinds/instance-home "main" "staging/u1/main"))
      "a staging batch over main is main's own write: it may create a project with its types")
  (is (= "main" (branch-kinds/instance-home "main" "feature-x")))
  (is (= "main" (branch-kinds/instance-home "main" "staging/u1/feature-x"))
      "a batch over a feature branch reads it as the feature branch does"))

(deftest a-branch-reads-what-the-instance-holds-from-main
  (let [store (ctx/system-store "main")
        b  (str "inst-" (random-uuid))]
    (with-models! store "m1")
    (world/fork! store "main" b {:kind :feature})
    (with-models! store "m1" "m2")
    (testing "registered on main after the fork: the branch sees it at once"
      (is (= #{"m1" "m2"} (set (keys (:llm-models (world/authored (world/at store b))))))))
    (testing "a branch commit does not change it — it is written as main holds it"
      (with-models! (world/at store b) "m1" "only-here")
      (is (= #{"m1" "m2"} (set (keys (:llm-models (world/authored (world/at store b)))))))
      (is (= #{"m1" "m2"} (set (keys (:llm-models (world/authored store)))))))
    (testing "a merge preview does not list it as the branch's change"
      (with-models! store "m1" "m2" "m3")
      (is (not-any? #(= "llm-models" (:section %))
                    (:diff (bm/preview-branch-schema-merge store b "main")))))
    (testing "a branch install that changes it is refused, naming the section"
      (let [e (try (install/install-on-branch! (world/at store b)
                                               (assoc-in (world/authored (world/at store b))
                                                         [:llm-models "mx"] model))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :instance-scoped (:reason (ex-data e))))
        (is (= 422 (anomaly/status (ex-data e))))))))

(def ^:private roh-rid "ri.sutra.main.dataset.00000000-0000-0000-0000-0000000000a1")

(def ^:private sync-rid "ri.sutra.main.transform.00000000-0000-0000-0000-0000000000b1")

(def ^:private sync-t
  {:id sync-rid :transform-name :roh-sync :input {:kind :datasource :datasource :erp}
   :output {:kind :dataset :dataset roh-rid}})
(def ^:private compute-t
  {:id "ri.sutra.main.transform.00000000-0000-0000-0000-0000000000b2"
   :transform-name :sauber :language :pg-sql :input {:kind :dataset :dataset roh-rid}})

(deftest a-sync-and-what-it-writes-belong-to-the-instance
  (testing "as data: a transform reading a source, and the dataset it writes"
    (let [ks
            (ents/instance-keys
             {:transforms {sync-rid sync-t
                           "ri.sutra.main.transform.00000000-0000-0000-0000-0000000000b2" compute-t}
              :datasets   {roh-rid {:dataset-name :roh}
                           "ri.sutra.main.dataset.00000000-0000-0000-0000-0000000000a2"
                             {:dataset-name :aus}}})]
      (is (= #{sync-rid} (:transforms ks)))
      (is (= #{roh-rid} (:datasets ks)))))
  (testing "authored: a dataset carrying :external-source is a sync"
    (is (ents/authored-instance? :datasets {:external-source {:datasource :erp}}))
    (is (not (ents/authored-instance? :datasets {:kind :standalone}))))
  (let [store (ctx/system-store "main")
        b  (str "sync-" (random-uuid))
        with! (fn [store ts]
                (world/install! store (-> (world/authored store)
                                       (update :transforms merge ts)
                                       (update :datasets merge
                                               {roh-rid {:id roh-rid :dataset-name :roh
                                                         :kind :standalone}}))))]
    (with! store {sync-rid sync-t})
    (world/fork! store "main" b {:kind :feature})
    (testing "a branch changes the transforms it owns, never the sync"
      (with! (world/at store b)
             {sync-rid (assoc sync-t :primary-key [:x])
              "ri.sutra.main.transform.00000000-0000-0000-0000-0000000000b2" compute-t})
      (let [w (world/authored (world/at store b))]
        (is (= sync-t (get-in w [:transforms sync-rid])) "the sync stays as main holds it")
        (is (= compute-t
               (get-in w
                       [:transforms
                        "ri.sutra.main.transform.00000000-0000-0000-0000-0000000000b2"]))
            "the branch's own transform stays")))
    (testing "a sync changed on main is the branch's at once"
      (with! store {sync-rid (assoc sync-t :primary-key [:id])})
      (is (= [:id]
             (get-in (world/authored (world/at store b)) [:transforms sync-rid :primary-key]))))))

(defn- refused
  [branch home schema action args]
  (try (action-apply/refuse-instance-action! {:branch branch
                                              :instance-home (constantly home)}
                                             schema action [args])
       nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest an-instance-action-is-refused-off-its-home
  (testing "a whole instance section on a branch"
    (let [d (refused "feature-x" "main" {} :create-project {:name :netzwerk})]
      (is (= :instance-scoped (:reason d)))
      (is (= 422 (anomaly/status d)))
      (is (= :projects (:section d)))))
  (testing "on the home itself nothing is refused — there the write is the one that belongs"
    (is (nil? (refused "main" nil {} :create-project {:name :netzwerk})))
    (is (nil? (refused "main" nil {} :update-source {:name :glpi}))
        "a protected home still takes it from an unrestricted authority — instance-block"))
  (testing "a branched section stays the branch's to carry"
    (is (nil? (refused "feature-x" "main" {} :create-object-type {:name :auftrag})))
    (is (nil? (refused "feature-x" "main" {} :update-object-type {:name :auftrag}))))
  (testing "and the entry-level rule, which only the live world knows"
    (let [schema {:transforms {sync-rid sync-t} :datasets {roh-rid {:dataset-name :roh}}}]
      (is (= :transforms (:section (refused "feature-x" "main" schema
                                            :update-transform {:name :roh-sync})))
          "a transform reading a source is a sync, whatever its section says")
      (is (nil? (refused "feature-x" "main" schema :update-transform {:name :sauber}))
          "a computing transform is the branch's"))))

(deftest a-protected-main-takes-instance-changes-from-the-admin-only
  (is (nil? (policy/instance-block "main" true)))
  (is (= 403 (anomaly/status (policy/instance-block "main" false)))))
