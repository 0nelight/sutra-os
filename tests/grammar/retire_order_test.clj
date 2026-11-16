(ns ^:unit grammar.retire-order-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.kernel :as kernel]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.authoring.install.findings.retire :as retire]
            [sutra.parser.project-export :as pe]))

(defn- ix [W] (live/world-index W))

(def ^:private ctl "ri.sutra.main.repository.ctl")
(defn- ot [n] (str "ri.sutra.main.object-type." (name n)))
(defn- lt [n] (str "ri.sutra.main.link-type." (name n)))
(defn- tx [n] (str "ri.sutra.main.transform." (name n)))

(def ^:private kept [:projekt :arbeitsauftrag :einsatzort :kundenauftrag :partner])
(def ^:private id-attrs [{:attribute-name :id :attribute-type :string}])

(defn- fk-link [from to]
  (let [n (keyword (str (name from) "-" (name to)))]
    {:links {(lt n) {:link-type-name n :from-object-type (ot from) :to-object-type (ot to)
                     :cardinality :many-to-one}}
     :fk    {(tx (str "link-" (name n)))
             {:transform-name (keyword (str "link-" (name n)))
              :output {:kind :link-type :link-type (lt n) :fk-attribute :fk}}}}))

(def ^:private live
  (let [links (concat (for [to [:arbeitsauftrag :einsatzort :kundenauftrag :projekt]]
                        (fk-link :auftragsbestandteil to))
                      [(fk-link :projektbeteiligung :projekt)])]
    {:repositories {ctl {:repository-name :werft}}
     :object-types (into {(ot :auftragsbestandteil) {:object-type-name :auftragsbestandteil
                                                     :status :experimental :attribute-list id-attrs}
                          (ot :projektbeteiligung)  {:object-type-name :projektbeteiligung
                                                     :status :deprecated :attribute-list id-attrs}}
                         (for [k kept] [(ot k) {:object-type-name k :attribute-list id-attrs}]))
     :link-types (into {(lt :projektbeteiligung-partner)
                        {:link-type-name :projektbeteiligung-partner
                         :from-object-type (ot :projektbeteiligung) :to-object-type (ot :partner)
                         :cardinality :many-to-one}}
                       (mapcat :links links))
     :transforms (into {(tx :shape-auftragsbestandteil)
                        {:transform-name :shape-auftragsbestandteil :repository ctl :language :pg-sql
                         :query "select 1" :primary-key :id
                         :output {:kind :object-type :object-type (ot :auftragsbestandteil)}}
                        (tx :kundenauftrag-auftragsbestandteil)
                        {:transform-name :kundenauftrag-auftragsbestandteil :repository ctl
                         :input {:kind :object-type :object-type (ot :projekt)}
                         :output {:kind :object-type :object-type (ot :auftragsbestandteil)
                                  :role :supplement :primary-key :id}}
                        (tx :shape-projektbeteiligung)
                        {:transform-name :shape-projektbeteiligung :repository ctl :language :pg-sql
                         :query "select 1" :primary-key :id
                         :output {:kind :object-type :object-type (ot :projektbeteiligung)}}
                        (tx :resolve-projektbeteiligung-partner)
                        {:transform-name :resolve-projektbeteiligung-partner :repository ctl
                         :language :python :code "x"
                         :output {:kind :link-type :link-type (lt :projektbeteiligung-partner)}}}
                       (mapcat :fk links))}))

(def ^:private ontology-tree
  {:objects (into {} (for [k kept] [k {:attributes [[:id :string]]}]))})

(def ^:private onto-opts
  {:strict? true :strict-sections (pe/carried-sections true)})

(def ^:private code-opts
  {:strict? true :strict-sections pe/repo-sections :strict-repository ctl})

(defn- rules [ps] (set (map :rule ps)))

(defn- apply-deletes [schema plan]
  (reduce (fn [s {:keys [kind entity outcome]}]
            (if (= :deleted outcome)
              (let [kk (case kind :object-type :object-types :link-type :link-types
                             :transform :transforms nil)
                    nf (case kind :object-type :object-type-name :link-type :link-type-name
                             :transform :transform-name nil)]
                (cond-> s kk (update kk #(into {} (remove (fn [[_ v]] (= entity (nf v)))) %))))
              s))
          schema plan))

(deftest the-ontology-goes-first-and-its-producers-block-nothing
  (let [plan (install/plan-schema ontology-tree live onto-opts)
        dels (mapv (juxt :kind :entity) (filter #(= :deleted (:outcome %)) plan))
        at   #(.indexOf dels %)
        ps   (install-plan/plan-problems ontology-tree live plan)
        ret  (install-plan/plan-retirements ontology-tree live onto-opts)]
    (is (empty? (filter (comp #{:retire-referenced :retire-protected :producer-removed} :rule) ps))
        (pr-str (map :message ps)))
    (doseq [l [:auftragsbestandteil-projekt :auftragsbestandteil-kundenauftrag
               :projektbeteiligung-partner]]
      (is (< -1 (at [:link-type l]) (at [:object-type (if (= l :projektbeteiligung-partner)
                                                        :projektbeteiligung
                                                        :auftragsbestandteil)]))
          (str l " goes before its type")))
    (is (<= 0 (at [:transform :link-auftragsbestandteil-projekt]))
        "the generated FK producer leaves with its link")
    (is (not (contains? (set dels) [:transform :shape-auftragsbestandteil]))
        "a source-repository producer is not the ontology's to delete")
    (is (= ["kundenauftrag-auftragsbestandteil" "resolve-projektbeteiligung-partner"
            "shape-auftragsbestandteil" "shape-projektbeteiligung"]
           (:orphaned ret)))
    (testing "the state after it is a valid schema, the orphans included"
      (let [after (apply-deletes live plan)
            why   (kernel/reasons after)]
        (is (not (contains? why :dangling-references)) (pr-str (:dangling-references why)))
        (is (not (contains? why :transform-errors)) (pr-str (:transform-errors why)))
        (is (= 4 (count (retirement/orphaned-producers after))))
        (testing "and then the code repository drops them"
          (let [cplan (install/plan-schema {} after code-opts)
                cps   (install-plan/plan-problems {} after cplan)]
            (is (= #{:shape-auftragsbestandteil :kundenauftrag-auftragsbestandteil
                     :shape-projektbeteiligung :resolve-projektbeteiligung-partner}
                   (set (keep #(when (= :deleted (:outcome %)) (:entity %)) cplan))))
            (is (empty? (filter (comp #{:producer-removed :retire-referenced} :rule) cps))
                (pr-str (map :message cps)))))))))

(deftest a-link-kept-in-the-tree-blocks-its-type
  (let [tree (assoc ontology-tree :links [[:auftragsbestandteil-projekt :auftragsbestandteil
                                           :projekt :many-to-one]])
        plan (install/plan-schema tree live onto-opts)]
    (is (some #(and (= :retire-referenced (:rule %))
                    (re-find #"link-type auftragsbestandteil-projekt" (:message %)))
              (retire/retire-problems live plan (ix live))))))

(deftest an-active-type-is-still-refused
  (let [live* (assoc-in live [:object-types (ot :auftragsbestandteil) :status] :active)
        plan  (install/plan-schema ontology-tree live* onto-opts)]
    (is (contains? (rules (retire/retire-problems live* plan (ix live*))) :retire-protected))))

(deftest code-first-names-the-order
  (let [plan (install/plan-schema {} live code-opts)
        ps   (install-plan/plan-problems {} live plan)]
    (is (some #(and (= :producer-removed (:rule %))
                    (re-find #"ontology FIRST" (:message %)))
              ps))))

(deftest a-build-skips-an-orphan-and-says-so
  (let [schema {:object-types {}
                :transforms {:shape-auftragsbestandteil
                             {:transform-name :shape-auftragsbestandteil :language :pg-sql
                              :query "select 1" :primary-key :id
                              :output-columns [{:attribute-name :id :attribute-type :string}]
                              :output {:kind :object-type :object-type (ot :auftragsbestandteil)}}}}
        nodes  (atom nil)
        out    (with-out-str (reset! nodes
                                     (vec
                                      (@(requiring-resolve
                                         'sutra.builds.plan.producers/producer-nodes)
                                       schema (constantly true) (constantly true)))))]
    (is (empty? @nodes))
    (is (re-find #"shape-auftragsbestandteil übersprungen: verwaist" out))))

(deftest users-block-a-type
  (let [app   (assoc-in live [:applications "ri.sutra.main.application.x"]
                        {:application-name :auftrag-cockpit :object-type (ot :auftragsbestandteil)})
        act   (assoc-in live [:actions :bestandteil-anlegen]
                        {:name :bestandteil-anlegen
                         :parameters {:ab {:type :object :object-type (ot :auftragsbestandteil)}}})
        refd? (fn [l tree]
                (contains? (rules (retire/retire-problems
                                   l (install/plan-schema (merge ontology-tree tree) l onto-opts)
                                   (ix l)))
                           :retire-referenced))]
    (is (refd? app {:applications {:auftrag-cockpit {:object-type :auftragsbestandteil}}})
        "an app kept in the tree uses the type")
    (is (refd? act {:actions {:bestandteil-anlegen {:name :bestandteil-anlegen}}})
        "an action parameter uses the type")))

(deftest a-rollup-over-a-going-link-blocks-by-name
  (let [live* (update-in live [:object-types (ot :projekt) :attribute-list] conj
                         {:attribute-name :bestandteile :attribute-type :integer
                          :opts {:derived {:expression [:connected-count
                                                        :auftragsbestandteil-projekt]}}})
        plan  (install/plan-schema ontology-tree live* onto-opts)
        ps    (filter #(re-find #"derived projekt.bestandteile" (:message %))
                      (retire/retire-problems live* plan (ix live*)))]
    (is (= #{[:link-type :auftragsbestandteil-projekt] [:object-type :auftragsbestandteil]}
           (set (map (juxt :entity-class :entity) ps)))
        "named on the link it rolls over AND on the type the link leaves with")
    (is (every? #(= ["derived projekt.bestandteile"] (:referrers %)) ps))))

(deftest a-template-search-around-is-judged-at-install
  (let [tpl  (fn [sides] {:templates {:bericht {:parameters
                                                {:ka {:type :object :object-type :kundenauftrag}
                                                 :x  {:type :object-set :object-type :projekt
                                                      :of :ka :search-around sides}}}}})
        ps   (fn [schema] (filter #(= :template-search-around (:rule %))
                                  (install-plan/plan-problems schema live [])))]
    (is (empty? (ps (tpl [:auftragsbestandteil-kundenauftrag :auftragsbestandteil-projekt]))))
    (is (re-find #"template bericht — .*ends at auftragsbestandteil"
                 (:message (first (ps (tpl [:auftragsbestandteil-kundenauftrag]))))))))

(deftest deleting-a-produced-dataset-names-the-counterpart-order
  (let [ds   "ri.sutra.main.dataset.journal_raw"
        live {:datasets   {ds {:dataset-name :journal_raw}}
              :transforms {(tx :journal-sync)
                           {:transform-name :journal-sync
                            :input  {:kind :datasource :datasource :amos}
                            :output {:kind :dataset :dataset ds}}}}
        ps   (retire/retire-problems
              live [{:kind :dataset :entity :journal_raw :outcome :deleted}]
              (ix live))]
    (is (some #(and (= :retire-referenced (:rule %))
                    (re-find #"journal-sync produces it: remove the producer FIRST"
                             (:message %)))
              ps)
        (pr-str (map :message ps)))))
