(ns ^:unit grammar.install-plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.versions :as vers]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.authoring.install.findings.actions :as actions]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.migration :as migration]
            [clojure.set]
            [clojure.string :as str]
            [clojure.walk]
            [sutra.parser.sugar :as sugar]
            [sutra.types.rid :as rid]
            [sutra.catalog.kernel :as kernel]
            [sutra.ref.install :as ri]
            [sutra.parser.project-emit :as pemit]))

(def ^:private desired
  {:projects {:linienwartung {}}
   :objects {:triebwerk {:project :linienwartung :attributes [[:id :string] [:zyklen :integer]]
                         :plural "Triebwerke"}
             :fahrwerk {:project :linienwartung :attributes [[:id :string]]
                        :implements [:wartbares-teil]
                        :plural "Fahrwerke"}}
   :interfaces {:wartbares-teil {:project :linienwartung :property-list [[:id :string]]}}
   :links   [[:verbaut-mit :triebwerk :fahrwerk :one-to-many {:project :linienwartung}]]
   :datasets {:raw {:project :linienwartung :columns []}}
   :functions {:zyklen-zaehlen {:project :linienwartung :target-type :triebwerk :language :python
                                :source (str "from functions.api import function\n"
                                             "@function(edits=[])\n"
                                             "def zyklen_zaehlen():\n"
                                             "    return []\n")}}})

(defn- plan
  ([] (plan {} {}))
  ([current opts] (install/plan-schema desired current opts)))

(deftest planning-is-deterministic
  (is (= (plan) (plan)) "same inputs, same plan — no ids minted, no clock read"))

(deftest an-edit-carries-what-it-needs-and-nothing-more
  (doseq [{:keys [outcome action args] :as e} (plan)]
    (testing (pr-str (select-keys e [:phase :entity :outcome]))
      (if (= :skipped outcome)
        (is (and (nil? action) (nil? args))
            "a skip is a decision, not an instruction — it carries no action")
        (is (and (some? action) (some? args))
            "anything that is not a skip must say what to stage, and with what")))))

(deftest the-plan-follows-the-declared-staging-order
  (let [rank (into {} (map-indexed (fn [i {:keys [phase]}] [phase i]) install-plan/staging-order))
        ranks (map (comp rank :phase) (plan {} {:strict? true}))]
    (is (every? some? ranks)
        "every edit's phase must be declared in staging-order")
    (is (apply <= ranks)
        "the plan must be emitted in staging-order — this is the invariant the
         old comment-numbering could not express, let alone check")))

(deftest object-types-precede-their-implements-restaging
  (let [phases (map :phase (plan))
        first-of (fn [p] (.indexOf (vec phases) p))]
    (is (< (first-of :object-types) (first-of :interfaces)))
    (is (< (first-of :interfaces)   (first-of :link-types)))
    (is (< (first-of :link-types)   (first-of :object-implements)))
    (is (some #(= :object-implements %) phases)
        ":fahrwerk declares :implements, so the re-stage phase must appear")))

(deftest an-empty-live-schema-creates-everything
  (let [p (plan)]
    (is (every? #(= :created (:outcome %))
                (remove #(= :object-implements (:phase %)) p))
        "nothing exists yet, so every phase but the :implements re-stage creates")
    (is (= (count p) (+ (:created (install-plan/plan-tally p))
                        (:updated (install-plan/plan-tally p))
                        (:skipped (install-plan/plan-tally p)))))))

(deftest the-tally-is-derived-from-the-plan-not-counted-during-it
  (let [p (plan)
        t (install-plan/plan-tally p)]
    (is (= (count (filter #(= :created (:outcome %)) p)) (:created t)))
    (is (= (count (filter #(= :updated (:outcome %)) p)) (:updated t)))
    (is (= (count (filter #(= :skipped (:outcome %)) p)) (:skipped t)))
    (is (not (contains? t :deleted))
        ":deleted appears only when strict mode removed something — matching
         what the imperative (fnil inc 0) tally produced")))

(deftest strict-deletes-come-last-and-in-reverse-dependency-order
  (let [live {:object-types {"rid-1" {:object-type-name :orphan :plural "Orphans"}}
              :link-types   {"rid-2" {:link-type-name :orphan-link}}}
        p    (install/plan-schema desired live {:strict? true})
        dels (filter #(= :deletes (:phase %)) p)]
    (is (seq dels) "an entity live but absent from the .ent is deleted")
    (is (= (take-last (count dels) p) dels)
        "deletes are the tail of the plan")
    (is (every? #(= :deleted (:outcome %)) dels))
    (let [kinds (map :kind dels)]
      (is (< (.indexOf (vec kinds) :link-type) (.indexOf (vec kinds) :object-type))
          "a link-type is dropped before the object-type it references"))
    (is (= 2 (:deleted (install-plan/plan-tally p))))))

(deftest without-strict-nothing-is-deleted
  (let [live {:object-types {"rid-1" {:object-type-name :orphan :plural "Orphans"}}}]
    (is (empty? (filter #(= :deleted (:outcome %))
                        (install/plan-schema desired live {})))
        "deletion is opt-in — a non-strict install never removes anything")))

(deftest an-explicit-empty-match-keys-converges
  (let [desired {:objects {:triebwerk {:attributes [[:id :string]] :match-keys []
                                       :plural "Triebwerke"}}}
        live    {:object-types
                 {"rid-w" {:object-type-name :triebwerk
                           :attribute-list [{:attribute-name :id
                                             :attribute-type :string}]
                           :match-keys [:id]
                           :plural "Triebwerke"}}}
        step    (->> (install/plan-schema desired live {})
                     (filter #(= :object-types (:phase %)))
                     first)]
    (is (= :updated (:outcome step))
        "the declaration no longer names match-keys, and that is a retraction")
    (is (not (contains? (:args step) :match-keys))
        "staged ABSENT — which `merge-existing` dissocs under the whole-declaration marker")
    (testing "and the world that produces plans nothing — one step, not a loop"
      (let [after (update-in live [:object-types "rid-w"] dissoc :match-keys)]
        (is (= :skipped (->> (install/plan-schema desired after {})
                             (filter #(= :object-types (:phase %)))
                             first :outcome)))))))

(deftest what-the-kernel-would-store-is-what-the-plan-compares-against
  (doseq [[section entity authored spec-fn]
          [[:agents :befund-agent {:model "grok-4" :description ""}
            (partial ri/spec kernel/install-cx :agent)]
           [:applications :portal {:status :published :description "" :icon ""}
            (partial ri/spec kernel/install-cx :application)]]]
    (testing (name section)
      (let [stored  (spec-fn entity authored)
            live    {section {"ri.x.1" stored}}
            desired {section {entity authored}}
            outcome (:outcome (first (install/plan-schema desired live {})))]
        (is (= :skipped outcome)
            (str section "/" entity " never converges: authored " (pr-str authored)
                 " but the kernel stores " (pr-str stored)
                 " — the update stages a value the kernel drops again"))))))

(deftest an-app-publishes-version-1-on-create-and-then-only-on-auto-publish
  (let [app      {:object-type :triebwerk :layout {:cards []}}
        portal   #(first (filter (fn [e] (= :portal (:entity e)))
                                 (install/plan-schema {:applications {:portal %1}} %2 {})))
        live     {:applications
                  {"ri.a.9" (vers/publish ((partial ri/spec kernel/install-cx :application)
                                           :portal app)
                                          {})}}
        edited   (assoc app :layout {:cards [{:widget :attributes}]})]
    (testing "create: nothing live, so the first save cuts version 1 and opens the app"
      (let [e (portal app {})]
        (is (= :created (:outcome e)))
        (is (= "1" (:published (:args e))))
        (is (vers/published? (:args e)))))
    (testing "a re-install that moves nothing is no change — that would be the loop"
      (is (= :skipped (:outcome (portal app live)))))
    (testing "an edit without :auto-publish keeps the released version"
      (let [args (:args (portal edited live))]
        (is (= "1" (:published args)))
        (is (= ["1"] (vers/versions args)))))
    (testing "with :auto-publish the edit is released as version 2"
      (let [args (:args (portal (assoc edited :auto-publish true) live))]
        (is (= "2" (:published args)))
        (is (= ["1" "2"] (vers/versions args)))))))

(deftest an-actions-authored-metadata-is-stored-so-the-plan-converges
  (let [authored {:name         :freigeben
                  :display-name "Freigeben"
                  :description  "Setzt den Status auf freigegeben."
                  :visibility   :prominent
                  :status       :deprecated
                  :deprecation  {:reason "superseded by :freigabe-erteilen"
                                 :deadline "2027-01-01"
                                 :replacement :freigabe-erteilen}
                  :rules        [{:op :update :target [:get :id] :values {:x 1}}]}
        stored   ((partial ri/spec kernel/install-cx :action) nil authored)]
    (doseq [k [:display-name :description :visibility :status :deprecation]]
      (is (= (k authored) (k stored))
          (str k " authored on an action and stored nowhere")))
    (is (= :skipped
           (:outcome (first (install/plan-schema {:actions {:freigeben authored}}
                                                 {:actions {:freigeben stored}}
                                                 {}))))
        "an action whose metadata the kernel drops re-stages the same update forever")))

(def ^:private live-schema
  {:object-types
   {"ri.o.1" {:object-type-name :triebwerk
              :attribute-list [{:attribute-name :id   :attribute-type :string}
                               {:attribute-name :zyklen :attribute-type :integer}]
              :symbol "W" :plural "Triebwerke" :match-keys [:id]
              :display-name "Triebwerk" :description "Ein Antrieb." :aliases ["Antrieb"]
              :visibility :prominent}
    "ri.o.2" {:object-type-name :fahrwerk
              :attribute-list [{:attribute-name :id :attribute-type :string}]
              :implements ["ri.i.1"]
              :plural "Fahrwerke"}}
   :interfaces
   {"ri.i.1" {:interface-name :wartbares-teil
              :property-list [{:attribute-name :id :attribute-type :string}]}}
   :datasets
   {"ri.d.1" {:dataset-name :raw :kind :standalone :title "ACARS-Rohdaten"
              :columns [] :description "landing"}}
   :shared-properties
   {"ri.sp.1" {:shared-property-name :wartungskosten :type :decimal :display-name "Wartungskosten"
               :format {:kind :currency :currency "EUR"} :render-hints [:sortable]}}
   :value-types
   {"ri.vt.1" {:value-type-name :email-address :type :string
               :constraints {:format :email}}}
   :object-type-groups
   {"ri.g.1" {:object-type-group-name :core :description "Kern"
              :members ["ri.o.1"]}}
   :applications
   {"ri.a.1" {:application-name :portal :status :published :layout {:x 1}
              :display-name "Portal" :description "" :icon ""}}
   :custom-applications
   {"ri.ca.1" {:custom-application-name :field-tool :status :draft
               :display-name "Field Tool"
               :client {:grant-types [:authorization-code]
                        :redirect-uris ["http://localhost:8787/callback"]
                        :token-ttl 3600}
               :scope {:object-types ["ri.o.1"]
                       :actions ["ri.sutra.main.action.00000000-0000-0000-0000-0000000000a1"]
                       :operations [:objects :object-types]}
               :sdk {:namespace "field.sdk"}
               :permissions {:roles [:admin]}
               :metrics true}}
   :collections
   {"ri.c.1" {:collection-name :betrieb :description "Betrieb" :order 1}}
   :workspaces
   {"ri.w.1" {:workspace-name :leitung :description "Leitung"
              :landing "ri.a.1" :apps ["ri.a.1"]}}
   :link-types
   {"ri.l.1" {:link-type-name :verbaut-mit :from-object-type "ri.o.1"
              :to-object-type "ri.o.2" :cardinality :one-to-many}}
   :transforms
   {"ri.t.1" {:transform-name :roll-up :language :pg-sql
              :query "SELECT 1"
              :inputs {:w {:kind :object-type :object-type "ri.o.1"}}
              :output {:kind :dataset :dataset "ri.d.1"}}
    "ri.t.2" {:transform-name :shape-raw :language :pg-sql
              :query "SELECT 2"
              :pipeline "ri.pl.1"
              :inputs {:r {:kind :dataset :dataset "ri.d.1"}}
              :output {:kind :dataset :dataset "ri.d.1"}}
    "ri.t.3" {:transform-name :rohdaten-nach-camo-export
              :input {:kind :dataset :dataset "ri.d.1"}
              :output {:kind :export :export "ri.ex.1"}}}
   :pipelines
   {"ri.pl.1" {:id "ri.pl.1" :pipeline-name :raw-shaping :kind :batch
               :description "Kern" :imports ["ri.d.1"]}}
   :functions
   {"ri.w.1" {:function-name :zyklen-zaehlen :target-type "ri.o.1"
              :language :clojure
              :source "(defn zyklen-zaehlen [t] (-> OntologyClient/action (.update-triebwerk t {:zyklen 1})))"}}
   :templates
   {"ri.tp.1" {:template-name :monatsbericht :title "Monatsbericht"
               :format :html :engine :jinja2 :status :published
               :source "<h1>{{ monat }}</h1>"
               :parameters {:monat {:type :string :required true}}
               :page {:zyklen :a4 :orientation :portrait}
               :repository "ri.r.1"}}
   :object-sets
   {"ri.os.1" {:object-set-name :alle-triebwerke :description "Alle Triebwerke"
               :root "ri.o.1" :limit 100}}
   :agents
   {"ri.ag.1" {:agent-name :befund-agent :model "grok-4"
               :system-prompt "You file things."
               :tools [{:kind :object-query :object-type :triebwerk}]}}
   :roles
   {:auditor {:operations [:read :discover] :description "liest alles"}}
   :markings
   {:geheim {:category "vertraulichkeit" :rank 2 :cleared ["acme-staff"]}}
   :organizations
   {"ri.org.1" {:organization-name "acme" :description "Die Firma" :members ["acme-staff"]}}
   :spaces
   {"ri.space.1" {:space-name "flugbetrieb" :organization "ri.org.1"}}
   :projects
   {"ri.p.1" {:project-name "core" :description "Kernprojekt" :space "ri.space.1"}}
   :repositories
   {"ri.r.1" {:repository-name :ingest :project "ri.p.1"
              :description "ETL transforms." :type :transforms}}
   :sources
   {"ri.s.1" {:source-name :camo-db :type :postgres :jdbc-url "jdbc:postgresql://x/y"}}
   :media-sets
   {"ri.ms.1" {:media-set-name :befundfotos
               :description "Fotos zu einem Befund." :formats [:jpeg]}}
   :exports
   {"ri.ex.1" {:export-name :rohdaten-nach-camo
               :source "ri.s.1" :dataset "ri.d.1"
               :destination {:table "acars_raw"} :mode :mirror
               :description "Die Rohdaten zurück in die CAMO-Datenbank."}}
   :eval-suites
   {:rechnungskopf {:function :kopf-lesen
                    :evaluators [{:name :netto :kind :numeric-range :field :netto
                                  :tolerance 0.01}]}}
   :projections
   {:modell {:filename {:yaml {:language :yaml :name ["*.ent.yaml"]
                               :owner [:declared :facet :file-name]
                               :home-for [:*]}}
             :select   {:sections [:objects :links]}
             :edits    [:update]
             :description "Nur das Objektmodell"}}
   :actions
   {"ri.sutra.main.action.00000000-0000-0000-0000-0000000000a1"
    {:action-name :triebwerk-erfassen
     :name        :triebwerk-erfassen
     :params      {:id {:type :string}}
     :rules       [{:op :create :object-type :triebwerk :id :id :values {:id :id}}]
     :edits       [{:object-type :triebwerk}]}}})

(def ^:private export-gaps
  #{})

(defn- export-live
  []
  (pemit/export live-schema {:inline? true}))

(def ^:private installable-sections
  (let [phase->section {:object-types :objects, :link-types :links
                        :edit-functions :functions}
        step-phases    #{:dataset-source-transforms :media-set-source-transforms
                         :export-transforms
                         :object-source-transforms :link-source-transforms
                         :object-implements :deletes}]
    (into #{}
          (comp (map :phase)
                (remove step-phases)
                (map #(phase->section % %)))
          install-plan/staging-order)))

(deftest the-fixture-covers-every-installable-section
  (let [want    (into #{} (map #(get {:objects :object-types
                                      :links   :link-types} % %))
                      installable-sections)
        missing (clojure.set/difference want (set (keys live-schema)))]
    (is (empty? missing)
        (str "live-schema is missing " (sort missing)
             " — until a section is in the fixture, nothing below can see it"))))

(deftest export-covers-every-section-the-installer-handles
  (let [{:keys [ent units]} (export-live)
        exports (into (set (keys (:schema ent))) (comp (mapcat :nodes) (map first)) units)
        missing (clojure.set/difference installable-sections exports
                                        #{:sources})]
    (is (= export-gaps missing)
        (str "export/install section coverage changed. Missing from export: "
             (sort missing) ". If that set GREW, a section can now be installed "
             "but not exported — authored intent is silently lost on round trip. "
             "If it SHRANK, delete the entry from `export-gaps`."))))

(deftest export-then-reinstall-changes-nothing
  (let [desired (sugar/lower-schema (:schema (:ent (export-live))))
        plan    (install/plan-schema desired live-schema {})]
    (is (seq plan) "the fixture must actually exercise the comparison")
    (doseq [{:keys [phase entity outcome]} plan]
      (is (= :skipped outcome)
          (str phase "/" entity " does not round-trip: exported from the live "
               "schema, it plans a " (name outcome) " against that same schema")))))

(deftest no-rid-escapes-into-the-exported-ent
  (let [rids (into #{} (mapcat (comp keys live-schema)) rid/rid-kinds)
        seen (atom [])]
    (is (seq rids) "the fixture must be RID-keyed for this to mean anything")
    (clojure.walk/postwalk
     (fn [x] (when (contains? rids x) (swap! seen conj x)) x)
     (:ent (export-live)))
    (is (empty? @seen)
        (str "these live RIDs reached the exported .ent instead of the names "
             "they point at: " (distinct @seen)))))

(deftest an-empty-collection-and-an-absent-key-compare-equal
  (let [live (assoc-in live-schema ["ri.d.1" :columns] [])
        p    (install/plan-schema (sugar/lower-schema (:schema (:ent (export-live)))) live {})]
    (is (every? #(= :skipped (:outcome %)) p))))

(deftest a-live-rid-compares-equal-to-the-name-that-authored-it
  (let [live {:object-types {"ri.o.1" {:object-type-name :triebwerk :attribute-list []
                                       :plural "Triebwerke"}
                             "ri.o.2" {:object-type-name :fahrwerk :attribute-list []
                                       :implements ["ri.i.1"]
                                       :plural "Fahrwerke"}}
              :interfaces   {"ri.i.1" {:interface-name :wartbares-teil :property-list []}}
              :object-type-groups {"ri.g.1" {:object-type-group-name :grp
                                             :members ["ri.o.1"]}}
              :functions    {"ri.w.1" {:function-name :zyklen-zaehlen :target-type "ri.o.1"
                                       :language :clojure
                                       :source "(defn zyklen-zaehlen [t] (-> OntologyClient/action (.update-triebwerk t {:zyklen 1})))"}}
              :applications {"ri.a.1" {:application-name :portal :object-type "ri.o.1"
                                       :status :published}}
              :link-types   {"ri.l.1" {:link-type-name :verbaut-mit
                                       :from-object-type "ri.o.1"
                                       :to-object-type "ri.o.2"
                                       :cardinality :one-to-many}}}
        desired {:objects    {:triebwerk {:attributes [] :plural "Triebwerke"}
                              :fahrwerk {:attributes [] :implements [:wartbares-teil]
                                         :plural "Fahrwerke"}}
                 :interfaces {:wartbares-teil {:property-list []}}
                 :object-type-groups {:grp {:members [:triebwerk]}}
                 :functions
                 {:zyklen-zaehlen {:target-type :triebwerk :language :clojure
                                       :source "(defn zyklen-zaehlen [t] (-> OntologyClient/action (.update-triebwerk t {:zyklen 1})))"}}
                 :applications {:portal {:object-type :triebwerk :status :published}}
                 :links      [[:verbaut-mit :triebwerk :fahrwerk :one-to-many]]}]
    (doseq [{:keys [phase entity outcome]} (install/plan-schema desired live {})]
      (is (= :skipped outcome)
          (str phase "/" entity " re-stages although the authored names resolve "
               "to exactly the RIDs already live — a no-op update, forever")))))

(deftest a-live-status-survives-the-implements-restage
  (let [live {:object-types {"ri.o.2" {:object-type-name :fahrwerk :attribute-list []
                                       :implements ["ri.i.1"] :status :active
                                       :plural "Fahrwerke"}}
              :interfaces   {"ri.i.1" {:interface-name :wartbares-teil :property-list []}}}
        desired {:objects    {:fahrwerk {:attributes [] :implements [:wartbares-teil]
                                         :status :active
                                         :plural "Fahrwerke"}}
                 :interfaces {:wartbares-teil {:property-list []}}}]
    (doseq [{:keys [phase entity outcome]} (install/plan-schema desired live {})]
      (is (= :skipped outcome)
          (str phase "/" entity " re-stages although nothing changed — the "
               ":object-implements pass dropped :status from the args it "
               "compares, so every live status reads as a difference")))))

(defn- staged-attr-names
  [plan type-kw]
  (some->> plan
           (filter #(and (= :object-types (:phase %)) (= type-kw (:entity %))))
           first :args :attributes (mapv first)))

(deftest merge-mode-adds-a-column-without-dropping-the-rest
  (let [live {:object-types
              {"ri.o.1"
               {:object-type-name :teileeingang
                :implements ["ri.i.pos"]
                :attribute-list
                [{:attribute-name :eingang-id :attribute-type :string
                  :opts {:primary-key true}}
                 {:attribute-name :stueckpreis :attribute-type :eur-amount
                  :opts {:required true}}
                 {:attribute-name :mwst-satz :attribute-type :prozent
                  :opts {:constraints {:min 0 :max 100}}}]
                :plural "Teileeingänge"}}
              :interfaces {"ri.i.pos" {:interface-name :teileposition :property-list []}}}
        desired {:objects {:teileeingang
                           {:reconcile-mode :merge
                            :attributes [[:resolved-hersteller-id :string]]
                            :plural "Teileeingänge"}}}
        plan  (install/plan-schema desired live {})
        names (staged-attr-names plan :teileeingang)]
    (is (= [:eingang-id :stueckpreis :mwst-satz :resolved-hersteller-id] names)
        "merge keeps every live attr (PK + value-typed) and appends the new one")
    (let [merged (->> plan (filter #(= :teileeingang (:entity %)))
                      first :args :attributes)]
      (is (= {:primary-key true} (nth (first merged) 2)) "PK opt preserved")
      (is (= {:min 0 :max 100} (get (nth (nth merged 2) 2) :constraints))
          "value-type constraint preserved"))))

(deftest replace-mode-is-the-default-and-still-replaces
  (let [live {:object-types
              {"ri.o.1" {:object-type-name :triebwerk
                         :attribute-list [{:attribute-name :id :attribute-type :string}
                                          {:attribute-name :zyklen :attribute-type :integer}]
                         :plural "Triebwerke"}}}
        desired {:objects {:triebwerk {:attributes [[:only-me :string]] :plural "Triebwerke"}}}
        names   (staged-attr-names (install/plan-schema desired live {}) :triebwerk)]
    (is (= [:only-me] names) "default replace drops :id and :zyklen — unchanged")))

(deftest merge-is-idempotent-a-second-plan-skips
  (let [live {:object-types
              {"ri.o.1" {:object-type-name :triebwerk
                         :attribute-list [{:attribute-name :id :attribute-type :string}
                                          {:attribute-name :fk :attribute-type :string}]
                         :plural "Triebwerke"}}}
        desired {:objects {:triebwerk {:reconcile-mode :merge
                                       :attributes [[:fk :string]]
                                       :plural "Triebwerke"}}}
        outcome (->> (install/plan-schema desired live {})
                     (filter #(= :object-types (:phase %))) first :outcome)]
    (is (= :skipped outcome) "the FK is already live — merge stages nothing")))

(deftest merge-survives-the-implements-restage
  (let [live {:object-types
              {"ri.o.1" {:object-type-name :teileposition
                         :implements ["ri.i.1"]
                         :attribute-list
                         [{:attribute-name :pos-id :attribute-type :string
                           :opts {:primary-key true}}
                          {:attribute-name :stueckpreis :attribute-type :eur-amount}]
                         :plural "Teilepositionen"}}
              :interfaces {"ri.i.1" {:interface-name :materialzeile :property-list []}}}
        desired {:objects {:teileposition {:reconcile-mode :merge
                                           :implements [:materialzeile]
                                           :attributes [[:resolved-hersteller-id :string]]
                                           :plural "Teilepositionen"}}
                 :interfaces {:materialzeile {:property-list []}}}
        by-phase (->> (install/plan-schema desired live {})
                      (filter #(= :object-type (:kind %)))
                      (map (juxt :phase #(mapv first (:attributes (:args %)))))
                      (into {}))]
    (is (= [:pos-id :stueckpreis :resolved-hersteller-id] (:object-types by-phase))
        "the first staging merges — this half always worked")
    (is (= [:pos-id :stueckpreis :resolved-hersteller-id] (:object-implements by-phase))
        "and the re-stage must carry the SAME list, or it silently truncates the
         type back to what the author happened to name")))

(deftest merge-mode-adds-an-interface-property-without-dropping-the-rest
  (let [live {:interfaces
              {"ri.i.1" {:interface-name :nachweis
                         :property-list [{:attribute-name :schein-nr :attribute-type :string}
                                         {:attribute-name :ausgestellt-am :attribute-type :date}]}}}
        desired {:interfaces {:nachweis {:reconcile-mode :merge
                                         :property-list [[:archiviert :boolean]]}}}
        props (->> (install/plan-schema desired live {})
                   (filter #(= :interfaces (:phase %))) first :args :property-list
                   (mapv first))]
    (is (= [:schein-nr :ausgestellt-am :archiviert] props)
        "merge keeps the interface's existing contract and adds one property")))

(deftest a-new-link-producer-supersedes-the-differently-named-old-one
  (let [live {:object-types {"ri.o.1" {:object-type-name :stundenbuchung :attribute-list []
                                       :plural "Stundenbuchungen"}
                             "ri.o.2" {:object-type-name :arbeitsauftrag :attribute-list []
                                       :plural "Arbeitsaufträge"}}
              :link-types {"ri.l.1" {:link-type-name :buchung-auftrag
                                     :from-object-type "ri.o.1" :to-object-type "ri.o.2"
                                     :cardinality :many-to-one}}
              :transforms {"ri.t.old" {:transform-name :reconcile-buchung-auftrag
                                       :language :pg-sql :query "SELECT 1"
                                       :output {:kind :link-type :link-type "ri.l.1"
                                                :from-key-column :b :to-key-column :r}}}}
        desired {:objects {:stundenbuchung {:attributes [] :plural "Stundenbuchungen"}
                           :arbeitsauftrag {:attributes [] :plural "Arbeitsaufträge"}}
                 :links [[:buchung-auftrag :stundenbuchung :arbeitsauftrag :many-to-one]]
                 :transforms {:link-buchung-auftrag
                              {:input {:kind :object-type :object-type :stundenbuchung}
                               :output {:kind :link-type :link-type :buchung-auftrag
                                        :fk-attribute :resolved-auftrag-id}}}}
        plan (install/plan-schema desired live {})
        del  (filter #(and (= :deletes (:phase %)) (= :transform (:kind %))) plan)]
    (is (= [:reconcile-buchung-auftrag] (map :entity del))
        "the legacy producer is superseded (delete staged in the :deletes phase)")
    (is (= :delete-transform (:action (first del))))
    (is (every? #(#{:created :updated :skipped :deleted} (:outcome %)) plan))))

(deftest a-same-named-producer-is-updated-in-place-not-superseded
  (let [live {:object-types {"ri.o.1" {:object-type-name :arbeitspaket :attribute-list []
                                       :plural "Arbeitspakete"}
                             "ri.o.2" {:object-type-name :arbeitsauftrag :attribute-list []
                                       :plural "Arbeitsaufträge"}}
              :link-types {"ri.l.1" {:link-type-name :paket-auftrag
                                     :from-object-type "ri.o.1" :to-object-type "ri.o.2"
                                     :cardinality :many-to-one}}
              :transforms {"ri.t.1" {:transform-name :link-paket-auftrag
                                     :language :pg-sql :query "SELECT 1"
                                     :output {:kind :link-type :link-type "ri.l.1"
                                              :from-key-column :k :to-key-column :b}}}}
        desired {:objects {:arbeitspaket {:attributes [] :plural "Arbeitspakete"}
                           :arbeitsauftrag {:attributes [] :plural "Arbeitsaufträge"}}
                 :links [[:paket-auftrag :arbeitspaket :arbeitsauftrag :many-to-one]]
                 :transforms {:link-paket-auftrag
                              {:input {:kind :object-type :object-type :arbeitspaket}
                               :output {:kind :link-type :link-type :paket-auftrag
                                        :fk-attribute :resolved-ba-nr}}}}
        plan (install/plan-schema desired live {})]
    (is (empty? (filter #(and (= :deletes (:phase %)) (= :transform (:kind %))) plan))
        "a same-named producer is updated in place — nothing superseded")))

(deftest merge-mode-adds-a-satisfies-membership-without-dropping-the-rest
  (let [live {:object-types {"ri.o.1" {:object-type-name :freigabeschein :attribute-list []
                                       :plural "Freigabescheine"}
                             "ri.o.2" {:object-type-name :wartungsbetrieb :attribute-list []
                                       :plural "Wartungsbetriebe"}}
              :link-types
              {"ri.l.1" {:link-type-name :schein-betrieb
                         :from-object-type "ri.o.1" :to-object-type "ri.o.2"
                         :cardinality :many-to-one
                         :satisfies [{:interface :nachweis :constraint :aussteller}]}}}
        desired {:objects {:freigabeschein {:attributes [] :plural "Freigabescheine"}
                           :wartungsbetrieb {:attributes [] :plural "Wartungsbetriebe"}}
                 :links [[:schein-betrieb :freigabeschein :wartungsbetrieb :many-to-one
                          {:reconcile-mode :merge
                           :satisfies {:interface :zertifikat :constraint :zertifizierer}}]]}
        sat (->> (install/plan-schema desired live {})
                 (filter #(= :link-types (:phase %))) first :args :satisfies set)]
    (is (= #{{:interface :nachweis :constraint :aussteller}
             {:interface :zertifikat :constraint :zertifizierer}} sat)
        "merge unions the interface-link memberships, keeping the existing one")))

(deftest a-datasets-external-source-stages-a-sync-transform
  (let [desired {:datasets
                 {:einbau_roh
                  {:kind :standalone
                   :external-source {:datasource  :amos
                                     :query       "SELECT name FROM GRO_MONTAGE_STUNDENS"
                                     :primary-key :name}}}}
        plan    (install/plan-schema desired {} {})
        sync    (->> plan
                     (filter #(= :dataset-source-transforms (:phase %)))
                     first)]
    (is (some? sync) "a dataset carrying :external-source stages a sync transform")
    (is (= :transform (:kind sync)))
    (is (= :einbau_roh-sync (:name (:args sync)))
        "named <dataset>-sync — distinct from the object-terminal <type>-rows")
    (is (= {:kind :dataset :dataset :einbau_roh} (:output (:args sync)))
        "it outputs the DATASET, which is the entire point of the layer line")
    (is (= {:kind :datasource :datasource :amos} (:input (:args sync))))
    (is (= :name (:primary-key (:args sync)))
        "PK rides along so an incremental re-emit upserts instead of duplicating")

    (testing "the dataset itself still stages, and WITHOUT the custom field"
      (let [ds (->> plan (filter #(= :datasets (:phase %))) first)]
        (is (some? ds))
        (is (nil? (:external-source (:args ds)))
            "carrying it on the dataset too would re-stage on every install —
             the live spec never has it back (install strips it once lifted)")))))

(deftest a-dataset-without-external-source-stages-no-sync
  (let [plan (install/plan-schema {:datasets {:raw_uploads {:kind :standalone}}} {} {})]
    (is (empty? (filter #(= :dataset-source-transforms (:phase %)) plan))
        "an uploaded/producer-minted dataset has no source to sync from")))

(deftest a-datasets-project-ownership-is-staged-and-compared
  (testing "an authored :project reaches the create action"
    (let [plan (install/plan-schema
                {:datasets {:tag_agg {:kind :standalone :project :linienwartung}}} {} {})
          ds   (->> plan (filter #(= :datasets (:phase %))) first)]
      (is (= :created (:outcome ds)))
      (is (= :linienwartung (:project (:args ds)))
          "without this the dataset is created unowned and no re-install fixes it")))

  (testing "a live dataset whose owner differs is an UPDATE, not a skip"
    (let [live {:datasets {"ri.d.9" {:dataset-name :tag_agg :kind :standalone
                                     :project "ri.p.1"}}
                :projects {"ri.p.1" {:project-name :materialwirtschaft}}}
          plan (install/plan-schema
                {:datasets {:tag_agg {:kind :standalone :project :linienwartung}}} live {})
          ds   (->> plan (filter #(= :datasets (:phase %))) first)]
      (is (= :updated (:outcome ds)))
      (is (= :linienwartung (:project (:args ds))))))

  (testing "matching ownership still skips — the live RID compares as a NAME"
    (let [live {:datasets {"ri.d.9" {:dataset-name :tag_agg :kind :standalone
                                     :project "ri.p.1"}}
                :projects {"ri.p.1" {:project-name :linienwartung}}}
          plan (install/plan-schema
                {:datasets {:tag_agg {:kind :standalone :project :linienwartung}}} live {})
          ds   (->> plan (filter #(= :datasets (:phase %))) first)]
      (is (= :skipped (:outcome ds)))))

  (testing "an .ent that says nothing about ownership does not STRIP it"
    (let [live {:datasets {"ri.d.9" {:dataset-name :tag_agg :kind :standalone
                                     :project "ri.p.1"}}
                :projects {"ri.p.1" {:project-name :linienwartung}}}
          plan (install/plan-schema {:datasets {:tag_agg {:kind :standalone}}} live {})
          ds   (->> plan (filter #(= :datasets (:phase %))) first)]
      (is (= :skipped (:outcome ds))
          "otherwise re-installing the file would quietly unown the dataset"))))

(defn- problems
  ([desired] (problems desired {}))
  ([desired current]
   (install-plan/plan-problems desired current (install/plan-schema desired current {}))))

(deftest a-plan-that-carries-every-binding-has-no-problems
  (is (empty? (problems {:datasets {:raw {:project :linienwartung
                                          :external-source {:datasource :sap-pm :table "T"}}}}))
      "a declared sync that stages its <name>-sync transform is exactly right")
  (is (empty? (problems {:projects {:linienwartung {}}
                         :objects {:triebwerk {:project :linienwartung :attributes [[:id :string]]
                                               :external-source
                                               {:datasource :sap-pm :table "W"}
                                               :plural "Triebwerke"}}}))
      "the object-terminal form is DISCOURAGED, not unbacked — lint's job, not the gate's")
  (is (empty? (problems desired))
      "the ordinary fixture — no :external-source anywhere — is silent"))

(deftest a-declared-binding-that-stages-no-producer-is-fatal
  (let [desired {:datasets {:einbau_roh
                            {:project :linienwartung
                             :external-source {:datasource :amos :table "EINBAU"}}}}
        gutted  (remove #(= :dataset-source-transforms (:phase %))
                        (install/plan-schema desired {} {}))
        probs   (install-plan/plan-problems desired {} gutted)]
    (is (= 1 (count probs)))
    (is (= :binding-dropped (:rule (first probs))))
    (is (= :dataset (:entity-class (first probs))))
    (is (= :einbau_roh (:entity (first probs))))
    (is (re-find #"no transform that produces it" (:message (first probs))))))

(deftest removing-the-last-producer-of-a-live-entity-is-fatal
  (let [live {:object-types
              {"ri.o.1" {:object-type-name :einbau
                         :attribute-list [{:attribute-name :name :attribute-type :string}]
                         :plural "Einbauten"}}
              :transforms
              {"ri.t.1" {:transform-name :einbau-rows
                         :input  {:kind :datasource :datasource :amos}
                         :output {:kind :object-type :object-type "ri.o.1"}}}}
        desired {:objects {:einbau {:attributes [[:name :string]] :plural "Einbauten"}}}
        probs   (install-plan/plan-problems
                 desired live (install/plan-schema desired live {:strict? true}))]
    (is (= [:producer-removed] (map :rule probs)))
    (is (= :einbau (:entity (first probs))))
    (is (= [:einbau-rows] (:was (first probs)))
        "the message names what it would remove, so the fix is obvious")))

(deftest a-dataset-that-loses-its-producer-stays-unbuilt
  (let [live    {:datasets   {"ri.d.1" {:dataset-name :journal_raw}}
                 :transforms {"ri.t.1" {:transform-name :journal-sync
                                        :input  {:kind :datasource :datasource :amos}
                                        :output {:kind :dataset :dataset "ri.d.1"}}}}
        desired {:datasets {:journal_raw {}}}
        plan    (install/plan-schema desired live {:strict? true})
        f       (->> (install-plan/plan-findings desired live plan)
                     (filter #(= :journal_raw (:entity %)))
                     first)]
    (is (empty? (install-plan/plan-problems desired live plan))
        "code first, dataset later — that order needs no override")
    (is (= [:dataset-no-longer-built :warn] [(:rule f) (:severity f)]))
    (is (re-find #"no longer built" (:message f)))))

(deftest deleting-the-entity-along-with-its-producer-is-not-a-problem
  (let [live {:object-types
              {"ri.o.1" {:object-type-name :einbau :attribute-list [] :plural "Einbauten"}}
              :transforms
              {"ri.t.1" {:transform-name :einbau-rows
                         :output {:kind :object-type :object-type "ri.o.1"}}}}
        probs (install-plan/plan-problems {} live (install/plan-schema {} live {:strict? true}))]
    (is (empty? probs))))

(deftest replacing-a-producer-in-the-same-ent-is-not-a-problem
  (let [live {:object-types
              {"ri.o.1" {:object-type-name :einbau
                         :attribute-list [{:attribute-name :name :attribute-type :string}]
                         :plural "Einbauten"}}
              :transforms
              {"ri.t.1" {:transform-name :einbau-rows
                         :output {:kind :object-type :object-type "ri.o.1"}}}}
        desired {:objects  {:einbau {:project :linienwartung :attributes [[:name :string]]
                                     :plural "Einbauten"}}
                 :datasets {:einbau_roh
                            {:project :linienwartung
                             :external-source {:datasource :amos :table "EINBAU"}}}
                 :transforms {:einbau-formen
                              {:project :linienwartung :language :pg-sql
                               :query    "SELECT name FROM einbau_roh"
                               :inputs   {:einbau_roh {:kind :dataset
                                                       :dataset :einbau_roh}}
                               :output   {:kind :object-type :object-type :einbau}}}}]
    (is (empty? (install-plan/plan-problems
                 desired live (install/plan-schema desired live {:strict? true})))
        "the new producer arrives in the same plan that sweeps the old one")))

(deftest a-dropped-binding-has-no-override
  (let [desired {:datasets {:raw {:project :linienwartung
                                  :external-source {:datasource :sap-pm :table "T"}}}}
        gutted  (remove #(= :dataset-source-transforms (:phase %))
                        (install/plan-schema desired {} {}))]
    (is (= [:binding-dropped]
           (map :rule (install-plan/plan-problems desired {} gutted)))
        "a dropped binding is a planner bug — it has no override, by design")))

(defn- live-type
  ([spec pk] (live-type spec pk {}))
  ([spec pk t-extra]
   {:projects     {"ri.p.1" {:project-name :werk}}
    :object-types {"ri.o.1" (merge {:object-type-name :hersteller :project "ri.p.1"
                                    :attribute-list [{:attribute-name :name :attribute-type :string}
                                                     {:attribute-name :zugelassen-seit
                                                      :attribute-type :string}]}
                                   spec)}
    :transforms   {"ri.t.1" (merge {:transform-name :hersteller-rows :project "ri.p.1"
                                    :language :pg-sql
                                    :query "SELECT name, zugelassen-seit FROM src"
                                    :inputs {:src {:kind :dataset :dataset :src}}
                                    :output {:kind :object-type :object-type "ri.o.1"
                                             :primary-key pk}}
                                   t-extra)}}))

(defn- findings [desired live]
  (fnd/scan migration/identity-changes
            (diff/diff desired live (install/plan-schema desired live {}) {}
                       (live/world-index live))))

(deftest a-declared-grain-the-mint-does-not-key-by-is-fatal
  (let [live    (live-type {:match-keys [:name]} :name)
        desired {:objects {:hersteller {:project :werk
                                        :attributes [[:name :string] [:zugelassen-seit :string]]
                                        :match-keys [:name :zugelassen-seit]
                                        :plural "Hersteller"}}}
        f       (first (findings desired live))]
    (is (= 1 (count (findings desired live))))
    (is (= :identity-inert (:rule f)))
    (is (= :error (:severity f)))
    (is (= :hersteller (:entity f)))
    (is (re-find #":primary-key :name" (:message f))
        "the message names the column that actually keys the type")
    (is (contains? (set (map :rule
                             (install-plan/plan-problems desired live
                                                         (install/plan-schema desired live {}))))
                   :identity-inert)
        "and it is a refusal, not a note — plan-problems carries it")))

(deftest a-declaration-the-mint-does-key-by-is-silent
  (testing "one match-key, and it IS the minted column"
    (is (empty? (findings {:objects {:hersteller {:project :werk :attributes [[:name :string]]
                                                  :match-keys [:name]
                                                  :plural "Hersteller"}}}
                          (live-type {:match-keys [:name]} :name)))))
  (testing "a composite key the shaping transform COMPUTED — unreadable, so unjudged"
    (is (empty? (findings {:objects
                           {:hersteller {:project :werk
                                         :attributes [[:name :string] [:zugelassen-seit :string]]
                                         :match-keys [:name :zugelassen-seit]
                                         :plural "Hersteller"}}}
                          (live-type {:match-keys [:name :zugelassen-seit]} :satz-key)))))
  (testing "a RESOLVED entity — there :match-keys really is the rule"
    (is (empty? (findings {:objects
                           {:hersteller {:project :werk
                                         :attributes [[:name :string] [:zugelassen-seit :string]]
                                         :resolve-entity true
                                         :match-keys [:name :zugelassen-seit]
                                         :plural "Hersteller"}}}
                          (live-type {:resolve-entity true :match-keys [:name :zugelassen-seit]}
                                     :name))))))

(deftest moving-the-key-of-an-active-type-is-refused
  (doseq [status [:active :endorsed]]
    (testing (str "status " status)
      (let [live    (live-type {:match-keys [:name] :status status} :name)
            desired {:objects {:hersteller {:project :werk
                                            :attributes [[:name :string] [:zugelassen-seit :string]]
                                            :match-keys [:name]
                                            :plural "Hersteller"}}
                     :transforms {:hersteller-rows
                                  {:project :werk :language :pg-sql :query "SELECT satz_key FROM src"
                                   :inputs {:src {:kind :dataset :dataset :src}}
                                   :output {:kind :object-type :object-type :hersteller
                                            :primary-key :satz-key}}}}
            f       (first (findings desired live))]
        (is (= :identity-locked (:rule f)))
        (is (= :error (:severity f)))
        (is (re-find #"experimental" (:message f))
            "and it names the demotion that unblocks it")))))

(deftest moving-the-key-with-nothing-to-retract-the-old-rows-is-refused
  (let [live    (live-type {:match-keys [:name]} :name
                           {:incremental {:write-mode :modify} :semantic-version 1})
        desired {:objects {:hersteller {:project :werk
                                        :attributes [[:name :string] [:zugelassen-seit :string]]
                                        :match-keys [:name]
                                        :plural "Hersteller"}}
                 :transforms {:hersteller-rows
                              {:project :werk :language :pg-sql :query "SELECT satz_key FROM src"
                               :inputs {:src {:kind :dataset :dataset :src}}
                               :incremental {:write-mode :modify}
                               :semantic-version 1
                               :output {:kind :object-type :object-type :hersteller
                                        :primary-key :satz-key}}}}
        f       (first (findings desired live))]
    (is (= :identity-unretractable (:rule f)))
    (is (= :error (:severity f)))
    (is (and (re-find #":semantic-version" (:message f))
             (re-find #":write-mode :replace" (:message f)))
        "the message IS the fix — both halves of the pair, named")))

(deftest moving-the-key-so-it-takes-effect-is-allowed-and-reported
  (let [live    (live-type {:match-keys [:name]} :name
                           {:incremental {:write-mode :modify} :semantic-version 1})
        desired {:objects {:hersteller {:project :werk
                                        :attributes [[:name :string] [:zugelassen-seit :string]]
                                        :match-keys [:name]
                                        :plural "Hersteller"}}
                 :transforms {:hersteller-rows
                              {:project :werk :language :pg-sql :query "SELECT satz_key FROM src"
                               :inputs {:src {:kind :dataset :dataset :src}}
                               :incremental {:write-mode :replace}
                               :semantic-version 2
                               :output {:kind :object-type :object-type :hersteller
                                        :primary-key :satz-key}}}}
        plan    (install/plan-schema desired live {})
        f       (first (install-plan/plan-warnings desired live plan))]
    (is (empty? (filter #(str/starts-with? (name (:rule %)) "identity")
                        (install-plan/plan-problems desired live plan)))
        "the pair is shipped — nothing to refuse")
    (is (= :identity-remint (:rule f)))
    (is (= :warn (:severity f)))
    (is (re-find #"drop-edits" (:message f))
        "we name the verb")
    (is (re-find #"unavailable" (:message f))
        "and the reindex window the objects are unavailable in")))

(deftest a-non-incremental-producer-remints-by-itself
  (let [live    (live-type {:match-keys [:name]} :name)
        desired {:objects {:hersteller {:project :werk
                                        :attributes [[:name :string] [:zugelassen-seit :string]]
                                        :match-keys [:name]
                                        :plural "Hersteller"}}
                 :transforms {:hersteller-rows
                              {:project :werk :language :pg-sql :query "SELECT satz_key FROM src"
                               :inputs {:src {:kind :dataset :dataset :src}}
                               :output {:kind :object-type :object-type :hersteller
                                        :primary-key :satz-key}}}}]
    (is (= [:identity-remint] (map :rule (findings desired live))))))

(deftest a-resolved-entitys-match-keys-ARE-its-mint
  (let [live    (live-type {:resolve-entity true :match-keys [:name]} :name)
        desired {:objects {:hersteller {:project :werk
                                        :attributes [[:name :string] [:zugelassen-seit :string]]
                                        :resolve-entity true
                                        :match-keys [:name :zugelassen-seit]
                                        :plural "Hersteller"}}}]
    (is (= [:identity-remint] (map :rule (findings desired live))))))

(deftest an-identity-moved-from-the-CODE-repository-is-caught-too
  (let [live    (live-type {:match-keys [:name]} :name)
        desired {:transforms {:hersteller-rows
                              {:project :werk :language :pg-sql :query "SELECT satz_key FROM src"
                               :inputs {:src {:kind :dataset :dataset :src}}
                               :output {:kind :object-type :object-type :hersteller
                                        :primary-key :satz-key}}}}]
    (is (= [:identity-remint] (map :rule (findings desired live))))))

(deftest a-brand-new-type-changes-no-identity
  (is (empty? (findings {:objects {:neu {:attributes [[:name :string]]
                                         :match-keys [:name]
                                         :plural "Neue"}}} {})))
  (is (= [:identity-inert]
         (map :rule
              (findings {:objects {:neu {:attributes [[:name :string] [:zugelassen-seit :string]]
                                         :match-keys [:name :zugelassen-seit]
                                         :plural "Neue"}}
                         :transforms {:neu-rows
                                      {:language :pg-sql :query "SELECT name FROM src"
                                       :inputs {:src {:kind :dataset :dataset :src}}
                                       :output {:kind :object-type :object-type :neu
                                                :primary-key :name}}}}
                        {})))))

(deftest gaining-or-losing-a-backing-is-not-a-key-change
  (let [live (live-type {:match-keys [:name]} :name)]
    (testing "losing it"
      (is (empty? (filter #(= :warn (:severity %))
                          (findings {:objects
                                     {:hersteller {:project :werk :attributes [[:name :string]]
                                                   :match-keys [:name]
                                                   :plural "Hersteller"}}}
                                    live))))
      (is (= [:producer-removed]
             (map :rule
                  (install-plan/plan-problems
                   {:objects {:hersteller {:project :werk :attributes [[:name :string]]
                                           :match-keys [:name]
                                           :plural "Hersteller"}}}
                   live
                   (install/plan-schema
                    {:objects {:hersteller {:project :werk :attributes [[:name :string]]
                                            :match-keys [:name]
                                            :plural "Hersteller"}}}
                    live {:strict? true}))))
          "only the backing gate speaks — and its override still works"))
    (testing "gaining it"
      (is (empty? (findings {:transforms {:hersteller-rows
                                          {:project :werk :language :pg-sql
                                           :query "SELECT name FROM src"
                                           :inputs {:src {:kind :dataset :dataset :src}}
                                           :output {:kind :object-type :object-type :hersteller
                                                    :primary-key :name}}}}
                            {:object-types {"ri.o.1" {:object-type-name :hersteller
                                                      :match-keys [:name]
                                                      :attribute-list []
                                                      :plural "Hersteller"}}}))))))

(deftest an-untouched-type-is-not-re-judged
  (is (empty? (findings {} (live-type {:match-keys [:name :zugelassen-seit]} :name)))
      "the live state is inconsistent, and this install does not touch it"))

(deftest a-minted-output-dataset-is-not-a-change
  (let [desired (sugar/lower-schema
                 {:objects {:stundenbuchung {:attributes [[:auftrag-nr :string]]
                                             :plural "Stundenbuchungen"}
                            :arbeitsauftrag     {:attributes [[:auftrag-nr :string]]
                                                 :plural "Arbeitsaufträge"}}
                  :links [[:sb-auftrag :stundenbuchung :arbeitsauftrag :many-to-one
                           {:fk {:attribute :auftrag-nr :on-unresolved :skip}}]]})
        generated (get-in desired [:transforms :link-sb-auftrag])
        live-tx   (fn [extra]
                    {:transforms {"ri.t.1" (merge {:transform-name :link-sb-auftrag}
                                                  (update generated :output merge extra))}})
        outcome   (fn [live] (->> (install/plan-schema desired live {})
                                  (filter #(and (= :transform (:kind %))
                                                (= :link-sb-auftrag (:entity %))))
                                  first :outcome))]
    (is (= :skipped (outcome (live-tx {:dataset :link-sb-auftrag
                                       :name    :link-sb-auftrag})))
        "the installer stamped those; the author said nothing about them, so the
         comparison must say nothing about them either")
    (is (= :skipped (outcome (live-tx {}))) "and with nothing stamped, still equal")))

(deftest an-authored-output-dataset-is-still-compared
  (let [desired {:transforms {:t {:language :pg-sql :query "SELECT 1"
                                  :inputs {:src {:kind :dataset :dataset :src}}
                                  :output {:kind :dataset :dataset :mine}}}}
        live    {:transforms {"ri.t.1" {:transform-name :t
                                        :language :pg-sql :query "SELECT 1"
                                        :inputs {:src {:kind :dataset :dataset :src}}
                                        :output {:kind :dataset :dataset :somebody-elses}}}}]
    (is (= :updated (->> (install/plan-schema desired live {})
                         (filter #(= :t (:entity %))) first :outcome))
        "an author who NAMES the dataset gets it compared")))

(deftest a-second-create-of-a-staged-name-is-refused
  (testing "the plan is diffed against the COMMITTED schema, so staging the same
            new entity twice does not converge — it mints a second one, and a
            batch with two creates of one name can never commit"
    (let [staged {[:project :werkzeug] 10425}
          plan   [{:kind :project :entity :werkzeug :outcome :created}
                  {:kind :project :entity :materialwirtschaft :outcome :created}
                  {:kind :object-type :entity :werkzeug :outcome :created}]
          probs  (install-plan/already-staged-problems staged plan)]
      (is (= 1 (count probs)) "only the project — the kind is part of the key")
      (is (= :already-staged (:rule (first probs))))
      (is (= 10425 (:seq (first probs))))
      (is (str/includes? (:message (first probs)) "changeset drop 10425")
          "the seq is IN the sentence: a refusal nobody can act on is not one")))

  (testing "an UPDATE against a staged create is not a duplicate — only creates mint"
    (is (empty? (install-plan/already-staged-problems
                 {[:project :werkzeug] 10425}
                 [{:kind :project :entity :werkzeug :outcome :updated}]))))

  (testing "and an empty batch judges nothing"
    (is (empty? (install-plan/already-staged-problems
                 {} [{:kind :project :entity :werkzeug :outcome :created}])))))

(defn- action-value-problems [schema]
  (fnd/scan actions/rule-values {:text schema}))

(def ^:private with-actions
  {:objects {:mangel {:attributes [[:mangel-nr :string]
                                   [:luftfahrzeug :string {:description "FK auf :kennzeichen."}]
                                   [:bauteil :string]]
                      :plural "Mängel"}
             :luftfahrzeug {:attributes [[:kennzeichen :string] [:bezeichnung :string]]
                            :plural "Luftfahrzeuge"}}
   :actions
   {:melden-falsch  {:params {:luftfahrzeug {:type :luftfahrzeug} :bauteil {:type :string}}
                     :rules  [{:object-type :mangel :op :create
                               :values {:luftfahrzeug [:get :luftfahrzeug]
                                        :bauteil        [:get :bauteil]}}]}
    :melden-per-feld {:params {:luftfahrzeug {:type :luftfahrzeug}}
                      :rules  [{:object-type :mangel :op :create
                                :values {:luftfahrzeug [:get :luftfahrzeug :kennzeichen]}}]}
    :melden-per-key  {:params {:luftfahrzeug {:type :string}}
                      :rules  [{:object-type :mangel :op :create
                                :values {:luftfahrzeug [:get :luftfahrzeug]}}]}}})

(deftest an-object-parameter-may-not-be-written-into-an-attribute
  (let [ps (action-value-problems with-actions)]
    (testing "exactly the whole-object write is flagged"
      (is (= 1 (count ps)))
      (is (= :melden-falsch (:entity (first ps))))
      (is (= :luftfahrzeug (:attribute (first ps))))
      (is (= :object-param-into-attribute (:rule (first ps)))))

    (testing "the message names the declared type and BOTH working spellings"
      (let [m (:message (first ps))]
        (is (str/includes? m ":string") "the attribute's declared type")
        (is (str/includes? m ":type :string") "declare the parameter as the key")
        (is (str/includes? m ":some-attribute") "or read a field off the object")))

    (testing "a scalar parameter and a field read are both fine"
      (is (not-any? #{:melden-per-feld :melden-per-key} (map :entity ps))))))

(deftest the-gate-is-quiet-on-a-schema-with-no-actions
  (is (empty? (action-value-problems desired))))

(deftest a-create-reading-a-list-parameter-is-flagged
  (let [ps (action-value-problems
            {:actions
             {:je-element   {:params {:nrs {:type :string :multiple true}}
                             :rules  [{:object-type :mangel :op :create
                                       :values {:mangel-nr [:get :nrs]}}]}
              :ausdruecklich {:params {:nrs {:type :string :multiple true}}
                              :rules  [{:op :for-each :collection :nrs :as :nr
                                        :steps [{:object-type :mangel :op :create
                                                 :values {:mangel-nr [:get :nr]}}]}]}
              :aendern      {:params {:ms {:type :mangel :multiple true}}
                             :rules  [{:object-type :mangel :op :update :target [:get :ms]
                                       :values {:bauteil [:get :ms :bauteil]}}
                                      {:object-type :mangel :op :create-or-update :target [:get :ms]
                                       :values {:bauteil [:get :ms :bauteil]}}]}}})]
    (is (= [[:je-element :create-from-list-param]] (map (juxt :entity :rule) ps)))
    (is (str/includes? (:message (first ps)) ":for-each") "the message names the explicit form")))

(defn- action-target-problems [schema current plan]
  (fnd/scan actions/rule-targets (diff/diff schema current plan {} (live/world-index current))))

(def ^:private with-targets
  {:objects {:arbeitskarte {:attributes [[:karten-nr :string] [:stand :string]]
                            :plural "Arbeitskarten"}
             :nacharbeit        {:attributes [[:nacharbeit-id :string] [:karten-id :string]]
                                 :plural "Nacharbeiten"}}
   :actions
   {:nacharbeit-verwerfen
    {:params {:nacharbeit {:type :nacharbeit :prefill :current-object :required true}}
     :rules  [{:object-type :arbeitskarte :op :update
               :target [:get :nacharbeit :karten-id] :values {:stand "offen"}}
              {:object-type :nacharbeit :op :delete-object :target [:get :nacharbeit]}]}
    :nacharbeit-verwerfen-param
    {:params {:nacharbeit {:type :nacharbeit}
              :karte  {:type :arbeitskarte}}
     :rules  [{:object-type :arbeitskarte :op :update
               :target [:get :karte] :values {:stand "offen"}}
              {:object-type :nacharbeit :op :delete-object :target [:get :nacharbeit :id]}]}
    :stand-setzen-mehrere
    {:params {:karten {:type :arbeitskarte :multiple true}}
     :rules  [{:object-type :arbeitskarte :op :update :target :karten
               :values {:stand "offen"}}]}
    :per-schluessel
    {:params {:karten-nr {:type :string}}
     :rules  [{:object-type :arbeitskarte :op :delete-object :target [:get :karten-nr]}]}
    :per-uuid
    {:params {:karten-uuid {:type :uuid}}
     :rules  [{:object-type :arbeitskarte :op :update :target [:get :karten-uuid :id]
               :values {:stand "offen"}}]}
    :falscher-typ
    {:params {:nacharbeit {:type :nacharbeit}}
     :rules  [{:object-type :arbeitskarte :op :update :target [:get :nacharbeit]
               :values {:stand "offen"}}]}
    :verknuepfen-per-wert
    {:params {:von {:type :string} :nach {:type :arbeitskarte}}
     :rules  [{:op :create-link :link-type :x
               :from-id [:get :von] :to-id [:get :nach :id]}]}
    :anlegen-und-aendern
    {:params {}
     :rules  [{:op :create :object-type :arbeitskarte :as :neu :values {:karten-nr "n"}}
              {:op :update :object-type :arbeitskarte :target [:get :neu]
               :values {:stand "neu"}}]}}})

(deftest a-rule-target-must-be-an-object-reference-parameter
  (let [ps (action-target-problems with-targets {} [])
        by (group-by :entity ps)]
    (testing "the measured prod shape is refused, naming action, rule index and the fix"
      (let [[p & more] (by :nacharbeit-verwerfen)]
        (is (nil? more) "only rule 0 — rule 1 targets the parameter itself")
        (is (= :rule-target-not-object-param (:rule p)))
        (is (= :error (:severity p)))
        (is (= 0 (:rule-index p)))
        (is (str/includes? (:message p) "Aktion nacharbeit-verwerfen, Regel 0"))
        (is (str/includes? (:message p) "[:get :nacharbeit :karten-id]"))
        (is (str/includes? (:message p) "eigenen Objektparameter"))
        (is (str/includes? (:message p) ":type :arbeitskarte"))))
    (testing "a scalar parameter, a wrong-typed one and a scalar link endpoint are refused too"
      (is (= 1 (count (by :per-schluessel))))
      (is (= 1 (count (by :per-uuid))) "any base value type is a value, :uuid too")
      (is (= 1 (count (by :falscher-typ))))
      (is (= [:from-id] (map :slot (by :verknuepfen-per-wert)))))
    (testing "[:get :p], [:get :p :id], a bare :p, a :multiple param and an :as binding pass"
      (is (not-any? #{:nacharbeit-verwerfen-param :stand-setzen-mehrere :anlegen-und-aendern}
                    (map :entity ps))))))

(deftest an-installed-unchanged-action-is-only-warned-about
  (let [plan [{:kind :action :entity :nacharbeit-verwerfen :outcome :skipped}
              {:kind :action :entity :per-schluessel :outcome :updated}]
        sev  (into {} (map (juxt :entity :severity))
                   (action-target-problems with-targets {} plan))]
    (is (= :warn (sev :nacharbeit-verwerfen)) "already live and not touched by this push")
    (is (= :error (sev :per-schluessel)) "changed in this push")
    (is (= :error (sev :falscher-typ)) "new in this push")))

(def ^:private with-interface-links
  {:interfaces {:wartbares-teil {:property-list [[:seriennummer :string]]
                                 :link-constraints [{:link-name :eingebaut-in
                                                     :to-object-type :luftfahrzeug
                                                     :cardinality :one}]}}
   :objects {:triebwerk    {:implements [:wartbares-teil]
                            :attributes [[:seriennummer :string] [:kennzeichen :string]]
                            :plural "Triebwerke"}
             :fahrwerk     {:implements [:wartbares-teil]
                            :attributes [[:seriennummer :string] [:kennzeichen :string]]
                            :plural "Fahrwerke"}
             :luftfahrzeug {:attributes [[:kennzeichen :string]] :plural "Luftfahrzeuge"}
             :werkzeug     {:attributes [[:nr :string] [:kennzeichen :string]] :plural "Werkzeuge"}}
   :links [[:triebwerk-luftfahrzeug :triebwerk :luftfahrzeug :many-to-one
            {:fk :kennzeichen :satisfies [{:interface :wartbares-teil :constraint :eingebaut-in}]}]
           [:fahrwerk-luftfahrzeug :fahrwerk :luftfahrzeug :many-to-one
            {:fk :kennzeichen :satisfies [{:interface :wartbares-teil :constraint :eingebaut-in}]}]
           [:werkzeug-luftfahrzeug :werkzeug :luftfahrzeug :many-to-one {}]]
   :transforms
   {:triebwerk-src {:language :pg-sql :query "select 1"
                    :inputs {:a {:kind :dataset :dataset :roh}}
                    :outputs [{:kind :object-type :object-type :triebwerk
                               :primary-key :seriennummer}
                              {:kind :link-type :link-type :triebwerk-luftfahrzeug
                               :fk-attribute :kennzeichen}]}
    :fahrwerk-src  {:language :pg-sql :query "select 1"
                    :inputs {:a {:kind :dataset :dataset :roh}}
                    :outputs [{:kind :object-type :object-type :fahrwerk :primary-key :seriennummer}
                              {:kind :link-type :link-type :fahrwerk-luftfahrzeug
                               :fk-attribute :kennzeichen}]}}
   :actions
   {:teil-einbauen
    {:params {:teil {:type :wartbares-teil :required true}
              :lfz  {:type :luftfahrzeug :required true}}
     :rules  [{:op :create-link-of-interface :interface :wartbares-teil
               :constraint :eingebaut-in :from-id [:get :teil] :to-id [:get :lfz]}]}
    :werkzeug-zuordnen
    {:params {:w {:type :werkzeug :required true} :lfz {:type :luftfahrzeug :required true}}
     :rules  [{:op :create-link :link-type :werkzeug-luftfahrzeug
               :from-id [:get :w :id] :to-id [:get :lfz :id]}]}}})

(deftest an-interface-link-rule-is-refused-where-its-realisation-is-fk-derived
  (let [fs (fnd/scan actions/derived-link-writes
                     (diff/diff with-interface-links {} [] {} (live/world-index {})))]
    (testing "the rule names no link type at all, and is still caught"
      (is (= #{:teil-einbauen} (set (map :entity fs)))
          "the writable :create-link on :werkzeug-luftfahrzeug stays silent"))
    (testing "every realisation of the constraint is reported, not just the first"
      (is (= 2 (count fs)))
      (is (= #{:triebwerk-luftfahrzeug :fahrwerk-luftfahrzeug}
             (set (keep #(some->> (:message %) (re-find #"(triebwerk|fahrwerk)-luftfahrzeug")
                                  first keyword)
                        fs)))))))

