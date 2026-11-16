(ns ^:api platform.project-export-test
  (:require [support.system :as system]
            [sutra.types.value-types :as vt]
            [sutra.catalog.shape :as shape]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-emit :as pemit]
            [sutra.catalog.functions :as functions]
            [sutra.store.seat]
            [sutra.projects.http :as kompass]
            [sutra.wire.api :as api]
            [support.in-project :refer [in-project]]
            [support.pod :as pod])
  (:import [java.util.zip ZipInputStream]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private ent-path "tests/support/fixtures/werft/werft.ent")

(defn- inline-code-files [schema]
  (let [dir (.getParentFile (io/file ent-path))]
    (walk/postwalk
     (fn [x] (if (and (map? x) (:code-file x) (not (:code x)))
               (-> x (assoc :code (slurp (io/file dir (:code-file x)))) (dissoc :code-file))
               x))
     schema)))

(defn- src-schema []
  (->> (slurp ent-path) edn/read-string :schema (in-project :werft) sugar/lower-schema
       inline-code-files))

(defn- objects-fp [schema]
  (into {} (map (fn [[_ s]] [(:object-type-name s)
                             {:attrs  (:attribute-list s)
                              :symbol (:symbol s)
                              :plural (:plural s)}]))
        (:object-types schema)))

(defn- links-fp [schema]
  (let [ot (into {} (map (fn [[rid s]] [rid (:object-type-name s)])) (:object-types schema))]
    (set (map (fn [[_ s]] [(:link-type-name s)
                           (ot (:from-object-type s) (:from-object-type s))
                           (ot (:to-object-type s) (:to-object-type s))
                           (:cardinality s)])
              (:link-types schema)))))

(defn- transforms-fp [schema]
  (into {} (map (fn [[_ s]] [(:transform-name s)
                             {:language (:language s)
                              :code     (:code s)
                              :query    (:query s)}]))
        (:transforms schema)))

(deftest scoping-matches-kompass-membership
  (let [schema {:projects      {"ri.proj.alpha" {:project-name :alpha :description "A"}}
                :object-types  {"ri.ot.1" {:object-type-name :owned
                                           :attribute-list
                                             [{:attribute-name :id :attribute-type :string}]
                                           :project "ri.proj.alpha"
                                           :plural "Owneds"}
                                "ri.ot.2" {:object-type-name :foreign
                                           :attribute-list
                                             [{:attribute-name :id :attribute-type :string}]
                                           :project "ri.proj.beta"
                                           :plural "Foreigns"}
                                "ri.ot.3" {:object-type-name :loose
                                           :attribute-list
                                             [{:attribute-name :id :attribute-type :string}]
                                           :plural "Looses"}}}]
    (testing "named project keeps only its members"
      (let [objs (get-in (:ent (pemit/export schema {:project :alpha :inline? true}))
                         [:schema :objects])]
        (is (contains? objs :owned))
        (is (not (contains? objs :foreign)))
        (is (not (contains? objs :loose)) "unassigned resource is not in a named project")))
    (testing "default scope keeps the UNASSIGNED resources"
      (let [objs (get-in (:ent (pemit/export schema {:project nil :inline? true}))
                         [:schema :objects])]
        (is (contains? objs :loose))
        (is (not (contains? objs :owned)))
        (is (not (contains? objs :foreign)))))
    (testing "the project declaration is emitted for a named project"
      (is (= {:description "A"}
             (get-in (:ent (pemit/export schema {:project :alpha :inline? true}))
                     [:schema :projects :alpha]))))))

(deftest link-carries-its-satisfies-and-its-fk-backing
  (let [schema {:object-types
                {"ri.ot.doc"   {:object-type-name :doc
                                :attribute-list [{:attribute-name :id :attribute-type :string}
                                                 {:attribute-name :party-id
                                                  :attribute-type :string}]
                                :plural "Docs"}
                 "ri.ot.party" {:object-type-name :party
                                :attribute-list [{:attribute-name :id :attribute-type :string}]
                                :plural "Parties"}}
                :interfaces
                {"ri.if.beleg" {:interface-name :beleg
                                :property-list  []
                                :link-constraints [{:link-name :partner-of
                                                    :cardinality :many-to-one
                                                    :to-object-type "ri.ot.party"}]}}
                :link-types
                {"ri.lt.dp" {:link-type-name :doc-party
                             :from-object-type "ri.ot.doc"
                             :to-object-type   "ri.ot.party"
                             :cardinality      :many-to-one
                             :satisfies [{:interface "ri.if.beleg" :constraint :partner-of}]}}
                :transforms
                {"ri.tx.dp" {:transform-name :link-doc-party
                             :input  {:kind :object-type :object-type "ri.ot.doc"}
                             :output {:kind :link-type :link-type "ri.lt.dp"
                                      :fk-attribute :party-id :on-unresolved :skip}}}}
        sch   (:schema (:ent (pemit/export schema {:all-projects? true :inline? true})))
        entry (some #(when (= :doc-party (first %)) %) (:links sch))
        opts  (last entry)]

    (testing "the link is emitted at all"
      (is (some? entry))
      (is (= [:doc-party :doc :party :many-to-one] (subvec entry 0 4))
          "endpoints resolved RID→name"))

    (testing ":satisfies round-trips, with the interface as a NAME"
      (is (= [{:interface :beleg :constraint :partner-of}] (:satisfies opts))
          "interface-link membership survives the export"))

    (testing "the generated endpoint-fk producer folds back into `:fk` sugar"
      (is (= {:attribute :party-id :on-unresolved :skip} (:fk opts)))
      (is (not (contains? (:fk opts) :side)) "side is implied by the cardinality"))

    (testing "the producer is NOT ALSO emitted as a transform (that would collide)"
      (is (not (contains? (:transforms sch) :link-doc-party))))

    (testing "the emitted .ent is valid author grammar and lowers back to the producer"
      (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch)))
      (let [lowered (sugar/lower-schema sch)]
        (is (= {:kind :object-type :object-type :doc}
               (get-in lowered [:transforms :link-doc-party :input]))
            "re-lowering rebuilds the SAME producer, on the same side")
        (is (= :party-id (get-in lowered [:transforms :link-doc-party :output :fk-attribute])))))))

(deftest prose-descriptions-reflow-but-code-newlines-survive
  (let [schema {:datasets
                {"ri.ds.d" {:dataset-name :arbeitspaket :kind :standalone
                            :title "Arbeitspaket\n   (MRO, roh)"
                            :description (str "paket-code ohne Trenner, uppercase — Gegenstück zu\n"
                                              "                    :arbeitsauftrag.wo-nr-norm. "
                                              "Die\n"
                                              "                    Normalisierung stand 9× in den "
                                              "Queries.")
                            :columns [{:attribute-name :a :attribute-type :integer}]}}
                :transforms
                {"ri.tx.q" {:transform-name :q :language :pg-sql
                            :query "SELECT 1\n  FROM t\n WHERE x = 2"
                            :inputs {} :output {:kind :dataset :dataset "ri.ds.d"}}}}
        sch   (:schema (:ent (pemit/export schema {:all-projects? true :inline? true})))
        d     (get-in sch [:datasets :arbeitspaket])]
    (testing "a multi-line :description collapses to one line of prose, words intact"
      (is
       (= "paket-code ohne Trenner, uppercase — Gegenstück zu :arbeitsauftrag.wo-nr-norm. Die Normalisierung stand 9× in den Queries."
          (:description d)))
      (is (not (re-find #"\n" (:description d))) "no newline survives in prose"))
    (testing ":title reflows too"
      (is (= "Arbeitspaket (MRO, roh)" (:title d))))
    (testing "a transform :query keeps its newlines — layout there is meaning"
      (is (= "SELECT 1\n  FROM t\n WHERE x = 2"
             (get-in sch [:transforms :q :query]))))))

(deftest datasource-sync-round-trips-as-external-source
  (let [schema {:datasets
                {"ri.ds.lief" {:dataset-name :lieferant_raw :kind :standalone
                               :columns [{:attribute-name :adr_id :attribute-type :integer}]}}
                :object-types
                {"ri.ot.mit" {:object-type-name :mitarbeiter
                              :attribute-list [{:attribute-name :pnr :attribute-type :integer}]
                              :plural "Mitarbeiter"}}
                :transforms
                {"ri.tx.lief" {:transform-name :lieferant_raw-sync
                               :input  {:kind :datasource :datasource :mro}
                               :source {:query "SELECT a.ADR_ID AS adr_id FROM ADR_STAMM a"}
                               :primary-key :adr_id
                               :output {:kind :dataset :dataset "ri.ds.lief"}}
                 "ri.tx.mit"  {:transform-name :mitarbeiter-rows
                               :input  {:kind :datasource :datasource :mro}
                               :source {:table "MITARBEITER"}
                               :primary-key :pnr :column-map {:PNR :pnr}
                               :output {:kind :object-type :object-type "ri.ot.mit"}}}}
        sch   (:schema (:ent (pemit/export schema {:all-projects? true :inline? true})))]

    (testing "a dataset sync un-lifts to the authorable :external-source"
      (is (= {:strategy :simple :datasource :mro :primary-key :adr_id
              :query "SELECT a.ADR_ID AS adr_id FROM ADR_STAMM a"}
             (get-in sch [:datasets :lieferant_raw :external-source]))))

    (testing "an object-type sync un-lifts too, carrying its :column-map + :table"
      (is (= {:strategy :simple :datasource :mro :primary-key :pnr
              :column-map {:PNR :pnr} :table "MITARBEITER"}
             (get-in sch [:objects :mitarbeiter :external-source]))))

    (testing "the lifted producers are NOT ALSO emitted as transforms"
      (is (not (contains? (:transforms sch) :lieferant_raw-sync)))
      (is (not (contains? (:transforms sch) :mitarbeiter-rows))))

    (testing "the emitted .ent is valid grammar and re-installs to the SAME producers"
      (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch)))
      (let [re (-> sch sugar/lower-schema shape/unpack-schema
                   shape/migrate-external-source-to-transforms)]
        (is (= {:kind :dataset :dataset :lieferant_raw}
               (get-in re [:transforms :lieferant_raw-sync :output])))
        (is (= "SELECT a.ADR_ID AS adr_id FROM ADR_STAMM a"
               (get-in re [:transforms :lieferant_raw-sync :source :query])))
        (is (= {:kind :object-type :object-type :mitarbeiter}
               (get-in re [:transforms :mitarbeiter-rows :output])))
        (is (= {:PNR :pnr} (get-in re [:transforms :mitarbeiter-rows :column-map]))
            "the column-map survives the round trip")
        (is (nil? (get-in re [:datasets :lieferant_raw :external-source]))
            "re-install re-strips :external-source off the dataset (lifted, not stored)")))))

(deftest output-columns-round-trip-as-authored-tuples
  (let [schema {:transforms
                {"ri.tx.z" {:transform-name :zwischen
                            :language :pg-sql
                            :query   "SELECT 1 AS key, 2 AS betrag"
                            :inputs  {:in {:kind :datasource :datasource :mro}}
                            :output-columns [{:attribute-name :key    :attribute-type :string}
                                             {:attribute-name :betrag :attribute-type :integer}]}}}
        sch   (:schema (:ent (pemit/export schema {:all-projects? true :inline? true})))]
    (testing "the projected :output-columns are AUTHORED tuples, not stored maps"
      (is (= [[:key :string] [:betrag :integer]]
             (get-in sch [:transforms :zwischen :output-columns]))))
    (testing "and the emitted schema re-lints clean (a re-push would not fail)"
      (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch))))))

(deftest werft-round-trips
  (testing "install the werft ontology"
    (is (= 200 (:status (api/post! :install {:schema (src-schema)}))) "install stages")
    (is (= 200 (:status (api/post! :staging))) "commit"))

  (let [stored1 (:body (api/get! :schema))
        {:keys [ent files skipped]} (pemit/export stored1 {:project :werft :inline? true})]

    (testing "value-types PERSIST first-class (not lowered-and-dropped)"
      (is (seq (:value-types stored1))
          (str "value-types must persist in the live schema; got: "
               (pr-str (:value-types stored1))))
      (is (contains? (set (map :value-type-name (vals (:value-types stored1))))
                     :blockzeit)
          "the :blockzeit value-type must be present by name"))

    (testing "value-type references are DURABLE + wrap-not-replace"
      (let [attrs (mapcat (fn [[_ s]] (map #(assoc % ::ot (:object-type-name s))
                                           (:attribute-list s)))
                          (:object-types stored1))
            by-vt (fn [vt] (filter #(= vt (get-in % [:opts :value-type])) attrs))]
        (is (seq (by-vt :blockzeit)) ":blockzeit value-type ref persists on some attribute")
        (is (every? #(= :decimal (:attribute-type %)) (by-vt :blockzeit))
            "wrap-not-replace: the field type is the value-type's BASE type, frozen on the
             attr — a base type, so bare :decimal; the width belongs to the producing
             transform's :output-columns, not to the property")
        (is (seq (by-vt :flug-kategorie)) ":flug-kategorie ref persists")
        (is (seq (by-vt :import-status)) ":import-status ref persists")
        (is (every? #(nil? (get-in % [:opts :constraints :one-of])) (by-vt :flug-kategorie))
            "value-type :one-of is NOT spliced onto the attribute (resolves live)")
        (is (every? #(nil? (get-in % [:opts :description])) (by-vt :blockzeit))
            "the value-type's :description is NOT spliced either — `[:blockzeit :blockzeit]`
             is authored with no opts, so the stored attribute carries none")))

    (testing "the live resolver hands the value-type's payload back"
      (let [attrs (get-in stored1 [:object-types
                                   (some (fn [[k s]] (when (= :monatswert (:object-type-name s)) k))
                                         (:object-types stored1))
                                   :attribute-list])
            eff   (vt/resolve-value-types attrs (:value-types stored1))
            by-nm (fn [nm] (first (filter #(= nm (:attribute-name %)) eff)))]
        (is (= "Blockzeit in Stunden (Monatssumme eines Luftfahrzeugs)."
               (get-in (by-nm :blockzeit) [:opts :description]))
            ":description resolves from the registry, not from a frozen copy")
        (is (= #{"Linienflug" "Charter" "Überführung" "Testflug" "Schulung"}
               (set (get-in (by-nm :kategorie) [:opts :constraints :one-of])))
            ":constraints resolve through the same door")
        (is (= "Denormalisiert von :luftfahrzeug — das Diagramm braucht keinen Join."
               (get-in (by-nm :kategorie) [:opts :description]))
            "the attribute's OWN :description still wins over the value-type's")))

    (testing "the emitted .ent RE-LIFTS the value-type reference into the type slot"
      (let [objs  (get-in ent [:schema :objects])
            attr  (fn [ot nm] (first (filter #(= nm (first %))
                                             (get-in objs [ot :attributes]))))]
        (is (= [:blockzeit :blockzeit] (attr :monatswert :blockzeit))
            "type slot names the value-type; the SPLICED :description and the
             :value-type opt are gone — the definition lives in :value-types")
        (is (= :flug-kategorie (second (attr :monatswert :kategorie)))
            "a ref with the author's OWN :description keeps it, in the type slot")
        (is (= {:description "Denormalisiert von :luftfahrzeug — das Diagramm braucht keinen Join."}
               (nth (attr :monatswert :kategorie) 2))
            "…and carries nothing else — no leftover :value-type opt")))

    (testing "the emitted .ent is STRUCTURALLY VALID author grammar"
      (is (nil? (ent/explain-schema (:schema ent)))
          (str "emitted schema must validate; errors: " (ent/explain-schema (:schema ent)))))

    (testing "inline mode carries code IN the .ent, no sibling files"
      (is (empty? files) "no external files in inline mode")
      (let [px (get-in ent [:schema :transforms :parse-flugbuch])]
        (is (= :python (:language px)))
        (is (re-find #"def " (:code px)) "python code inlined")))

    (testing "no sources leaked; every code transform emitted"
      (is (= 2 (:transforms skipped))
          "only the 2 FK-backing link producers are non-code (folded into :fk)")
      (is (zero? (:sources skipped)) "werft declares no egress sources")
      (let [fk-of (fn [nm] (some #(when (= nm (first %)) (:fk (last %)))
                                 (get-in ent [:schema :links])))]
        (is (every? some? [(fk-of :wert-luftfahrzeug) (fk-of :wert-import)])
            "both endpoint-fk backings come back as `:fk` on their links")))

    (testing "re-installing the emitted .ent is a faithful, idempotent round-trip"
      (is (= 200 (:status (api/post! :install ent))) "emitted .ent re-installs")
      (is (= 200 (:status (api/post! :staging))) "re-install commits")
      (let [stored2 (:body (api/get! :schema))]
        (is (= (objects-fp stored1) (objects-fp stored2)) "object-types + attributes preserved")
        (is (= (links-fp stored1) (links-fp stored2)) "links preserved")
        (is (= (transforms-fp stored1) (transforms-fp stored2)) "transforms + code preserved")))

    (testing "bundle mode emits :code-file refs + sibling .py files"
      (let [{ent2 :ent files :files} (pemit/export stored1 {:project :werft :inline? false})
            px (get-in ent2 [:schema :transforms :parse-flugbuch])]
        (is (= "transforms/parse_flugbuch.py" (:code-file px)))
        (is (nil? (:code px)) "code lives in the sibling file, not inline")
        (is (some #(= "transforms/parse_flugbuch.py" (:path %)) files) "py file present")
        (is (every? #(and (:path %) (string? (:content %))) files) "each file has path+content")))

    (testing "HTTP handler: .ent download (attachment, valid EDN body)"
      (let [resp (kompass/kompass-export {:query-params {"format" "ent" "project" "werft"}
                                      :sutra/schema (system/seated-schema)})
            body (slurp (:body resp))]
        (is (= 200 (:status resp)))
        (is (= "application/edn; charset=utf-8" (get-in resp [:headers "Content-Type"])))
        (is (re-find #"attachment; filename=\"werft.ent\""
                     (get-in resp [:headers "Content-Disposition"])))
        (is (map? (edn/read-string body)) "body is a readable .ent map")
        (is (get-in (edn/read-string body) [:schema :objects :luftfahrzeug])
            "carries the object model")))

    (testing "HTTP handler: .zip download contains the .ent + code files"
      (let [resp  (kompass/kompass-export {:query-params {"format" "zip" "project" "werft"}
                                       :sutra/schema (system/seated-schema)})
            names (loop [zis (ZipInputStream. (:body resp)) acc []]
                    (if-let [e (.getNextEntry zis)] (recur zis (conj acc (.getName e))) acc))]
        (is (= 200 (:status resp)))
        (is (= "application/zip" (get-in resp [:headers "Content-Type"])))
        (is (some #(= "werft.ent" %) names) "zip has the project's root .ent")
        (is (some #(= "transforms/parse_flugbuch.py" %) names) "zip has the python code file")))))

(defn- project-rid-of [schema pname]
  (some (fn [[rid s]] (when (= pname (:project-name s)) rid)) (:projects schema)))

(defn- owner-of [schema section rid-of-name nm]
  (some (fn [[_ s]] (when (= nm (rid-of-name s)) (:project s))) (get schema section)))

(deftest project-ownership-round-trips
  (testing "create the owning project (install does NOT stage :projects)"
    (is (= 200
           (:status (api/mutate :create-project {:name :acme :description "Acme"} {:stage? true}))))
    (is (= 200 (:status (api/post! :staging)))))

  (testing "install resources that DECLARE :project :acme"
    (let [schema {:interfaces {:thing {:property-list [[:id :string]] :project :acme}}
                  :objects    {:widget {:primary-key :id
                                        :attributes [[:id :string] [:label :string]]
                                        :implements [:thing] :project :acme
                                        :plural "Widgets"}
                               :gadget {:primary-key :id :attributes [[:id :string]] :project :acme
                                        :plural "Gadgets"}}
                  :links      [[:widget-gadget :widget :gadget :many-to-many {:project :acme}]]
                  :transforms {:link-widget-gadget
                               {:language :pg-sql
                                :query    "SELECT w.id AS w_id, g.id AS g_id FROM widget w JOIN gadget g ON w.id = g.id"
                                :inputs   {:widget {:kind :object-type :object-type :widget}
                                           :gadget {:kind :object-type :object-type :gadget}}
                                :output   {:kind :link-type :link-type :widget-gadget
                                           :from-key-column :w_id :to-key-column :g_id}
                                :project  :acme}}}]
      (is (= 200 (:status (api/post! :install {:schema schema}))) "authored :project installs")
      (is (= 200 (:status (api/post! :staging))))))

  (let [stored1 (:body (api/get! :schema))
        acme    (project-rid-of stored1 :acme)]
    (testing "LAYER 1 — install persisted :project (was silently dropped before the fix)"
      (is (some? acme) "acme project exists")
      (is (= acme (owner-of stored1 :object-types :object-type-name :widget)) "object-type owned")
      (is (= acme (owner-of stored1 :object-types :object-type-name :gadget)) "second type owned")
      (is (= acme (owner-of stored1 :transforms  :transform-name   :link-widget-gadget))
          "transform owned")
      (is (= acme (owner-of stored1 :link-types  :link-type-name   :widget-gadget)) "link owned"))

    (let [{:keys [ent]} (pemit/export stored1 {:project :acme :inline? true})
          sch (:schema ent)]
      (testing "LAYER 2 — export re-emits :project on every kind"
        (is (= :acme (get-in sch [:objects :widget :project])) "object carries owner")
        (is (= [:thing] (get-in sch [:objects :widget :implements]))
            "implements resolved to interface NAME, not RID (whole-schema re-install fidelity)")
        (is (= :acme (get-in sch [:interfaces :thing :project])) "interface carries owner")
        (is (= :acme (get-in sch [:transforms :link-widget-gadget :project]))
            "transform carries owner")
        (is (= :acme
               (some (fn [l] (when (= :widget-gadget (first l)) (:project (last l)))) (:links sch)))
            "link carries owner in its opts map")
        (is (contains? (:projects sch) :acme) "project container emitted"))

      (testing "emitted .ent validates the (now :project-aware) author grammar"
        (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch))))

      (testing "LAYER 3 — re-install preserves ownership (full round-trip)"
        (is (= 200 (:status (api/post! :install ent))))
        (is (= 200 (:status (api/post! :staging))))
        (let [stored2 (:body (api/get! :schema))
              acme2   (project-rid-of stored2 :acme)]
          (is (= acme2 (owner-of stored2 :object-types :object-type-name :widget)))
          (is (= acme2 (owner-of stored2 :transforms  :transform-name   :link-widget-gadget)))
          (is (= acme2 (owner-of stored2 :link-types  :link-type-name   :widget-gadget))))))

    (testing "rename-project is RID-stable — ownership survives the rename"
      (let [rid-before (project-rid-of (:body (api/get! :schema)) :acme)]
        (is (= 200 (:status (api/rename :project :acme :acme-renamed))))
        (let [after (:body (api/get! :schema))]
          (is (nil? (project-rid-of after :acme)) "old name gone")
          (is (= rid-before (project-rid-of after :acme-renamed)) "same RID under the new name")
          (is (= rid-before (owner-of after :object-types :object-type-name :widget))
              "widget still owned by the same project RID after rename"))))))

(deftest generated-link-producer-inherits-the-links-owner
  (testing "create the owning project"
    (is (= 200
           (:status (api/mutate :create-project {:name :sugar :description "Sugar"}
                                {:stage? true}))))
    (is (= 200 (:status (api/post! :staging)))))

  (testing "the LOWERING stamps the link's owner onto BOTH generated backings"
    (let [tx (fn [card opts] (-> {:links [[:l :a :b card opts]]}
                                 sugar/lower-schema :transforms :link-l))]
      (is (= :sugar (:project (tx :many-to-one {:fk :b-id :project :sugar})))
          ":fk producer inherits the link's project")
      (is (= :sugar
             (:project (tx :many-to-many
                           {:join {:on [:x :y] :from-pk :i :to-pk :j} :project :sugar})))
          ":join producer inherits the link's project")
      (is (nil? (:project (tx :many-to-one {:fk :b-id})))
          "an ownerless link yields an ownerless producer — no invented owner")))

  (testing "install a link whose producer is GENERATED from :join sugar"
    (let [authored {:objects {:bolt {:primary-key :id :attributes [[:id :string] [:box-ref :string]]
                                     :project :sugar
                                     :plural "Bolts"}
                              :box  {:primary-key :id :attributes [[:id :string]] :project :sugar
                                     :plural "Boxes"}}
                    :links   [[:bolt-box :bolt :box :many-to-many
                               {:join {:on [:box-ref :id] :from-pk :id :to-pk :id}
                                :project :sugar}]]}
          schema   (sugar/lower-schema authored)]
      (is (contains? (:transforms schema) :link-bolt-box) "lowering generated the producer")
      (is (= 200 (:status (api/post! :install {:schema schema}))) "sugar-backed link installs")
      (is (= 200 (:status (api/post! :staging))))))

  (let [stored (:body (api/get! :schema))
        sugar  (project-rid-of stored :sugar)]
    (testing "LAYER 1 — the GENERATED producer is owned, not orphaned"
      (is (some? sugar) "sugar project exists")
      (is (= sugar (owner-of stored :transforms :transform-name :link-bolt-box))
          "generated producer inherited the link's project")
      (is (= sugar (owner-of stored :link-types :link-type-name :bolt-box))))

    (testing "LAYER 2 — the producer appears in its project's export"
      (let [{:keys [ent]} (pemit/export stored {:project :sugar :inline? true})
            sch (:schema ent)]
        (is (contains? (:transforms sch) :link-bolt-box)
            "generated producer present in the project export")
        (is (= :sugar (get-in sch [:transforms :link-bolt-box :project])))
        (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch)))))))

(deftest query-function-vs-build-transform-round-trips
  (testing "install a build transform (:transforms) + a query function (:functions)"
    (let [schema {:objects {:qfxthing   {:primary-key :id
                                         :attributes [[:id :string] [:label :string]]
                                         :plural "Qfxthings"}
                            :qfxout     {:attributes [[:id :string]] :plural "Qfxouts"}}
                  :transforms {:qfx-build
                               {:language :pg-sql
                                :query    "SELECT id FROM qfxthing"
                                :inputs   {:w {:kind :object-type :object-type :qfxthing}}
                                :output   {:kind :object-type :object-type :qfxout}
                                :primary-key :id}}
                  :functions {:qfx-query
                              {:language :python
                               :code     (str "from functions.api import function\n\n"
                                              "@function\n"
                                              "def compute(w):\n    return w\n")
                               :inputs   {:w {:kind :object-type :object-type :qfxthing}}}}}]
      (let [r (api/post! :install {:schema (in-project :qfx schema)})]
        (is (= 200 (:status r)) (str "installs: " (pr-str (:body r)))))
      (is (= 200 (:status (api/post! :staging))))))

  (let [stored  (:body (api/get! :schema))
        by-name (fn [tname] (some #(when (= tname (:transform-name %)) %)
                                  (vals (:transforms stored))))]
    (testing "both landed (guards the rest against silent no-op installs)"
      (is (some? (by-name :qfx-build)) "build transform stored")
      (is (some? (by-name :qfx-query)) "query function stored (in :transforms)"))

    (testing "the query function is STAMPED :function-name; the build transform is not"
      (is (= :qfx-query (:function-name (by-name :qfx-query))) "query fn stamped")
      (is (nil? (:function-name (by-name :qfx-build))) "build transform NOT stamped"))

    (testing "the read seam (decl) splits query functions from build transforms"
      (is (functions/query-function? (by-name :qfx-query)))
      (is (not (functions/query-function? (by-name :qfx-build))))
      (is (contains? (set (map :function-name (vals (functions/query-functions stored)))) :qfx-query))
      (is (contains? (set (map :transform-name (vals (functions/build-transforms stored)))) :qfx-build))
      (is (not (contains? (set (map :transform-name (vals (functions/build-transforms stored))))
                          :qfx-query))
          "query fn is NOT a build transform")
      (is (= #{:query} (->> (functions/all-functions stored)
                            (filter #(= :qfx-query (:name %))) (map :kind) set))))

    (testing "export re-emits the query function as :functions, the build transform as :transforms"
      (let [sch (:schema (:ent (pemit/export stored {:project :qfx :inline? true})))]
        (is (contains? (:functions sch) :qfx-query) "query fn round-trips as :functions")
        (is (not (contains? (:transforms sch) :qfx-query)) "query fn is NOT under :transforms")
        (is (contains? (:transforms sch) :qfx-build) "build transform stays under :transforms")
        (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch)))))))

(deftest sql-transform-exports-the-key-it-mints
  (testing "install a sql transform that names its key on the transform"
    (let [schema {:objects {:pkxsrc {:primary-key :id :attributes [[:id :string] [:label :string]]
                                     :plural "Pkxsrcs"}
                            :pkxout {:attributes [[:pkx-id :string] [:label :string]]
                                     :plural "Pkxouts"}}
                  :transforms {:pkx-shape
                               {:language    :pg-sql
                                :query       "SELECT id AS pkx_id, label FROM pkxsrc"
                                :inputs      {:pkxsrc {:kind :object-type :object-type :pkxsrc}}
                                :output      {:kind :object-type :object-type :pkxout}
                                :primary-key :pkx-id}}}]
      (is (= 200 (:status (api/post! :install {:schema (in-project :pkx schema)}))) "installs")
      (is (= 200 (:status (api/post! :staging))))))

  (let [stored (:body (api/get! :schema))
        spec   (some #(when (= :pkx-shape (:transform-name %)) %) (vals (:transforms stored)))]
    (testing "the instance stores the key on the transform (guards the rest)"
      (is (some? spec) "transform stored")
      (is (= :pkx-id (:primary-key spec)) "stored beside :output, not inside it")
      (is (nil? (:primary-key (:output spec))) "and NOT lifted into :output"))

    (testing "the emitted .ent names it — the only place a fresh instance can read it"
      (let [sch (:schema (:ent (pemit/export stored {:project :pkx :inline? true})))
            tx  (:pkx-shape (:transforms sch))]
        (is (some? tx) "sql transform emitted")
        (is (= :pkx-id (:primary-key tx))
            "exported entry names the key; without it the tree reinstalls a keyless object transform")
        (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch)))
        (testing "and it survives the author door, so a fresh install would carry it"
          (is (= :pkx-id
                 (get-in (sugar/lower-schema sch) [:transforms :pkx-shape :primary-key]))))))))

(deftest language-less-producer-round-trips
  (let [schema {:object-types
                {"ri.ot.zb" {:object-type-name :zeitbuchung
                             :attribute-list
                               [{:attribute-name :buchung-id :attribute-type :string}]
                             :plural "Zeitbuchungen"}
                 "ri.ot.mi" {:object-type-name :mitarbeiter
                             :attribute-list [{:attribute-name :id :attribute-type :string}]
                             :plural "Mitarbeiter"}}
                :datasets
                {"ri.ds.zb" {:dataset-name :zeitbuchung_raw}}
                :link-types
                {"ri.lt.zm" {:link-type-name :zeit-mitarbeiter
                             :from-object-type "ri.ot.zb"
                             :to-object-type   "ri.ot.mi"
                             :cardinality      :many-to-one}}
                :transforms
                {"ri.tx.zb"
                 {:transform-name :shape-zeitbuchung
                  :input {:kind :dataset :dataset "ri.ds.zb"}
                  :output {:kind :object-type :object-type "ri.ot.zb"}
                  :primary-key :buchung-id}
                 "ri.tx.conn"
                 {:transform-name :zeit-mitarbeiter-conn-rows
                  :input {:kind :object-type :object-type "ri.ot.zb"}
                  :output {:kind :link-type :link-type "ri.lt.zm"
                           :fk-attribute :mitarbeiter-id}}
                 "ri.tx.join"
                 {:transform-name :zeit-mitarbeiter-conn
                  :input {:kind :datasource :datasource :mro}
                  :source {:query "SELECT a AS from_id, b AS to_id FROM t"}
                  :from-key-column :from_id
                  :to-key-column :to_id
                  :output {:kind :link-type :link-type "ri.lt.zm"}}}}
        sch   (:schema (:ent (pemit/export schema {:all-projects? true :inline? true})))
        txs   (:transforms sch)]

    (testing "der sprachlose Shaper steht im Artefakt"
      (is (contains? txs :shape-zeitbuchung))
      (is (= {:kind :object-type :object-type :zeitbuchung}
             (:output (:shape-zeitbuchung txs))))
      (is (= :buchung-id (:primary-key (:shape-zeitbuchung txs))))
      (is (= {:kind :dataset :dataset :zeitbuchung_raw}
             (:input (:shape-zeitbuchung txs)))
          "die Verdrahtung faehrt mit — sonst restauriert die Datei einen Typ ohne Quelle")
      (is (not (contains? (:shape-zeitbuchung txs) :inputs))
          "und kein leeres :inputs daneben")
      (is (not (contains? (:shape-zeitbuchung txs) :language))
          "kein erfundenes :language — sprachlos bleibt sprachlos")
      (is (not (contains? (:shape-zeitbuchung txs) :code-file))
          "kein Zeiger auf eine Datei, die es nicht gibt"))

    (testing "ein anders benannter FK-Produzent faellt nicht zwischen die Stuehle"
      (is (contains? txs :zeit-mitarbeiter-conn-rows)
          "der Zucker traegt nur `link-<name>` — dieser hier braucht seinen Eintrag")
      (is (nil? (:fk (some #(when (= :zeit-mitarbeiter (first %)) (last %)) (:links sch))))
          "und der Link traegt ihn folglich NICHT als :fk"))

    (testing "ein sprachloser Produzent MIT Rumpf verliert ihn nicht"
      (let [t (:zeit-mitarbeiter-conn txs)]
        (is (= "SELECT a AS from_id, b AS to_id FROM t" (:query (:input t)))
            "sonst steht die Verdrahtung da und der Rumpf ist weg — vollstaendig aussehend und leer")
        (is (not (contains? t :source))
            "`:source` ist Install-Form; die Tuer weist den Schluessel ab")
        (is (= [:from_id :to_id] [(:from-key-column t) (:to-key-column t)])
            "ohne Schluesselspalten installiert der Link-Produzent nicht")))

    (testing "das Ergebnis ist gueltige Autorengrammatik und lowert auf dieselben Produzenten"
      (is (nil? (ent/explain-schema sch)) (str "errors: " (ent/explain-schema sch)))
      (let [lowered (sugar/lower-schema sch)]
        (is (= :buchung-id (get-in lowered [:transforms :shape-zeitbuchung :primary-key])))
        (is (= {:kind :object-type :object-type :zeitbuchung}
               (get-in lowered [:transforms :shape-zeitbuchung :output])))
        (is (= {:kind :dataset :dataset :zeitbuchung_raw}
               (get-in lowered [:transforms :shape-zeitbuchung :input])))
        (is (= :mitarbeiter-id
               (get-in lowered
                       [:transforms :zeit-mitarbeiter-conn-rows :output :fk-attribute])))))))
