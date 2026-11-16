(ns ^:unit grammar.install-metadata-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.catalog.kernel :as kernel]
            [sutra.ref.install :as ri]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-emit :as pemit]))

(def ^:private authored
  {:shared-properties
   {:kosten {:type :decimal :name "Kosten" :description "Netto, ohne Steuer"
             :format {:kind :currency :currency "EUR" :decimals 2}
             :render-hints [:sortable] :visibility :prominent
             :constraints {:min -1000000}}
    :notiz  {:type :string}}
   :objects
   {:mangel {:name "Mangel" :plural "Mängel" :symbol "🛠"
               :description "Was an einem Luftfahrzeug nicht stimmt."
               :aliases ["Beanstandung" "Befund"] :visibility :prominent
               :attributes [[:mangel-nr :string] :kosten :notiz]}}})

(defn- stage
  [desired live]
  (reduce (fn [w {:keys [kind entity outcome args]}]
            (if (#{:created :updated} outcome)
              (let [tag  kind
                    spec (partial ri/spec kernel/install-cx tag)
                    sec  (:section (ri/class-by-tag tag))]
                (assoc-in w [sec (str "ri." (name tag) "." (name entity))]
                          (merge (get-in w [sec (str "ri." (name tag) "." (name entity))])
                                 (spec entity args))))
              w))
          live
          (install/plan-schema desired live {})))

(defn- stored [world section nm-field nm]
  (some (fn [[_ v]] (when (= nm (get v nm-field)) v)) (get world section)))

(defn- attr [ot nm]
  (some #(when (= nm (:attribute-name %)) %) (:attribute-list ot)))

(deftest a-shared-propertys-metadata-reaches-the-object-type-using-it
  (let [w  (stage (sugar/lower-schema authored) {})
        ot (stored w :object-types :object-type-name :mangel)
        b  (attr ot :kosten)]
    (is (= :decimal (:attribute-type b)))
    (is (= {:name "Kosten" :description "Netto, ohne Steuer"
            :format {:kind :currency :currency "EUR" :decimals 2}
            :render-hints [:sortable] :visibility :prominent
            :constraints {:min -1000000}}
           (:opts b))
        "display name, description, format, render hints, visibility, constraints")
    (is (nil? (:opts (attr ot :notiz))) "a shared property without metadata lends none")
    (testing "the shared property itself keeps it too"
      (let [sp (stored w :shared-properties :shared-property-name :kosten)]
        (is (= "Kosten" (:display-name sp)))
        (is (= {:kind :currency :currency "EUR" :decimals 2} (:format sp)))
        (is (= [:sortable] (:render-hints sp)))
        (is (= :prominent (:visibility sp)))))))

(deftest the-installed-metadata-converges
  (let [desired (sugar/lower-schema authored)
        w       (stage desired {})]
    (doseq [{:keys [phase entity outcome]} (install/plan-schema desired w {})]
      (is (= :skipped outcome) (str phase "/" entity " re-stages what it just stored")))
    (testing "editing the shared property in ONE place re-stages every user"
      (let [edited (assoc-in desired [:shared-properties :kosten :format :decimals] 0)
            plan   (install/plan-schema edited w {})
            by     (into {} (map (juxt (juxt :phase :entity) :outcome)) plan)]
        (is (= :updated (by [:shared-properties :kosten])))
        (is (= :updated (by [:object-types :mangel])))
        (is (= 0 (get-in (attr (stored (stage edited w) :object-types :object-type-name :mangel)
                               :kosten)
                         [:opts :format :decimals])))))))

(deftest an-object-types-metadata-is-stored
  (let [ot (stored (stage (sugar/lower-schema authored) {}) :object-types :object-type-name
                   :mangel)]
    (is (= "Mangel" (:display-name ot)))
    (is (= "Mängel" (:plural ot)))
    (is (= "Was an einem Luftfahrzeug nicht stimmt." (:description ot)))
    (is (= ["Beanstandung" "Befund"] (:aliases ot)))
    (is (= :prominent (:visibility ot)))))

(deftest the-export-says-what-the-file-said
  (let [w   (stage (sugar/lower-schema authored) {})
        out (get-in (pemit/export w {:all-projects? true}) [:ent :schema])]
    (testing "an object type's display name goes back out as the :name it was written as"
      (is (= "Mangel" (get-in out [:objects :mangel :name])))
      (is (not (contains? (get-in out [:objects :mangel]) :display-name)))
      (is (= ["Beanstandung" "Befund"] (get-in out [:objects :mangel :aliases]))))
    (testing "a property that IS the shared property goes back out as the bare reference"
      (is (= [[:mangel-nr :string] :kosten :notiz]
             (get-in out [:objects :mangel :attributes]))))
    (testing "the shared property keeps its metadata"
      (is (= "Kosten" (get-in out [:shared-properties :kosten :name])))
      (is (= :prominent (get-in out [:shared-properties :kosten :visibility]))))
    (testing "and the export re-installs to nothing"
      (doseq [{:keys [phase entity outcome]}
              (install/plan-schema (sugar/lower-schema out) w {})]
        (is (= :skipped outcome) (str phase "/" entity))))))

(deftest a-local-property-that-differs-stays-local-on-export
  (let [w   (stage (sugar/lower-schema
                    (assoc-in authored [:objects :mangel :attributes]
                              [[:mangel-nr :string] [:kosten :decimal {:name "Schätzung"}]]))
                   {})
        out (get-in (pemit/export w {:all-projects? true}) [:ent :schema])]
    (is (= [:kosten :decimal {:name "Schätzung"}]
           (second (get-in out [:objects :mangel :attributes]))))))

(deftest an-applications-display-name-is-stored
  (let [spec   (partial ri/spec kernel/install-cx :application)
        app    {:name :zahlungen :display-name "Zahlungen" :status :published}
        stored (spec :zahlungen app)]
    (is (= "Zahlungen" (:display-name stored)))
    (is (= :skipped (:outcome (first (install/plan-schema
                                      {:applications {:zahlungen (dissoc app :name)}}
                                      {:applications {"ri.a.1" stored}} {})))))))

(deftest a-whole-declaration-speaks-for-the-presentation-it-omits
  (let [desired (sugar/lower-schema authored)
        w       (stage desired {})]
    (testing "deleting the description from the file reaches the stored type"
      (let [edited (update-in desired [:objects :mangel] dissoc :description :aliases)
            plan   (install/plan-schema edited w {})
            ot-e   (some #(when (= [:object-types :mangel] [(:phase %) (:entity %)]) %) plan)
            step   (kernel/step nil :update :object-type (assoc (:args ot-e) :name "mangel"))
            live   (get-in w [:object-types "ri.object-type.mangel"])
            after  (:value (kernel/prepare {:object-types {"ri.object-type.mangel" live}} step))]
        (is (= :updated (:outcome ot-e)) "an omitted description is a difference")
        (is (not (contains? after :description)))
        (is (not (contains? after :aliases)))
        (is (= "Mangel" (:display-name after)) "what is still said stays")))
    (testing "a :merge declaration adds a column without retracting the presentation"
      (let [merged {:objects {:mangel {:reconcile-mode :merge
                                         :attributes [[:zusatz :string]]
                                       :plural "Mängel"}}}
            ot-e   (some #(when (= :mangel (:entity %)) %)
                         (install/plan-schema merged w {}))]
        (is (= "Mangel" (:display-name (:args ot-e))))
        (is (= ["Beanstandung" "Befund"] (:aliases (:args ot-e))))))))

(def ^:private described
  {:transforms {:summe {:description "Summiert die Beträge." :language :pg-sql
                        :query "select id from roh"
                        :inputs {:roh {:kind :dataset :dataset :roh}}
                        :output-columns [[:id :string]]}}
   :functions  {:suche  {:description "Findet Beträge." :language :python
                         :code "def f(): pass"
                         :inputs {:roh {:kind :dataset :dataset :roh}}}
                :aendern {:description "Ändert." :target-type :mangel
                          :source
                            "(defn aendern [r] (-> OntologyClient/action (.update-rechnung r {:notiz \"x\"})))"}}})

(deftest a-transforms-and-a-functions-description-survives-the-install
  (let [w   (stage (sugar/lower-schema (merge authored described)) {})
        out (get-in (pemit/export w {:all-projects? true :inline? true}) [:ent :schema])]
    (is (= "Summiert die Beträge."
           (:description (stored w :transforms :transform-name :summe))))
    (is (= "Findet Beträge." (:description (stored w :transforms :transform-name :suche))))
    (is (= "Ändert." (:description (stored w :functions :function-name :aendern))))
    (testing "the export says it again"
      (is (= "Summiert die Beträge." (get-in out [:transforms :summe :description])))
      (is (= "Findet Beträge." (get-in out [:functions :suche :description])))
      (is (= "Ändert." (get-in out [:functions :aendern :description]))))
    (testing "and re-installs to nothing"
      (doseq [{:keys [phase entity outcome]}
              (install/plan-schema (sugar/lower-schema out) w {})
              :when (#{:transforms :functions :edit-functions} phase)]
        (is (= :skipped outcome) (str phase "/" entity))))))
