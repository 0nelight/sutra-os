(ns sutra-ui.object-type-form-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.forms.external :as ext-forms]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.forms.property :as prop-forms]))

(def ^:private konto
  {:object-type-name :bankkonto
   :id "ri.sutra.main.object-type.0cb589fd"
   :dataset :bankkonto
   :symbol "🏦"
   :plural "Bankkonten"
   :display-name "Bankkonto"
   :description "Ein Konto bei einer Bank."
   :aliases ["Girokonto" "Konto"]
   :visibility :prominent
   :title-key :bezeichnung
   :status :experimental
   :implements [:geldkonto]
   :resolve-entity true
   :match-keys [:iban]
   :survivorship {:bezeichnung :latest}
   :attribute-list
   [{:attribute-name :bankkonto-id :attribute-type :string}
    {:attribute-name :bezeichnung :attribute-type :string
     :opts {:name "Bezeichnung" :render-hints [:searchable :sortable] :visibility :prominent}}
    {:attribute-name :art :attribute-type :string
     :opts {:constraints {:one-of ["giro" "spar"]}}}
    {:attribute-name :iban :attribute-type :string :opts {:value-type :iban-nr}}
    {:attribute-name :tags :attribute-type [:array :string]}
    {:attribute-name :saldo :attribute-type :decimal
     :opts {:description "Summe der Bewegungen"
            :format {:kind :currency :currency "EUR" :decimals 2 :grouping true}
            :derived {:expression [:connected-sum :bewegungen :betrag]}
            :conditional-format [{:when [:< :saldo 0] :color "red"}]}}]})

(deftest a-loaded-type-saves-back-unchanged
  (is (nil? (ontology/object-form-round-trip konto)))
  (testing "a type without any optional key, too"
    (is (nil? (ontology/object-form-round-trip
               {:object-type-name :notiz :plural "Notizen"
                :attribute-list [{:attribute-name :notiz-id :attribute-type :string}]})))))

(deftest property-opts-travel-with-the-field
  (let [form (ontology/object-type->form konto)
        saldo (some #(when (= "saldo" (:name %)) %) (:fields form))]
    (is (= :connected-sum (get-in saldo [:opts :derived :expression 0])))
    (is (= [:array :string] (:type (some #(when (= "tags" (:name %)) %) (:fields form))))
        "a composite type stays data")
    (testing "an edited display name lands beside the untouched options"
      (let [spec (ontology/->object-type-spec
                  (assoc-in form [:fields 5 :opts :name] "Saldo"))
            [_ t opts] (nth (:attributes spec) 5)]
        (is (= :decimal t))
        (is (= "Saldo" (:name opts)))
        (is (= {:kind :currency :currency "EUR" :decimals 2 :grouping true} (:format opts)))))
    (testing "an emptied input removes its option instead of storing a blank"
      (is (= [:bankkonto-id :string]
             (first (:attributes (ontology/->object-type-spec
                                  (assoc-in form [:fields 0 :opts]
                                            {:name "" :description nil})))))))))

(deftest unticking-reaches-the-server
  (let [form (-> (ontology/object-type->form konto)
                 (assoc :implements [] :resolve-entity false :match-keys []))
        spec (ontology/->object-type-spec (assoc form :whole? true))]
    (is (= [] (:implements spec)) "an empty list is stated, so the kernel can empty it")
    (is (not (contains? spec :resolve-entity)))
    (is (not (contains? spec :match-keys)))
    (is (true? (:whole-declaration? spec)) "the identity axis it omits is retracted")
    (is (= {:bezeichnung :latest} (:survivorship spec)) "what the form does not edit is restated")))

(deftest the-form-names-the-mandatory-fields
  (let [ok {:name "fahrzeug" :plural "Fahrzeuge" :title "kennzeichen" :backing-mode :in-system
            :fields [{:name "kennzeichen" :type "string"}]}]
    (is (nil? (object-forms/object-form-problem ok true)))
    (is (= :refusal.scaffold/object-type-needs-title
           (object-forms/object-form-problem (dissoc ok :title) true)))
    (is (nil? (object-forms/object-form-problem (dissoc ok :title) false))
        "an installed type without a title stays editable")
    (is (= :refusal.scaffold/object-type-needs-property
           (object-forms/object-form-problem (assoc ok :fields [] :title nil) true)))
    (is (= :refusal.scaffold/object-type-id-invalid
           (object-forms/object-form-problem (assoc ok :name "Fahrzeug") true)))
    (is (= :refusal.scaffold/property-id-invalid
           (object-forms/object-form-problem
            (update ok :fields conj {:name "1x" :type "string"}) true)))
    (is (= :refusal.scaffold/property-ids-unique
           (object-forms/object-form-problem
            (update ok :fields conj {:name "kennzeichen" :type "integer"})
            true)))
    (is (= :refusal.scaffold/title-not-a-property
           (object-forms/object-form-problem (assoc ok :title "marke") false)))
    (is (= :refusal.scaffold/plural-required
           (object-forms/object-form-problem (assoc ok :plural " ") false)))))

(deftest title-keys-follow-the-type-table
  (is (prop-forms/title-key-type? "string"))
  (is (prop-forms/title-key-type? "decimal"))
  (is (prop-forms/title-key-type? [:array :string]))
  (is (not (prop-forms/title-key-type? "media-reference")))
  (is (not (prop-forms/title-key-type? "geoshape")))
  (is (not (prop-forms/title-key-type? [:struct {:a :string}]))))

(deftest an-interface-names-what-it-still-needs
  (is (= [{:attribute-name :waehrung :attribute-type :string}]
         (prop-forms/missing-interface-properties
          [{:name "iban" :type "string"} {:name "bezeichnung" :type "string"}]
          [{:attribute-name :bezeichnung :attribute-type :string}
           {:attribute-name :iban :attribute-type :string}
           {:attribute-name :waehrung :attribute-type :string}
           {:attribute-name :waehrung :attribute-type :string}]))))

(deftest an-untouched-mapping-goes-back-as-loaded
  (let [es   {:datasource :erp :table "KONTEN" :primary-key :id :column-map {:id :ID}}
        form {:name "konto" :plural "Konten" :fields [{:name "id" :type "string"}]
              :backing-mode :synced :external-orig es
              :external (ext-forms/ext-source->form-external es) :extra {}}]
    (is (= es (:external-source (object-forms/object-form->spec form))))
    (is (true? (:whole? (object-forms/object-form->spec form))))))

(def ^:private with-links
  {:interfaces {:besitz {:interface-name :besitz
                         :link-constraints [{:link-name :eigentuemer :to-interface :partei
                                             :cardinality :one :required true}]}
                :partei {:interface-name :partei}}
   :object-types {:person {:object-type-name :person :implements [:partei]}}
   :link-types {:fahrzeug-eigentuemer {:link-type-name :fahrzeug-eigentuemer
                                       :satisfies [{:interface :besitz :constraint :eigentuemer}]
                                       :from-object-type :fahrzeug :to-object-type :person
                                       :cardinality :many-to-one}}})

(deftest an-interface-names-the-links-it-still-needs
  (is (= [] (prop-forms/unmet-link-constraints with-links :fahrzeug [:besitz]))
      "a link to a type implementing the target interface meets it")
  (is (= [{:interface :besitz :link-name :eigentuemer :to :partei :cardinality :one}]
         (prop-forms/unmet-link-constraints with-links :werkzeug [:besitz]))
      "the commit gate would refuse this — the form says so first")
  (is (= :ui/ot-interface-links-unmet
         (object-forms/object-form-problem
          {:name "werkzeug" :plural "Werkzeuge" :backing-mode :in-system}
          false [{:interface :besitz}]))))

(deftest a-pipeline-fed-type-names-its-transforms
  (let [was @catalog/canonical-schema]
    (reset! catalog/canonical-schema
            {:transforms {:import-buchungen {:output {:kind :object-type :object-type :bankbuchung}}
                          :aufteilen {:outputs [{:kind :object-type :object-type :bankbuchung}
                                                {:kind :dataset :dataset :rest}]}
                          :link-x {:output {:kind :link-type :link-type :x}}}})
    (is (= [:aufteilen :import-buchungen] (object-forms/feeding-transforms :bankbuchung)))
    (is (= [] (object-forms/feeding-transforms :bankkonto)))
    (reset! catalog/canonical-schema was)))

(deftest an-object-types-metadata-is-edited-in-the-form
  (let [form (ontology/object-type->form konto)]
    (is (= "Bankkonto" (:display-name form)))
    (is (= "Girokonto, Konto" (:aliases form)) "aliases are one comma-separated input")
    (testing "what the form edits goes out as the stored keys"
      (let [spec (ontology/->object-type-spec
                  (assoc form :display-name " Bankverbindung " :aliases "Giro, , Konto"
                              :visibility :hidden))]
        (is (= "Bankverbindung" (:display-name spec)))
        (is (= ["Giro" "Konto"] (:aliases spec)))
        (is (= :hidden (:visibility spec)))
        (is (= "Ein Konto bei einer Bank." (:description spec)))))
    (testing "an emptied field is left out, so the whole save retracts it"
      (let [spec (ontology/->object-type-spec
                  (assoc form :description "" :aliases "" :visibility nil :whole? true))]
        (is (not-any? #(contains? spec %) [:description :aliases :visibility]))
        (is (true? (:whole-declaration? spec)))))
    (testing "and the form's own save path keeps them"
      (is (= "Bankkonto" (:display-name (object-forms/object-form->spec form)))))))
