(ns ^:unit grammar.breaking-schema-changes-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.migration :as migration]))

(def ^:private breaking migration/breaking-schema-changes)

(defn- schema
  ([attrs pk] (schema attrs pk :amos :bauteile-parsen))
  ([attrs pk ds producer]
   {:object-types {"rid-ot" {:object-type-name :bauteil
                             :attribute-list (mapv (fn [[n t]] {:attribute-name n
                                                                :attribute-type t}) attrs)
                             :plural "Bauteile"}}
    :transforms {producer
                 {:transform-name producer
                  :input  {:kind :datasource :datasource ds}
                  :output {:kind :object-type :object-type :bauteil :primary-key pk}}}}))

(def ^:private live (schema [[:seriennummer :string] [:benennung :string]] :seriennummer))

(deftest unconditional-breaks-need-no-edits
  (testing "a changed property TYPE breaks regardless of edits"
    (is (= [{:object-type :bauteil :change :property-type-changed
             :property :benennung :from :string :to :integer}]
           (breaking (schema [[:seriennummer :string] [:benennung :integer]] :seriennummer)
                     live #{}))))

  (testing "a changed PRIMARY KEY breaks regardless of edits"
    (is (= [{:object-type :bauteil :change :primary-key-changed
             :from [:primary-key :seriennummer] :to [:primary-key :benennung]}]
           (breaking (schema [[:seriennummer :string] [:benennung :string]] :benennung)
                     live #{})))))

(defn- resolved
  []
  (-> (schema [[:seriennummer :string] [:benennung :string]] :seriennummer)
      (assoc-in [:object-types "rid-ot" :resolve-entity] true)
      (assoc-in [:object-types "rid-ot" :match-keys] [:seriennummer])))

(deftest dropping-entity-resolution-is-a-moved-identity
  (is (= [{:object-type :bauteil :change :primary-key-changed
           :from [:entity-resolution [:seriennummer]] :to [:primary-key :seriennummer]}]
         (breaking live (resolved) #{})))

  (testing "and the reverse move is just as breaking"
    (is (= [{:object-type :bauteil :change :primary-key-changed
             :from [:primary-key :seriennummer] :to [:entity-resolution [:seriennummer]]}]
           (breaking (resolved) live #{}))))

  (testing "a match-key change on a type that IS resolved moves the rule too"
    (is (= [{:object-type :bauteil :change :primary-key-changed
             :from [:entity-resolution [:seriennummer]]
             :to   [:entity-resolution [:seriennummer :benennung]]}]
           (breaking (assoc-in (resolved) [:object-types "rid-ot" :match-keys]
                               [:seriennummer :benennung])
                     (resolved) #{}))))

  (testing "…and a match-key change on a NON-resolved type moves nothing"
    (is (= [] (breaking (assoc-in live [:object-types "rid-ot" :match-keys] [:benennung])
                        live #{})))))

(deftest a-deletion-breaks-only-where-edits-would-be-orphaned
  (let [after (schema [[:seriennummer :string]] :seriennummer)]
    (testing "no edits on the deleted property ⇒ an ordinary change"
      (is (= [] (breaking after live #{}))))

    (testing "edits on it ⇒ breaking, because they would have nowhere to land"
      (is (= [{:object-type :bauteil :change :property-deleted
               :property :benennung :from :string}]
             (breaking after live #{[:bauteil :benennung]}))))

    (testing "edits on a DIFFERENT property do not make this one breaking"
      (is (= [] (breaking after live #{[:bauteil :seriennummer]}))))))

(deftest changes-that-are-not-breaking-stay-silent
  (testing "an ADDED property is never breaking"
    (is (= [] (breaking (schema [[:seriennummer :string] [:benennung :string] [:neu :string]]
                                :seriennummer)
                        live #{[:bauteil :benennung]}))))

  (testing "an identical schema reports nothing"
    (is (= [] (breaking live live #{[:bauteil :benennung]}))))

  (testing "a type the desired schema does not mention is a RETIREMENT, not a property break"
    (is (= [] (breaking {:object-types {} :transforms {}} live #{[:bauteil :benennung]}))
        "removing a whole type is its own concern — the retire gate owns it")))

(deftest eine-umdeklaration-in-dieselbe-spalte-ist-keine-migration
  (testing ":string → :geoshape bewegt kein Byte — beide sind TEXT"
    (is (= [] (breaking (schema [[:seriennummer :string] [:benennung :geoshape]] :seriennummer)
                        live #{[:bauteil :benennung]}))
        "die einzige Migration hier ist 'drop all edits'; sie fuer eine Aenderung zu
         verlangen, die nichts verschiebt, kostet jede handgezeichnete Flaeche"))
  (testing "eine Aenderung, die die Spalte WECHSELT, bleibt brechend"
    (is (= [:property-type-changed]
           (map :change
                (breaking (schema [[:seriennummer :string] [:benennung :integer]] :seriennummer)
                          live #{[:bauteil :benennung]}))))
    (is (= [:property-type-changed]
           (map :change
                (breaking (schema [[:seriennummer :string] [:benennung :keyword]] :seriennummer)
                          live #{[:bauteil :benennung]})))
        "TEXT → VARCHAR(255) kann kuerzen, ist also keine blosse Umdeklaration")))

(deftest several-breaks-are-all-reported
  (let [after (schema [[:seriennummer :integer]] :seriennummer)
        r     (breaking after live #{[:bauteil :benennung]})]
    (is (= 2 (count r)))
    (is (= #{:property-type-changed :property-deleted} (set (map :change r)))
        "a caller has to be able to answer for every one of them, not just the first")))

(deftest a-changed-input-datasource-breaks
  (testing "same producer, different datasource"
    (let [r (breaking (schema [[:seriennummer :string] [:benennung :string]] :seriennummer
                              :amos-neu :bauteile-parsen)
                      live #{})]
      (is (= [:backing-changed] (map :change r)))
      (is (= :amos     (get-in (first r) [:from :datasource])))
      (is (= :amos-neu (get-in (first r) [:to :datasource])))))

  (testing "a different PRODUCER for the same type is the same class of break"
    (is (= [:backing-changed]
           (map :change (breaking (schema [[:seriennummer :string] [:benennung :string]]
                                          :seriennummer :amos :bauteile-parsen-v2)
                                  live #{}))))))

(def ^:private with-struct
  (schema [[:seriennummer :string]
           [:einsatzort [:struct {:position :string :station :string}]]]
          :seriennummer))

(deftest struct-fields-are-reported-per-field
  (testing "a changed FIELD type is named as such, not as a whole-property change"
    (let [r (breaking (schema [[:seriennummer :string]
                               [:einsatzort [:struct {:position :string :station :integer}]]]
                              :seriennummer)
                      with-struct #{})]
      (is (= [{:object-type :bauteil :change :struct-field-type-changed
               :property :einsatzort :field :station :from :string :to :integer}]
             r))))

  (testing "a deleted FIELD breaks only where its property carries edits"
    (let [after (schema [[:seriennummer :string]
                         [:einsatzort [:struct {:position :string}]]]
                        :seriennummer)]
      (is (= [] (breaking after with-struct #{})))
      (is (= [{:object-type :bauteil :change :struct-field-deleted
               :property :einsatzort :field :station :from :string}]
             (breaking after with-struct #{[:bauteil :einsatzort]})))))

  (testing "struct → non-struct falls back to the coarse verdict"
    (is (= [:property-type-changed]
           (map :change
                (breaking (schema [[:seriennummer :string] [:einsatzort :string]] :seriennummer)
                          with-struct #{}))))))

(deftest a-rename-is-reported-as-a-question-not-a-guess
  (let [after (schema [[:seriennummer :string] [:benennung-neu :string]] :seriennummer)
        r     (breaking after live #{[:bauteil :benennung]})]
    (testing "the deletion is still reported — the rename is an ADDITIONAL hint"
      (is (contains? (set (map :change r)) :property-deleted)))
    (testing "and the candidate is named"
      (is (= {:object-type :bauteil :change :property-renamed?
              :property :benennung :candidate :benennung-neu :type :string}
             (first (filter #(= :property-renamed? (:change %)) r))))))

  (testing "no rename hint without edits — an unedited delete+add is ordinary"
    (is (= [] (breaking (schema [[:seriennummer :string] [:benennung-neu :string]] :seriennummer)
                        live #{}))))

  (testing "no hint when the added property has a DIFFERENT type"
    (is (= [:property-deleted]
           (map :change (breaking (schema [[:seriennummer :string] [:benennung-neu :integer]]
                                          :seriennummer)
                                  live #{[:bauteil :benennung]}))))))

(deftest ein-neuer-api-name-auf-derselben-spalte-ist-dieselbe-eigenschaft
  (let [vorher  (schema [[:seriennummer :string] [:prüfstatus :string]] :seriennummer)
        nachher (schema [[:seriennummer :string] [:pruefstatus :string]] :seriennummer)]
    (testing "gleiche Spalte, gleicher Speichertyp: umbenannt, nicht geloescht"
      (is (= [{:object-type :bauteil :change :property-renamed :from :prüfstatus :to :pruefstatus}]
             (breaking nachher vorher #{[:bauteil :prüfstatus]}))))
    (testing "ohne Edits genauso — der Planer soll es wissen"
      (is (= [:property-renamed] (map :change (breaking nachher vorher #{})))))
    (testing "andere Spalte bleibt eine Loeschung neben einer Anlage"
      (is (= [:property-deleted :property-renamed?]
             (map :change (breaking (schema [[:seriennummer :string] [:pruefstatus-neu :string]]
                                            :seriennummer)
                                    vorher #{[:bauteil :prüfstatus]})))))
    (testing "anderer Speichertyp auf derselben Spalte ist keine Umbenennung"
      (is (= [:property-deleted]
             (map :change (breaking (schema [[:seriennummer :string] [:pruefstatus :integer]]
                                            :seriennummer)
                                    vorher #{[:bauteil :prüfstatus]})))))))
