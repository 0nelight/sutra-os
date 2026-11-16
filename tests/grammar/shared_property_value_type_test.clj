(ns ^:unit grammar.shared-property-value-type-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.value-types :as vt]
            [sutra.catalog.constraints :as cstr]
            [sutra.catalog.kernel :as kernel]
            [sutra.authoring.install :as install]
            [sutra.parser.ent :as ent]
            [sutra.parser.resolve :as res]))

(def ^:private vts
  {:kennzeichen-format {:type :string :constraints {:regex "D-[A-Z]{4}"}}
   :betriebsstunden  {:type :decimal :format {:kind :currency :currency "EUR"}}})

(def ^:private authored
  {:value-types vts
   :shared-properties {:kennzeichen   {:value-type :kennzeichen-format :name "Kennzeichen"
                                       :render-hints [:searchable]}
                       :betriebsstunden {:value-type :betriebsstunden :type :decimal
                                         :name "Betrag"}}
   :objects {:luftfahrzeug {:attributes [[:luftfahrzeug-id :string] :kennzeichen
                                         :betriebsstunden]
                            :plural "Luftfahrzeuge"}}})

(defn- lowered [s] (ent/validate-schema! s "shared-property-value-type-test"))

(deftest lowering-derives-the-base
  (let [sp (:shared-properties (lowered authored))]
    (is (= {:type :string :value-type :kennzeichen-format}
           (select-keys (:kennzeichen sp) [:type :value-type]))
        "no :type written → the value type's base")
    (is (= :decimal (get-in sp [:betriebsstunden :type])) "a stated :type equal to the base stays"))
  (testing "a base mismatch is refused, as for an attribute"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"wraps its base type"
                          (lowered (assoc-in authored [:shared-properties :kennzeichen :type]
                                             :integer)))))
  (testing "an unknown value type is refused when the base cannot be derived"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"names :value-type :nope, which is not a declared"
                          (lowered (assoc-in authored [:shared-properties :kennzeichen :value-type]
                                             :nope))))))

(deftest the-resolver-names-an-unknown-value-type
  (let [s  (-> authored
               (assoc-in [:shared-properties :kennzeichen]
                         {:type :string :value-type :kennzeichn-format}))
        fs (res/findings s)]
    (is
     (some
      #(re-find #"shared property kennzeichen :value-type :kennzeichn-format, which is not a declared value type — did you mean :kennzeichen-format\?"
                (:message %))
      fs)
     (pr-str (map :message fs))))
  (testing "…judged against live when the file declares no :value-types"
    (let [s {:shared-properties {:kennzeichen {:type :string :value-type :kennzeichen-format}}}]
      (is (empty? (res/findings s {:value-type-names #{:kennzeichen-format}})))
      (is (seq (res/findings s {:value-type-names #{}}))))))

(deftest the-plan-carries-it-to-every-user
  (let [p   (install/plan-schema (lowered authored) {} {})
        of  (fn [kind entity] (some #(when (and (= kind (:kind %)) (= entity (:entity %))) %) p))]
    (is (= {:value-type :kennzeichen-format :type :string}
           (select-keys (:args (of :shared-property :kennzeichen)) [:value-type :type])))
    (is (= [[:luftfahrzeug-id :string]
            [:kennzeichen :string
             {:value-type :kennzeichen-format :name "Kennzeichen" :render-hints [:searchable]}]
            [:betriebsstunden :decimal {:value-type :betriebsstunden :name "Betrag"}]]
           (:attributes (:args (of :object-type :luftfahrzeug))))
        "the object type's property carries the SAME :value-type opt a local one would —
         and the shared property's own metadata beside it")))

(deftest the-staging-door-stores-and-judges-it
  (let [{:keys [value]} (kernel/step nil :create :shared-property
                                     {:name :kennzeichen :type :string
                                      :value-type "kennzeichen-format"})]
    (is (= :kennzeichen-format (:value-type value))
        "the mutation's :value-type is kept, as a keyword"))
  (let [live-vts {"ri.vt.1" {:value-type-name :kennzeichen-format :type :string}}
        probs    (fn [sp] (filter #(= :shared-property/value-type (:rule %))
                                  (kernel/findings
                                   {:value-types live-vts :shared-properties {"ri.sp.1" sp}})))]
    (is (empty? (probs {:shared-property-name :kennzeichen :type :string
                        :value-type :kennzeichen-format})))
    (is (re-find #"names no declared value type"
                 (:message (first (probs {:shared-property-name :kennzeichen :type :string
                                          :value-type :nope})))))
    (is (re-find #"is not the base :string"
                 (:message (first (probs {:shared-property-name :kennzeichen :type :integer
                                          :value-type :kennzeichen-format})))))))

(deftest the-runtime-enforces-it-like-a-local-property
  (let [attrs (mapv (fn [[n t o]] {:attribute-name n :attribute-type t :opts o})
                    (:attributes (:args (some #(when (= :luftfahrzeug (:entity %)) %)
                                              (install/plan-schema (lowered authored) {} {})))))
        eff   (vt/resolve-value-types attrs vts)]
    (is (seq (cstr/violations eff {:luftfahrzeug-id "k" :kennzeichen "OE-LBA"}
                              {:require-complete? false})))
    (is (empty? (cstr/violations eff {:luftfahrzeug-id "k" :kennzeichen "D-AIMA"}
                                 {:require-complete? false})))))
