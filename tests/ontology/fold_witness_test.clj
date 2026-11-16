(ns ^:unit ontology.fold-witness-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [support.select-sql :as sel]
            [sutra.store.derived :as sd]
            [sutra.types.witness :as wit]))

(def ^:private auftrag  :ri.test.object-type.auftrag)
(def ^:private position :ri.test.object-type.position)
(def ^:private hat-pos  :ri.test.link-type.positionen)

(defn- attr
  [nm t & [expression]]
  (cond-> {:attribute-name nm :attribute-type t}
    expression (assoc :opts {:derived {:expression expression}})))

(def ^:private world
  {:object-types
   {auftrag  {:object-type-name :auftrag
              :attribute-list [(attr :nummer :string)
                               (attr :summe :decimal [:connected-sum :positionen :netto])]
              :plural "Aufträge"}
    position {:object-type-name :position
              :attribute-list [(attr :netto :decimal) (attr :storniert :boolean)]
              :plural "Positionen"}}
   :link-types
   {hat-pos {:link-type-name :positionen :from-object-type auftrag
             :to-object-type position :cardinality :one-to-many}}})

(def ^:private summe [:connected-sum :positionen :netto])

(deftest the-witness-is-two-ordinary-counts-over-the-same-side
  (let [{:keys [questions pairs]} (wit/plan [[:summe summe]])]
    (is (= 1 (count questions)) "one fold, one witness")
    (is (= [[:sutra.types.witness/summe.0.seen [:connected-count :positionen]]
            [:sutra.types.witness/summe.0.used [:connected-count :positionen [:nil? :netto false]]]]
           pairs)
        "both :connected-count over the side the fold itself reads")))

(deftest a-fold-s-own-predicate-narrows-its-witness
  (testing "else the witness would count rows the fold never saw and call a sound 0 broken"
    (is (= [[:sutra.types.witness/summe.0.seen
             [:connected-count :positionen [:= :storniert false]]]
            [:sutra.types.witness/summe.0.used
             [:connected-count :positionen [:and [:= :storniert false] [:nil? :netto false]]]]]
           (:pairs (wit/plan [[:summe
                               [:connected-sum :positionen :netto [:= :storniert false]]]]))))))

(deftest an-extreme-of-nothing-is-nil-and-needs-no-witness
  (is (empty? (:questions (wit/plan [[:zuletzt [:connected-max :abschnitte :entschieden-am]]
                                     [:zuerst [:connected-min :abschnitte :entschieden-am]]])))))

(deftest a-count-needs-no-witness
  (testing ":connected-count counts ROWS, and of no rows 0 is the whole truth"
    (is (= [] (:pairs (wit/plan [[:n [:connected-count :positionen]]]))))
    (is (= [] (:pairs (wit/plan [[:x [:connected-one :positionen :netto]]]))))))

(deftest the-witness-rides-in-the-fold-s-own-statement
  (let [derived (into [[:summe summe]] (:pairs (wit/plan [[:summe summe]])))
        ir      {:op :scan :root auftrag}
        plan    (sd/plan world ir derived)
        steps   (loop [n plan acc []]
                  (if (:in n) (recur (:in n) (cond-> acc (= :with (:op n)) (conj n))) acc))]
    (is (= 3 (count steps)) "the fold and its two counts, all three as :with columns")
    (is (= #{:positionen} (set (map :link steps)))
        "over one side — so sutra.store.plan/merge-chains folds them into ONE grouped pass")
    (is (= #{:sum :count} (set (map :fn steps))))
    (let [{:keys [sql tables]} (sel/compile-read world plan)]
      (is (str/includes? sql "SUM(") "the fold")
      (is (<= 2 (count (re-seq #"COUNT\(" sql))) "and its two counts, in the same statement")
      (is (= 1 (count tables))
          "over one edge relation: the witness costs the read no pass of its own"))))

(deftest a-relation-that-is-merely-empty-is-no-defect
  (let [qs (:questions (wit/plan [[:summe summe]]))
        settle (fn [vs] (wit/settle {:id "1" :values vs} qs))]
    (testing "no rows at all — 0 is honest, and crying wolf here would fire on every
              rollup over a relation nobody has filled yet"
      (is (nil? (:defects (settle {:summe 0
                                   :sutra.types.witness/summe.0.seen 0
                                   :sutra.types.witness/summe.0.used 0})))))
    (testing "rows that really do sum to 0"
      (is (nil? (:defects (settle {:summe 0
                                   :sutra.types.witness/summe.0.seen 3
                                   :sutra.types.witness/summe.0.used 3})))))
    (testing "rows reached, none of them folded — THIS is the broken fold"
      (let [d (:defects (settle {:summe 0
                                 :sutra.types.witness/summe.0.seen 5
                                 :sutra.types.witness/summe.0.used 0}))]
        (is (= [{:over :positionen :of :netto :seen 5}]
               (mapv #(dissoc % :why) (:summe d))))
        (is (str/includes? (:why (first (:summe d))) "5 Zeilen"))))
    (testing "the witness leaves no trace on a row it had nothing to say about"
      (is (= {:summe 7} (:values (settle {:summe 7
                                          :sutra.types.witness/summe.0.seen 2
                                          :sutra.types.witness/summe.0.used 2})))))))

(deftest a-property-that-reads-two-folds-is-answered-for-each
  (let [e  [:- [:connected-sum :positionen :netto] [:connected-sum :positionen :rabatt]]
        qs (:questions (wit/plan [[:marge e]]))
        d  (:defects (wit/settle {:id "1"
                                  :values {:marge 0
                                           :sutra.types.witness/marge.0.seen 4
                                           :sutra.types.witness/marge.0.used 4
                                           :sutra.types.witness/marge.1.seen 4
                                           :sutra.types.witness/marge.1.used 0}}
                                 qs))]
    (is (= 2 (count qs)))
    (is (= [:rabatt] (mapv :of (:marge d)))
        "the half that folded nothing is named; the half that folded is not")))

(deftest an-expression-fold-is-judged-by-its-value-only-when-it-stands-alone
  (let [one  (:questions (wit/plan [[:schnitt [:connected-avg :positionen [:- :brutto :rabatt]]]]))
        both (:questions (wit/plan [[:x [:+ [:connected-avg :positionen [:- :a :b]]
                                         [:connected-sum :positionen :netto]]]]))]
    (testing "no attribute to ask 'is it set' of, so no `used` count — alone, the value
              answers it: of no values an average is nil"
      (is (nil? (:used (first one))))
      (is (some? (:defects (wit/settle {:id "1" :values {:schnitt nil
                                                         :sutra.types.witness/schnitt.0.seen 4}}
                                       one)))))
    (testing "in company the value is arithmetic over several folds and says nothing about
              any one of them, so the question stays unanswered rather than guessed"
      (is (nil? (:defects (wit/settle {:id "1" :values {:x nil
                                                        :sutra.types.witness/x.0.seen 4
                                                        :sutra.types.witness/x.1.seen 4
                                                        :sutra.types.witness/x.1.used 4}}
                                      both)))))))

(deftest the-verdict-can-be-taken-over-the-whole-type
  (let [prs  (:pairs (wit/plan [[:summe summe]]))
        cols (sd/stated-columns world auftrag prs)
        seen (cols :sutra.types.witness/summe.0.seen)
        used (cols :sutra.types.witness/summe.0.used)]
    (is (and seen used)
        "both counts are answered from a column of their own, so a filter can name them")
    (let [ir {:op :filter
              :where [:and [:> seen 0] [:= used 0]]
              :in (sd/plan world {:op :scan :root auftrag} prs)}
          {:keys [sql tables]} (sel/compile-read world ir)]
      (is (str/includes? sql "WHERE") "the verdict is a WHERE, not a row walk")
      (is (= 1 (count tables))
          "still one edge relation: counting the broken rows of a type costs one pass"))))
