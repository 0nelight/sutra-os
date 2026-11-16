(ns ^:unit ontology.fold-far-expr-test
  (:require [clojure.test :refer [deftest is testing]]
            [support.select-sql :as sel]
            [sutra.store.derived :as sd]))

(def ^:private auftrag  :ri.test.object-type.auftrag)
(def ^:private position :ri.test.object-type.position)
(def ^:private teil     :ri.test.object-type.teil)

(defn- attr
  [nm t & [expression]]
  (cond-> {:attribute-name nm :attribute-type t}
    expression (assoc :opts {:derived {:expression expression}})))

(def ^:private world
  {:object-types
   {auftrag
    {:object-type-name :auftrag
     :attribute-list [(attr :menge-summe :decimal [:connected-sum :positionen :menge])
                      (attr :wert-summe   :decimal [:connected-sum :positionen :zeilenwert])
                      (attr :wert-max     :decimal [:connected-max :positionen :zeilenwert])
                      (attr :netto-summe  :decimal [:connected-sum :positionen :netto])
                      (attr :tiefe-summe  :decimal [:connected-sum :positionen :unter-summe])
                      (attr :offene-summe :decimal [:connected-sum :positionen :menge
                                                    [:> :zeilenwert 0]])
                      (attr :tiefe-filter :decimal [:connected-sum :positionen :menge
                                                    [:> :unter-summe 0]])
                      (attr :offen-summe  :decimal [:connected-sum :positionen :offen?])
                      (attr :offen-count  :integer [:connected-count :positionen
                                                    [:= :offen? true]])]
     :plural "Aufträge"}
    position
    {:object-type-name :position
     :attribute-list [(attr :menge  :decimal)
                      (attr :preis  :decimal)
                      (attr :rabatt :decimal)
                      (attr :zeilenwert :decimal [:* [:get :menge] [:get :preis]])
                      (attr :netto      :decimal [:- [:get :zeilenwert] [:get :rabatt]])
                      (attr :unter-summe :decimal [:connected-sum :teile :wert])
                      (attr :offen? :boolean [:connected? :teile])]
     :plural "Positionen"}
    teil
    {:object-type-name :teil
     :attribute-list [(attr :wert :decimal)]
     :plural "Teile"}}
   :link-types
   {:ri.test.link-type.positionen
    {:link-type-name :positionen :from-object-type auftrag
     :to-object-type position :cardinality :one-to-many}
    :ri.test.link-type.teile
    {:link-type-name :teile :from-object-type position
     :to-object-type teil :cardinality :one-to-many}}})

(defn- pair
  [a]
  (first (filter #(= a (first %)) (sd/derived-of world auftrag))))

(defn- steps-of
  [a]
  (:steps (sd/lower world auftrag a)))

(defn- fold-of [a] (first (filter #(= :with (:op %)) (steps-of a))))

(def ^:private ir {:op :static :root auftrag :ids []})

(deftest a-far-value-computed-from-the-far-row-is-folded-as-that-arithmetic
  (testing "the fold carries the far property's expression over the far row's OWN columns,
            with the cast its declared type gives it — so the store answers it"
    (is (= {:fn :sum :link :positionen :expr [:* :menge :preis] :cast [:decimal 10]}
           (select-keys (fold-of :wert-summe) [:fn :link :expr :cast])))
    (testing "and the shape of the fold is the only thing that changes — :max folds the
              same expression"
      (is (= {:fn :max :expr [:* :menge :preis] :cast [:decimal 10]}
             (select-keys (fold-of :wert-max) [:fn :expr :cast])))))

  (testing "a far property the far type STORES still rides as :attr; an expression where a
            column will do would state the same thing in more text"
    (is (= {:attr :menge :fn :sum} (select-keys (fold-of :menge-summe) [:attr :fn])))
    (is (nil? (:expr (fold-of :menge-summe)))))

  (testing "nothing is left for the evaluator in either case: that is the whole point —
            the far objects are not loaded, so no round of link reads is spent and no
            size cap has anything to bound"
    (is (= [] (sd/leftovers world ir [(pair :wert-summe)])))
    (is (= [] (sd/leftovers world ir [(pair :menge-summe)])))))

(defn- far-in-of [a] (:far-in (fold-of a)))

(deftest a-far-value-that-is-itself-a-fold-folds-over-a-relation-that-carries-it
  (testing "no expression can state a fold — its value is an aggregate over rows the outer
            fold never sees — so the far side becomes a RELATION carrying it as a column,
            and the fold reads that column. Two grouped passes, one statement"
    (let [rel (far-in-of :tiefe-summe)]
      (is (= :sutra.store.derived/unter-summe (:attr (fold-of :tiefe-summe)))
          "the outer fold folds the column, not the property name")
      (is (= [:with :compute] [(:op (:in rel)) (:op rel)])
          "the relation is the inner fold and the cast its declared type gives it")
      (is (= {:fn :sum :link :teile :attr :wert} (select-keys (:in rel) [:fn :link :attr]))
          "the inner fold is the far type's own, lowered by the same function")
      (is (= {:op :scan :root position} (:in (:in rel)))
          "over the far type's rows, and nothing narrower")))

  (testing "a chain keeps its roundings: each property is a layer of the relation with its
            own cast, which is what an inlined expression had nowhere to put — where a
            property is computed must change the cost, never the value"
    (is (= [:decimal 10] (:cast (far-in-of :tiefe-summe))))
    (is (= :sutra.store.derived/netto (:attr (fold-of :netto-summe)))
        "netto reads zeilenwert, which is rounded before netto subtracts from it"))

  (testing "a :where may now name a derived far property, because the relation gives it a
            column — the shape that once filtered on a column the indexer keeps NULL and
            answered 0 for every row in prod (5257b127)"
    (is (= [:> :sutra.store.derived/unter-summe 0] (:where (fold-of :tiefe-filter))))
    (is (= :menge (:attr (fold-of :tiefe-filter))) "while it still folds the stored column")
    (is (some? (far-in-of :tiefe-filter)))
    (testing "and a yes/no one compares as the COUNT its column holds: the far property is
              `that count is positive`, so the comparison becomes that and not an equality
              against a column holding a number"
      (is (= [:> :sutra.store.derived/offen? 0] (:where (fold-of :offen-count))))))

  (testing "nothing is left above the store for any of them"
    (doseq [a [:tiefe-summe :netto-summe :offene-summe :tiefe-filter :offen-count]]
      (is (= [] (sd/leftovers world ir [(pair a)])) (str a " is stated whole")))))

(deftest what-the-statement-still-cannot-state-falls-to-the-evaluator
  (testing "a fold whose :of is a yes/no the far type derives has nothing to sum: the
            column is a COUNT and the property is `that count is positive`, so summing the
            property is not what the column holds"
    (is (nil? (steps-of :offen-summe)))
    (is (= [:offen-summe] (mapv first (sd/leftovers world ir [(pair :offen-summe)]))))))

(deftest a-nested-fold-is-one-grouped-pass-over-another
  (let [[sql params] (sel/compile-sql world (sd/plan world ir [(pair :tiefe-filter)]))]
    (testing "the inner fold is a grouped pass, and the outer one folds over its result"
      (is (= 2 (count (re-seq #"GROUP BY" sql)))
          "one per level of folding and not one more — never one per row")
      (is (re-find #"SUM\(wf\.`menge`\).*AS v0 FROM .* JOIN \(SELECT t\.\*, " sql)
          "the outer fold's far side is a relation, not a bare table"))
    (testing "the predicate on the far type's own derived column reads that column, as the
              aggregate's own FILTER — which is what lets it share the pass with a fold
              that filters differently or not at all"
      (is (re-find #"FILTER \(WHERE wf\.`w__unter-summe` > \?\)" sql))
      (is (= [0] (vec params))))
    (testing "and the far rows are never carried out of the statement"
      (is (not (re-find #"internal_uuid IN \(" sql))))))

(deftest the-fold-is-one-statement-over-the-far-rows
  (let [[sql _] (sel/compile-sql world (sd/plan world ir [(pair :wert-summe)]))]
    (testing "the arithmetic sits INSIDE the aggregate, over the far alias"
      (is (re-find #"SUM\(ROUND\(CAST\(\(wf\.`menge` \* wf\.`preis`\) AS numeric\), 10\)\)" sql)
          "each far row's value is computed and rounded, then the rows are folded"))
    (testing "and it is still ONE grouped pass over the edge relation, not one per row"
      (is (= 1 (count (re-seq #"GROUP BY" sql)))))
    (testing "the far type is read once, as the fold's own relation — the chain costs no
              second statement"
      (is (= 1 (count (re-seq #"object_type\.position" sql)))))))
