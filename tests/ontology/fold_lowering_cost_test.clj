(ns ^:unit ontology.fold-lowering-cost-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.store.derived :as sd]
            [support.select-sql :as sel]))

(def ^:private auftrag :ri.test.object-type.auftrag)
(def ^:private position :ri.test.object-type.position)

(defn- attr
  [nm t & [expression]]
  (cond-> {:attribute-name nm :attribute-type t}
    expression (assoc :opts {:derived {:expression expression}})))

(defn- world
  [{:keys [folds chain]}]
  {:object-types
   {auftrag
    {:object-type-name :auftrag
     :attribute-list
     (-> [(attr :teiler :decimal)]
         (into (for [i (range (max 1 folds))]
                 (attr (keyword (str "f" i)) :decimal [:connected-sum :positionen :netto])))
         (into (for [i (range 1 (inc chain))]
                 (let [below (keyword (str (if (= 1 i) "f" "c") (dec i)))]
                   (attr (keyword (str "c" i)) :decimal
                         [:+ [:get below] [:get below]])))))
     :plural "Aufträge"}
    position
    {:object-type-name :position
     :attribute-list [(attr :netto :decimal)]
     :plural "Positionen"}}
   :link-types
   {:ri.test.link-type.positionen
    {:link-type-name :positionen :from-object-type auftrag
     :to-object-type position :cardinality :one-to-many}}})

(defn- read-shape
  [spec]
  (let [W  (world spec)
        d  (vec (sd/derived-of W auftrag))
        ir {:op :static :root auftrag :ids []}
        [sql _] (sel/compile-sql W (sd/plan W ir d))]
    (assoc (sqltrace/shape sql) :sql sql)))

(deftest a-row-read-states-no-correlated-fold
  (testing "a fold belongs in the FROM, keyed by the near end, so pg folds the edge
            relation ONCE — not once per row, which it cannot decorrelate out of a
            target list, and which over an fk-derived link re-derives a join of two
            object tables per row"
    (let [{:keys [folds fold-joins]} (read-shape {:folds 4 :chain 0})]
      (is (zero? folds)
          "no correlated scalar fold survives in a row read")
      (is (= 1 fold-joins)
          "and one grouped pass for all four rollups, because all four traverse the same
           SIDE: they differ in what they aggregate, not in the rows they aggregate over,
           so they share the scan, the edge de-duplication and the hash aggregate, each as
           one aggregate column with its own predicate as a FILTER. A pass per rollup would
           scan the same rows once per declared property — in prod a projekt declares
           twenty folds over its betriebsaufträge")))

  (testing "so what a read costs follows the SIDES the type reads, not how many properties
            it declares over them: twenty rollups over one side are still one pass"
    (doseq [n [1 4 20]]
      (is (= 1 (:fold-joins (read-shape {:folds n :chain 0})))
          (str n " rollups over one side")))))

(deftest a-fold-is-stated-once-however-many-columns-read-it
  (testing "every reader of a column reads its LABEL; a label is the one form that
            cannot be inlined a second time"
    (doseq [chain [0 1 4 9]]
      (let [{:keys [folds fold-joins]} (read-shape {:folds 1 :chain chain})]
        (is (= 1 fold-joins)
            (str "chain of " chain " computed columns over one rollup still states it once"))
        (is (zero? folds)
            (str "and states it as a grouped pass, not a per-row subquery (chain " chain ")"))))))

(deftest the-text-stays-linear-in-the-length-of-a-compute-chain
  (testing "restating a column made the text grow by powers of two: a column read twice
            carried its whole text twice, so nine computed columns deep held 1024 copies
            of one subquery and 265 154 characters of SQL. Measured per step, growth is
            now a column's own text and nothing more."
    (let [chars (into {} (for [n [0 3 6 9 12]] [n (:chars (read-shape {:folds 1 :chain n}))]))
          step  (/ (double (- (chars 12) (chars 0))) 12)]
      (is (< (chars 12) (* 4 (chars 0)))
          (str "twelve deep must stay within a small multiple of the plain read, not 4096×"
               " — measured " (chars 12) " against " (chars 0)))
      (testing "and each further column adds the same amount, which is what linear means"
        (doseq [[a b] [[0 3] [3 6] [6 9] [9 12]]]
          (let [per (/ (double (- (chars b) (chars a))) (- b a))]
            (is (< (Math/abs (- per step)) (* 0.5 step))
                (str "columns " a "→" b " add " (long per) " chars each, against "
                     (long step) " on average"))))))))

(deftest the-measure-itself-is-pinned
  (testing "shape is what both this test and `sutra trace` judge a statement by, so it
            must keep measuring: a text that stopped matching would read as a clean bill"
    (let [of (fn [sql] (assoc (sqltrace/shape sql) ::length (count sql)))]
      (is (= {:chars 55 ::length 55 :folds 1 :distinct 1 :repeat 1}
             (of "SELECT (SELECT COUNT(*) FROM b WHERE b.id = a.k) FROM a"))
          "a correlated fold is seen, :chars is the WHOLE text's length — the number the
           tracer would otherwise lose, since it keeps only a 4000-character head")
      (is (= {:chars 98 ::length 98 :folds 2 :distinct 1 :repeat 2}
             (of (str "SELECT (SELECT COUNT(*) FROM b WHERE b.id = a.k),"
                      " (SELECT COUNT(*) FROM b WHERE b.id = a.k) FROM a")))
          "the SAME fold twice is one distinct text restated — the pathology, named")
      (is (= {:chars 86 ::length 86 :folds 0 :distinct 0 :fold-joins 1}
             (of (str "SELECT g.v FROM a LEFT JOIN (SELECT k, COUNT(*) AS v"
                      " FROM b GROUP BY k) g ON g.k = a.k")))
          "and the grouped form is counted as a join, not as a correlated fold")
      (is (= {:chars 0 ::length 0 :folds 0 :distinct 0} (of ""))
          "and a text with nothing in it measures to nothing, rather than dividing by it"))))
