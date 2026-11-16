(ns ^:unit ontology.rollup-filter-test
  (:require [clojure.test :refer [deftest is testing]]
            [support.select-sql :as sel]
            [sutra.types.expr :as expr]
            [sutra.store.derived :as sd]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.types.aggregate :as agg]
            [sutra.types.link-side :as ls]
            [sutra.ref.objectset :as ros]
            [sutra.store.pg.sql :as sql]))

(defn- lowered
  [rollup]
  (-> {:objects {:auftrag {:attributes [[:x :decimal {:derived {:rollup rollup}}]]
                           :plural "Aufträge"}}}
      sugar/lower-rollups
      (get-in [:objects :auftrag :attributes 0 2 :derived :expression])))

(deftest a-where-rides-as-the-ops-last-argument
  (testing "the filtered forms"
    (is (= [:connected-sum :zellen :wert [:= :code "A03"]]
           (lowered {:over :zellen :agg :sum :of :wert :where [:= :code "A03"]})))
    (is (= [:connected-count :zellen [:= :code "A03"]]
           (lowered {:over :zellen :agg :count :where [:= :code "A03"]})))
    (is (= [:connected-avg :zellen :wert [:and [:= :code "A03"] [:> :wert 0]]]
           (lowered {:over :zellen :agg :avg :of :wert
                     :where [:and [:= :code "A03"] [:> :wert 0]]})))
    (is (= [:connected-arg-max :zellen :datum :kind [:= :code "A03"]]
           (lowered {:over :zellen :agg :arg-max :by :datum :pick :kind
                     :where [:= :code "A03"]}))))

  (testing "and an UNfiltered rollup lowers byte-identically to what it always did
            — the argument is trailing precisely so nothing installed moves"
    (is (= [:connected-sum :zellen :wert] (lowered {:over :zellen :agg :sum :of :wert})))
    (is (= [:connected-count :zellen]     (lowered {:over :zellen :agg :count}))))

  (testing "the link is still findable in the first position, filter or not"
    (is (= #{:zellen} (expr/link-refs
                       (lowered {:over :zellen :agg :sum :of :wert
                                 :where [:= :code "A03"]}))))
    (testing "and the predicate's fields belong to the NEIGHBOUR, so they must
              not be projected onto the root"
      (is (= #{} (expr/field-refs
                  (lowered {:over :zellen :agg :sum :of :wert
                            :where [:= :code "A03"]})))))))

(deftest the-door-refuses-a-where-that-cannot-mean-anything
  (let [ent-of (fn [rollup]
                 {:schema {:objects {:auftrag {:attributes
                                               [[:x :decimal {:derived {:rollup rollup}}]]
                                               :plural "Aufträge"}}}})
        bad?   (fn [rollup] (some? (ent/explain (ent-of rollup))))]
    (testing "the well-formed one passes"
      (is (not (bad? {:over :zellen :agg :sum :of :wert :where [:= :code "A03"]}))))

    (testing "a predicate outside sutra.types.query's vocabulary is not a filter"
      (is (bad? {:over :zellen :agg :sum :of :wert :where [:matches :code "A0.*"]}))
      (is (bad? {:over :zellen :agg :sum :of :wert :where {:code "A03"}}))
      (is (bad? {:over :zellen :agg :sum :of :wert :where [:= :code]})))

    (testing ":one takes no :where — its at-most-one promise is checked at
              DECLARATION time against the link's cardinality, and a filter would
              move that promise to a runtime with nothing left to do about it"
      (is (bad? {:over :l :agg :one :pick :name :where [:= :code "A03"]}))
      (is (not (bad? {:over :l :agg :one :pick :name}))))))

(def ^:private zellen
  [{:id "z1" :values {:code "A03" :wert 10}}
   {:id "z2" :values {:code "B05" :wert 7}}
   {:id "z3" :values {:code "A03" :wert 5}}
   {:id "z4" :values {:code "A03" :wert nil}}])

(defn- ctx-of [neighbours]
  {:state nil :object-id "a1" :traverse-fn (fn [_ _ _ _] neighbours)})

(defn- ev [expr] (expr/eval {} expr (ctx-of zellen)))

(deftest the-filter-narrows-the-fold
  (is (= 15 (ev [:connected-sum :zellen :wert [:= :code "A03"]]))
      "only the A03 cells, and a nil value is skipped exactly as it is unfiltered")
  (is (= 22 (ev [:connected-sum :zellen :wert]))
      "…and without the filter every cell still counts")
  (is (= 3 (ev [:connected-count :zellen [:= :code "A03"]]))
      ":count counts the ROWS that survive the filter, nil value or not")
  (is (= 7.5 (ev [:connected-avg :zellen :wert [:= :code "A03"]]))
      "the mean is over the non-nil values of the surviving rows")
  (is (= 10 (ev [:connected-max :zellen :wert [:= :code "A03"]])))
  (is (= 5 (ev [:connected-min :zellen :wert [:= :code "A03"]])))

  (testing "a compound predicate, and one over a text op"
    (is (= 10 (ev [:connected-sum :zellen :wert
                   [:and [:= :code "A03"] [:> :wert 5]]])))
    (is (= 15 (ev [:connected-sum :zellen :wert [:starts-with :code "A0"]]))))

  (testing "the filter is evaluated against the NEIGHBOUR, never the root — a
            field only the root has is simply missing, not silently the root's"
    (is (= 0 (ev [:connected-sum :zellen :wert [:= :nummer "4711"]])))))

(deftest an-emptied-set-keeps-the-ops-own-empty-value
  (let [none [:= :code "ZZZ"]]
    (is (= 0   (ev [:connected-sum :zellen :wert none])) "Σ of nothing is 0")
    (is (= 0   (ev [:connected-count :zellen none])))
    (is (nil?  (ev [:connected-avg :zellen :wert none])) "no rows, no average")
    (is (nil?  (ev [:connected-max :zellen :wert none])))
    (is (nil?  (ev [:connected-arg-max :zellen :datum :kind none])))))

(deftest a-rollup-nested-under-arithmetic-still-filters
  (is (= 3 (expr/eval {:teiler 5}
                      [:div [:connected-sum :zellen :wert [:= :code "A03"]] [:get :teiler]]
                      (ctx-of zellen)))))

(def ^:private auftrag :ri.test.object-type.auftrag)
(def ^:private zelle   :ri.test.object-type.zelle)
(def ^:private hat     :ri.test.link-type.hat-zellen)

(def ^:private schema
  {:object-types
   {auftrag {:object-type-name :auftrag
             :attribute-list [{:attribute-name :nummer :attribute-type :string}]
             :plural "Aufträge"}
    zelle   {:object-type-name :zelle
             :attribute-list [{:attribute-name :code :attribute-type :string}
                              {:attribute-name :wert :attribute-type :integer}]
             :plural "Zellen"}}
   :link-types
   {hat {:link-type-name :hat-zellen
         :from-object-type auftrag :to-object-type zelle
         :cardinality :one-to-many}}})

(defn- rollup-ir [where]
  {:op :aggregate
   :aggs [{:fn :sum :attr :wert :name :s}]
   :group-by [[:origin]]
   :in (let [hop {:op :search-around :link hat :dir :to
                  :in {:op :static :ids ["a1" "a2"]}}]
         (if where {:op :filter :where where :in hop} hop))})

(deftest the-origin-grouped-hop-becomes-one-join
  (let [[sql params] (sel/compile-sql schema (rollup-ir [:= :code "A03"]))]
    (testing "the link table is JOINed rather than hidden in an IN-subquery —
              that is the whole point: its near side has to be a column the
              GROUP BY can name"
      (is
       (re-find #"JOIN \(SELECT DISTINCT e\.to_id AS self_id, e\.from_id AS other_id FROM `c_ri\.test\.link_type\.hat_zellen` e\) c ON c\.self_id = t\.internal_uuid" sql)
       "over the DISTINCT edge pairs, so a parallel edge is one instance"))
    (testing "the near side is projected AND grouped, by code point; sum of nothing is 0"
      (is
       (re-find #"SELECT c\.other_id COLLATE \"C\" AS `ga`, COALESCE\(SUM\(t\.`wert`\), 0\) AS `aa`"
                sql))
      (is (re-find #"GROUP BY c\.other_id COLLATE \"C\"" sql)))
    (testing "the origin ids bound the read through the near type's table, and the rollup's :where is a WHERE"
      (is
       (re-find #"c\.other_id IN \(SELECT s2\.internal_uuid FROM `d_ri\.test\.object_type\.auftrag` s2 WHERE s2\.internal_uuid = ANY\(\?\)\)" sql))
      (is (re-find #"t\.`code` = \?" sql)))
    (testing "binds in statement order — an id list, then the predicate's value"
      (is (= [(sql/text-array ["a1" "a2"]) "A03"] (vec params))))))

(deftest an-unfiltered-rollup-lowers-to-the-same-statement-without-the-predicate
  (let [[sql params] (sel/compile-sql schema (rollup-ir nil))]
    (is (re-find #"GROUP BY c\.other_id COLLATE \"C\"" sql))
    (is (not (re-find #"t\.`code`" sql)))
    (is (= [(sql/text-array ["a1" "a2"])] (vec params)))))

(deftest origin-without-a-traversal-is-refused-rather-than-guessed
  (testing "a flat aggregate over a plain scan has no hop to have come FROM"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"takes no traversal"
         (sel/compile-sql schema {:root zelle :aggregate [{:fn :sum :attr :wert}]
                                   :group-by [[:origin]]}))))
  (testing "and neither does a composed set whose membership is not a hop"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"needs a traversal to name"
         (sel/compile-sql
          schema {:op :aggregate :aggs [{:fn :sum :attr :wert}] :group-by [[:origin]]
                  :in {:op :union :in [{:op :scan :root zelle}
                                       {:op :static :ids ["z1"] :root zelle}]}})))))

(deftest the-origin-term-is-part-of-the-vocabulary-not-a-special-case
  (testing "it normalizes like every other group term — a name and a positional
            alias, minted by the one normalizer both emitter and reshaper read"
    (let [{:keys [group-by]} (agg/normalize {:aggs [{:fn :sum :attr :wert}]
                                             :group-by [[:origin]]})]
      (is (= [{:kind :origin :name :origin :alias "ga"}] group-by))))

  (testing "an unknown group term still names what IS legal, [:origin] included"
    (is (re-find #"\[:origin\]"
                 (:message (agg/explain-aggregate {:aggs [{:fn :sum :attr :wert}]
                                                   :group-by [[:nonsense :wert]]}))))))

(deftest a-self-link-hop-has-one-direction-in-both-arms
  (testing "the pushdown and the fold both resolve the NAMED side, so a
            self-link is pushed down like any other link"
    (let [self {:object-types {zelle {}}
                :link-types {:z {:link-type-name :z :from-object-type zelle :to-object-type zelle
                                 :cardinality :many-to-one
                                 :forward-api-name :ober :reverse-api-name :unter}}}]
      (is (= [:to :from] (map #(:dir (ls/resolve-side self zelle %)) [:ober :unter]))))))

(def ^:private with-fn
  {:connected-sum :sum :connected-count :count :connected-avg :avg
   :connected-min :min :connected-max :max})

(defn- as-the-reference-would
  [op where rows]
  (let [cx  {:sigma {:extension (fn [_ T] (if (= T auftrag)
                                            [{:id "a1" :values {:nummer "4711"}}]
                                            rows))
                     :edges     (fn [_ _] (for [r rows] ["a1" (:id r)]))}}
        col (cond-> {:op :with :name :c :fn (with-fn op) :link hat :dir :to
                     :in {:op :static :root auftrag :ids ["a1"]}}
              (not= :connected-count op) (assoc :attr :wert)
              where                      (assoc :where where))]
    (get-in (first (ros/read cx schema col)) [:values :c])))

(defn- as-postgres-would
  [op v]
  (case op
    :connected-sum (some-> v bigdec)
    :connected-avg (some-> v bigdec (.setScale 16 java.math.RoundingMode/HALF_EVEN))
    v))

(deftest the-store-column-and-the-evaluator-answer-the-same-number
  (doseq [op    (keys with-fn)
          where [nil [:= :code "A03"] [:= :code "ZZZ"] [:> :wert 6]]
          rows  [zellen
                 []
                 [{:id "z9" :values {:code "A03" :wert nil}}]]]
    (let [node   (if (= :connected-count op)
                   (cond-> [op :zellen] where (conj where))
                   (cond-> [op :zellen :wert] where (conj where)))
          folded (expr/eval {} node (ctx-of rows))
          read   #(sd/fold-value (with-fn op) %)
          column (as-the-reference-would op where rows)
          what   (str op " over " (pr-str where) " on " (count rows) " rows")]
      (is (= folded (read column))
          (str what ": the reference's column, read as a reader reads it, is the evaluator's fold"))
      (is (= folded (read (as-postgres-would op column)))
          (str what ": and so is Postgres's answer")))))
