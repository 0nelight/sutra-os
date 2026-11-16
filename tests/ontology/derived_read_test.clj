(ns ^:unit ontology.derived-read-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.grants :as grants]
            [sutra.types.expr :as expr]
            [sutra.objects.views :as views]
            [sutra.parser.sugar :as sugar]
            [sutra.objects.planner :as planner]
            [sutra.ref.objectset :as ros]
            [sutra.store.access.substrate :as sub]
            [sutra.store.derived :as sd]))

(defn- lowered
  [rollup]
  (-> {:objects {:bestellung {:attributes [[:x :string {:derived {:rollup rollup}}]]
                              :plural "Bestellungen"}}}
      sugar/lower-rollups
      (get-in [:objects :bestellung :attributes 0 2 :derived :expression])))

(deftest every-authorable-rollup-counts-as-link-traversing
  (doseq [rollup [{:over :l :agg :sum :of :netto}
                  {:over :l :agg :max :of :netto}
                  {:over :l :agg :min :of :netto}
                  {:over :l :agg :avg :of :netto}
                  {:over :l :agg :count}
                  {:over :l :agg :arg-max :by :datum :pick :kind}
                  {:over :l :agg :one :pick :name}]]
    (let [expr (lowered rollup)]
      (is (expr/link-expr? expr)
          (str (pr-str rollup) " lowers to " (pr-str expr)
               " — which must be recognised as needing a traversal"))
      (is (= #{:l} (expr/link-refs expr))
          "and the link it traverses must be findable without knowing the op"))))

(deftest a-link-op-without-a-ctx-throws-rather-than-answering
  (testing "which is why classifying one as scalar was silent: the throw is
            caught per row, and the property just reads back nil"
    (doseq [op expr/link-ops]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires a link context"
                            (expr/eval {} [op :l :attr :more]))
          (str op " must demand a ctx")))))

(deftest link-refs-walks-the-whole-expression
  (testing "a traversal NESTS — an arithmetic head does not make it scalar"
    (is (= #{:positionen}
           (expr/link-refs [:div [:connected-sum :positionen :netto] [:get :teiler]])))
    (is (expr/link-expr? [:div [:connected-sum :positionen :netto] [:get :teiler]])))
  (testing "several links in one expression are all found"
    (is (= #{:a :b} (expr/link-refs [:- [:connected-sum :a :x] [:connected-sum :b :x]]))))
  (testing "an expression that traverses nothing has no refs"
    (is (= #{} (expr/link-refs [:div [:get :netto] [:get :menge]])))
    (is (not (expr/link-expr? [:div [:get :netto] [:get :menge]])))))

(deftest div-over-two-decimals-answers-instead-of-throwing
  (testing "clojure's `/` demands an EXACT result from two BigDecimals, and a
            money quotient almost never has one — so `:div` threw
            ArithmeticException, the per-row catch turned it into nil, and
            arbeitsauftrag.gewinn-prozent read back empty on every one of 2150
            rows with both operands filled"
    (is (= 7.93143787946098000M
           (expr/eval {:gewinn 2142.44M :faktura 27012.00M}
                      [:* [:div [:get :gewinn] [:get :faktura]] 100]))))
  (testing "an exact quotient keeps its exact value"
    (is (= 25M (expr/eval {:a 100.00M :b 4.00M} [:div [:get :a] [:get :b]]))))
  (testing "one decimal operand is enough to take the decimal path"
    (is (= 0.3333333333333333M (expr/eval {:a 1M} [:div [:get :a] 3]))))
  (testing "longs and doubles keep the behaviour they had"
    (is (= 3.5 (expr/eval {} [:div 7 2])))
    (is (= 0.5 (expr/eval {} [:div 1.0 2.0]))))
  (testing "a zero divisor is still refused, and a DECIMAL zero counts — the
            `#{0 0.0}` set never matched 0M, so the platform's own guard was
            unreachable for exactly the operands money arrives in"
    (doseq [z [0 0.0 0M 0.00M]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Division by zero"
                            (expr/eval {:a 1M :b z} [:div [:get :a] [:get :b]]))
          (str z " must be refused by the guard, not by BigDecimal")))))

(deftest field-refs-are-the-OWN-columns-only
  (testing "both spellings of a field read"
    (is (= #{:netto :menge} (expr/field-refs [:div [:get :netto] :menge]))))
  (testing "a literal is not a field"
    (is (= #{:netto} (expr/field-refs [:div [:get :netto] 2]))))
  (testing "a link op's arguments belong to the LINK and the FAR side"
    (is (= #{} (expr/field-refs [:connected-sum :positionen :netto]))
        ":netto is the neighbour's column, not this row's")
    (is (= #{:teiler} (expr/field-refs [:div [:connected-sum :positionen :netto] [:get :teiler]]))
        "only the arithmetic half contributes a column to project")))

(def ^:private bestellung :ot/bestellung)
(def ^:private position   :ot/position)

(def ^:private schema
  {:object-types
   {bestellung
    {:object-type-name :bestellung
     :attribute-list [{:attribute-name :nr :attribute-type :string}
                      {:attribute-name :summe :attribute-type [:decimal 12 2]
                       :opts {:derived {:expression [:connected-sum :positionen :netto]}}}
                      {:attribute-name :schnitt :attribute-type [:decimal 12 2]
                       :opts {:derived {:expression [:div [:get :brutto] 2]}}}
                      {:attribute-name :brutto :attribute-type [:decimal 12 2]}
                      {:attribute-name :zeilen :attribute-type :integer}
                      {:attribute-name :offen :attribute-type [:decimal 12 2]
                       :opts {:derived {:expression [:- [:get :brutto] [:get :summe]]}}}
                      {:attribute-name :gerundet :attribute-type [:decimal 12 2]
                       :opts {:derived {:expression [:round [:get :brutto]]}}}]
     :plural "Bestellungen"}
    position
    {:object-type-name :position
     :attribute-list [{:attribute-name :netto :attribute-type [:decimal 12 2]}
                      {:attribute-name :anteil :attribute-type [:decimal 12 2]
                       :opts {:derived {:expression [:div [:get :netto] 2]}}}]
     :plural "Positionen"}}
   :link-types
   {:lt/positionen {:link-type-name :positionen
                    :from-object-type bestellung :to-object-type position}}})

(defn- shape [spec]
  (@#'views/read-shape grants/system-authority schema nil spec))

(defn- reads-column?
  [ir]
  (boolean (some #(#{:with :compute} (:op %)) (tree-seq map? (comp #(if (map? %) [%] %) :in) ir))))

(def ^:private refused #"the store cannot compute")

(deftest a-plain-column-still-goes-to-the-substrate
  (testing "the fast path is untouched when no derived property is involved"
    (let [{:keys [ir]} (shape {:find :bestellung :where {:zeilen {:> 3}}})]
      (is (= [:> :zeilen 3] (:where ir)) "the filter compiled into the set")
      (is (not (reads-column? ir))))
    (is (not (reads-column? (:ir (shape {:find :bestellung :order-by :zeilen})))))))

(deftest filtering-ordering-and-folding-a-derived-property-is-the-stores
  (testing "a rollup the plan language states is a column the store computes — so the
            filter, the order and the fold all land in the substrate"
    (doseq [spec [{:find :bestellung :where {:summe {:> 100}}}
                  {:find :bestellung :order-by :summe}
                  {:find :bestellung :aggs [{:fn :sum :attr :summe}]}]]
      (is (reads-column? (:ir (shape spec))) (pr-str spec))))
  (testing "arithmetic over the row is a column too"
    (is (reads-column? (:ir (shape {:find :bestellung :where {:schnitt {:> 1}}})))))
  (testing "one the plan language cannot state — a rounding — is refused: a derived
            property is computed in the store or not at all"
    (doseq [spec [{:find :bestellung :where {:gerundet {:> 1}}}
                  {:find :bestellung :order-by :gerundet}
                  {:find :bestellung :aggs [{:fn :sum :attr :gerundet}]}]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo refused (shape spec)) (pr-str spec))))
  (testing "…and reading it is fine: it is evaluated after the store answers"
    (is (some #{:gerundet} (:compute (:derived (shape {:find :bestellung})))))))

(deftest an-aggregate-over-a-derived-property
  (testing "folds in the store, over its column, under the name the aggregate would give it"
    (let [{:keys [ir]} (shape {:find :bestellung :aggs [{:fn :sum :attr :summe}]})]
      (is (= :aggregate (:op ir)) "the substrate is asked for buckets")
      (is (= [:summe-sum] (map :name (:aggs ir))))
      (is (reads-column? ir))))
  (testing "grouping BY one is the same case"
    (is (= :aggregate
           (:op (:ir (shape {:find :bestellung :aggs [{:fn :count}] :group-by [:summe]})))))))

(deftest after-a-hop-the-derived-properties-are-the-FAR-types
  (let [{:keys [result-rid ir]} (shape {:find :bestellung :around [:positionen] :order-by :anteil})]
    (is (= position result-rid))
    (is (reads-column? ir) ":anteil is position's derived property, ordered on in the store"))
  (testing "a stored attribute of the far type still orders inside the traversal"
    (is (not (reads-column? (:ir (shape {:find :bestellung :around [:positionen]
                                         :order-by :netto}))))))
  (testing "the root's OWN derived property is not the far type's"
    (is (not (reads-column? (:ir (shape {:find :bestellung :around [:positionen]
                                         :order-by :summe}))))
        ":summe is not a property of :position at all — it orders on nothing here"))
  (testing "and the type it names is the one the PLANNER lands on"
    (let [{:keys [ir result-rid]} (shape {:find :bestellung :around [:positionen]})]
      (is (= result-rid
             (:root (planner/plan schema ir {:authority grants/system-authority}))))))
  (testing "filtering on a derived property before a hop filters the root in the store"
    (is (reads-column? (:ir (shape {:find :bestellung :around [:positionen]
                                    :where {:summe {:> 1}}})))))
  (testing "one the plan cannot state is refused there too"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo refused
                          (shape {:find :bestellung :around [:positionen]
                                  :where {:gerundet {:> 1}}})))))

(deftest show-narrows-what-gets-computed
  (testing "no :show ⇒ every runtime derived property of the result type"
    (is (= #{:schnitt :summe :offen :gerundet}
           (set (:compute (:derived (shape {:find :bestellung})))))))
  (testing ":show names what is wanted"
    (is (= [:summe] (:compute (:derived (shape {:find :bestellung :show [:nr :summe]})))))
    (is (= [] (:compute (:derived (shape {:find :bestellung :show [:nr]}))))
        "a read that shows only stored columns traverses nothing"))
  (testing "…but what the request USES is needed whether or not it is shown"
    (is (= [:summe] (:compute (:derived (shape {:find :bestellung :show [:nr]
                                                :order-by :summe})))))
    (is (= [] (:compute (:derived (shape {:find :bestellung :show [:nr]
                                          :where {:summe {:> 1}}}))))
        "a filter the store computes needs no value above it")
    (is (= [:summe] (:compute (:derived (shape {:find :bestellung :show [:nr]
                                                :aggs [{:fn :sum :attr :summe}]})))))))

(deftest a-derived-property-drags-in-the-one-it-is-COMPUTED-FROM
  (testing "asking for the dependent property asks for its dependency"
    (is (= [:summe :offen]
           (:compute (:derived (shape {:find :bestellung :show [:offen]}))))))
  (testing "…and computes it FIRST, whatever the declaration order says"
    (let [reordered (update-in schema [:object-types bestellung :attribute-list]
                               (fn [attrs] (into [(last attrs)] (butlast attrs))))]
      (is (= [:summe :offen]
             (:compute (:derived (@#'views/read-shape grants/system-authority reordered nil
                                                      {:find :bestellung :show [:offen]}))))
          "declaration order was dependency order by luck, not by rule"))))

(deftest a-filter-on-a-derived-property-saves-as-an-object-set
  (testing "the IR names its column, so the set holds what the read held"
    (is (reads-column? (:ir (views/object-set-ir {:authority grants/system-authority} schema
                                                 {:find :bestellung :where {:summe {:> 100}}})))))
  (testing "one the store cannot compute is refused, naming the way out"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"compute the value in a transform"
         (views/object-set-ir {:authority grants/system-authority} schema
                              {:find :bestellung :where {:gerundet {:> 100}}}))))
  (is (some? (views/object-set-ir {:authority grants/system-authority} schema
                                  {:find :bestellung :where {:zeilen {:> 3}}}))
      "a plain column saves fine"))

(deftest not=-and-!=-are-one-op
  (is (= (expr/eval {:s "offen"} [:!=   [:get :s] "abgeschlossen"])
         (expr/eval {:s "offen"} [:not= [:get :s] "abgeschlossen"]))
      "the two names answer identically")
  (is (true?  (expr/eval {:s "offen"}         [:not= [:get :s] "abgeschlossen"])))
  (is (false? (expr/eval {:s "abgeschlossen"} [:not= [:get :s] "abgeschlossen"]))))

(deftest nil-answers-a-comparison-and-defeats-an-arithmetic
  (testing "equality against nil is how \u201edas Feld ist noch leer\u201c is said"
    (is (true?  (expr/eval {:ordner nil}     [:= [:get :ordner] nil])))
    (is (false? (expr/eval {:ordner "offen"} [:= [:get :ordner] nil])))
    (is (true?  (expr/eval {:ordner nil}     [:not= [:get :ordner] "erledigt"]))
        "an empty column refuses cleanly instead of throwing"))
  (testing "a nil operand still kills arithmetic and ordering, where it IS a defect"
    (is (thrown? Exception (expr/eval {:netto nil} [:- [:get :netto] 1])))
    (is (thrown? Exception (expr/eval {:a nil} [:> [:get :a] 1])))))

(deftest a-read-is-one-question
  (testing "the objects and their derived properties come in one answer, as withProperties
            returns them: the read plan carries the columns"
    (let [stored {bestellung [{:id "b1" :object-type-ref bestellung
                               :values {:nr "B-1" :brutto 10M}}]
                  position   [{:id "p1" :object-type-ref position :values {:netto 4M}}]}
          cx     {:sigma {:extension (fn [_ T] (get stored T))
                          :edges     (fn [_ _] [["b1" "p1"]])}}
          asked  (atom [])
          read   (fn [spec]
                   (with-redefs [sub/query  (fn [_ {:keys [ir]}] (swap! asked conj ir)
                                              (ros/read cx schema ir))
                                 sub/reader (fn [_] (fn [[_ ir]] (swap! asked conj ir)
                                                      (ros/read cx schema ir)))]
                     (views/run-object-query {:authority grants/system-authority} schema spec)))
          reads  #(remove (fn [ir] (= :aggregate (:op ir))) @asked)]
      (is (= 4.00M
             (get-in (read {:find :bestellung :show [:nr :summe :offen]})
                     [:rows 0 :values :summe])))
      (is (= 1 (count (reads))) "one question, not one for the rows and one for their columns")
      (is (reads-column? (first (reads))))
      (testing "a property needed but not shown — the order — reads from the same answer"
        (reset! asked [])
        (is (= 6.00M
               (get-in (read {:find :bestellung :show [:nr :offen] :order-by :summe})
                       [:rows 0 :values :offen])))
        (is (= 1 (count (reads))))))))

(def ^:private kopf :ri.test.object-type.kopf)
(def ^:private zeile :ri.test.object-type.zeile)
(def ^:private zeilen :ri.test.link-type.zeilen)
(def ^:private posten :ri.test.object-type.posten)
(def ^:private posten-link :ri.test.link-type.posten)

(defn- chained-schema
  []
  {:object-types
   {kopf   {:object-type-name :kopf
            :attribute-list [{:attribute-name :summe :attribute-type :decimal
                              :opts {:derived {:expression [:connected-sum :zeilen :wert]}}}]
            :plural "Köpfe"}
    zeile  {:object-type-name :zeile
            :attribute-list [{:attribute-name :menge :attribute-type :decimal}
                             {:attribute-name :wert :attribute-type :decimal
                              :opts {:derived {:expression [:round [:get :menge]]}}}]
            :plural "Zeilen"}
    posten {:object-type-name :posten
            :attribute-list [{:attribute-name :betrag :attribute-type :decimal}]
            :plural "Posten"}}
   :link-types
   {zeilen {:link-type-name :zeilen :from-object-type kopf
            :to-object-type zeile :cardinality :one-to-many}
    posten-link {:link-type-name :posten :from-object-type zeile
                 :to-object-type posten :cardinality :one-to-many}}})

(deftest a-chain-is-planned-before-it-is-read-and-counts-no-levels
  (testing "what used to stand here was a depth cap: at six levels the read was refused,
            and that refusal asked the wrong question — how LONG the chain is, instead of
            how much one of its steps loads. The graph answers the first question before
            anything is read (ontology.derive-graph-test), so nothing here counts rounds"
    (let [schema (chained-schema)]
      (is (= {:levels 1
              :sides ["kopf.zeilen"]
              :properties ["zeile.wert"]}
             (views/chain-of schema kopf (vec (sd/derived-of schema kopf))))))))

(deftest a-fold-the-statement-states-is-no-chain-at-all
  (let [folding (fn [of] (-> (chained-schema)
                             (assoc-in [:object-types kopf :attribute-list 0 :opts :derived
                                        :expression]
                                       [:connected-sum :zeilen of])))]
    (testing "a fold onto a STORED far property is one SQL statement, so it is in no plan
              above the store and no bound of this evaluator applies to it"
      (let [schema (folding :menge)]
        (is (nil? (views/chain-of schema kopf (vec (sd/derived-of schema kopf))))
            "no level above the store, so nothing for a plan to lay out")
        (is (some? (sd/lower schema kopf :summe))
            "and the plan states the fold itself — one statement, as :sum over a column")))

    (testing "and a fold onto a far property the far type DERIVES from its own row is one
              statement too, since the plan folds that arithmetic itself (`:with :expr`) —
              which is what makes the chain above the store the exception it should be"
      (let [schema (-> (folding :wert)
                       (assoc-in [:object-types zeile :attribute-list 1 :opts :derived :expression]
                                 [:* [:get :menge] 2]))]
        (is (nil? (views/chain-of schema kopf (vec (sd/derived-of schema kopf))))
            "the far objects are never loaded, so the read spends no round on them")
        (is (= [:* :menge 2] (:expr (first (:steps (sd/lower schema kopf :summe)))))
            "the far row's arithmetic rides in the fold")))

    (testing "and so is a fold onto a far property that is itself a FOLD: the fold folds
              over a relation carrying it (`:far-in`), which is two grouped passes rather
              than a round of reads — the case prod is made of"
      (let [schema (-> (folding :wert)
                       (assoc-in [:object-types zeile :attribute-list 1 :opts :derived :expression]
                                 [:connected-sum :posten :betrag]))]
        (is (nil? (views/chain-of schema kopf (vec (sd/derived-of schema kopf)))))
        (is (= :sutra.store.derived/wert
               (:attr (first (:steps (sd/lower schema kopf :summe)))))
            "the outer fold folds a column of that relation")))))
