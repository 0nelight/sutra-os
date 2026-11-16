(ns ^:unit apps.board-typing-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.board :as board]
            [sutra.parser.ent :as ent]
            [sutra.parser.ontology :as onto]
            [sutra.parser.spec :as spec]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.kernel :as kernel]
            [sutra.authoring.install :as install]
            [sutra.types.profile :as profile]))

(def ^:private entnahmen
  [{:name :id :type :string}
   {:name :menge :type :int}
   {:name :stueckpreis :type :decimal}
   {:name :ata :type :string}])

(def ^:private lagerorte
  [{:name :id :type :string}
   {:name :station :type :string}])

(def ^:private cols
  {{:kind :dataset :dataset :entnahmen} entnahmen
   {:kind :dataset :dataset :lagerorte} lagerorte})

(def ^:private board
  {:inputs {:a {:kind :dataset :dataset :entnahmen}
            :k {:kind :dataset :dataset :lagerorte}}
   :ops [{:op :from :input :a}
         {:op :filter :where [:> :menge 0]}
         {:op :derive :name :wert :expr [:* :menge :stueckpreis]}
         {:op :join :input :k :type :left :on [[:id :id]]}
         {:op :aggregate :group-by [:ata]
          :aggs [{:name :n :fn :count} {:name :s :fn :sum :col :wert}]}]})

(defn- typed [step] (into {} (map (juxt :name :type)) (:cols step)))

(deftest input-types-reach-the-output
  (let [{:keys [output-columns]} (board/board->sql board cols)]
    (is (= [{:attribute-name :ata :attribute-type :string}
            {:attribute-name :n :attribute-type :long}
            {:attribute-name :s :attribute-type :decimal}]
           output-columns)
        "count is a long, a sum over a decimal stays decimal, a group key keeps its own type")))

(deftest inferred-types-are-authorable-value-types
  (testing "every type the inference can mint is one an .ent attribute may carry —
            an intermediate board's :output-columns go through the same spec check"
    (let [legal (set (rest @#'onto/ScalarType))
          t     {:menge :long :stueckpreis :decimal :name :string}
          minted (into #{(:attribute-type (first (:output-columns (board/board->sql board cols))))}
                       (concat
                        (keep #(board/expr-type t %)
                              [[:+ :menge 1] [:* :menge :stueckpreis] [:div :menge :menge]
                               [:> :menge 0] [:concat :name "x"] [:length :name]
                               [:coalesce :name "x"] [:abs :menge] :menge])
                        (map :type (:cols (board/board->schema board cols)))
                        (keep :result board/agg-fns)
                        (keep :result board/expr-ops)
                        (:options (second (:args (board/expr-op-spec :cast))))))]
      (is (= #{} (disj (into #{} (remove legal) minted) :numeric :column))
          "…apart from the catalog's two abstract result markers"))))

(deftest untyped-cols-fn-still-compiles
  (testing "a caller that hands back bare keywords gets the old :string default"
    (let [{:keys [output-columns query]}
          (board/board->sql {:inputs {:a {:kind :dataset :dataset :entnahmen}}
                         :ops [{:op :from :input :a} {:op :select :columns [:id]}]}
                        (constantly [:id :menge]))]
      (is (= [{:attribute-name :id :attribute-type :string}] output-columns))
      (is (re-find #"SELECT id FROM" query)))))

(deftest every-step-names-its-own-relation
  (let [{:keys [steps]} (board/board->schema board cols)]
    (is (= [:from :filter :derive :join :aggregate] (mapv :op steps)))
    (is (= {:id :string :menge :int :stueckpreis :decimal :ata :string} (typed (first steps))))
    (is (= :decimal (:wert (typed (nth steps 2))))
        "a derived quotient/product is typed by its operands, not by the column it lands in")
    (is (= :string (:station (typed (nth steps 3))))
        "the joined side contributes its columns with their types")))

(deftest a-broken-op-keeps-the-steps-before-it
  (let [r (board/board->schema (update board :ops conj {:op :nonsense}) cols)]
    (is (= 5 (count (:steps r))))
    (is (= 5 (:error-index r)))
    (is (re-find #"unknown table op" (:error r)))))

(deftest expression-types
  (let [t {:menge :int :stueckpreis :decimal :name :string}]
    (is (= :long (board/expr-type t [:+ :menge 1])))
    (is (= :decimal (board/expr-type t [:* :menge :stueckpreis])))
    (is (= :decimal (board/expr-type t [:div :menge :menge])) "a quotient is never an int")
    (is (= :boolean (board/expr-type t [:> :menge 0])))
    (is (= :string (board/expr-type t [:concat :name "-x"])))
    (is (= :long (board/expr-type t [:length :name])))
    (is (= :decimal (board/expr-type t [:cast :name :decimal])))
    (is (= :string (board/expr-type t :name)))
    (is (nil? (board/expr-type t :unknown-column)))))

(deftest the-catalog-covers-what-the-compiler-lowers
  (testing "every op the editor can offer is an op board->sql knows"
    (let [known (set (map :op board/table-ops))]
      (is (= known
             (into #{} (map :op)
                   (filter #(some? (board/op-spec (:op %))) board/table-ops))))
      (is (contains? known :drop-columns))
      (is (contains? known :rename))))
  (testing "every catalogued expression compiles"
    (doseq [{:keys [op]} board/expr-ops
            :let [e (case op
                      :get [:get :a]
                      :cast [:cast :a :string]
                      :split [:split :a "," 1]
                      :contains? [:contains? :a "x"]
                      :not [:not :a]
                      :in [:in :a 1 2]
                      (:abs :length) [op :a]
                      [op :a :b])]]
      (is (string? (board/expr->sql e)) (str op " lowers to SQL")))))

(deftest explain-op-names-the-hole
  (let [relation [{:name :ata :type :string}]]
    (is (nil? (board/explain-op {:op :select :columns [:ata]} relation [:a])))
    (is (= :columns (:param (board/explain-op {:op :select :columns [:nope]} relation [:a]))))
    (is (= :where (:param (board/explain-op {:op :filter} relation [:a])))
        "a required param that is missing is reported before any SQL is built")
    (is (= :input (:param (board/explain-op {:op :from :input :zzz} [] [:a]))))
    (is (some? (board/explain-op {:op :teleport} relation [:a])))))

(defn- q [brd] (:query (board/board->sql brd cols)))
(defn- naive [brd] (:query (board/board->sql brd cols {:optimize? false})))
(defn- selects [sql] (count (re-seq #"(?i)\bSELECT\b" sql)))

(deftest rowwise-run-becomes-one-select
  (let [{:keys [query plan]} (board/board->sql board cols)]
    (is (= 3 (:selects plan)) "filter+derive fuse; join and aggregate keep their own")
    (is (= 5 (:selects-naive plan)) "…where one-SELECT-per-op needed five")
    (is (re-find #"FROM a WHERE" query)
        "the first op reads its input table directly, not through SELECT * FROM")))

(deftest a-sibling-derivation-is-inlined-not-aliased
  (testing "SQL cannot reference a select alias from another expression in the
            same SELECT, so a filter on a derived column carries its expression"
    (let [brd {:inputs {:a {:kind :dataset :dataset :entnahmen}}
               :ops [{:op :from :input :a}
                     {:op :derive :name :wert :expr [:* :menge :stueckpreis]}
                     {:op :filter :where [:> :wert 5]}]}]
      (is (re-find #"WHERE \(\(menge \* stueckpreis\) > 5\)" (q brd)))
      (is (= 1 (selects (q brd)))))))

(deftest filters-move-up-only-where-that-cannot-change-the-answer
  (let [brd {:inputs {:a {:kind :dataset :dataset :entnahmen}}
             :ops [{:op :from :input :a}
                   {:op :sort :by [[:menge :desc]]}
                   {:op :filter :where [:> :menge 1]}]}]
    (is (= [:push-filters] (:rewrites (:plan (board/board->sql brd cols))))))
  (testing "a filter reading a derived column stays put — moving it would read
            a column that does not exist yet"
    (let [brd {:inputs {:a {:kind :dataset :dataset :entnahmen}}
               :ops [{:op :from :input :a}
                     {:op :derive :name :wert :expr [:* :menge :stueckpreis]}
                     {:op :filter :where [:> :wert 5]}]}]
      (is (= [] (:rewrites (:plan (board/board->sql brd cols)))))
      (is (= [:from :derive :filter] (mapv :op (:ops (board/optimize (:ops brd)))))))))

(deftest orderings-and-dedups-that-nobody-can-observe-are-dropped
  (let [ops [{:op :from :input :a}
             {:op :sort :by [[:menge :desc]]}
             {:op :aggregate :group-by [:ata] :aggs [{:name :n :fn :count}]}
             {:op :distinct}]
        {opt :ops fired :rewrites} (board/optimize ops)]
    (is (= [:from :aggregate] (mapv :op opt)))
    (is (= [:drop-dead-order :drop-redundant-distinct] fired)))
  (testing "a sort nobody destroys survives"
    (let [ops [{:op :from :input :a} {:op :sort :by [[:menge :desc]]}]]
      (is (= ops (:ops (board/optimize ops)))))))

(deftest sort-and-distinct-decorate-the-select-they-follow
  (let [brd {:inputs {:a {:kind :dataset :dataset :entnahmen}}
             :ops [{:op :from :input :a} {:op :distinct}]}]
    (is (= "SELECT DISTINCT * FROM a" (q brd)))
    (is (= 2 (selects (naive brd))))))

(deftest the-plan-never-claims-work-it-did-not-do
  (let [brd {:inputs {:a {:kind :dataset :dataset :entnahmen}}
             :ops [{:op :from :input :a} {:op :select :columns [:id]}]}
        {:keys [plan]} (board/board->sql brd cols)]
    (is (= [] (:rewrites plan)))
    (is (= (:ops-authored plan) (:ops-emitted plan)))))

(deftest optimizing-does-not-change-the-declared-output
  (doseq [brd [board
               {:inputs {:a {:kind :dataset :dataset :entnahmen}}
                :ops [{:op :from :input :a}
                      {:op :derive :name :wert :expr [:* :menge :stueckpreis]}
                      {:op :rename :renames {:ata :kapitel}}
                      {:op :sort :by [[:wert :desc]]}]}]]
    (is (= (:output-columns (board/board->sql brd cols {:optimize? false}))
           (:output-columns (board/board->sql brd cols)))
        "the optimizer moves the plan, never the contract")))

(deftest column-profile
  (let [rows [{:n 1 :s "x "} {:n 3 :s "y"} {:n nil :s "x"}]
        [n s] (profile/profile rows [:n :s])]
    (is (= {:count 3 :nulls 1 :distinct 2} (select-keys n [:count :nulls :distinct])))
    (is (= [1.0 3.0 2.0] [(:min n) (:max n) (:mean n)]))
    (is (nil? (:min s)) "a string column gets no numeric summary")
    (is (= 1 (:whitespace-edged s)) "the trailing blank that makes a join miss is visible")
    (is (= 3 (:count s)))))

(def ^:private entp-file
  {:name       :materialkosten
   :kind       :batch
   :imports    [:entnahmen]
   :transforms {:shape-orders
                {:board {:inputs {:a {:kind :dataset :dataset :entnahmen}}
                         :ops    [{:op :from   :input :a}
                                  {:op :filter :where [:> :menge 0]}
                                  {:op :derive :name :wert :expr [:* :menge :stueckpreis]}
                                  {:op :select :columns [:id :wert]}]}}}})

(def ^:private live-with-orders
  {:object-types {} :link-types {}
   :datasets {"ri.d.entnahmen"
              {:id "ri.d.entnahmen" :dataset-name :entnahmen :kind :standalone
               :columns [{:attribute-name :id :attribute-type :string}
                         {:attribute-name :menge :attribute-type :long}
                         {:attribute-name :stueckpreis :attribute-type :decimal}]}}})

(deftest a-board-only-entp-member-installs-as-a-valid-transform
  (let [desired (:schema (ent/parse-entp entp-file :inline))
        plan    (install/plan-schema desired live-with-orders {})
        entry   (first (filter #(= :shape-orders (:entity %)) plan))
        args    (:args entry)]
    (testing "the reconcile stages a create for the member"
      (is (= :transform (:kind entry)))
      (is (= :create-transform (:action entry))))
    (testing "…with the query COMPILED from the board (nobody authored one)"
      (is (re-find #"\(menge \* stueckpreis\) AS wert" (:query args)))
      (is (re-find #"WHERE \(menge > 0\)" (:query args))))
    (testing "…and the board's inputs lifted onto the transform: a board names its
              inputs by the alias its SQL selects FROM, so the two are one fact"
      (is (= {:a {:kind :dataset :dataset :entnahmen}} (:inputs args))))
    (testing "…which together make it a transform the CANONICAL layer accepts —
              the assertion that failed twice. Asserted through the kernel adapter
              rather than on the wire args: `transform-valid?` judges the canonical
              spec, and the args are AUTHOR vocabulary (`:output-columns` as pairs),
              so validating them directly would test the wrong shape."
      (let [canon (:value (kernel/step nil :create :transform (assoc args :name :shape-orders)))]
        (is (spec/transform-valid? canon))
        (is (= 1 (count (filter canon [:output :outputs :output-columns])))
            "output-mutex: exactly one output field, here the compiler's columns")))
    (testing "and the member carries its pipeline membership through the plan"
      (is (= :materialkosten (:pipeline args))))))

(deftest recompile-needs-no-schema-to-lower-named-columns
  (testing "the lowering is column-AGNOSTIC where the ops name their columns, so an
            input the schema cannot resolve still compiles — and the missing dataset
            is then reported by the reference gates rather than by the compiler"
    (let [body {:query "SELECT 1" :inputs {:z {:kind :dataset :dataset :nope}}
                :board {:inputs {:z {:kind :dataset :dataset :nope}}
                        :ops [{:op :from :input :z} {:op :select :columns [:gone]}]}}]
      (is (= "SELECT gone FROM z" (:query (outputs/recompile-board {} body))))))
  (testing "…but a board the compiler REJECTS leaves the authored query standing"
    (let [body {:query "SELECT 1"
                :board {:inputs {:z {:kind :dataset :dataset :nope}}
                        :ops [{:op :filter :where [:> :x 0]}]}}]
      (is (= "SELECT 1" (:query (outputs/recompile-board {} body)))))))

(deftest expressions-lower-to-postgres
  (is (= "split_part(a, ',', 2)" (board/expr->sql [:split :a "," 2])))
  (is (= "CAST(a AS BIGINT)" (board/expr->sql [:cast :a :long])))
  (is (= "CAST(a AS TIMESTAMP)" (board/expr->sql [:cast :a :timestamp])))
  (is (= "(CAST(a AS NUMERIC) / NULLIF(b, 0))" (board/expr->sql [:div :a :b]))))
