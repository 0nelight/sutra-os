(ns sutra.parser.board
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.types.value-types :as vt]))

(defn- sql-lit [v subst]
  (cond
    (string? v)  (str "'" (str/replace v "'" "''") "'")
    (keyword? v) (get subst v (name v))
    (boolean? v) (if v "TRUE" "FALSE")
    (nil? v)     "NULL"
    :else        (str v)))

(defn- sql-type
  [t]
  (case (keyword t)
    :string "TEXT" :text "TEXT"
    :int "BIGINT" :integer "BIGINT" :long "BIGINT"
    :float "NUMERIC" :double "NUMERIC" :decimal "NUMERIC"
    :boolean "BOOLEAN" :date "DATE" :datetime "TIMESTAMP" :timestamp "TIMESTAMP"
    "TEXT"))

(declare expr->sql)

(defn- infix [op args subst]
  (str "(" (str/join (str " " op " ") (map #(expr->sql % subst) args)) ")"))

(defn- contains->sql
  [[h & needles] subst]
  (let [hs (str "lower(" (expr->sql h subst) ")")
        one (fn [n]
              (if (string? n)
                (str hs " LIKE '%" (str/lower-case (str/replace n "'" "''")) "%'")
                (str hs " LIKE lower(CONCAT('%', " (expr->sql n subst) ", '%'))")))]
    (str "(" (str/join " OR " (map one needles)) ")")))

(defn expr->sql
  ([e] (expr->sql e nil))
  ([e subst]
   (if-not (vector? e)
     (sql-lit e subst)
     (let [[op & args] e
           s (fn [x] (expr->sql x subst))]
       (case op
         :get (let [k (first args)] (get subst k (name k)))
         :param (str "${" (name (first args)) "}")
         (:+ :- :*) (infix (name op) args subst)
         :div (str "(CAST(" (s (first args)) " AS NUMERIC) / NULLIF(" (s (second args)) ", 0))")
         :mod (str "MOD(" (s (first args)) ", " (s (second args)) ")")
         :abs (str "ABS(" (s (first args)) ")")
         (:> :< :>= :<=) (str "(" (s (first args)) " " (name op) " "
                              (s (second args)) ")")
         := (str "(" (s (first args)) " = " (s (second args)) ")")
         :!= (str "(" (s (first args)) " <> " (s (second args)) ")")
         :and (infix "AND" args subst)
         :or  (infix "OR" args subst)
         :not (str "(NOT " (s (first args)) ")")
         :in  (str "(" (s (first args)) " IN ("
                   (str/join ", " (map s (rest args))) "))")
         :coalesce (str "COALESCE(" (str/join ", " (map s args)) ")")
         :concat   (str "CONCAT(" (str/join ", " (map s args)) ")")
         :length   (str "LENGTH(" (s (first args)) ")")
         :split    (str "split_part(" (s (first args)) ", "
                        (s (second args)) ", " (s (nth args 2 1)) ")")
         :cast     (str "CAST(" (s (first args)) " AS " (sql-type (second args)) ")")
         :contains? (contains->sql args subst)
         (refusal/refuse! :refusal.parser/board-scalar-op {:op op :expr e}))))))

(defn expr-cols
  [e]
  (cond
    (keyword? e) #{e}
    (not (vector? e)) #{}
    (= :get (first e)) #{(second e)}
    :else (into #{} (mapcat expr-cols) (rest e))))

(def numeric-types
  (conj vt/numeric-types :int))

(defn- widest [ts]
  (let [ts (remove nil? ts)]
    (cond
      (empty? ts) nil
      (some #{:decimal :float :double :number} ts) :decimal
      (every? numeric-types ts) :long
      :else (first ts))))

(defn expr-type
  [types e]
  (cond
    (keyword? e) (get types e)
    (string? e) :string
    (boolean? e) :boolean
    (integer? e) :long
    (number? e) :decimal
    (not (vector? e)) nil
    :else
    (let [[op & args] e
          t (fn [x] (expr-type types x))]
      (case op
        :get (get types (first args))
        (:+ :- :* :mod :coalesce) (widest (map t args))
        :div :decimal
        :abs (t (first args))
        (:> :< :>= :<= := :!= :and :or :not :in :contains?) :boolean
        (:concat :split) :string
        :length :long
        :cast (keyword (second args))
        nil))))

(defn- agg-type [types {:keys [fn col expr]}]
  (case (keyword fn)
    :count :long
    :avg :decimal
    :sum (or (if expr (expr-type types expr) (get types col)) :decimal)
    (:min :max) (if expr (expr-type types expr) (get types col))
    nil))

(defn- norm-col [c]
  (cond
    (keyword? c) {:name c :type nil}
    (map? c) {:name (or (:name c) (:attribute-name c))
              :type (or (:type c) (:attribute-type c))}
    :else {:name (keyword (str c)) :type nil}))

(defn- col-types [cols] (into {} (map (juxt :name :type)) cols))

(defn- col-name [c] (if (keyword? c) (name c) (str c)))

(defn- wrap [prev alias] (str "(" (:sql prev) ") " alias))

(defn- fold-op
  [cols-fn i {psql :sql pcols :cols} op]
  (let [a     (str "_s" i)
        tmap  (col-types pcols)
        cn    (fn [c] (col-name (:name c)))
        typed (fn [c] {:name c :type (get tmap c)})]
    (case (:op op)
      :from
      (let [in (:input op)]
        {:sql   (str "SELECT * FROM " (name in))
         :table (name in)
         :cols  (mapv norm-col (cols-fn in))})

      :filter
      {:sql (str "SELECT * FROM " (wrap {:sql psql} a)
                 " WHERE " (expr->sql (:where op)))
       :cols pcols}

      :select
      (let [cs (mapv typed (:columns op))]
        {:sql  (str "SELECT " (str/join ", " (map cn cs))
                    " FROM " (wrap {:sql psql} a))
         :cols cs})

      :drop-columns
      (let [drop-set (set (:columns op))
            keep     (vec (remove (comp drop-set :name) pcols))]
        {:sql  (str "SELECT " (str/join ", " (map cn keep))
                    " FROM " (wrap {:sql psql} a))
         :cols keep})

      :rename
      (let [rn   (:renames op)
            proj (map (fn [c] (if-let [nw (get rn (:name c))]
                                (str (cn c) " AS " (col-name nw))
                                (cn c)))
                      pcols)]
        {:sql  (str "SELECT " (str/join ", " proj) " FROM " (wrap {:sql psql} a))
         :cols (mapv (fn [c] (update c :name #(get rn % %))) pcols)})

      :derive
      {:sql  (str "SELECT " a ".*, " (expr->sql (:expr op)) " AS " (col-name (:name op))
                  " FROM " (wrap {:sql psql} a))
       :cols (conj pcols {:name (:name op) :type (expr-type tmap (:expr op))})}

      :distinct
      {:sql  (str "SELECT DISTINCT * FROM " (wrap {:sql psql} a))
       :cols pcols}

      :sort
      (let [by (map (fn [[c dir]] (str (col-name c) " " (if (= :desc dir) "DESC" "ASC")))
                    (:by op))]
        {:sql  (str "SELECT * FROM " (wrap {:sql psql} a)
                    " ORDER BY " (str/join ", " by))
         :cols pcols})

      :join
      (let [r-in   (:input op)
            r-cols (mapv norm-col (cols-fn r-in))
            l "_l" r "_r"
            jt (case (:type op) :left "LEFT JOIN" :right "RIGHT JOIN" :full "FULL JOIN" "JOIN")
            on (str/join " AND "
                         (map (fn [[lc rc]] (str l "." (col-name lc) " = " r "." (col-name rc)))
                              (:on op)))
            lset  (set (map :name pcols))
            r-new (remove (comp lset :name) r-cols)
            proj  (concat (map (fn [c] (str l "." (cn c))) pcols)
                          (map (fn [c] (str r "." (cn c))) r-new))]
        {:sql  (str "SELECT " (str/join ", " proj)
                    " FROM " (wrap {:sql psql} l)
                    " " jt " " (name r-in) " " r " ON " on)
         :cols (vec (concat pcols r-new))})

      :union
      (let [r-in (:input op)
            all? (not= false (:all op))]
        {:sql  (str "(" psql ")" (if all? " UNION ALL " " UNION ")
                    "(SELECT * FROM " (name r-in) ")")
         :cols pcols})

      :aggregate
      (let [gb   (mapv typed (:group-by op))
            aggs (:aggs op)
            agg->sql (fn [{:keys [name fn col expr]}]
                       (let [inner (cond expr (expr->sql expr)
                                         (or (nil? col) (= :* col)) "*"
                                         :else (col-name col))]
                         (str (str/upper-case (clojure.core/name fn)) "(" inner ") AS "
                              (col-name name))))
            proj (concat (map cn gb) (map agg->sql aggs))]
        {:sql  (str "SELECT " (str/join ", " proj)
                    " FROM " (wrap {:sql psql} a)
                    (when (seq gb) (str " GROUP BY " (str/join ", " (map cn gb)))))
         :cols (vec (concat gb (map (fn [ag] {:name (:name ag) :type (agg-type tmap ag)}) aggs)))})

      (refusal/refuse! :refusal.parser/board-table-op {:op op} {:name (:op op)}))))

(defn- fold-board
  [inputs ops cols-fn]
  (let [resolve-cols (fn [alias] (cols-fn (get inputs alias alias)))]
    (loop [rel {:sql nil :cols []} i 0 steps []]
      (if (>= i (count ops))
        {:rel rel :steps steps}
        (let [op  (nth ops i)
              res (try {:ok (fold-op resolve-cols i rel op)}
                       (catch #?(:clj Throwable :cljs :default) e
                         {:error (or #?(:clj (.getMessage ^Throwable e)
                                        :cljs (.-message e))
                                     (str e))
                          :cause e}))]
          (if-let [err (:error res)]
            {:rel rel :steps steps :error err :error-index i :cause (:cause res)}
            (recur (:ok res) (inc i)
                   (conj steps {:index i :op (:op op) :cols (:cols (:ok res))}))))))))

(defn board->schema
  ([board] (board->schema board (constantly [])))
  ([{:keys [inputs ops]} cols-fn]
   (let [{:keys [rel steps error error-index]} (fold-board inputs (vec ops) cols-fn)]
     (cond-> {:steps steps :cols (:cols rel)}
       error (assoc :error error :error-index error-index)))))

(def rowwise-ops
  #{:filter :derive :select :drop-columns :rename})

(def ^:private order-destroying-ops #{:aggregate :distinct :sort :union})

(defn- filter-passable?
  [op cols]
  (case (:op op)
    :sort     true
    :distinct true
    (:select :drop-columns) true
    :derive   (not (contains? cols (:name op)))
    false))

(defn- push-filters
  [ops]
  (reduce
   (fn [acc op]
     (if (not= :filter (:op op))
       (conj acc op)
       (let [cols (expr-cols (:where op))
             i    (loop [i (count acc)]
                    (if (and (pos? i) (filter-passable? (nth acc (dec i)) cols))
                      (recur (dec i))
                      i))]
         (vec (concat (subvec acc 0 i) [op] (subvec acc i))))))
   [] ops))

(defn- drop-dead-order
  [ops]
  (vec (keep-indexed
        (fn [i op]
          (when-not (and (= :sort (:op op))
                         (some (comp order-destroying-ops :op) (drop (inc i) ops)))
            op))
        ops)))

(defn- drop-redundant-distinct
  [ops]
  (vec (keep-indexed
        (fn [i op]
          (when-not (and (= :distinct (:op op))
                         (contains? #{:aggregate :distinct} (:op (nth ops (dec i) nil))))
            op))
        ops)))

(def ^:private rewrites
  [[:push-filters push-filters]
   [:drop-dead-order drop-dead-order]
   [:drop-redundant-distinct drop-redundant-distinct]])

(defn optimize
  [ops]
  (reduce (fn [{:keys [ops rewrites] :as acc} [nm f]]
            (let [ops' (vec (f ops))]
              (if (= ops' ops) acc {:ops ops' :rewrites (conj rewrites nm)})))
          {:ops (vec ops) :rewrites []}
          rewrites))

(defn- lower-rowwise
  [cols-fn i rel ops]
  (let [from (or (:table rel) (str "(" (:sql rel) ") _s" i))
        [cols sqlof where]
        (reduce
         (fn [[cols sqlof where] op]
           (let [cols' (:cols (fold-op cols-fn i {:sql "" :cols cols} op))]
             (case (:op op)
               :derive [cols' (assoc sqlof (:name op) (expr->sql (:expr op) sqlof)) where]
               :filter [cols' sqlof (conj where (expr->sql (:where op) sqlof))]
               :rename [cols' (reduce (fn [m [old nw]]
                                        (assoc m nw (get m old (col-name old))))
                                      sqlof (:renames op))
                        where]
               [cols' sqlof where])))
         [(:cols rel)
          (into {} (map (fn [c] [(:name c) (col-name (:name c))])) (:cols rel))
          []]
         ops)
        identity? (every? (fn [c] (= (get sqlof (:name c)) (col-name (:name c)))) cols)
        proj (if (and identity? (= (map :name cols) (map :name (:cols rel))))
               "*"
               (str/join ", "
                         (map (fn [c]
                                (let [s (get sqlof (:name c) (col-name (:name c)))]
                                  (if (= s (col-name (:name c)))
                                    s
                                    (str s " AS " (col-name (:name c))))))
                              cols)))]
    {:sql  (str "SELECT " proj " FROM " from
                (when (seq where) (str " WHERE " (str/join " AND " where))))
     :cols cols}))

(defn- decorate
  [rel op]
  (case (:op op)
    :distinct
    (when-not (:distinct? rel)
      (assoc rel :sql (str/replace-first (:sql rel) #"(?i)^SELECT " "SELECT DISTINCT ")
             :distinct? true :table nil))

    :sort
    (when-not (:order? rel)
      (let [by (map (fn [[c dir]] (str (col-name c) " " (if (= :desc dir) "DESC" "ASC")))
                    (:by op))]
        (assoc rel :sql (str (:sql rel) " ORDER BY " (str/join ", " by))
               :order? true :table nil)))
    nil))

(defn- lower
  [cols-fn ops]
  (let [flush (fn [{:keys [rel pending n] :as acc}]
                (if (seq pending)
                  (assoc acc :rel (lower-rowwise cols-fn n rel pending) :pending [])
                  acc))]
    (:rel
     (flush
      (reduce
       (fn [{:keys [n] :as acc} op]
         (let [acc (assoc acc :n (inc n))]
           (cond
             (rowwise-ops (:op op))
             (update acc :pending conj op)

             :else
             (let [{:keys [rel] :as acc} (flush acc)]
               (if-let [dec (and (:sql rel) (decorate rel op))]
                 (assoc acc :rel dec)
                 (assoc acc :rel (fold-op cols-fn n rel op)))))))
       {:rel {:sql nil :cols []} :pending [] :n 0}
       ops)))))

(defn- count-selects [sql]
  (count (re-seq #"(?i)\bSELECT\b" (or sql ""))))

(defn board->sql
  ([board] (board->sql board (constantly [])))
  ([board cols-fn] (board->sql board cols-fn nil))
  ([{:keys [inputs ops output primary-key parameters]} cols-fn
    {:keys [optimize?] :or {optimize? true}}]
   (when (not= :from (:op (first ops)))
     (refusal/refuse! :refusal.parser/board-first-op {:ops ops}))
   (let [resolve-cols (fn [alias] (cols-fn (get inputs alias alias)))
         {:keys [rel steps error error-index cause]} (fold-board inputs (vec ops) cols-fn)]
     (when error
       (let [at {:op (:op (nth (vec ops) error-index nil)) :index error-index}
             d  (ex-data cause)]
         (if-let [code (:refusal/code d)]
           (refusal/refuse! code (merge d at) (:refusal/params d) cause)
           (refusal/refuse! :refusal.parser/board-op-failed at {:why error} cause))))
     (let [naive (:sql rel)
           {opt-ops :ops fired :rewrites} (optimize ops)
           query (if optimize?
                   (:sql (lower resolve-cols opt-ops))
                   naive)]
       (cond-> {:query query
                :inputs inputs
                :steps steps
                :plan {:rewrites fired
                       :ops-authored (count ops)
                       :ops-emitted (count opt-ops)
                       :selects-naive (count-selects naive)
                       :selects (count-selects query)}
                :output-columns (mapv (fn [{:keys [name type]}]
                                        {:attribute-name name
                                         :attribute-type (or type :string)})
                                      (:cols rel))}
         output      (assoc :output output)
         primary-key (assoc :primary-key primary-key)
         (seq parameters) (assoc :parameters parameters))))))

(def agg-fns
  [{:fn :count :label "Count"   :arg :optional :result :long}
   {:fn :sum   :label "Sum"     :arg :required :arg-types numeric-types}
   {:fn :avg   :label "Average" :arg :required :arg-types numeric-types :result :decimal}
   {:fn :min   :label "Minimum" :arg :required}
   {:fn :max   :label "Maximum" :arg :required}])

(def table-ops
  [{:op :from :label "From" :group :source
    :params [{:key :input :kind :input :required true}]}
   {:op :filter :label "Filter rows" :group :row
    :params [{:key :where :kind :expr :result :boolean :required true}]}
   {:op :join :label "Join" :group :combine
    :params [{:key :input :kind :input :required true}
             {:key :type :kind :enum :options [:inner :left :right :full] :default :inner}
             {:key :on :kind :join-on :required true}]}
   {:op :union :label "Union" :group :combine
    :params [{:key :input :kind :input :required true}
             {:key :all :kind :enum :options [true false] :default true}]}
   {:op :derive :label "Derive column" :group :column
    :params [{:key :name :kind :new-name :required true}
             {:key :expr :kind :expr :required true}]}
   {:op :select :label "Select columns" :group :column
    :params [{:key :columns :kind :columns :required true}]}
   {:op :drop-columns :label "Drop columns" :group :column
    :params [{:key :columns :kind :columns :required true}]}
   {:op :rename :label "Rename columns" :group :column
    :params [{:key :renames :kind :renames :required true}]}
   {:op :aggregate :label "Aggregate" :group :row
    :params [{:key :group-by :kind :columns}
             {:key :aggs :kind :aggs :required true}]}
   {:op :sort :label "Sort" :group :row
    :params [{:key :by :kind :sort-by :required true}]}
   {:op :distinct :label "Drop duplicates" :group :row :params []}])

(def expr-ops
  [{:op :get :label "Column value" :group :field :args [{:kind :column}] :result :column}
   {:op :param :label "Parameter" :group :field :args [{:kind :param}]}
   {:op :+ :label "Add" :group :arith :args [{:kind :expr}] :variadic true :result :numeric}
   {:op :- :label "Subtract" :group :arith :args [{:kind :expr}] :variadic true :result :numeric}
   {:op :* :label "Multiply" :group :arith :args [{:kind :expr}] :variadic true :result :numeric}
   {:op :div :label "Divide" :group :arith :args [{:kind :expr} {:kind :expr}] :result :decimal}
   {:op :mod :label "Remainder" :group :arith :args [{:kind :expr} {:kind :expr}] :result :numeric}
   {:op :abs :label "Absolute value" :group :arith :args [{:kind :expr}] :result :numeric}
   {:op :> :label "Greater than" :group :compare :args [{:kind :expr} {:kind :expr}]
    :result :boolean}
   {:op :< :label "Less than" :group :compare :args [{:kind :expr} {:kind :expr}] :result :boolean}
   {:op :>= :label "At least" :group :compare :args [{:kind :expr} {:kind :expr}] :result :boolean}
   {:op :<= :label "At most" :group :compare :args [{:kind :expr} {:kind :expr}] :result :boolean}
   {:op := :label "Equals" :group :compare :args [{:kind :expr} {:kind :expr}] :result :boolean}
   {:op :!= :label "Not equal" :group :compare :args [{:kind :expr} {:kind :expr}] :result :boolean}
   {:op :and :label "All of" :group :logic :args [{:kind :expr}] :variadic true :result :boolean}
   {:op :or :label "Any of" :group :logic :args [{:kind :expr}] :variadic true :result :boolean}
   {:op :not :label "Not" :group :logic :args [{:kind :expr}] :result :boolean}
   {:op :in :label "Is one of" :group :logic :args [{:kind :expr} {:kind :literal}] :variadic true
    :result :boolean}
   {:op :coalesce :label "First non-null" :group :logic :args [{:kind :expr}] :variadic true}
   {:op :concat :label "Concatenate strings" :group :string :args [{:kind :expr}] :variadic true
    :result :string}
   {:op :length :label "String length" :group :string :args [{:kind :expr}] :result :long}
   {:op :split :label "Split string" :group :string
    :args [{:kind :expr} {:kind :literal :label "delimiter"}
           {:kind :literal :label "part" :default 1}]
    :result :string}
   {:op :contains? :label "Contains text" :group :string
    :args [{:kind :expr} {:kind :literal :label "needle"}] :variadic true :result :boolean}
   {:op :cast :label "Cast" :group :convert
    :args [{:kind :expr}
           {:kind :enum :options [:string :long :decimal :boolean :date :timestamp]}]}])

(def ^:private table-op-index (coll/index-by :op table-ops))
(def ^:private expr-op-index (coll/index-by :op expr-ops))

(defn op-spec [op] (get table-op-index op))
(defn expr-op-spec [op] (get expr-op-index op))

(defn explain-op
  [op cols aliases]
  (let [spec  (op-spec (:op op))
        known (set (map :name cols))
        als   (set aliases)]
    (if-not spec
      {:error (str "unknown op " (pr-str (:op op)))}
      (some (fn [{:keys [key kind required]}]
              (let [v (get op key)]
                (cond
                  (and required (or (nil? v) (and (coll? v) (empty? v))))
                  {:error (str (name key) " is required") :param key}

                  (and (= :input kind) v (not (als v)))
                  {:error (str (name v) " is not an input of this board") :param key}

                  (and (= :column kind) v (seq known) (not (known v)))
                  {:error (str (name v) " is not a column here") :param key}

                  (and (= :columns kind) (seq known) (seq v))
                  (when-let [miss (first (remove known v))]
                    {:error (str (name miss) " is not a column here") :param key}))))
            (:params spec)))))

(defn output-removals
  [{:keys [remove-markings remove-organizations]}]
  (not-empty (cond-> {}
               (seq remove-markings)      (assoc :stop-propagating (vec remove-markings))
               (seq remove-organizations) (assoc :stop-requiring (vec remove-organizations)))))

(defn with-output-removals
  [inputs board]
  (if-let [rm (output-removals board)]
    (into (empty inputs)
          (map (fn [[alias in]]
                 [alias (if (map? in)
                          (merge-with (fn [a b] (vec (distinct (concat a b)))) in rm)
                          in)]))
          inputs)
    inputs))

(defn security-problems
  [board]
  (when (and (output-removals board) (not (:security-approvals board)))
    [{:problem :removal-without-security-approvals
      :removes (output-removals board)}]))
