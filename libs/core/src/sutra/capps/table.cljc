(ns sutra.capps.table
  (:require [sutra.kit.coerce :refer [->kw]]
            [clojure.string :as str]
            [sutra.capps.format :as fmt :refer [v]]
            [sutra.ref.expr :as rexpr]
            [sutra.types.decimal :as d]
            [sutra.types.value-types :as vt]
            [sutra.capps.variables :refer [where-ops]]))

(defn react-safe
  [x]
  (if (d/exact? x) (d/plain x) x))

(defn- places
  [n]
  (fn [x ctx] (fmt/fmt x n ctx)))

(defn- percent-places
  [n]
  (fn [x ctx] (fmt/prozent x n ctx)))

(def formats
  {:eur      fmt/eur      :eur2    fmt/eur          :eur1  fmt/eur1   :eur0  fmt/eur0
   :menge    fmt/menge    :num     fmt/menge
   :num0     (places 0)   :num1    (places 1)       :num2  (places 2) :num3 (places 3)
   :int      (places 0)
   :pct      fmt/prozent  :prozent fmt/prozent
   :pct0     (percent-places 0) :prozent0 (percent-places 0)
   :pct2     (percent-places 2) :prozent2 (percent-places 2)
   :datum    fmt/datum    :monat   fmt/monat-name
   :uhrzeit  (fn [x _] (fmt/dezimalstunde x))
   :zeitpunkt (fn [x ctx] (fmt/temporal-text x {:style :datetime-short} ctx))
   :bool     (fn [x _] (cond (nil? x) nil x "Ja" :else "Nein"))
   :text     (fn [x _] (when (some? x) (str x)))
   :auto     fmt/auto})

(defn fmt-by
  [kind]
  (if (fn? kind)
    kind
    (or (get formats (->kw kind)) fmt/auto)))

(defn fmt-problem
  [kind]
  (when-not (fn? kind)
    (when-let [k (->kw kind)]
      (when-not (contains? formats k)
        (str "[capp/fmt] unknown :fmt " (pr-str k)
             " — falling back to :auto. Known: "
             (pr-str (vec (sort (keys formats)))))))))

(defn humanize
  [prop]
  (when prop
    (let [s (str/replace (name prop) "-" " ")]
      (str (str/upper-case (subs s 0 1)) (subs s 1)))))

(defn prop-display
  [entry prop]
  (when entry
    (let [t (->kw (:type entry))
          f (or (fmt/by-spec (:format entry))
                (case t
                  :date      fmt/datum
                  :timestamp (fn [x ctx] (fmt/temporal-text x {:style :datetime-short} ctx))
                  :geoshape  fmt/geoshape
                  nil))]
      (cond-> {:label (or (not-empty (str (:name entry))) (humanize prop))}
        f                                          (assoc :fmt f)
        (contains? vt/numeric-types (->kw (:type entry))) (assoc :align :right)))))

(defn- rule-env
  [values]
  (reduce-kv (fn [m k x] (assoc m (->kw k) (if (d/exact? x) (d/host-number x) x)))
             {} (or values {})))

(defn cond-format
  [rules values]
  (when (seq rules)
    (let [env (rule-env values)]
      (some (fn [{w :when :as rule}]
              (when (or (nil? w)
                        (try (rexpr/pred env w)
                             (catch #?(:clj Exception :cljs :default) _ false)))
                (not-empty (select-keys rule [:color :background :icon :title]))))
            rules))))

(defn prop-look
  [entry prop values]
  (let [d (prop-display entry prop)]
    (cond-> {:label (or (:label d) (humanize prop))
             :fmt   (or (:fmt d) fmt/auto)}
      (seq (:conditional-format entry))
      (assoc :look (cond-format (:conditional-format entry) values)))))

(defn with-prop-display
  [cols props]
  (if (empty? props)
    cols
    (mapv (fn [{:keys [prop] :as col}]
            (if-let [d (and prop (prop-display (or (get props prop)
                                                   (get props (name prop)))
                                               prop))]
              (reduce-kv (fn [col k v] (if (some? (get col k)) col (assoc col k v))) col d)
              col))
          cols)))

(defn badge-spec
  [table prop row]
  (let [raw (get (v row) prop)]
    (or (get table raw)
        (get table ((:truthy where-ops) raw nil))
        (:default table))))

(def list-roles #{:title :trailing :sub :hide})

(def ^:private trailing-fmts
  #{:eur :eur2 :eur1 :eur0 :menge :num :num0 :num1 :num2 :num3 :int})

(def ^:private date-fmts #{:datum :zeitpunkt :monat})

(defn- title-cell? [c] (some-> (:cell-class c) (str/includes? "capp-table__td--title")))

(defn list-role-of
  [cols]
  (let [own      (mapv #(list-roles (->kw (:list-role %))) cols)
        free     (fn [taken pred]
                   (some (fn [[i c]] (when (and (nil? (own i)) (not= i taken) (pred c)) i))
                         (map-indexed vector cols)))
        trailing (when-not (some #{:trailing} own)
                   (or (free nil #(or (= :right (->kw (:align %)))
                                      (trailing-fmts (->kw (:fmt %)))))
                       (free nil #(date-fmts (->kw (:fmt %))))))
        title    (when-not (some #{:title} own)
                   (or (free trailing title-cell?)
                       (free trailing #(not (or (:badge %)
                                                (date-fmts (->kw (:fmt %)))
                                                (trailing-fmts (->kw (:fmt %))))))
                       (free trailing (complement :badge))))]
    (vec (map-indexed (fn [i _] (or (own i)
                                    (cond (= i trailing) :trailing
                                          (= i title)    :title
                                          :else          :sub)))
                      cols))))

(defn with-list-roles
  [cols]
  (mapv (fn [c role]
          (update c :cell-class #(str/trim (str % " capp-li--" (name role)))))
        cols (list-role-of cols)))

(defn cell-value
  [{:keys [prop]} row]
  (when prop (get (v row) prop)))

(defn cell-text
  [{:keys [badge fmt] :as col} row ctx]
  (when-not badge
    ((fmt-by fmt) (cell-value col row) ctx)))

(defn cmp-vals
  [a b]
  (let [[ca cb] (fmt/compare-operands a b)]
    (cond (and ca cb)             (compare ca cb)
          (and (nil? a) (nil? b)) 0
          (nil? a)                1
          (nil? b)                -1
          :else                   (compare (str a) (str b)))))

(defn- sort-keys
  [sort]
  (keep (fn [s] (cond (or (keyword? s) (string? s)) [(->kw s) :asc]
                      (and (sequential? s) (or (keyword? (first s)) (string? (first s))))
                      [(->kw (first s)) (or (->kw (second s)) :asc)]))
        sort))

(defn- operand [x] [x (fmt/temporal x) (delay (fmt/calc x))])

(defn- cmp-operands
  [[a ta na] [b tb nb]]
  (cond (and ta tb)             (compare ta tb)
        (and @na @nb)           (compare @na @nb)
        (and (nil? a) (nil? b)) 0
        (nil? a)                1
        (nil? b)                -1
        :else                   (compare (str a) (str b))))

(defn- row-key [ks row] (mapv (fn [[prop _]] (operand (get (v row) prop))) ks))

(defn- key-cmp
  [ks]
  (fn [k1 k2]
    (loop [[[_ dir] & more] ks [o1 & r1] k1 [o2 & r2] k2]
      (if (nil? dir) 0
          (let [c (cmp-operands o1 o2)
                c (if (= dir :desc) (- c) c)]
            (if (zero? c) (recur more r1 r2) c))))))

(defn- sort-rows-by
  [ks key-of rows]
  (let [cmp (key-cmp ks)]
    (->> rows
         (mapv (fn [r] [(some->> (key-of r) (row-key ks)) r]))
         (sort-by first (fn [k1 k2]
                          (cond (and (nil? k1) (nil? k2)) 0
                                (nil? k1)                 1
                                (nil? k2)                 -1
                                :else                     (cmp k1 k2))))
         (mapv second))))

(defn sort-limit-rows
  [rows sort limit]
  (let [ks   (sort-keys sort)
        rows (if (seq ks) (sort-rows-by ks identity rows) (vec rows))]
    (if (and limit (number? limit) (pos? limit)) (vec (take limit rows)) rows)))

(defn pivot-agg [agg nums]
  (if (fn? agg)
    (agg nums)
    (case (->kw agg)
      :count (count nums)
      :avg   (if (seq nums) (/ (reduce + 0 nums) (count nums)) 0)
      :min   (when (seq nums) (reduce min nums))
      :max   (when (seq nums) (reduce max nums))
      (reduce + 0 nums))))

(defn field-fn
  [k]
  (cond
    (fn? k)                       k
    (or (keyword? k) (string? k)) (let [k (->kw k)]
                                    (fn [item]
                                      (if (contains? item k) (get item k) (get (v item) k))))
    :else                         nil))

(defn pivot
  [{:keys [rows row col value agg row-label col-label] :or {agg :sum}}]
  (let [row    (field-fn row)
        col    (or (field-fn col) (constantly nil))
        value  (or (field-fn value) (constantly 1))
        obj?   (fn [k] (and (map? k) (contains? k :id)))
        idf    (fn [k] (if (obj? k) (:id k) k))
        by-group (fn [f] (into {} (map (fn [it] [(idf (f it)) it])) rows))
        rrow   (by-group row)
        crow   (by-group col)
        labf   (fn [user-lab k source]
                 (cond (obj? k)  (or (:title k) (str (:id k)))
                       (fn? user-lab) (user-lab k)
                       user-lab  ((field-fn user-lab) (get source (idf k)))
                       :else     (str k)))
        cells  (group-by (fn [it] [(idf (row it)) (idf (col it))]) rows)
        rrep   (into {} (map (fn [it] [(idf (row it)) (row it)])) rows)
        crep   (into {} (map (fn [it] [(idf (col it)) (col it)])) rows)
        rkeys  (vec (sort (distinct (map (comp idf row) rows))))
        ckeys  (vec (sort (distinct (map (comp idf col) rows))))
        vals   (fn [ks] (mapcat (fn [k] (map value (get cells k []))) ks))
        at     (fn [rk ck] (pivot-agg agg (map value (get cells [rk ck] []))))]
    {:col-keys   ckeys
     :col-labels (mapv (fn [ck] (labf col-label (crep ck) crow)) ckeys)
     :row-keys   rkeys
     :row-labels (mapv (fn [rk] (labf row-label (rrep rk) rrow)) rkeys)
     :cells      (mapv (fn [rk] (mapv (fn [ck] (at rk ck)) ckeys)) rkeys)
     :row-totals (mapv (fn [rk] (pivot-agg agg (vals (map (fn [ck] [rk ck]) ckeys)))) rkeys)
     :col-totals (mapv (fn [ck] (pivot-agg agg (vals (map (fn [rk] [rk ck]) rkeys)))) ckeys)
     :grand      (pivot-agg agg (map value rows))}))

(defn compare-rows
  [base by-scenario segs key-fn]
  (let [k*      (fn [row] (some-> (key-fn row) str))
        base-ix (into {} (keep (fn [r] (when-let [k (k*  r)] [k r]))) base)
        seg-ix  (into {} (map (fn [s] [s (into {} (keep (fn [r] (when-let [k (k* r)] [k r])))
                                               (get by-scenario s))])) segs)
        base-ks (into [] (keep k*) base)
        extra   (into [] (comp (mapcat (fn [s] (keep k* (get by-scenario s))))
                               (remove (set base-ks))
                               (distinct))
                      segs)]
    (mapv (fn [k]
            {:key  k
             :base (get base-ix k)
             :by   (into {} (map (fn [s] [s (get-in seg-ix [s k])])) segs)})
          (into base-ks extra))))

(defn compare-changed?
  [col rows seg ctx]
  (let [f (fmt-by (:fmt col))
        t (fn [row] (when row (str (f (cell-value col row) ctx))))]
    (boolean (some (fn [r]
                     (let [b (:base r) s (get (:by r) seg)]
                       (if (or (nil? b) (nil? s))
                         true
                         (not= (t b) (t s)))))
                   rows))))

(defn compare-tag-rows
  [base by-scenario labels field base-label]
  (let [tag (fn [row label]
              (if (map? (:values row))
                (assoc-in row [:values field] label)
                (assoc row field label)))]
    (into (mapv #(tag % base-label) base)
          (mapcat (fn [[seg rows]] (mapv #(tag % (get labels seg (str seg))) rows)))
          by-scenario)))

(defn compare-sort
  [rows sort]
  (let [ks (sort-keys sort)]
    (if-not (seq ks)
      (vec rows)
      (sort-rows-by ks :base rows))))

(defn active-row
  [a]
  (cond (map? a) a (coll? a) (first a) :else nil))

(defn active-sync
  [held rows auto?]
  (when (and (nil? (active-row held)) auto? (seq rows))
    [(first rows)]))
