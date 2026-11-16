(ns sutra.store.pg.rows
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.store.lib :as lib]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [sutra.store.scan :as scan]
            [sutra.store.semantics :as sem]
            [sutra.types.value-types :as vt])
  (:import (java.time Instant LocalDateTime ZoneOffset)
           (java.time.temporal ChronoUnit)))

(set! *warn-on-reflection* true)

(defn- markings-of
  [m]
  (not-empty (into #{} (comp (remove str/blank?) (map keyword)) (some-> m str (str/split #",")))))

(defn- encode-markings [ms]
  (when (seq ms) (str "," (str/join "," (sort (map name ms))) ",")))

(def ^:private pk-k (keyword layout/pk))
(def ^:private markings-k (keyword layout/markings))

(defn- typed-json
  [t v]
  (let [head (if (vector? t) (first t) t)]
    (cond
      (nil? v) v
      (and (= :array head) (sequential? v))  (mapv #(typed-json (second t) %) v)
      (and (= :vector head) (sequential? v)) (mapv #(if (number? %) (double %) %) v)
      (and (= :struct head)
           (map? v))        (reduce-kv (fn [m k ft]
                                         (cond-> m (contains? m k) (update k #(typed-json ft %))))
                                       v (second t))
      (not (number? v))                      v
      (= :decimal head)                      (bigdec v)
      (#{:double :float} head)               (double v)
      (#{:integer :long :short :byte} head)  (long v)
      :else                                  v)))

(defn- json-or-text
  [^String s]
  (try (json/read-str s :key-fn keyword :bigdec true)
       (catch Exception _ s)))

(defn decoder
  [sig T]
  (into {} (for [{attribute-name :name attribute-type :type} (layout/attrs sig T)]
             [(keyword (layout/column attribute-name))
              [attribute-name
               (if (layout/json-attribute? attribute-type)
                 #(typed-json attribute-type (if (string? %) (json-or-text %) %))
                 identity)]])))

(defn ->row
  [decode r]
  (let [ms (markings-of (get r markings-k))]
    (cond-> {:id     (str (get r pk-k))
             :values (persistent!
                      (reduce-kv (fn [m k v]
                                   (if-let [[a f] (decode k)]
                                     (assoc! m a (some-> v sql/value f))
                                     m))
                                 (transient {}) r))}
      ms (assoc :markings ms))))

(defn- param [v] (sql/->sql-param v))

(defn rows
  [c {:keys [ns sig as-of view]} T ids]
  (if (and ids (empty? ids))
    []
    (when-let [{rel :sql ps :params} (layout/relation view ns (layout/object-table T) as-of)]
      (mapv (partial ->row (decoder sig T))
            (sql/rows c
                      (str "SELECT * FROM " rel " q"
                           (when ids (str " WHERE q." (sql/dq layout/pk) " = ANY(?)")))
                      (cond-> ps ids (conj (sql/text-array ids))))))))

(defn- tuple-of
  [[op a b :as p]]
  (case op
    := (when (some? b) [[a] [b]])
    :and (let [ts (map tuple-of (rest p))]
           (when (and (seq ts) (every? some? ts))
             [(into [] (mapcat first) ts) (into [] (mapcat second) ts)]))
    nil))

(def ^:private json-heads #{:struct :vector})

(defn- class-of-type
  [t]
  (let [head (if (vector? t) (first t) t)]
    (cond
      (= :array head)            (let [c (class-of-type (second t))]
                                   (if (#{:num :text} c) [:array c] :json))
      (json-heads head)          :json
      (= :decimal head)          :num
      (= :float t)               nil
      (= :number (vt/class-of t)) :num
      (#{:text :keyword :uuid :geo :media :series} (vt/class-of t)) :text
      (= :boolean (vt/class-of t)) :bool
      (#{:instant :timestamp} t) :inst)))

(defn- classes
  [sig T]
  (into (zipmap scan/key-attrs (repeat :key))
        (map (juxt :name (comp class-of-type :type)))
        (layout/attrs sig T)))

(defn- value-class
  [v]
  (let [v (sem/canon v)]
    (cond (and (float? v) (Double/isNaN (double v))) nil
          (ratio? v)                                nil
          (number? v)                               :num
          (string? v)                               :text
          (boolean? v)                              :bool
          (inst? v)                                 :inst)))

(defn- compares?
  [c v]
  (let [vc (value-class v)]
    (and vc (if (= :key c) (= :text vc) (= c vc)))))

(defn- bound
  [v]
  (let [v (sem/canon v)]
    (cond (instance? java.sql.Timestamp
                     v)  (.truncatedTo (.toLocalDateTime ^java.sql.Timestamp v) ChronoUnit/MILLIS)
          (inst? v)
          (LocalDateTime/ofInstant (Instant/ofEpochMilli (inst-ms v)) ZoneOffset/UTC)
          (instance? clojure.lang.BigInt v) (bigdec v)
          (instance? java.math.BigInteger v) (bigdec v)
          :else                            v)))

(defn- sql-pred?
  [cls [op a b :as p]]
  (case op
    (:and :or) (every? (partial sql-pred? cls) (rest p))
    :not       (sql-pred? cls a)
    (let [c (get cls a)]
      (and (some? c)
           (case op
             :nil?                       true
             (:= :not= :< :<= :>
                 :>=)    (and (not= :json c) (not (vector? c)) (or (nil? b) (compares? c b)))
             :in                         (and (not= :json c) (not (vector? c))
                                              (every? #(compares? c %) (remove nil? b)))
             :starts-with                (and (#{:text :key} c) (some? b))
             :has                        (and (vector? c) (some? b) (not (coll? b))
                                              (case (second c)
                                                :num
                                                (and (integer? b)
                                                     (not (instance? clojure.lang.BigInt b)))
                                                :text (= :text (value-class b))))
             false)))))

(defn- col-sql [a] (str "q." (sql/dq (if (scan/key-attrs a) layout/pk (layout/column a)))))

(defn- ordered-col
  [cls a]
  (case (get cls a)
    :inst        (str "date_trunc('milliseconds', " (col-sql a) ")")
    (col-sql a)))

(defn- pred-sql
  [cls [op a b :as p]]
  (let [junction (fn [j ps] (case (count ps)
                              0 [(if (= " AND " j) "TRUE" "FALSE") []]
                              1 (pred-sql cls (first ps))
                              (let [xs (map (partial pred-sql cls) ps)]
                                [(str "(" (str/join j (map first xs)) ")")
                                 (into [] (mapcat second) xs)])))
        tuples   (when (= :or op) (let [ts (map tuple-of (rest p))]
                                    (when (and (seq ts) (every? some? ts) (apply = (map first ts)))
                                      ts)))
        cmp      (fn [sym]
                   (if (nil? b) ["FALSE" []] [(str (ordered-col cls a) " " sym " ?") [(bound b)]]))]
    (case op
      :and  (junction " AND " (rest p))
      :or   (if-let [[[as] :as ts] tuples]
              [(str "(" (str/join ", " (map (partial ordered-col cls) as)) ") IN ("
                    (sql/tuples (count ts) (count as)) ")")
               (into [] (comp (mapcat second) (map bound)) ts)]
              (junction " OR " (rest p)))
      :not  (let [[s ps] (pred-sql cls a)] [(str "NOT coalesce(" s ", FALSE)") ps])
      :in   (let [vs (remove nil? b)]
              (if (empty? vs) ["FALSE" []]
                  [(str (ordered-col cls a) " IN (" (sql/placeholders (count vs)) ")")
                   (mapv bound vs)]))
      :nil? [(str (col-sql a) (if b " IS NULL" " IS NOT NULL")) []]
      :=    (cmp "=")
      :not= (cmp "<>")
      :<    (cmp "<")
      :<=   (cmp "<=")
      :>    (cmp ">")
      :>=   (cmp ">=")
      :starts-with [(str "starts_with(" (col-sql a) ", ?)") [(str (sem/canon b))]]
      :has  [(str "(" (col-sql a) ")::jsonb @> ?::jsonb") [(json/write-str [(sem/canon b)])]])))

(defn- order-sql
  [cls order]
  (str " ORDER BY "
       (str/join ", " (concat (for [[a dir] order]
                                (str (ordered-col cls a)
                                     (if (= :desc dir) " DESC NULLS FIRST" " ASC NULLS LAST")))
                              [(str (ordered-col cls :db/id) " ASC")]))))

(defn- sql-scan?
  [cls {:keys [where order after agg]}]
  (let [scalar? #(#{:num :text :bool :inst :key} (get cls %))]
    (and (every? (partial sql-pred? cls) where)
         (every? (comp scalar? first) order)
         (or (nil? after) (= :text (value-class after)))
         (or (nil? (:group-by agg)) (scalar? (:group-by agg))))))

(defn scan-rows
  [c {:keys [ns sig view as-of]} T {:keys [where project order after limit agg] :as s}]
  (if-let [{rel :sql ps :params} (layout/relation view ns (layout/object-table T) as-of)]
    (let [decode  (decoder sig T)
          cls     (classes sig T)
          entries (fn [rows]
                    (mapv (fn [r] (let [{:keys [id values]} (->row decode r)] [id values])) rows))
          from    (fn [sql-where] (let [[w wps] (pred-sql cls (into [:and] sql-where))]
                                    [(str " FROM " rel " q WHERE " w) (into ps wps)]))]
      (if (sql-scan? cls s)
        (let [[f fps] (from (cond-> where (some? after) (conj [:> :db/id after])))
              g       (:group-by agg)]
          (cond
            (:count? agg)
            (long (:n (sql/one c (str "SELECT count(*) AS n" f) fps)))

            g
            (let [[a dec] (if (scan/key-attrs g) [nil identity]
                              (get decode (keyword (layout/column g))))]
              (reduce (fn [m r]
                        (update m (sem/group-key (if a (some-> (:g r) sql/value dec) (str (:g r))))
                                (fnil + 0) (long (:n r))))
                      {}
                      (sql/rows c
                                (str "SELECT " (col-sql g) " AS g, count(*) AS n" f " GROUP BY "
                                     (col-sql g)) fps)))

            :else
            (let [attrs (set (map :name (layout/attrs sig T)))
                  cols  (if project
                          (str/join ", "
                                    (cons (col-sql :db/id)
                                          (map col-sql
                                               (filter attrs (remove scan/key-attrs project)))))
                          "q.*")
                  paged (or (some? after) limit)]
              (entries (sql/rows c (str "SELECT " cols f
                                        (cond (seq order) (order-sql cls order)
                                              paged (order-sql cls []))
                                        (when limit " LIMIT ?"))
                                 (cond-> fps limit (conj limit)))))))
        (let [{in true out false} (group-by #(boolean (sql-pred? cls %)) where)
              [f fps] (from in)]
          (scan/run (entries (sql/rows c (str "SELECT q.*" f) fps))
                    (assoc s :where (vec out))))))
    (scan/run [] (dissoc s :where))))

(defn- ->link [sig L r]
  (let [id (or (get r (keyword layout/link-pk))
               (lib/pair-id sig L (:from_id r) (:to_id r)))
        ms (markings-of (get r markings-k))]
    (cond-> {:id (str id) :from (str (:from_id r)) :to (str (:to_id r))}
      ms (assoc :markings ms))))

(defn- where-clause
  [{:keys [ids from to ends]} by-id?]
  (cond from   ["l.from_id = ANY(?)" [from]]
        to     ["l.to_id = ANY(?)" [to]]
        ends   ["(l.from_id = ANY(?) OR l.to_id = ANY(?))" [ends ends]]
        by-id? [(str "l." (sql/dq layout/link-pk) " = ANY(?)") [ids]]))

(defn stored-links
  [c {:keys [ns sig as-of view]} L {:keys [ids from to ends] :as where}]
  (let [[clause vs] (where-clause where (and ids (not (or from to ends))))
        rel         (when-not (or (some empty? vs) (layout/fk-link? sig L))
                      (layout/relation view ns (layout/link-table L) as-of))]
    (if-let [{rel :sql ps :params} rel]
      (mapv (partial ->link sig L)
            (sql/rows c (str "SELECT * FROM " rel " l" (when clause (str " WHERE " clause)))
                      (into ps (map sql/text-array) vs)))
      [])))

(defn fk-links
  [c {:keys [ns sig as-of view]} L {:keys [ids from to ends]}]
  (let [by-id?      (and ids (not (layout/fk-link? sig L)) (not (or from to ends)))
        [clause vs] (where-clause {:ids ids :from from :to to :ends ends} by-id?)
        {rel :sql ps :params :as relation}
        (when-not (some empty? vs) (layout/link-relation view ns sig L as-of))]
    (if relation
      (let [ls (mapv (partial ->link sig L)
                     (sql/rows c
                               (str "SELECT * FROM " rel " l" (when clause (str " WHERE " clause)))
                               (into ps (map sql/text-array) vs)))]
        (if (and ids (not by-id?))
          (let [want (set (map str ids))] (filterv #(want (:id %)) ls))
          ls))
      [])))

(defn- target!
  [c cat ns t]
  (let [s (pgns/own-schema ns)]
    (layout/ensure-own! c cat ns t)
    (when-not (layout/table? cat s t)
      (throw (ex-info (str "no table for " t " in namespace " ns ": the layout did not create it")
                      {:ns ns :table t})))
    s))

(defn- upsert!
  [c cat s t cols rows]
  (when (seq rows)
    (let [k     (layout/key-of t)
          known (set (layout/data-columns cat s t))
          extra (remove known cols)
          _     (when (seq extra)
                  (throw (ex-info (str s "." t " has no column " (str/join ", " extra)
                                       " — the layout does not match the world")
                                  {:table t :columns (vec extra)})))
          per   (max 1 (quot 30000 (count cols)))
          sets  (str/join ", "
                          (for [col cols :when (not= col k)]
                            (str (sql/dq col) " = EXCLUDED." (sql/dq col))))]
      (doseq [ch (partition-all per rows)]
        (sql/update! c
                     (str "INSERT INTO " (sql/qualified s t) " (" (str/join ", " (map sql/dq cols))
                          ") VALUES "
                          (sql/tuples (count ch) (count cols))
                          " ON CONFLICT (" (sql/dq k) ") DO UPDATE SET " sets)
                     (into [] (mapcat (fn [r] (map #(get r %) cols))) ch))))))

(defn write-rows!
  [c {:keys [ns sig view]} T rows]
  (let [t     (layout/object-table T)
        attrs (mapv :name (layout/attrs sig T))
        cols  (into [layout/pk layout/markings] (map layout/column) attrs)]
    (doseq [{:keys [values]} rows
            :let [undeclared (remove (set attrs) (keys values))]
            :when (seq undeclared)]
      (anomaly/raise! :incorrect
                      (str T " declares no attribute " (str/join ", " undeclared))
                      {:type T :attributes (vec undeclared)}))
    (upsert! c view (target! c view ns t) t cols
             (for [{:keys [id values markings]} rows]
               (into {layout/pk (str id) layout/markings (encode-markings markings)}
                     (map (fn [a] [(layout/column a) (param (get values a))]))
                     attrs)))))

(defn- remove-keys!
  [c cat ns t ks]
  (when (seq ks)
    (let [s (pgns/own-schema ns) k (layout/key-of t) ks (map str ks)]
      (when (layout/table? cat s t)
        (sql/update! c (str "DELETE FROM " (sql/qualified s t) " WHERE " (sql/dq k) " = ANY(?)")
                     [(sql/text-array ks)]))
      (layout/tomb! c cat ns t ks))))

(defn delete-rows!
  [c {:keys [ns view]} T ids]
  (remove-keys! c view ns (layout/object-table T) ids))

(defn write-links!
  [c {:keys [ns sig view]} L ls]
  (let [t (layout/link-table L) cat view]
    (upsert! c cat (target! c cat ns t) t (mapv :name (layout/link-columns sig L))
             (for [{:keys [id from to markings]} ls]
               {layout/link-pk (str id) "from_id" (str from) "to_id" (str to)
                layout/markings (encode-markings markings)}))))

(defn delete-links!
  [c {:keys [ns view]} L ids]
  (remove-keys! c view ns (layout/link-table L) ids))

(defn scan-fk-links
  [c cx L {:keys [where]}]
  (let [narrow (some (fn [[op a b :as p]]
                       (cond (and (= :in op) (scan/key-attrs a)) {:ids b}
                             (and (= :in op) (#{:from :to} a)) {a b}
                             (and (= :or op) (= #{:from :to} (set (map second (rest p))))
                                  (every? #(= :in (first %)) (rest p))
                                  (apply = (map #(nth % 2) (rest p))))
                             {:ends (nth (second p) 2)}))
                     where)]
    (mapv (juxt :id #(dissoc % :id)) (fk-links c cx L (or narrow {})))))

(defn link-count
  [c {:keys [ns sig as-of view]} L]
  (if-let [{rel :sql ps :params} (layout/link-relation view ns sig L as-of)]
    (long (:n (sql/one c (str "SELECT count(*) AS n FROM " rel " l") ps)))
    0))

(defn scan-links
  [c cx L {:keys [where agg] :as s}]
  (if (and (:count? agg) (nil? (:group-by agg)) (empty? where))
    (link-count c cx L)
    (scan/run (scan-fk-links c cx L s) s)))
