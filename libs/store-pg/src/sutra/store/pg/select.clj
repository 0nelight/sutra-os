(ns sutra.store.pg.select
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.ref.objectset :as ros]
            [sutra.store.pg.sql :as sql :refer [frag frag-cat frag-wrap]]
            [sutra.store.pg.tokens :as tokens]
            [sutra.types.column :as column]
            [sutra.store.lib :as lib]
            [sutra.store.plan :as splan]
            [sutra.store.semantics :as sem]
            [sutra.store.pg.layout :as layout]
            [sutra.types.aggregate :as agg]
            [sutra.types.object-set :as os]
            [sutra.types.query :as q]
            [sutra.ref.geo :as geo]
            [sutra.types.sql-ident :as sqli]))

(set! *warn-on-reflection* true)

(defn- conjunction [frags]
  (when-let [fs (seq (remove nil? frags))]
    (frag-cat " AND " (map #(frag-wrap "(" % ")") fs))))

(defn- where-clause
  [frags]
  (if-let [c (conjunction frags)] (frag-wrap " WHERE " c "") (frag "")))

(def ^:private select-clauses
  [{:key :select :lead "SELECT "} {:key :from :lead " FROM "} {:key :joins :lead "" :sep ""}
   {:key :where :lead " WHERE "} {:key :group-by :lead " GROUP BY " :sep ", "}
   {:key :order-by :lead " ORDER BY " :sep ", "} {:key :limit :lead " LIMIT "}])

(defn- render-select [clauses]
  (frag-cat (for [{:keys [key lead sep]} select-clauses
                  :let [v (get clauses key)
                        f (cond (nil? v) nil (sequential? v) (frag-cat (or sep "") v) :else v)]
                  :when (seq (:sql f))]
              (frag-wrap lead f ""))))

(def pk layout/pk)
(def markings-col (sqli/quote-ident layout/markings))

(defn- attr-col [a] (sqli/quote-ident (column/sql-name a)))

(defn- qualify [cx col] (if-let [a (:alias cx)] (str a "." col) col))

(defn- table [cx t] ((:rel cx) t))

(defn- collated [col] (str col " COLLATE \"C\""))

(defn marking-where
  ([denied] (marking-where denied nil))
  ([denied q]
   (if (empty? denied)
     (frag "TRUE")
     (let [col (if q (str q "." markings-col) markings-col)]
       (frag (str "(" col " IS NULL OR ("
                  (str/join " AND " (repeat (count denied) (str col " NOT LIKE ? ESCAPE '\\'")))
                  "))")
             (mapv #(str "%," (name %) ",%") denied))))))

(defn- denies? [f] (not= "TRUE" (:sql f)))

(def ^:private text-types #{:string :text})

(defn- iface? [sig T] (contains? (:interfaces sig) T))

(defn- text-attr? [sig T a]
  (contains? text-types (some #(when (= a (:name %)) (:type %)) (layout/attrs sig T))))

(defn- check-attrs!
  [sig T attrs clause]
  (let [known (set (map :name (layout/attrs sig T)))]
    (doseq [a attrs :when (and (some? a) (not (known a)))]
      (anomaly/raise! :incorrect
                      (str T " has no attribute " a)
                      {:kind :unknown-attribute :type T :attribute a :clause clause}))))

(defn- pred-attrs [p]
  (when (vector? p)
    (let [[op a] p]
      (cond (#{:and :or} op)      (mapcat pred-attrs (rest p))
            (q/comparison-ops op) [a]
            (q/tokens? op)        a
            (q/geo? op)           (geo/field-attrs a)))))

(defn- flip [dir] (case dir :to :from :from :to))

(defn- self-other-cols [dir] (case dir :to ["from_id" "to_id"] :from ["to_id" "from_id"]))

(defn- like-escape [s]
  (-> (str s) (str/replace "\\" "\\\\") (str/replace "%" "\\%") (str/replace "_" "\\_")))

(defn- comparison-frag [op col val]
  (let [col (if (string? col) (frag col) col)]
    (cond
      (q/nullity? op)
      (frag-wrap "" col (if val " IS NULL" " IS NOT NULL"))

      (= :membership (:class (q/predicate-ops op)))
      (if (nil? val)
        (frag "FALSE")
        (frag-cat [(frag-wrap "(" col ")::jsonb @> ?::jsonb")
                   (frag "" [(json/write-str [(sem/canon val)])])]))

      :else
      (let [{:keys [class sql]} (q/predicate-ops op)
            [op-sql param] (if (= :text class)
                             ["LIKE"
                              (q/like-pattern op
                                              (like-escape
                                               (cond-> (str val)
                                                 (q/fold-case? op) str/lower-case)))]
                             [sql val])]
        (frag-cat [(if (q/fold-case? op) (frag-wrap "LOWER(" col ")") col)
                   (frag (str " " op-sql " ?" (when (= "LIKE" op-sql) " ESCAPE '\\'"))
                         [param])])))))

(declare with-frag)
(declare column-ref)

(def ^:private geopoint-re
  "'^\\s*[-+]{0,1}([0-9]+[.]{0,1}[0-9]*|[.][0-9]+)\\s*,\\s*[-+]{0,1}([0-9]+[.]{0,1}[0-9]*|[.][0-9]+)\\s*$'")

(defn- geo-coords
  [cx field]
  (let [col (fn [a] (qualify cx (attr-col a)))]
    (if (vector? field)
      (mapv #(str "(" (col %) ")::float8") field)
      (let [c (col field)]
        (mapv #(str "(CASE WHEN " c " ~ " geopoint-re " THEN btrim(split_part(" c ", ',', " %
                    "))::float8 END)")
              [1 2])))))

(defn- compile-geo
  [cx op field v]
  (let [[lat lon] (geo-coords cx field)
        n (geo/normalize op v)]
    (case op
      :within-distance
      (let [[clat clon] (:center n)]
        (frag (str "(" lat " IS NOT NULL AND " lon " IS NOT NULL AND "
                   "2 * ? * asin(sqrt(least(1.0, "
                   "power(sin(radians(" lat " - ?) / 2), 2) + "
                   "cos(radians(?)) * cos(radians(" lat ")) * power(sin(radians(" lon
                   " - ?) / 2), 2)"
                   "))) <= ?)")
              [geo/earth-radius-m clat clat clon (:meters n)]))
      :within-bounding-box
      (let [[[north west] [south east]] [(:top-left n) (:bottom-right n)]]
        (frag (str "(" lat " BETWEEN ? AND ? AND "
                   (if (<= west east)
                     (str lon " BETWEEN ? AND ?)")
                     (str "(" lon " >= ? OR " lon " <= ?))")))
              [south north west east])))))

(defn- compile-tokens [cx op attrs text]
  (if-let [query (when (seq attrs) (tokens/tsquery text))]
    (frag (str (tokens/document (map #(qualify cx (attr-col %))
                                     (or (when (= :matches-tokens op) (:token-attrs cx)) attrs)))
               " @@ to_tsquery('simple'::regconfig, ?)")
          [query])
    (frag "FALSE")))

(defn- compile-comparison [cx op field val]
  (if-let [w (get (:computed cx) field)]
    (comparison-frag op (cond-> (column-ref cx field w)
                          (and (= :ordering (:class (q/predicate-ops op))) (:text? w))
                          (as-> f (frag-wrap "" f " COLLATE \"C\"")))
                     val)
    (comparison-frag op (qualify cx (attr-col field)) val)))

(defn- id-in-clause [col src]
  (case (first src)
    :ids (let [ids (vec (second src))]
           (if (empty? ids) (frag "FALSE") (frag (str col " = ANY(?)") [(sql/text-array ids)])))
    :sql (frag-wrap (str col " IN (") (second src) ")")))

(defn- edge-source [cx link alias]
  (let [{:keys [from derived?]} ((:edges cx) link)]
    {:from (frag-wrap "" from (str " " alias)) :derived? derived?}))

(defn- hop-clause
  ([cx link dir src] (hop-clause cx link dir src nil))
  ([cx link dir src having]
   (let [[self-col other-col] (self-other-cols dir)
         {:keys [from derived?]} (edge-source cx link "e")
         mw (marking-where (:denied cx))]
     (frag-cat [(frag (str (qualify cx pk) " IN (SELECT " self-col " FROM "))
                from
                (frag " WHERE ")
                (id-in-clause other-col src)
                (when (and (not derived?) (denies? mw)) (frag-wrap " AND " mw ""))
                (when having (frag-wrap " " having ""))
                (frag ")")]))))

(defn- interface-select [cx iface]
  (let [{:keys [props implementers]} (get-in cx [:sig :interfaces iface])
        pcols (mapv (comp attr-col :name) props)
        impls implementers
        cols  (str/join ", " (cons pk pcols))]
    (if (seq impls)
      (frag-cat " UNION ALL "
                (for [impl impls]
                  (frag-wrap (str "SELECT " cols ", '" impl "' AS object_type, " markings-col
                                  " FROM ")
                             (table cx (layout/object-table impl)) " i")))
      (frag (str "SELECT NULL AS " pk (apply str (for [c pcols] (str ", NULL AS " c)))
                 ", NULL AS object_type, NULL AS " markings-col " WHERE FALSE")))))

(defn root-table
  [cx root]
  (if (iface? (:sig cx) root)
    (frag-wrap "(" (interface-select cx root) ")")
    (table cx (layout/object-table root))))

(defn- distinct-edges
  [cx link dir far-type]
  (let [[self-col other-col] (self-other-cols dir)
        {:keys [from derived?]} (edge-source cx link "e")
        emw (marking-where (:denied cx) "e")
        fmw (marking-where (:denied cx) "f")
        w   (conjunction [(when (and (not derived?) (denies? emw)) emw)
                          (when (and far-type (denies? fmw)) fmw)])]
    (frag-cat [(frag (str "(SELECT DISTINCT e." self-col " AS self_id, e." other-col
                          " AS other_id FROM "))
               from
               (when far-type
                 (frag-wrap " JOIN " (root-table cx far-type)
                            (str " f ON f." pk " = e." other-col)))
               (when w (frag-wrap " WHERE " w ""))
               (frag ")")])))

(defn- hop-join [cx link dir alias lhs]
  {:col  (str alias ".other_id")
   :join (frag-cat [(frag " JOIN ") (distinct-edges cx link dir nil)
                    (frag (str " " alias " ON " alias ".self_id = " lhs))])})

(defn- compile-having-all
  [cx include-by-as as-name coll]
  (let [{:keys [link dir]} (get include-by-as as-name)
        _ (when-not link
            (anomaly/raise! :incorrect
                            (str "having-all references unknown :include " as-name)
                            {:as as-name :include include-by-as}))
        [self-col other-col] (self-other-cols dir)
        ids (vec (distinct coll))]
    (if (empty? ids)
      (frag "TRUE")
      (hop-clause cx link dir [:ids ids]
                  (frag (str "GROUP BY " self-col " HAVING count(DISTINCT " other-col ") = ?")
                        [(count ids)])))))

(defn compile-where
  [cx include-by-as pred]
  (if (nil? pred)
    (frag "TRUE")
    (let [{:keys [shape identity]} (q/predicate-ops (first pred))]
      (when-let [problem (q/explain-predicate pred)]
        (anomaly/raise! :incorrect (str "Unsupported predicate shape: " (pr-str pred))
                        (assoc problem :pred pred)))
      (case shape
        :junction   (let [op (first pred)
                          parts (mapv #(compile-where cx include-by-as %) (rest pred))]
                      (if (empty? parts)
                        (frag (if identity "TRUE" "FALSE"))
                        (frag-wrap "(" (frag-cat (str ") " (str/upper-case (name op)) " (") parts)
                                   ")")))
        :comparison (compile-comparison cx (first pred) (second pred) (nth pred 2))
        :geo        (compile-geo cx (first pred) (second pred) (nth pred 2))
        :tokens     (compile-tokens cx (first pred) (second pred) (nth pred 2))
        :id-set     (id-in-clause (qualify cx pk) [:ids (second pred)])
        :having-all (let [[_ as-name coll] pred]
                      (compile-having-all cx include-by-as as-name coll))))))

(defn- includes-by-as
  [includes]
  (into {} (map (juxt :as identity)) (or includes [])))

(defn- group-link-join
  [cx sides {:keys [name alias title-alias]}]
  (let [{:keys [dir] far-type :far link-rid :link} (get sides name)
        calias (str "c" alias) lalias (str "l" alias)
        title-key (get-in cx [:sig :types far-type :title])]
    {:col        (str calias ".other_id")
     :join       (frag-cat [(frag " LEFT JOIN ") (distinct-edges cx link-rid dir far-type)
                            (frag (str " " calias " ON " calias ".self_id = t." pk))])
     :title-join (when title-key
                   (frag-wrap " LEFT JOIN " (root-table cx far-type)
                              (str " " lalias " ON " lalias "." pk " = " calias ".other_id")))
     :title-col  (when title-key (str "MIN(" lalias "." (attr-col title-key) ")"))
     :title-alias title-alias}))

(defn- group-expr [dialect origin link-of col-of t]
  (cond-> (agg/group-sql dialect t
                         (case (:kind t)
                           :origin (or (:col origin)
                                       (anomaly/raise!
                                        :incorrect
                                        (str "[:origin] groups by the object each member was"
                                             " reached FROM — this read takes no traversal,"
                                             " so there is no origin to group by")
                                        {:group-by :origin}))
                           :link   (:col (link-of (:name t)))
                           (some-> (:attr t) col-of)))
    (#{:origin :link :bucket} (:kind t))
    collated))

(defn- aggregate-select-cols [aggregate groups group-exprs link-of col-of]
  (let [title-cols (keep (fn [t]
                           (when-let [tc (:title-col (link-of (:name t)))]
                             (str tc " AS " (sqli/quote-ident (:title-alias (link-of (:name t)))))))
                         groups)
        agg-cols   (map (fn [a] (str (agg/agg-sql a (some-> (:attr a) col-of))
                                     " AS " (sqli/quote-ident (:alias a))))
                        (:aggs aggregate))]
    (str/join ", "
              (concat (map (fn [e {:keys [alias]}] (str e " AS " (sqli/quote-ident alias)))
                           group-exprs groups)
                      title-cols agg-cols))))

(defn- row-select-cols [sig root sel]
  (check-attrs! sig root sel :select)
  (let [attrs (cond->> (layout/attrs sig root) sel (filterv #(contains? sel (:name %))))]
    (str/join ", " (concat [(str "t." pk)]
                           (map #(str "t." (attr-col (:name %))) attrs)
                           (when (iface? sig root) ["t.object_type"])))))

(defn- order-terms [qt aggregate groups]
  (let [dir       (str/upper-case (name (or (:sort-dir qt) :asc)))
        agg-alias (into {} (map (juxt :name :alias)) (concat groups (:aggs aggregate)))
        id-col    (str "t." pk)]
    (cond
      (and aggregate (empty? groups)) nil
      aggregate (if-let [ob (:order-by qt)]
                  (if-let [a (agg-alias ob)]
                    (into [(str (sqli/quote-ident a) " " dir)]
                          (map #(str (sqli/quote-ident (:alias %)) " " dir))
                          (remove #(= ob (:name %)) groups))
                    (lib/refuse-aggregate-order! ob (set (keys agg-alias))))
                  (mapv #(str (sqli/quote-ident (:alias %)) " ASC") groups))
      (:order-by qt) [(str (str "t." (attr-col (:order-by qt))) " " dir)
                      (str id-col " " dir)]
      :else [(str id-col " ASC")])))

(defn- relevance-order
  [cx sig root where]
  (let [text  (some #(when (and (vector? %) (= :matches-tokens (first %))) (nth % 2))
                    (tree-seq vector? seq where))
        query (some-> text tokens/tsquery)
        by-pk (frag (str "t." pk " ASC"))]
    (if query
      [(frag (str "ts_rank_cd("
                  (tokens/document (map #(qualify cx (attr-col %)) (tokens/text-attrs sig root)))
                  ", to_tsquery('simple'::regconfig, ?), 2) DESC")
             [query])
       by-pk]
      [by-pk])))

(defn- with-label [nm] (str "w__" (name nm)))

(declare spine-from)

(defn- compile-select [cx qt extra-where origin sides]
  (lib/check-paging! qt)
  (let [sig       (:sig cx)
        dialect   (:dialect cx sqli/default-dialect)
        root      (:root qt)
        aggregate (lib/qt-aggregate qt)
        groups    (:group-by aggregate)
        computed  (or (:computed cx) {})
        stored    (fn [attrs] (remove #(contains? computed %) attrs))
        _         (check-attrs! sig root (pred-attrs (:where qt)) :where)
        _         (when-not aggregate
                    (check-attrs! sig root (remove #{q/relevance} (stored [(:order-by qt)]))
                                  :order-by))
        _         (when aggregate
                    (check-attrs! sig root (stored (keep :attr (concat groups (:aggs aggregate))))
                                  :aggregate))
        col-of    (fn [a]
                    (str "t." (if (computed a) (sqli/quote-ident (with-label a)) (attr-col a))))
        link-of   (into {}
                        (keep (fn [t]
                                (when (= :link (:kind t))
                                  [(:name t) (group-link-join cx sides t)]))) groups)
        gexprs    (mapv #(group-expr dialect origin link-of col-of %) groups)
        sel       (some-> (not-empty (:select qt)) set)
        cx        (cond-> cx (seq computed) (assoc :materialized (set (keys computed))))
        tcx       (assoc cx :alias "t")
        from      (spine-from cx root "t")
        cols      (if aggregate
                    (frag (aggregate-select-cols aggregate groups gexprs link-of col-of))
                    (frag-cat ", "
                              (cons
                               (frag
                                (row-select-cols sig root (when sel (set (remove computed sel)))))
                               (for [[nm w] (sort-by key computed) :when (or (nil? sel) (sel nm))]
                                 (frag-wrap "" (column-ref tcx nm w)
                                            (str " AS " (sqli/quote-ident (with-label nm))))))))
        where     (when (:where qt)
                    (compile-where (assoc cx :alias "t" :token-attrs (tokens/text-attrs sig root))
                                   (includes-by-as (:include qt)) (:where qt)))
        mw        (marking-where (:denied cx) "t")
        limit     (cond
                    (and aggregate (seq groups)) (frag (str (inc agg/max-buckets)))
                    aggregate nil
                    (:limit qt) (frag (str (long (:limit qt))
                                           (when-let [o (:offset qt)] (str " OFFSET " (long o))))))
        ljoins    (frag-cat (mapcat (fn [t]
                                      (when-let [lj (link-of (:name t))]
                                        [(:join lj) (:title-join lj)])) groups))]
    (render-select {:select   cols
                    :from     from
                    :joins    [(:join origin) ljoins]
                    :where    (conjunction [where extra-where (when (denies? mw) mw)])
                    :group-by (mapv frag gexprs)
                    :order-by (if-let [w (and (not aggregate) (get computed (:order-by qt)))]
                                (let [dir (str/upper-case (name (or (:sort-dir qt) :asc)))]
                                  [(frag-cat [(column-ref tcx (:order-by qt) w)
                                              (frag (str (when (:text? w) " COLLATE \"C\"") " "
                                                         dir))])
                                   (frag (str "t." pk " " dir))])
                                (if (and (not aggregate) (= q/relevance (:order-by qt)))
                                  (relevance-order tcx sig root (:where qt))
                                  (mapv frag (order-terms qt aggregate groups))))
                    :limit    limit})))

(defn- computing
  [cx ir]
  (let [sig (:sig cx)]
    (assoc cx
           :spine (splan/column-order ir)
           :computed
           (update-vals (splan/columns ir)
                        (fn [{f :fn :keys [attr far] :as w}]
                          (assoc w
                                 :text?
                                   (boolean (and attr (#{:min :max} f)
                                                 (text-attr? sig far attr)))))))))

(declare with-frag*)

(defn- number-literal [x]
  (cond (instance? java.math.BigDecimal x) (.toPlainString ^java.math.BigDecimal x)
        (ratio? x)                         (str (double x))
        :else                              (str x)))

(defn- compute-frag
  [cx {:keys [expr in cast]}]
  (let [below (:computed (computing cx in))
        sig   (:sig cx)
        root  (:type in)]
    (check-attrs! sig root (remove below (filter keyword? (tree-seq vector? rest expr))) :compute)
    (letfn [(e [x]
              (cond
                (number? x)  (frag (number-literal x))
                (string? x)  (frag "CAST(? AS text)" [x])
                (nil? x)     (frag "NULL")
                (keyword? x) (if-let [c (below x)] (column-ref cx x c)
                               (frag (qualify cx (attr-col x))))
                :else
                (let [[op & args] x
                      fs (mapv e args)
                      infix (fn [o] (frag-wrap "(" (frag-cat (str " " o " ") fs) ")"))]
                  (case op
                    :+        (infix "+")
                    :*        (infix "*")
                    :-        (if (next fs) (infix "-") (frag-wrap "(-" (first fs) ")"))
                    :abs      (frag-wrap "ABS(" (first fs) ")")
                    :coalesce (frag-wrap "COALESCE(" (frag-cat ", " fs) ")")
                    :concat-strings
                    (frag-wrap "NULLIF(CONCAT_WS(" (frag-cat ", " (cons (first fs)
                                                     (map #(frag-wrap "CAST(" % " AS text)")
                                                          (rest fs))))
                               "), '')")
                    :div      (let [t (if cast "numeric" "double precision")]
                                (frag-cat [(frag-wrap "(CAST(" (first fs) (str " AS " t ") / "))
                                           (frag-wrap "CAST(NULLIF(" (second fs)
                                                      (str ", 0) AS " t "))"))]))))))]
      (let [x (e expr)]
        (cond
          (nil? cast)    x
          (= :double cast)  (frag-wrap "CAST(" x " AS double precision)")
          (= :integer cast) (frag-wrap "CAST(ROUND(CAST(" x " AS numeric)) AS bigint)")
          :else          (frag-wrap "ROUND(CAST(" x
                                    (str " AS numeric), " (long (second cast)) ")")))))))

(defn- with-frag
  [cx w]
  (if (= :compute (:op w))
    (compute-frag cx w)
    (with-frag* cx w)))

(defn- column-ref
  [cx nm w]
  (if (contains? (:materialized cx) nm)
    (frag (qualify cx (sqli/quote-ident (with-label nm))))
    (with-frag cx w)))

(defn- fold-value
  [cx far {:keys [attr expr cast]}]
  (cond
    attr (if-let [w (get (:computed cx) attr)]
           (column-ref cx attr w)
           (frag (qualify cx (attr-col attr))))
    expr (compute-frag cx {:expr expr :cast cast :in {:type far}})))

(defn- fold-agg
  [f v pred]
  (let [a (case f
            :count (if v (frag-wrap "COUNT(" v ")") (frag "COUNT(*)"))
            :sum   (frag-wrap "SUM(" v ")")
            :avg   (frag-wrap "AVG(" v ")")
            :min   (frag-wrap "MIN(" v ")")
            :max   (frag-wrap "MAX(" v ")"))]
    (if pred
      (frag-cat [a (frag-wrap " FILTER (WHERE " pred ")")])
      a)))

(defn- fold-attrs
  [cx {:keys [attr expr where]}]
  (let [cols (set (keys (:computed cx)))]
    (-> (vec (pred-attrs where))
        (cond-> attr (conj attr))
        (into (ros/compute-refs expr))
        (->> (remove cols)))))

(defn- far-cx
  [cx alias far-in]
  (let [cols (:computed (when far-in (computing cx far-in)))]
    (assoc cx :alias alias :spine nil
           :computed cols
           :materialized (when cols (set (keys cols))))))

(defn- fold-relation
  [cx far far-in]
  (if far-in
    (spine-from (computing cx far-in) far "wf")
    (frag-wrap "" (root-table cx far) " wf")))

(defn- fold-join
  [cx ws far-in alias]
  (let [sig  (:sig cx)
        {:keys [link dir far]} (first ws)
        [self-col other-col] (self-other-cols dir)
        {:keys [from derived?]} (edge-source cx link "e")
        emw  (marking-where (:denied cx) "e")
        fmw  (marking-where (:denied cx) "wf")
        fcx  (far-cx cx "wf" far-in)
        cols (map-indexed (fn [i w]
                            (check-attrs! sig far (fold-attrs fcx w) :with)
                            {:name (:name w)
                             :v    (str "v" i)
                             :zero? (boolean (#{:count :sum} (:fn w)))
                             :agg  (fold-agg (:fn w) (fold-value fcx far w)
                                             (when (:where w)
                                               (compile-where fcx {} (:where w))))})
                          ws)
        edge-where (when (and (not derived?) (denies? emw)) emw)]
    {:join
     (frag-cat (concat [(frag (str " LEFT JOIN (SELECT we." self-col " AS k"))]
                       (for [{:keys [agg v]} cols] (frag-wrap ", " agg (str " AS " v)))
                       [(frag (str " FROM (SELECT DISTINCT e." self-col ", e." other-col " FROM "))
                        from
                        (if edge-where (frag-wrap " WHERE " edge-where "") (frag ""))
                        (frag (str ") we JOIN "))
                        (fold-relation cx far far-in)
                        (frag (str " ON wf." pk " = we." other-col))
                        (where-clause [(when (denies? fmw) fmw)])
                        (frag (str " GROUP BY we." self-col ") " alias
                                   " ON " alias ".k = " (qualify cx pk)))]))
     :values (into {} (for [{:keys [name v zero?]} cols]
                        [name (frag (if zero?
                                      (str "COALESCE(" alias "." v ", 0)")
                                      (str alias "." v)))]))}))

(defn- with-frag*
  [cx {:keys [link dir far far-in] f :fn :as w}]
  (let [sig  (:sig cx)
        [self-col other-col] (self-other-cols dir)
        {:keys [from derived?]} (edge-source cx link "we")
        emw  (marking-where (:denied cx) "we")
        fmw  (marking-where (:denied cx) "wf")
        fcx  (far-cx cx "wf" far-in)
        _    (check-attrs! sig far (fold-attrs fcx w) :with)
        agg  (let [a (fold-agg f (fold-value fcx far w) nil)]
               (if (= :sum f) (frag-wrap "COALESCE(" a ", 0)") a))
        wh   (when (:where w) (compile-where fcx {} (:where w)))]
    (frag-cat [(frag "(SELECT ")
               agg
               (frag " FROM ")
               (fold-relation cx far far-in)
               (frag (str " WHERE wf." pk " IN (SELECT we." other-col " FROM "))
               from
               (frag (str " WHERE we." self-col " = " (qualify cx pk)))
               (when (and (not derived?) (denies? emw)) (frag-wrap " AND " emw ""))
               (frag ")")
               (when (denies? fmw) (frag-wrap " AND " fmw ""))
               (when wh (frag-wrap " AND (" wh ")"))
               (frag ")")])))

(defn- spine-from
  [cx root as]
  (let [tcx      (assoc cx :alias "t")
        order    (vec (:spine cx))
        node     (:computed cx)
        folds    (filterv #(not= :compute (:op (node %))) order)
        computes (filterv #(= :compute (:op (node %))) order)
        passes   (->> folds
                      (reduce (fn [acc nm]
                                (let [w (node nm)]
                                  (update acc [(:link w) (:dir w)] (fnil conj []) w)))
                              {})
                      (sort-by (fn [[_ ws]] (str (:name (first ws)))))
                      (map-indexed (fn [i [_ ws]]
                                     (fold-join tcx ws
                                                (some-> (seq (keep :far-in ws)) splan/merge-chains)
                                                (str "g" i)))))
        values   (into {} (mapcat :values) passes)
        alias-of (fn [last?] (if last? as "t"))
        base     (if (empty? passes)
                   (frag-wrap "" (root-table cx root) (str " " (alias-of (empty? computes))))
                   (frag-cat (concat [(frag "(SELECT t.*")]
                                     (for [nm folds]
                                       (frag-wrap ", " (values nm)
                                                  (str " AS " (sqli/quote-ident (with-label nm)))))
                                     [(frag-wrap " FROM " (root-table cx root) " t")]
                                     (map :join passes)
                                     [(frag (str ") " (alias-of (empty? computes))))])))]
    (reduce (fn [prev [i nm]]
              (let [below (into (set folds) (take-while #(not= nm %) order))
                    lcx   (assoc tcx :materialized below)]
                (frag-cat [(frag "(SELECT t.*, ")
                           (frag-wrap "" (compute-frag lcx (node nm))
                                      (str " AS " (sqli/quote-ident (with-label nm))))
                           (frag-wrap " FROM " prev
                                      (str ") " (alias-of (= i (dec (count computes))))))])))
            base
            (map-indexed vector computes))))

(declare member-clause)

(defn- typed-origin
  [in hop]
  (cond-> in
    (and (= :static (:op in)) (nil? (:root in)))
    (assoc :root (:near hop) :type (:near hop))))

(defn- id-source [cx ir depth]
  (let [t     (:type ir)
        alias (str "s" depth)
        cx    (dissoc cx :materialized)
        mw    (marking-where (:denied cx) alias)
        w     (member-clause (assoc cx :alias alias) ir depth)]
    [:sql
     (frag-cat [(frag-wrap (str "SELECT " alias "." pk " FROM ") (root-table cx t)
                           (str " " alias " WHERE "))
                w
                (when (denies? mw) (frag-wrap " AND " mw ""))])]))

(defn- filter-clauses [cx level-type {:keys [where include in]}]
  (let [sig (:sig cx)
        cx  (computing cx in)]
    (check-attrs! sig level-type (remove (:computed cx) (pred-attrs where)) :where)
    (if where
      [(compile-where (assoc cx :token-attrs (tokens/text-attrs sig level-type))
                      (includes-by-as include) where)]
      [])))

(defn- combine [joiner identity-sql parts]
  (let [parts (remove #(= identity-sql (:sql %)) parts)]
    (if (empty? parts) (frag identity-sql) (frag-cat joiner (map #(frag-wrap "(" % ")") parts)))))

(def ^:private membership-ops
  (into #{} (keep (fn [[op {:keys [kind]}]] (when (#{:leaf :member} kind) op))) os/object-set-ops))

(defn- member-clause [cx ir depth]
  (case (:op ir)
    :scan   (frag "TRUE")
    :static (id-in-clause (qualify cx pk) [:ids (vec (:ids ir))])
    :filter (combine " AND " "TRUE" (into [(member-clause cx (:in ir) depth)]
                                          (filter-clauses cx (:type ir) ir)))
    :search-around
    (let [{:keys [in dir link]} ir]
      (hop-clause cx link (flip dir) (id-source cx (typed-origin in ir) (inc depth))))
    :union     (combine " OR "  "FALSE" (mapv #(member-clause cx % depth) (:in ir)))
    :intersect (combine " AND " "TRUE"  (mapv #(member-clause cx % depth) (:in ir)))
    :subtract  (let [[f & r] (mapv #(member-clause cx % depth) (:in ir))]
                 (frag-cat [(frag-wrap "(" f ") AND (") (combine " OR " "FALSE" (vec r))
                            (frag ") IS NOT TRUE")]))
    (:order :project :with :compute) (member-clause cx (:in ir) depth)
    (anomaly/raise! :incorrect
                    (str (os/unsupported-message ir membership-ops) " in a membership position")
                    {:node ir})))

(defn- origin-member [cx ir depth]
  (let [[hop wraps] (loop [node ir wraps []]
                      (case (:op node)
                        :search-around            [node wraps]
                        (:filter :with :compute :order
                                 :project) (recur (:in node) (conj wraps node))
                        [nil nil]))]
    (when-not hop
      (anomaly/raise! :incorrect
                      (str "[:origin] needs a traversal to name: this set is " (pr-str (:op ir))
                           ", not a :search-around (optionally filtered)")
                      {:ir ir}))
    (let [{:keys [in dir link]} hop
          j    (hop-join cx link (flip dir) "c" (qualify cx pk))
          in-f (id-in-clause (:col j) (id-source cx (typed-origin in hop) (inc depth)))]
      (assoc j :member (combine " AND " "TRUE"
                                (into [in-f] (mapcat #(if (= :filter (:op %))
                                                        (filter-clauses cx (:type %) %)
                                                        []))
                                      wraps))))))

(defn- origin-grouped? [shaping]
  (boolean (some #(= :origin (:kind %))
                 (:group-by (agg/normalize {:aggs (:aggregate shaping)
                                            :group-by (:group-by shaping)})))))

(defn- peel-shaping [ir]
  (loop [node ir qt {}]
    (if (= :shaping (:kind (os/object-set-ops (:op node))))
      (do (when (and (= :aggregate (:op node)) (:accuracy node) (not= :exact (:accuracy node)))
            (anomaly/raise! :incorrect
                            (str "an :accuracy " (pr-str (:accuracy node))
                                 " aggregate is declared in the"
                                 " vocabulary but no approximate lowering answers it")
                            {:node node}))
          (recur (:in node) (merge qt (select-keys node (os/qt-keys (:op node)))
                                   (when (= :aggregate (:op node)) {:aggregate (:aggs node)}))))
      [node qt])))

(defn compile-plan
  [cx plan]
  (let [sides (:sides (first (filter #(= :aggregate (:op %)) (os/nodes plan))))]
    (if-let [qt (os/ir->qt plan)]
      (compile-select cx qt nil nil sides)
      (let [[member shaping] (peel-shaping plan)
            qt     (assoc shaping :root (:type member))
            ccx    (computing cx member)
            mcx    (assoc cx :alias "t" :materialized (set (keys (:computed ccx))))
            origin (when (origin-grouped? shaping) (origin-member (assoc cx :alias "t") member 1))
            part   (or (:member origin) (member-clause mcx member 1))]
        (compile-select ccx qt part origin sides)))))

(defn- reshape-aggregate [{:keys [group-by aggs window]} rows]
  (lib/check-bucket-count! (count rows))
  (let [cols (fn [terms row]
               (into {}
                     (map (fn [{:keys [name alias kind title-alias]}]
                            [name (if (= :link kind)
                                    (when-some [id (get row (keyword alias))]
                                      (cond-> {:id id}
                                        title-alias (assoc :title (get row (keyword title-alias)))))
                                    (get row (keyword alias)))]))
                     terms))
        {:keys [limit offset]} window]
    (cond->> (mapv (fn [row] (agg/bucket-row (cols group-by row) (cols aggs row))) rows)
      (and (seq group-by) offset) (drop offset)
      (and (seq group-by) limit)  (take limit)
      true vec)))

(defn shape
  [sig plan rows decode-row]
  (let [node (first (filter #(= :aggregate (:op %)) (os/nodes plan)))
        page (first (filter #(= :page (:op %)) (os/nodes plan)))
        root (:type plan)]
    (if node
      (reshape-aggregate (assoc (agg/normalize node) :window (select-keys page [:limit :offset]))
                         rows)
      (let [labels (into {} (map (fn [nm] [(keyword (with-label nm)) nm]))
                         (keys (splan/columns (first (peel-shaping plan)))))]
        (mapv (fn [r]
                (let [T (if (iface? sig root) (get r :object_type) root)
                      {:keys [id
                              values]} (decode-row T (apply dissoc r :object_type (keys labels)))]
                  {:id id :object-type-ref T
                   :values (into values
                                 (keep (fn [[k nm]]
                                         (when (contains? r k) [nm (sql/value (get r k))])))
                                 labels)}))
              rows)))))

(def empty-relation
  (frag
   (str "(SELECT NULL::varchar AS " pk ", NULL::varchar AS " markings-col
        ", NULL::varchar AS id, NULL::varchar AS from_id, NULL::varchar AS to_id WHERE FALSE)")))
