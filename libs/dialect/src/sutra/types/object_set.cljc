(ns sutra.types.object-set
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.types.link-side :as ls]
            [sutra.types.rid :as rid]
            [sutra.types.table :as tbl]
            #?(:clj [sutra.ref.objectset :as ros])))

(def object-set-ops
  {:scan
   {:inputs :none :kind :leaf :flat? true
    :fields   #{:root}
    :required #{:root}
    :doc "Every object of one type or interface. `:root` is an object-type or
          interface RID."}

   :static
   {:inputs :none :kind :leaf :flat? true
    :fields   #{:ids :root}
    :required #{:ids}
    :doc "
          `sutra.types.query`'s `:id-in` is the same thing as a predicate; this is it
          as a set, so it can be the INPUT of a hop. `:root` is optional — a
          static set only needs a type when it is read directly (which table),
          never when it is only the start of a traversal. An EMPTY `:ids` is the
          empty set and matches NOTHING, never 'no filter'."}

   :filter
   {:inputs :one :kind :member :flat? true
    :fields #{:where :include}
    :doc "The subset satisfying a predicate. `:where` is a `sutra.types.query`
          predicate-IR node, reused UNCHANGED — this layer does not have its own
          comparison vocabulary and must not grow one. `:include` names the link
          sides a `[:having-all …]` in `:where` counts over."}

   :search-around
   {:inputs :one :kind :member :flat? false :retypes? true
    :fields   #{:link :dir}
    :required #{:link}
    :doc "THE missing composable hop: the objects reached from the input set by
          traversing one SIDE of a link type. Inbound, `:link` is that side's API
          name and the node carries no
          `:dir` — a side already names its direction, and the boundary refuses
          one. The server resolves it once (`data-plane/resolve-sides`) into the
          stored form `{:link <link-type RID> :dir :to|:from}`.

          This is the node the whole algebra exists for. Without it a second hop
          is a second `run-query` with the two result sets paired in memory, and
          then paging happens ABOVE the seam over a set the database already
          truncated — the failure mode `sutra.store.access.substrate`'s docstring names."}

   :with
   {:inputs :one :kind :member :flat? false
    :fields   #{:name :fn :attr :expr :cast :link :dir :where :far-in}
    :required #{:name :fn :link}
    :doc "The input's members, each with one more column `:name`: `:fn` (:count :sum
          :avg :min :max) over its distinct visible neighbours across one link side,
          those satisfying `:where`. What it folds over each neighbour is either `:attr`,
          a property they carry, or `:expr` — arithmetic over the neighbour's own fields,
          `:cast` and all, computed exactly as a `:compute` column is but on the FAR row.
          So a value the far type DERIVES from its own fields is folded inside the fold,
          in one statement, instead of loading the far objects to derive them above the
          store. `:expr` cannot itself fold — but `:far-in` can: it is the RELATION the
          fold folds over, the far type's rows carrying columns the far type itself
          computes (a chain of `:with`/`:compute` above a leaf). `:attr`, `:expr` and
          `:where` may name those columns as they name an attribute, so a fold OF a fold
          is two grouped passes in one statement, and a filter on a value the far type
          derives has a column to filter on.
          A :filter, :order, :project or :aggregate
          above it reads the column as an attribute, so a value a type derives from its
          links can be filtered and ordered on where the rows are. The column is seen
          along the chain of :filter and :with above it; a set operator or a hop ends it."}

   :compute
   {:inputs :one :kind :member :flat? false
    :fields   #{:name :expr :cast}
    :required #{:name :expr}
    :doc "The input's members, each with one more column `:name`: arithmetic `:expr`
          (:+ :- :* :div :abs :coalesce) over numbers, the member's attributes and the
          columns below it. An operand nil or a divisor zero makes the column nil, as in
          SQL. Without `:cast` a quotient is a double; with it (:integer, :double,
          [:decimal scale]) the arithmetic is exact and the column is cast, rounded half
          away from zero. Seen like a :with column."}

   :union
   {:inputs :many :kind :member :flat? false
    :doc "Set union of its inputs."}
   :intersect
   {:inputs :many :kind :member :flat? false
    :doc "Set intersection of its inputs."}
   :subtract
   {:inputs :many :kind :member :flat? false
    :doc "The first input minus every later one."}

   :order
   {:inputs :one :kind :shaping :flat? true
    :fields #{:order-by :sort-dir}
    :doc "Order by a root attribute. Every lowering breaks ties on the PK, so a
          page is taken over a TOTAL order (see compile-query)."}

   :page
   {:inputs :one :kind :shaping :flat? true
    :fields #{:limit :offset}
    :doc "A bounded window. `:offset` without `:limit` is refused by
          `data-plane/check-paging!` — a page needs a size, not just a start."}

   :project
   {:inputs :one :kind :shaping :flat? true
    :fields #{:select}
    :doc "Narrow to the named attributes — a partial read, in ontology
          vocabulary (attribute NAMES, never columns). The flat `qt`'s
          `:select`."}

   :aggregate
   {:inputs :one :kind :shaping :flat? true
    :fields   #{:aggs :group-by :accuracy}
    :required #{:aggs}
    :doc "A question ABOUT the set instead of the set — answered by the
          substrate rather than by handing every row up and folding it above the
          seam. `:aggs` and `:group-by` are `sutra.types.aggregate`'s vocabulary,
          reused UNCHANGED and validated by its owner: `:count` (the bare
          set-size question, the one spelling that predates that namespace) or
          `[{:fn :sum :attr :stunden} …]`, grouped by any number of terms —
          a raw attribute, a date bucket `[:bucket <attr> :week]`, or a
          fixed-width numeric one.

          One term is about the PLAN rather than the rows: `[:origin]` groups
          the members by the object each was reached FROM, and is therefore
          legal only over an input that took a `:search-around`. It is what
          makes `aggregate(filter(search-around(static ids)))` answer a link
          ROLLUP for a whole page in one statement — the lowering joins the link
          table so the hop's near side is a column (`data-plane/origin-member`),
          and `sutra.objects.views` builds exactly that plan for every derived
          property whose fold the substrate can reproduce.

          `:accuracy` is still declared-but-refused: every lowering here is
          exact, and a node DEMANDING an approximate answer gets a refusal
          rather than a silent downgrade. The planner's `:accuracy` OPTION is
          the preference; this field is a demand."}

   :template
   {:inputs :one :kind :meta :flat? true
    :fields   #{:param-slots}
    :required #{:param-slots}
    :doc "An object set with HOLES — the saved `:object-sets` shape, whose
          `:param-slots` are substituted by `data-plane/resolve-object-set`
          before the set can run. Not a set operation: it wraps one. A planner
          refuses it, because a template is not a read."}})

(def leaf-ops
  (tbl/where object-set-ops #(= :none (:inputs %))))

(defn inputs
  [node]
  (case (:inputs (object-set-ops (:op node)))
    :none []
    :one  (if-let [in (:in node)] [in] [])
    :many (vec (:in node))
    []))

(defn nodes
  [ir]
  (when (map? ir)
    (filter map? (tree-seq map? inputs ir))))

(defn count-op
  [ir op]
  (count (filter #(= op (:op %)) (nodes ir))))

(defn hops
  [ir]
  (count-op ir :search-around))

(defn source
  [ir]
  (let [ins (inputs ir)]
    (cond
      (leaf-ops (:op ir)) ir
      (= 1 (count ins))   (recur (first ins))
      :else               nil)))

(defn flat?
  [ir]
  (and (some? ir)
       (every? #(:flat? (object-set-ops (:op %)) false) (nodes ir))
       (let [n (frequencies (map :op (nodes ir)))]
         (and (every? #(<= % 1) (vals n))
              (not (and (n :static) (n :filter)))))
       (some? (source ir))))

(def qt-keys
  {:filter    [:where :include]
   :order     [:order-by :sort-dir]
   :page      [:limit :offset]
   :project   [:select]
   :aggregate [:aggregate :group-by :accuracy]
   :template  [:param-slots]
   :scan      [:root]})

(def ^:private modelled-qt-keys
  (into #{} cat (vals qt-keys)))

(def flat-keys
  modelled-qt-keys)

(defn- wrap
  [ir op fields]
  (let [fields (into {} (remove (comp nil? val)) fields)]
    (if (empty? fields) ir (into {:op op :in ir} fields))))

(defn qt->ir
  ([qt] (qt->ir qt identity))
  ([qt member-fn]
   (cond
     (nil? qt)   nil
     (:op qt)    (member-fn qt)
     :else
     (let [unknown (remove flat-keys (keys qt))
           pick    (fn [k] (select-keys qt (qt-keys k)))]
       (when (seq unknown)
         (anomaly/coded! :refusal.dialect/object-set-unknown-keys :incorrect
           "unknown key(s) {unknown:pr} in a flat object set — known: {allowed:pr}"
           {:kind :unknown-key :unknown (vec unknown)
            :allowed (vec (sort flat-keys))}))
       (-> (cond-> {:op :scan}
             (contains? qt :root) (assoc :root (:root qt)))
           (wrap :filter  (pick :filter))
           member-fn
           (wrap :aggregate {:aggs     (:aggregate qt)
                             :group-by (:group-by qt)
                             :accuracy (:accuracy qt)})
           (wrap :project (pick :project))
           (wrap :order   (pick :order))
           (wrap :page    (pick :page))
           (wrap :template (pick :template)))))))

(defn ir->qt
  [ir]
  (when (flat? ir)
    (reduce (fn [qt {:keys [op] :as node}]
              (merge qt
                     (select-keys node (qt-keys op))
                     (when (= :static op)
                       (cond-> {:where [:id-in (vec (:ids node))]}
                         (:root node) (assoc :root (:root node))))
                     (when (and (= :aggregate op) (some? (:aggs node)))
                       {:aggregate (:aggs node)})))
            {}
            (reverse (nodes ir)))))

(defn rewrite-bottom-up
  [f ir]
  (let [walk (fn walk [n]
               (f (cond-> n
                    (map? (:in n))        (update :in walk)
                    (sequential? (:in n)) (update :in #(mapv walk %)))))]
    (walk ir)))

#?(:clj
   (defn result-type
     [schema ir]
     (ros/type-of {} schema ir)))

(defn match-hop
  [{:keys [link dir far-type]} where]
  (let [far {:op :scan :root far-type}]
    {:op :search-around :link link :dir (ls/flip dir)
     :in (if where {:op :filter :where where :in far} far)}))

(defn ids-hop
  [{:keys [link dir]} ids]
  {:op :search-around :link link :dir (ls/flip dir)
   :in {:op :static :ids (if (keyword? ids) [:param ids] ids)}})

(defn intersect-hops
  [ir hops]
  (if (seq hops) {:op :intersect :in (into [ir] hops)} ir))

#?(:clj
   (defn assert-readable!
     [ir]
     (try (ros/check-read! ir)
          ir
          (catch clojure.lang.ExceptionInfo e
            (anomaly/coded! :refusal.dialect/malformed-object-set :incorrect
              "malformed object set: {reason}"
              (ex-data e) {:reason (ex-message e)} e)))))

(defn unsupported-message
  [node supported]
  (str "cannot lower object-set node " (pr-str (:op node))
       " — supported: " (pr-str (vec (sort supported)))))

(def lifecycles
  {:permanent {:home :schema  :rid-tag :object-set
               :creator-only? false :ttl-seconds nil
               :doc "Saved in the ontology for reuse across the platform."}
   :temporary {:home :kernel  :rid-tag :temporary-object-set
               :creator-only? true  :ttl-seconds (* 60 60 24)
               :doc "Hands a set from one application/service to another;
                     readable only by its creator, expires within 24 hours."}})

(defn new-temporary-rid
  [uuid]
  (rid/new-rid (get-in lifecycles [:temporary :rid-tag]) uuid))

(defn temporary-rid?
  [x]
  (= (get-in lifecycles [:temporary :rid-tag]) (rid/rid-kind x)))

(defn temporary-ttl-seconds
  []
  (get-in lifecycles [:temporary :ttl-seconds]))

(def resource-keys
  #{:id :object-set-name :project :ignore-inherited :description :lifecycle})

(defn definition
  [spec]
  (when spec (apply dissoc spec resource-keys)))

(defn definition-kind
  [ir]
  (let [leaves (filter #(contains? leaf-ops (:op %)) (nodes ir))]
    (if (and (seq leaves) (every? #(= :static (:op %)) leaves))
      :static
      :dynamic)))

(defn by-name
  [object-sets ident]
  (when (and (map? object-sets) (some? ident))
    (or (get object-sets ident)
        (some (fn [[_ spec]] (when (= ident (:object-set-name spec)) spec))
              object-sets))))

(defn names
  [object-sets]
  (into #{} (keep (fn [[k spec]]
                    (or (:object-set-name spec) (when (keyword? k) k))))
        object-sets))

(defn param? [x] (and (vector? x) (= :param (first x))))

#?(:clj
   (defn resolve-sides
     [schema ir]
     (rewrite-bottom-up
      (fn [n]
        (if (#{:search-around :with} (:op n))
          (let [{:keys [link dir]} (ros/resolve-hop {} schema (result-type schema (:in n)) n)]
            (assoc n :link link :dir dir))
          n))
      ir)))
