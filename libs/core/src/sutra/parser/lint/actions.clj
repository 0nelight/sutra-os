(ns sutra.parser.lint.actions
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.lint.support :refer [->name]]
            [clojure.string :as str]
            [sutra.kit.tree :as tree]
            [sutra.capps.derived :as cd]
            [sutra.capps.prop-source :as ps]
            [sutra.capps.table :as tbl]
            [sutra.types.rid :as rid]
            [sutra.types.submission :as csub]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(def rules
  [{:id :fk-without-link :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (let [rid->name (into {} (for [[rid spec] (:object-types schema)]
                                       [rid (some-> (:object-type-name spec) ->name)]))
                  nm-of     (fn [x] (or (rid->name x) (->name x)))
                  known     (into #{} (keep identity) (vals rid->name))
                  pairs     (into #{} (for [[_ l] (:link-types schema)
                                            :let [f (nm-of (:from-object-type l))
                                                  t (nm-of (:to-object-type l))]
                                            :when (and f t)
                                            p [[f t] [t f]]]
                                        p))]
              (for [[rid spec] (:object-types schema)
                    :let [nm (rid->name rid)]
                    :when nm
                    a (:attribute-list spec)
                    :let [fk   (get-in a [:opts :fk])
                          attr (->name (:attribute-name a))
                          tgt  (->name fk)]
                    :when (and attr tgt)
                    :when (or (not (known tgt)) (not (pairs [nm tgt])))]
                {:entity (str nm "." attr) :entity-class :object-types
                 :message (str "Attribute " (pr-str attr) " on " (pr-str nm)
                               " declares `:fk " (pr-str fk) "`"
                               (if (known tgt)
                                 (str " but no link-type connects " (pr-str nm) " and "
                                      (pr-str tgt)
                                      ". A foreign key with no link is a key a consumer can"
                                      " only resolve by joining outside the ontology."
                                      " Declare the link (`[:" nm "-" tgt " :" nm " :" tgt
                                      " :many-to-one"
                                      " {:fk {:attribute :" attr "}}]`) — then `:group-by"
                                      " [[:link …]]` and a :derived selection across it"
                                      " become expressible.")
                                 (str " but " (pr-str tgt)
                                      " is not an object type in this ontology.")))})))}

   {:id :action-param-without-name :severity :error :gate? true :scope :change
    :doc "A parameter is the action's interface to App Studio and Object Views: the form shows it by its name, and without one
           the person filling it in reads its id."
    :scan (fn [schema]
            (for [[k a] (:actions schema)
                  :let  [nm (->name (or (:action-name a) (:name a) k))]
                  [p spec] (:params a)
                  :when (and (map? spec)
                             (not= :hidden (some-> (:exposure spec) keyword))
                             (str/blank? (str (:name spec))))]
              {:entity nm :entity-class :actions
               :message (str "Action " (pr-str nm) " shows parameter " (pr-str (->name p))
                             " with no :name — the form would read " (pr-str (->name p))
                             ". Name it (`:name \"…\"`), or let it write ONE property"
                             " whose attribute carries a :name, and it takes that one.")}))}

   {:id :action-param-type-follows-property :severity :info
    :doc nil
    :scan (fn [schema]
            (let [by-name (into {} (for [[_ s] (:object-types schema)]
                                     [(:object-type-name s) s]))
                  attr-type (fn [ot prop]
                              (some (fn [{a :attribute-name t :attribute-type}]
                                      (when (= a prop) t))
                                    (:attribute-list (get by-name ot))))
                  param-of (fn [v] (cond (keyword? v) v
                                         (and (vector? v) (= :get (first v))) (second v)))]
              (for [[k a] (:actions schema)
                    :let  [nm     (->name (or (:action-name a) (:name a) k))
                           params (:params a)
                           writes (reduce (fn [m {:keys [object-type values]}]
                                            (reduce-kv (fn [m prop v]
                                                         (if-let [p (param-of v)]
                                                           (update m p (fnil conj #{})
                                                                   [object-type prop])
                                                           m))
                                                       m (or values {})))
                                          {} (filter :values (:rules a)))]
                    [p spec] params
                    :let  [tgt (get writes p)]
                    :when (= 1 (count tgt))
                    :let  [[ot prop] (first tgt)
                           want (some-> (attr-type ot prop) csub/param-type-of)]
                    :when (not (or (contains? by-name (:type spec)) (rid/rid? (:type spec))))
                    :when (and want (:type spec) (not= want (:type spec)))]
                {:entity (str nm "." (->name p))
                 :entity-class :actions
                 :message (str "Action " (pr-str nm) "'s parameter " (pr-str p)
                               " is declared " (pr-str (:type spec)) " but writes "
                               (->name ot) "." (->name prop) ", whose type derives "
                               "a " (pr-str want) " parameter.")})))}

   {:id :prefill-type-mismatch :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (let [types   (:object-types schema)
                  by-name (into {} (for [[_ s] types] [(->name (:object-type-name s)) s]))
                  ot-of   (fn [t] (or (get types t)
                                      (when (keyword? t) (get types (subs (str t) 1)))
                                      (get by-name (->name t))))
                  prop-t  (fn [ot prop]
                            (some (fn [{a :attribute-name t :attribute-type}]
                                    (when (= (->name a) (->name prop)) t))
                                  (:attribute-list ot)))
                  cls     (fn [t] (vt/class-of (if (sequential? t) (first t) t)))]
              (for [[k a] (sort-by (comp str key) (:actions schema))
                    :let  [nm     (->name (or (:action-name a) (:name a) k))
                           params (:params a)]
                    [p spec] (sort-by (comp str key) params)
                    :let  [d (:default spec)]
                    :when (and (map? d) (:from d) (:property d))
                    :let  [want (cls (->kw (:type spec)))
                           src  (get params (->kw (:from d)))
                           ot   (some-> (:type src) ot-of)
                           have (when ot (prop-t ot (:property d)))]
                    :when (and (#{:number :boolean :temporal} want) have
                               (not= want (cls have)))]
                {:entity (str nm "." (->name p)) :entity-class :actions
                 :message (str "Action " (pr-str nm) ": Parameter " (pr-str (->kw p))
                               " ist " (pr-str (->kw (:type spec))) ", seine Vorbelegung "
                               (->name (:object-type-name ot)) "." (->name (:property d))
                               " ist " (pr-str have)
                               ". Typisiere die Eigenschaft passend (im Transform, der"
                               " sie liefert) oder den Parameter wie die Eigenschaft.")})))}

   {:id :action-writes-derived :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (let [by-name  (into {} (mapcat (fn [[rid s]] [[rid s] [(:object-type-name s) s]]))
                                 (:object-types schema))
                  derived? (fn [ot prop]
                             (some (fn [{a :attribute-name o :opts}]
                                     (and (= a prop) (:derived o)))
                                   (:attribute-list (get by-name ot))))]
              (for [[k a] (sort-by (comp str key) (:actions schema))
                    :let [nm (->name (or (:action-name a) (:name a) k))]
                    {:keys [object-type values]} (:rules a)
                    prop (sort (keys values))
                    :when (derived? object-type prop)]
                {:entity (str nm "." (->name prop)) :entity-class :actions
                 :message (str "Action " (pr-str nm) " schreibt "
                               (->name (or (:object-type-name (get by-name object-type))
                                           object-type)) "."
                               (->name prop)
                               ", eine :derived-Eigenschaft. der geschriebene Wert wird beim Lesen"
                               " überdeckt. Soll ein Mensch den Wert übersteuern können, schreibe"
                               " eine gespeicherte Eigenschaft und leite ab mit [:coalesce [:get"
                               " <gespeichert>] <ausdruck>].")})))}

   {:id :column-restates-property :severity :info
    :doc nil
    :scan (fn [schema]
            (let [by-name (into {} (for [[_ s] (:object-types schema)]
                                     [(:object-type-name s) s]))
                  display (fn [ot]
                            (into {} (for [{a :attribute-name t :attribute-type o :opts}
                                           (:attribute-list (get by-name ot))]
                                       [a (tbl/prop-display
                                           {:type t :name (:name o) :format (:format o)} a)])))]
              (for [[_ app] (:applications schema)
                    :let  [nm   (->name (or (:application-name app) (:name app)))
                           tree (:layout app)]
                    :when (map? tree)
                    node  (tree/nodes tree)
                    :when (#{:object-table :action-table} (:widget node))
                    :let  [ot (ps/prop-type tree node)
                           d  (when ot (display ot))]
                    :when (seq d)
                    col   (:cols node)
                    :let  [inherit (get d (:prop col))]
                    :when inherit
                    [k what] [[:label "header"] [:align "alignment"]]
                    :when (and (some? (get col k)) (= (get col k) (get inherit k)))]
                {:entity (str nm "." (->name (:prop col)) "." (->name k))
                 :entity-class :applications
                 :message (str "Column " (pr-str (:prop col)) " in app " (pr-str nm)
                               " restates the " what " " (pr-str (get col k))
                               " that " (pr-str ot) "." (->name (:prop col))
                               " already declares — drop the key and it inherits, "
                               "so the ontology stays the one place it is said.")})))}

   {:id :data-freshness-names-nothing :severity :error
    :doc nil
    :scan (fn [schema]
            (let [names (fn [section k] (into #{} (map (comp ->name k val)) (get schema section)))
                  types (names :object-types :object-type-name)
                  rsrcs (into (names :datasets :dataset-name)
                              (names :transforms :transform-name))]
              (for [[_ app] (:applications schema)
                    :when (map? (:layout app))
                    node  (tree/nodes (:layout app))
                    :when (= :data-freshness (some-> (:widget node) ->kw))
                    :let  [nm (->name (or (:application-name app) (:name app)))]
                    {:keys [object-type sources]} (:items node)
                    [what known x] (cons ["object type" types object-type]
                                         (map (fn [s] ["datasource" rsrcs (:resource s)]) sources))
                    :when (not (contains? known (->name x)))]
                {:entity nm :entity-class :applications
                 :message (str "Data Freshness in app " (pr-str nm) " names " what " "
                               (pr-str x) ", which does not exist — the widget would leave "
                               "it out without a word.")})))}

   {:id :where-names-no-property :severity :error
    :doc nil
    :scan (fn [schema]
            (let [by-name (into {} (for [[_ s] (:object-types schema)]
                                     [(:object-type-name s) s]))
                  of-type (fn [app ot]
                            (concat (map :attribute-name
                                         (:attribute-list (get by-name ot)))
                                    (map first (cd/properties (:derived app) ot))))
                  joined  (fn joined [app index nm seen]
                            (let [spec (get index (->kw nm))]
                              (when (and (map? spec) (not (contains? seen nm)))
                                (concat
                                 (when-let [j (:join spec)]
                                   (or (seq (map ->kw (:as j)))
                                       (some->> (ps/ref-type index {:val (:index j)})
                                                (of-type app))))
                                 (joined app index (:from spec) (conj seen nm))))))
                  props   (fn [app index vn ot]
                            (into #{:id :*} (concat (of-type app ot)
                                                    (joined app index vn #{}))))]
              (for [[_ app] (:applications schema)
                    :let  [nm    (->name (or (:application-name app) (:name app)))
                           tree  (:layout app)]
                    :when (map? tree)
                    :let  [index (ps/vars-index tree)]
                    [vn spec] index
                    :when (and (map? spec) (sequential? (:where spec)))
                    :let  [ot    (ps/ref-type index {:val vn})
                           known (when ot (props app index vn ot))]
                    :when (seq known)
                    clause (:where spec)
                    :when (sequential? clause)
                    pname  (let [p (first clause)]
                             (if (sequential? p) p [p]))
                    :when  (and (keyword? pname) (not (contains? known pname)))]
                {:entity (str nm "." (->name vn) "." (->name pname))
                 :entity-class :applications
                 :message (str "Variable " (pr-str vn) " in app " (pr-str nm)
                               " filters on " (pr-str pname) ", which " (->name ot)
                               " does not declare — the clause matches no row, so the"
                               " widget reading this variable stays empty and nothing"
                               " says why. Name a property of the type, or take the"
                               " selection straight from the table's :active output"
                               " instead of re-finding it by id.")})))}])
