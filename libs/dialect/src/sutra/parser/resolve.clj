(ns sutra.parser.resolve
  (:require [sutra.types.transform-io :as tio]
            [sutra.kit.anomaly :as anomaly]
            [sutra.types.refs :as refs]
            [sutra.types.column :as column]
            [sutra.types.attribute :as attribute]
            [sutra.parser.spec :as spec]
            [sutra.types.entities :as ents]
            [sutra.types.languages :as langs]
            [sutra.types.object-set :as os]
            [sutra.types.derive-graph :as dg]
            [sutra.types.query :as q]
            [sutra.types.table :as tbl]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def attr-name attribute/attr-name)

(defn- interface-attrs
  [interfaces iface seen]
  (if (or (contains? seen iface) (not (contains? interfaces iface)))
    #{}
    (let [i (get interfaces iface)]
      (into (set (map attr-name (concat (:property-list i) (:properties i))))
            (mapcat #(interface-attrs interfaces % (conj seen iface)) (:extends i))))))

(defn- full-attr-names
  [interfaces obj]
  (into (set (map attr-name (:attributes obj)))
        (when-not (:ignore-inherited obj)
          (mapcat #(interface-attrs interfaces % #{}) (:implements obj)))))

(defn- levenshtein [a b]
  (let [a (name a) b (name b)
        m (count a) n (count b)]
    (if (zero? (min m n))
      (max m n)
      (loop [i 1 prev (vec (range (inc n)))]
        (if (> i m)
          (peek prev)
          (recur (inc i)
                 (reduce (fn [cur j]
                           (conj cur (min (inc (peek cur))
                                          (inc (get prev j))
                                          (+ (get prev (dec j))
                                             (if (= (nth a (dec i)) (nth b (dec j))) 0 1)))))
                         [i] (range 1 (inc n)))))))))

(defn- suggest
  [miss candidates]
  (when (seq candidates)
    (let [[best d] (apply min-key second
                          (map (fn [c] [c (levenshtein miss c)]) candidates))]
      (when (<= d 3) best))))

(defn- did-you-mean [miss candidates]
  (if-let [s (suggest miss candidates)] (str " — did you mean " s "?") ""))

(defn symbol-table
  ([schema] (symbol-table schema nil))
  ([schema live]
   (let [objects    (:objects schema)
         interfaces (:interfaces schema)
         links      (:links schema)]
     {:object-names    (into (set (keys objects)) (:object-names live))
      :interface-names (into (set (keys interfaces)) (:interface-names live))
      :link-names      (into (set (map first links)) (:link-names live))
      :object-set-names (into (os/names (:object-sets schema)) (:object-set-names live))
      :action-names    (into (set (keys (:actions schema))) (:action-names live))
      :function-names  (into (set (keys (:functions schema)))
                             (:function-names live))
      :llm-models      (into (set (keys (:llm-models schema))) (:llm-models live))
      :application-names (into (set (keys (:applications schema))) (:application-names live))
      :project-names   (into (set (keys (:projects schema))) (:project-names live))
      :space-names     (into (set (keys (:spaces schema))) (:space-names live))
      :organization-names (into (set (keys (:organizations schema))) (:organization-names live))
      :value-type-names (into (set (keys (:value-types schema))) (:value-type-names live))
      :ss-types        (into (set (keep (fn [[ot o]] (when (:semantic-search o) ot)) objects))
                             (:ss-types live))
      :links           (merge (:links live)
                              (into {} (map (fn [l] [(first l) {:from (nth l 1) :to (nth l 2)
                                                                :cardinality (nth l 3 nil)
                                                                :opts (get l 4)}])) links))
      :cols            (into {} (map (fn [[ot obj]]
                                       [ot (set (map column/attr->col
                                                     (full-attr-names interfaces obj)))])) objects)
      :iface-links     (merge-with into
                                   (into {}
                                         (map (fn [[k i]]
                                                [k (set (map :link-name
                                                             (concat (:link-constraints i)
                                                                     (:links i))))]))
                                         interfaces)
                                   (:iface-links live))
      :produced-links  (->> (:transforms schema)
                            vals
                            (mapcat tio/outputs-of)
                            (keep (fn [o] (when (= :link-type (:kind o)) (:link-type o))))
                            set)})))

(defn- connected-link-refs
  [expr]
  (map second (q/connected-nodes expr)))

(defn- link-sides
  [symbols]
  (reduce (fn [m [l {:keys [from to cardinality opts]}]]
            (-> m
                (update-in [from (or (:forward-api-name opts) l)] (fnil conj [])
                           {:link l :dir :to :far to :cardinality cardinality})
                (update-in [to (or (:reverse-api-name opts) l)] (fnil conj [])
                           {:link l :dir :from :far from :cardinality cardinality})))
          {} (:links symbols)))

(defn- sides-of
  [schema symbols ot]
  (let [by-type (link-sides symbols)]
    (apply merge-with into (get by-type ot)
           (map #(get by-type %) (get-in schema [:objects ot :implements])))))

(defn- side-of
  [schema symbols ot handle]
  (let [cands (get (sides-of schema symbols ot) handle)]
    (when (= 1 (count cands)) (first cands))))

(defn- side-name-conflict-errors
  [schema symbols]
  (for [ot          (keys (:objects schema))
        [n cands]   (sides-of schema symbols ot)
        :when       (< 1 (count cands))]
    {:severity :error :where [:objects ot]
     :message  (str (name ot) " reaches " (count cands) " link sides named " n " ("
                    (str/join ", " (map #(str (name (:link %)) " " (name (:dir %))) cands))
                    ") — a side's API name must be unique per object type. Give the"
                    " link :forward-api-name / :reverse-api-name that differ.")}))

(defn- connected-one-refs
  [expr]
  (filter #(= :connected-one (first %)) (q/connected-nodes expr)))

(defn- at-most-one?
  [{:keys [dir cardinality]}]
  (cond
    (nil? cardinality) nil
    (= :to dir)        (contains? #{:many-to-one :one-to-one} cardinality)
    :else              (contains? #{:one-to-many :one-to-one} cardinality)))

(defn- connected-one-cardinality-errors
  [schema symbols]
  (for [[ot obj] (:objects schema)
        item     (:attributes obj)
        :when    (vector? item)
        :let     [expr (get-in item [2 :derived :expression])]
        :when    expr
        node     (connected-one-refs expr)
        :let     [lref (second node)
                  side (side-of schema symbols ot lref)]
        :when    (and side (false? (at-most-one? side)))]
    {:severity :error :where [:objects ot (attr-name item)]
     :message  (str ":connected-one on " (name ot) "." (name (attr-name item))
                    " traverses " lref " (" (name (:cardinality side)) ") from the "
                    (if (= :to (:dir side)) "from" "to") " side, which can reach MANY"
                    " objects — :connected-one selects THE linked object. Use :agg :arg-max"
                    " (pick by a sort attribute) or an aggregate like :sum/:count instead.")}))

(defn- derived-expr-map
  [schema]
  (into {}
        (for [[ot obj] (:objects schema)]
          [ot (into {} (for [item (:attributes obj)
                             :when (vector? item)
                             :let  [ex (get-in item [2 :derived :expression])]
                             :when ex]
                         [(attr-name item) ex]))])))

(defn- rollup-derived-cycle-errors
  [schema symbols]
  (let [dmap (derived-expr-map schema)
        g    (dg/graph (keys dmap)
                       #(get dmap %)
                       (fn [ot handle] (:far (side-of schema symbols ot handle))))]
    (for [cyc (dg/cycles g)]
      {:severity :error :where [:objects (ffirst cyc) (second (first cyc))]
       :message  (dg/cycle-message (fn [[o a]] (str (name o) "." (name a))) cyc)})))

(defn- attr-type
  [schema obj attr]
  (or (some (fn [item] (when (and (vector? item) (= attr (first item))) (second item)))
            (:attributes obj))
      (get-in schema [:shared-properties attr :type])))

(defn- vector-type? [t] (and (vector? t) (= :vector (first t))))

(def ^:private sections
  {:objects     [:object-names     "object type"]
   :links       [:link-names       "link type"]
   :interfaces  [:interface-names  "interface"]
   :object-sets [:object-set-names "object-set"]
   :actions     [:action-names     "action"]
   :applications [:application-names "application"]
   :functions   [:function-names   "function"]
   :llm-models  [:llm-models       "llm-model"]
   :projects    [:project-names    "project"]
   :spaces      [:space-names      "space"]
   :organizations [:organization-names "organization"]
   :value-types [:value-type-names "value type"]})

(def empty-slot
  ::none)

(defn- unresolved
  [{:keys [rule where subject slot ref names noun]
    :or {rule :referenz/ungelöst}}]
  {:severity :error :rule rule :where where
   :message  (if (= empty-slot ref)
               (str subject " names no " slot)
               (str subject " " slot " " (pr-str ref) ", which is not a declared " noun
                    (did-you-mean ref names)))})

(def ^:private owning-sections
  (into (sorted-set)
        (keep (fn [[section refs]]
                (when (some #(= [:project] (:at %)) refs)
                  (ents/section->author section))))
        ents/refs))

(defn- section-entries
  [schema section]
  (if (= :links section)
    (for [l (:links schema)] [(first l) (get l 4)])
    (seq (get schema section))))

(defn- options-from-sources
  [schema]
  (concat
   (for [[an action] (:actions schema)
         [pn pspec]  (:params action)
         :let  [of (:options-from pspec)]
         :when of]
     [[:actions an :params pn]
      (str "action " (name an) " param " (name pn)) of])
   (for [[tn tmpl] (:templates schema)
         [pn pspec] (:parameters tmpl)
         :let  [of (:options-from pspec)]
         :when of]
     [[:templates tn :parameters pn]
      (str "template " (name tn) " param " (name pn)) of])))

(def ^:private interface-rule-ops
  #{:create-of-interface :update-of-interface :delete-of-interface
    :create-link-of-interface :delete-link-of-interface})

(def ^:private interface-link-rule-ops
  #{:create-link-of-interface :delete-link-of-interface})

(defn- constraint-names
  [schema symbols i]
  (loop [acc #{} seen #{} todo [i]]
    (if-let [[x & more] (seq todo)]
      (if (seen x)
        (recur acc seen more)
        (recur (into acc (get (:iface-links symbols) x #{})) (conj seen x)
               (concat more (get-in schema [:interfaces x :extends]))))
      acc)))

(defn- interface-rule-steps
  [schema ops]
  (for [[an action] (:actions schema)
        step        (refs/steps (:rules action))
        :when       (ops (:op step))]
    [an step]))

(def reference-rules
  [{:slot ":value-type" :into :value-types :required false
    :grammar '[sutra.parser.ent/SharedPropertyMap :value-type]
    :refs (fn [schema _st]
            (for [[sp spec] (:shared-properties schema)
                  :let  [vt (:value-type spec)]
                  :when (keyword? vt)]
              {:where [:shared-properties sp :value-type]
               :subject (str "shared property " (name sp)) :ref vt}))}

   {:slot "traverses link" :into :links :required false
    :refs (fn [schema symbols]
            (for [[ot obj] (:objects schema)
                  item     (:attributes obj)
                  :when    (vector? item)
                  :let     [expr (get-in item [2 :derived :expression])]
                  :when    expr
                  lref     (distinct (connected-link-refs expr))
                  :when    (keyword? lref)]
              {:where   [:objects ot (attr-name item)]
               :subject (str "rollup/derived on " (name ot) "." (name (attr-name item)))
               :ref     lref
               :noun    (str "link side of " (name ot))
               :names   (into (set (keys (sides-of schema symbols ot)))
                              (remove (set (keys (:links symbols))))
                              (:link-names symbols))}))}

   {:slot "implements" :into :interfaces :required false
    :grammar '[sutra.parser.ent/ObjectEntry :implements]
    :refs (fn [schema _st]
            (for [[ot obj] (:objects schema)
                  iface    (:implements obj)
                  :when    (keyword? iface)]
              {:where [:objects ot :implements] :subject (str ot) :ref iface}))}

   {:slot ":satisfies" :into :interfaces :required true
    :grammar '[sutra.parser.ent/InterfaceLinkMembership :interface]
    :refs (fn [schema _st]
            (for [link (:links schema)
                  :let [s (:satisfies (get link 4))]
                  s    (if (map? s) [s] s)]
              {:where [:links (first link) :satisfies] :subject (str "link " (first link))
               :ref (or (:interface s) empty-slot)}))}

   {:required true :sections [:interfaces]
    :grammar '[sutra.parser.ent/InterfaceLinkMembership :constraint]
    :refs (fn [schema symbols]
            (for [link  (:links schema)
                  :let  [s (:satisfies (get link 4))]
                  s     (if (map? s) [s] s)
                  :let  [iface (:interface s)]
                  :when (contains? (:interface-names symbols) iface)]
              {:where [:links (first link) :satisfies] :subject (str "link " (first link))
               :slot (str ":satisfies " iface "/") :noun (str "link-constraint of " iface)
               :ref (or (:constraint s) empty-slot)
               :names (get (:iface-links symbols) iface #{})}))}

   {:slot ":options-from references object-set" :into :object-sets :required false
    :grammar '[sutra.parser.ent/OptionsFromMap :object-set]
    :refs (fn [schema _st]
            (for [[where subject of] (options-from-sources schema)
                  :when (:object-set of)]
              {:where where :subject subject :ref (:object-set of)}))}

   {:slot ":options-from references object-type" :into :objects :required false
    :grammar '[sutra.parser.ent/OptionsFromMap :object-type]
    :refs (fn [schema _st]
            (for [[where subject of] (options-from-sources schema)
                  :when (:object-type of)]
              {:where where :subject subject :ref (:object-type of)}))}

   {:slot ":interface" :into :interfaces :required true
    :refs (fn [schema _st]
            (for [[an step] (interface-rule-steps schema interface-rule-ops)
                  :let  [i (:interface step)]
                  :when (or (nil? i) (keyword? i))]
              {:where [:actions an :rules (:op step)]
               :subject (str "action " (name an) " rule " (:op step))
               :ref (or i empty-slot)}))}

   {:slot ":object-type" :into :objects :required false
    :refs (fn [schema _st]
            (for [[an step] (interface-rule-steps schema interface-rule-ops)
                  :when     (keyword? (:object-type step))]
              {:where [:actions an :rules (:op step)]
               :subject (str "action " (name an) " rule " (:op step))
               :ref (:object-type step)}))}

   {:required true :sections [:interfaces]
    :refs (fn [schema symbols]
            (for [[an step] (interface-rule-steps schema interface-link-rule-ops)
                  :let  [i (:interface step)]
                  :when (contains? (:interface-names symbols) i)]
              {:where [:actions an :rules (:op step)]
               :subject (str "action " (name an) " rule " (:op step))
               :slot (str ":constraint of " i "/") :noun (str "link-constraint of " i)
               :ref (or (:constraint step) empty-slot)
               :names (constraint-names schema symbols i)}))}

   {:rule :aktion/unbekannter-typ :required false
    :refs (fn [schema _st]
            (for [[an action] (:actions schema)
                  step        (refs/steps (:rules action))
                  :let  [[k into] (case (:op step)
                                    (:create :update :delete-object) [:object-type :objects]
                                    (:create-link :delete-link)      [:link-type :links]
                                    nil)]
                  :when (and k (keyword? (get step k)))]
              {:where [:actions an :rules (:op step)]
               :subject (str "action " (name an) " rule " (:op step))
               :slot (str "names " k) :into into :ref (get step k)}))}

   {:rule :aktion/function-rule :slot "function rule names" :into :functions :required false
    :refs (fn [schema _st]
            (for [[an action] (:actions schema)
                  :let  [f (:function (spec/function-rule action))]
                  :when (keyword? f)]
              {:where [:actions an :rules spec/function-rule-op]
               :subject (str "action " (name an))
               :ref f}))}

   {:rule :aktion/unbekanntes-ziel :required false
    :refs (fn [schema symbols]
            (let [kw   #(if (string? %) (keyword %) %)
                  both (into (:object-names symbols) (:link-names symbols))]
              (for [[an action] (:actions schema)
                    :let  [tgt (:target action)
                           r   (cond
                                 (or (keyword? tgt) (string? tgt))
                                 {:slot ":target" :noun "object type or link type"
                                  :sections [:objects :links] :ref (kw tgt) :names both}
                                 (:target-object-type tgt)
                                 {:slot ":target-object-type" :into :objects
                                  :ref (kw (:target-object-type tgt))}
                                 (:target-link-type tgt)
                                 {:slot ":target-link-type" :into :links
                                  :ref (kw (:target-link-type tgt))})]
                    :when r]
                (assoc r :where [:actions an :target] :subject (str "action " (name an))))))}

   {:rule :aktion/unbekannter-parametertyp :slot "has type"
    :noun "storable value type, object/link type or interface" :sections [:objects :links]
    :required false
    :refs (fn [schema symbols]
            (let [both (into (:object-names symbols)
                             (concat (:link-names symbols) (:interface-names symbols)))]
              (for [[an action] (:actions schema)
                    [pn pspec]  (:params action)
                    :let  [t (:type pspec)]
                    :when (and (keyword? t) (not (spec/scalar-value-types t)))]
                {:where [:actions an :params pn]
                 :subject (str "action " (name an) " param " (name pn))
                 :ref t :names both})))}

   {:slot ":semantic-search :property" :sections [:objects] :required true
    :grammar '[sutra.parser.ent/SemanticSearch :property]
    :refs (fn [schema _st]
            (for [[ot obj] (:objects schema)
                  :let  [ss (:semantic-search obj)]
                  :when ss]
              {:where [:objects ot :semantic-search] :subject (str ot)
               :noun (str "attribute of " ot) :ref (or (:property ss) empty-slot)
               :names (into (full-attr-names (:interfaces schema) obj)
                            (keys (:shared-properties schema)))}))}

   {:slot ":semantic-search :model" :into :llm-models :required false
    :grammar '[sutra.parser.ent/SemanticSearch :model]
    :refs (fn [schema _st]
            (for [[ot obj] (:objects schema)
                  :let  [m (:model (:semantic-search obj))]
                  :when m]
              {:where [:objects ot :semantic-search] :subject (str ot)
               :ref m}))}

   {:required false
    :grammar '[sutra.parser.ent/AgentTool :action]
    :refs (fn [schema _st]
            (for [[k agent] (:agents schema)
                  :let  [an (or (:agent-name agent) k)]
                  {:keys [kind action function object-type]} (:tools agent)
                  :let  [r (case kind
                             :action       {:slot "tool :action" :into :actions :ref action}
                             :function     {:slot "tool :function" :into :functions :ref function}
                             :object-query {:slot "tool :object-query" :into :objects
                                            :ref object-type}
                             nil)]
                  :when (and r (some? (:ref r)))]
              (assoc r :where [:agents an] :subject (str "agent " (name an)))))}

   {:required false
    :grammar '[sutra.parser.ent/WorkspaceItem :app]
    :refs (fn [schema _st]
            (for [[k ws] (:workspaces schema)
                  :let  [wn (or (:workspace-name ws) k)]
                  panel (:home ws)
                  {:keys [kind app object-type]} (:items panel)
                  :let  [r (case kind
                             :app         {:slot "home item :app" :into :applications :ref app}
                             :object-type {:slot "home item :object-type" :into :objects
                                           :ref object-type}
                             nil)]
                  :when (and r (some? (:ref r)))]
              (assoc r :where [:workspaces wn]
                     :subject (str "workspace " (name wn)))))}

   {:slot ":model" :into :llm-models :required false
    :grammar '[sutra.parser.ent/AgentEntry :model]
    :refs (fn [schema _st]
            (for [[k agent] (:agents schema)
                  :let  [an (or (:agent-name agent) k)]
                  :when (:model agent)]
              {:where [:agents an] :subject (str "agent " (name an)) :ref (:model agent)}))}

   {:slot ":retrieval object-type" :noun "type declaring :semantic-search"
    :sections [:objects] :required true
    :grammar '[sutra.parser.ent/AgentRetrieval :object-type]
    :refs (fn [schema symbols]
            (for [[k agent] (:agents schema)
                  :let  [an (or (:agent-name agent) k)
                         {:keys [kind object-type]} (:retrieval agent)]
                  :when (= :semantic-search kind)]
              {:where [:agents an] :subject (str "agent " (name an))
               :ref (or object-type empty-slot) :names (:ss-types symbols)}))}

   {:slot ":project" :into :projects :required false
    :refs (fn [schema _st]
            (for [section  owning-sections
                  [k spec] (section-entries schema section)
                  :let     [p (:project spec)]
                  :when    (keyword? p)]
              {:where   [section k :project]
               :subject (str (name section) " " (name k))
               :ref     p}))}

   {:slot ":space" :into :spaces :required false
    :refs (fn [schema _st]
            (for [[k proj] (:projects schema)
                  :let     [s (:space proj)]
                  :when    (keyword? s)]
              {:where [:projects k :space] :subject (str "project " (name k)) :ref s}))}

   {:slot ":organization" :into :organizations :required false
    :refs (fn [schema _st]
            (for [[k space] (:spaces schema)
                  :let      [o (:organization space)]
                  :when     (keyword? o)]
              {:where [:spaces k :organization] :subject (str "space " (name k)) :ref o}))}

   {:slot ":shared-with" :into :organizations :required false
    :refs (fn [schema _st]
            (for [section [:projects :spaces]
                  [k spec] (get schema section)
                  o        (:shared-with spec)
                  :when    (keyword? o)]
              {:where [section k :shared-with] :subject (str (name section) " " (name k)) :ref o}))}

   {:slot ":imports" :into :projects :required false
    :refs (fn [schema _st]
            (for [[k proj] (:projects schema)
                  imp      (:imports proj)
                  :when    (keyword? imp)]
              {:where   [:projects k :imports]
               :subject (str "project " (name k))
               :ref     imp}))}])

(tbl/holds! "every reference rule says whether its ref is required"
            (keep #(when-not (boolean? (:required %)) (or (:slot %) (:rule %))) reference-rules))

(tbl/holds! "a named slot of a rule has one row"
            (for [[k n] (frequencies (keep #(when (:slot %) [(:rule %) (:slot %)]) reference-rules))
                  :when (> n 1)]
              k))

(defn- reference-errors
  [schema symbols run?]
  (for [{:keys [refs] :as rule} reference-rules
        :let  [static (or (:sections rule) (some-> (:into rule) vector))]
        :when (or (:required rule) (nil? static) (every? run? static))
        r     (refs schema symbols)
        :let  [r     (merge (dissoc rule :refs :grammar) r)
               ref   (:ref r)
               [names-key noun] (sections (:into r))
               known (or (:names r) (get symbols names-key))
               sects (or (:sections r) [(:into r)])]
        :when (if (= empty-slot ref)
                (:required r)
                (and (every? run? sects)
                     (not (contains? known ref))))]
    (unresolved (assoc r :names known :noun (or (:noun r) noun)))))

(defn- action-target-shape-errors
  [schema]
  (for [[an action] (:actions schema)
        :let  [tgt (:target action)]
        :when (and tgt (not (or (keyword? tgt) (string? tgt)
                                (:target-object-type tgt) (:target-link-type tgt))))]
    {:severity :error :rule :aktion/unbekanntes-ziel
     :where [:actions an :target]
     :message  (str "action " (name an) " :target " (pr-str tgt) " is neither a type name nor a"
                    " {:target-object-type …} / {:target-link-type …} map")}))

(defn- semantic-search-type-errors
  [schema]
  (for [[ot obj] (:objects schema)
        :let  [prop (:property (:semantic-search obj))
               t    (when prop (attr-type schema obj prop))]
        :when (and t (not (vector-type? t)))]
    {:severity :error :where [:objects ot :semantic-search]
     :message  (str ot " :semantic-search :property " prop
                    " is not a [:vector n] property (got " (pr-str t) ")")}))

(defn- function-rule-errors
  [schema]
  (concat
   (for [[an action] (:actions schema)
         :let  [problem (spec/function-rule-problem action)]
         :when problem]
     {:severity :error :rule :aktion/function-rule
      :where [:actions an :rules spec/function-rule-op]
      :message   (str "action " (name an) ": " problem)})
   (for [[an action] (:actions schema)
         :let  [dep     (first (filter spec/function-dep-problem
                                       (spec/function-deps action)))
                problem (some-> dep spec/function-dep-problem)]
         :when problem]
     {:severity :error :rule :aktion/function-version
      :where [:actions an :rules (:op dep)]
      :message   (str "action " (name an) ": " problem)})))

(defn- action-edit-errors
  [schema _st]
  (for [[an action] (:actions schema)
        :when (nil? (spec/function-rule action))
        :let  [derived (set (spec/derive-edits action))]
        edit  (:edits action)
        :when (not (contains? derived edit))]
    {:severity :error :rule :aktion/phantom-edit
     :where [:actions an :edits]
     :message  (str "action " (name an) " declares the edit " (pr-str edit)
                    " but no rule performs it — declared :edits must be a subset of "
                    "what the rules mutate"
                    (when (seq derived)
                      (str "; the rules mutate " (pr-str (vec derived)))))}))

(defn- alias->object
  [query inputs]
  (let [key->ot (into {} (keep (fn [[k v]] (when (= :object-type (:kind v))
                                             [(name k) (:object-type v)])) inputs))]
    (into {}
          (for [[_ view als] (re-seq #"(?i)(?:FROM|JOIN)\s+(\w+)(?:\s+(?:AS\s+)?(\w+))?" query)
                :let [ot (key->ot view)]
                :when ot
                :let [alias (or (and (not (#{"on" "where" "group" "join" "left" "inner"}
                                           (some-> als str/lower-case))) als)
                                view)]]
            [alias ot]))))

(defn- sql-column-warnings
  [schema symbols]
  (for [[tname t] (:transforms schema)
        :when     (and (langs/query-language? (:language t)) (string? (:query t))
                       (map? (:inputs t)))
        :let      [a->o (alias->object (:query t) (:inputs t))]
        [_ qual col] (re-seq #"(\w+)\.(\w+)" (:query t))
        :let      [ot   (a->o qual)
                   cols (get (:cols symbols) ot)]
        :when     (and ot cols (not (contains? cols (keyword col))))]
    {:severity :warn :where [:transforms tname]
     :message  (str "transform " tname ": `" qual "." col "` — " ot " has no column "
                    col (did-you-mean (keyword col) cols))}))

(defn- orphan-link-warnings [schema symbols]
  (for [link  (:links schema)
        :let  [nm (first link) opts (get link 4)]
        :when (and (not (contains? (:produced-links symbols) nm))
                   (not (:external-source opts)))]
    {:severity :warn :where [:links nm]
     :message  (str "link " nm " has no producer (no transform outputs it, no "
                    ":external-source, no inline :join) — it will be an empty edge "
                    "unless an Action populates it")}))

(defn- column-fold-collision-errors
  [schema symbols]
  (for [[ot obj] (:objects schema)
        :when    (contains? (:cols symbols) ot)
        :let     [by-col (group-by column/attr->col (full-attr-names (:interfaces schema) obj))]
        [col attrs] by-col
        :when    (< 1 (count attrs))]
    {:severity :error :rule :attribut/spaltenkollision
     :where [:objects ot]
     :message (str ot " has " (count attrs) " attributes that become the SAME physical column "
                   (pr-str col) ": " (str/join ", " (map pr-str (sort attrs)))
                   ". The fold to a column name drops `?` and folds umlauts, so these differ"
                   " only in characters no column may carry. Rename one — the catalog would"
                   " otherwise refuse the whole install with \"Column with name "
                   (name col) " already exists\", after the push.")}))

(defn- located
  [{:keys [where severity rule] :as f}]
  (let [[cls ent] where]
    (cond-> (assoc f :rule (or rule (if (= :error severity)
                                      :referenz/ungelöst
                                      :referenz/verdächtig)))
      (some? cls) (assoc :entity-class cls)
      (some? ent) (assoc :entity ent))))

(defn findings
  ([schema] (findings schema nil))
  ([schema live]
   (let [symbols   (symbol-table schema live)
         run? (fn [section] (or (some? live) (contains? schema section)))]
     (->> (concat (reference-errors schema symbols run?)
                  (when (run? :objects) (rollup-derived-cycle-errors schema symbols))
                  (when (run? :links)   (connected-one-cardinality-errors schema symbols))
                  (when (run? :links)   (side-name-conflict-errors schema symbols))
                  (action-target-shape-errors schema)
                  (action-edit-errors schema symbols)
                  (function-rule-errors schema)
                  (semantic-search-type-errors schema)
                  (sql-column-warnings schema symbols)
                  (when (run? :links) (orphan-link-warnings schema symbols))
                  (when (run? :objects) (column-fold-collision-errors schema symbols)))
          (remove nil?)
          (distinct)
          (mapv located)))))

(defn problems
  ([schema] (problems schema nil))
  ([schema live]
   (let [all   (group-by :severity (findings schema live))
         errs  (->> (:error all) (map :message) distinct vec)
         warns (->> (:warn all)  (map :message) distinct vec)]
     (when (or (seq errs) (seq warns))
       (cond-> {} (seq errs) (assoc :errors errs) (seq warns) (assoc :warnings warns))))))

(defn explain
  ([schema] (explain schema nil))
  ([schema live] (seq (:errors (problems schema live)))))

(defn check!
  [schema source]
  (if-let [errs (explain schema)]
    (anomaly/coded! :refusal.dialect/unresolved-references :incorrect
      "Invalid .ent ({source}): unresolved references\n  - {listed}"
      {:source source :errors errs}
      {:listed (str/join "\n  - " errs)})
    schema))
