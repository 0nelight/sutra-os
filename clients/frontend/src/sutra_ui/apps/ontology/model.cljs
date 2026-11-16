(ns sutra-ui.apps.ontology.model
  (:require [sutra.types.submission :as csub]
            [sutra-ui.apps.ontology.events]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [clojure.string :as str]))

(defonce ui (shell/slice :ontology))

(defonce ontology (machine/frame ui {:name :ontology}))

(defn send! [event] (machine/dispatch event {:frame ontology}))

(defn edit!
  [path f & args]
  (send! (into [:ontology/edit path f] args)))

(defn value-type-base
  [t]
  (cond
    (nil? t)     nil
    (keyword? t) (name t)
    (string? t)  t
    (vector? t)  (name (first t))
    :else        (str t)))

(defn update-interface! [old-kw form] (send! [:ontology.interface/update old-kw form]))

(defn update-object-type-group! [g-kw form] (send! [:ontology.group/update g-kw form]))

(defn add-function! [form] (send! [:ontology.function/create form]))

(defn add-transform! [m] (send! [:ontology.transform/create m]))

(defn add-shared-property! [form] (send! [:ontology.shared-property/create form]))

(defn update-function!
  [old-name form then]
  (send! [:ontology.function/update old-name form then]))

(def action-rule-ops
  [{:op :create        :label "Create object"   :cat "Object"
    :type-ref {:field :object-type :kind :object-types}
    :binds [[:id "Object id ← param"] [:values "Values ← param"]]
    :value-map? true}
   {:op :update        :label "Modify object"   :cat "Object"
    :type-ref {:field :object-type :kind :object-types}
    :binds [[:target "Object id ← param"] [:values "Values ← param"]]
    :value-map? true}
   {:op :delete-object :label "Delete object"   :cat "Object"
    :type-ref {:field :object-type :kind :object-types}
    :binds [[:target "Object id ← param"]]}
   {:op :create-link   :label "Create link"     :cat "Link"
    :type-ref {:field :link-type :kind :link-types}
    :binds [[:id "Link id ← param"] [:from-id "From id ← param"] [:to-id "To id ← param"]]}
   {:op :delete-link   :label "Delete link"     :cat "Link"
    :type-ref {:field :link-type :kind :link-types}
    :binds [[:target "Link id ← param"]]}
   {:op :apply-marking :label "Apply marking"   :cat "Object"
    :type-ref {:field :object-type :kind :object-types}
    :binds [[:target "Object id ← param"] [:marking "Marking ← param"]]}
   {:op :remove-marking :label "Remove marking" :cat "Object"
    :type-ref {:field :object-type :kind :object-types}
    :binds [[:target "Object id ← param"] [:marking "Marking ← param"]]}
   {:op :invoke-function :label "Call a Query Function — binds its rows" :cat "Function"
    :picks [[:function :query-functions "Query Function"]]
    :free  [[:as "Bind result as"]]
    :texts [[:version "Pin version (e.g. 1.2.0) — blank = live source"]]
    :bools [[:auto-upgrade "Auto-upgrade to newer compatible releases"]]}
   {:op :apply-edit-function :label "Function rule — the Function makes every edit"
    :cat "Function"
    :picks [[:function :edit-functions "Edit Function"]]
    :texts [[:version "Pin version (e.g. 1.2.0) — blank = live source"]]
    :bools [[:auto-upgrade "Auto-upgrade to newer compatible releases"]
            [:batched "Batched execution — one call with the whole batch"]]
    :type-ref {:field :object-type :kind :object-types}
    :type-label "May edit (@edits provenance)"
    :type-is-provenance? true}
   {:op :invoke-webhook :label "Webhook side-effect" :cat "Side effect"
    :picks [[:source :sources "Source"]]
    :free  [[:webhook "Webhook name"]]
    :webhook-params? true}
   {:op :notify        :label "Notification"    :cat "Side effect"
    :free  [[:channel "Channel"]] :binds [[:payload "Payload ← param"]]}])

(defn action-op-meta [op] (some #(when (= (keyword op) (:op %)) %) action-rule-ops))

(defn- bind-param-name
  [v]
  (when (and (vector? v) (= :get (keyword (first v))) (> (count v) 1))
    (name (if (and (= :arg0 (keyword (second v))) (> (count v) 2)) (nth v 2) (second v)))))

(defn- val-expr
  [pname]
  [:get :arg0 (keyword pname)])

(def ^:private numeric-bases
  #{"integer" "long" "short" "byte" "double" "float" "decimal" "number"})

(defn object-type-props
  [ot]
  (when (seq (str ot))
    (vec (for [{a :attribute-name t :attribute-type}
               (get-in @catalog/canonical-schema [:object-types (keyword ot) :attribute-list])]
           [(name a) (value-type-base t)]))))

(defn- prop-base [ot prop]
  (some (fn [[p t]] (when (= p (str prop)) t)) (object-type-props ot)))

(defn form-param-type [form pname]
  (some #(when (= (:pname %) pname) (:type %)) (:params form)))

(defn rule-object-type
  [form rule]
  (cond
    (seq (:type rule))                   (:type rule)
    (get-in rule
            [:extra :object-type])  (some-> (get-in rule [:extra :object-type])
                                                catalog/name-of name)
    :else                                (form-param-type form (get-in rule [:bind :target]))))

(defn log-object-type?
  [spec]
  (str/starts-with? (str (:plural spec)) "[LOG]"))

(defn authorable-object-types
  []
  (->> (:object-types @catalog/canonical-schema)
       (remove (comp log-object-type? val))
       (map key) sort))

(defn object-param-names
  ([form] (object-param-names form nil))
  ([form ot]
   (let [ots (set (map name (keys (:object-types @catalog/canonical-schema))))]
     (->> (:params form)
          (filter #(and (contains? ots (:type %)) (or (nil? ot) (= (str ot) (:type %)))))
          (map :pname) (filter seq) vec))))

(defn- ->static [base v]
  (let [s (str v)]
    (cond
      (contains? numeric-bases base) (when-not (str/blank? s)
                                       (let [n (js/parseFloat s)] (if (js/isNaN n) s n)))
      (= "boolean" base)             (= "true" (str/lower-case (str/trim s)))
      :else                          s)))

(defn- context-param [src base]
  (csub/context-param (if (= "user" src) :current-user :now) base))

(defn- val-row->expr [ot {:keys [prop src param oprop value raw orig]}]
  (case src
    "param"        (when (seq param) (if (= param (bind-param-name orig)) orig (val-expr param)))
    "prop"         (when (and (seq param) (seq oprop))
                     (if (= [(keyword param) (keyword oprop)] (when (vector? orig) (subvec orig 1)))
                       orig
                       [:get (keyword param) (keyword oprop)]))
    "static"       (if (and (some? orig) (not (coll? orig)) (= value (str orig)))
                     orig
                     (->static (prop-base ot prop) value))
    ("now" "user") [:get (context-param src (prop-base ot prop))]
    "expr"         raw
    nil))

(defn- value-map [form rule]
  (let [ot (rule-object-type form rule)]
    (into {} (for [r (:vals rule)
                   :when (seq (:prop r))
                   :let [e (val-row->expr ot r)]
                   :when (some? e)]
               [(keyword (:prop r)) e]))))

(defn- webhook-param-map
  [rule]
  (into {} (for [r (:wvals rule)
                 :let [e (val-row->expr nil r)]
                 :when (some? e)]
             [(keyword (:prop r)) e])))

(defn webhook-inputs
  [source webhook]
  (when (and (seq (str source)) (seq (str webhook)))
    (mapv name
          (get-in @catalog/canonical-schema
                  [:sources (keyword source) :webhooks (keyword webhook) :params]))))

(defn- context-params [form]
  (into {}
        (concat
         (for [rule (:rules form)
               :when (:value-map? (action-op-meta (:op rule)))
               :let [ot (rule-object-type form rule)]
               r (:vals rule)
               :when (and (seq (:prop r)) (#{"now" "user"} (:src r)))
               :let [p (context-param (:src r) (prop-base ot (:prop r)))]]
           [p (get-in csub/implicit-context-params [p :spec])])
         (when (some #(= "user" (:left-src %))
                     (tree-seq :group :children
                               {:group true :children (:submission-criteria form)}))
           [[:regel-benutzer (get-in csub/implicit-context-params [:regel-benutzer :spec])]]))))

(defn- ->action-rule
  [form r]
  (let [m    (action-op-meta (:op r))
        tref (:type-ref m)]
    (cond-> (merge (:extra r) {:op (or (:op m) (keyword (:op r)))})
      (and tref (seq (:type r)) (not (:type-is-provenance? m)))
      (assoc (:field tref) (catalog/rid-of (:kind tref) (keyword (:type r))))
      true (into (for [[f _] (:binds m) :let [v (get-in r [:bind f])
                                              orig (get-in r [:bind-raw f])
                                              obj? (and (= :update (:op m)) (= :target f))]
                       :when (seq v)]
                   [f (cond
                        (and (= v (bind-param-name orig))
                             (not (and obj? (= :arg0 (keyword (second orig)))))) orig
                        obj?  [:get (keyword v)]
                        :else (val-expr v))]))
      true (into (for [[f _ _] (:picks m) :let [v (get-in r [:pick f])] :when (seq v)]
                   [f (keyword v)]))
      true (into (for [[f _] (:free m) :let [v (get-in r [:free f])] :when (seq v)]
                   [f (keyword v)]))
      true (into (for [[f _] (:texts m) :let [v (get-in r [:text f])] :when (seq v)]
                   [f (str v)]))
      true (into (for [[f _] (:bools m) :when (get-in r [:bool f])]
                   [f true]))
      (and (:value-map? m) (seq (:vals r)))
      (assoc :values (value-map form r))
      (and (:webhook-params? m) (seq (webhook-param-map r)))
      (assoc :params (webhook-param-map r)))))

(def ^:private edit-deriving-ops
  #{:create :update :delete-object :create-link :delete-link})

(defn- derive-action-edits
  [rules]
  (->> rules
       (keep (fn [r]
               (let [op   (keyword (:op r))
                     m    (action-op-meta op)
                     tref (:type-ref m)]
                 (when (and tref (seq (:type r))
                            (or (contains? edit-deriving-ops op)
                                (:type-is-provenance? m)))
                   {(:field tref) (catalog/rid-of (:kind tref) (keyword (:type r)))}))))
       distinct vec))

(defn- ->crit-value [v]
  (let [t (str/trim (str v))
        n (js/parseFloat t)]
    (cond (= t "") nil
          (and (not (js/isNaN n)) (re-matches #"-?\d+(\.\d+)?" t)) n
          :else t)))

(def param-form-keys [:type :name :required :exposure :prefill :default :constraints])

(defn- split-list [s]
  (->> (str/split (str s) #"[,\n]") (map str/trim) (remove str/blank?) vec))

(defn- one-of-values
  [{:keys [type one-of one-of-orig]}]
  (if (and (some? one-of-orig) (= one-of (str/join ", " (map str one-of-orig))))
    one-of-orig
    (mapv #(->static (if (contains? numeric-bases type) "number" type) %) (split-list one-of))))

(defn- ->param-spec
  [{:keys [type label required hidden prefill default-src default default-from default-prop extra
           constraints]
    :as p}]
  (let [allowed (one-of-values p)
        cons    (cond-> (or constraints {})
                  (seq allowed) (assoc :one-of allowed))]
    (cond-> (merge extra {:type (keyword type)})
      (seq label)    (assoc :name label)
      required       (assoc :required true)
      hidden         (assoc :exposure :hidden)
      (seq prefill)  (assoc :prefill (keyword prefill))
      (seq cons)     (assoc :constraints cons)
      (and (= "static" default-src) (seq (str default)))
      (assoc :default (if (and (contains? p :default-orig) (= default (str (:default-orig p))))
                        (:default-orig p)
                        (->static (if (contains? numeric-bases type) "number" type) default)))
      (and (= "prop" default-src) (seq default-from) (seq default-prop))
      (assoc :default {:from (keyword default-from) :property (keyword default-prop)}))))

(def crit-operators
  [{:op "="       :for #{:any}}
   {:op "not="    :for #{:any}}
   {:op "<"       :for #{:ordered}}
   {:op ">"       :for #{:ordered}}
   {:op "<="      :for #{:ordered}}
   {:op ">="      :for #{:ordered}}
   {:op "matches" :for #{:text}}
   {:op "in"      :for #{:text :ordered}}])

(defn- crit-side [src param prop]
  (case src
    "prop" (when (and (seq param) (seq prop)) [:get (keyword param) (keyword prop)])
    "user" [:get :regel-benutzer]
    (when (seq param) [:get (keyword param)])))

(defn- binder
  [test]
  (fn [pref expr]
    (loop [n 1]
      (let [k (if (= 1 n) pref (keyword (str (name pref) n)))
            v (get @test k :sutra-ui.state/none)]
        (cond (= :sutra-ui.state/none v)  (do (swap! test assoc k expr) k)
              (= v expr)    k
              :else         (recur (inc n)))))))

(defn- crit-cond-pred
  [bind! {:keys [left-src param prop op right-src value value-orig rparam rprop lname rname]}]
  (when-let [l (crit-side left-src param prop)]
    (let [lk  (bind! (or lname :l) l)
          r   (when (#{"param" "prop"} right-src) (crit-side right-src rparam rprop))
          rk  (when r (bind! (or rname :r) r))
          opk (keyword (or (not-empty op) "="))]
      (cond
        (and (#{"param" "prop"} right-src) (nil? r)) nil
        rk                    [opk lk rk]
        (= "none" right-src)  [opk lk nil]
        (= :in opk)           (into [:in lk]
                                    (if (and (vector? value-orig)
                                             (= value (str/join ", " (map str value-orig))))
                                      value-orig
                                      (map ->crit-value (split-list value))))
        :else                 [opk lk (if (and (some? value-orig) (not (coll? value-orig))
                                               (= value (str value-orig)))
                                        value-orig
                                        (->crit-value value))]))))

(defn- crit-node-pred [bind! node]
  (if-let [g (:group node)]
    (let [ps (vec (keep #(crit-node-pred bind! %) (:children node)))]
      (when (seq ps)
        (case g
          "any"  (into [:or] ps)
          "none" (if (and (= 1 (count ps)) (not (:or-wrap node)))
                   [:not (first ps)]
                   [:not (into [:or] ps)])
          (into [:and] ps))))
    (crit-cond-pred bind! node)))

(defn- ->criterion [c]
  (if (:raw c)
    (:raw c)
    (let [test (atom {})
          pred (crit-node-pred (binder test) c)]
      (when pred
        (cond-> {:test @test :pred pred}
          (seq (:message c)) (assoc :message (:message c)))))))

(defn- get-refs [x]
  (->> (tree-seq coll? seq x)
       (filter #(and (vector? %) (= :get (first %)) (> (count %) 1)))
       (mapcat (fn [[_ a b]] (if (= :arg0 a) [b] [a])))
       (filter keyword?)
       set))

(defn- insert-at [v [i x]]
  (let [i (min i (count v))]
    (vec (concat (subvec v 0 i) [x] (subvec v i)))))

(defn- ->hide-value [s orig]
  (cond (= s (str orig))                  orig
        (= "true" s)                      true
        (= "false" s)                     false
        (re-matches #"-?\d+(\.\d+)?" s) (js/parseFloat s)
        :else                             s))

(defn- form->overrides [{:keys [hide-raw hide-param hide-op hide-value hide-orig]}]
  (cond
    hide-raw hide-raw
    (and (seq hide-param) (seq hide-op))
    (let [p (keyword hide-param)]
      [{:exposure :hidden
        :when (case hide-op
                "set"   [:!= p nil]
                "empty" [:= p nil]
                [(keyword hide-op) p (->hide-value (str hide-value) hide-orig)])}])))

(defn- ->form-section
  [declared {:keys [title description columns collapsible collapsed params] :as sec}]
  (let [ps  (into [] (comp (filter declared) (distinct) (map keyword)) params)
        ovr (form->overrides sec)]
    (when (or (seq ps) (seq (str/trim (str title))))
      (cond-> {:params ps}
        (seq ovr)                          (assoc :overrides ovr)
        (seq (str/trim (str title)))       (assoc :title (str/trim (str title)))
        (seq (str/trim (str description))) (assoc :description (str description))
        (= 2 columns)                      (assoc :columns 2)
        (and collapsible (not collapsed))  (assoc :collapsible true)
        collapsed                          (assoc :collapsed true)))))

(defn ->action-form [{:keys [sections] :as form}]
  (let [declared (into #{} (keep (fn [{:keys [pname type]}]
                                   (when (and (seq pname) (seq type)) pname)))
                       (:params form))
        secs     (into [] (keep #(->form-section declared %)) sections)]
    (when (seq secs) {:sections secs})))

(defn ->action-spec
  [{:keys [name params rules submission-criteria implicit log-rules display-name description]
    :as form}]
  (let [live-rules (remove #(str/blank? (str (:op %))) rules)
        rule-maps  (reduce insert-at (mapv #(->action-rule form %) live-rules) log-rules)
        crits      (vec (keep ->criterion submission-criteria))
        frm        (->action-form form)
        refs       (get-refs [rule-maps crits])
        ps         (merge (into {} (filter (comp refs key)) implicit)
                          (into {} (for [{:keys [pname type] :as p} params
                                         :when (and (seq pname) (seq type))]
                                     [(keyword pname) (->param-spec p)]))
                          (context-params form))]
    (cond-> (merge (:extra form) {:name (keyword (str/trim (str name)))})
      (seq (str/trim (str display-name))) (assoc :display-name (str/trim (str display-name)))
      (seq (str/trim (str description)))  (assoc :description (str description))
      (seq ps)        (assoc :params ps)
      (seq rule-maps) (assoc :rules rule-maps
                             :edits (derive-action-edits live-rules))
      (seq crits)     (assoc :submission-criteria crits)
      frm             (assoc :form frm))))

(defn add-action! [form] (send! [:ontology/mutate :create-action (->action-spec form)]))

(defn update-action! [form] (send! [:ontology/mutate :update-action (->action-spec form)]))

(defn- get-expr? [v n]
  (and (vector? v) (= n (count v)) (= :get (keyword (first v)))))

(defn- classify-value
  [params v]
  (let [param? #(contains? params (keyword %))]
    (cond
      (= [:now] v)                   {:src "now"}
      (= [:current-user] v)          {:src "user"}
      (and (get-expr? v 3) (= :arg0 (keyword (second v))) (param? (nth v 2)))
      {:src "param" :param (name (nth v 2))}
      (and (get-expr? v 2) (csub/implicit-context-param? (second v)))
      {:src (if (= :current-user
                   (get-in csub/implicit-context-params [(keyword (second v)) :source]))
              "user" "now")}
      (and (get-expr? v 2) (param? (second v)))
      {:src "param" :param (name (second v))}
      (and (get-expr? v 3) (param? (second v)))
      {:src "prop" :param (name (second v)) :oprop (name (nth v 2))}
      (coll? v)                      {:src "expr" :raw v}
      :else                          {:src "static" :value (str v)})))

(defn- rule-handled-keys [m]
  (cond-> (into #{:op} (map first) (concat (:binds m) (:picks m) (:free m) (:texts m) (:bools m)))
    (:type-ref m) (conj (get-in m [:type-ref :field]))))

(defn- webhook-row [params k v]
  (let [c (classify-value params v)]
    (merge {:prop (name k) :orig v}
           (if (#{"param" "prop" "static"} (:src c)) c {:src "expr" :raw v}))))

(defn- action-rule->form [params r]
  (let [m    (action-op-meta (:op r))
        tref (:type-ref m)
        tref-rid (when tref (get r (:field tref)))
        vmap?    (and (:value-map? m) (map? (:values r)))
        wmap?    (and (:webhook-params? m) (map? (:params r)))]
    (cond-> {:op (name (:op r))
             :extra (cond-> (apply dissoc r (rule-handled-keys m))
                      wmap? (dissoc :params))}
      (string? tref-rid) (assoc :type (some-> tref-rid catalog/name-of name))
      true (assoc :bind (into {} (for [[f _] (:binds m) :let [v (get r f)]
                                       :when (and v (not (and vmap? (= :values f))))]
                                   [f (bind-param-name v)])))
      true (assoc :bind-raw (into {} (for [[f _] (:binds m) :let [v (get r f)] :when (vector? v)]
                                       [f v])))
      true (assoc :pick (into {} (for [[f _ _] (:picks m) :let [v (get r f)] :when v]
                                   [f (name v)])))
      true (assoc :free (into {} (for [[f _] (:free m) :let [v (get r f)] :when v]
                                   [f (name v)])))
      true (assoc :text (into {} (for [[f _] (:texts m) :let [v (get r f)] :when v]
                                   [f (str v)])))
      true (assoc :bool (into {} (for [[f _] (:bools m) :let [v (get r f)] :when v]
                                   [f (boolean v)])))
      vmap? (assoc :vals (vec (for [[k v] (:values r)]
                                (merge {:prop (name k) :orig v} (classify-value params v)))))
      wmap? (assoc :wvals (vec (for [[k v] (:params r)] (webhook-row params k v)))))))

(defn- crit-side-form [e]
  (cond
    (and (get-expr? e 3) (= :arg0 (keyword (second e)))) {:src "param" :param (name (nth e 2))}
    (and (get-expr? e 2) (= :regel-benutzer (keyword (second e)))) {:src "user"}
    (get-expr? e 2) {:src "param" :param (name (second e))}
    (get-expr? e 3) {:src "prop" :param (name (second e)) :prop (name (nth e 2))}))

(def ^:private cmp-ops
  #{:= :not= :!= :< :> :<= :>= :matches :includes :includes-any :each= :each!= :approx=})

(defn- parse-crit-node
  [test used p]
  (let [side (fn [x] (cond
                       (keyword? x) (when (contains? test x)
                                      (swap! used conj x)
                                      (crit-side-form (get test x)))
                       (vector? x)  (crit-side-form x)))
        group (fn [g wrap xs]
                (let [cs (mapv #(parse-crit-node test used %) xs)]
                  (when (and (seq cs) (every? some? cs))
                    (cond-> {:group g :children cs} wrap (assoc :or-wrap true)))))]
    (when (vector? p)
      (let [[op & args] p]
        (case op
          :and (group "all" false args)
          :or  (group "any" false args)
          :not (when (= 1 (count args))
                 (let [x (first args)]
                   (if (and (vector? x) (= :or (first x)))
                     (group "none" true (rest x))
                     (group "none" false [x]))))
          :in  (let [[a & vs] args
                     l (side a)]
                 (when (and l (keyword? a) (seq vs) (not-any? coll? vs) (not-any? keyword? vs))
                   {:left-src (:src l) :param (or (:param l) "") :prop (or (:prop l) "")
                    :lname a :op "in" :right-src "static"
                    :value (str/join ", " (map str vs)) :value-orig (vec vs)}))
          (when (and (contains? cmp-ops op) (= 2 (count args)))
            (let [[a b] args
                  l (side a)
                  r (when (or (keyword? b) (vector? b)) (side b))]
              (when (and l (or r (not (or (keyword? b) (vector? b)))))
                (cond-> {:left-src (:src l) :param (or (:param l) "") :prop (or (:prop l) "")
                         :op (name op)}
                  (keyword? a)   (assoc :lname a)
                  (keyword? b)   (assoc :rname b)
                  r              (assoc :right-src (:src r) :rparam (:param r) :rprop
                                        (or (:prop r) ""))
                  (nil? b)       (assoc :right-src "none")
                  (and (nil? r)
                       (some? b)) (assoc :right-src "static" :value (str b) :value-orig b))))))))))

(defn- action-crit->form [c]
  (let [{:keys [test pred message]} c
        used (atom #{})
        node (when (map? c) (parse-crit-node (or test {}) used pred))]
    (if (or (nil? node)
            (seq (dissoc c :test :pred :message))
            (not= (set (keys test)) @used))
      {:raw c}
      (assoc node :message (or message "")))))

(defn- param->form [[p v]]
  (let [d       (:default v)
        cons    (:constraints v)
        allowed (:one-of cons)
        rest-c  (not-empty (dissoc cons :one-of))]
    (cond-> {:pname (name p) :type (some-> (:type v) name)
             :label (or (:name v) "") :required (true? (:required v))
             :hidden (= :hidden (some-> (:exposure v) keyword))
             :prefill (some-> (:prefill v) name)
             :extra (cond-> (apply dissoc v param-form-keys)
                      (not= :hidden
                            (some-> (:exposure v) keyword)) (merge (select-keys v [:exposure])))}
      rest-c                         (assoc :constraints rest-c)
      (sequential? allowed)          (assoc :one-of (str/join ", " (map str allowed)) :one-of-orig
                                            (vec allowed))
      (and (some? allowed) (not (sequential? allowed))) (assoc-in [:constraints :one-of] allowed)
      (map? d)                       (assoc :default-src "prop"
                                            :default-from (some-> (:from d) name)
                                            :default-prop (some-> (:property d) name))
      (and (some? d)
           (not (map? d))) (assoc :default-src "static" :default (str d) :default-orig d))))

(defn- log-rule?
  [r]
  (and (= :create (keyword (:op r)))
       (= [:get :log-id] (get-in r [:values :log-id]))))

(def ^:private log-params #{:log-id :log-at :log-by})

(defn hidden-action-param?
  [spec p]
  (or (csub/implicit-param? p)
      (and (contains? log-params (keyword p)) (some log-rule? (:rules spec)))))

(defn- hide-when->form [overrides]
  (let [[o & more] overrides
        [op p v :as w] (:when o)
        op (some-> op name)]
    (cond
      (empty? overrides) {}
      (and (nil? more) (= :hidden (some-> (:exposure o) name keyword))
           (= 3 (count w)) (#{"=" "!="} op) (or (keyword? p) (string? p))
           (not (coll? v)))
      (let [p (name p)]
        (cond (and (nil? v) (= "!=" op)) {:hide-param p :hide-op "set"}
              (nil? v)                   {:hide-param p :hide-op "empty"}
              :else {:hide-param p :hide-op op :hide-value (str v) :hide-orig v}))
      :else {:hide-raw (vec overrides)})))

(defn- section->form [sec]
  (merge
   (hide-when->form (:overrides sec))
   {:title       (str (or (:title sec) ""))
    :description (str (or (:description sec) ""))
    :columns     (or (:columns sec) 1)
    :collapsible (true? (or (:collapsible sec) (:collapsed sec)))
    :collapsed   (true? (:collapsed sec))
    :params      (mapv name (:params sec))}))

(defn action->form
  [name-kw spec]
  (let [rules  (vec (:rules spec))
        logs   (vec (keep-indexed (fn [i r] (when (log-rule? r) [i r])) rules))
        hide?  (fn [[p _]] (or (csub/implicit-param? p)
                               (and (seq logs) (contains? log-params (keyword p)))))
        pnames (set (map keyword (keys (:params spec))))]
    {:editing-name name-kw
     :name (name name-kw)
     :display-name (str (or (:display-name spec) ""))
     :description (str (or (:description spec) ""))
     :extra (dissoc spec :id :name :params :rules :submission-criteria :edits :display-name
                    :description :form)
     :sections (mapv section->form (get-in spec [:form :sections]))
     :params (vec (for [e (:params spec) :when (not (hide? e))] (param->form e)))
     :implicit (into {} (filter hide?) (:params spec))
     :rules (vec (for [r rules :when (not (log-rule? r))] (action-rule->form pnames r)))
     :log-rules logs
     :submission-criteria (mapv action-crit->form (:submission-criteria spec))}))

(defn param-type-of-base
  [base]
  (cond (contains? numeric-bases base)          "number"
        (#{"boolean" "date" "timestamp"} base)  base
        :else                                   "string"))

(defn ensure-param
  ([form pname type] (ensure-param form pname type {}))
  ([form pname type more]
   (loop [n 1]
     (let [p (if (= 1 n) pname (str pname "-" n))
           existing (some #(when (= p (:pname %)) %) (:params form))]
       (cond
         (nil? existing)            [(update form :params (fnil conj [])
                                             (merge {:pname p :type type :label "" :prefill ""}
                                                    more))
                                     p]
         (= type (:type existing))  [form p]
         :else                      (recur (inc n)))))))

(defn set-value-prop
  [form i j prop]
  (let [form (assoc-in form [:rules i :vals j :prop] prop)
        row  (get-in form [:rules i :vals j])]
    (if (and (seq prop) (= "param" (or (:src row) "param")) (str/blank? (:param row)))
      (let [ot     (rule-object-type form (get-in form [:rules i]))
            [f p]  (ensure-param form prop (param-type-of-base (prop-base ot prop)))]
        (assoc-in f [:rules i :vals j :param] p))
      form)))

(defn new-param-for-value
  [form i j]
  (let [row   (get-in form [:rules i :vals j])
        ot    (rule-object-type form (get-in form [:rules i]))
        prop  (:prop row)
        [f p] (ensure-param form (if (seq prop) prop "wert")
                            (param-type-of-base (prop-base ot prop)))]
    (assoc-in f [:rules i :vals j] (assoc row :src "param" :param p))))

(defn new-param-for-webhook-input
  [form i j]
  (let [row   (get-in form [:rules i :wvals j])
        [f p] (ensure-param form (:prop row) "string")]
    (assoc-in f [:rules i :wvals j] (assoc row :src "param" :param p))))

(defn- pk-attribute
  [ot]
  (let [spec  (get-in @catalog/canonical-schema [:object-types (keyword ot)])
        props (set (map first (object-type-props ot)))
        mk    (some-> (first (:match-keys spec)) name)]
    (cond (and mk (contains? props mk)) mk
          (contains? props (str ot "-id")) (str ot "-id")
          :else nil)))

(defn set-rule-type
  [form i ot]
  (let [form (assoc-in form [:rules i :type] ot)
        op   (keyword (get-in form [:rules i :op]))]
    (cond
      (str/blank? ot) form

      (#{:update :delete-object} op)
      (let [cur (get-in form [:rules i :bind :target])]
        (if (and (seq cur) (= ot (form-param-type form cur)))
          form
          (let [existing (first (object-param-names form ot))
                [f p]    (if existing
                           [form existing]
                           (ensure-param form ot ot {:required true :prefill "current-object"}))]
            (assoc-in f [:rules i :bind :target] p))))

      (= :create op)
      (if (seq (get-in form [:rules i :bind :id]))
        form
        (let [pk    (pk-attribute ot)
              [f p] (ensure-param form (or pk (str ot "-id")) "string"
                                  {:hidden true :prefill "uuid"})
              f     (assoc-in f [:rules i :bind :id] p)]
          (if (and pk (not-any? #(= pk (:prop %)) (get-in f [:rules i :vals])))
            (update-in f [:rules i :vals] (fnil conj []) {:prop pk :src "param" :param p})
            f)))

      :else form)))

(defn api-name-of
  [s]
  (-> (str/lower-case (str s))
      (str/replace #"ä" "ae") (str/replace #"ö" "oe") (str/replace #"ü" "ue")
      (str/replace #"ß" "ss")
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+|-+$" "")))

(defn wizard->form
  [{:keys [display-name kind object-type props]}]
  (let [base {:editing-name nil :name (api-name-of display-name) :display-name (str display-name)
              :description "" :params [] :rules [] :submission-criteria []}
        op   (case kind "create" "create" "update" "update" "delete" "delete-object" nil)]
    (if (or (nil? op) (str/blank? object-type))
      base
      (let [f (-> base
                  (assoc :rules
                         [{:op op :type "" :bind {} :pick {} :free {} :text {} :bool {} :vals []}])
                  (set-rule-type 0 object-type))]
        (if (= "delete-object" op)
          f
          (reduce (fn [f prop]
                    (if (some #(= prop (:prop %)) (get-in f [:rules 0 :vals]))
                      f
                      (let [j (count (get-in f [:rules 0 :vals]))]
                        (-> f
                            (update-in [:rules 0 :vals] (fnil conj [])
                                       {:prop "" :src "param" :param ""})
                            (set-value-prop 0 j prop)))))
                  f props))))))

(defn rename-param
  [form idx new-name]
  (let [old (get-in form [:params idx :pname])
        f   (assoc-in form [:params idx :pname] new-name)]
    (if (or (str/blank? old) (> (count (filter #(= old (:pname %)) (:params form))) 1))
      f
      (let [ren  #(if (= old %) new-name %)
            crit (fn crit [n] (if (:group n)
                                (update n :children #(mapv crit %))
                                (-> n (update :param ren) (update :rparam ren))))]
        (-> f
            (update :params (fn [ps] (mapv #(update % :default-from ren) ps)))
            (update :sections (fn [ss]
                                (mapv (fn [sec]
                                        (cond-> (update sec :params #(mapv ren %))
                                          (:hide-param sec) (update :hide-param ren)))
                                      ss)))
            (update :rules (fn [rs]
                             (mapv (fn [r]
                                     (-> r
                                         (update :bind #(update-vals (or % {}) ren))
                                         (update :vals #(mapv (fn [v] (update v :param ren)) %))
                                         (update :wvals #(mapv (fn [v] (update v :param ren)) %))))
                                   rs)))
            (update :submission-criteria #(mapv crit %)))))))

(defn- crit-nodes [roots]
  (mapcat #(tree-seq :group :children %) roots))

(defn action-form-problems
  [form]
  (let [declared (set (keep #(not-empty (:pname %)) (:params form)))
        known    (into declared (map name (keys (:implicit form))))
        problem  (fn [level k args] {:level level :key k :args args})
        rule-n   #(inc %)]
    (vec
     (concat
      (when (str/blank? (:name form))
        [(problem :error :ui/prob-no-name {})])
      (when (and (nil? (:editing-name form)) (seq (:name form))
                 (contains? (:actions @catalog/canonical-schema) (keyword (str/trim (:name form)))))
        [(problem :error :ui/prob-name-taken {:name (:name form)})])
      (for [[i p] (map-indexed vector (:params form))
            :when (or (str/blank? (:pname p)) (str/blank? (:type p)))]
        (problem :error :ui/prob-param-incomplete {:n (inc i)}))
      (for [[n c] (frequencies (keep #(not-empty (:pname %)) (:params form))) :when (> c 1)]
        (problem :error :ui/prob-param-dup {:name n}))
      (let [ops (keep #(not-empty (str (:op %))) (:rules form))]
        (when (and (some #{"apply-edit-function"} ops) (> (count ops) 1))
          [(problem :error :ui/rule-function-exclusive {})]))
      (mapcat
       (fn [[i r]]
         (let [m  (action-op-meta (:op r))
               op (keyword (:op r))
               n  (rule-n i)]
           (concat
            (when (str/blank? (str (:op r)))
              [(problem :error :ui/prob-rule-no-op {:n n})])
            (when (and (#{:create :delete-object :create-link :delete-link} op)
                       (str/blank? (:type r))
                       (not (get-in r [:extra (get-in m [:type-ref :field])])))
              [(problem :error :ui/prob-rule-type {:n n})])
            (when (and (#{:update :delete-object} op) (str/blank? (get-in r [:bind :target])))
              [(problem :error :ui/prob-rule-target {:n n})])
            (when (and (= :update op) (seq (:type r)) (seq (get-in r [:bind :target]))
                       (contains? declared (get-in r [:bind :target]))
                       (not= (:type r) (form-param-type form (get-in r [:bind :target]))))
              [(problem :error :ui/prob-rule-target-type {:n n :type (:type r)})])
            (for [v (:vals r)
                  :when (or (str/blank? (:prop v))
                            (and (contains? #{"param" "prop" nil} (:src v)) (str/blank? (:param v)))
                            (and (= "prop" (:src v)) (str/blank? (:oprop v))))]
              (problem :error :ui/prob-value-incomplete
                       {:n n :prop (or (not-empty (:prop v)) "?")}))
            (for [[prop c] (frequencies (keep #(not-empty (:prop %)) (:vals r))) :when (> c 1)]
              (problem :error :ui/prob-value-dup {:n n :prop prop}))
            (when (:webhook-params? m)
              (let [mapped (set (keep #(when (or (not (contains? #{"param" "prop" nil} (:src %)))
                                                 (seq (:param %)))
                                         (:prop %))
                                      (:wvals r)))
                    missing (remove mapped
                                    (webhook-inputs
                                     (get-in r [:pick :source]) (get-in r [:free :webhook])))]
                (when (seq missing)
                  [(problem :warn :ui/prob-webhook-unmapped
                            {:n n :names (str/join ", " missing)})]))))))
       (map-indexed vector (:rules form)))
      (for [[i c] (map-indexed vector (:submission-criteria form))
            :when (not (:raw c))
            node (crit-nodes [c])
            :when (if (:group node)
                    (empty? (:children node))
                    (or (nil? (crit-side (:left-src node) (:param node) (:prop node)))
                        (and (#{"param" "prop"} (:right-src node))
                             (nil? (crit-side (:right-src node) (:rparam node) (:rprop node))))))]
        (problem :error (if (:group node) :ui/prob-group-empty :ui/prob-crit-incomplete)
                 {:n (inc i)}))
      (let [row-ref (fn [v] (when (contains? #{"param" "prop" nil} (:src v)) (:param v)))
            refs (concat (mapcat (fn [r] (concat (vals (:bind r))
                                                 (keep row-ref (:vals r))
                                                 (keep row-ref (:wvals r))))
                                 (:rules form))
                         (keep :default-from (:params form))
                         (mapcat (fn [n] [(when-not (= "user" (:left-src n)) (:param n))
                                          (when (contains? #{"param" "prop"} (:right-src n))
                                            (:rparam n))])
                                 (crit-nodes (remove :raw (:submission-criteria form)))))]
        (for [p (distinct (remove str/blank? (map str (remove nil? refs))))
              :when (not (contains? known p))]
          (problem :error :ui/prob-unknown-param {:name p})))))))

(defn open-application! [app-kw] (send! [:ontology/open-application app-kw]))

(defn om-recents [] (get-in @kompass/om-discover [(kompass/om-key) :recents] []))
