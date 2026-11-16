(ns sutra.catalog.program
  (:require [sutra.types.entities :as ents]
            [sutra.kit.refusal :as refusal]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.link-side :as ls]
            [sutra.types.object-set :as os]
            [sutra.types.llm-output :as llm-out]
            [clojure.edn]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(declare compile-filter compile-step symbol->binding)

(defn- make-ctx [schema]
  {:schema schema
   :name->rid (sch/name-index schema)
   :bind-counter 0 :query-counter 0
   :bindings {} :bind-types {} :object-sets {} :steps []})

(defn- ->type [ctx name-kw]
  (or (rid/resolve-name->rid (:name->rid ctx) ents/object-targets name-kw) name-kw))
(defn- ->conn [ctx name-kw]
  (or (rid/resolve-name->rid (:name->rid ctx) #{:link-types} name-kw) name-kw))

(defn- next-name [ctx prefix]
  [(keyword (str prefix (inc (:bind-counter ctx))))
   (update ctx :bind-counter inc)])

(defn- next-binding [ctx] (next-name ctx "_b"))

(defn- next-query-name [ctx]
  [(keyword (str "_wf_q" (inc (:query-counter ctx))))
   (update ctx :query-counter inc)])

(defn- inherit-from
  [dst src]
  (-> dst
      (assoc :bind-counter (:bind-counter src)
             :query-counter (:query-counter src))
      (update :object-sets merge (:object-sets src))))

(defn- register-binding
  ([ctx name-kw internal-kw]
   (update ctx :bindings assoc name-kw internal-kw))
  ([ctx name-kw internal-kw obj-type]
   (-> ctx
       (update :bindings assoc name-kw internal-kw)
       (update :bind-types assoc name-kw obj-type))))

(defn- binding-type [ctx name-kw] (get (:bind-types ctx) name-kw))

(defn- resolve-name [ctx name-kw]
  (or (get (:bindings ctx) name-kw)
      (refusal/refuse! :refusal.catalog/unknown-binding
                       {:binding name-kw :known (keys (:bindings ctx))})))

(defn- add-object-set [ctx qt-name qt-def]
  (update ctx :object-sets assoc qt-name qt-def))

(defn- add-step [ctx step]
  (update ctx :steps conj step))

(defn- resolve-value [ctx v]
  (cond
    (and (vector? v) (= :ref (first v)))
    (if (= 3 (count v))
      [:get (resolve-name ctx (second v)) (nth v 2)]
      (resolve-name ctx (second v)))
    (and (vector? v) (= :pluck (first v)))    [:pluck (resolve-name ctx (second v)) (nth v 2)]
    (and (vector? v) (= :pluck-id (first v))) [:pluck-id (resolve-name ctx (second v))]
    (and (keyword? v) (contains? (:bindings ctx) v)) (resolve-name ctx v)
    (map? v)                                  (update-vals v #(resolve-value ctx %))
    (and (vector? v) (seq v))                 (mapv #(resolve-value ctx %) v)
    :else v))

(defn- resolve-values-map [ctx m]
  (update-vals (or m {}) #(resolve-value ctx %)))

(defn- parse-order-by [order-by]
  (when order-by
    (if (keyword? order-by)
      {:field order-by}
      {:field (:field order-by) :sort-dir (:dir order-by :asc)})))

(defn- compile-eq-filter [ctx root-type filter-spec]
  (let [path (or (:path filter-spec) (:field filter-spec))
        value (:value filter-spec)
        path-str (name path)
        dot-idx (.indexOf path-str ".")]
    (if (pos? dot-idx)
      (let [rel-name (keyword (subs path-str 0 dot-idx))
            attr (keyword (subs path-str (inc dot-idx)))
            resolved-val (resolve-value ctx value)
            [param-name ctx] (next-name ctx "_cm")
            side (ls/resolve-side (:schema ctx) root-type rel-name)]
        [ctx {:hops [(os/match-hop side [:= attr [:param param-name]])]
              :param-bindings {param-name resolved-val}
              :param-slots [param-name]}])
      (let [resolved-val (resolve-value ctx value)]
        (if (or (keyword? resolved-val) (vector? resolved-val))
          (let [[param-name ctx] (next-name ctx "_p")]
            [ctx {:where [:= path [:param param-name]]
                  :param-bindings {param-name resolved-val}
                  :param-slots [param-name]}])
          [ctx {:where [:= path resolved-val]}])))))

(defn- compile-each [ctx f xs]
  (reduce (fn [[ctx acc] x]
            (let [[ctx s] (f ctx x)]
              [ctx (conj acc s)]))
          [ctx []]
          xs))

(defn- merge-subs [subs]
  (let [includes     (into [] (mapcat :include) subs)
        hops         (into [] (mapcat :hops) subs)
        param-binds  (apply merge (map :param-bindings subs))
        param-slots  (into [] (mapcat :param-slots) subs)
        wheres       (keep :where subs)]
    (cond-> {}
      (seq includes)        (assoc :include includes)
      (seq hops)            (assoc :hops hops)
      (seq param-binds)     (assoc :param-bindings param-binds)
      (seq param-slots)     (assoc :param-slots param-slots)
      (= 1 (count wheres))  (assoc :where (first wheres))
      (> (count wheres) 1)  (assoc :where (into [:and] wheres)))))

(defn- compile-one-of-filter [ctx root-type {:keys [path values]}]
  (when (str/includes? (name path) ".")
    (refusal/refuse! :refusal.catalog/one-of-over-link {:path path} {:property (name path)}))
  (let [[ctx subs] (compile-each ctx #(compile-eq-filter %1 root-type {:path path :value %2})
                                 values)
        wheres     (keep :where subs)]
    [ctx (cond-> (dissoc (merge-subs subs) :where)
           (seq wheres) (assoc :where (into [:or] wheres))
           (empty? values) (assoc :where [:id-in []]))]))

(defn- compile-having-all-filter [ctx root-type filter-spec]
  (let [{:keys [relation values-from]} filter-spec
        as-name (keyword (str "_inc_" (name relation)))
        [param-name ctx] (next-name ctx "_hv")
        {conn-name :link dir :dir} (ls/resolve-side (:schema ctx) root-type relation)]
    [ctx {:include [{:link conn-name :dir dir :as as-name}]
          :where [:having-all as-name [:param param-name]]
          :param-bindings {param-name (if values-from
                                        (resolve-name ctx values-from)
                                        (resolve-value ctx (:values filter-spec)))}
          :param-slots [param-name]}]))

(defn- compile-connected-to-filter [ctx root-type filter-spec]
  (let [path (or (:path filter-spec) (:relation filter-spec))
        ids-from (:ids-from filter-spec)
        [param-name ctx] (next-name ctx "_cw")]
    [ctx {:hops [(os/ids-hop (ls/resolve-side (:schema ctx) root-type path) param-name)]
          :param-bindings {param-name (if ids-from
                                        [:pluck-id (resolve-name ctx ids-from)]
                                        (resolve-value ctx (:ids filter-spec)))}
          :param-slots [param-name]}]))

(def ^:private comparison-op-kw
  {:lt :< :gt :> :lte :<= :gte :>=})

(defn- compile-comparison-filter [ctx _root-type filter-spec]
  (let [op    (some (set (keys comparison-op-kw)) (keys filter-spec))
        spec  (op filter-spec)
        op-kw (comparison-op-kw op)]
    [ctx
     (if (:ref spec)
       (let [ref-val (resolve-value ctx (:ref spec))]
         {:where [op-kw (if (keyword? ref-val) ref-val (second ref-val)) (:value spec)]})
       {:where [op-kw (:field spec) (:value spec)]})]))

(def ^:private filter-compilers
  {:eq           (fn [ctx rt spec] (compile-eq-filter           ctx rt (:eq spec)))
   :one-of       (fn [ctx rt spec] (compile-one-of-filter       ctx rt (:one-of spec)))
   :having-all   (fn [ctx rt spec] (compile-having-all-filter   ctx rt (:having-all spec)))
   :connected-to (fn [ctx rt spec] (compile-connected-to-filter ctx rt (:connected-to spec)))
   :lt           compile-comparison-filter
   :gt           compile-comparison-filter
   :lte          compile-comparison-filter
   :gte          compile-comparison-filter})

(def ^:private filter-tags (set (keys filter-compilers)))

(defn- compile-filter [ctx root-type filter-spec]
  (cond
    (and (vector? filter-spec) (= :and (first filter-spec)))
    (let [[ctx subs] (compile-each ctx #(compile-filter %1 root-type %2) (rest filter-spec))]
      [ctx (merge-subs subs)])

    (map? filter-spec)
    (if-let [f (some filter-compilers (keys filter-spec))]
      (f ctx root-type filter-spec)
      (refusal/refuse! :refusal.catalog/unknown-filter-type {:filter filter-spec}))

    (and (vector? filter-spec) (filter-tags (first filter-spec)))
    (let [[op & args] filter-spec]
      (compile-filter ctx root-type
                      {op (if (= 2 (count args)) {:path (first args) :value (second args)}
                              (first args))}))

    :else (refusal/refuse! :refusal.catalog/unknown-filter-format {:filter filter-spec})))

(defn- compile-query-step
  [ctx {:keys [qt-name qt-def as-name target-type take-mode step-params]}]
  (let [[coll-bk ctx] (next-binding ctx)
        query-step {:op :query :object-set qt-name :arguments (or step-params {}) :as coll-bk}
        ctx (-> ctx (add-object-set qt-name qt-def) (add-step query-step))]
    (if (= take-mode :first)
      (let [[first-bk ctx] (next-binding ctx)]
        (-> ctx
            (add-step {:op :compute :from coll-bk :expr [:first] :as first-bk})
            (register-binding as-name first-bk target-type)))
      (register-binding ctx as-name coll-bk target-type))))

(defn- compile-traverse [ctx step]
  (let [{:keys [from relation order-by limit as]} step
        from-bk   (resolve-name ctx from)
        from-type (binding-type ctx from)
        schema    (:schema ctx)
        _ (when-not from-type
            (refusal/refuse! :refusal.catalog/traverse-untyped {:binding from :relation relation}))
        {ct-name :link dir :dir tgt-type :far-type} (ls/resolve-side schema from-type relation)
        [ctx compiled] (if (:filter step)
                         (compile-filter ctx tgt-type (:filter step))
                         [ctx nil])
        [qt-name ctx] (next-query-name ctx)
        param-name (keyword (str (name from) "-id"))
        hop       {:op :search-around :link ct-name :dir dir
                   :in {:op :static :ids [:param param-name]}}
        parsed-ob (parse-order-by order-by)
        qt-def    (os/qt->ir (cond-> {:root tgt-type
                                      :param-slots (into [param-name] (:param-slots compiled))}
                               (:where compiled)     (assoc :where (:where compiled))
                               (:include compiled)   (assoc :include (:include compiled))
                               parsed-ob             (assoc :order-by (:field parsed-ob))
                               (:sort-dir parsed-ob) (assoc :sort-dir (:sort-dir parsed-ob))
                               limit                 (assoc :limit limit))
                             #(os/intersect-hops % (into [hop] (:hops compiled))))]
    (compile-query-step ctx {:qt-name qt-name :qt-def qt-def :as-name as
                             :target-type tgt-type :take-mode (:take step)
                             :step-params (merge {param-name [:get from-bk :id]}
                                                 (:param-bindings compiled))})))

(defn- compile-search [ctx step]
  (let [{:keys [type order-by limit as]} step
        t (->type ctx type)
        _ (when-not (or (contains? (:object-types (:schema ctx)) t)
                        (contains? (:interfaces (:schema ctx)) t))
            (refusal/refuse! :refusal.catalog/search-unknown-type
                             {:type type
                              :known (vec (sort (keys (:object-types (:schema ctx)))))
                              :step step}))
        [ctx compiled] (if (:filter step)
                         (compile-filter ctx t (:filter step))
                         [ctx nil])
        parsed-ob   (parse-order-by order-by)
        [qt-name ctx] (next-query-name ctx)
        qt-def      (os/qt->ir (cond-> {:root t :param-slots (vec (:param-slots compiled))}
                                 (:where compiled)     (assoc :where (:where compiled))
                                 (:include compiled)   (assoc :include (:include compiled))
                                 parsed-ob             (assoc :order-by (:field parsed-ob))
                                 (:sort-dir parsed-ob) (assoc :sort-dir (:sort-dir parsed-ob))
                                 limit                 (assoc :limit limit))
                               #(os/intersect-hops % (:hops compiled)))]
    (compile-query-step ctx {:qt-name qt-name :qt-def qt-def :as-name as
                             :target-type t :take-mode (:take step)
                             :step-params (into {} (:param-bindings compiled))})))

(defn- compile-for-each [ctx step]
  (let [{:keys [over as steps]} step
        coll-bk   (resolve-name ctx over)
        coll-type (binding-type ctx over)
        child-ctx (-> ctx
                      (assoc :steps [])
                      (register-binding as as coll-type))
        child-ctx (reduce compile-step child-ctx steps)]
    (-> ctx
        (inherit-from child-ctx)
        (add-step {:op :for-each :collection coll-bk :as as :steps (:steps child-ctx)}))))

(defn- compile-create [ctx step]
  (let [{:keys [type values as]} step
        t (->type ctx type)
        [bk ctx] (next-binding ctx)]
    (-> ctx
        (add-step {:op :create :object-type t :values (resolve-values-map ctx values) :as bk})
        (register-binding as bk t))))

(defn- link-endpoint
  [ctx ref]
  (if (keyword? ref)
    [:get (resolve-name ctx ref) :id]
    (resolve-value ctx ref)))

(defn- compile-create-link [ctx step]
  (add-step ctx {:op :create-link
                 :link-type (->conn ctx (:link-type step))
                 :from-id   (link-endpoint ctx (:from step))
                 :to-id     (link-endpoint ctx (:to step))}))

(defn- compile-update [ctx step]
  (add-step ctx {:op :update
                 :target (resolve-name ctx (:target step))
                 :values (resolve-values-map ctx (:values step))}))

(defn- compile-delete-object [ctx step]
  (let [target (:target step)]
    (add-step ctx (cond-> {:op :delete-object :target (resolve-name ctx target)}
                    (binding-type ctx target)
                    (assoc :object-type (binding-type ctx target))))))

(defn- compile-compute-on [ctx step]
  (let [{:keys [target expr result-attr as]} step
        tgt-bk (resolve-name ctx target)
        tgt-type (binding-type ctx target)
        [bk ctx] (if as (next-binding ctx) [nil ctx])]
    (cond-> (add-step ctx {:op :compute-on :target tgt-bk
                           :expr (resolve-value ctx expr)
                           :result-attr result-attr :as (or bk tgt-bk)})
      as (register-binding as bk tgt-type))))

(defn- compile-collection-op [ctx step]
  (let [{:keys [from as]} step
        from-bk (resolve-name ctx from)
        from-type (binding-type ctx from)
        [bk ctx] (next-binding ctx)
        expr (case (:op step)
               :compute (:expr step)
               :min-by  [:min-by (:attr step)]
               :max-by  [:max-by (:attr step)])]
    (-> ctx
        (add-step (cond-> {:op :compute :from from-bk :expr expr :as bk}
                    (:default step) (assoc :default (:default step))))
        (register-binding as bk from-type))))

(defn- compile-branch [ctx step]
  (let [test-spec (:test step)
        [ctx test-map]
        (if (vector? test-spec)
          (let [[op ref-expr value] test-spec
                resolved (resolve-value ctx ref-expr)
                [test-key ctx] (next-name ctx "_t")]
            [ctx {:test {test-key resolved} :pred [op test-key value]}])
          [ctx {:test (reduce-kv (fn [m k v] (assoc m k (resolve-value ctx v))) {} test-spec)
                :pred (:pred step)}])
        compile-sub (fn [c steps]
                      (reduce compile-step (assoc c :steps []) steps))
        then-ctx (compile-sub ctx (:then step))
        else-ctx (compile-sub (inherit-from ctx then-ctx) (:else step))]
    (-> ctx
        (inherit-from else-ctx)
        (add-step {:op :branch :test (:test test-map) :pred (:pred test-map)
                   :then (:steps then-ctx) :else (:steps else-ctx)}))))

(defn- compile-invocation [ctx step]
  (let [{:keys [fn args as returns]} step
        [bk ctx] (if as (next-binding ctx) [nil ctx])
        resolved-args (resolve-values-map ctx args)]
    (cond-> (add-step ctx {:op (:op step) :fn fn :args resolved-args :as bk})
      as (register-binding as bk returns))))

(def ^:private prompt-var-re #"\{\{\s*([A-Za-z][\w-]*)((?:\.[\w-]+)*)\s*\}\}")

(defn prompt-vars
  [prompt]
  (into #{} (map (comp symbol->binding second)) (re-seq prompt-var-re (or prompt ""))))

(defn- action-arg
  [ctx v]
  (cond
    (and (keyword? v) (contains? (:bindings ctx) v)) [:get (resolve-name ctx v)]
    (and (vector? v) (= :ref (first v)) (= 2 (count v))) [:get (resolve-name ctx (second v))]
    (map? v) (update-vals v #(action-arg ctx %))
    :else (resolve-value ctx v)))

(defn- compile-apply-action [ctx {:keys [action params]}]
  (add-step ctx
            {:op :effect :fn action
             :args {:arg0 (update-vals (or params {}) #(action-arg ctx %))}}))

(defn- media-problem
  [{:keys [media media-as media-pages]}]
  (cond
    (nil? media) nil
    (not (sequential? media)) ":media is a vector of \"{{var.prop}}\" references"
    (some #(not (and (string? %) (re-matches #"\s*\{\{[^}]+\}\}\s*" %))) media)
    (str ":media entries are single \"{{var.prop}}\" references, not " (pr-str (vec media)))
    (and media-as (not (#{:auto :text :image} media-as))) ":media-as is :auto, :text or :image"
    (and (some? media-pages) (not (pos-int? media-pages))) ":media-pages is a positive integer"))

(defn- compile-use-llm [ctx {:keys [prompt system media as output] :as step}]
  (when-let [missing (seq (remove #(some? (get step %)) [:model :prompt]))]
    (refusal/refuse! :refusal.catalog/use-llm-missing {:step step :missing (vec missing)}))
  (when-let [p (llm-out/problem (or output :string))]
    (refusal/refuse! :refusal.catalog/use-llm-output {:step step :output output} {:problem p}))
  (when-let [p (media-problem step)]
    (refusal/refuse! :refusal.catalog/use-llm-media {:step step :media media} {:problem p}))
  (let [vars     (into {} (for [v (into (prompt-vars prompt)
                                        (mapcat prompt-vars (cons system media)))]
                            [v (resolve-name ctx v)]))
        [bk ctx] (if as (next-binding ctx) [nil ctx])]
    (cond-> (add-step ctx (-> (select-keys step [:model :source :prompt :system :output :tools
                                                 :max-iterations :temperature
                                                 :media :media-as :media-pages])
                              (assoc :op :use-llm :vars vars :as bk)))
      as (register-binding as bk))))

(defn- compile-step [ctx step]
  (case (:op step)
    :traverse   (compile-traverse ctx step)
    :search     (compile-search ctx step)
    :for-each   (compile-for-each ctx step)
    :create      (compile-create ctx step)
    :create-link (compile-create-link ctx step)
    :update     (compile-update ctx step)
    :delete-object (compile-delete-object ctx step)
    :compute-on (compile-compute-on ctx step)
    (:compute :min-by :max-by) (compile-collection-op ctx step)
    :branch     (compile-branch ctx step)
    (:call :effect) (compile-invocation ctx step)
    :action     (add-step ctx {:op :action :chain (:chain step)})
    :apply-action (compile-apply-action ctx step)
    :use-llm    (compile-use-llm ctx step)
    :python-edit (add-step ctx (update step :args update-vals #(resolve-name ctx %)))
    :clojure-edit (add-step ctx (update step :args #(mapv (fn [b] (resolve-name ctx b)) %)))
    (refusal/refuse! :refusal.catalog/unknown-kernel-op {:step step} {:op (:op step)})))

(def edit-step-ops
  #{:create :update :create-link :delete-object :delete-link :python-edit :clojure-edit})

(defn writes-an-edit?
  [steps]
  (boolean (some (fn [{:keys [op steps then else tools]}]
                   (or (contains? edit-step-ops op)
                       (= :apply-action op)
                       (and (= :use-llm op) (some :apply-action tools))
                       (writes-an-edit? steps)
                       (writes-an-edit? then)
                       (writes-an-edit? else)))
                 steps)))

(defn compile-function
  ([schema steps] (compile-function schema steps {}))
  ([schema steps params]
   (let [ctx (reduce (fn [c [name-kw {:keys [binding type]}]]
                       (register-binding c name-kw binding (->type c type)))
                     (make-ctx schema) params)
         ctx (reduce compile-step ctx steps)]
     {:object-sets    (:object-sets ctx)
      :steps          (:steps ctx)
      :param-bindings (or params {})})))

(defn bind-inputs
  [param-bindings inputs]
  (reduce-kv
   (fn [acc caller-name value]
     (if-let [entry (get param-bindings caller-name)]
       (assoc acc (:binding entry) value)
       (refusal/refuse! :refusal.catalog/unknown-function-input
                        {:input caller-name
                         :declared (vec (keys (or param-bindings {})))})))
   {} (or inputs {})))

(defmulti parse-source
  :language)

(defmethod parse-source :kernel [{:keys [source]}]
  (if (string? source) (clojure.edn/read-string source) source))

(defmethod parse-source :default [{:keys [language]}]
  (refusal/refuse! :refusal.catalog/unknown-language
                   {:language language :supported [:clojure :python :json :kernel]}))

(defn- apply-type-map
  [result type-map]
  (update result :params
          (fn [params]
            (reduce-kv (fn [m k v]
                         (if-let [t (get type-map k)]
                           (assoc m k (assoc v :type t)) m))
                       params params))))

(defn- read-forms
  [source]
  (let [rdr (java.io.PushbackReader. (java.io.StringReader. source))]
    (loop [forms []]
      (let [form (try (binding [*read-eval* false] (read rdr false ::eof))
                      (catch Exception e
                        (refusal/refuse! :refusal.catalog/clojure-parse-error
                                         {:error (.getMessage e) :forms-so-far forms})))]
        (if (= form ::eof)
          forms
          (recur (conj forms form)))))))

(defn- method-name?
  [sym]
  (and (symbol? sym)
       (.startsWith (name sym) ".")))

(defn- strip-dot
  [sym]
  (subs (name sym) 1))

(defn- method->relation
  [method-sym]
  (let [^String n (strip-dot method-sym)]
    (keyword (if (.startsWith n "get-")
               (subs n 4)
               n))))

(defn- symbol->binding
  [sym]
  (keyword (str/replace (name sym) "_" "-")))

(defn- form->ref
  [form]
  (cond
    (symbol? form)
    (symbol->binding form)

    (and (seq? form) (keyword? (first form)) (= 2 (count form)))
    [:ref (symbol->binding (second form)) (first form)]

    (and (seq? form) (symbol? (first form)) (method-name? (first form)) (= 2 (count form)))
    [:ref (symbol->binding (second form)) (method->relation (first form))]

    (map? form)
    (reduce-kv (fn [m k v] (assoc m k (form->ref v))) {} form)

    (vector? form)
    (mapv form->ref form)

    (seq? form)
    (refusal/refuse! :refusal.catalog/computed-value {:form (pr-str form)})

    :else form))

(defn- is-deref? [form]
  (or (= form 'deref)
      (and (seq? form) (= (first form) 'clojure.core/deref))
      (and (symbol? form) (= (name form) "deref"))))

(defn- ontology-sym?
  [sym]
  (and (symbol? sym) (= "OntologyClient" (namespace sym))))

(defn- is-ontology-search? [form]
  (and (seq? form) (ontology-sym? (first form)) (= "search" (name (first form)))))

(defn- is-ontology-action? [form]
  (and (ontology-sym? form) (= "action" (name form))))

(defn- ontology-call-named? [form n]
  (and (seq? form) (ontology-sym? (first form)) (= n (name (first form)))))

(defn- use-llm-form [form as]
  (assoc (second form) :op :use-llm :as as))

(defn- apply-action-form [form]
  (let [[_ action params] form]
    {:op :apply-action :action (keyword action) :params (form->ref params)}))

(defn- is-ontology-call? [form]
  (and (seq? form) (ontology-sym? (first form))))

(defn- equality-body->filter
  [body param]
  (when (and (seq? body) (= '= (first body)) (= 3 (count body)))
    (let [[_ a b] body
          method-side? (fn [form]
                         (and (seq? form) (= 2 (count form))
                              (method-name? (first form))
                              (= param (second form))))
          [method-form literal] (cond
                                  (method-side? a) [a b]
                                  (method-side? b) [b a])]
      (when method-form
        {:eq {:path (keyword (strip-dot (first method-form)))
              :value literal}}))))

(defn- method-head
  [m]
  (cond
    (method-name? m) m
    (and (seq? m) (method-name? (first m))) (first m)))

(defn- method-call
  [steps method-str]
  (some #(when (and (seq? %) (= method-str (some-> (method-head %) strip-dot))) %) steps))

(defn- bare-methods
  [chain]
  (map #(if (and (seq? %) (= 1 (count %)) (method-name? (first %))) (first %) %) chain))

(defn- exact-match-filter
  [steps match-step]
  (let [path-kw (->> steps
                     (filter method-head)
                     (take-while #(not= match-step %))
                     (map strip-dot)
                     (str/join ".")
                     keyword)
        conv    (fn [match-val]
                  (cond
                    (and (seq? match-val) (method-name? (first match-val))
                         (symbol? (second match-val)))
                    [:ref (symbol->binding (second match-val))
                     (method->relation (first match-val))]
                    (symbol? match-val) (symbol->binding match-val)
                    :else match-val))
        match-vals (rest match-step)]
    (if (= 1 (count match-vals))
      {:eq {:path path-kw :value (conv (first match-vals))}}
      {:one-of {:path path-kw :values (mapv conv match-vals)}})))

(defn- parse-lambda-filter
  [lambda-form]
  (when (and (seq? lambda-form) (= 'fn (first lambda-form)))
    (let [[_ params body] lambda-form
          head (when (seq? body) (first body))]
      (case head
        = (equality-body->filter body (first params))

        -> (let [steps        (bare-methods (rest body))
                 relation     #(method->relation (first (filter method-name? steps)))
                 contains-all (method-call steps "contains-all")
                 exact-match  (method-call steps "exact-match")
                 is-one-of    (method-call steps "is-one-of")]
             (cond
               contains-all {:having-all {:relation    (relation)
                                          :values-from (symbol->binding (second contains-all))}}
               exact-match  (exact-match-filter steps exact-match)
               is-one-of    {:connected-to {:relation (relation)
                                            :ids-from (symbol->binding (second is-one-of))}}))

        nil))))

(defn- collect-filters
  [chain]
  (let [filter-forms (keep (fn [step]
                             (when (and (seq? step) (method-name? (first step))
                                        (= "filter" (strip-dot (first step))))
                               (parse-lambda-filter (second step))))
                           chain)]
    (when (seq filter-forms)
      (if (= 1 (count filter-forms))
        (first filter-forms)
        (into [:and] filter-forms)))))

(declare analyze-form analyze-body analyze-doseq analyze-let-binding)

(defn- chain-has? [chain method-str]
  (some #(= method-str (some-> (method-head %) strip-dot)) chain))

(defn- extract-order-by [chain]
  (let [ob-step (first (filter #(and (seq? %) (= "order-by" (strip-dot (first %)))) chain))
        ob-lambda (when ob-step (second ob-step))]
    (when (and (seq? ob-lambda) (= 'fn (first ob-lambda)))
      (let [body (nth ob-lambda 2)
            field-steps (when (and (seq? body) (= '-> (first body))) (rest body))
            field-methods (filter method-name? (rest (or field-steps [])))
            field (first field-methods)
            dir-method (second field-methods)]
        (cond-> {}
          field (assoc :field (method->relation field))
          dir-method (assoc :dir (keyword (strip-dot dir-method))))))))

(defn- order-by-value [order-expr]
  (when order-expr (if (:dir order-expr) order-expr (:field order-expr))))

(defn- args->map [args]
  (into {} (map-indexed (fn [i a] [(keyword (str "arg" i)) (form->ref a)]) args)))

(defn- analyze-threading
  [forms ctx]
  (let [target (first forms)
        chain (bare-methods (rest forms))]
    (cond
      (is-ontology-search? target)
      (let [type-call (first chain)
            type-name (if (method-name? type-call)
                        (method->relation type-call)
                        (when (seq? type-call) (method->relation (first type-call))))
            has-all (or (some #(= 'deref %) chain) (chain-has? chain "all"))
            take-first (and (some is-deref? chain) (not has-all))]
        [{:op :search
          :type type-name
          :filter (collect-filters chain)
          :order-by (order-by-value (extract-order-by chain))
          :take (when take-first :first)
          :as (:as ctx)}])

      (is-ontology-action? target)
      (let [method-call (second forms)
            fn-name (when (seq? method-call) (method->relation (first method-call)))
            raw-args (when (seq? method-call) (rest method-call))
            fn-str  (some-> fn-name name)]
        (cond
          (and fn-str (str/starts-with? fn-str "create-"))
          [{:op :create
            :type (keyword (subs fn-str (count "create-")))
            :values (form->ref (first raw-args))
            :as (:as ctx)}]

          (and fn-str (str/starts-with? fn-str "link-"))
          [{:op :create-link
            :link-type (keyword (subs fn-str (count "link-")))
            :from (form->ref (first raw-args))
            :to   (form->ref (second raw-args))}]

          (and fn-str (str/starts-with? fn-str "update-"))
          [{:op :update
            :target (form->ref (first raw-args))
            :values (form->ref (second raw-args))}]

          (and fn-str (str/starts-with? fn-str "delete-"))
          [{:op :delete-object :target (form->ref (first raw-args))}]

          :else
          [{:op :effect :fn fn-name :args (args->map raw-args)}]))

      (is-ontology-call? target)
      (let [fn-sym (first target)
            fn-name (keyword (subs (name fn-sym) (inc (.indexOf (name fn-sym) "/"))))]
        [{:op :call :fn fn-name :args (args->map (rest target)) :as (:as ctx)}])

      (symbol? target)
      (if (or (chain-has? chain "filter") (chain-has? chain "all"))
        (let [method-calls (take-while #(when-let [m (method-head %)]
                                          (and (not (#{"filter" "order-by" "all"} (strip-dot m)))
                                               (not (is-deref? %))))
                                       chain)
              take-first (and (some is-deref? chain) (not (chain-has? chain "all")))
              last-idx (dec (count method-calls))]
          (loop [steps [] src (symbol->binding target) idx 0]
            (let [m    (nth method-calls idx)
                  last? (= idx last-idx)
                  as-kw (if last? (:as ctx) (keyword (str "_chain_" idx)))
                  step (cond-> {:op :traverse :from src :as as-kw
                                :relation (method->relation (method-head m))}
                         (not last?) (assoc :take :first)
                         last?       (assoc :filter (collect-filters chain)
                                            :order-by (order-by-value (extract-order-by chain)))
                         (and last? take-first) (assoc :take :first))]
              (if last?
                (conj steps step)
                (recur (conj steps step) as-kw (inc idx))))))

        (let [m (some method-head chain)]
          [{:op :traverse
            :from (symbol->binding target)
            :relation (method->relation m)
            :take (when (some is-deref? chain) :first)
            :as (:as ctx)}]))

      :else
      (refusal/refuse! :refusal.catalog/unrecognized-threading
                       {:target target :chain (vec chain)}))))

(defn- analyze-reduce-min
  [form ctx]
  (let [[_ reducer-fn default-val coll] form]
    (when (and (seq? reducer-fn) (= 'fn (first reducer-fn)))
      (let [body (nth reducer-fn 2)
            attr (when (and (seq? body) (= 'if (first body)))
                   (let [test-form (second body)]
                     (when (and (seq? test-form)
                                (or (= '< (first test-form)) (= '> (first test-form))))
                       (let [left (second test-form)]
                         (when (seq? left)
                           (cond
                             (keyword? (first left)) (first left)
                             (method-name? (first left)) (method->relation (first left))))))))]
        [{:op (if (and (seq? body) (seq? (second body))
                       (= '> (first (second body))))
                :max-by :min-by)
          :from (symbol->binding coll)
          :attr (or attr :cost)
          :default (when (and (map? default-val) (not= default-val {}))
                     default-val)
          :as (:as ctx)}]))))

(defn- analyze-if-branch
  [form _ctx]
  (let [[_ test-form then-form else-form] form
        test-step (when (and (seq? test-form) (#{'< '> '<= '>=  '= 'not=} (first test-form)))
                    (let [op (first test-form)
                          left (second test-form)
                          right (nth test-form 2)
                          op-kw (get {'< :< '> :> '<= :<= '>= :>= '= := 'not= :!=} op)]
                      (cond
                        (and (seq? left) (keyword? (first left)))
                        [op-kw [:ref (symbol->binding (second left)) (first left)] right]

                        (and (seq? left) (symbol? (first left)) (method-name? (first left)))
                        [op-kw
                         [:ref (symbol->binding (second left)) (method->relation (first left))]
                         right]

                        :else
                        [op-kw (symbol->binding left) right])))]
    [{:op :branch
      :test test-step
      :then (analyze-body then-form)
      :else (analyze-body else-form)}]))

(defn- form-head [form]
  (when (seq? form) (first form)))

(defn- analyze-body
  [form]
  (case (form-head form)
    do                 (vec (mapcat #(analyze-form % {}) (rest form)))
    clojure.core/deref (analyze-form (second form) {})
    ->                 (analyze-threading (rest form) {})
    if                 (analyze-if-branch form {})
    doseq              (analyze-doseq form)
    (if (ontology-call-named? form "apply-action")
      [(apply-action-form form)]
      [])))

(defn- analyze-let-binding
  [name-sym value-form]
  (let [ctx {:as (symbol->binding name-sym)}]
    (case (form-head value-form)
      ->     (analyze-threading (rest value-form) ctx)
      reduce (or (analyze-reduce-min value-form ctx)
                 (refusal/refuse! :refusal.catalog/unrecognized-reduce {:form value-form}))
      (if (ontology-call-named? value-form "use-llm")
        [(use-llm-form value-form (:as ctx))]
        (refusal/refuse! :refusal.catalog/unrecognized-binding
                         {:name name-sym :form value-form})))))

(defn- analyze-doseq
  [form]
  (let [[_ [sym coll & more] & body] form]
    (when (or (not (symbol? sym)) (seq more))
      (refusal/refuse! :refusal.catalog/doseq-one-binding {:form form}))
    (let [over  (if (symbol? coll) (symbol->binding coll) (keyword (str "doseq-" (name sym))))
          pre   (when-not (symbol? coll) (analyze-let-binding (symbol (name over)) coll))]
      (conj (vec pre)
            {:op :for-each :over over :as (symbol->binding sym)
             :steps (vec (mapcat #(analyze-form % {}) body))}))))

(defn- analyze-form
  [form ctx]
  (case (form-head form)
    doseq              (analyze-doseq form)
    let                (-> []
                           (into (mapcat (fn [[n v]] (analyze-let-binding n v)))
                                 (partition 2 (second form)))
                           (into (mapcat #(analyze-form % ctx)) (drop 2 form)))
    ->                 (analyze-threading (rest form) ctx)
    if                 (analyze-if-branch form ctx)
    clojure.core/deref (analyze-form (second form) ctx)
    (if (ontology-call-named? form "apply-action")
      [(apply-action-form form)]
      [])))

(declare py-object-type)

(defn- tag-type
  [p schema]
  (some-> (:tag (meta p)) str (py-object-type schema)))

(defn- parse-defn
  [source schema]
  (let [forms (read-forms source)
        defn-form (first (filter #(and (seq? %) (#{'defn 'defn-} (first %))) forms))]
    (when-not defn-form
      (refusal/refuse! :refusal.catalog/no-defn
                       {:source (subs source 0 (min 100 (count source)))}))
    (let [[_ _name params & body] defn-form
          steps (vec (mapcat #(analyze-form % {}) body))]
      {:params (into {} (map (fn [p] (let [b (symbol->binding p)]
                                       [b {:binding b :type (tag-type p schema)}])))
                     params)
       :steps  steps})))

(defn- parse-with-types
  [source type-map schema]
  (apply-type-map (parse-defn source schema) type-map))

(defn- executed-defn
  [source]
  (some #(when (and (seq? %) (#{'defn 'defn-} (first %)) (:sutra/function (meta (second %)))) %)
        (read-forms source)))

(defn- parse-executed
  [[_ fname & more] source schema]
  (let [params (first (filter vector? more))
        decl   (:sutra/function (meta fname))]
    (when-let [bad (seq (remove symbol? params))]
      (refusal/refuse! :refusal.catalog/edit-params-destructured {:params (vec bad)}))
    (when-not (and (map? decl) (sequential? (:edits decl)))
      (refusal/refuse! :refusal.catalog/edit-function-edits {:function (str fname)}))
    (let [ps (mapv (fn [p] {:binding (symbol->binding p) :type (tag-type p schema)}) params)]
      {:params (into {} (map (fn [{:keys [binding type]}] [binding {:binding binding :type type}]))
                     ps)
       :steps  [{:op :clojure-edit :source source :entry (name fname)
                 :declared (mapv keyword (:edits decl))
                 :args (mapv :binding ps)}]})))

(defmethod parse-source :clojure
  [{:keys [source types schema]}]
  (if-let [f (executed-defn source)]
    (parse-executed f source schema)
    (if types
      (parse-with-types source types schema)
      (parse-defn source schema))))

(defn- py-close-paren
  [^String s open]
  (loop [i (inc open) depth 1]
    (when (< i (count s))
      (case (.charAt s i)
        (\( \[ \{) (recur (inc i) (inc depth))
        (\) \] \}) (if (= 1 depth) i (recur (inc i) (dec depth)))
        (recur (inc i) depth)))))

(defn- py-split-top
  [^String s]
  (loop [i 0 depth 0 start 0 out []]
    (if (= i (count s))
      (conj out (subs s start))
      (let [c (.charAt s i)]
        (cond
          (#{\( \[ \{} c) (recur (inc i) (inc depth) start out)
          (#{\) \] \}} c) (recur (inc i) (dec depth) start out)
          (and (= \, c) (zero? depth)) (recur (inc i) depth (inc i) (conj out (subs s start i)))
          :else (recur (inc i) depth start out))))))

(def ^:private py-scalar-types
  #{"str" "int" "float" "bool" "Integer" "Long" "Short" "Byte" "Float" "Double" "Decimal"
    "String" "Boolean" "Date" "Timestamp" "date" "datetime"})

(defn- kebab-of-pascal
  [s]
  (-> s
      (str/replace #"([a-z0-9äöüß])([A-ZÄÖÜ])" "$1-$2")
      str/lower-case
      keyword))

(defn pascal-name
  [type-name]
  (->> (str/split (name type-name) #"[-_]")
       (remove str/blank?)
       (map #(str (str/upper-case (subs % 0 1)) (subs % 1)))
       (apply str)))

(defn- py-object-type
  [annotation schema]
  (let [a (-> annotation str/trim
              (str/replace #"^(typing\.)?Optional\[(.*)\]$" "$2")
              (str/replace #"^(typing\.)?(list|List|Array|Sequence|set|Set)\[(.*)\]$" "$3")
              (str/replace #"^(.*)\s*\|\s*None$" "$1")
              str/trim)]
    (when (and (re-matches #"[A-ZÄÖÜ][\wäöüßÄÖÜ]*" a) (not (py-scalar-types a)))
      (if schema
        (some (fn [[k spec]]
                (let [n (or (:object-type-name spec) k)]
                  (when (= a (pascal-name n)) (keyword (name n)))))
              (:object-types schema))
        (kebab-of-pascal a)))))

(defn- py-param
  [schema raw]
  (let [[lhs] (str/split raw #"=" 2)
        [nm ann] (map str/trim (str/split lhs #":" 2))]
    (when-not (or (str/blank? nm) (str/starts-with? nm "*") (= "self" nm))
      {:py nm :binding (symbol->binding nm) :type (some-> ann (py-object-type schema))})))

(defn- py-functions
  [^String source]
  (for [m (re-seq #"(?m)^@function\b[^\n]*\n(?:@[^\n]*\n)*def\s+(\w+)\s*\(" source)
        :let [entry (second m)
              idx   (.indexOf source ^String (first m))
              open  (+ idx (dec (count (first m))))]]
    {:entry entry :open open :close (py-close-paren source open)}))

(def ^:private scalar-annotations
  {"str" :string "String" :string
   "int" :integer "Integer" :integer "Long" :long "Short" :short "Byte" :byte
   "float" :double "Float" :float "Double" :double "Decimal" :decimal
   "bool" :boolean "Boolean" :boolean
   "Date" :date "date" :date "Timestamp" :timestamp "datetime" :timestamp})

(defn- annotation-type
  [annotation]
  (let [a (-> (str annotation) str/trim
              (str/replace #"^(typing\.)?Optional\[(.*)\]$" "$2")
              (str/replace #"^(.*)\s*\|\s*None$" "$1")
              str/trim)]
    (if-let [[_ _ _ inner] (re-matches
                            #"^(typing\.)?(list|List|Array|Sequence|set|Set)\[(.*)\]$" a)]
      (when-let [t (annotation-type inner)] [:array t])
      (or (scalar-annotations a)
          (when (re-matches #"[A-ZÄÖÜ][\wäöüßÄÖÜ]*" a) (kebab-of-pascal a))))))

(defn- python-declared
  [source]
  (when-let [{:keys [open close]} (first (py-functions source))]
    (when close
      (into {}
            (for [raw (py-split-top (subs source (inc open) close))
                  :let [[lhs default] (str/split raw #"=" 2)
                        [nm ann] (map str/trim (str/split lhs #":" 2))]
                  :when (not (or (str/blank? nm) (str/starts-with? nm "*") (= "self" nm)))]
              [(symbol->binding nm) (cond-> {:type (some-> ann annotation-type)}
                                      default (assoc :optional true))])))))

(defn- clojure-declared
  [source]
  (let [forms (read-forms source)
        form  (or (executed-defn source)
                  (first (filter #(and (seq? %) (#{'defn 'defn-} (first %))) forms)))
        ps    (take-while #(not= '& %) (filter symbol? (first (filter vector? form))))]
    (into {}
          (for [p ps]
            [(symbol->binding p) (cond-> {:type (some-> (:tag (meta p)) annotation-type)}
                                   (:optional (meta p)) (assoc :optional true))]))))

(defn declared-params
  [{:keys [language source code]}]
  (let [text (or source code)]
    (case (keyword (name (or language :clojure)))
      :python  (when (string? text) (python-declared text))
      :clojure (when (string? text) (clojure-declared text))
      :kernel  (update-vals (:params (parse-source {:language :kernel :source text}))
                            #(select-keys % [:type :optional]))
      nil)))

(defmethod parse-source :python
  [{:keys [source schema]}]
  (let [fns (vec (py-functions source))]
    (when-not (= 1 (count fns))
      (refusal/refuse! :refusal.catalog/python-edit-one-def
                       {:functions (mapv :entry fns)}
                       {:count (count fns)
                        :named (if (seq fns) (str " " (pr-str (mapv :entry fns))) "")}))
    (let [{:keys [entry open close]} (first fns)
          _      (when-not close
                   (refusal/refuse! :refusal.catalog/python-unbalanced-params {:entry entry}))
          params (keep #(py-param schema %) (py-split-top (subs source (inc open) close)))]
      {:params (into {} (map (fn [{:keys [binding type]}] [binding {:binding binding :type type}]))
                     params)
       :steps  [{:op :python-edit :source source :entry entry
                 :args (into {} (map (juxt :py :binding)) params)}]})))
