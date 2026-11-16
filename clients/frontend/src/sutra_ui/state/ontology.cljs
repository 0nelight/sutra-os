(ns sutra-ui.state.ontology
  (:require [clojure.string :as str]
            [sutra.types.shape :as tshape]
            [sutra.types.attribute :as attribute]
            [sutra-ui.api :as api]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]))

(declare mutate! mutate-queue object-attrs->server)

(defn- field->opts
  [{:keys [required display-name description]}]
  (cond-> {}
    required           (assoc :required true)
    (seq display-name) (assoc :name display-name)
    (seq description)  (assoc :description description)))

(defn- attrs->server [fields]
  (mapv (fn [{nm :name t :type :as f}]
          (let [opts (field->opts f)]
            (if (seq opts)
              [(keyword nm) (keyword t) opts]
              [(keyword nm) (keyword t)])))
        fields))

(defn- ->link-constraint
  [{:keys [link-name to to-kind cardinality required]}]
  (let [base  (cond-> {:link-name (keyword link-name) :cardinality (keyword cardinality)}
                required (assoc :required true))
        to-kw (when (seq to) (keyword to))]
    (cond
      (and to-kw (= to-kind "interface")) (assoc base :to-interface to-kw)
      to-kw                                (assoc base :to-object-type to-kw)
      :else                                base)))

(defn aliases->vec
  [aliases]
  (if (string? aliases)
    (into [] (comp (map str/trim) (remove str/blank?)) (str/split aliases #","))
    (vec aliases)))

(defn ->object-type-spec
  [{:keys [name fields implements external-source symbol plural title
           display-name description aliases visibility
           resolve-entity match-keys extra whole?]}]
  (cond-> (merge extra {:name (keyword name) :attributes (object-attrs->server fields)
                        :implements (mapv keyword implements)})
    (some? external-source) (assoc :external-source external-source)
    (seq symbol)            (assoc :symbol symbol)
    (seq plural)            (assoc :plural plural)
    (not (str/blank? display-name)) (assoc :display-name (str/trim display-name))
    (not (str/blank? description))  (assoc :description description)
    (seq (aliases->vec aliases))    (assoc :aliases (aliases->vec aliases))
    (seq (some-> visibility clojure.core/name)) (assoc :visibility (keyword visibility))
    (seq (str title))       (assoc :title (keyword title))
    resolve-entity          (assoc :resolve-entity true)
    (not resolve-entity)    (dissoc :resolve-entity)
    (seq match-keys)        (assoc :match-keys (mapv keyword match-keys))
    (empty? match-keys)     (dissoc :match-keys)
    whole?                  (assoc :whole-declaration? true)))

(defn- ->link-type-spec [{:keys [name from to cardinality external-source
                                 forward-api-name reverse-api-name
                                 forward-name reverse-name satisfies]}]
  (cond-> {:name        (keyword name)
           :from        (keyword from)
           :to          (keyword to)
           :cardinality (keyword cardinality)}
    (some? external-source) (assoc :external-source external-source)
    (seq forward-api-name)  (assoc :forward-api-name (keyword forward-api-name))
    (seq reverse-api-name)  (assoc :reverse-api-name (keyword reverse-api-name))
    (seq forward-name)      (assoc :forward-name forward-name)
    (seq reverse-name)      (assoc :reverse-name reverse-name)
    (some? satisfies)       (assoc :satisfies (vec satisfies))))

(defn ->interface-spec [{:keys [name properties links extends
                                display-name description status searchable]}]
  (cond-> {:name             (keyword name)
           :property-list    (attrs->server properties)
           :link-constraints (mapv ->link-constraint
                                   (filter #(seq (:link-name %)) links))}
    (seq extends)      (assoc :extends (mapv keyword extends))
    (seq display-name) (assoc :display-name display-name)
    (seq description)  (assoc :description description)
    (seq status)       (assoc :status (keyword status))
    (some? searchable) (assoc :searchable (boolean searchable))))

(defn rename-or-update!
  [old-kw spec update-verb rename-fn]
  (if (= old-kw (:name spec))
    (mutate! update-verb spec)
    (let [renamed (-> @mutate-queue
                      (.then (fn [_] (rename-fn old-kw (:name spec)))))]
      (reset! mutate-queue (.catch renamed (fn [_] nil)))
      (-> (shell/with-loading renamed)
          (.then (fn [_] (mutate! update-verb spec)))))))

(defn add-object-type! [form]
  (mutate! :create-object-type (->object-type-spec form)))

(defn update-object-type! [old-kw form]
  (rename-or-update! old-kw (->object-type-spec form)
                     :update-object-type (partial api/rename :object-type)))

(defn delete-object-type! [kw]
  (mutate! :delete-object-type {:name kw}))

(def object-form-keys
  #{:name :attributes :implements :external-source :symbol :plural :title
    :display-name :description :aliases :visibility
    :resolve-entity :match-keys})

(defn- blank-val? [v]
  (or (nil? v) (and (string? v) (str/blank? v)) (and (coll? v) (empty? v))))

(defn clean-prop-opts
  [opts]
  (not-empty (into {} (remove (comp blank-val? val)) opts)))

(defn object-attrs->server
  [fields]
  (mapv (fn [{nm :name t :type :as f}]
          (let [opts (clean-prop-opts (merge (:opts f) (field->opts f)))]
            (cond-> [(keyword (str/trim (clojure.core/name nm)))
                     (if (string? t) (keyword t) t)]
              opts (conj opts))))
        (filter #(seq (str/trim (str (some-> (:name %) clojure.core/name)))) fields)))

(defn attr->form-field
  [{:keys [attribute-name attribute-type opts]}]
  (cond-> {:name (clojure.core/name attribute-name)
           :type (if (keyword? attribute-type) (clojure.core/name attribute-type) attribute-type)}
    (seq opts) (assoc :opts opts)))

(defn object-type->form
  [canon]
  {:name           (clojure.core/name (:object-type-name canon))
   :plural         (or (:plural canon) "")
   :symbol         (or (:symbol canon) "")
   :display-name   (or (:display-name canon) "")
   :description    (or (:description canon) "")
   :aliases        (str/join ", " (:aliases canon))
   :visibility     (:visibility canon)
   :title          (some-> (:title-key canon) clojure.core/name)
   :fields         (mapv attr->form-field (:attribute-list canon))
   :implements     (mapv keyword (:implements canon))
   :resolve-entity (boolean (:resolve-entity canon))
   :match-keys     (mapv clojure.core/name (:match-keys canon))
   :extra          (apply dissoc (tshape/->author :object-types canon {}) object-form-keys)})

(defn object-spec->canonical
  [spec]
  (tshape/->canonical :object-types spec
                      (cond-> {:object-type-name (keyword (:name spec))
                               :attribute-list
                               (mapv #(select-keys (attribute/normalize %)
                                                   [:attribute-name :attribute-type :opts])
                                     (:attributes spec))}
                        (seq (:implements spec))
                        (assoc :implements (mapv keyword (:implements spec)))
                        (:external-source spec)
                        (assoc :external-source (:external-source spec)))))

(defn object-form-round-trip
  [canon]
  (let [was (dissoc canon :id :dataset)
        now (object-spec->canonical
             (->object-type-spec (assoc (object-type->form canon)
                                        :external-source (:external-source canon))))]
    (when (not= was now) {:was was :now now})))

(defn- ->object-type-group-spec [{:keys [name description members]}]
  (cond-> {:name (keyword name)}
    (seq description) (assoc :description description)
    (seq members)     (assoc :members (mapv keyword members))))

(defn add-object-type-group! [form]
  (mutate! :create-object-type-group (->object-type-group-spec form)))

(defn update-object-type-group! [old-kw form]
  (rename-or-update! old-kw (->object-type-group-spec form)
                     :update-object-type-group (partial api/rename :object-type-group)))

(defn set-object-type-groups!
  [obj-kw group-kws]
  (let [want (set group-kws)
        groups (catalog/object-type-groups)]
    (doseq [[g-kw spec] groups
            :let [members (set (:members spec))
                  in?     (contains? members obj-kw)
                  keep?   (contains? want g-kw)]
            :when (not= in? keep?)
            :let [members* (vec (if keep?
                                  (conj members obj-kw)
                                  (disj members obj-kw)))]]
      (update-object-type-group!
       g-kw {:name (clojure.core/name g-kw)
             :description (:description spec)
             :members members*}))))

(defn add-link-type! [form]
  (mutate! :create-link-type (->link-type-spec form)))

(defn update-link-type! [form]
  (mutate! :update-link-type (->link-type-spec form)))

(defn delete-link-type! [name-kw]
  (mutate! :delete-link-type {:name name-kw}))

(defn delete-link-by-id!
  [conn-id]
  (when-let [c (some #(when (= (catalog/link-id %) conn-id) %)
                     (:links @catalog/schema))]
    (delete-link-type! (:name c))))

(defn add-interface! [form]
  (mutate! :create-interface (->interface-spec form)))

(defonce ^:private mutate-queue (atom (js/Promise.resolve nil)))

(defn mutate!
  [action args]
  (let [next-p (-> @mutate-queue
                   (.then (fn [_]
                            (-> (api/mutate action args {:stage? true})
                                (.then (fn [resp]
                                         (staging/refresh-staged!)
                                         (instances/refresh-instance-state!)
                                         (data/refresh-object-counts!)
                                         resp))))))]
    (reset! mutate-queue (.catch next-p (fn [_] nil)))
    (shell/with-loading next-p)))
