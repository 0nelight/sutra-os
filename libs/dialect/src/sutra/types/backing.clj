(ns sutra.types.backing
  (:require [sutra.types.languages :as langs]))

(set! *warn-on-reflection* true)

(defn in-project-of
  [source transform]
  (cond-> transform (:project source) (assoc :project (:project source))))

(def backed
  {:object-type     {:suffix "-rows"}
   :link-type       {:suffix "-conn-rows"}
   :dataset         {:suffix "-sync"}
   :media-set       {:suffix "-sync"}
   :materialization {:suffix "-materialize"}
   :export          {:suffix "-export"}})

(defn transform-name
  ([kind kw] (transform-name kind kw nil))
  ([kind kw suffix]
   (let [s (or (get-in backed [kind :suffix])
               (throw (ex-info (str "nothing generated backs a " (pr-str kind))
                               {:kind kind :backed (vec (keys backed))})))]
     (keyword (str (name kw) s suffix)))))

(defn sourceless-root?
  [t]
  (and (= :datasource (get-in t [:input :kind]))
       (not (seq (get-in t [:source :table])))
       (not (seq (get-in t [:source :query])))))

(defn- source-of [{:keys [table query incremental filters]}]
  (cond-> {}
    (seq table)  (assoc :table table)
    (seq query)  (assoc :query query)
    (seq filters) (assoc :filters filters)
    incremental  (assoc :incremental incremental)))

(defn- mode-fields
  [{:keys [language code inputs query datasource transforms incremental] :as ext}]
  (if (langs/compute-language? language)
    (assoc {:language language :inputs inputs}
           (langs/body-field language)
           (if (= :query (langs/body-field language)) query code))
    (cond-> {:input  {:kind :datasource :datasource datasource}
             :source (source-of ext)}
      incremental      (assoc :incremental incremental)
      (seq transforms) (assoc :ops (vec transforms)))))

(defn object-transform
  ([type-kw ext] (object-transform type-kw ext nil))
  ([type-kw {:keys [primary-key column-map] :as ext} suffix]
   (merge {:transform-name (transform-name :object-type type-kw suffix)
           :primary-key    primary-key
           :column-map     column-map
           :output         {:kind :object-type :object-type type-kw}}
          (mode-fields ext))))

(defn dataset-transform
  [ds-kw {:keys [primary-key column-map] :as ext}]
  (merge (cond-> {:transform-name (transform-name :dataset ds-kw)
                  :output         {:kind :dataset :dataset ds-kw}}
           primary-key (assoc :primary-key primary-key)
           column-map  (assoc :column-map column-map))
         (mode-fields ext)))

(defn materialization-transform
  [ds-kw object-type]
  {:transform-name (transform-name :materialization ds-kw)
   :input  {:kind :object-type :object-type object-type}
   :output {:kind :dataset :dataset ds-kw}})

(defn media-set-transform
  [set-kw {:keys [source subfolder]}]
  {:transform-name (transform-name :media-set set-kw)
   :input  {:kind :datasource :datasource source}
   :source {:table (or (not-empty subfolder) "/")}
   :output {:kind :media-set :media-set set-kw}})

(defn export-transform
  [export-kw {:keys [dataset]}]
  {:transform-name (transform-name :export export-kw)
   :input  {:kind :dataset :dataset dataset}
   :output {:kind :export :export export-kw}})

(defn link-producer-name
  [link-kw]
  (keyword (str "link-" (name link-kw))))

(defn fk-producer
  [link-kw key-type {:keys [fk-attribute on-null on-unresolved on-duplicate]}]
  {:transform-name (link-producer-name link-kw)
   :input          {:kind :object-type :object-type key-type}
   :output         (cond-> {:kind :link-type :link-type link-kw :fk-attribute fk-attribute}
                     on-null       (assoc :on-null       on-null)
                     on-unresolved (assoc :on-unresolved on-unresolved)
                     on-duplicate  (assoc :on-duplicate  on-duplicate))})

(defn dataset-join-producer
  [link-kw {:keys [dataset from-key-column to-key-column]}]
  {:transform-name (transform-name :link-type link-kw)
   :language       langs/default-query-language
   :inputs         {:edges {:kind :dataset :dataset dataset}}
   :query          "SELECT * FROM edges"
   :output         {:kind :link-type :link-type link-kw
                    :from-key-column from-key-column :to-key-column to-key-column}})

(defn link-transform
  [conn-kw from to {:keys [strategy sources datasource language code query inputs
                           from-key-column to-key-column match-column shared-property
                           transforms fk-side dataset]
                    :as ext}]
  (let [base {:transform-name (transform-name :link-type conn-kw)
              :output         {:kind :link-type :link-type conn-kw}}]
    (case (cond
            (#{:resolve-by-shared-property :polymorphic :endpoint-fk} strategy) strategy
            dataset  :dataset
            language :compute
            :else    :join-table)
      :resolve-by-shared-property
      (-> base
          (assoc :strategy :resolve-by-shared-property
                 :language language :code code :inputs inputs)
          (update :output assoc
                  :from-key-column from-key-column
                  :match-column    match-column
                  :shared-property shared-property))

      :polymorphic
      (assoc base
             :strategy :polymorphic
             :sources  (mapv #(select-keys % [:from-type :to-type :datasource :table :query
                                              :from-key-column :to-key-column :transforms
                                              :language :code :inputs])
                             sources))

      :endpoint-fk (fk-producer conn-kw (case fk-side :from from :to to) ext)

      :dataset (dataset-join-producer conn-kw ext)

      :compute
      (assoc base
             :language language :code code :query query :inputs inputs
             :from-key-column from-key-column :to-key-column to-key-column)

      :join-table
      (cond-> (assoc base
                     :input           {:kind :datasource :datasource datasource}
                     :source          (source-of ext)
                     :from-key-column from-key-column
                     :to-key-column   to-key-column)
        (seq transforms) (assoc :ops (vec transforms))))))

(defn endpoint-fk-output-problem
  [output]
  (when-let [k (some #(when (get output %) %) [:match-column :shared-property])]
    (str "an endpoint-fk output carries " (pr-str k)
         " — an endpoint-fk link resolves the target on its PRIMARY KEY only"
         " (the FK attribute maps FK→PK), never a match/shared property. Remove"
         " it; to match on a non-PK, compute the FK upstream and declare `{:fk …}`.")))

(defn endpoint-fk-producer?
  [spec]
  (let [{:keys [kind link-type fk-attribute]} (:output spec)]
    (boolean (and (nil? (:language spec))
                  (nil? (:code spec))
                  (= :link-type kind)
                  fk-attribute
                  link-type
                  (= (:transform-name spec) (link-producer-name link-type))))))

(def projected-languages
  (conj (set langs/compute-language-names) nil))

(defn projected-transform?
  [spec]
  (and (contains? projected-languages (:language spec))
       (not (endpoint-fk-producer? spec))))

(defn generated
  [kind k]
  (case kind
    :object-type #{(transform-name :object-type k)}
    :link-type   #{(transform-name :link-type k) (link-producer-name k)}
    :dataset     #{(transform-name :dataset k) (transform-name :materialization k)}
    #{}))
