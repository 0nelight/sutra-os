(ns sutra.observe
  (:refer-clojure :exclude [find])
  (:require [clojure.string :as str]))

(def layers
  [{:name "Idle"      :key :idle      :color "transparent"}
   {:name "Other"     :key :other     :color "grey"}
   {:name "Blocked"   :key :blocked   :color "lightblue"}
   {:name "SQL"       :key :sql       :color "blue"}
   {:name "Store"     :key :store     :color "green"}
   {:name "Index"     :key :index     :color "orange"}
   {:name "Serve"     :key :serve     :color "yellow"}
   {:name "Conductor" :key :conductor :color "purple"}
   {:name "Encode"    :key :encode    :color "magenta"}
   {:name "Clojure"   :key :clojure   :color "brown"}
   {:name "JVM"       :key :jvm       :color "grey"}
   {:name "Render"    :key :render    :color "lightgreen"}
   {:name "Python"    :key :python    :color "red"}])

(def layer-index
  (into {} (map-indexed (fn [i {:keys [key]}] [key i])) layers))

(defn layer-id ^long [k]
  (long (get layer-index k (get layer-index :other))))

(defn layer-name [id]
  (:name (nth layers (long id) nil)))

(def ^:private span-field
  {:key :span :label "Request" :format :string :searchable true})

(def ^:private phase-fields
  [{:key :note :label "Size" :format :string}])

(def ^:private backend-markers
  {:Request
   {:layer   :serve
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.method} {marker.data.uri}"
    :table   "{marker.data.method} {marker.data.uri} — {marker.data.status}, {marker.data.bytes}"
    :fields  [{:key :method :label "Method" :format :string}
              {:key :uri    :label "URI"    :format :string :searchable true}
              {:key :status :label "Status" :format :integer :size? false}
              {:key :bytes  :label "Body"   :format :bytes}]}

   :SQL
   {:layer   :sql
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.rows} rows — {marker.data.sql}"
    :table   "{marker.data.conn} {marker.data.rows} rows — {marker.data.sql}"
    :fields  [{:key :sql   :label "Statement" :format :string :searchable true}
              {:key :rows  :label "Rows"      :format :integer}
              {:key :conn  :label "Pool"      :format :string}
              {:key :folds :label "Correlated folds" :format :integer :size? false}
              {:key :chars :label "Statement size"   :format :bytes :size? false}
              {:key :path  :label "Called by"        :format :string :searchable true}]}

   :Auth
   {:layer   :serve
    :show    [:chart :table :timeline]
    :tooltip "auth and edge"
    :fields  []}

   :Read
   {:layer   :store
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.find} {marker.data.around}"
    :table   "{marker.data.find} {marker.data.around}"
    :fields  [{:key :find   :label "Type"  :format :string :searchable true}
              {:key :around :label "Sides" :format :string}]}

   :Pool
   {:layer   :blocked
    :show    [:chart :table :timeline]
    :tooltip "waited for a connection on {marker.data.conn}"
    :fields  [{:key :conn :label "Pool" :format :string}]}

   :Rows
   {:layer   :serve
    :show    [:chart :table]
    :tooltip "{marker.data.rows} × {marker.data.type-name}"
    :table   "{marker.data.rows} × {marker.data.type-name}"
    :fields  [{:key :type-name :label "Object type" :format :string :searchable true}
              {:key :rows      :label "Rows"        :format :integer}]}

   :Encode
   {:layer   :encode
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.format} → {marker.data.bytes}"
    :table   "{marker.data.format} → {marker.data.bytes}"
    :fields  [{:key :format :label "Format"  :format :string}
              {:key :bytes  :label "Encoded" :format :bytes}]}

   :Fn
   {:layer   :clojure
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.fn}"
    :table   "{marker.data.fn}"
    :fields  [{:key :fn :label "Function" :format :string :searchable true}]}

   :Stuck
   {:layer   :blocked
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.method} {marker.data.uri} — {marker.data.frame}"
    :table   "EXPIRED {marker.data.method} {marker.data.uri} in {marker.data.frame}"
    :fields  [{:key :method :label "Method" :format :string}
              {:key :uri    :label "URI"    :format :string :searchable true}
              {:key :frame  :label "Blocked in" :format :string :searchable true}
              {:key :thread :label "Thread" :format :string}]}

   :Click
   {:layer   :render
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.target}"
    :table   "{marker.data.target}"
    :fields  [{:key :target :label "Element" :format :string :searchable true}]}

   :State
   {:layer   :render
    :show    [:chart :table]
    :tooltip "{marker.data.atom} {marker.data.changed}"
    :table   "{marker.data.atom} {marker.data.changed}"
    :fields  [{:key :atom    :label "Atom" :format :string :searchable true}
              {:key :changed :label "Changed" :format :string}]}

   :Error
   {:layer   :other
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.message}"
    :table   "{marker.data.level} {marker.data.src} — {marker.data.message}"
    :fields  [{:key :level   :label "Level" :format :string}
              {:key :src     :label "Source" :format :string :searchable true}
              {:key :message :label "Message" :format :string :searchable true}]}

   :Python
   {:layer   :python
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.name} {marker.data.model}"
    :table   "{marker.data.transform} {marker.data.name} {marker.data.model}"
    :fields  [{:key :name      :label "Span"      :format :string :searchable true}
              {:key :transform :label "Transform" :format :string :searchable true}
              {:key :model     :label "Model"     :format :string}
              {:key :failed    :label "Failed"    :format :string}]}

   :Gzip
   {:layer   :encode
    :show    [:chart :table :timeline]
    :tooltip "{marker.data.bytes-in} → {marker.data.bytes}"
    :table   "gzip {marker.data.bytes-in} → {marker.data.bytes}"
    :fields  [{:key :bytes-in :label "Before" :format :bytes :size? false}
              {:key :bytes    :label "After"  :format :bytes}]}})

(def phases
  {:api/transit            {:name "api/transit"            :layer :serve}
   :api/transit-decode     {:name "api/transit-decode"     :layer :encode}
   :app/response-text      {:name "app/response.text"      :layer :serve}
   :app/json-parse         {:name "app/JSON.parse"         :layer :encode}
   :app/lift-numbers       {:name "app/lift-numbers"       :layer :encode}
   :app/js->clj-descriptor {:name "app/js->clj descriptor" :layer :encode}
   :app/js->clj-data       {:name "app/js->clj data"       :layer :encode :settled? true}
   :app/->clj              {:name "app/->clj"              :layer :encode}
   :app/on-ok              {:name "app/on-ok"              :layer :render}
   :app/refresh            {:name "app/refresh"            :layer :serve :settled? true}
   :boot/navigation        {:name "boot/navigation"        :layer :serve}
   :boot/main              {:name "boot/main"              :layer :render}
   :boot/module            {:name "boot/module"            :layer :serve}
   :boot/resource          {:name "boot/resource"          :layer :serve}
   :boot/paint             {:name "boot/paint"             :layer :render}
   :boot/long-task         {:name "boot/long-task"         :layer :render}
   :boot/head-visible      {:name "boot/head-visible"      :layer :render :settled? true}})

(def markers
  (merge
   (into {} (map (fn [[k m]] [k (assoc m :name (name k))])) backend-markers)
   (into {} (map (fn [[k {:keys [name layer]}]]
                   [k {:name name :layer layer
                       :show [:chart :table :timeline]
                       :tooltip "{marker.data.note}"
                       :table "{marker.data.note}"
                       :fields phase-fields}]))
         phases)))

(defn marker [k]
  (get markers (keyword k)))

(defn marker-name [k]
  (or (:name (marker k))
      (if (keyword? k) (subs (str k) 1) (str k))))

(defn marker-layer ^long [k]
  (layer-id (:layer (marker k) :other)))

(defn marker-fields [k]
  (conj (vec (:fields (marker k))) span-field))

(def size-fields
  (into {}
        (map (fn [[_ {:keys [name fields]}]]
               [name (into [] (comp (filter #(#{:bytes :integer} (:format %)))
                                    (remove #(false? (:size? %)))
                                    (map :key))
                           fields)]))
        markers))

(def settled-phases
  (into [] (comp (filter (comp :settled? val)) (map (comp :name val))) phases))

(def span-header "X-Sutra-Span")

(def timing-header "Server-Timing")

(def ring-kinds
  {:click             :Click
   :state             :State
   :console-error     :Error
   :console-warn      :Error
   :uncaught          :Error
   :promise-rejection :Error
   :render-error      :Error
   :api-error         :Error})

(def profile-header "X-Sutra-Profile")

(def ^:private span-header-lc "x-sutra-span")

(defn span-of [headers]
  (when (map? headers)
    (or (not-empty (str (get headers span-header-lc)))
        (not-empty (str (get headers span-header)))
        (some (fn [[k v]] (when (= span-header-lc (str/lower-case (name k)))
                            (not-empty (str v))))
              headers))))

(defn event [{:keys [kind at dur track tid span data]}]
  {:name  (marker-name kind)
   :layer (marker-layer kind)
   :start (double at)
   :end   (double (+ (double at) (double (or dur 0))))
   :track track
   :tid   tid
   :data  (cond-> (or data {}) span (assoc :span span))})

(defn foreign-events [started-epoch evs]
  (let [t0 (double (or started-epoch 0))]
    (into []
          (comp (filter :kind)
                (map (fn [{:keys [epoch-ms] :as e}]
                       (event (assoc e :at (max 0.0 (- (double (or epoch-ms t0)) t0)))))))
          evs)))
