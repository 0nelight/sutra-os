(ns sutra.types.virtual
  (:require [clojure.string :as str]
            [sutra.types.column :as column]
            [sutra.types.source :as source]))

(set! *warn-on-reflection* true)

(def Detect
  [:map {:closed true}
   [:column {:optional true} :keyword]
   [:every-ms {:optional true} [:int {:min 1000}]]])

(def Virtual
  [:map {:closed true}
   [:source :keyword]
   [:table :string]
   [:primary-key {:optional true} [:vector {:min 1} :keyword]]
   [:detect {:optional true} Detect]])

(def default-probe-ms 60000)

(defn virtual?
  [ds]
  (= :virtual (some-> (:kind ds) name keyword)))

(defn column-key
  [a]
  (keyword (str/lower-case (column/sql-name a))))

(defn declared-key
  [ds]
  (some->> (get-in ds [:virtual :primary-key]) (mapv column-key)))

(defn read-scan
  ([ds] (read-scan ds nil))
  ([ds limit]
   (cond-> {}
     (seq (:columns ds)) (assoc :project
                                (into #{} (map (comp column-key :attribute-name)) (:columns ds)))
     limit               (assoc :limit limit))))

(defn source-name [ds] (get-in ds [:virtual :source]))

(defn table [ds] (get-in ds [:virtual :table]))

(defn probe-ms
  [ds]
  (or (get-in ds [:virtual :detect :every-ms]) default-probe-ms))

(defn detected?
  [ds]
  (contains? (:virtual ds) :detect))

(defn probe-due?
  [ds last-ms now-ms]
  (or (nil? last-ms) (>= (- now-ms last-ms) (probe-ms ds))))

(defn probe-column
  [ds]
  (some-> (get-in ds [:virtual :detect :column]) column-key))

(defn probe-sql-column
  [ds]
  (some-> (get-in ds [:virtual :detect :column]) column/sql-name))

(defn probe-signature
  [ds {:keys [max count rows digest]}]
  (if (probe-column ds)
    [:max (some-> max str) :count (some-> count long)]
    [:digest (or digest (hash (frequencies rows)))]))

(defn token-signature
  [token]
  [:token (pr-str token)])

(defn signature-of-rows
  [ds rows]
  (if-let [c (probe-column ds)]
    (let [vs (keep #(get % c) rows)]
      (probe-signature ds {:max (when (seq vs) (reduce #(if (pos? (compare %2 %1)) %2 %1) vs))
                           :count (count rows)}))
    (probe-signature ds {:rows rows})))

(defn- plain-ident? [s] (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (str s))))

(defn problems
  [sources writers ds-name ds]
  (let [src-nm  (source-name ds)
        src     (get sources src-nm)
        caps    (some-> (:type src) keyword source/profile :capabilities)
        cols    (into #{} (map (comp column-key :attribute-name)) (:columns ds))
        missing (fn [ks] (vec (remove cols ks)))]
    (cond-> []
      (nil? src)
      (conj {:problem :unknown-source :source src-nm})

      (and src (or (:external-transform src) (not (contains? caps :scan))))
      (conj {:problem :source-cannot-scan :source src-nm :type (:type src)})

      (not (plain-ident? (table ds)))
      (conj {:problem :unsafe-table :table (table ds)})

      (and (seq cols) (seq (missing (declared-key ds))))
      (conj {:problem :key-not-a-column :columns (missing (declared-key ds))})

      (and (seq cols) (probe-column ds) (seq (missing [(probe-column ds)])))
      (conj {:problem :detect-not-a-column :column (probe-column ds)})

      (seq (get writers ds-name))
      (conj {:problem :written-by-transform :transforms (vec (sort (get writers ds-name)))}))))
