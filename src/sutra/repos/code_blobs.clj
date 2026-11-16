(ns sutra.repos.code-blobs
  (:require [sutra.blobs.api :as blobs]
            [clojure.java.io :as io]))

(set! *warn-on-reflection* true)

(def ^:private source-fields
  {:transforms   #{:code}
   :applications #{:source}
   :functions    #{:source}})

(def ^:private min-lift-bytes 512)

(def ^:private marker-key :sutra/blob)

(defn- marker? [v]
  (and (map? v) (blobs/valid-address? (get v marker-key))))

(defn- liftable? [v]
  (and (string? v) (>= (count (.getBytes ^String v "UTF-8")) min-lift-bytes)))

(defn- put-source! [b ^String s]
  {marker-key (:hash (blobs/put! b (.getBytes s "UTF-8")))})

(defn- fetch [b sha where]
  (if-let [in (blobs/open b sha)]
    (with-open [is in] (slurp (io/reader is :encoding "UTF-8")))
    (throw (ex-info (str "code blob " sha " is not in the CAS — " where
                         " references a source body whose bytes are gone")
                    {:sutra/missing-code-blob sha :sutra/at where}))))

(defn- walk-spec
  [f kind spec]
  (if-not (map? spec)
    spec
    (let [nm (or (:transform-name spec) (:name spec) (:id spec))
          at (fn [k] (str (name kind) " " (pr-str nm) " " k))
          ks (get source-fields kind)
          s  (reduce (fn [m k] (if (contains? m k) (update m k f (at k)) m)) spec ks)]
      (if (and (= kind :functions) (map? (:releases s)))
        (update s :releases update-vals #(walk-spec f kind %))
        s))))

(defn- walk-world [f world]
  (reduce-kv (fn [w kind entities]
               (if (and (contains? source-fields kind) (map? entities))
                 (assoc w kind (update-vals entities #(walk-spec f kind %)))
                 w))
             world
             world))

(defn- lifting
  [b shas]
  (fn [v _where]
    (cond
      (liftable? v) (let [m (put-source! b v)]
                      (vswap! shas conj (get m marker-key))
                      m)
      (marker? v)   (do (vswap! shas conj (get v marker-key)) v)
      :else         v)))

(defn lift
  [b world]
  (let [shas (volatile! #{})]
    [(walk-world (lifting b shas) world) @shas]))

(def ^:private ^java.util.concurrent.ConcurrentHashMap cache
  (java.util.concurrent.ConcurrentHashMap.))

(defn- interned [b sha where]
  (loop []
    (let [^java.lang.ref.SoftReference r
          (.computeIfAbsent cache sha
                            (reify java.util.function.Function
                              (apply [_ _] (java.lang.ref.SoftReference.
                                            (delay (fetch b sha where))))))]
      (if-let [d (.get r)]
        (try @d
             (catch Throwable e
               (.remove cache sha r)
               (throw e)))
        (do (.remove cache sha r) (recur))))))

(defn lower
  [b world]
  (walk-world (fn [v where] (if (marker? v) (interned b (get v marker-key) where) v)) world))
