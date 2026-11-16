(ns ent-lsp.read
  (:require [edamame.core :as e])
  (:import [java.util ArrayList]))

(defrecord Node [v loc])

(defn- wrap [{:keys [obj loc]}] (->Node obj loc))

(defn- walk
  [node path ^ArrayList acc]
  (let [v   (:v node)
        loc (:loc node)]
    (cond
      (map? v)
      (do (.add acc {:path path :loc loc :coll :map})
          (reduce (fn [m [kn vn]]
                    (let [k (walk kn path (ArrayList.))]
                      (.add acc {:path (conj path k) :loc (:loc kn) :value k :key? true})
                      (assoc m k (walk vn (conj path k) acc))))
                  {} v))

      (or (vector? v) (seq? v) (set? v))
      (do (.add acc
                {:path path :loc loc :coll (cond (vector? v) :vector (set? v) :set :else :list)})
          (let [items (into [] (map-indexed (fn [i n] (walk n (conj path i) acc))) v)]
            (cond (vector? v) items (set? v) (set items) :else (apply list items))))

      :else
      (do (.add acc {:path path :loc loc :value v}) v))))

(defn- indexed [data nodes]
  {:data    data
   :nodes   nodes
   :by-path (into {} (map (fn [n] [(if (:key? n) [:key (:path n)] (:path n)) n])) nodes)
   :by-row  (group-by (comp :row :loc)
                      (filter #(and (keyword? (:value %)) (:loc %)) nodes))})

(defn parse
  [text]
  (try
    (let [acc  (ArrayList.)
          data (walk (e/parse-string text {:postprocess wrap}) [] acc)]
      (indexed data (vec acc)))
    (catch Exception ex
      (let [{:keys [row col]} (ex-data ex)]
        {:error {:message (ex-message ex) :row (or row 1) :col (or col 1)}}))))

(def ^:private nowhere {:row 1 :col 1 :end-row 1 :end-col 2})

(defn from-data
  [data]
  (let [acc (ArrayList.)
        node (fn node [x] (->Node (cond (map? x) (into {} (map (fn [[k v]] [(node k) (node v)])) x)
                                        (vector? x) (mapv node x)
                                        :else x)
                                  nowhere))]
    (assoc (indexed (walk (node data) [] acc) (vec acc)) :synthetic true)))

(defn under-schema
  [{:keys [data nodes] :as parsed}]
  (let [root  (some #(when (= [] (:path %)) %) nodes)
        nodes (into (if root [(assoc root :coll :map)] [])
                    (map #(update % :path (fn [p] (into [:schema] p))))
                    nodes)]
    (merge parsed (indexed {:schema data} nodes))))

(defn leaf-at
  [parsed row col]
  (some (fn [{:keys [loc] :as n}]
          (when (and (<= (:col loc) col) (<= col (:end-col loc))) n))
        (get-in parsed [:by-row row])))
