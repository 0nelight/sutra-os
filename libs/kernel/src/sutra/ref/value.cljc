(ns sutra.ref.value
  (:require [clojure.string :as str]
            [sutra.ref.err :refer [fail!]]))

(defn handle? [x]
  (and (map? x)
       (contains? x :values)
       (or (contains? x :object-type-ref) (contains? x :link-type-ref))))

(defn value [o a]
  (get-in o [:values a]))

(defn emit [run-state e]
  (update run-state :events conj
          (merge {:target nil :before nil :after nil :actor nil :provenance nil} e)))

(defn dissoc-in [m path]
  (let [p (vec (butlast path)) k (last path)]
    (cond (empty? p) (cond-> m (seq path) (dissoc k))
          (contains? (get-in m p) k) (update-in m p dissoc k)
          :else m)))

(defn attr-of [o a]
  (cond
    (handle? o) (if (contains? (:values o) a) (get-in o [:values a]) (get o a))
    (map? o)    (get o a)
    :else       o))

(def coll-ops
  {:pluck    {:ref? true :fn (fn [objs a] (mapv #(value % a) objs))}
   :pluck-id {:ref? true :fn (fn [objs _] (mapv :id objs))}
   :count    {:ref? true
              :fn (fn [objs _]
                    (cond (nil? objs) 0
                          (or (sequential? objs) (set? objs)) (count objs)
                          :else (fail! :count-not-list
                                       (str ":count counts a LIST, and got " (pr-str objs)
                                      " — a single object has no length.")
                                 {:op :count :value objs})))}
   :min-by   {:fn (fn [objs a] (when (seq objs) (apply min-key #(value % a) objs)))}
   :max-by   {:fn (fn [objs a] (when (seq objs) (apply max-key #(value % a) objs)))}
   :first    {:fn (fn [objs _] (first objs))}
   :sort-by  {:fn (fn [objs a] (vec (sort-by #(value % a) objs)))}})

(def binding-ref-ops
  (into #{:get} (keep (fn [[k v]] (when (:ref? v) k))) coll-ops))

(defn coll-op [op objs a]
  (if-let [row (coll-ops op)]
    ((:fn row) objs a)
    (fail! :unknown-coll-op (str "unknown collection op " (pr-str op)
                                 " — known: " (pr-str (vec (sort (keys coll-ops)))))
           {:op op :known (set (keys coll-ops))})))

(defn- internal? [k]
  (and (keyword? k) (str/starts-with? (name k) "_")))

(defn- unbound! [B k form]
  (fail! :unbound (str "Unbound function binding: " k)
         {:binding k :form form :known (vec (sort (keys B)))}))

(defn resolve-binding
  [B v]
  (cond
    (and (keyword? v) (contains? B v)) (get B v)
    (internal? v) (unbound! B v v)
    (and (vector? v) (binding-ref-ops (first v)))
    (let [k (second v)]
      (when (and (internal? k) (not (contains? B k))) (unbound! B k v))
      (if (= :get (first v))
        (let [o (get B k)] (if (= 2 (count v)) o (attr-of o (nth v 2))))
        (coll-op (first v) (get B k) (nth v 2 nil))))
    :else v))

(defn resolve-bindings [B m]
  (update-vals (or m {}) #(resolve-binding B %)))

(defn resolve-arg [B v]
  (if (map? v) (resolve-bindings B v) (resolve-binding B v)))

(defn resolve-deep
  [B m]
  (cond
    (map? m)     (update-vals m #(resolve-deep B %))
    (vector? m)  (if (and (seq m) (binding-ref-ops (first m)))
                   (resolve-binding B m)
                   (mapv #(resolve-deep B %) m))
    (keyword? m) (if (str/starts-with? (name m) "$") (get B m m) m)
    :else m))
