(ns sutra.cli.text-location
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [rewrite-clj.zip :as z]))

(def ^:private plumbing
  #{:items :body :tabs :pages})

(defn- key= [zloc k]
  (and (z/sexpr-able? zloc) (= k (z/sexpr zloc))))

(defn- key-of
  [zloc k]
  (when (= :map (z/tag zloc))
    (loop [c (z/down zloc)]
      (cond (nil? c)   nil
            (key= c k) c
            :else      (recur (some-> c z/right z/right))))))

(defn- nth-child [zloc i]
  (when (#{:vector :list} (z/tag zloc))
    (loop [c (z/down zloc) n i]
      (cond (nil? c)  nil
            (zero? n) c
            :else     (recur (z/right c) (dec n))))))

(defn- step [zloc k]
  (if (int? k)
    (when-let [c (nth-child zloc k)] {:value c :at c})
    (when-let [c (key-of zloc k)] {:value (z/right c) :at c})))

(defn- word
  [zloc]
  (some (fn [k]
          (when-let [c (key-of zloc k)]
            (let [v (z/right c)]
              (when (z/sexpr-able? v)
                (let [x (z/sexpr v)]
                  (cond (keyword? x) (name x)
                        (and (string? x) (not (str/blank? x))) x))))))
        [:title :widget :kind]))

(defn locate
  [text path]
  (try
    (loop [zloc  (z/of-string text {:track-position? true})
           line  (first (z/position zloc))
           trail []
           path  path]
      (if-let [{:keys [value at]} (when (seq path) (step zloc (first path)))]
        (let [k (first path)
              w (word value)]
          (recur value
                 (first (z/position at))
                 (cond-> trail
                   (and (keyword? k) (not (plumbing k))) (conj (name k))
                   w                                     (conj w)
                   (and (int? k) (nil? w))               (conj (str k)))
                 (rest path)))
        {:line line :trail trail}))
    (catch Exception _ nil)))

(defn- seg->step [s]
  (let [s (str/trim s)]
    (if (re-matches #"\d+" s) (parse-long s) (keyword s))))

(defn- parse-vector [s]
  (try (let [v (edn/read-string s)] (when (vector? v) v))
       (catch Exception _ nil)))

(defn problems-of
  [line]
  (let [[head tail] (str/split (str line) #" — " 2)
        located     #"^(.*) — bei (\[.*\])$"]
    (or (when-let [parts (and tail (str/includes? tail " — bei [")
                              (str/split tail #" · "))]
          (not-empty
           (vec (keep (fn [part]
                        (when-let [[_ why pfad] (re-matches located part)]
                          (when-let [p (parse-vector pfad)] {:path p :reason why})))
                      parts))))
        (when (and tail (not= "<root>" head))
          [{:path (mapv seg->step (str/split head #" > ")) :reason tail}])
        [{:path [] :reason (or tail (str line))}])))

(defn line-for
  [source text {:keys [path reason]}]
  (if-let [{:keys [line trail]} (when (and text (seq path)) (locate text path))]
    (str source ":" line " " (str/join " › " trail) " — " reason)
    (str (when source (str source " — ")) reason)))
