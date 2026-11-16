(ns sutra.ref.tokens
  (:require [clojure.string :as str]))

(def separators
  "-_/.,:;@+#&|\\()[]{}'\"!?=<>*%~^$`")

(def ^:private cut
  (re-pattern (str "[\\s" (str/join (map #(str "\\" %) separators)) "]+")))

(defn tokens
  [s]
  (into [] (remove str/blank?) (str/split (str/lower-case (str s)) cut)))

(defn- lex
  [s]
  (map (fn [[_ quoted paren word]]
         (cond quoted                     [:phrase quoted]
               (= "(" paren)              [:open]
               (= ")" paren)              [:close]
               (#{"AND" "OR" "NOT"} word) [(keyword (str/lower-case word))]
               :else                      [:word word]))
       (re-seq #"\"([^\"]*)\"|(\(|\))|([^\s()\"]+)" (str s))))

(declare parse-or)

(def ^:private term-start? #{:word :phrase :not :open})

(defn- parse-atom
  [implicit ts]
  (let [[t & more] ts]
    (case (first t)
      :not            (let [[x r] (parse-atom implicit more)] [[:not x] r])
      (:word :phrase) [t more]
      :open           (let [[x r] (parse-or implicit more)]
                        [x (if (= :close (ffirst r)) (rest r) r)])
      [nil more])))

(defn- joined [op acc] (if (next acc) (into [op] acc) (first acc)))

(defn- parse-and
  [implicit ts]
  (loop [[x r] (parse-atom implicit ts) acc []]
    (let [acc  (cond-> acc x (conj x))
          head (ffirst r)]
      (cond
        (= :and head)                                 (recur (parse-atom implicit (rest r)) acc)
        (and (= :and implicit) (term-start? head))    (recur (parse-atom implicit r) acc)
        :else                                         [(joined :and acc) r]))))

(defn- parse-or
  [implicit ts]
  (loop [[x r] (parse-and implicit ts) acc []]
    (let [acc  (cond-> acc x (conj x))
          head (ffirst r)]
      (cond
        (= :or head)                                  (recur (parse-and implicit (rest r)) acc)
        (and (= :or implicit) (term-start? head))     (recur (parse-and implicit r) acc)
        :else                                         [(joined :or acc) r]))))

(defn parse
  [text implicit]
  (first (parse-or implicit (lex text))))

(defn- cut-terms
  [[op & args :as tree]]
  (case op
    (:word :phrase) (when-let [ts (not-empty (tokens (first args)))] [op ts])
    :not            (when-let [x (some-> (first args) cut-terms)] [:not x])
    (:and :or)      (when-let [xs (seq (keep cut-terms args))] (joined op (vec xs)))
    tree))

(defn search-tree
  [text]
  (some-> (parse text :or) cut-terms))

(defn- written
  [[op & args]]
  (case op
    :word   (first args)
    :phrase (str "\"" (first args) "\"")
    :not    (str "NOT (" (written (first args)) ")")
    :and    (str "(" (str/join " AND " (map written args)) ")")
    :or     (str "(" (str/join " OR " (map written args)) ")")))

(defn all-words
  [text]
  (some-> (parse text :and) written))

(defn- run-at? [doc i ts]
  (= ts (subvec doc i (min (count doc) (+ i (count ts))))))

(defn- holds?
  [doc [op & args]]
  (case op
    :word   (every? (fn [t] (some #(str/starts-with? % t) doc)) (first args))
    :phrase (boolean (some #(run-at? doc % (first args)) (range (count doc))))
    :not    (not (holds? doc (first args)))
    :and    (every? #(holds? doc %) args)
    :or     (boolean (some #(holds? doc %) args))))

(defn match?
  [values text]
  (let [doc (tokens (str/join " " (remove nil? values)))]
    (boolean (some->> (search-tree text) (holds? doc)))))
