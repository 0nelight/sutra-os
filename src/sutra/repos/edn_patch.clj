(ns sutra.repos.edn-patch
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.parser.canonical-print :as cp]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn- diff
  [path a b]
  (cond
    (= a b) []

    (and (map? a) (map? b))
    (-> []
        (into (for [k (keys a) :when (not (contains? b k))]
                {:op :remove :path path :key k}))
        (into (mapcat (fn [[k v]]
                        (if (contains? a k)
                          (diff (conj path [:k k]) (get a k) v)
                          [{:op :add :path path :key k :value v}])))
              b))

    (and (vector? a) (vector? b) (= (count a) (count b)))
    (vec (mapcat (fn [i] (diff (conj path [:i i]) (nth a i) (nth b i)))
                 (range (count a))))

    :else [{:op :replace :path path :value b}]))

(defn- ignorable? [node]
  (or (n/whitespace-or-comment? node) (= :uneval (n/tag node))))

(defn- significant [children]
  (into [] (keep-indexed (fn [i c] (when-not (ignorable? c) i))) children))

(defn- root-loc
  [node]
  (let [loc (z/of-node node)]
    (loop [l loc]
      (if (and l (ignorable? (z/node l))) (recur (z/right l)) l))))

(defn- child-loc
  [loc i]
  (nth (iterate z/right (z/down loc)) i))

(defn- step
  [loc [kind x]]
  (case kind
    :i (child-loc loc x)
    :k (let [sig (significant (n/children (z/node loc)))
             ks  (take-nth 2 (range (count sig)))]
         (some (fn [j]
                 (let [kl (child-loc loc j)]
                   (when (= x (z/sexpr kl)) (child-loc loc (inc j)))))
               ks))))

(def ^:private open-width
  {:map 1 :vector 1 :list 1 :set 2 :forms 0})

(defn- col-of
  [loc]
  (loop [l loc, acc 0]
    (if-let [left (z/left* l)]
      (let [s (n/string (z/node left))
            i (str/last-index-of s "\n")]
        (if i
          (inc (+ acc (- (count s) i 1)))
          (recur left (+ acc (count s)))))
      (if-let [up (z/up* l)]
        (recur up (+ acc (long (get open-width (n/tag (z/node up)) 1))))
        (inc acc)))))

(defn- render
  [v col]
  (let [lines (str/split-lines (str/trimr (cp/canonical-str v)))
        pad   (apply str (repeat (dec col) \space))]
    (str/join (str "\n" pad) lines)))

(defn- ->node [v col] (p/parse-string (render v col)))

(defn- pair-at
  [children sig k]
  (some (fn [j]
          (let [kn (nth children (nth sig j))]
            (when (= k (n/sexpr kn)) j)))
        (take-nth 2 (range (count sig)))))

(defn- remove-pair
  [loc k]
  (let [children (vec (n/children (z/node loc)))
        sig      (significant children)
        j        (pair-at children sig k)
        ki       (nth sig j)
        vi       (nth sig (inc j))
        blank?   #(n/whitespace? (nth children %))
        [start end]
        (if (zero? j)
          [ki (dec (or (first (remove blank? (range (inc vi) (count children))))
                       (count children)))]
          (let [after (inc (nth sig (dec j)))
                past  (or (first (remove #(and (blank? %) (not (n/linebreak? (nth children %))))
                                         (range after ki)))
                          ki)]
            [(if (and (< past ki) (n/comment? (nth children past))) (inc past) after) vi]))]
    (z/replace loc (n/replace-children (z/node loc)
                                       (into (subvec children 0 start)
                                             (subvec children (inc end)))))))

(defn- add-pair
  [loc k v]
  (let [node     (z/node loc)
        children (vec (n/children node))
        sig      (significant children)
        kstr     (pr-str k)]
    (if (empty? sig)
      (z/replace loc (n/replace-children node [(p/parse-string kstr) (n/spaces 1)
                                               (->node v (+ (col-of loc) 2 (count kstr)))]))
      (let [kcol     (col-of (child-loc loc 0))
            last-v   (peek sig)
            comments (filter #(n/comment? (nth children %))
                             (take-while #(or (n/whitespace? (nth children %))
                                              (n/comment? (nth children %)))
                                         (range (inc last-v) (count children))))
            at       (if (seq comments) (inc (last comments)) (inc last-v))
            lead     (cond (seq comments)                [(n/spaces (dec kcol))]
                           (some n/linebreak? children)  [(n/newlines 1) (n/spaces (dec kcol))]
                           :else                         [(n/spaces 1)])
            pair     (into lead [(p/parse-string kstr) (n/spaces 1)
                                 (->node v (+ kcol (count kstr) 1))])]
        (z/replace loc (n/replace-children node (-> (subvec children 0 at)
                                                    (into pair)
                                                    (into (subvec children at)))))))))

(defn- apply-edit
  [node {:keys [op path key value]}]
  (let [loc (reduce (fn [l s] (or (step l s) (reduced nil))) (root-loc node) path)]
    (when loc
      (z/root
       (case op
         :replace (z/replace loc (->node value (col-of loc)))
         :remove  (remove-pair loc key)
         :add     (add-pair loc key value))))))

(defn- apply-edits
  [parsed edits]
  (some-> (reduce (fn [nd e] (or (apply-edit nd e) (reduced nil))) parsed edits)
          n/string))

(def ^:private max-minimize
  24)

(defn patch
  [^String text target owned same?]
  (let [authored (edn/read-string text)]
    (when (and (map? authored) (map? target))
      (let [mine  (if owned #(select-keys % owned) identity)
            edits  (diff [] (mine authored) (mine target))
            parsed (p/parse-string-all text)
            full   (apply-edits parsed edits)]
        (when (and full (same? full))
          (if (< max-minimize (count edits))
            full
            (loop [kept edits, i 0, best full]
              (if (>= i (count kept))
                best
                (let [without (into (subvec kept 0 i) (subvec kept (inc i)))
                      t       (apply-edits parsed without)]
                  (if (and t (same? t))
                    (recur without i t)
                    (recur kept (inc i) best)))))))))))
