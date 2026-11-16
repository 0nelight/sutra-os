(ns sutra.parser.synonyms
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn fold
  [nm]
  (-> (name nm) str/lower-case
      (str/replace "ä" "ae") (str/replace "ö" "oe") (str/replace "ü" "ue") (str/replace "ß" "ss")
      (str/replace #"[-_\s]" "")))

(def ^:private plural-endings ["en" "er" "e" "n" "s"])

(defn- stem [s]
  (or (some #(when (and (str/ends-with? s %) (> (- (count s) (count %)) 3))
               (subs s 0 (- (count s) (count %))))
            plural-endings)
      s))

(defn- levenshtein [^String a ^String b]
  (let [n (count b)]
    (peek (reduce (fn [prev [i ^Character ca]]
                    (reduce (fn [row j]
                              (conj row (min (inc (peek row))
                                             (inc (nth prev (inc j)))
                                             (+ (nth prev j) (if (= ca (.charAt b j)) 0 1)))))
                            [(inc i)] (range n)))
                  (vec (range (inc n)))
                  (map-indexed vector a)))))

(defn- umlaut-plural [s]
  (-> s (str/replace "ae" "a") (str/replace "oe" "o") (str/replace "ue" "u")))

(defn- part-of? [part whole]
  (and (>= (count part) 4) (< (count part) (count whole))
       (or (str/ends-with? whole part) (str/starts-with? whole part))))

(defn- match [wanted cand]
  (let [w (fold wanted) c (fold cand) ws (stem w) cs (stem c)
        forms-w (hash-set w ws (umlaut-plural ws))
        forms-c (hash-set c cs (umlaut-plural cs))]
    (cond
      (= w c)                         {:score 1.0 :why :schreibweise}
      (some forms-c forms-w)          {:score 0.9 :why :plural}
      (some #(part-of? % c) [w ws])
      {:score (+ 0.5 (* 0.3 (/ (count w) (count c)))) :why :teilwort}
      (some #(part-of? % w) [c cs])
      {:score (+ 0.4 (* 0.3 (/ (count c) (count w)))) :why :oberbegriff}
      :else
      (let [d (levenshtein ws cs)]
        (when (<= d (if (> (count ws) 7) 2 1))
          {:score (- 0.7 (* 0.1 d)) :why :tippfehler})))))

(defn candidates
  ([wanted names] (candidates wanted names {}))
  ([wanted names known]
   (let [adopted (for [[alias target] known
                       :when (= (fold alias) (fold wanted))]
                   {:name target :score 1.1 :why :schon-synonym})
         guessed (for [n names
                       :let [m (match wanted n)]
                       :when m]
                   (assoc m :name n))]
     (->> (concat adopted guessed)
          (reduce (fn [acc c] (update acc (:name c) #(if (and % (>= (:score %) (:score c))) % c)))
                  {})
          vals
          (sort-by (juxt (comp - :score) (comp str :name)))
          vec))))
