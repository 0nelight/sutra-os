(ns sutra.types.witness
  (:require [sutra.types.query :as q]))

(set! *warn-on-reflection* true)

(def folded
  #{:connected-sum :connected-avg})

(defn folds
  [e]
  (into [] (filter (comp folded first)) (q/connected-nodes e)))

(defn- also
  [p q]
  (if p [:and p q] q))

(defn- key-of
  [a i part]
  (keyword "sutra.types.witness" (str (name a) "." i "." (name part))))

(defn questions
  [a e]
  (into []
        (map-indexed
         (fn [i [op & args :as node]]
           (let [{:keys [over of where]} (q/link-args op args)]
             {:attribute a :fold node :over over :of of :where where
              :seen (key-of a i :seen)
              :used (when (keyword? of) (key-of a i :used))})))
        (folds e)))

(defn pairs
  [qs]
  (into []
        (mapcat (fn [{:keys [over of where seen used]}]
                  (cond-> [[seen (cond-> [:connected-count over] where (conj where))]]
                    used (conj [used [:connected-count over (also where [:nil? of false])]]))))
        qs))

(defn plan
  [derived]
  (let [qs (into [] (mapcat (fn [[a e]] (questions a e))) derived)]
    {:questions qs :pairs (pairs qs)}))

(defn- folded-nothing?
  [{:keys [seen used]} values alone? value]
  (let [n (get values seen)
        m (cond used   (get values used)
                alone? (if (nil? value) 0 1))]
    (when (and (number? n) (number? m))
      (and (pos? n) (zero? m)))))

(defn describe
  [{:keys [seen over of]}]
  (str "der Fold über " (name over) " erreichte " seen
       (if (= 1 seen) " Zeile, die keinen Wert für " " Zeilen, von denen keine einen Wert für ")
       (if (keyword? of) (name of) (pr-str of))
       " trägt — der Wert ist nicht 0, sondern unbeantwortet"))

(defn settle
  [row questions]
  (let [values (:values row)
        per-a  (group-by :attribute questions)
        found  (into {}
                     (keep (fn [[a qs]]
                             (let [bad (filterv #(folded-nothing? % values (= 1 (count qs))
                                                                  (get values a))
                                                qs)]
                               (when (seq bad)
                                 [a (mapv (fn [q]
                                            (let [n (get values (:seen q))]
                                              {:over (:over q) :of (:of q) :seen n
                                               :why (describe (assoc q :seen n))}))
                                          bad)]))))
                     per-a)
        keys*  (into [] (comp (mapcat (juxt :seen :used)) (remove nil?)) questions)]
    (cond-> (update row :values #(apply dissoc % keys*))
      (seq found) (assoc :defects found))))
