(ns sutra.store.scan
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.store.semantics :as sem]
            [sutra.store.shapes :as shapes]))

(set! *warn-on-reflection* true)

(def key-attrs #{:db/key :db/id})

(defn- attr-of [[k v] a] (if (key-attrs a) k (get v a)))

(defn holds?
  [e [op a b :as pred]]
  (boolean
   (case op
     :and  (every? #(holds? e %) (rest pred))
     :or   (some #(holds? e %) (rest pred))
     :not  (not (holds? e a))
     (let [x (attr-of e a)]
       (case op
         :=           (sem/equal? x b)
         :not=        (and (some? x) (some? b) (not (sem/equal? x b)))
         :in          (some #(sem/equal? x %) b)
         :<           (and (some? x) (some? b) (neg? (sem/compare-values x b)))
         :<=          (and (some? x) (some? b) (not (pos? (sem/compare-values x b))))
         :>           (and (some? x) (some? b) (pos? (sem/compare-values x b)))
         :>=          (and (some? x) (some? b) (not (neg? (sem/compare-values x b))))
         :nil?        (if b (nil? x) (some? x))
         :starts-with (and (some? x) (str/starts-with? (str (sem/canon x)) (str (sem/canon b))))
         :has         (and (coll? x) (some #(sem/equal? % b) x))
         (anomaly/raise! :incorrect
                         (str "unknown scan predicate " (pr-str pred))
                         {:pred pred}))))))

(defn- in-fn
  [b]
  (let [cb    (map sem/canon b)
        texts (into #{} (filter string?) cb)
        other (into [] (remove string?) cb)]
    (fn [x]
      (let [x (sem/canon x)]
        (cond (nil? x)    false
              (string? x) (contains? texts x)
              :else       (boolean (some #(sem/equal? x %) other)))))))

(defn- pred-fn
  [[op a b :as pred]]
  (case op
    :and (let [fs (mapv pred-fn (rest pred))] (fn [e] (every? #(% e) fs)))
    :or  (let [fs (mapv pred-fn (rest pred))] (fn [e] (boolean (some #(% e) fs))))
    :not (let [f (pred-fn a)] (fn [e] (not (f e))))
    :in  (let [f (in-fn b)] (fn [e] (f (attr-of e a))))
    (fn [e] (holds? e pred))))

(defn ops-of
  [[op a :as pred]]
  (case op
    (:and :or) (mapcat ops-of (rest pred))
    :not       (ops-of a)
    [op]))

(defn scans?
  [p {[kind] :source}]
  (contains? (:sources p) kind))

(defn- structure? [[op]] (contains? #{:and :or :not} op))

(defn- negates? [[op :as p]]
  (case op
    :not       true
    (:and :or) (boolean (some negates? (rest p)))
    (= op :not=)))

(defn value-class
  [v]
  (cond (nil? v)                        :nil
        (string? v)                     :text
        (or (integer? v) (decimal? v))  :exact-number
        :else                           :other))

(defn comparisons-of
  [[op a b :as pred]]
  (case op
    (:and :or) (mapcat comparisons-of (rest pred))
    :not       (comparisons-of a)
    :in        (let [cs (into #{} (map value-class) (remove nil? b))]
                 [[op a (case (count cs) 1 (first cs) 0 :nil :other)]])
    [[op a (value-class b)]]))

(defn- pred-judgement
  [preds [op attr class]]
  (let [j (get preds op :none)]
    (if (map? j) (get j [attr class] (:default j :none)) j)))

(defn- nots?
  [[op :as p]]
  (case op
    :not       true
    (:and :or) (boolean (some nots? (rest p)))
    false))

(defn- judge
  [{:keys [preds compound? not?]} pred]
  (let [ks (map #(pred-judgement preds %) (comparisons-of pred))
        j  (cond (some #{:none} ks) :none (some #{:inexact} ks) :inexact :else :exact)]
    (cond (and (false? compound?) (structure? pred)) :none
          (and (false? not?) (nots? pred))           :none
          (and (= :inexact j) (negates? pred))       :none
          :else                                      j)))

(defn orders?
  [{:keys [order?]} order]
  (cond (empty? order)  true
        (set? order?)   (every? (fn [[a]] (contains? order? a)) order)
        :else           (boolean order?)))

(defn offer
  [{:keys [page? project? agg-mode] aggs :agg :as p} {:keys [where order after limit project agg]}]
  (let [ws    (mapv (partial judge p) where)
        exact (and (every? #{:exact} ws) (orders? p order))]
    {:where   ws
     :order   (boolean (and (seq order) (orders? p order)))
     :page    (boolean (and (or (some? after) limit) (not agg) page? exact))
     :project (boolean (and project project?))
     :agg     (if (and agg exact (nil? after)
                       (contains? (or aggs #{}) (cond (:count? agg) :count (:pick agg) :pick
                                                      :else :group-by)))
                (or agg-mode :complete)
                :none)}))

(defn- attrs-of [[op a :as pred]]
  (case op
    (:and :or) (mapcat attrs-of (rest pred))
    :not       (attrs-of a)
    [a]))

(defn- core-reads
  [{:keys [where order agg]} offer]
  (cond-> (into #{}
                (comp (keep (fn [[p o]] (when (not= :exact o) p))) (mapcat attrs-of)
                      (remove key-attrs))
                (map vector where (:where offer)))
    (and (seq order) (not (:order offer)))       (into (remove key-attrs) (map first order))
    (and (:group-by agg) (= :none (:agg offer))) (into (remove key-attrs) [(:group-by agg)])
    (and (:pick agg) (= :none (:agg offer)))
    (into (remove key-attrs) (let [{:keys [by max min]} (:pick agg)] (concat by max min)))))

(defn- widened
  [scan offer]
  (into (:project scan) (core-reads scan offer)))

(defn pushed
  [{:keys [where] :as scan} offer]
  (cond-> (assoc scan
                 :where (into [] (keep (fn [[p o]] (when (not= :none o) p)))
                              (map vector where (:where offer))))
    (not (:order offer))         (dissoc :order)
    (not (:page offer))          (dissoc :after :limit)
    (not (:project offer))       (dissoc :project)
    (:project offer)             (assoc :project (widened scan offer))
    (= :none (:agg offer))       (dissoc :agg)))

(defn residual
  [{:keys [where order after limit project agg] :as scan} offer]
  (cond-> {:where (into [] (keep (fn [[p o]] (when (not= :exact o) p)))
                        (map vector where (:where offer)))}
    (and (seq order) (not (:order offer)))                  (assoc :order order)
    (and (or (some? after) limit) (not (:page offer)) (= :none (:agg offer)))
    (assoc :page (cond-> {} (some? after) (assoc :after after) limit (assoc :limit limit)))
    (and project (or (not (:project offer)) (not= project (widened scan offer))))
    (assoc :project project)
    (and agg (= :none (:agg offer)))                        (assoc :agg agg)))

(defn agg-state
  [entries agg]
  (let [g (:group-by agg)]
    (reduce (fn [m e] (update-in m [(if g (sem/group-key (attr-of e g)) []) :count] (fnil inc 0)))
            {} entries)))

(defn- exact-sum
  [x y]
  (try (+ x y)
       (catch ArithmeticException _
         (throw (ex-info "a partial sum overflows its integer type"
                         {:store/engine-contract :pushdown :int-overflow [x y]})))))

(defn merge-agg-states
  [states]
  (let [least    (fn [x y] (first (sort sem/compare-values (remove nil? [x y]))))
        greatest (fn [x y] (last (sort sem/compare-values (remove nil? [x y]))))]
    (apply merge-with
           (fn [a b]
             (cond-> {}
               (or (:count a) (:count b)) (assoc :count (+ (or (:count a) 0) (or (:count b) 0)))
               (or (:sum a) (:sum b))     (assoc :sum (exact-sum (or (:sum a) 0) (or (:sum b) 0)))
               (or (:min a) (:min b))     (assoc :min (least (:min a) (:min b)))
               (or (:max a) (:max b))     (assoc :max (greatest (:max a) (:max b)))))
           {} (map #(update-keys % sem/group-key) states))))

(defn- pick [entries {:keys [by max min]}]
  (let [attrs (or max min)
        rank  (fn [e] [(str (or (some #(attr-of e %) attrs) "")) (str (first e))])
        better? (if max pos? neg?)]
    (->> entries
         (reduce (fn [m e]
                   (let [g (mapv #(attr-of e %) by)
                         o (get m g)]
                     (if (or (nil? o) (better? (compare (rank e) (rank o)))) (assoc m g e) m)))
                 {})
         vals
         (sort-by first sem/compare-values)
         vec)))

(defn finish-agg
  [agg state]
  (if (:group-by agg)
    (into {} (map (fn [[k s]] [k (:count s)])) state)
    (get-in state [[] :count] 0)))

(defn- by-order [order]
  (fn [ea eb]
    (or (some (fn [[a dir]]
                (let [c (sem/compare-in dir (attr-of ea a) (attr-of eb a))] (when-not (zero? c) c)))
              order)
        (sem/compare-values (first ea) (first eb)))))

(defn run
  [entries {:keys [where order project agg page] :as s}]
  (let [{:keys [after limit]} (or page s)
        found (cond->> entries
                (seq where)   (filter (let [fs (mapv pred-fn where)] (fn [e] (every? #(% e) fs))))
                (some? after) (filter #(pos? (sem/compare-values (first %) after)))
                (seq order)   (sort (by-order order)))]
    (cond
      (:pick agg)
      (cond->> (pick found (:pick agg))
        project (mapv (fn [[k v]] [k (select-keys v project)])))

      agg
      (finish-agg agg (agg-state found agg))

      :else
      (cond->> found
        limit   (take limit)
        project (map (fn [[k v]] [k (select-keys v project)]))
        true    vec))))

(defn answer
  [scan offer pushed-answer]
  (let [r (residual scan offer)]
    (cond
      (and (= :partial (:agg offer)) (:pick (:agg scan)))
      (run pushed-answer {:agg (:agg scan) :project (:project scan)})
      (= :partial (:agg offer)) (finish-agg (:agg scan) (merge-agg-states pushed-answer))
      (= [:where] (keys r))     (if (empty? (:where r)) pushed-answer (run pushed-answer r))
      :else                     (run pushed-answer r))))

(defn reference
  [entries scan]
  (run (sort-by first sem/compare-values entries) scan))

(defn check!
  [scan]
  (when-not (shapes/valid? :driver/scan scan)
    (anomaly/raise! :incorrect
                    (str "malformed scan " (pr-str scan))
                    {:scan scan :explain (shapes/explain :driver/scan scan)}))
  scan)

(defn same-answer?
  [{:keys [order agg]} a b]
  (cond
    agg         (sem/same-data? a b)
    (seq order) (sem/same-data? (vec a) (vec b))
    :else       (let [by-key #(vec (sort-by (fn [[k v]] [k (pr-str v)]) sem/compare-values %))]
                  (sem/same-data? (by-key a) (by-key b)))))

(defn- as-entries
  [a]
  (cond (map? a) (map (fn [[k n]] [(sem/group-key k) n]) a)
        (sequential? a) a))

(defn- unmatched
  [xs ys]
  (remove (fn [x] (some #(sem/same-data? x %) ys)) xs))

(defn divergence
  [kind request offer pushed reference n]
  (let [cut #(if (sequential? %) (vec (take n %)) %)
        ps  (as-entries pushed)
        rs  (as-entries reference)]
    (cond-> {:store/engine-contract :pushdown :kind kind :request request
             :pushed (cut pushed) :reference (cut reference)}
      offer (assoc :offer offer)
      (and ps rs)
      (assoc :diff
             (let [only-p (unmatched ps rs) only-r (unmatched rs ps)]
               {:only-pushed    (vec (take n only-p))
                :only-reference (vec (take n only-r))
                :order-differs? (and (empty? only-p) (empty? only-r) (sequential? pushed)
                                     (not (sem/same-data? (vec pushed) (vec reference))))})))))
