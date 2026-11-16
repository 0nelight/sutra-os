(ns sutra.store.judge
  (:require [clojure.string :as str]
            [sutra.kit.time :as time]
            [sutra.store.model :as model]))

(set! *warn-on-reflection* true)

(defn- finding [duty msg data] {:duty duty :msg msg :data data})

(defn code-point-compare [^String a ^String b]
  (let [la (.length a) lb (.length b)]
    (loop [i 0 j 0]
      (cond (and (>= i la) (>= j lb)) 0
            (>= i la) -1
            (>= j lb) 1
            :else (let [ca (.codePointAt a i) cb (.codePointAt b j)]
                    (if (= ca cb)
                      (recur (+ i (Character/charCount ca)) (+ j (Character/charCount cb)))
                      (compare ca cb)))))))

(defn- key-compare [a b]
  (if (and (string? a) (string? b)) (code-point-compare a b) (compare a b)))

(defn clock [prior rev]
  (cond (not (int? rev))
        (finding :duty/clock "a revision that is not an integer" {:rev rev})
        (and prior (not (> rev prior)))
        (finding :duty/clock "a revision not above every one handed out before"
                 {:rev rev :before prior})))

(defn kv-scan [{:keys [after upto limit reverse? keys-only?] :as opts} answer]
  (let [ks (map first answer)]
    (or (when-not (every? #(and (vector? %) (= (if keys-only? 1 2) (count %))) answer)
          (finding :duty/kv-scan "an entry in another shape than the opts ask" {:opts opts}))
        (when (and limit (> (count answer) limit))
          (finding :duty/kv-scan "more keys than :limit" {:opts opts :count (count answer)}))
        (when-let [k (first (filter #(or (and after (not (pos? (key-compare % after))))
                                         (and upto (pos? (key-compare % upto))))
                                    ks))]
          (finding :duty/kv-scan "a key outside :after/:upto" {:opts opts :key k}))
        (when-let [[a b] (first (filter (fn [[a b]] (let [c (key-compare a b)]
                                                      (if reverse? (<= c 0) (>= c 0))))
                                        (partition 2 1 ks)))]
          (finding :duty/kv-scan "keys out of code-point order" {:opts opts :keys [a b]})))))

(defn kv-parts [parts]
  (when-let [[a b] (first (filter (fn [[a b]] (>= (key-compare a b) 0)) (partition 2 1 parts)))]
    (finding :duty/kv-parts "parts out of order or repeated" {:parts [a b]})))

(defn kv-read [ks answer]
  (let [asked (set ks)]
    (when-let [[k v] (first (filter (fn [[k v]] (or (not (asked k)) (nil? v))) answer))]
      (finding :duty/kv-read "a key not asked for, or one without a value" {:key k :value v}))))

(defn ids [duty T asked xs]
  (let [got (map :id xs)]
    (or (when (not= (count got) (count (set got)))
          (finding duty "an id twice" {:kind T}))
        (when asked
          (when-let [x (first (remove (set asked) got))]
            (finding duty "an id not asked for" {:kind T :id x}))))))

(defn attrs [sig T rows]
  (let [known (set (model/attrs-of sig T))]
    (when-let [x (first (filter #(seq (remove known (keys (:values %)))) rows))]
      (finding :duty/rows "a value of an attribute the signature does not name"
               {:kind T :id (:id x) :attributes (vec (remove known (keys (:values x))))}))))

(defn rows [sig T asked rows]
  (or (ids :duty/rows T asked rows) (attrs sig T rows)))

(defn links [L where links]
  (or (when-let [l (first (remove (model/matches where) links))]
        (finding :duty/links "a link outside :where" {:kind L :where where :id (:id l)}))
      (ids :duty/links L (:ids where) links)))

(defn stable [question prior answer]
  (when (and prior (not= prior answer))
    (finding :duty/read-stable "a read handle answered the same question otherwise"
             {:question (first question)})))

(defn- text-as [s read]
  (or (cond (inst? read)    (some-> (time/->instant s) java.util.Date/from)
            (number? read)  (when (re-matches #"[+-]?\d+(\.\d+)?([eE][+-]?\d+)?" s) (bigdec s))
            (boolean? read) ({"t" true "true" true "f" false "false" false} (str/lower-case s)))
      s))

(defn- as-read
  [rows back]
  (let [by-id (into {} (map (juxt :id :values)) back)]
    (mapv (fn [{:keys [id] :as row}]
            (let [rv (by-id id)]
              (update row :values
                      #(reduce-kv (fn [m k v]
                                    (let [r (get rv k)]
                                      (cond-> m (and (string? v) (some? r) (not (string? r)))
                                        (assoc k (text-as v r)))))
                                  % %))))
          rows)))

(defn own-write [T wrote back]
  (let [wrote (as-read wrote back)]
    (when-not (model/same-rows? back wrote)
      (let [by-id (into {} (map (juxt :id model/normal)) back)
            miss  (first (remove #(model/same-rows? [%] (keep by-id [(:id %)])) wrote))]
        (finding :duty/write-own "a write handle did not read its own write"
                 {:kind T :wrote miss :read (some-> miss :id by-id)})))))

(defn own-delete [T deleted back]
  (when (seq back)
    (finding :duty/write-own "a write handle still read a row it deleted"
             {:kind T :ids (vec deleted) :read (mapv :id back)})))

(defn record-key [coll prior assigned]
  (when-not (and (int? assigned) (or (nil? prior) (> assigned prior)))
    (finding :duty/record-auto "an assigned key not above every one assigned before"
             {:coll coll :key assigned :before prior})))

(defn same-rows [T a b]
  (let [b (as-read b a)]
    (when-not (model/same-rows? a b)
      (let [ia (set (map :id a)) ib (set (map :id b))
            ma (into {} (map (juxt :id model/normal)) a)
            mb (into {} (map (juxt :id model/normal)) b)
            id (first (remove #(model/same-rows? [(ma %)] [(mb %)]) (filter ib ia)))]
        (finding :duty/shadow "the engine answered otherwise than the reference semantics"
                 {:kind T :only-engine (vec (remove ib ia)) :only-reference (vec (remove ia ib))
                  :engine-row (ma id) :reference-row (mb id)})))))
