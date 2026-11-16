(ns sutra.ref.sides
  (:require [clojure.string :as str]
            [sutra.ref.err :refer [fail!]]))

(defn kinds
  [W T]
  (loop [acc #{T} todo (seq (get-in W [:object-types T :implements]))]
    (if-let [[i & more] todo]
      (if (acc i) (recur acc more)
        (recur (conj acc i) (seq (concat more (get-in W [:interfaces i :extends])))))
      acc)))

(defn side
  [W L dir]
  (let [lt  (get-in W [:link-types L])
        to? (= :to dir)]
    {:link L :dir dir
     :name (or (get lt (if to? :forward-api-name :reverse-api-name)) (:link-type-name lt) L)
     :near (get lt (if to? :from-object-type :to-object-type))
     :far  (get lt (if to? :to-object-type :from-object-type))}))

(defn- both-sides [W L] [(side W L :to) (side W L :from)])

(defn link-key
  [W h]
  (if (contains? (:link-types W) h)
    h
    (some (fn [[k lt]] (when (= h (:link-type-name lt)) k)) (:link-types W))))

(defn- by-link-key [W T h]
  (when-let [L (link-key W h)]
    (let [ks    (kinds W T)
          sides (filter #(ks (:near %)) (both-sides W L))
          sides (if (seq sides)
                  sides
                  (filter #(and (contains? (:interfaces W) (:near %)) (not (ks (:far %))))
                          (both-sides W L)))]
      (when (= 1 (count sides)) (first sides)))))

(defn resolve-side
  [W T handle]
  (let [h       (if (and (string? handle) (not (str/starts-with? handle "ri."))) (keyword handle)
                  handle)
        ks      (kinds W T)
        matches (filterv #(and (ks (:near %)) (= h (:name %)))
                         (mapcat #(both-sides W %) (keys (:link-types W))))]
    (case (count matches)
      1 (first matches)
      0 (or (by-link-key W T h)
            (fail! :unknown-side (str T " has no link side named " (pr-str handle))
                   {:type T :link handle}))
      (fail! :ambiguous-side (str "link side " (pr-str handle) " is ambiguous on " T)
             {:type T :link handle :candidates (mapv #(select-keys % [:link :dir]) matches)}))))

(defn- iface-chain
  [W i]
  (loop [acc [] seen #{} todo [i]]
    (if-let [[x & more] (seq todo)]
      (if (or (seen x) (not (contains? (:interfaces W) x)))
        (recur acc seen more)
        (recur (conj acc x) (conj seen x) (concat more (get-in W [:interfaces x :extends]))))
      acc)))

(defn- constraint-of
  [W i c]
  (some (fn [x] (some #(when (= c (:link-name %)) %)
                      (get-in W [:interfaces x :link-constraints])))
        (iface-chain W i)))

(defn- far-card [c dir]
  (case [dir (some-> c name keyword)]
    ([:to :many-to-one] [:to :one-to-one] [:from :one-to-many] [:from :one-to-one]) :one
    ([:to :one-to-many] [:to :many-to-many] [:from :many-to-one] [:from :many-to-many]) :many
    nil))

(defn constraint-card
  [c]
  (case (some-> c name keyword)
    (:one :many-to-one :one-to-one) :one
    (:many :one-to-many :many-to-many) :many
    nil))

(defn- satisfying [W chain c]
  (for [[L lt] (:link-types W)
        s (let [x (:satisfies lt)] (cond (sequential? x) x x [x]))
        :when (and (contains? chain (:interface s)) (= c (:constraint s)))]
    L))

(defn constraint-sides
  [W T i c]
  (if-let [{:keys [to-object-type to-interface cardinality]} (constraint-of W i c)]
    (let [chain    (set (iface-chain W i))
          expected (or to-object-type to-interface)
          want     (constraint-card cardinality)
          ks       (kinds W T)
          reaches? (fn [far] (or (= far expected)
                                 (and to-interface (contains? (kinds W far) to-interface))))
          named    (link-key W c)
          cands    (cond-> (set (satisfying W chain c))
                     (contains? (:link-types W) named) (conj named))]
      (vec (for [L   (sort-by str cands)
                 dir [:to :from]
                 :let [s (side W L dir)]
                 :when (and (contains? ks (:near s))
                            (reaches? (:far s))
                            (or (nil? want)
                                (= want (far-card (get-in W [:link-types L :cardinality]) dir))))]
             s)))
    []))

(defn resolve-hop
  [W T {:keys [link dir]}]
  (if dir
    (let [L (link-key W link)]
      (when-not (get-in W [:link-types L])
        (fail! :unknown-link (str "Unknown link: " link) {:link link}))
      (side W L dir))
    (resolve-side W T link)))
