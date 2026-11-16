(ns sutra.types.semver
  (:require [clojure.string :as str]))

(def ^:private version-re
  #"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$")

(defn parse
  [s]
  (when (string? s)
    (when-let [[_ major minor patch pre build] (re-matches version-re (str/trim s))]
      {:major (parse-long major)
       :minor (parse-long minor)
       :patch (parse-long patch)
       :pre   (when pre
                (mapv #(or (parse-long %) %) (str/split pre #"\.")))
       :build build})))

(defn valid?
  [s]
  (some? (parse s)))

(defn prerelease?
  [s]
  (boolean (seq (:pre (parse s)))))

(defn initial-development?
  [s]
  (some-> (parse s) :major zero?))

(defn- compare-pre-ids
  [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) -1
    (number? b) 1
    :else (compare a b)))

(defn- compare-pre
  [a b]
  (cond
    (and (empty? a) (empty? b)) 0
    (empty? a) 1
    (empty? b) -1
    :else (or (->> (map compare-pre-ids a b)
                   (remove zero?)
                   first)
              (compare (count a) (count b)))))

(defn compare-versions
  [a b]
  (let [pa (parse a) pb (parse b)]
    (cond
      (and (nil? pa) (nil? pb)) 0
      (nil? pa) -1
      (nil? pb) 1
      :else (or (->> [(compare (:major pa) (:major pb))
                      (compare (:minor pa) (:minor pb))
                      (compare (:patch pa) (:patch pb))
                      (compare-pre (:pre pa) (:pre pb))]
                     (remove zero?)
                     first)
                0))))

(defn newest
  [versions]
  (last (sort compare-versions (filter valid? versions))))

(defn satisfies-range?
  [{:keys [version auto-upgrade]} v]
  (let [pm (parse version) pv (parse v)]
    (cond
      (or (nil? pm) (nil? pv)) false
      (not auto-upgrade)       (zero? (compare-versions version v))
      :else (and (empty? (:pre pv))
                 (= (:major pm) (:major pv))
                 (or (pos? (:major pm)) (= (:minor pm) (:minor pv)))
                 (>= (compare-versions v version) 0)))))

(defn max-satisfying
  [versions dep]
  (newest (filter #(satisfies-range? dep %) versions)))
