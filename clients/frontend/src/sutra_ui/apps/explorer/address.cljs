(ns sutra-ui.apps.explorer.address
  (:require [sutra.types.rid :as rid]
            [sutra-ui.routing :as routing]))

(def blank-set
  {:filters [] :search "" :sort nil :selection #{} :viz :table})

(def ^:private set-tags
  #{(get rid/kinds-key->tag :object-sets) :temporary-object-set :exploration})

(defn set-rid?
  [s]
  (and (rid/rid? s) (contains? set-tags (rid/rid-kind s))))

(def ^:private perspective->seg {:charts "charts" :map "map"})
(def ^:private seg->perspective (into {} (map (juxt val key)) perspective->seg))

(def ^:private reserved
  #{"keyword" "search" "saved" "objectSet"})

(defn- type-seg
  [t]
  (let [n (routing/detail-id->name-str t)]
    (if (contains? reserved n) (routing/detail-id->str :object-type t) n)))

(defn tail
  [{t :object-type set-rid :object-set sets :set}]
  (cond
    set-rid [(routing/enc set-rid)]
    t       (into [(routing/enc (type-seg t))]
                  (some-> (get-in sets [t :viz]) perspective->seg vector))
    :else   []))

(defn- ->type [s] (routing/str->detail-id :object-type (routing/dec' s)))

(defn- prop [f] (some-> (get f "propertyType") keyword))

(defn- iso [ms] (when (number? ms) (.toISOString (js/Date. ms))))

(defn- range-filter
  [field lo hi]
  (cond
    (and (some? lo) (some? hi)) {:field field :op :between :value (str lo) :value2 (str hi)}
    (some? lo)                  {:field field :op :gte :value (str lo) :value2 ""}
    (some? hi)                  {:field field :op :lte :value (str hi) :value2 ""}))

(defn- property-filter
  [f]
  (let [field (prop f)
        v     (get f "value")]
    (when field
      (case (get v "type")
        "textFilter"   (when-let [t (not-empty (str (get v "text")))]
                         {:field field :op :contains :value t :value2 ""})
        "valuesFilter" (let [vs (get v "values")]
                         (when (= 1 (count vs))
                           {:field field :op :eq :value (str (first vs)) :value2 ""}))
        "numberRangeFilter" (range-filter field (get v "min") (get v "max"))
        "dateRangeFilter"   (let [d (get v "dateRangeFilter")]
                              (range-filter field (get d "start") (get d "end")))
        "timestampRangeFilter" (range-filter field (iso (get v "startMillis"))
                                             (iso (get v "endMillis")))
        nil))))

(defn search->params
  [m]
  (let [t  (some->> (first (get m "objectTypes")) str (routing/str->detail-id :object-type))
        fs (->> (get m "filters")
                (filter #(= "propertyFilter" (get % "type")))
                (filter #(or (nil? (get % "objectType"))
                             (= (get % "objectType") (first (get m "objectTypes")))))
                (keep property-filter)
                (map-indexed (fn [i f] (assoc f :id (str "link-" i))))
                vec)]
    (if t
      {:object-type t :object-set nil :perspective nil
       :search {:filters fs :search (str (or (get m "keyword") ""))}}
      {:object-type nil :object-set nil :keyword (str (or (get m "keyword") ""))})))

(defn- read-json
  [s]
  (try (js->clj (js/JSON.parse s))
       (catch :default _ nil)))

(def ^:private none {:object-type nil :object-set nil :perspective nil})

(defn parse-tail
  [[a b]]
  (cond
    (nil? a)                    none
    (set-rid? (routing/dec' a)) (assoc none :object-set (routing/dec' a))
    :else                       (assoc none
                                       :object-type (->type a)
                                       :perspective (seg->perspective b))))

(defn parse-entry
  [[a b] q]
  (cond
    (= "keyword" a)            (assoc none :keyword (some-> b routing/dec'))
    (= "search" a)             (or (some-> b routing/dec' read-json search->params) none)
    (#{"saved" "objectSet"} a) (assoc none :object-set (some-> b routing/dec'))
    (and (nil? a) (get q "objectTypeId"))
    (assoc none
           :object-type (routing/str->detail-id :object-type (get q "objectTypeId"))
           :perspective (when-not (= "results" (get q "perspectiveId")) :charts))))

(defn enter
  [slice params]
  (let [has? #(contains? params %)
        t    (if (has? :object-type) (:object-type params) (:object-type slice))
        blank #(or % blank-set)]
    (cond-> slice
      (has? :object-type)  (assoc :object-type t :drawer? false)
      (has? :object-set)   (assoc :object-set (:object-set params))
      (has? :keyword)      (assoc :keyword (:keyword params))
      (and t (has? :perspective))
      (update-in [:set t] #(assoc (blank %) :viz (or (:perspective params) :table)))
      (and t (:search params))
      (update-in [:set t] #(-> (blank %)
                               (merge (:search params))
                               (dissoc :restrict))))))
