(ns sutra.foundation.profile.gecko
  (:require [sutra.foundation.profile :as prof]
            [sutra.observe :as obs]))

(set! *warn-on-reflection* true)

(def ^:const gecko-version 28)

(def ^:private panel
  {:chart "marker-chart" :table "marker-table" :timeline "timeline-overview"})

(def marker-schema
  (into []
        (map (fn [[k {:keys [name show tooltip table]}]]
               (cond-> {:name name
                        :display (mapv panel show)
                        :data (mapv (fn [{:keys [key label format searchable]}]
                                      (cond-> {:key (clojure.core/name key)
                                               :label label
                                               :format (clojure.core/name format)}
                                        searchable (assoc :searchable true)))
                                    (obs/marker-fields k))}
                 tooltip (assoc :tooltipLabel tooltip)
                 table   (assoc :tableLabel table))))
        obs/markers))

(defn- intern-in [[idx rows] x]
  (if-let [i (get idx x)]
    [[idx rows] i]
    (let [i (count rows)] [[(assoc idx x i) (conj rows x)] i])))

(def ^:private empty-tables
  {:strings [{} []] :frames [{} []] :stacks [{} []] :ids {}})

(defn tables [stacks]
  (reduce
   (fn [acc [id stack]]
     (let [[acc prefix]
           (reduce
            (fn [[acc prefix] [label line]]
              (let [[strings si] (intern-in (:strings acc) label)
                    [ft fi] (intern-in (:frames acc) [si line (prof/layer-of label)])
                    [kt ki] (intern-in (:stacks acc) [prefix fi])]
                [(assoc acc :strings strings :frames ft :stacks kt) ki]))
            [acc nil]
            (rseq stack))]
       (assoc-in acc [:ids id] prefix)))
   empty-tables
   (map-indexed vector stacks)))

(defn- marker-rows [strings markers]
  (reduce (fn [[strings rows] {:keys [name start end layer data]}]
            (let [[strings i] (intern-in strings name)
                  instant?    (== (double start) (double end))]
              [strings
               (conj rows
                     [i start (when-not instant? end) (if instant? 0 1) layer
                      (assoc (or data {}) :type name)])]))
          [strings []]
          markers))

(defn- thread->gecko
  [{:keys [pid]} process tid {:keys [name stacks samples markers]}]
  (let [tbl             (tables stacks)
        ids             (:ids tbl)
        frames          (second (:frames tbl))
        stack-rows      (second (:stacks tbl))
        [strings mrows] (marker-rows (:strings tbl) markers)]
    {:name (or name (str "tid-" tid))
     :registerTime 0
     :unregisterTime nil
     :processType "default"
     :processName process
     :pid (str pid)
     :tid tid
     :isMainThread false
     :stringTable (second strings)
     :frameTable
     {:schema {:location 0 :relevantForJS 1 :innerWindowID 2 :implementation 3
               :line 4 :column 5 :category 6 :subcategory 7}
      :data (mapv (fn [[si line cat]] [si false nil nil line nil cat 0]) frames)}
     :stackTable
     {:schema {:frame 0 :prefix 1}
      :data (mapv (fn [[prefix frame]] [frame prefix]) stack-rows)}
     :samples
     {:schema {:stack 0 :time 1 :eventDelay 2 :threadCPUDelta 3}
      :data (mapv (fn [[sid t cpu]] [(when sid (ids sid)) t 0 cpu]) samples)}
     :markers
     {:schema {:name 0 :startTime 1 :endTime 2 :phase 3 :category 4 :data 5}
      :data mrows}}))

(defn- by-tid [markers]
  (reduce (fn [m mk] (update m (:tid mk) (fnil conj []) mk)) {} markers))

(defn- tracks [threads markers]
  (let [mk (by-tid markers)]
    (->> (into (set (keys threads)) (keys mk))
         (map (fn [tid]
                [tid (assoc (get threads tid {:name nil :stacks [] :samples []})
                            :markers (get mk tid []))]))
         (sort-by (fn [[_ t]] [(- (count (:samples t))) (- (count (:markers t)))])))))

(defn profile
  ([snapshot] (profile snapshot nil))
  ([{:keys [threads markers interval-ms started-epoch ticks dropped host]}
    {:keys [label process processes]}]
   {:meta {:version gecko-version
           :interval (or interval-ms prof/default-interval-ms)
           :startTime (or started-epoch (System/currentTimeMillis))
           :shutdownTime nil
           :stackwalk 0
           :debug false
           :toolkit "jvm"
           :updateChannel "release"
           :extensions {:schema {:id 0 :name 1 :baseURL 2} :data []}
           :product (or label "sutra backend")
           :processType 0
           :platform "JVM"
           :oscpu (:os host)
           :abi (:abi host)
           :misc (:jvm host)
           :categories (mapv (fn [{:keys [name color]}]
                               {:name name :color color :subcategories ["Other"]})
                             obs/layers)
           :markerSchema marker-schema
           :sampleUnits {:time "ms" :eventDelay "ms" :threadCPUDelta "ns"}
           :sutra {:ticks ticks :dropped-markers dropped :cpus (:cpus host)}}
    :libs []
    :pages []
    :processes (vec processes)
    :threads (mapv (fn [[tid t]] (thread->gecko host (or process "sutra backend") tid t))
                   (tracks threads markers))}))
