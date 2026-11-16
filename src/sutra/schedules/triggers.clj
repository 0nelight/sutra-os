(ns sutra.schedules.triggers
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.grants :as grants]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.naming :as naming]
            [sutra.builds.api :as builds]
            [sutra.types.virtual :as vtypes]
            [sutra.schedules.listener-events :as listener-events]
            [sutra.foundation.cron :as cron])
  (:import (java.time LocalDateTime)
           (java.time.temporal ChronoUnit)))

(set! *warn-on-reflection* true)

(def compound-kinds
  #{:all-of :any-of})

(def evaluatable-leaf-kinds
  #{:cadence :parent-updated :listener})

(defn- kind-kw [t] (some-> (:kind t) name keyword))

(defn compound? [t] (contains? compound-kinds (kind-kw t)))

(defn leaves
  ([t] (leaves t []))
  ([t path]
   (cond
     (nil? t)      []
     (compound? t) (vec (mapcat (fn [i c] (leaves c (conj path i)))
                                (range) (:triggers t)))
     :else         [[path t]])))

(defn assert-evaluatable!
  [trigger]
  (when (some? trigger)
    (let [bad-kind (fn [k why]
                     (refusal/refuse! :refusal.schedules/trigger-not-evaluatable
                                      {:trigger trigger :kind k}
                                      {:why    why
                                       :leaves (pr-str (vec (sort evaluatable-leaf-kinds)))}))]
      (doseq [node (tree-seq compound? :triggers trigger)]
        (if (compound? node)
          (when-not (seq (:triggers node))
            (bad-kind (kind-kw node)
                      "names no :triggers — an empty :all-of fires every tick and an empty :any-of never fires"))
          (when-not (contains? evaluatable-leaf-kinds (kind-kw node))
            (bad-kind (kind-kw node)
                      "is not a trigger this runner can decide, so a schedule carrying it would arm and then never fire"))))))
  nil)

(defn- leaf-parents
  [leaf]
  (when (= :parent-updated (kind-kw leaf))
    (->> (let [of (:of leaf)] (if (sequential? of) of [of]))
         (remove nil?)
         (map keyword)
         distinct
         vec)))

(defn listener-of
  [leaf]
  (when (and (= :listener (kind-kw leaf)) (some? (:of leaf)))
    {:source (keyword (name (:of leaf))) :folders (vec (map str (:folders leaf)))}))

(defn listeners
  [schedules]
  (->> schedules
       (mapcat #(keep (comp listener-of second) (leaves (:trigger %))))
       distinct
       vec))

(defn assert-listenable!
  [schema trigger]
  (let [sources (into {} (for [[_ s] (:sources schema)] [(keyword (name (:source-name s))) s]))]
    (doseq [[_ leaf] (leaves trigger)
            :when (= :listener (kind-kw leaf))
            :let [{:keys [source]} (listener-of leaf)
                  spec (get sources source)]]
      (when-not (and spec (= "imap" (some-> (:type spec) name)))
        (if spec
          (refusal/refuse! :refusal.schedules/listener-not-imap {:trigger leaf}
                           {:source (name source) :type (name (:type spec))})
          (refusal/refuse! :refusal.schedules/listener-no-source {:trigger leaf}
                           {:of (:of leaf)}))))
    nil))

(defn assert-watchable!
  [schema trigger]
  (let [blind (->> (leaves trigger)
                   (mapcat (comp leaf-parents second))
                   (filter #(some-> (builds/spec-of schema %) vtypes/detected? not))
                   vec)]
    (when (seq blind)
      (refusal/refuse! :refusal.schedules/virtual-no-detect {:virtual blind}))
    nil))

(defn watched-parents
  [{:keys [trigger]}]
  (let [ps (->> (leaves trigger) (mapcat (comp leaf-parents second)) distinct vec)]
    (when (seq ps) ps)))

(defn- virtual-signature
  [store schema vds]
  (try (builds/signature (ctx/connect (grants/source-authority store)) schema vds)
       (catch InterruptedException e (throw e))
       (catch Exception e
         (binding [*out* *err*]
           (println (str "trigger: probe of virtual dataset " (:dataset-name vds) " failed — "
                         (ex-message e))))
         nil)))

(defn parent-signature
  [store schema nm]
  (if-let [ot (lookup/rid-of schema :object-types nm)]
    [:clock (sub/type-clock store ot)]
    (if-let [vds (builds/spec-of schema nm)]
      (virtual-signature store schema vds)
      (some->> (or (lookup/rid-of schema :datasets nm) (lookup/rid-of schema :transforms nm))
               naming/dataset-table
               (dsets/content-signature store)))))

(defn signature
  [store branch schema schedule]
  (let [evs   (for [[path leaf] (leaves (:trigger schedule))
                    :let [ps (leaf-parents leaf)
                          l  (listener-of leaf)]
                    :when (or (seq ps) l)]
                [path ps l])
        store (ctx/at store branch)]
    (when (seq evs)
      (into {}
            (map (fn [[path ps l]]
                   [path (if l
                           [(listener-events/signature (:source l))]
                           (mapv #(parent-signature store schema %) ps))]))
            evs))))

(defn time-leaf-due?
  [leaf last-fired ^LocalDateTime now]
  (let [ms (:every-ms leaf)]
    (cond
      (and ms (pos? ms))
      (if-let [lf (cron/->ldt last-fired (java.time.ZoneId/systemDefault))]
        (>= (.between ChronoUnit/MILLIS lf now) (long ms))
        true)

      (:cron leaf) (cron/due? (:cron leaf) last-fired now (java.time.ZoneId/systemDefault))
      :else        false)))

(defn satisfied?
  ([trigger truth] (satisfied? trigger truth []))
  ([trigger truth path]
   (cond
     (nil? trigger) false
     (compound? trigger)
     (let [kids (map-indexed (fn [i c] (satisfied? c truth (conj path i)))
                             (:triggers trigger))]
       (case (kind-kw trigger)
         :all-of (boolean (and (seq kids) (every? true? kids)))
         :any-of (boolean (some true? kids))))
     :else (true? (get truth path)))))
