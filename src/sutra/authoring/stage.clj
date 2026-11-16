(ns sutra.authoring.stage
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.read-only :as read-only]
            [sutra.store.world :as world]
            [sutra.actions.api :as actions]
            [sutra.store.seat :as seat]
            [sutra.store.kept :as kept]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.catalog.problems :as problems]
            [sutra.types.world-rules :as rules]
            [sutra.repos.api :as repos]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.resolve :as res]
            [sutra.types.forms :as forms]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]
            [sutra.store.access.scope :as ctx]))

(set! *warn-on-reflection* true)

(defn- new-problems
  [before after]
  (not-empty
   (into {}
         (keep (fn [[k v]]
                 (let [old (get before k)]
                   (cond
                     (nil? old) [k v]
                     (and (sequential? v) (sequential? old))
                     (let [ident   #(if (and (map? %) (:rule %) (contains? % :entity))
                                      (problems/lint-key %) %)
                           old-set (into #{} (map ident) old)
                           added   (vec (remove (comp old-set ident) v))]
                       (when (seq added) [k added]))
                     (= v old) nil
                     :else [k v]))))
         after)))

(defn- new-warnings
  [before after]
  (let [old (into #{} (map (juxt :rule :entity :message)) before)]
    (not-empty
     (into []
           (remove #(old ((juxt :rule :entity :message) %)))
           after))))

(defn- lint-failed
  [e]
  [{:severity :warn :rule :lint/unavailable
    :message  (str "lint could not run: " (or (ex-message e) (.getName (class e))))}])

(defn staged-lint
  [read]
  (actions/of-staging-schema read ::lint #(lint/run-lint % (spec/vocabulary))))

(defn- phase!
  [acc k f]
  (let [t0 (System/nanoTime)
        v  (f)]
    (swap! acc update k (fnil + 0) (/ (- (System/nanoTime) t0) 1e6))
    v))

(defn- log-install-phases!
  [t-start phases preview? tally]
  (let [total (/ (- (System/nanoTime) t-start) 1e6)
        p     @phases
        named (reduce + 0 (vals p))]
    (println (format (str "[install] %s: total %.0fms · grammar %.0f · references %.0f"
                          " · stage %.0f (%s rows) · kernel %.0f (before %.0f · after %.0f)"
                          " · basis %.0f · lint %.0f (advisories %.0f) · rest %.0f")
                     (if preview? "preview" "stage") total
                     (:grammar p 0.0) (:references p 0.0)
                     (:stage p 0.0) (or (some-> (:staged tally) count) "?")
                     (+ (:kernel-before p 0.0) (:kernel-after p 0.0))
                     (:kernel-before p 0.0) (:kernel-after p 0.0)
                     (:basis p 0.0)
                     (+ (:lint-before p 0.0) (:lint-after p 0.0))
                     (:advice p 0.0)
                     (- total named)))
    tally))

(defn- live-object
  [current project-name rid o]
  (let [pk (rules/object-primary-key current rid)]
    (cond-> {:attributes (mapv (fn [{:keys [attribute-name attribute-type opts]}]
                                 (cond-> [attribute-name attribute-type]
                                   (:name opts) (conj {:name (:name opts)})))
                               (:attribute-list o))}
      (seq (:match-keys o)) (assoc :match-keys (vec (:match-keys o)))
      (:title o) (assoc :title (:title o))
      pk (assoc :primary-key (keyword (name pk)))
      (:project o) (assoc :project (project-name (:project o))))))

(defn- live-objects
  ([current]
   (let [project-name (:project-name (forms/installed current))]
     (live/by-name (for [[rid o] (:object-types current)
                         :let [n (:object-type-name o)]
                         :when n]
                     [n rid (live-object current project-name rid o)]))))
  ([current names]
   (if-not names
     (live-objects current)
     (let [project-name (delay (:project-name (forms/installed current)))]
       (live/by-field current names :object-types :object-type-name
                      #(live-object current @project-name %1 %2)
                      (delay (live-objects current)))))))

(defn- live-value-type [v] (select-keys v [:type :constraints :format]))

(defn- live-value-types
  ([current]
   (live/by-name (for [[k v] (:value-types current)]
                   [(or (:value-type-name v) k) k (live-value-type v)])))
  ([current names]
   (if-not names
     (live-value-types current)
     (live/by-field current names :value-types :value-type-name
                    (fn [_ v] (live-value-type v))
                    (delay (live-value-types current)) {:own-key? true}))))

(defn- checked
  ([current ref-current params timed] (checked current ref-current params timed nil))
  ([current ref-current {:keys [schema repository] :as params} timed names]
   (if-not (map? schema)
     {:refusal [:refusal.authoring/install-needs-schema (dissoc params :schema)]}
     (let [lowered  (try (sugar/lower-schema-against schema
                                                     (live/as-lookup (live-objects current names))
                                                     (live/as-lookup
                                                      (live-value-types current names))
                                                     (boolean (seq (:value-types current))))
                         (catch clojure.lang.ExceptionInfo e e))
           repo-rid (when (some? repository) (repos/repository-rid current repository))]
       (or (when (instance? Throwable lowered)
             {:refusal [:refusal.authoring/install-not-lowerable (or (ex-data lowered) {})
                        {:why (ex-message lowered)}]})
           (when (and (some? repository) (nil? repo-rid))
             {:refusal [:refusal.authoring/install-no-repository {:repository repository}]})
           (when-let [errs (timed :grammar #(ent/explain-schema schema))]
             {:refusal [:refusal.authoring/install-unallowed-structure {:errors errs}]})
           (when-let [errs (timed :references
                                  #(try (res/explain lowered
                                                     (install-plan/live-ref-symbols ref-current))
                                        (catch Exception e
                                          [(str "reference check could not run: "
                                                (ex-message e))])))]
             {:refusal [:refusal.authoring/install-unresolved-references {:errors errs}]})
           {:lowered lowered :repo-rid repo-rid})))))

(defn- install-opts
  [{:keys [auto-ds strict?]} project current repo-rid scope]
  (cond-> {:auto-ds    (some-> auto-ds ->kw)
           :strict?    (boolean strict?)
           :provenance {:origin :install}
           :project    project}
    repo-rid (merge (repos/push-plan-opts repo-rid current))
    scope    (merge scope)))

(defn- preview-reads
  [cst lowered current opts timed]
  (let [read    (ctx/at cst (:read (actions/staging-view cst)))
        basis   (timed :basis #(try (actions/staging-schema read)
                                    (catch clojure.lang.ExceptionInfo _ ::unknown)))
        known?  (not= ::unknown basis)
        k       (when known? (kept/db read (:branch read)))
        retire  (when (:strict? opts)
                  (install/planned opts :retire
                                   #(install-plan/plan-retirements lowered current (merge opts %))))
        {:keys [stage] :as run}
        (timed :stage
               #(try (install/preview-install cst lowered current opts)
                     (catch clojure.lang.ExceptionInfo e
                       (let [d (ex-data e)]
                         (if (and retire (= :plan-problems (:sutra/gate d)))
                           {:tally (:tally d) :refused (vec (:problems d)) :planned (:planned d)}
                           (throw e))))))
        after   (if stage (actions/planned-schema stage) basis)
        warnings (future
                   (when known?
                     (timed :advice
                            #(try (kept/advisories k (:branch read) basis after)
                                  (catch Exception e
                                    {:before nil :after (lint-failed e)})))))
        {reasons-after  :after
         reasons-before :before}
        (when known? (kept/reasons k basis after))]
    (assoc (select-keys run [:tally :refused :planned])
           :retire          retire
           :staged          (if stage (actions/planned-rows stage) [])
           :reasons-before  (if known? reasons-before ::unknown)
           :reasons-after   reasons-after
           :warnings-before (:before @warnings)
           :warnings-after  (:after @warnings))))

(defn- preview-answer
  [{:keys [tally staged retire refused planned reasons-before reasons-after warnings-before
           warnings-after]}
   repo-rid]
  (let [advisories (new-warnings warnings-before warnings-after)]
    (cond-> (assoc tally :preview? true :staged staged)
      retire
      (assoc :strict-repository (or repo-rid :all)
             :retiring (:retiring retire)
             :orphaned (:orphaned retire))
      refused
      (assoc :refused? true
             :planned planned
             :problems {:plan (mapv (fn [p] {:name (some-> (:entity p) name) :error (:message p)})
                                    refused)})
      (and (seq reasons-after) (not= ::unknown reasons-before))
      (update :problems merge (new-problems reasons-before reasons-after))
      (seq advisories)
      (assoc :advisories advisories))))

(defn stage!
  [{:keys [store branch project] current :schema} {:keys [preview?] :as params} scope]
  (let [t-start (System/nanoTime)
        phases  (atom {})
        timed   (fn [k f] (phase! phases k f))
        cst     (ctx/at store branch)
        staged  (:read (actions/staging-view (world/at store branch)))
        ref-cur (if (= staged branch) current (seat/branch-schema (world/at store staged)))
        planned (install/on-index store branch)
        {:keys [refusal lowered repo-rid]}
        (planned :lowering #(checked current ref-cur params timed (:names %)))]
    (when-let [[code data params] refusal] (refusal/refuse! code data params))
    (log-install-phases!
     t-start phases preview?
     (let [opts (assoc (install-opts params project current repo-rid scope) :planned planned)]
       (if preview?
         (preview-answer (preview-reads (read-only/of cst) lowered current opts timed)
                         repo-rid)
         (install/stage-schema! cst lowered current opts))))))
