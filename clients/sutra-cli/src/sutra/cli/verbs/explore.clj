(ns sutra.cli.verbs.explore
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [live-name-index type-name-index]]
            [sutra.cli.out :refer [emit! emit-rows! mark render say! show-lines warn!]]
            [sutra.cli.session :as session]
            [sutra.i18n.core :as i18n]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.cli.view :refer
             [kw-name node-label objects->rows]]))

(defn- link-type-name-index [] (live-name-index :link-types :link-type-name))

(defmethod step! :explore [ctx [_ {:keys [text type]}]]
  (let [body (req! "explore" (api/get! :instances-search {:q text :type type}))]
    (emit-rows! ctx (objects->rows (:objects body) (type-name-index))
                (str "no objects match " (pr-str text)))))

(defn- seed-types
  [ctx ids types]
  (if (seq types)
    (vec types)
    (let [ids (set (map str ids))]
      (into [] (comp (filter #(ids (:id %))) (keep :type) (distinct))
            (some-> (:session ctx) session/rows)))))

(defmethod step! :around [ctx [_ {:keys [ids types]}]]
  (let [body (req! "around"
                   (api/post! :instances-around
                              {:ids (vec ids) :types (seed-types ctx ids types)}))]
    (emit-rows! ctx (objects->rows (:objects body) (type-name-index))
                "no neighbours")))

(def ^:private link-list-default-limit 200)

(defmethod step! :links [ctx [_ {:keys [type from limit all]}]]
  (let [want  (when-not all (or limit link-list-default-limit))
        links (req! "links" (api/list-links (cond-> {} type (assoc :type type)
                                                    from (assoc :from from)
                                                    want (assoc :limit (inc want)))))
        more? (and want (> (count links) want))
        names (link-type-name-index)
        rows  (vec (for [l (cond->> links want (take want))
                         :let [lt (get names (:link-type-ref l) (str (:link-type-ref l)))]]
                     {:id      (str (:id l))
                      :type    lt
                      :from-id (:from-id l)
                      :to-id   (:to-id l)}))]
    (emit-rows! ctx rows "no links")
    (when more?
      (warn! (mark :yellow "…" (str "bounded at " want
                                    " — `--limit N`, `--type`/`--from`, or `--all`"))))
    {:kind :objects :rows rows}))

(defmethod step! :dependents [ctx [_ {:keys [type]}]]
  (let [me  (clojure.core/name type)
        dep (req! (str "dependents " me) (api/get! :lineage-dependents {:type me}))]
    (if (:unknown? dep)
      (say! ctx (mark :yellow "⏸" (str "no object-type " (pr-str me)
                                       " — try `describe` for the list")))
      (emit! ctx dep))))

(defn- io-labels [t]
  (-> t (update :inputs (partial mapv node-label)) (update :outputs (partial mapv node-label))))

(defmethod step! :lineage [ctx [_ {:keys [node]}]]
  (let [g (req! "lineage" (api/get! :lineage-graph {:node node}))]
    (emit! ctx (cond-> g (:transform g) io-labels) ::lineage))
  nil)

(defmethod render ::lineage [_ v]
  (show-lines (cond-> v (:transforms v) (update :transforms (partial mapv io-labels)))))

(def ^:private fact-glyph
  {"dataset" "d" "object-type" "o" "link-type" "lt" "datasource" "ds" "transform" "t"
   "action" "act" "application" "app" "function" "fn" "eval-suite" "eval"
   "automation" "auto" "export" "exp" "interface" "i" "language-model" "llm"})

(defn- id-label [id]
  (let [[k n] (str/split (str id) #"/" 2)] (str (get fact-glyph k k) ":" n)))

(def ^:private fact-fields
  [:declared :triggers :last-run :last-success :last-failure :model :external-systems
   :source-tables
   :declared-not-called :inputs :outputs :readers :read-via-link-ends])

(def ^:private node-brief
  [:kind :name :build-status :freshness :rows :health :issues :sync-names :defines :backed-by
   :trashed?])

(defn- wanted-fields [fields]
  (into #{} (comp (map str/trim) (remove str/blank?) (map keyword)) (str/split (str fields) #",")))

(defn- trim-transform [keep-ks t]
  (cond-> (select-keys t keep-ks)
    (:dead? t)                (assoc :dead true)
    (:declared-not-called t) (assoc :declared-not-called (:declared-not-called t))))

(defn trim-facts
  [facts fields]
  (let [ws  (wanted-fields fields)
        all (contains? ws :all)]
    (-> facts
        (update :transforms
                (partial mapv (partial trim-transform
                                       (into [:transform :status :freshness]
                                             (filter (if all (constantly true) ws))
                                             fact-fields))))
        (update :nodes #(if all % (update-vals % (fn [n] (select-keys n (into node-brief ws)))))))))

(defn- none-shown
  [beyond t side]
  (let [n (or (some #(when (= t (:transform %)) (get % side)) beyond) 0)]
    (if (pos? n) (str "…" n " outside scope") "∅")))

(defn- edge-lines [{:keys [edges transforms beyond]}]
  (let [t-ids (set (map #(str "transform/" (:transform %)) transforms))
        ins   (group-by :to (filter #(= "input" (name (:rel %))) edges))
        outs  (group-by :from (filter #(= "output" (name (:rel %))) edges))
        label #(if (str/includes? % "/") (id-label %) %)
        io    (for [t (sort (into t-ids (concat (keys ins) (keys outs))))
                    :when (str/starts-with? t "transform/")
                    :let [is (or (seq (map :from (get ins t))) [(none-shown beyond t :in)])
                          os (or (seq (map :to (get outs t))) [(none-shown beyond t :out)])]
                    i is o os]
                (str "  " (label i) " ─[" (subs t 10) "]→ " (label o)))
        other (for [e edges :when (not (#{"input" "output"} (name (:rel e))))]
                (str "  " (id-label (:from e)) " ─" (name (:rel e)) "→ " (id-label (:to e))))]
    (concat io (sort other))))

(defn- fact-value [v]
  (cond (map? v)        (str/join " " (for [[k x] v] (str (name k) "=" (fact-value x))))
        (sequential? v) (if (empty? v) "—" (str/join ", " (map fact-value v)))
        (and (string? v) (str/includes? v "/")) (id-label v)
        :else (str v)))

(defn- csv [xs] (str/join "," xs))

(defn- path-line [{:keys [from to found? direction nodes]}]
  (if found?
    (str "path (" direction "): " (str/join " → " (map id-label nodes)))
    (str "no data path between " (id-label from) " and " (id-label to)
         " (input, output, backs)")))

(def ^:private node-marks
  [[:build-status identity]
   [:freshness    #(when (not= "up-to-date" %) (str "out-of-date: " %))]
   [:rows         #(str % " rows")]
   [:health       #(when-let [s (:status %)] (str "health " (kw-name s)))]
   [:issues       #(when (pos? %) (str "! " % " issues"))]
   [:schedules    #(when (pos? %) (str % " schedules"))]
   [:paused-schedules #(when (pos? %) (str % " paused"))]
   [:sync-names   #(str "⇄ " (csv %))]
   [:defines      #(str "defines " (csv %))]
   [:backed-by    #(str "backed by " (csv %))]
   [:resource     identity]
   [:storage      #(when (not= "sutra" %) (str "storage " %))]
   [:trashed?     #(when % "TRASHED")]])

(defn- node-line [n]
  (let [marks (keep (fn [[k f]] (some-> (get n k) f)) node-marks)
        more  (apply dissoc n :kind :name :project (map first node-marks))]
    (cons (str "  " (id-label (str (:kind n) "/" (:name n)))
               (when (seq marks) (str "  " (str/join " · " marks))))
          (for [[k v] (sort-by key more)]
            (str "      " (name k) ": " (fact-value v))))))

(defn- fresh-line [{:keys [node freshness logic changed behind]}]
  (str "  " (id-label node) "  " freshness (when logic "  (logic)")
       (when (seq changed) (str "  changed: " (str/join ", " (map id-label changed))))
       (when (seq behind) (str "  behind: " (str/join ", " (map id-label behind))))))

(defn- histogram-lines [h]
  (for [[facet bs] h]
    (cons (str "by " (name facet) ":")
          (for [{:keys [value count nodes]} bs]
            (str "  " value "  " count "  " (str/join ", " (map id-label (take 8 nodes)))
                 (when (> count 8) " …"))))))

(defn- schedule-line [{:keys [schedule paused when targets scope latest-run last-fired full]}]
  (str "  " schedule (when paused "  PAUSED") "  " when
       "  → " (if (seq targets) (csv targets) "whole ontology")
       (when scope (str "  scope " scope)) (when full "  full")
       (when latest-run (str "  latest " (:status latest-run) " " (or (:at latest-run) "")))
       (when (and last-fired (not latest-run)) (str "  fired " last-fired))))

(defmethod render ::facts [_ {:keys [nodes edges transforms dead stale blocked diff-live
                                     no-outputs coverage out-of-date modified histogram schedules]
                              :as f}]
  [(str (count nodes) " nodes · " (count edges) " edges · " (count transforms) " transforms"
        (when (seq (:anchors f)) (str " · from " (str/join ", " (map id-label (:anchors f))))))
   (when (:path f) (path-line (:path f)))
   (when (:column-note f) (str "column: " (:column-note f)))
   (when (seq edges) "edges:") (edge-lines f)
   (let [others (remove #(= "transform" (:kind %)) (vals nodes))]
     (when (some #(seq (dissoc % :kind :name :project)) others)
       (cons "nodes:" (mapcat node-line others))))
   (when (seq transforms) "transforms:")
   (for [t transforms]
     (cons (str "  " (:transform t) "  " (:status t)
                (when-let [w (:freshness t)] (when (not= "up-to-date" w) (str "  out-of-date: " w)))
                (when (:dead t) "  DEAD"))
           (for [k fact-fields :when (contains? t k)]
             (str "      " (name k) ": " (fact-value (get t k))))))
   (when dead
     (cons (str "no declared reader — review, not proof of disuse (" (count dead) "):")
           (for [d dead]
             (str "  " (:transform d) " → " (str/join ", " (map id-label (:outputs d)))))))
   (when (seq no-outputs)
     (str "no outputs declared — not judged (" (count no-outputs) "): "
          (str/join ", " no-outputs)))
   (when coverage
     [(str "  counted as readers: " (str/join "; " (:counted coverage)))
      (str "  NOT visible to --dead: " (str/join "; " (:not-counted coverage)))])
   (when stale
     (cons (str "stale (" (count stale) "):")
           (for [s stale]
             (str "  " (:transform s) "  last success " (or (:last-success s) "never")
                  "  changed since: " (str/join ", " (map id-label (:changed-inputs s)))))))
   (when blocked
     (cons (str "blocked from deletion (" (count blocked) "):")
           (for [{:keys [node reasons]} blocked]
             (str "  " (id-label node) "  "
                  (str/join "; " (for [r reasons]
                                   (str (name (:reason r))
                                        (when (:by r) (str " by " (fact-value (:by r))))
                                        (when (:repository r) (str " " (:repository r))))))))))
   (when out-of-date
     (cons (str "out of date (" (count out-of-date) "):") (map fresh-line out-of-date)))
   (when modified
     (cons (str "modified on this branch (" (count modified) "):")
           (map #(str "  " %) modified)))
   (when histogram (histogram-lines histogram))
   (when schedules
     (cons (str "schedules (" (count schedules) "):") (map schedule-line schedules)))
   (when diff-live
     ["diff against live:"
      (str "  gone from live: " (fact-value (:gone diff-live)))
      (str "  live, not shown: " (fact-value (:live-not-shown diff-live)))
      (for [e (:edges-not-shown diff-live)]
        (str "    " (id-label (:from e)) " ─" (name (:rel e)) "→ " (id-label (:to e))))])])

(def ^:private facts-query
  [[:node :node identity] [:up :up str] [:down :down str] [:to :to identity]
   [:project :project identity]
   [:graph :graph identity] [:column :column identity] [:stale :stale identity]
   [:by :by identity] [:dead? :dead (constantly true)] [:blocked? :blocked (constantly true)]
   [:diff-live? :diff-live (constantly true)] [:out-of-date? :out-of-date (constantly true)]
   [:modified? :modified (constantly true)]
   [:schedules? :schedules (constantly true)]])

(defn- ->facts-query [args]
  (into {} (keep (fn [[k q f]] (when-some [v (get args k)] (when v [q (f v)])))) facts-query))

(defmethod step! :lineage-facts
  [ctx [_ {:keys [detail] :as args}]]
  (let [f (req! "lineage facts" (api/get! :lineage-facts (->facts-query args)))]
    (emit! ctx (trim-facts f detail) ::facts)
    nil))

(defmethod step! :schedule-candidates [ctx [_ {:keys [node]}]]
  (let [{:keys [target
                candidates]} (req! "candidates" (api/get! :schedule-candidates {:target node}))]
    (emit! ctx {:target (or target (kw-name node)) :candidates (mapv :name candidates)})
    nil))

(defmethod step! :schedule-create
  [ctx [_ {:keys [name target on-parent-update on-listener listen-folders scope from cron
                  every-ms trigger-mode full? retry-count retry-backoff abort-on-failure?
                  parameters run-setup-key]}]]
  (let [namen    (fn [s] (let [ps (->> (str/split (str s) #",")
                                       (map str/trim) (remove str/blank?) (mapv keyword))]
                           (when (seq ps) (if (= 1 (count ps)) (first ps) ps))))
        parents  (when (seq on-parent-update) (namen on-parent-update))
        lauscher (when (seq on-listener)
                   (cond-> {:kind :listener :of (keyword (str/trim on-listener))}
                     (seq listen-folders)
                     (assoc :folders (->> (str/split (str listen-folders) #",")
                                          (map str/trim) (remove str/blank?) vec))))
        ereignis (cond
                   (and parents lauscher) {:kind :any-of
                                           :triggers [{:kind :parent-updated :of parents}
                                                      lauscher]}
                   parents                {:kind :parent-updated :of parents}
                   :else                  lauscher)
        ms       (when every-ms (Long/parseLong (str every-ms)))
        conjoin? (and (= :all trigger-mode) ereignis (or (seq cron) ms))
        cadence  (cond-> {:kind :cadence}
                   (seq cron) (assoc :cron cron)
                   ms         (assoc :every-ms ms))
        sched    (cond-> {:name name}
                   (seq target) (assoc :target target)
                   full?        (assoc :full true)
                   retry-count  (assoc :retry-count (or (parse-long (str retry-count)) retry-count))
                   retry-backoff (assoc :retry-backoff retry-backoff)
                   abort-on-failure? (assoc :abort-on-failure true)
                   (seq parameters) (assoc :parameters
                                           (update-vals parameters
                                                        #(when-not (str/blank? (str %)) %)))
                   run-setup-key (assoc :run-setup-key run-setup-key)
                   scope        (assoc :scope scope)
                   (seq from)   (assoc :from from)
                   (and (seq cron) (not conjoin?)) (assoc :cron cron)
                   (and ms (not conjoin?))         (assoc :every-ms ms)
                   conjoin?     (assoc :trigger {:kind :all-of
                                                 :triggers [cadence ereignis]})
                   (and ereignis (not conjoin?))
                   (assoc :trigger ereignis))]
    (when (and (nil? ereignis) (not (seq cron)) (nil? ms))
      (warn! (mark :yellow "⚠" (i18n/t :cli/schedule-never-fires))))
    (when (and (= :all trigger-mode) (not conjoin?))
      (warn! (mark :yellow "⚠" (i18n/t :cli/trigger-mode-all-needs-both))))
    (when (and (#{:connecting :between} scope) (not (seq from)))
      (warn! (mark :yellow "⚠" (i18n/t :cli/scope-connecting-needs-from))))
    (req! (str "create schedule " name) (api/post! :schedules sched))
    (emit! ctx sched)
    nil))
