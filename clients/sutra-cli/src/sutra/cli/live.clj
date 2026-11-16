(ns sutra.cli.live
  (:require [clojure.string :as str]
            [sutra.cli.errors :as errors :refer [ok? req!]]
            [sutra.cli.out :as out :refer [emit! mark paint say! show-lines]]
            [sutra.cli.view :refer [object-type-entry row-class]]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defn live-schema
  []
  (req! "schema" (api/get! :schema)))

(defn schema-sections
  [& sections]
  (req! "schema" (api/get! :schema-sections {:sections (str/join "," (map name sections))})))

(defn- optional
  [f]
  (let [r (errors/optional "read" f)] (when (ok? r) (:body r))))

(defn live-name-index
  [section field]
  (let [body (optional #(api/get! :schema-sections {:sections (name section)}))]
    (into {} (for [[rid spec] (get body section)]
               [rid (some-> (get spec field) name)]))))

(defn type-name-index [] (live-name-index :object-types :object-type-name))

(defn transform-name-index [] (live-name-index :transforms :transform-name))

(def backing* (memoize (fn [] (optional #(api/get! :schema-backing)))))

(def rider-kinds
  {:automations   {:noun "automation"   :call api/upsert-automation}
   :schedules     {:noun "schedule"     :call api/upsert-schedule   :target? true}
   :health-checks {:noun "health-check" :call api/upsert-health-check :target? true}})

(defn parse-param-flags
  [flags]
  (into {} (keep (fn [s]
                   (let [i (.indexOf (str s) "=")]
                     (when (pos? i)
                       [(keyword (subs (str s) 0 i)) (subs (str s) (inc i))])))
                 flags)))

(defn short-commit [c] (when c (let [s (str c)] (subs s 0 (min 10 (count s))))))

(defn commit-unless-staged!
  [ctx stage? done-msg]
  (if stage?
    (say! ctx (str "    " (paint :dim (i18n/t :cli/staged-only))))
    (do (req! "commit" (api/post! :staging))
        (say! ctx (mark :green "✓" done-msg)))))

(defn kompass-nodes! [] (:nodes (req! "kompass" (api/get! :kompass-nodes))))

(defn kompass-folder-named
  [nodes fname]
  (let [ms (filterv #(and (= :folder (:node-kind %)) (= fname (:name %))) nodes)]
    (when (> (count ms) 1)
      (throw (ex-info (str (count ms) " folders named " fname
                           " — ambiguous; rename one so the name is unique")
                      {:folder fname})))
    (first ms)))

(defn find-or-create-folder!
  [nodes fname]
  (or (:rid (kompass-folder-named nodes fname))
      (:rid (req! (str "folder " fname)
                  (api/post! :kompass-folders {:name fname :parent-rid nil})))))

(defn- staged-blast
  [staged]
  (when (seq staged)
    (when-let [body (errors/optional "impact of the staged change"
                                  #(schema-sections :object-types))]
      (let [otypes        (:object-types body)
            dependents-of (requiring-resolve 'sutra.parser.impact/dependents-of)]
        (->> staged
             (map row-class)
             distinct
             (keep (fn [nm]
                     (when (object-type-entry otypes (keyword nm))
                       (dependents-of body nm))))
             (filter #(pos? (:total %)))
             (sort-by (comp - :total))
             vec)))))

(defn render-blast!
  [ctx blast]
  (when (seq blast) (run! #(say! ctx %) (show-lines {:blast blast}))))

(defn check-report
  []
  (let [sp     (api/get! :schema-problems)
        body   (when (ok? sp) (:body sp))
        hc     (:results (optional #(api/post! :health-run-all)))
        sr     (optional #(api/get! :staging))
        staged (:staged sr)
        lint   (:findings (optional #(api/get! :schema-lint)))]
    {:valid?   (when body (boolean (:valid? body)))
     :problems (:problems body)
     :findings (:findings body)
     :health   (when hc {:pass (count (filter #(= "passed" (:status %)) hc))
                         :n    (count hc)})
     :staged   (count staged)
     :txns     (vec (:transactions sr))
     :blast    (staged-blast staged)
     :lint     (vec lint)}))

(defn print-landing!
  [ctx]
  (let [ov      (req! "overview" (api/get! :object-counts))
        appmap  (:applications (req! "apps"      (api/get! :applications)))
        projs   (:projects     (req! "projects"  (api/get! :projects)))
        ->n     (fn [x] (cond (keyword? x) (name x) (some? x) (str x) :else nil))
        by-proj (reduce (fn [m [nm spec]]
                          (update m (->n (:project spec)) (fnil conj []) (->n nm)))
                        {} appmap)
        pnames  (->> (concat (map #(->n (or (:project-name %) (:name %))) projs)
                             (keys by-proj))
                     (remove nil?) distinct sort)]
    (emit! ctx {:branch   (:branch ov)
                :counts   (:counts ov)
                :projects (vec (for [p pnames]
                                 {:project p :apps (vec (sort (get by-proj p)))}))})))
