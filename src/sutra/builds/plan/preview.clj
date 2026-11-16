(ns sutra.builds.plan.preview
  (:require [sutra.catalog.values :as values]
            [sutra.catalog.llm-models :as llm-models]
            [clojure.data.json :as json]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.plan :as plan]
            [sutra.builds.run :as runs]
            [sutra.parser.params :as params]
            [sutra.types.languages :as langs]
            [sutra.kit.noted :as noted]))

(set! *warn-on-reflection* true)

(defn- cap-preview-input-files!
  [manifest limit]
  (into []
        (keep (fn [[alias {:keys [path kind rows]}]]
                (when (and path (nil? rows) (not= :language-model kind))
                  (try
                    (let [rows (json/read-str (slurp path))]
                      (when (and (sequential? rows) (> (count rows) limit))
                        (spit path (json/write-str (vec (take limit rows))))
                        {:input alias :kept limit :total (count rows)}))
                    (catch Exception e (noted/note "preview: cap input sample" e) nil)))))
        manifest))

(defn- cap-preview-compute
  [resolved limit]
  (let [file-caps (cap-preview-input-files! (:inputs-manifest resolved) limit)
        row-caps  (for [[alias in] (concat (:inputs resolved) (:inputs-manifest resolved))
                        :let  [n (count (:rows in))]
                        :when (> n limit)]
                    {:input alias :kept limit :total n})
        cap-rows  (fn [ins]
                    (update-vals ins
                                 #(cond-> % (seq (:rows %))
                                          (update :rows (comp vec (partial take limit))))))]
    (cond-> (-> resolved
                (update :inputs cap-rows)
                (assoc ::inputs/caps (into (vec file-caps) row-caps)))
      (:inputs-manifest resolved) (update :inputs-manifest cap-rows))))

(defn- preview-input-sample
  [compute limit sample]
  (cond
    sample {:rows sample :why :requested}
    (some llm-models/language-model-input? (vals (:inputs compute)))
    {:rows (or limit 50) :why :language-model}))

(defn- preview-code
  [{:keys [resolve run schema]} compute limit sample output]
  (let [in-sample-why (preview-input-sample compute limit sample)
        in-sample     (:rows in-sample-why)]
    (try
      (let [resolved (cond-> (resolve compute)
                       in-sample (cap-preview-compute in-sample))
            rows     (inputs/select-output (run resolved) output)]
        {:rows (vec (take limit rows)) :count (count rows)
         :language (:language compute) :error nil
         :input-sample in-sample-why
         :input-caps (::inputs/caps resolved [])})
      (catch InterruptedException e (throw e))
      (catch Exception e
        {:rows [] :count 0 :language (:language compute) :input-caps []
         :input-sample in-sample-why
         :error (runs/describe-throwable schema e)}))))

(defn- preview-sql
  [ctx compute limit output]
  (try
    (let [rows (vec (inputs/select-output (inputs/run-compute! ctx compute nil) output))]
      {:rows (vec (take limit rows)) :count (count rows) :language (:language compute) :error nil})
    (catch InterruptedException e (throw e))
    (catch Exception e
      {:rows [] :count 0 :language (:language compute)
       :error (or (.getMessage e) (str (type e)))})))

(defn preview-spec!
  ([store schema t transform-name] (preview-spec! store schema t transform-name nil))
  ([store schema t transform-name {:keys [limit sample params output] :or {limit 50}}]
   (let [t    (params/with-overrides t params)
         lang (:language t)
         ctx  {:store store :schema schema :datasources (values/sources->datasources schema)
               :mat-cache (atom {})}
         nm   (or (:transform-name t) transform-name)]
     (cond
       (langs/code-language? lang)
       (preview-code {:schema  schema
                      :resolve #(inputs/resolve-code-compute ctx %)
                      :run     #(inputs/run-resolved-compute!
                                 {:schema schema :code-env (inputs/code-env store)} %)}
                     (plan/compute t nm :code-run) limit sample output)
       (langs/query-language? lang) (preview-sql ctx (plan/compute t nm :sql-run) limit output)
       :else
       {:error    (str "preview-transform supports " (pr-str langs/compute-language-names)
                       " transforms; this :" (name (or lang :unknown))
                       " transform's preview is not wired — "
                       "use `run-import --only <object-type> --trace` to inspect it")
        :language lang :rows [] :count 0}))))

(defn preview-transform!
  ([store schema transform-name] (preview-transform! store schema transform-name nil))
  ([store schema transform-name opts]
   (let [[t-kw t] (inputs/transform-entry schema transform-name)]
     (if (nil? t)
       {:error (str "no transform named " transform-name) :rows [] :count 0}
       (preview-spec! store schema t t-kw opts)))))
