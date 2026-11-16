(ns sutra.evals.extraction
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.compute.api :as compute]
            [sutra.media.api :as media]
            [sutra.store.access.scope :as ctx]
            [sutra.store.storage.instance-config :as ic]))

(set! *warn-on-reflection* true)

(def ^:private code
  (str "from functions.api import function\n"
       "import sutra_extraction\n\n\n"
       "@function\n"
       "def document_intelligence(doc, model, cfg):\n"
       "    return [sutra_extraction.run(doc, model, cfg)]\n"))

(def ^:private timeout-ms 240000)

(def strategies #{"raw-text" "ocr" "layout-ocr" "vlm" "entities"})

(def ^:private model-strategies #{"vlm" "entities"})

(defn needs-model? [strategy] (contains? model-strategies (str strategy)))

(defn models
  [schema]
  (vec (sort (for [[k {:keys [source deprecated]}] (:llm-models schema)
                   :when (and source (not deprecated))]
               (if (keyword? k) (subs (str k) 1) (str k))))))

(defn layout-source?
  [schema {:keys [kind name]} {:keys [strategy]}]
  (and (= "entities" (str strategy)) (= "dataset" kind)
       (contains? (:datasets schema) (keyword name))))

(defn- run-function!
  [store schema entry model cfg]
  (let [lm  (when model (llm-models/with-source schema {:kind :language-model :model model}))
        ins (cond-> {:doc {:kind :media} :cfg {:kind :scalar}} lm (assoc :model lm))
        res (cond-> {:doc entry :cfg {:kind :scalar :value (json/write-str cfg)}}
              lm (assoc :model (media/language-model-entry schema lm)))
        out (compute/run-code! :python
                               {:transform-name :document-intelligence :language :python
                                :code code :inputs ins :timeout-ms timeout-ms}
                               (assoc (compute/prepare-inputs :python res nil)
                                      :blobs (delay (ctx/blobs))
                                      :lm-service (delay (ic/lm-service-config store))))]
    (first (:rows out))))

(defn- with-cost [schema model {:keys [tokens_in tokens_out] :as row}]
  (cond-> row
    (and model (or tokens_in tokens_out))
    (assoc :cost (llm-models/cost schema model tokens_in tokens_out))))

(defn extract!
  [store schema entry page {:keys [strategy model] :as cfg}]
  (let [model (when (needs-model? strategy) (not-empty (str model)))
        row   (run-function! store schema entry model
                             (-> (select-keys cfg [:strategy :mode :languages :prompt
                                                   :preprocess :max_tokens :entities :max_pages])
                                 (assoc :op "extract" :page page)))]
    (with-cost schema model
      (cond-> row
        (:blocks row)   (update :blocks json/read-str :key-fn keyword)
        (:entities row) (update :entities json/read-str :key-fn keyword)))))

(defn evaluate!
  [store schema entry page markdown model]
  (let [row (run-function! store schema entry model
                           {:op "evaluate" :page page :markdown (str markdown)})]
    (with-cost schema model (update row :scores json/read-str :key-fn keyword))))

(def output-placeholder "REPLACE_ME")

(defn- py-value
  [v]
  (json/write-str v :escape-unicode false :escape-slash false))

(defn- strategy-source [strategy]
  (str "{\n"
       (apply str (for [[k v] (sort-by (comp name key) strategy)]
                    (str "    " (py-value (name k)) ": " (py-value v) ",\n")))
       "}"))

(defn- decorator
  [source input]
  (str "@transform(\n"
       "    inputs={\"media_input\": MediaSetInput(" (py-value (:name source)) ")"
       (when input (str ",\n            \"extractor\": " input))
       "},\n"
       "    output=Output(kind=\"dataset\", dataset=\"" output-placeholder
       "\", primary_key=\"page_id\"),\n"
       "    incremental={\"read_mode\": \"current\", \"write_mode\": \"replace\"},\n"
       "    max_per_run=200,\n)\n"))

(defn- header [source]
  (str "\"\"\"Deployed from Document Intelligence: one row per page of " (:name source) ".\n"
       "Specify the output dataset below, commit and push. To change the strategy, change it\n"
       "there, compare it and deploy again.\"\"\"\n"))

(defn- traditional-code [{:keys [source strategy]}]
  (str (header source)
       "from sutra_extraction import create_extraction\n"
       "from sutra_transforms import MediaSetInput, Output, previous_output, transform\n\n"
       "STRATEGY = " (strategy-source strategy) "\n\n\n"
       (decorator source nil)
       "def extract(media_input):\n"
       "    return create_extraction(media_input, STRATEGY, previous=previous_output())\n"))

(defn- vision-code [{:keys [source strategy prompts-module]}]
  (let [{:keys [model preprocess prompt languages]} strategy
        layout    ({"layout" "no_overlay" "layout-tables" "crop_tables"} preprocess)
        input     (if layout
                    "VisionLLMLayoutDocumentsExtractorInput"
                    "VisionLLMDocumentsExtractorInput")
        prompt-as (if layout "system_prompt" "prompt")]
    (str (header source)
         "from sutra_extraction import " input "\n"
         "from sutra_transforms import MediaSetInput, Output, transform\n"
         (when prompt (str "\nfrom " prompts-module " import PROMPT\n"))
         "\nTHREAD_NUMBER = 20\n\n\n"
         (decorator source (str input "(" (py-value model) ")"))
         "def extract(media_input, extractor):\n"
         "    return extractor.create_extraction(\n"
         "        media_input,\n"
         (if layout
           (str "        include_layout_info=" (py-value layout) ",\n")
           (str "        with_ocr=" (if (= "ocr" preprocess) "True" "False") ",\n"))
         (when prompt (str "        " prompt-as "=PROMPT,\n"))
         (when (seq languages) (str "        languages=" (py-value languages) ",\n"))
         "        thread_number=THREAD_NUMBER,\n    )\n")))

(defn- entities-code [{:keys [source strategy prompts-module]}]
  (let [{:keys [model prompt entities]} strategy]
    (str (header source)
         "from sutra_entities import EntityExtractorInput, documents_of\n"
         "from sutra_transforms import Input, Output, transform\n"
         (when prompt (str "\nfrom " prompts-module " import PROMPT\n"))
         "\nENTITIES = " (py-value entities) "\n\n\n"
         "@transform(\n"
         "    inputs={\"pages\": Input(kind=\"dataset\", dataset=" (py-value (:name source)) "),\n"
         "            \"extractor\": EntityExtractorInput(" (py-value model) ")},\n"
         "    output=Output(kind=\"dataset\", dataset=\"" output-placeholder
         "\", primary_key=\"media_reference\"),\n"
         "    incremental={\"read_mode\": \"current\", \"write_mode\": \"replace\"},\n"
         "    max_per_run=200,\n)\n"
         "def extract(pages, extractor):\n"
         "    return extractor.create_entity_extraction(\n"
         "        documents_of(pages), ENTITIES,"
         (when prompt " prompt=PROMPT,") " id_column=\"media_reference\")\n")))

(defn- readme-section [name path]
  (str "\n\n## Document extraction: " name "\n\n"
       "Deployed from Document Intelligence into `" path "`.\n\n"
       "1. Specify your output dataset in the `Output` of its `@transform`.\n"
       "2. Commit and `git push` to this branch; the decorator is the declaration.\n"
       "3. Merge the branch and build the transform.\n\n"
       "The rows carry `strategy_id`: documents read by an earlier strategy keep their rows\n"
       "until the current one reaches them. Do not edit the prompt here: change it in\n"
       "Document Intelligence, compare the result and deploy again.\n"))

(defn deployment
  [{:keys [name strategy] :as d}]
  (let [stem    (str/replace name "-" "_")
        path    (str "transforms/" stem ".py")
        module  (str stem "_prompts")
        kind    (:strategy strategy)
        prompt  (when (needs-model? kind) (:prompt strategy))
        d       (assoc d :prompts-module module)]
    {:path   path
     :files  (cond-> {path (case kind
                             "vlm"      (vision-code d)
                             "entities" (entities-code d)
                             (traditional-code d))}
               prompt (assoc (str "transforms/" module ".py")
                             (str "PROMPT = " (py-value prompt) "\n")))
     :readme (readme-section name path)}))
