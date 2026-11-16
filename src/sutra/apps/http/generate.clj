(ns sutra.apps.http.generate
  (:require [clojure.string :as str]
            [sutra.apps.http.session :refer [with-app]]
            [sutra.apps.ports :as ports]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.http.responses :refer [json-response]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [sutra.store.access.scope :as ctx]))

(set! *warn-on-reflection* true)

(def ^:private max-prompt-chars 100000)

(defn- generators
  [a]
  (filter #(= :ai-generated-content (some-> (:widget %) ->kw))
          (tree/nodes (dissoc (:layout a) :vars))))

(defn- default-model
  [schema]
  (some (fn [[k m]]
          (when (and (:default m) (not (:deprecated m)))
            (if (keyword? k) (subs (str k) 1) (str k))))
        (:llm-models schema)))

(defn- model-of
  [schema widgets wanted]
  (let [named (into #{} (keep #(some-> (:model %) str not-empty)) widgets)]
    (cond
      (and (seq (str wanted)) (named (str wanted))) (str wanted)
      (= 1 (count named))                           (first named)
      (empty? named)                                (default-model schema))))

(defn my-application-generate
  [req]
  (with-app req {:json? true :branch? true :preview? true}
    (fn [{:keys [schema branch a body store rid]}]
      (let [widgets (generators a)
            prompt  (str (:prompt body))
            model   (model-of schema widgets (:model body))
            temp    (:temperature body)]
        (cond
          (empty? widgets)
          (json-response 403 {:error "this application shows no generated content"})

          (str/blank? prompt)
          (json-response 400 {:error "a prompt is required"})

          (> (count prompt) max-prompt-chars)
          (json-response 400 {:error "the prompt is too long"})

          (nil? model)
          (json-response 400 {:error "no language model to use"})

          :else
          (try
            (let [{:keys [message]}
                  ((ports/llm-complete store)
                   {:store   (ctx/at store branch)
                    :source  (llm-models/source-of schema model)
                    :model   model
                    :request (cond-> {"model"    model
                                      "messages" [{"role" "user" "content" prompt}]}
                               (number? temp) (assoc "temperature" (max 0 (min 1 temp))))
                    :caller  (str "app:" (name rid))
                    :branch  branch
                    :schema  schema})]
              (json-response 200 {:text (:content message)}))
            (catch clojure.lang.ExceptionInfo e
              (json-response (or (:status (ex-data e)) 502) {:error (.getMessage e)}))))))))
