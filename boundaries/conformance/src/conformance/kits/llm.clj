(ns conformance.kits.llm
  (:require [clojure.string :as str]
            [conformance.http :as http]
            [conformance.json :as json]
            [conformance.report :as r]
            [conformance.schema :as schema]))

(def value-schema
  {"type" "object"
   "properties" {"city" {"type" "string"} "count" {"type" "integer"}}
   "required" ["city" "count"]
   "additionalProperties" false})

(defn chat-request [model]
  {"model" model
   "messages" [{"role" "user" "content" "Reply with the single word: ok"}]
   "max_tokens" 512})

(defn structured-request [model]
  {"model" model
   "messages" [{"role" "user"
                "content" "Paris has 3 airports. Call final_answer with city and count."}]
   "tools" [{"type" "function"
             "function" {"name" "final_answer"
                         "description" "Return the final answer. Call this exactly once."
                         "parameters" {"type" "object"
                                       "properties" {"value" value-schema}
                                       "required" ["value"]}}}]
   "tool_choice" "required"})

(def ^:private finish-reasons #{"stop" "length" "tool_calls" "content_filter"})

(defn- non-neg-int? [v] (and (integer? v) (not (neg? v))))

(defn completion-problem
  [answer]
  (let [c (first (get answer "choices"))
        m (get c "message")
        u (get answer "usage")]
    (cond
      (not (map? answer)) "the answer is not a JSON object"
      (not (seq (get answer "choices"))) "no choices"
      (not= "assistant" (get m "role")) (str "message role " (pr-str (get m "role")))
      (not (or (nil? (get m "content")) (string? (get m "content"))))
      "message content is neither a string nor null"
      (not (finish-reasons (get c "finish_reason")))
      (str "finish_reason " (pr-str (get c "finish_reason")))
      (not (and (non-neg-int? (get u "prompt_tokens")) (non-neg-int? (get u "completion_tokens"))))
      (str "usage without prompt_tokens/completion_tokens: " (pr-str u)))))

(defn final-answer-problem
  [answer]
  (let [calls (get-in answer ["choices" 0 "message" "tool_calls"])
        call  (first (filter #(= "final_answer" (get-in % ["function" "name"])) calls))
        args  (get-in call ["function" "arguments"])
        parsed (when (string? args) (json/parse-or-nil args))]
    (cond
      (empty? calls) "no tool_calls"
      (nil? call) (str "no call to final_answer among "
                       (mapv #(get-in % ["function" "name"]) calls))
      (not= "function" (get call "type")) (str "tool call type " (pr-str (get call "type")))
      (not (string? (get call "id"))) "the tool call has no id"
      (not (string? args)) "function.arguments is not a JSON string"
      (not (map? parsed)) "function.arguments does not parse to an object"
      :else (when-let [errs (seq (schema/errors value-schema (get parsed "value")))]
              (str "value: " (str/join "; " errs))))))

(defn- post [base key body]
  (let [resp (http/request {:method :post :timeout-s 120
                            :url (str (str/replace base #"/+$" "") "/chat/completions")
                            :headers (cond-> {"Content-Type" "application/json"}
                                       key (assoc "Authorization" (str "Bearer " key)))
                            :body (json/write body)})]
    (if (= 200 (:status resp))
      (json/parse-or-nil (:body resp))
      (throw (ex-info (str "HTTP " (:status resp) ": " (subs (str (:body resp)) 0
                                                              (min 200 (count (str (:body resp))))))
                      {})))))

(defn checks
  [{:keys [base model api-key chat-answer structured-answer]}]
  (let [chat (delay (or chat-answer (post base api-key (chat-request model))))
        st   (delay (or structured-answer (post base api-key (structured-request model))))]
    [(r/check "llm.chat" "a minimal chat answers as a chat completion"
              #(completion-problem @chat))
     (r/check "llm.chat-content" "the chat answer carries text"
              #(when-not (seq (str (get-in @chat ["choices" 0 "message" "content"])))
                 "empty content"))
     (r/check "llm.structured" "the structured-output request answers as a chat completion"
              #(completion-problem @st))
     (r/check "llm.structured-value" "the final_answer value follows its JSON Schema"
              #(final-answer-problem @st))]))
