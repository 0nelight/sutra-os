(ns sutra.llm.wire
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.encoding :as enc]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(set! *warn-on-reflection* true)

(def default-wire :openai-chat)

(def openai-chat
  {:hosts       []
   :chat-path   "chat/completions"
   :embed-path  "embeddings"
   :models-path "models"
   :headers     (fn [key _body] (when key {"Authorization" (str "Bearer " key)}))
   :request     identity
   :response    identity})

(def anthropic-version "2023-06-01")
(def anthropic-max-tokens 16000)

(def ^:private fallback-models
  #{"claude-fable-5-1" "claude-opus-5-5" "claude-opus-5" "claude-sonnet-5-5"})

(def ^:private fallback-beta "server-side-fallback-2026-07-01")

(defn- text-of [content]
  (if (string? content)
    content
    (str/join "\n" (keep #(when (= "text" (get % "type")) (get % "text")) content))))

(defn- part->block [p]
  (case (get p "type")
    "text"      {"type" "text" "text" (str (get p "text"))}
    "image_url" (let [url (str (get-in p ["image_url" "url"]))]
                  (if-let [[_ mime data] (re-matches #"(?s)data:([^;]+);base64,(.*)" url)]
                    {"type" "image" "source" {"type" "base64" "media_type" mime "data" data}}
                    {"type" "image" "source" {"type" "url" "url" url}}))
    {"type" "text" "text" (json/write-str p)}))

(defn- content-blocks [c]
  (cond
    (nil? c)    []
    (string? c) (if (str/blank? c) [] [{"type" "text" "text" c}])
    :else       (mapv part->block c)))

(defn- arguments [a]
  (cond (map? a)    a
        (string? a) (let [v (enc/json-or-nil a)] (if (map? v) v {}))
        :else       {}))

(defn- assistant-blocks [m]
  (or (not-empty (get m "provider_blocks"))
      (let [bs (into (content-blocks (get m "content"))
                     (for [tc (get m "tool_calls")]
                       {"type" "tool_use" "id" (get tc "id") "name" (get-in tc ["function" "name"])
                        "input" (arguments (get-in tc ["function" "arguments"]))}))]
        (if (seq bs) bs [{"type" "text" "text" "…"}]))))

(defn- tool-results? [m]
  (and (= "user" (get m "role")) (vector? (get m "content"))
       (every? #(= "tool_result" (get % "type")) (get m "content"))))

(defn- conversation
  [msgs]
  (reduce (fn [acc m]
            (case (get m "role")
              "system"    acc
              "tool"      (let [blk  {"type" "tool_result" "tool_use_id" (get m "tool_call_id")
                                      "content" (text-of (get m "content"))}
                                last (peek acc)]
                            (if (and last (tool-results? last))
                              (conj (pop acc) (update last "content" conj blk))
                              (conj acc {"role" "user" "content" [blk]})))
              "assistant" (conj acc {"role" "assistant" "content" (assistant-blocks m)})
              (conj acc {"role" "user" "content" (content-blocks (get m "content"))})))
          []
          msgs))

(defn ->anthropic
  [req]
  (let [msgs   (get req "messages")
        system (->> msgs (filter #(= "system" (get % "role"))) (map #(text-of (get % "content")))
                    (remove str/blank?) (str/join "\n\n"))
        tools  (for [t (get req "tools")
                     :let [f (get t "function")]]
                 (cond-> {"name" (get f "name")
                          "input_schema" (or (get f "parameters")
                                             {"type" "object" "properties" {}})}
                   (get f "description") (assoc "description" (get f "description"))))
        model  (str (get req "model"))]
    (cond-> {"model"         model
             "max_tokens"    (or (get req "max_tokens") (get req "max_completion_tokens")
                                 anthropic-max-tokens)
             "messages"      (conversation msgs)
             "cache_control" {"type" "ephemeral"}}
      (seq system)                    (assoc "system" system)
      (seq tools)                     (assoc "tools" (vec tools))
      (= "none" (get req "tool_choice")) (assoc "tool_choice" {"type" "none"})
      (fallback-models model)         (assoc "fallbacks" "default"))))

(defn- finish-reason [stop]
  (case stop
    "tool_use"   "tool_calls"
    "max_tokens" "length"
    "refusal"    "content_filter"
    "stop"))

(defn <-anthropic
  [resp]
  (let [blocks (get resp "content")
        text   (str/join (keep #(when (= "text" (get % "type")) (get % "text")) blocks))
        calls  (vec (for [b blocks :when (= "tool_use" (get b "type"))]
                      {"id" (get b "id") "type" "function"
                       "function" {"name" (get b "name")
                                   "arguments" (json/write-str (or (get b "input") {}))}}))
        stop   (get resp "stop_reason")
        u      (get resp "usage")
        text   (if (and (= "refusal" stop) (str/blank? text))
                 (str "The model declined this request"
                      (when-let [c (get-in resp ["stop_details" "category"])] (str " (" c ")")) ".")
                 text)]
    {"id"      (get resp "id")
     "model"   (get resp "model")
     "choices" [{"index" 0
                 "finish_reason" (finish-reason stop)
                 "message" (cond-> {"role" "assistant" "content" (not-empty text)
                                    "provider_blocks" blocks}
                             (seq calls) (assoc "tool_calls" calls))}]
     "usage"   (let [read    (long (or (get u "cache_read_input_tokens") 0))
                     written (long (or (get u "cache_creation_input_tokens") 0))]
                 {"prompt_tokens"         (+ (long (or (get u "input_tokens") 0)) read written)
                  "completion_tokens"     (long (or (get u "output_tokens") 0))
                  "prompt_tokens_details" {"cached_tokens" read "cache_write_tokens" written}})}))

(def anthropic-messages
  {:hosts       ["api.anthropic.com"]
   :chat-path   "messages"
   :models-path "models"
   :headers     (fn [key body]
                  (cond-> {"anthropic-version" anthropic-version}
                    key                   (assoc "x-api-key" key)
                    (get body "fallbacks") (assoc "anthropic-beta" fallback-beta)))
   :request     ->anthropic
   :response    <-anthropic})

(def builtin {:openai-chat openai-chat :anthropic-messages anthropic-messages})

(defn- host-of [base-url]
  (let [^java.net.URI u (try (java.net.URI. (str base-url))
                             (catch java.net.URISyntaxException _ nil))]
    (some-> u .getHost str/lower-case)))

(defn local-endpoint?
  [base-url]
  (let [h (host-of base-url)]
    (boolean
     (and h (or (#{"localhost" "::1" "[::1]" "host.containers.internal" "host.docker.internal"} h)
                (str/ends-with? h ".local")
                (re-matches #"127\..*|10\..*|192\.168\..*|172\.(1[6-9]|2\d|3[01])\..*" h))))))

(defn wire-for
  [wires spec base-url]
  (let [named (some-> (or (:wire spec) (get-in spec [:config :wire])) name keyword)
        host  (host-of base-url)]
    (cond
      named (if-let [w (get wires named)]
              [named w]
              (refusal/refuse! :refusal.llm/unknown-wire
                               {:wire named}
                               {:known (str/join ", " (map name (sort (keys wires))))}))
      :else (or (some (fn [[id w]] (when (and host (some #(= host %) (:hosts w))) [id w])) wires)
                [default-wire (get wires default-wire)]))))

(defn url [base path]
  (let [b (str/replace (str base) #"/+$" "")]
    (if (str/ends-with? b (str "/" path)) b (str b "/" path))))

(defn request
  [wire base key payload]
  (let [body ((:request wire) (walk/stringify-keys payload))]
    {:url     (url base (:chat-path wire))
     :headers (merge {"Content-Type" "application/json"} ((:headers wire) key body))
     :body    (json/write-str body)}))

(defn embed-request
  [wire base key payload]
  (when-let [path (:embed-path wire)]
    (let [body (walk/stringify-keys payload)]
      {:url     (url base path)
       :headers (merge {"Content-Type" "application/json"} ((:headers wire) key body))
       :body    (json/write-str body)})))

(defn response
  [wire body]
  (if (= identity (:response wire))
    body
    (json/write-str ((:response wire) (json/read-str body)))))
