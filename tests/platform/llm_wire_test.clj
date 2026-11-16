(ns ^:unit platform.llm-wire-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [sutra.llm.wire :as wire]))

(def ^:private chat
  {:model "claude-opus-5-5" :temperature 0.2 :tool_choice "required"
   :messages [{:role "system" :content "S"}
              {:role "user" :content [{:type "text" :text "q"}
                                      {:type "image_url"
                                       :image_url {:url "data:image/png;base64,QUJD"}}]}
              {:role "assistant" :content nil
               :tool_calls [{:id "t1" :type "function" :function {:name "a" :arguments "{\"x\":1}"}}
                            {:id "t2" :type "function" :function {:name "b" :arguments "{}"}}]}
              {:role "tool" :tool_call_id "t1" :content "r1"}
              {:role "tool" :tool_call_id "t2" :content "r2"}]
   :tools [{:type "function" :function {:name "a" :description "d" :parameters {:type "object"}}}]})

(deftest anthropic-request
  (let [{:keys [url headers
                body]} (wire/request wire/anthropic-messages "https://api.anthropic.com/v1/" "k"
                                     chat)
        b (json/read-str body)]
    (is (= "https://api.anthropic.com/v1/messages" url))
    (is (= "k" (get headers "x-api-key")))
    (is (= wire/anthropic-version (get headers "anthropic-version")))
    (is (nil? (get headers "Authorization")))
    (is (= {"type" "ephemeral"} (get b "cache_control")) "the prompt is cached for the next turn")
    (testing "system lifted out, sampling and forced tool choice dropped"
      (is (= "S" (get b "system")))
      (is (not (contains? b "temperature")))
      (is (not (contains? b "tool_choice")))
      (is (pos? (get b "max_tokens"))))
    (testing "tools as input_schema"
      (is (= [{"name" "a" "description" "d" "input_schema" {"type" "object"}}] (get b "tools"))))
    (testing "turns: image as base64 block, tool calls as tool_use, all results in one user turn"
      (let [[u a r] (get b "messages")]
        (is (= {"type" "image" "source" {"type" "base64" "media_type" "image/png" "data" "QUJD"}}
               (second (get u "content"))))
        (is (= [{"type" "tool_use" "id" "t1" "name" "a" "input" {"x" 1}}
                {"type" "tool_use" "id" "t2" "name" "b" "input" {}}]
               (get a "content")))
        (is (= "user" (get r "role")))
        (is (= ["t1" "t2"] (mapv #(get % "tool_use_id") (get r "content"))))
        (is (= 3 (count (get b "messages"))))))))

(deftest anthropic-response
  (let [out (json/read-str
             (wire/response wire/anthropic-messages
                            (json/write-str {:id "m" :model "claude-opus-5-5"
                                             :stop_reason "tool_use"
                                             :content [{:type "thinking" :thinking ""}
                                                       {:type "text" :text "let me look"}
                                                       {:type "tool_use" :id "x" :name "a"
                                                        :input {:q 1}}]
                                             :usage {:input_tokens 10 :cache_read_input_tokens 5
                                                     :output_tokens 3}})))
        msg (get-in out ["choices" 0 "message"])]
    (is (= "tool_calls" (get-in out ["choices" 0 "finish_reason"])))
    (is (= "let me look" (get msg "content")))
    (is (= [{"id" "x" "type" "function" "function" {"name" "a" "arguments" "{\"q\":1}"}}]
           (get msg "tool_calls")))
    (is (= 3 (count (get msg "provider_blocks"))) "the provider's own content rides along")
    (is (= {"prompt_tokens" 15 "completion_tokens" 3
            "prompt_tokens_details" {"cached_tokens" 5 "cache_write_tokens" 0}}
           (get out "usage")) "cache reads count as input, and say how many were cached"))
  (testing "a turn sent again carries its provider blocks verbatim"
    (let [blocks [{"type" "thinking" "thinking" "" "signature" "s"} {"type" "text" "text" "hi"}]
          b (json/read-str (:body (wire/request wire/anthropic-messages
                                                "https://api.anthropic.com/v1" "k"
                                                {:model "m" :messages [{:role "user" :content "q"}
                                                                       {:role "assistant"
                                                                        :content "hi"
                                                                        :provider_blocks blocks}
                                                                       {:role "user"
                                                                        :content "more"}]})))]
      (is (= blocks (get-in b ["messages" 1 "content"])))))
  (testing "a refusal says so instead of answering nothing"
    (let [out (json/read-str (wire/response wire/anthropic-messages
                                            (json/write-str {:content [] :stop_reason "refusal"
                                                             :stop_details {:category "cyber"}})))]
      (is (re-find #"declined.*cyber" (get-in out ["choices" 0 "message" "content"]))))))

(deftest choosing-a-wire
  (is (= :anthropic-messages (first (wire/wire-for wire/builtin {} "https://api.anthropic.com/v1")))
      "by host")
  (is (= :openai-chat (first (wire/wire-for wire/builtin {} "https://api.x.ai/v1"))) "the default")
  (is (= :anthropic-messages
         (first (wire/wire-for wire/builtin {:wire :anthropic-messages} "https://proxy.local/v1")))
      "named on the source (or by the model's :provider)")
  (is (thrown? clojure.lang.ExceptionInfo (wire/wire-for wire/builtin {:wire :nope} "https://x")))
  (testing "openai-chat passes the request through"
    (let [{:keys [url headers
                  body]} (wire/request wire/openai-chat "http://127.0.0.1:11434/v1" nil
                                       {:model "m"})]
      (is (= "http://127.0.0.1:11434/v1/chat/completions" url))
      (is (nil? (get headers "Authorization")) "no key, no header")
      (is (= {"model" "m"} (json/read-str body))))))

(deftest local-endpoints
  (is (wire/local-endpoint? "http://127.0.0.1:11434/v1"))
  (is (wire/local-endpoint? "http://localhost:8000"))
  (is (wire/local-endpoint? "http://192.168.1.20:11434"))
  (is (wire/local-endpoint? "http://gpu-box.local:8080/v1"))
  (is (not (wire/local-endpoint? "https://api.x.ai/v1")))
  (is (not (wire/local-endpoint? "https://172.32.0.1/v1"))))
