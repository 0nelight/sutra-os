(ns ^:unit platform.conformance-kits-more-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [conformance.http :as http]
            [conformance.json :as json]
            [conformance.kits.git :as git]
            [conformance.kits.llm :as llm]
            [conformance.kits.platform-sdk :as sdk])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)))

(defn- status-of [checks id] (:status (first (filter #(= id (:id %)) checks))))

(defn- serve [f]
  (let [s (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext s "/"
                    (reify HttpHandler
                      (handle [_ ex]
                        (let [^HttpExchange ex ex
                              req {:path    (.getPath (.getRequestURI ex))
                                   :query   (.getQuery (.getRequestURI ex))
                                   :headers (into {} (for [[k vs] (.getRequestHeaders ex)]
                                                       [(str/lower-case k) (first vs)]))}
                              {:keys [status headers body]} (f req)
                              bs (.getBytes (str body) StandardCharsets/UTF_8)]
                          (doseq [[k v] headers] (.add (.getResponseHeaders ex) k v))
                          (.sendResponseHeaders ex status (if (seq bs) (count bs) -1))
                          (with-open [o (.getResponseBody ex)] (.write o bs))))))
    (.start s)
    s))

(defmacro ^:private with-server [[base f] & body]
  `(let [s# (serve ~f)
         ~base (str "http://127.0.0.1:" (.getPort (.getAddress s#)))]
     (try ~@body (finally (.stop s# 0)))))

(defn- pkt [s] (str (format "%04x" (+ 4 (count (.getBytes ^String s "UTF-8")))) s))

(def ^:private sha "0123456789abcdef0123456789abcdef01234567")

(def ^:private advertisement
  (str (pkt "# service=git-upload-pack\n") "0000"
       (pkt (str sha " HEAD\u0000multi_ack side-band-64k ofs-delta agent=x\n"))
       (pkt (str sha " refs/heads/main\n")) "0000"))

(deftest pkt-lines-are-read
  (is (nil? (git/advertisement-problem "git-upload-pack" advertisement)))
  (is (= #{"multi_ack" "side-band-64k" "ofs-delta" "agent=x"} (git/capabilities advertisement)))
  (is (some? (git/advertisement-problem "git-upload-pack" "hello")))
  (is (some? (git/advertisement-problem "git-upload-pack"
                                        (str (pkt "# service=git-receive-pack\n") "0000")))))

(defn- git-server [{:keys [token cache?]}]
  (fn [{:keys [path headers]}]
    (let [ok? (= (get headers "authorization") (http/basic-auth "conformance" token))]
      (cond
        (not (str/includes? path "/a/b.git/")) {:status 404}
        (not ok?) {:status 401 :headers {"WWW-Authenticate" "Basic realm=\"x\""}}
        :else {:status 200
               :headers (cond-> {"Content-Type" "application/x-git-upload-pack-advertisement"}
                          cache? (assoc "Cache-Control" "no-cache"))
               :body advertisement}))))

(deftest the-git-kit-judges-a-server
  (with-server [base (git-server {:token "t" :cache? true})]
    (let [cs (git/checks {:base base :repo "a/b" :token "t"})]
      (doseq [id ["git.enabled" "git.anonymous-401" "git.wrong-token-401"
                  "git.receive-pack-guarded" "git.advertisement" "git.no-cache"
                  "git.capabilities" "git.unknown-repo"]]
        (is (= :pass (status-of cs id)) id))
      (is (= :skip (status-of cs "git.protocol-v2")) "v0 fallback is allowed")
      (is (= :skip (status-of cs "git.push")) "read-only without --allow-push")))
  (testing "a cached advertisement and a 404 for anonymous fail"
    (with-server [base (git-server {:token "t" :cache? false})]
      (is (= :fail (status-of (git/checks {:base base :repo "a/b" :token "t"}) "git.no-cache")))
      (is (= :fail (status-of (git/checks {:base base :repo "x/y" :token "t"})
                              "git.enabled"))))))

(def ^:private sdk-text
  (str "ENDPOINTS = {\n"
       "    \"hello\": {\"path\": \"/api/hello\", \"methods\": [\"get\"], \"auth\": \"public\"},\n"
       "    \"thing\": {\"path\": \"/api/thing/{name}\", \"params\": [\"name\"], "
       "\"methods\": [\"delete\", \"get\"], \"auth\": \"user\"},\n"
       "}\n"))

(def ^:private served
  {"openapi" "3.1.0"
   "paths" {"/api/hello" {"get" {"security" []}}
            "/api/thing/{id}" {"get" {"security" [{"bearer" []}]}
                               "delete" {"security" [{"bearer" []}]}}}})

(deftest the-sdk-kit-holds-the-table-against-the-api
  (is (= {"hello" {:path "/api/hello" :methods #{"get"} :auth "public"}
          "thing" {:path "/api/thing/{name}" :methods #{"delete" "get"} :auth "user"}}
         (sdk/sdk-endpoints sdk-text)))
  (is (every? #(= :pass (:status %)) (sdk/checks {:openapi served :sdk-text sdk-text})))
  (testing "a missing method and a guarded public endpoint fail"
    (let [cs (sdk/checks {:openapi (-> served
                                       (update-in ["paths" "/api/thing/{id}"] dissoc "delete")
                                       (assoc-in ["paths" "/api/hello" "get" "security"]
                                                 [{"bearer" []}]))
                          :sdk-text sdk-text})]
      (is (= :fail (status-of cs "sdk.paths")))
      (is (= :fail (status-of cs "sdk.auth")))))
  (testing "against a served document"
    (with-server [base (fn [_] {:status 200 :headers {"Content-Type" "application/json"}
                                :body (json/write served)})]
      (is (every? #(= :pass (:status %)) (sdk/checks {:base base :sdk-text sdk-text}))))))

(def ^:private chat-answer
  {"id" "c1" "object" "chat.completion" "model" "m"
   "choices" [{"index" 0 "finish_reason" "stop"
               "message" {"role" "assistant" "content" "ok"}}]
   "usage" {"prompt_tokens" 12 "completion_tokens" 1 "total_tokens" 13}})

(defn- structured-answer [value]
  {"id" "c2" "model" "m"
   "choices" [{"index" 0 "finish_reason" "tool_calls"
               "message" {"role" "assistant" "content" nil
                          "tool_calls" [{"id" "call_1" "type" "function"
                                         "function" {"name" "final_answer"
                                                     "arguments" (json/write {"value" value})}}]}}]
   "usage" {"prompt_tokens" 40 "completion_tokens" 9}})

(deftest the-llm-kit-reads-the-canonical-wire
  (let [good (structured-answer {"city" "Paris" "count" 3})]
    (is (every? #(= :pass (:status %))
                (llm/checks {:chat-answer chat-answer :structured-answer good})))
    (testing "a value off its schema, a missing usage and a missing tool call fail"
      (is (some? (llm/final-answer-problem (structured-answer {"city" "Paris"}))))
      (is (some? (llm/final-answer-problem (structured-answer {"city" "P" "count" 3 "x" 1}))))
      (is (some? (llm/completion-problem (dissoc chat-answer "usage"))))
      (is (some? (llm/final-answer-problem chat-answer)))))
  (testing "against a provider"
    (with-server [base (fn [{:keys [path]}]
                         {:status 200 :headers {"Content-Type" "application/json"}
                          :body (json/write (if (= "/v1/chat/completions" path)
                                              (structured-answer {"city" "Paris" "count" 3})
                                              {}))})]
      (let [cs (llm/checks {:base (str base "/v1") :model "m"})]
        (is (= :pass (status-of cs "llm.structured-value")))))))
