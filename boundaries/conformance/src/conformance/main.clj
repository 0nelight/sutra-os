(ns conformance.main
  "clojure -M:conformance <kit> [options] <target…>

   kits:
     http-api <base-url>                       B-1
     identity <base-url>                       B-2
     transform-runtime [options] -- <command…> B-6
     events <listen-port>                      B-12
     git <base-url> --repo ADDRESS [--token T] B-5
     platform-sdk <base-url>                   B-7
     llm <provider-base-url> --model M         B-10
   options:
     --specs DIR          the published boundaries/ directory (default boundaries)
     --results DIR        also write <kit>.junit.xml and <kit>.summary.json there
     --sample N           http-api: protected operations to probe (default 20)
     --issuer URL         identity: issuer (default <base-url>/api/oauth)
     --client-id ID --client-secret S   identity: a registered OAuth client
     --id-token JWT       identity: an id_token obtained by a login to verify
     --allow-http         identity: an http issuer skips instead of failing
     --env K=V            transform-runtime: an environment variable (repeatable)
     --timeout S          transform-runtime: per run; events: how long to listen
     --count N            events: deliveries to wait for (default 1)
     --repo ADDRESS       git: ontology, r/<rid> or <project>/<repo>
     --token T            git: a clone token (HTTP Basic password)
     --allow-push         git: also push a probe branch (only against a throwaway repo)
     --sdk-paths FILE     platform-sdk: the SDK's endpoint table (default: the repo's
                          clients/python-platform-sdk/sutra_platform_sdk/paths.py)
     --openapi FILE       platform-sdk: judge against this document instead of fetching it
     --api-key K          llm: bearer key (default: environment LLM_API_KEY)
     --chat-response F --structured-response F   llm: judge recorded answers offline
   Exit 0 when no check failed, 1 when one did, 2 on a usage error."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [conformance.json :as json]
            [conformance.kits.events :as events]
            [conformance.kits.git :as git]
            [conformance.kits.http-api :as http-api]
            [conformance.kits.identity :as identity]
            [conformance.kits.llm :as llm]
            [conformance.kits.platform-sdk :as sdk]
            [conformance.kits.transform-runtime :as runtime]
            [conformance.report :as r]))

(def ^:private flags #{"--allow-http" "--allow-push"})

(def ^:private sdk-paths "clients/python-platform-sdk/sutra_platform_sdk/paths.py")

(defn parse-args
  "{:kit :positional [...] :command [...] :opts {...}}; a usage error throws ex-info."
  [[kit & args]]
  (let [[before after] (split-with #(not= "--" %) args)]
    (loop [xs before, pos [], opts {}]
      (if-let [[x & more] (seq xs)]
        (cond
          (flags x) (recur more pos (assoc opts (keyword (subs x 2)) true))
          (str/starts-with? x "--")
          (if-let [v (first more)]
            (recur (rest more) pos
                   (if (= "--env" x)
                     (let [[k val] (str/split v #"=" 2)] (assoc-in opts [:env k] (str val)))
                     (assoc opts (keyword (subs x 2)) v)))
            (throw (ex-info (str x " needs a value") {})))
          :else (recur more (conj pos x) opts))
        {:kit kit :positional pos :command (vec (rest after)) :opts opts}))))

(defn- spec-file [opts & parts] (str (apply io/file (get opts :specs "boundaries") parts)))

(defn- int-opt [opts k d] (if-let [v (get opts k)] (Long/parseLong v) d))

(defn run
  "Run a kit from parsed args; answers [target checks]."
  [{:keys [kit positional command opts]}]
  (case kit
    "http-api"
    (let [base (or (first positional) (throw (ex-info "http-api needs <base-url>" {})))]
      [base (http-api/checks {:base base :sample (int-opt opts :sample 20)
                              :published (json/read-file (spec-file opts "http-api"
                                                                    "openapi.json"))})])
    "identity"
    (let [base (or (first positional) (throw (ex-info "identity needs <base-url>" {})))]
      [base (identity/checks (assoc (select-keys opts [:issuer :client-id :client-secret
                                                       :id-token :allow-http])
                                    :base base))])
    "transform-runtime"
    (do (when (empty? command)
          (throw (ex-info "transform-runtime needs -- <command…>" {})))
        [(str/join " " command)
         (runtime/checks {:command command :env (:env opts)
                          :timeout-s (int-opt opts :timeout 60)
                          :root (json/read-file (spec-file opts "transform-runtime"
                                                           "protocol.schema.json"))})])
    "events"
    (let [port (Long/parseLong (or (first positional)
                                   (throw (ex-info "events needs <listen-port>" {}))))]
      [(str "port " port)
       (events/checks {:port port :count (int-opt opts :count 1)
                       :timeout-s (int-opt opts :timeout 300)
                       :root (json/read-file (spec-file opts "events" "cloudevent.schema.json"))}
                      #(println "listening on port" port))])
    "git"
    (let [base (or (first positional) (throw (ex-info "git needs <base-url>" {})))
          repo (or (:repo opts) (throw (ex-info "git needs --repo" {})))]
      [(git/repo-url base repo)
       (git/checks {:base base :repo repo :token (:token opts)
                    :allow-push (:allow-push opts)})])
    "platform-sdk"
    (let [base (first positional)
          doc  (some-> (:openapi opts) json/read-file)]
      (when-not (or base doc)
        (throw (ex-info "platform-sdk needs <base-url> or --openapi" {})))
      [(or base (:openapi opts))
       (sdk/checks {:base base :openapi doc
                    :sdk-text (slurp (get opts :sdk-paths sdk-paths))})])
    "llm"
    (let [base     (first positional)
          recorded (:chat-response opts)]
      (when-not (or recorded (and base (:model opts)))
        (throw (ex-info "llm needs <provider-base-url> --model, or recorded responses" {})))
      [(or base (str "recorded " recorded))
       (llm/checks {:base base :model (:model opts)
                    :api-key (or (:api-key opts) (System/getenv "LLM_API_KEY"))
                    :chat-answer (some-> recorded json/read-file)
                    :structured-answer (some-> (:structured-response opts) json/read-file)})])
    (throw (ex-info (str "unknown kit " (pr-str kit)) {}))))

(defn -main [& args]
  (let [parsed (try (parse-args args)
                    (catch clojure.lang.ExceptionInfo e (println (ex-message e)) nil))
        [target checks] (when parsed
                          (try (run parsed)
                               (catch clojure.lang.ExceptionInfo e
                                 (println (ex-message e)) nil)))]
    (if-not checks
      (do (println (-> 'conformance.main the-ns meta :doc))
          (System/exit 2))
      (do (r/print-run (:kit parsed) target checks)
          (when-let [dir (get-in parsed [:opts :results])]
            (println "wrote" (str/join ", " (r/write-results! dir (:kit parsed) target checks))))
          (shutdown-agents)
          (System/exit (if (r/failed? checks) 1 0))))))
