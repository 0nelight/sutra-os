(ns ^:api ontology.build-estimate-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private model "grok-4")
(def ^:private rate-in 3.0)
(def ^:private rate-out 15.0)

(defn- py
  [& {:as extra}]
  (str "from sutra_transforms import transform, Input, Output, LanguageModel\n"
       "\n"
       "@transform(\n"
       "    inputs={'seed': Input(kind='dataset', dataset='seed_ai'),\n"
       "            'llm': LanguageModel(source='xai', model='" model "')},\n"
       "    output=Output(kind='dataset', dataset='widget_out', primary_key='pk'),\n"
       "    incremental=True"
       (apply str (for [[k v] extra] (str ",\n    " k "=" v)))
       ")\n"
       "def compute(seed, llm):\n"
       "    return []\n"))

(defn- schema []
  {:schema
   {:transforms
    {:mk-widget
     {:project :est :language :python :code (py)
      :incremental true
      :inputs {:seed {:kind :dataset :dataset :seed_ai}
               :llm  {:kind :language-model :source :xai :model model}}
      :primary-key :pk
      :output {:kind :dataset :dataset :widget_out}}}}})

(defn- estimate [] (:body (api/build-estimate {:only [:mk-widget]})))

(defn- entry [body] (first (:transforms body)))

(defn- register-model!
  []
  (api/mutate :create-llm-model {:name model :in rate-in :out rate-out :currency "USD"}
              {:stage? true}))

(defn- log-call!
  [caller in out]
  (is (ok? (api/post! :dataset-write {:name "llm_call_log"}
                      {:rows [{:values {:ts (str (java.time.Instant/now))
:request_id (str (java.util.UUID/randomUUID))
:caller caller :model model :source "xai"
:prompt_tokens in :completion_tokens out
:latency_ms 42}}]
:mode :append :message "seed llm_call_log"}))))

(defn- drain-schema
  []
  (assoc-in (schema) [:schema :transforms :mk-widget]
            {:project :est :language :python :code (py "done_key" "'seed.tid'" "max_per_run" 1)
             :incremental true
             :inputs {:seed {:kind :dataset :dataset :seed_ai}
                      :llm  {:kind :language-model :source :xai :model model}}
             :primary-key :pk
             :done-key {:input :seed :key :tid}
             :max-per-run 1
             :output {:kind :dataset :dataset :widget_out}}))

(defn- prompt-key-schema
  []
  (-> (drain-schema)
      (assoc-in [:schema :transforms :mk-widget :prompt-key]
                {:input :seed :columns [:label] :column :prompt-hash})
      (assoc-in [:schema :transforms :mk-widget :code]
                (py "done_key" "'seed.tid'" "max_per_run" 1
                    "prompt_key" "['seed.label']" "prompt_hash_column" "'prompt_hash'"))))

(deftest build-estimate-prices-the-build-before-it-runs
  (testing "install: a seed dataset, a registered model, an LLM transform over it"
    (is (ok? (api/mutate :create-project {:name :est} {:stage? true})))
    (is (ok? (api/mutate :create-source
                         {:name :xai :project :est :type :rest
                          :config {:base-url "https://api.x.ai"}
                          :egress ["api.x.ai:443"] :external-transform true}
                         {:stage? true}))
        "the model's source, released to code — the LanguageModel input reaches it")
    (is (ok? (api/mutate :create-dataset
                         {:name :seed_ai :project :est
                          :columns [(col :tid :string) (col :label :string)]}
                         {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :widget_out :project :est
                          :columns [(col :pk :string) (col :label :string)
                      (col :prompt_hash :string)]}
                         {:stage? true})))
    (is (ok? (register-model!)))
    (is (ok? (api/post! :staging)) "commit dataset + model")
    (is (ok? (api/post! :install (schema))) "install stages the transform")
    (is (ok? (api/post! :staging)) "commit schema")
    (is (ok? (api/post! :dataset-write {:name "seed_ai"} {:rows [{:values {:tid "a" :label "alpha"}}
                                        {:values {:tid "b" :label "beta"}}]
                                 :mode :append :message "seed"}))))

  (testing "with NO history the transform is UNCALIBRATED — unknown, never free"
    (let [b (estimate)
          e (entry b)]
      (is (= "mk-widget" (:transform e)) "the scope resolved to the producing transform")
      (is (= model (:model e)))
      (is (= :uncalibrated (:unknown e)))
      (is (nil? (:cost e)) "no history ⇒ no cost — NOT 0.0")
      (is (nil? (:cost (:total b))) "and the build total is unknown, not free")
      (is (true? (:lower-bound? (:total b))))
      (is (= [{:transform "mk-widget" :reason :uncalibrated}] (:unknown (:total b))))))

  (testing "ROWS is the delta this build would read — a never-consumed input reads WHOLE"
    (let [e (entry (estimate))]
      (is (= 2 (:rows e)))
      (is (= :bootstrap (:basis e))
          "no consumer-pointer yet ⇒ full read, exactly as the build would")
      (is (= "seed" (:input e)) "and it names the input it counted")))

  (testing "history prices it — and the log's UNDERSCORED caller joins the hyphenated transform"
    (log-call! "mk_widget" 1000 100)
    (log-call! "mk_widget" 1000 100)
    (let [b (estimate)
          e (entry b)]
      (is (nil? (:unknown e)) "calibrated now")
      (is (= :per-call (:source (:per-row e)))
          "the seed carries no ledger transactions, so tokens are measured PER CALL and 1 call/row is assumed — and it says so")
      (is (= 2 (:calls e)) "2 pending rows × 1 call/row")
      (is (= 2000 (:in-tokens e)))
      (is (= 200 (:out-tokens e)))
      (is (< (Math/abs (- 0.009 (:cost e))) 1e-9)
          "rows × measured tokens × the CURRENT registered rate")
      (is (< (Math/abs (- 0.009 (:cost (:total b)))) 1e-9))
      (is (false? (:lower-bound? (:total b))))))

  (testing "--calls-per-row overrides a fan-out the history cannot see yet"
    (let [e (first
             (:transforms (:body (api/build-estimate {:only [:mk-widget] :calls-per-row 4}))))]
      (is (= :override (:source (:per-row e))))
      (is (= 8 (:calls e)) "2 rows × 4 calls")
      (is (< (Math/abs (- 0.036 (:cost e))) 1e-9) "and the token cost scales with the fan-out")))

  (testing "the verdict against the daily budget rides along"
    (let [{:keys [budget]} (estimate)]
      (is (number? (:budget budget)))
      (is (number? (:remaining budget)))
      (is (contains? budget :fits?))))

  (testing "a scope with no language-model transform estimates as nothing to pay"
    (let [b (:body (api/build-estimate {:only [:seed-ai]}))]
      (is (empty? (:transforms b)))
      (is (nil? (:cost (:total b))))))

  (testing "declaring done-key + max-per-run flips the estimate onto the backlog"
    (is (ok? (register-model!)))
    (is (ok? (api/post! :install (drain-schema))))
    (is (ok? (api/post! :staging)))
    (let [e (entry (estimate))]
      (is (= :done-key (:pending-basis e))
          "the transform declares its done-key, so the estimate stops guessing from the delta")
      (is (= 2 (:pending e)))
      (is (= 1 (:calls-next-run e)) "the declared cap bounds one build")
      (is (= 2 (:runs e))           "so draining two rows takes two builds")
      (is (< (:cost-next-run e) (:cost e))
          "what pressing run costs is NOT what the backlog costs — the pair the cap creates")))

  (testing "a row already in the output drops OUT of the backlog (the anti-join is real)"
    (is (ok? (api/post! :dataset-write {:name "widget_out"}
                        {:rows [{:values {:pk "a" :label "alpha"}}]
          :mode :append :message "done row"})))
    (let [e (entry (estimate))]
      (is (= 1 (:pending e)) "seed row 'a' is done; only 'b' is left")
      (is (= 1 (:runs e)))))

  (testing "declaring prompt-key moves the anti-join onto the prompt hash"
    (is (ok? (register-model!)))
    (is (ok? (api/post! :install (prompt-key-schema))))
    (is (ok? (api/post! :staging)))
    (let [e (entry (estimate))]
      (is (= :prompt-key (:pending-basis e)))
      (is (= 2 (:pending e)) "both questions are open: the existing row carries no hash")
      (is (true? (:hash-mismatch? e))
          "and THAT is flagged: a non-empty output whose hashes match nothing is a broken
           key, not a cold start — arithmetically identical, operationally the opposite")))

  (testing "a row stamped with the hash python computes drops OUT — the two sides meet"
    (is (ok? (api/post! :dataset-write {:name "widget_out"} {:rows [{:values {:pk "a" :label "alpha"
                                                  :prompt_hash "f4047fd99848e33263917f954d33ebca"}}]
                                 :mode :snapshot :message "answered, with its question stamped"})))
    (let [e (entry (estimate))]
      (is (= 1 (:pending e))
          "seed row 'a' asked and answered; only 'b' is left — the SQL md5 matched python's")
      (is (false? (:hash-mismatch? e)) "and the alarm falls silent once the two sides meet")))

  (testing "a semantic-version bump re-opens every question, with no replace-mode run"
    (is (ok? (register-model!)))
    (is (ok? (api/post! :install (assoc-in (prompt-key-schema)
                                    [:schema :transforms :mk-widget :code]
                                    (py "done_key" "'seed.tid'" "max_per_run" 1
                                        "prompt_key" "['seed.label']"
                                        "prompt_hash_column" "'prompt_hash'"
                                        "semantic_version" 2)))))
    (is (ok? (api/post! :staging)))
    (let [e (entry (estimate))]
      (is (= 2 (:pending e))
          "the stamped row was answered under version 1; version 2 asks a different question")))

  (testing "the roll-up carries both numbers, and the budget verdict is about the NEXT run"
    (let [{:keys [total budget]} (estimate)]
      (is (some? (:cost-next-run total)))
      (is (>= (:cost total) (:cost-next-run total)))
      (is (empty? (remove nil? (:delta-based total)))
          "nothing in this scope is delta-based any more")
      (is (= (:estimate budget) (:cost-next-run total))
          "the budget binds per day, so it is held against what one run costs"))))
