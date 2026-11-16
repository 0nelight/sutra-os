(ns ^:unit platform.llm-failure-classification-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def ^:private worker-dir
  "src/data-plane/python")

(defn- python3? []
  (zero? (:exit (try (sh/sh "python3" "--version") (catch Exception _ {:exit 1})))))

(defn- py [src]
  (sh/sh "python3" "-c" src :dir worker-dir
         :env {"PYTHONPATH" "." "PATH" (System/getenv "PATH")}))

(deftest platform_failures_are_fatal_provider_failures_are_not
  (if-not (python3?)
    (println "  (llm-failure-classification SKIPPED — no python3 on PATH)")
    (let [{:keys [exit out err]}
          (py "import sutra_llm as llm
cases = [
  ('pool-exhausted', 500,
   'sutra:pg-control - Connection is not available, request timed out after 30000ms'),
  ('pool-timeout',   500, 'Connection is not available, request timed out after 30000ms'),
  ('provider-500',   500, 'xai upstream: internal server error'),
  ('provider-503',   503, 'service unavailable'),
  ('rate-limit',     429, 'too many requests'),
  ('auth-401',       401, ''),
  ('auth-403-body',  None, 'agent: endpoint returned HTTP 403'),
]
for name, code, detail in cases:
    print('%s=%s' % (name, llm._fatal_http(code, detail)))
")]
      (is (zero? exit) (str "probe must run — " err))
      (let [r (into {} (for [l (str/split-lines (str/trim out))
                             :let [[k v] (str/split l #"=")]]
                         [k (= "True" v)]))]
        (testing "OUR failure ⇒ fatal: the build must not publish a reduced
                  output and call it success"
          (is (true? (get r "pool-exhausted")))
          (is (true? (get r "pool-timeout"))))
        (testing "the PROVIDER's failure ⇒ still a per-item transient, so one
                  bad call does not stop an ontology"
          (is (false? (get r "provider-500")))
          (is (false? (get r "provider-503")))
          (is (false? (get r "rate-limit"))))
        (testing "the auth family is unchanged"
          (is (true? (get r "auth-401")))
          (is (true? (get r "auth-403-body"))))))))

(deftest the-facade-still-publishes-the-whole-surface
  (if-not (python3?)
    (println "  (facade-surface SKIPPED — no python3 on PATH)")
    (let [surface ["transform" "Input" "Output" "FileSystem" "FileStatus" "MediaSet"
                   "get_secret" "previous_output" "output_enum" "put_media_item"
                   "cap_batch" "trace" "commission_code"
                   "LanguageModel" "ChatModel" "LanguageModelError"
                   "parallel_map" "completion_column" "error_column_for" "tool_args"]
          {:keys [exit out err]}
          (py (str "import sutra_transforms as st\n"
                   "missing = [n for n in ["
                   (clojure.string/join ", " (map pr-str surface))
                   "] if not hasattr(st, n)]\n"
                   "print('MISSING=%s' % missing)\n"))]
      (is (zero? exit) (str "the façade must import — " err))
      (is (str/includes? out "MISSING=[]")
          (str "a name fell off the façade: " out)))))

(deftest a-per-item-failure-fails-the-job-unless-the-caller-opted-out
  (if-not (python3?)
    (println "  (llm-failure-classification SKIPPED — no python3 on PATH)")
    (let [{:keys [exit err]}
          (py "import sutra_transforms as st
def fn(i):
    if i == 2:
        raise st.LanguageModelError('provider hiccup')
    return {'n': i}
print(st.parallel_map(fn, [1,2,3]))
")]
      (is (pos? exit) "the DEFAULT fails the job on a per-item error")
      (is (str/includes? err "provider hiccup")
          (str "and it fails with the item's own error, got: " err)))))

(deftest an-opted-in-skip-is-reported-not-silent
  (if-not (python3?)
    (println "  (llm-failure-classification SKIPPED — no python3 on PATH)")
    (let [{:keys [exit out err]}
          (py "import sutra_transforms as st
def fn(i):
    if i == 2:
        raise st.LanguageModelError('provider hiccup')
    return {'n': i}
print(st.parallel_map(fn, [1,2,3], on_error='skip'))
")]
      (is (zero? exit) (str "an OPTED-IN skip survives one transient — " err))
      (testing "the item is skipped (None in its slot), order preserved"
        (is (str/includes? out "[{'n': 1}, None, {'n': 3}]")
            (str "unexpected result: " out)))
      (testing "…and the skip is announced on stderr. A green build that
                dropped rows must say so, or 'green' and 'complete' stop
                meaning the same thing"
        (is (str/includes? err "SKIPPED 1 of 3")
            (str "expected a loud skip report, got: " err))))))

(deftest an-enrichment-failure-stays-a-row
  (if-not (python3?)
    (println "  (llm-failure-classification SKIPPED — no python3 on PATH)")
    (let [{:keys [exit out err]}
          (py "import sutra_transforms as st
rows = [{'id': 1}, {'id': 2}, {'id': 3}]
def fn(r):
    if r['id'] == 2:
        raise st.LanguageModelError('provider hiccup')
    return 'a%d' % r['id']
out = st.completion_column(fn, rows, 'llm_answer')
print('N=%d' % len(out))
print('ERR=%s' % out[1]['_completion_error-llm_answer'])
print('ANS=%s' % out[1]['llm_answer'])
print('OK2=%s' % out[2]['llm_answer'])
print('CLEAN=%s' % out[0]['_completion_error-llm_answer'])
print('KEEPS_INPUT=%s' % out[1]['id'])
")]
      (is (zero? exit) (str "an enrichment failure must not fail the job — " err))
      (testing "N rows in, N rows out — the failed item is NOT dropped"
        (is (str/includes? out "N=3") (str "got: " out))
        (is (str/includes? out "KEEPS_INPUT=2")
            "the failed row keeps its input columns, so it is still identifiable"))
      (testing "the failure is a VALUE in the error column, the answer is null"
        (is (str/includes? out "ERR=LanguageModelError: provider hiccup") (str "got: " out))
        (is (str/includes? out "ANS=None") (str "got: " out)))
      (testing ""
        (is (str/includes? out "CLEAN=None") (str "got: " out))
        (is (str/includes? out "OK2=a3") (str "got: " out))))))

(deftest a-total-wipeout-still-fails
  (if-not (python3?)
    (println "  (llm-failure-classification SKIPPED — no python3 on PATH)")
    (let [{:keys [exit err]}
          (py "import sutra_transforms as st
def fn(i):
    raise st.LanguageModelError('everything is down')
st.parallel_map(fn, [1,2,3], on_error='skip')
")]
      (is (pos? exit) "all-N-failed is systemic and must raise even under 'skip'")
      (is (str/includes? err "systemic outage")
          (str "expected the systemic message, got: " err)))))
