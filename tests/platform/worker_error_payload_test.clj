(ns ^:unit platform.worker-error-payload-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.data.json :as json])
  (:import [java.io File]))

(def ^:private worker-dir
  "src/data-plane/python")

(defn- python3? []
  (zero? (:exit (try (sh/sh "python3" "--version") (catch Exception _ {:exit 1})))))

(def ^:private failing-transform
  "from sutra_transforms import transform, Output

def _search():
    return ('OK', [])

def _uids():
    _, data = _search()
    return (data[0] or b'').split()

@transform(inputs={}, output=Output(kind='dataset', dataset='x', primary_key='id'))
def boom():
    return [{'id': u} for u in _uids()]
")

(defn- run-worker
  [src]
  (let [f (File/createTempFile "boom" ".py")]
    (try
      (spit f src)
      (let [{:keys [exit err]} (sh/sh "python3" "-m" "sutra_transforms"
                                      "--run" (.getAbsolutePath f)
                                      :dir worker-dir
                                      :env {"PYTHONPATH" "."
                                            "PATH" (System/getenv "PATH")})
            line (str/trim err)]
        {:exit exit
         :raw  line
         :file (.getName f)
         :payload (try (json/read-str line :key-fn keyword) (catch Exception _ nil))})
      (finally (.delete f)))))

(deftest a-dead-transform-reports-its-crash-site
  (if-not (python3?)
    (println "  (worker-error-payload SKIPPED — no python3 on PATH)")
    (let [{:keys [exit raw file payload]} (run-worker failing-transform)]
      (testing "the run fails loudly with one parseable json line on stderr"
        (is (= 1 exit))
        (is (some? payload) (str "stderr was not a json object: " raw)))
      (when payload
        (testing "the error itself still reads as before — this is additive"
          (is (= "IndexError" (:type payload)))
          (is (= "list index out of range" (:error payload))))
        (testing "`where` names the user's module, its line, its function AND the
                  source expression — the four facts `str(e)` drops"
          (let [w (:where payload)]
            (is (str/includes? w file)
                (str "must point into the user module, got: " w))
            (is (str/includes? w "in _uids")
                (str "must be the DEEPEST user frame, not the transform entry "
                     "point — got: " w))
            (is (str/includes? w "data[0]")
                (str "must carry the failing expression verbatim — got: " w))))
        (testing "the full traceback rides along for the container log"
          (is (str/includes? (:traceback payload) "Traceback (most recent call last)")))
        (testing "`where` survives the 400-char stderr cap that
                  `conductor.log.runs/describe-throwable` applies — it must sit ahead
                  of the unbounded traceback, and be readable within the cap"
          (let [capped (subs raw 0 (min 400 (count raw)))]
            (is (str/includes? capped "\"where\"")
                (str "`where` fell outside the 400-char window the job error "
                     "keeps — it must precede `traceback` in the payload:\n"
                     capped))
            (is (< (.indexOf raw "\"where\"") (.indexOf raw "\"traceback\""))
                "payload key order regressed: `traceback` must come last")))))))

(deftest a-crash-inside-a-library-still-reports-a-site
  (testing "no user frame matched — `where` falls back to the deepest frame there
            is, rather than going missing exactly when the cause is least obvious"
    (if-not (python3?)
      (println "  (worker-error-payload SKIPPED — no python3 on PATH)")
      (let [{:keys [payload]}
            (run-worker
             "from sutra_transforms import transform, Output
import json

@transform(inputs={}, output=Output(kind='dataset', dataset='x', primary_key='id'))
def boom():
    return json.loads('{not json')
")]
        (is (some? (:where payload)))
        (is (str/includes? (:where payload) ".py:")
            (str "expected a file:line, got: " (:where payload)))))))
