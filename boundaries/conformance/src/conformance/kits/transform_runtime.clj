(ns conformance.kits.transform-runtime
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [conformance.json :as json]
            [conformance.report :as r]
            [conformance.schema :as schema])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent TimeUnit)))

(def ^:private rows-module
  "from sutra_transforms import transform, Input, Output

@transform(inputs={'src': Input(kind='dataset', dataset='src')},
           output=Output(kind='object-type', object_type='t', primary_key='id'))
def t(src):
    return [{'id': r['id'], 'qty': r['qty'], 'note': r['note']} for r in src]
")

(def ^:private scalar-csv-module
  "from sutra_transforms import transform, Input, Output

@transform(inputs={'k': Input(kind='scalar'), 'f': Input(kind='csv')},
           output=Output(kind='object-type', object_type='t', primary_key='id'))
def t(k, f):
    return [{'id': r['id'], 'k': k} for r in f]
")

(def ^:private failing-module
  "from sutra_transforms import transform, Output

@transform(inputs={}, output=Output(kind='object-type', object_type='t', primary_key='id'))
def t():
    raise ValueError('conformance: this transform fails on purpose')
")

(def rows-input
  [{"id" "a" "qty" "~f1.50" "note" "~~tilde"} {"id" "b" "qty" "~f-0.001" "note" "plain"}])

(defn- temp-dir []
  (str (Files/createTempDirectory "sutra-conformance" (make-array FileAttribute 0))))

(defn- delete-tree [dir]
  (doseq [f (reverse (file-seq (io/file dir)))] (io/delete-file f true)))

(defn run-case
  [{:keys [command env version timeout-s]} module files manifest]
  (let [dir (temp-dir)]
    (try
      (let [path   #(str (io/file dir %))
            _      (spit (path "mod.py") module)
            _      (doseq [[n c] files] (spit (path n) c))
            mf     (into {} (map (fn [[k e]] [k (cond-> e (get e "file")
                                                    (-> (assoc "path" (path (get e "file")))
                                                        (dissoc "file")))]))
                         manifest)
            _      (spit (path "manifest.json") (json/write mf))
            pb     (ProcessBuilder. ^java.util.List
                                    (vec (concat command ["--run" (path "mod.py")
                                                          "--inputs" (path "manifest.json")])))
            penv   (.environment pb)
            _      (.clear penv)
            _      (doseq [[k v] (merge {"PATH" (or (System/getenv "PATH") "/usr/bin:/bin")
                                         "LANG" "C.UTF-8" "PYTHONDONTWRITEBYTECODE" "1"}
                                        env
                                        (when version {"SUTRA_PROTOCOL_VERSION" version}))]
                     (.put penv k v))
            _      (.redirectOutput pb (io/file (path "stdout")))
            _      (.redirectError pb (io/file (path "stderr")))
            p      (.start pb)
            done?  (.waitFor p timeout-s TimeUnit/SECONDS)]
        (when-not done? (.destroyForcibly p) (.waitFor p))
        {:exit   (if done? (.exitValue p) :timeout)
         :stdout (slurp (path "stdout"))
         :stderr (slurp (path "stderr"))
         :manifest mf})
      (finally (delete-tree dir)))))

(defn- lines [s] (remove str/blank? (str/split-lines (str s))))

(defn- major [v] (first (str/split (str v) #"\.")))

(defn result-problems
  [root published stdout]
  (let [ls  (lines stdout)
        res (json/parse-or-nil (last ls))]
    (cond
      (not= 1 (count ls)) (str "stdout holds " (count ls) " lines, not one JSON object")
      (not (map? res)) "stdout is not a JSON object"
      :else
      (let [errs (schema/errors-at root "#/$defs/result" res)]
        (cond
          (seq errs) (str/join "; " (take 5 errs))
          (not= 1 (count (filter #(contains? res %) ["rows" "outputs"])))
          "not exactly one of rows and outputs"
          (not= (major published) (major (get res "protocol_version")))
          (str "protocol_version " (pr-str (get res "protocol_version"))))))))

(defn stderr-problems
  [root stderr]
  (seq (for [l (lines stderr)
             :let [x (json/parse-or-nil l)]
             :when (map? x)
             :let [ref (cond (contains? x "sutra_span") "#/$defs/span-line"
                             (contains? x "sutra_intermediate") "#/$defs/intermediate-line")]
             :when ref
             e (schema/errors-at root ref x)]
         e)))

(defn error-line-problems
  [root {:keys [exit stderr]} t]
  (let [x (json/parse-or-nil (last (lines stderr)))]
    (cond
      (not= 1 exit) (str "exit " exit ", not 1")
      (not (map? x)) "the last stderr line is not a JSON object"
      (seq (schema/errors-at root "#/$defs/error-line" x))
      (str/join "; " (schema/errors-at root "#/$defs/error-line" x))
      (and t (not= t (get x "type"))) (str "type " (pr-str (get x "type")) ", not " t))))

(defn checks
  [{:keys [command env root timeout-s] :or {timeout-s 60}}]
  (let [v      (str (get root "version"))
        opts   {:command command :env env :timeout-s timeout-s}
        rows   (delay (run-case (assoc opts :version v) rows-module
                                {"src.json" (json/write rows-input)}
                                {"src" {"kind" "dataset" "file" "src.json"}}))
        sc     (delay (run-case (assoc opts :version v) scalar-csv-module
                                {"f.csv" "id,name\r\nx,\"a, b\"\r\ny,c\r\n"}
                                {"k" {"kind" "scalar" "value" 3}
                                 "f" {"kind" "csv" "file" "f.csv"}}))
        exited (fn [run] (when-not (= 0 (:exit run))
                           (str "exit " (:exit run) ": " (last (lines (:stderr run))))))]
    [(r/check "manifests-valid" "the manifests this kit hands over match $defs/manifest"
              #(let [errs (mapcat (fn [run] (schema/errors-at root "#/$defs/manifest"
                                                             (:manifest @run)))
                                  [rows sc])]
                 (when (seq errs) (str/join "; " errs))))
     (r/check "rows-run" "a row input runs and answers one protocol result"
              #(or (exited @rows) (result-problems root v (:stdout @rows))))
     (r/check "wire-values" "exact decimals and ~-texts come back in wire encoding, unchanged"
              #(let [got (get (json/parse-or-nil (last (lines (:stdout @rows)))) "rows")]
                 (when (not= rows-input got) (str "rows " (pr-str got)))))
     (r/check "scalar-csv-run" "a scalar and an RFC 4180 csv input materialize"
              #(or (exited @sc) (result-problems root v (:stdout @sc))
                   (let [got (get (json/parse-or-nil (last (lines (:stdout @sc)))) "rows")]
                     (when (not= [{"id" "x" "k" 3} {"id" "y" "k" 3}] got)
                       (str "rows " (pr-str got))))))
     (r/check "stderr-structured" "structured stderr lines match their definitions"
              #(when-let [es (seq (concat (stderr-problems root (:stderr @rows))
                                          (stderr-problems root (:stderr @sc))))]
                 (str/join "; " (take 5 es))))
     (r/check "error-line" "a failing transform exits 1 with an error line"
              #(error-line-problems root (run-case (assoc opts :version v) failing-module {} {})
                                    nil))
     (r/check "major-refused" "a host of another MAJOR is refused (ProtocolVersionMismatch)"
              #(error-line-problems root
                                    (run-case (assoc opts :version "99.0") rows-module
                                              {"src.json" (json/write rows-input)}
                                              {"src" {"kind" "dataset" "file" "src.json"}})
                                    "ProtocolVersionMismatch"))
     (r/check "minor-served" "a host of a later MINOR of the same MAJOR is served"
              #(let [run (run-case (assoc opts :version (str (major v) ".99")) rows-module
                                   {"src.json" (json/write rows-input)}
                                   {"src" {"kind" "dataset" "file" "src.json"}})]
                 (or (exited run) (result-problems root v (:stdout run)))))]))
