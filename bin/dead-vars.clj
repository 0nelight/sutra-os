(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.java.shell :as sh]
         '[clojure.string :as str])

(def dirs (or (seq *command-line-args*) ["clients/frontend/src" "clients/frontend/test"]))

(def analysis
  (let [cfg "{:output {:format :edn} :analysis {:locals false :keywords false}}"
        {:keys [out err]} (apply sh/sh "clj-kondo" "--config" cfg
                                 (mapcat (fn [d] ["--lint" d]) dirs))]
    (or (:analysis (edn/read-string {:default (fn [_ v] v)} out))
        (throw (ex-info (str "clj-kondo gave no analysis: " err) {})))))

(def plain-defs
  '#{cljs.core/def cljs.core/defn cljs.core/defonce clojure.core/def clojure.core/defn
     clojure.core/defonce cljs.core/defn- clojure.core/defn-})

(def used
  (into #{}
        (keep (fn [{:keys [to name from from-var]}]
                (when-not (and (= to from) (= name from-var)) [to name])))
        (:var-usages analysis)))

(def configs
  (apply str (for [d dirs
                   n ["shadow-cljs.edn" "deps.edn"]
                   :let [f (io/file d ".." n)]
                   :when (.exists f)]
               (slurp f))))

(def sci-namespaces
  (into #{}
        (for [d dirs
              f (file-seq (io/file d))
              :when (re-find #"\.clj[sc]?$" (.getName ^java.io.File f))
              [_ n] (re-seq #"sci/copy-ns\s+([\w.-]+)" (slurp f))]
          (symbol n))))

(def dead
  (->> (:var-definitions analysis)
       (filter (fn [{:keys [ns name export test defined-by] :as d}]
                 (and (plain-defs defined-by)
                      (not export) (not test)
                      (not (str/starts-with? (str name) "_"))
                      (not (sci-namespaces ns))
                      (not (str/includes? configs (str ns "/" name)))
                      (not (str/ends-with? (str ns) "smoke-test"))
                      (not (:clj-kondo/ignore d))
                      (not (used [ns name])))))
       (sort-by (juxt :filename :row))))

(doseq [{:keys [ns name filename row private]} dead]
  (println (str filename ":" row) (str ns "/" name) (if private "(private)" "")))
(println (count dead) "unread vars")
