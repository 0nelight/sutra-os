(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.java.shell :as sh]
         '[clojure.string :as str]
         '[clojure.data.json :as json])

(def args *command-line-args*)
(defn opt [k] (second (drop-while #(not= % k) args)))
(def scene-file (first args))
(when-not (and scene-file (opt "--home"))
  (println "usage: szene.clj <scene.edn> --home <sutra-solo data dir> [--no-setup] [--out dir]")
  (System/exit 2))

(def here (.getParentFile (.getCanonicalFile (io/file *file*))))
(def repo (.getParentFile here))
(def scene (edn/read-string (slurp scene-file)))
(def home (.getCanonicalPath (io/file (opt "--home"))))
(def creds (str home "/cli-creds.edn"))
(def base (:base-url (edn/read-string (slurp creds))))
(def out (.getCanonicalPath (io/file (or (opt "--out") (str here "/out/szenen")))))

(defn sutra [& xs]
  (let [{:keys [exit out err]} (apply sh/sh "timeout" "30" "bin/sutra" "--base" base "--creds" creds
                                      (concat (map str xs) [:dir (str repo)]))
        text (str out err)]
    (when (or (pos? exit) (re-find #"✗" text))
      (println (str "  ✗ sutra " (str/join " " xs) "\n"
                    (str/join "\n" (filter #(re-find #"✗" %) (str/split-lines text))))))
    text))

(defn arg-text [v]
  (cond (keyword? v) (name v) (boolean? v) (str v) :else (str v)))

(def scene-dir (.getParentFile (.getCanonicalFile (io/file scene-file))))

(defn fill [s]
  (-> (str s)
      (str/replace "{{eingang}}" (str home "/eingang"))
      (str/replace "{{scene}}" (.getPath scene-dir))))

(defn- landed [text]
  (println (str/join "\n" (filter #(re-find #"landed|staged|unchanged" %) (str/split-lines text)))))

(defn push-texts!
  [projection texts]
  (let [dir (io/file out (str "checkout-" (name projection)))]
    (sh/sh "rm" "-rf" (.getPath dir))
    (.mkdirs dir)
    (apply sutra "projection" "pull" (name projection)
           (concat (keys texts) ["--to" (.getCanonicalPath dir) "--force"]))
    (doseq [[target src] texts]
      (spit (io/file dir target) (fill (slurp (io/file scene-dir src)))))
    (landed (apply sutra "projection" "push"
                   (for [target (keys texts)] (.getCanonicalPath (io/file dir target)))))))

(defn setup! [{:keys [project description model push files cli seed]}]
  (sutra "project" "create" (name project) "--description" (or description (name project)))
  (when model
    (push-texts! :ontologie {(str (name project) ".objects.ent") model}))
  (doseq [[projection texts] push]
    (push-texts! projection texts))
  (doseq [[target src] files]
    (io/copy (io/file scene-dir src) (io/file (fill target))))
  (doseq [args cli]
    (println (str "  sutra " (str/join " " args)))
    (apply sutra (map fill args)))
  (doseq [[action params] seed]
    (apply sutra "action" (name action)
           (concat (for [[k v] params :when (some? v)] (str (name k) "=" (arg-text v)))
                   ["--yes"])))
  (println (str "  seeded " (count seed) " actions")))

(def target-ops #{:click :hover :type :select :highlight})

(defn target [t]
  (cond (string? t) {:text t}
        (:field t)  {:in  (format ".capp-actionform__row:has(.capp-actionform__label:text-is(%s))"
                                  (json/write-str (:field t)))
                     :css "input,select,textarea"}
        :else t))

(defn step [[op & xs]]
  (into [(name op)] (if (target-ops op) (cons (target (first xs)) (rest xs)) xs)))

(.mkdirs (io/file out))
(when-not (some #{"--no-setup"} args) (setup! (:setup scene)))

(let [plan (-> (select-keys scene [:viewport :beat :travel :type-delay :settle :css :locale])
               (assoc :typeDelay (:type-delay scene) :base base :out out
                      :name (:name scene "szene") :steps (mapv step (:steps scene))))
      plan-file (io/file out (str (:name plan) ".plan.json"))
      _ (spit plan-file (json/write-str plan))
      {:keys [exit out err]} (sh/sh "node" (str here "/szene.mjs") (.getCanonicalPath plan-file)
                                    :dir (str here))
      lines (str/split-lines out)]
  (doseq [l (butlast lines)] (println l))
  (when (seq err) (println err))
  (let [{:strs [webm trim]} (json/read-str (last lines))
        mp4 (str/replace webm #"\.webm$" ".mp4")]
    (sh/sh "ffmpeg" "-y" "-loglevel" "error" "-ss" (str trim) "-i" webm
           "-vf" "fps=30,format=yuv420p" "-c:v" "libx264" "-crf" "18" "-preset" "slow"
           "-movflags" "+faststart" mp4)
    (println (str "\n  film: " mp4))
    (System/exit (if (zero? exit) 0 1))))
