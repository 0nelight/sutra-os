(require '[clojure.string :as str] '[clojure.java.io :as io] '[clojure.edn :as edn])

(def src "clients/frontend/src")
(def css "resources/public/css")
(def ausnahmen-datei "clients/frontend/fe-regeln.edn")

(defn rel [f] (str/replace (str f) #"^\./" ""))
(defn dateien [dir ext]
  (->> (file-seq (io/file dir)) (filter #(str/ends-with? (str %) ext)) (map rel) sort))

(def px-re
  #":style[^}]*\"[0-9.]+px\"|:style \{[^}]*:(?:width|height|margin|padding|gap|top|left|right|bottom|min-width|max-width|min-height|max-height|font-size)[a-z-]* [0-9]+[ }]")

(defn style-px []
  (set (filter #(re-find px-re (slurp %)) (dateien src ".cljs"))))

(defn hex-css []
  (set (for [f (dateien css ".css")
             :when (not (str/includes? f "/blueprint-cljs/"))
             :when (not (str/ends-with? f "/tokens.css"))
             :when (re-find #"#[0-9a-fA-F]{3,8}\b" (slurp f))]
         f)))

(defn budget []
  (let [b (io/file css "budget.edn")]
    (set (for [[f n] (when (.exists b) (edn/read-string (slurp b)))
               :let [z (count (str/split-lines (slurp (io/file css f))))]
               :when (> z n)]
           (str f " " z ">" n)))))

(defn zustand-aliase [text]
  (set (for [[_ n a] (re-seq #"\[([a-z0-9.-]+)\s+:as\s+([a-z0-9-]+)" text)
             :when (or (str/starts-with? n "sutra-ui.state.")
                       (re-find #"\.(state|model)$" n))]
         a)))

(defn view? [f]
  (and (not (str/includes? f "/sutra_ui/state/"))
       (not (str/includes? f "/sutra_ui/ui/"))
       (not (str/ends-with? f "_test.cljs"))))

(defn cursor-swap []
  (set (for [f (dateien src ".cljs") :when (view? f)
             :let [t (slurp f) as (zustand-aliase t)]
             :when (or (re-find #"\(r/cursor " t)
                       (some #(re-find (re-pattern (str "\\((?:swap|reset)! " % "/")) t) as))]
         f)))

(defn roh-table []
  (set (for [f (dateien src ".cljs")
             :when (not (str/includes? f "/sutra_ui/ui/"))
             :when (not (str/ends-with? f "_test.cljs"))
             :when (re-find #"\[:table[ .\]\n]" (slurp f))]
         f)))

(def regeln {:style-px style-px :hex-css hex-css :budget budget
             :cursor-swap cursor-swap :roh-table roh-table})
(def ausnahmen (let [f (io/file ausnahmen-datei)] (if (.exists f) (edn/read-string (slurp f)) {})))

(def stand (into (sorted-map) (for [[k f] regeln] [k (f)])))

(if (= "--stand" (first *command-line-args*))
  (let [neu (into (sorted-map)
                  (for [[k ist] stand :when (not= k :budget)]
                    [k (into (sorted-set)
                             (if (contains? ausnahmen k) (filter (get ausnahmen k) ist) ist))]))]
    (spit ausnahmen-datei
          (str ";; Ausnahmen zu bin/fe-regeln.clj - duerfen nur schrumpfen.\n"
               "{" (str/join "\n "
                             (for [[k fs] neu]
                               (str k " #{" (str/join "\n   " (map pr-str fs)) "}")))
               "}\n"))
    (println "Ausnahmen:" (update-vals neu count)))
  (let [neu   (for [[k ist] stand, x ist :when (not (contains? (get ausnahmen k #{}) x))] [k x])
        sauber (for [[k erl] ausnahmen, x erl :when (not (contains? (get stand k) x))] [k x])]
    (doseq [[k x] neu] (println "VERSTOSS" k x))
    (doseq [[k x] sauber] (println "sauber, aus der Ausnahmeliste streichen:" k x))
    (println "Stand:" (update-vals stand count)
             "Ausnahmen:" (update-vals ausnahmen count))
    (System/exit (if (seq neu) 1 0))))
