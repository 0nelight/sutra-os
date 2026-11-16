(require '[clojure.data.json :as json]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.java.shell :as sh]
         '[clojure.pprint :as pprint]
         '[clojure.string :as str]
         '[clojure.walk :as walk])
(import '[java.lang ProcessBuilder$Redirect]
        '[java.net ServerSocket]
        '[java.nio.file Files]
        '[java.nio.file.attribute FileAttribute]
        '[java.util.concurrent TimeUnit])

(def repo (.getCanonicalPath (.getParentFile (.getParentFile (io/file *file*)))))

(defn die [& msg]
  (binding [*out* *err*] (println (apply str "bin/readme-media: " msg)))
  (throw (ex-info "readme-media failed" {:die true})))

(defn step [& msg] (println (apply str "▶ " msg)) (flush))

(defn free-port []
  (with-open [s (ServerSocket. 0)] (.getLocalPort s)))

(defn delete-tree [^java.io.File f]
  (when (.isDirectory f) (run! delete-tree (.listFiles f)))
  (.delete f))

(defn start-solo
  [env log]
  (let [pb (doto (ProcessBuilder. ["setsid" "bin/sutra-solo"])
             (.directory (io/file repo))
             (.redirectErrorStream true)
             (.redirectOutput (ProcessBuilder$Redirect/to log)))]
    (.putAll (.environment pb) env)
    (.start pb)))

(defn- kill-group [sig ^Process p]
  (.waitFor (.start (ProcessBuilder. ["kill" sig "--" (str "-" (.pid p))]))))

(defn stop-solo
  [^Process p]
  (when (and p (.isAlive p))
    (kill-group "-TERM" p)
    (when-not (.waitFor p 60 TimeUnit/SECONDS)
      (kill-group "-KILL" p))))

(defn await-ready [^Process p log]
  (loop [n 0]
    (cond
      (str/includes? (slurp log) "laeuft allein") :ready
      (not (.isAlive p)) (do (println (->> (str/split-lines (slurp log)) (take-last 40)
                                           (str/join "\n")))
                             (die "sutra-solo exited during start"))
      (> n 900) (die "sutra-solo not ready after 15 min")
      :else (do (Thread/sleep 1000) (recur (inc n))))))

(defn run-in
  [env argv & {:keys [in]}]
  (let [r (apply sh/sh (concat argv [:dir repo :env (merge (into {} (System/getenv)) env)]
                               (when in [:in in])))]
    {:exit (:exit r) :out (str (:out r) (:err r))}))

(defn sutra-fn [env base]
  (fn [args & {:keys [in]}]
    (run-in env (concat ["timeout" "30"]
                        ["bin/sutra" "--base" base
                         "--creds" (str (get env "SUTRA_HOME") "/cli-creds.edn")]
                        args)
            :in in)))

(def farm (str repo "/media/hero/farm"))

(defn translator
  [lang]
  (if (= lang "en")
    identity
    (let [d (edn/read-string (slurp (io/file farm (str lang ".edn"))))]
      (fn [s] (get d s s)))))

(defn tr-data [tr x] (walk/postwalk #(if (string? %) (tr %) %) x))

(defn tr-csv [tr text]
  (str (str/join "\n"
                 (for [line (str/split-lines text)]
                   (str/join "," (for [cell (re-seq #"\"[^\"]*\"|[^,]+" line)
                                       :let [v (tr (str/replace cell "\"" ""))]]
                                   (if (str/includes? v ",") (str "\"" v "\"") v)))))
       "\n"))

(defn json-after-brace [s]
  (json/read-str (subs s (str/index-of s "{"))))

(defn push-model! [sutra tr work]
  (step "model: resources/presets/beispiele/farm.ent")
  (sutra ["project" "create" "farm" "--yes"])
  (let [proj (str work "/proj")
        r    (sutra ["projection" "pull" "ontologie" "farm.objects.ent" "--to" proj])
        _    (when-not (zero? (:exit r)) (die "projection pull failed:\n" (:out r)))
        read (fn [p] (:schema (edn/read-string (slurp (io/file repo p)))))
        m    (tr-data tr (merge-with merge (read "resources/presets/beispiele/farm.ent")
                                    (read "media/hero/farm/users.ent")))
        text (str proj "/farm.objects.ent")]
    (spit text (with-out-str
                 (pprint/pprint {:schema (select-keys m [:objects :links :actions])})))
    (let [r (sutra ["projection" "push" text])]
      (when-not (str/includes? (:out r) "landed") (die "ontology push did not land:\n" (:out r))))))

(defn push-ingest!
  [sutra tr work home]
  (step "sources: media/hero/farm/*.csv → raw datasets")
  (let [inbox (io/file home "eingang")
        dir   (str work "/ingest")
        text  (str dir "/farm.ingest.ent")]
    (.mkdirs inbox)
    (doseq [c ["baling_log.csv" "field_register.csv" "buyer_list.csv"]]
      (spit (io/file inbox c) (tr-csv tr (slurp (io/file farm c)))))
    (let [r (sutra ["projection" "pull" "ontologie" "farm.ingest.ent" "--to" dir])]
      (when-not (zero? (:exit r)) (die "ingest pull failed:\n" (:out r))))
    (spit text (-> (with-out-str
                     (pprint/pprint (tr-data tr (edn/read-string
                                                 (slurp (io/file farm "ingest.ent"))))))
                   (str/replace "{{inbox}}" (str inbox))))
    (let [r (sutra ["projection" "push" text])]
      (when-not (str/includes? (:out r) "landed") (die "ingest push did not land:\n" (:out r))))))

(defn transform-line [nm input output pk sql]
  (str "transform create " nm " --language sql --input " input " --output-type " output
       " --pk " pk " --query-file " farm "/" sql))

(defn declare-transforms!
  [sutra]
  (step "transforms (declared): hay_lots → Lot, fields → Field, buyers → Buyer")
  (let [r (sutra ["--yes"]
                 :in (str/join "\n"
                               [(transform-line "hay_lots" "log:dataset:baling_log" "lot" "name"
                                                "hay_lots.sql")
                                (transform-line "fields" "register:dataset:field_register" "field"
                                                "field-id" "fields.sql")
                                (transform-line "buyers" "list:dataset:buyer_list" "buyer"
                                                "buyer-id" "buyers.sql")
                                "changeset commit --yes" ""]))]
    (when-not (str/includes? (:out r) "\"committed\": true")
      (die "pipeline did not commit:\n" (:out r)))))

(defn tr-file
  [tr work nm]
  (let [out (str work "/" nm)]
    (spit out (pr-str (tr-data tr (edn/read-string (slurp (io/file farm nm))))))
    out))

(defn seed! [sutra tr work]
  (step "seed: media/hero/farm/seed.ent")
  (let [r (sutra [(tr-file tr work "seed.ent") "--yes"])]
    (when-not (zero? (:exit r)) (die "seed failed:\n" (:out r))))
  (step "refused sale: media/hero/farm/refused.ent")
  (let [r (sutra [(tr-file tr work "refused.ent") "--yes"])]
    (when (zero? (:exit r)) (die "the 40-bale sale was accepted; it must be refused"))
    (when-not (str/includes? (:out r) (tr "Not that many bales in the barn."))
      (die "refused for the wrong reason:\n" (:out r)))))

(defn north-meadow-id [sutra tr]
  (let [rows (get (json-after-brace (:out (sutra ["object" "query" "lot" "--json"]))) "rows")
        lot  (first (filter #(= (tr "North Meadow, 1st cut 2025")
                                   (get-in % ["values" "name"]))
                                rows))]
    (when-not (= 25 (get-in lot ["values" "in-barn"]))
      (die "seeded numbers are off: North Meadow, 1st cut 2025 must have 25 in the barn"))
    (get lot "id")))

(defn slug
  [title]
  (-> title str/lower-case str/trim (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+|-+$" "")))

(defn graph! [sutra title nodes & opts]
  (let [slug (slug title)
        r    (sutra (concat ["lineage-graph" "create" title]
                            (mapcat #(vector "--node" %) nodes) opts ["--yes"]))
        _    (when-not (zero? (:exit r)) (die "lineage graph " slug " not created:\n" (:out r)))
        out  (:out (sutra ["lineage-graph" "show" slug "--json"]))
        rid  (second (re-find #"\"id\": \"(ri\.[^\"]*)\"" out))]
    (or rid (die "no lineage graph " slug ":\n" out))))

(defn graphs! [sutra tr]
  (step "graphs")
  {:lineage (graph! sutra (tr "Hay barn") ["dataset/baling_log" "object-type/sale"]
                    "--ancestors" "all" "--descendants" "all")
   :model   (graph! sutra (tr "Farm lineage")
                    (concat (map #(str "datasource/" %) ["baler" "field-register" "buyer-list"])
                            (map #(str "dataset/" %) ["baling_log" "field_register" "buyer_list"])
                            (map #(str "transform/" %)
                                 ["hay_lots" "fields" "buyers" "link-sale-lot" "link-lot-field"
                                  "link-harvest-field" "link-harvest-buyer"])
                            (map #(str "object-type/" %) ["field" "buyer" "harvest" "lot" "sale"])
                            (map #(str "action/" %) ["add-field" "add-buyer" "plant"
                                                     "record-harvest" "sell-harvest"
                                                     "sell-hay"])))})

(defn arrange! [sutra slug arrangement]
  (let [r (sutra ["lineage-graph" "arrange" slug arrangement "--yes"])]
    (when-not (zero? (:exit r)) (die "lineage graph " slug " not arranged:\n" (:out r)))))

(defn check-prereqs []
  (doseq [[d hint] [["tests/frontend/node_modules/playwright-core" "tests/frontend"]
                    ["clients/frontend/node_modules" "clients/frontend"]]]
    (when-not (.isDirectory (io/file repo d)) (die "run (cd " hint " && npm install) first")))
  (doseq [t ["vhs" "ttyd" "ffmpeg" "nvim" "pyright-langserver" "bb"]]
    (when-not (zero? (:exit (sh/sh "sh" "-c" (str "command -v " t))))
      (die t " not found (needed for media/sdk/record)"))))

(defn gifs! []
  (step "editor GIFs → media/sdk/")
  (let [r (run-in {} ["media/sdk/record"])]
    (print (:out r)) (flush)
    (when-not (zero? (:exit r)) (die "media/sdk/record failed"))))

(defn run
  [lang serve?]
  (let [tr      (translator lang)
        screens (str repo "/media/screens" (when (not= lang "en") (str "/" lang)))
        hero    (str "sutra-hero-farm" (when (not= lang "en") (str "." lang)))
        work    (str (Files/createTempDirectory "readme-media" (make-array FileAttribute 0)))
        port    (free-port)
        base    (str "http://127.0.0.1:" port)
        env     {"SUTRA_HOME" (str work "/home") "SUTRA_SOLO_PAKET" "keins"
                 "SUTRA_HTTP_PORT" (str port)}
        log     (io/file work "solo.log")
        solo    (atom nil)
        stop    (fn [] (stop-solo @solo) (delete-tree (io/file work)))
        hook    (Thread. ^Runnable stop)]
    (.addShutdownHook (Runtime/getRuntime) hook)
    (.mkdirs (io/file work "home"))
    (step "[" lang "] sutra-solo on " base " (log: " log ")")
    (reset! solo (start-solo env log))
    (await-ready @solo log)
    (let [sutra (sutra-fn env base)
          _     (push-model! sutra tr work)
          _     (push-ingest! sutra tr work (get env "SUTRA_HOME"))
          _     (declare-transforms! sutra)
          _     (seed! sutra tr work)
          lot   (north-meadow-id sutra tr)
          {:keys [lineage model]} (graphs! sutra tr)
          _     (arrange! sutra (slug (tr "Farm lineage")) "hier-lr")
          shots ["node" "tests/frontend/readme-shots.mjs" base screens lot lineage model lang]]
      (if serve?
        (do (step "serving; shots: " (str/join " " shots))
            (.waitFor ^Process @solo))
        (do (step "[" lang "] screenshots → " screens)
            (.mkdirs (io/file screens))
            (let [r (run-in {} shots)]
              (print (:out r)) (flush)
              (when-not (zero? (:exit r)) (die "screenshots failed")))
            (step "[" lang "] hero")
            (let [r (run-in {} ["bin/hero" hero])]
              (print (:out r)) (flush)
              (when-not (zero? (:exit r)) (die "bin/hero failed")))
            (.removeShutdownHook (Runtime/getRuntime) hook)
            (stop))))))

(defn main [args]
  (check-prereqs)
  (let [serve? (some #{"--serve"} args)
        lang   (second (drop-while #(not= "--lang" %) args))
        langs  (cond lang   [lang]
                     serve? ["en"]
                     :else  (into ["en"]
                                  (sort (keep #(second (re-matches #"([a-z]{2})\.edn" %))
                                              (.list (io/file farm))))))]
    (doseq [l langs]
      (when-not (or (= l "en") (.exists (io/file farm (str l ".edn"))))
        (die "no dictionary media/hero/farm/" l ".edn")))
    (doseq [l langs] (run l serve?))
    (when-not serve? (gifs!))
    (println "✓ done")))

(try
  (main *command-line-args*)
  (shutdown-agents)
  (catch clojure.lang.ExceptionInfo e
    (if (:die (ex-data e)) (System/exit 1) (throw e))))
