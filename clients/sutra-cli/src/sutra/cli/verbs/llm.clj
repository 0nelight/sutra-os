(ns sutra.cli.verbs.llm
  (:require [sutra.cli.dispatch :as dispatch :refer [step!]]
            [sutra.cli.errors :as errors :refer [ok? req!]]
            [sutra.cli.live :refer [schema-sections]]
            [sutra.cli.out :refer [emit! emit-rows! fmt json? mark paint progress! render rpad
                                   say! show-lines warn!]]
            [sutra.kit.coerce :refer
             [->num0]]
            [sutra.cli.json :as json]
            [sutra.i18n.core :as i18n]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.types.eval-runs :as er]
            [sutra.types.llm-cost :as llm-cost]
            [sutra.types.llm-registration :as llm-reg]
            [sutra.wire.api :as api]))

(defn- eval-pct [x] (when (number? x) (fmt "%.1f%%" (* 100.0 (double x)))))

(defn- eval-num [x]
  (cond (nil? x) "—"
        (and (number? x) (<= 0 x 1)) (eval-pct x)
        (number? x) (fmt "%.2f" (double x))
        :else (str x)))

(defn- eval-status [status]
  (case (str status)
    "no-cases"        (paint :red "NO CASES")
    "no-intermediate" (paint :red "NOT EXPOSED")
    (paint :dim "—")))

(defn- eval-objective [{:keys [objective]}]
  (when-let [{:keys [min max expect]} objective]
    (str "per case "
         (if (some? expect)
           (str "= " expect)
           (str/join " " (remove nil? [(when min (str ">=" min)) (when max (str "<=" max))]))))))

(defn- eval-tally
  [{{:keys [hits n tp fp abstained] fneg :fn} :detail}]
  (cond (some? tp)        (str "tp " tp " · fp " fp " · fn " fneg)
        (some? abstained) (str abstained "/" n)
        (some? hits)      (str hits "/" n)
        (some? n)         (str "n " n)))

(defn- eval-metric-line
  [{:keys [name value variance status baseline delta pass-rate] :as m}]
  (let [arrow  (when (and delta (not (zero? delta)))
                 (paint (if (pos? delta) :green :red)
                        (str (if (pos? delta) "▲+" "▼") (fmt "%.0fpp" (* 100.0 (double delta))))))
        hi-var (>= (or variance 0) 0.03)
        obj    (eval-objective m)]
    (str "  " (rpad (str (clojure.core/name name) " ") 22)
         (rpad (if baseline (str (eval-num baseline) " → " (eval-num value)) (eval-num value)) 18)
         "  " (or arrow (rpad "" 8))
         "  " (rpad (eval-status status) 10)
         (when-let [t (eval-tally m)] (str "  " (paint :dim t)))
         (when pass-rate (str "  " (paint :dim (str "cases pass " (eval-pct pass-rate)))))
         (when-not (str/blank? obj) (str "  " (paint :dim (str "(" obj ")"))))
         (when (pos? (or variance 0))
           (str "  " (paint :dim (str "σ " (fmt "%.1fpp" (* 100.0 (double variance)))))))
         (when hi-var (str "  " (paint :yellow "⚠ high variance"))))))

(defn- short-val [v]
  (let [s (pr-str v)] (if (> (count s) 120) (str (subs s 0 117) "…") s)))

(defn- eval-case-line
  [{:keys [id passed metrics expected actual error intermediates]}]
  (let [failed (vec (for [[nm per] (sort-by key metrics)
                          :let [last-it (last per)]
                          :when (false? (:pass? last-it))]
                      (str (clojure.core/name nm) " " (short-val (:value last-it))
                           (when-let [d (:debug last-it)] (str " (" (short-val d) ")")))))]
    (cond-> {:id (str id) :status (str passed) :passed passed}
      error           (assoc :error (short-val error))
      (seq failed)    (assoc :failed failed)
      (false? passed) (assoc :expected (short-val expected)
                             :actual (short-val (if (and (map? actual) (map? expected))
                                                  (select-keys actual (map keyword (keys expected)))
                                                  actual)))
      (and (false? passed) (seq intermediates)) (assoc :steps (short-val intermediates)))))

(defn- case-line
  [{:keys [id passed error failed expected actual steps]}]
  (str "    " id "  "
       (case passed true (paint :green "PASS") false (paint :red "FAIL") (paint :dim "—"))
       (when error (str "  " (paint :red (str "error: " error))))
       (when (seq failed) (str "  " (str/join " · " failed)))
       (when (false? passed)
         (str "\n        expected " expected
              "\n        actual   " actual
              (when steps (str "\n        steps    " steps))))))

(defn- eval-captured-line
  [{:keys [model] :as c}]
  (let [v (er/captured-version c)]
    (str/join "  · " (cond-> []
                       v     (conj (str "version " v))
                       model (conj (str "model " model))))))

(defn- eval-header-lines
  [{:keys [suite ref iterations cases summary results-dataset run-id]}]
  (let [{:keys [passed failed ungraded pass-rate skipped-cases experiment meta]} summary]
    [(str (paint :bold (str suite)) "  " (paint :dim (str "run " run-id)))
     (str "  " cases " case(s) × " iterations " iteration(s) on " ref
          (when skipped-cases (paint :yellow (str "  · " skipped-cases
                                                  " object(s) without input skipped"))))
     (str "  cases: " (paint :green (str passed " passed")) " · "
          (paint (if (pos? (or failed 0)) :red :dim) (str failed " failed"))
          (when (pos? (or ungraded 0)) (paint :dim (str " · " ungraded " ungraded")))
          (when pass-rate (str "  →  " (paint :bold (eval-pct pass-rate)))))
     (when experiment (str "  " (paint :dim (str "experiment " (:name experiment) " "
                                                 (pr-str (:params experiment))))))
     (when-let [c (not-empty (:captured summary))]
       (str "  " (paint :dim (eval-captured-line c))))
     (when (seq meta) (str "  " (paint :dim (str "meta " (pr-str meta)))))
     (when results-dataset
       (str "  " (paint :dim (str (:written results-dataset) " row(s) → dataset "
                                  (:name results-dataset)))))]))

(defn- eval-run-lines [{:keys [metrics case-results] :as body}]
  (let [bad (filter #(false? (:passed %)) case-results)]
    [(eval-header-lines body)
     (map eval-metric-line metrics)
     (when (seq bad)
       [(str "\n  " (paint :bold "failing cases")
             (paint :dim (str " (first 15 of " (count bad) ")")))
        (map #(case-line (eval-case-line %)) (take 15 bad))
        (paint :dim (str "  every case: eval cases " (:run-id body)))])]))

(defn- eval-suite-arg
  [suite]
  (let [f (io/file suite)]
    (if (and (str/ends-with? (str suite) ".edn") (.exists f))
      (edn/read-string (slurp f))
      suite)))

(defn- eval-ref-short [r]
  (let [s (str (or r ""))] (if (> (count s) 12) (subs s 0 12) s)))

(defn- eval-run-row
  [{:keys [id ref run-at summary]}]
  (let [{:keys [passed total pass-rate captured experiment]} summary]
    {:id         (str id)
     :ref        (eval-ref-short ref)
     :run-at     (str (or run-at ""))
     :passed     passed
     :total      total
     :pass-rate  pass-rate
     :version    (er/captured-version captured)
     :model      (:model captured)
     :experiment (:params experiment)}))

(defn- eval-run-line
  [{:keys [id ref run-at passed total pass-rate version model experiment]}]
  (str "  " (rpad (str id) 38)
       (rpad (if (empty? (str ref)) "—" ref) 14)
       (rpad (str run-at) 30)
       (paint (if (and passed total (= passed total)) :green :yellow)
              (str (or passed "?") "/" (or total "?") " cases pass"
                   (when pass-rate (str " (" (eval-pct pass-rate) ")"))))
       (when version (str "  " version))
       (when model (str "  " (paint :dim model)))
       (when experiment (str "  " (paint :dim (pr-str experiment))))))

(defn- kv-pairs [s]
  (into {} (for [kv (remove str/blank? (str/split (str s) #","))
                 :let [[k v] (str/split kv #"=" 2)]]
             [(keyword (str/trim k)) (str/trim (str v))])))

(defn- grid-value
  [s]
  (cond (re-matches #"-?\d+(\.\d+)?" s) (or (parse-long s) (parse-double s))
        (#{"true" "false"} s)            (= "true" s)
        :else                            s))

(defn- grid-params [s]
  (update-vals (kv-pairs s) #(mapv grid-value (str/split % #"\|"))))

(defn- run-cases [run]
  (req! "eval cases" (api/get! :eval-run-cases {:id run})))

(defn- stored-metrics
  [run rows]
  (if (seq rows)
    (mapv (fn [m] (merge (:detail m) {:name (keyword (:metric m)) :value (:value m)
                                      :variance (:variance m) :status (:status m)}))
          rows)
    (mapv (fn [[k v]] {:name (keyword k) :value v}) (get-in run [:summary :by-metric]))))

(defn- run-label [{:keys [id run-at summary] :as run}]
  (let [{:keys [passed total pass-rate]} summary]
    (str (eval-ref-short id) "  " (or run-at "") "  " (or passed "?") "/" (or total "?")
         (when pass-rate (str " (" (eval-pct pass-rate) ")"))
         (when-let [v (er/run-version run)] (str "  " v))
         (when-let [m (er/run-model run)] (str "  " m)))))

(defn- compare-metrics
  [ma mb]
  (let [by-a (into {} (map (juxt :name identity)) ma)
        names (distinct (concat (map :name ma) (map :name mb)))]
    (vec (for [nm names
               :let [x (get by-a nm) y (some #(when (= nm (:name %)) %) mb)
                     a (:value x) b (:value y)]]
           (assoc (or y {:name nm})
                  :baseline (if (some? a) a "—")
                  :delta (when (and (number? a) (number? b)) (- b a)))))))

(defn- compare-runs
  [ra rb]
  (let [ca (into {} (map (juxt :case-id identity)) (:cases ra))
        cb (into {} (map (juxt :case-id identity)) (:cases rb))
        ids (distinct (concat (keys ca) (keys cb)))
        ka (get-in ra [:run :summary :captured])
        kb (get-in rb [:run :summary :captured])]
    (into
     (vec (for [k [:branch :version :model]
                :let [f (if (= k :version) er/captured-version k)
                      x (f ka) y (f kb)]
                :when (not= x y)]
            {:id (name k) :status "captured" :from x :to y}))
     (for [id ids
           :let [x (get ca id) y (get cb id)
                 pa (:passed x) pb (:passed y)]
           :when (or (not= pa pb) (not= (:actual x) (:actual y)))]
       {:id       (str id)
        :status   (str pa "→" pb)
        :passed-a pa
        :passed-b pb
        :actual-a (short-val (:actual x))
        :actual-b (short-val (:actual y))}))))

(defn- compare-line
  [{:keys [id status from to passed-a passed-b actual-a actual-b]}]
  (str "    " id "  "
       (if (= "captured" status)
         (paint :bold (str from " → " to))
         (str (case [passed-a passed-b]
                [true false] (paint :red "PASS → FAIL")
                [false true] (paint :green "FAIL → PASS")
                (paint :dim (str passed-a " → " passed-b)))
              "\n        " actual-a
              "\n     →  " actual-b))))

(defn- compare-stored!
  [ctx suite-runs a b]
  (let [ra (run-cases a) rb (run-cases b)
        hs (into {} (for [sid (distinct (keep #(get-in % [:run :suite-id]) [ra rb]))
                          r (suite-runs sid)]
                      [(:id r) (:metrics r)]))
        ma (stored-metrics (:run ra) (get hs a))
        mb (stored-metrics (:run rb) (get hs b))
        ms (compare-metrics ma mb)
        rows (compare-runs ra rb)
        n  (fn [st] (count (filter #(= st (:status %)) rows)))
        tally {:regressions (n "true→false") :fixes (n "false→true")
               :output-only (count (filter #(#{"true→true" "false→false"} (:status %)) rows))}]
    (emit! ctx {:a (assoc (:run ra) :id a) :b (assoc (:run rb) :id b)
                :metrics ms :tally tally :rows rows}
           ::compare)
    {:kind :objects :rows rows}))

(defmethod render ::compare [_ {:keys [a b metrics tally rows]}]
  [(str "  " (paint :dim "a ") (run-label a))
   (str "  " (paint :dim "b ") (run-label b))
   ""
   (map eval-metric-line metrics)
   (str "\n  " (paint :red (str (:regressions tally) " PASS → FAIL"))
        " · " (paint :green (str (:fixes tally) " FAIL → PASS"))
        " · " (paint :dim (str (:output-only tally) " output changed, verdict kept")))
   (map compare-line rows)])

(def ^:private spark-chars "▁▂▃▄▅▆▇█")

(defn- sparkline [vs]
  (let [ns (filter number? vs) lo (when (seq ns) (apply min ns)) hi (when (seq ns) (apply max ns))]
    (apply str (for [v vs]
                 (cond (not (number? v)) " "
                       (== lo hi)        (nth spark-chars 3)
                       :else (nth spark-chars
                                  (min 7 (int (* 8 (/ (- v lo) (double (- hi lo))))))))))))

(defn- metric-series
  [runs]
  (let [vals-of (fn [r] (into {"pass-rate" (get-in r [:summary :pass-rate])}
                              (map (fn [m] [(clojure.core/name (:name m)) (:value m)]))
                              (stored-metrics r (:metrics r))))
        per     (mapv vals-of runs)
        names   (distinct (mapcat keys per))]
    (vec (for [nm names
               :let [vs (mapv #(get % nm) per) ns (filter number? vs)]]
           {:metric nm :values vs :first (first ns) :last (last ns)
            :min (when (seq ns) (apply min ns)) :max (when (seq ns) (apply max ns))
            :mean (er/mean ns)}))))

(defn- history-groups [by runs series]
  (let [names (map :metric series)]
    (vec (for [[g rs] (er/groups by runs)
               :let [s (metric-series rs)]]
           {:group (or g "—") :runs (count rs) :newest (:run-at (first rs))
            :means (into {} (for [nm names]
                              [nm (:mean (some #(when (= nm (:metric %)) %) s))]))}))))

(defn- eval-history! [ctx suite-runs suite by experiment]
  (let [{:keys [suites]} (req! "eval suites" (api/get! :eval-suites))
        row (first (filter #(= (str (:name %)) (str suite)) suites))]
    (if-not row
      (do (warn! (mark :yellow "⏸" (str "no eval suite named " suite " — try `eval suites`")))
          nil)
      (let [runs   (cond->> (suite-runs (:id row))
                     experiment (filterv #(= experiment
                                             (get-in % [:summary :experiment :name]))))
            rows   (mapv eval-run-row runs)
            series (metric-series (reverse runs))
            groups (when by (history-groups by runs series))]
        (emit! ctx (cond-> {:rows rows :trend series} groups (assoc :by by :groups groups))
               ::history)
        {:kind :objects :rows rows}))))

(defmethod render ::history [_ {:keys [rows trend by groups]}]
  (if (empty? rows)
    "  no runs yet — score it with `eval <suite>`"
    [(map eval-run-line rows)
     (str "\n  " (paint :bold "course")
          (paint :dim (str "  " (count rows) " run(s), oldest → newest")))
     (let [w (reduce max 22 (map (comp inc count :metric) trend))]
       (for [{:keys [metric values] :as m} trend]
         (str "  " (rpad metric w) (sparkline values) "  "
              (eval-num (:first m)) " → " (paint :bold (eval-num (:last m)))
              (paint :dim (str "   range " (eval-num (:min m)) " … " (eval-num (:max m)))))))
     (when groups
       [(str "\n  " (paint :bold (str "mean by " by)))
        (for [{:keys [group newest means] n :runs} groups]
          (str "  " (rpad group 28) (rpad (str n " run(s)") 10)
               (str/join "  " (for [{nm :metric} trend] (str nm " " (eval-num (get means nm)))))
               (paint :dim (str "  newest " newest))))])]))

(def ^:private eval-poll-ms 1000)

(defn- eval-job-result
  [id]
  (progress! (paint :dim "  scoring "))
  (loop []
    (let [j (req! "eval job" (api/get! :eval-job {:id id}))]
      (case (:status j)
        "running" (do (progress! (paint :dim "."))
                      (Thread/sleep eval-poll-ms)
                      (recur))
        "done"    (do (warn!) (:result j))
        (do (warn!)
            (throw (ex-info (str "eval job " id " " (:status j) ": " (:error j)) j)))))))

(defmethod step! :eval
  [ctx [_ {:keys [op suite iterations parallel diff model version meta grid experiment run failing?
                  detach? again? by preview?]}]]
  (let [suite-runs #(:results (req! "eval history" (api/get! :eval-results {:id %})))]
    (case (or op :run)
      :suites
      (let [{:keys [suites]} (req! "eval suites" (api/get! :eval-suites))]
        (emit-rows! ctx (vec (for [{:keys [name transform declared latest]} suites
                                   :let [s (:summary latest)]]
                               {:id          (str name)
                                :transform   (str transform)
                                :declared    (boolean declared)
                                :last-passed (:passed s)
                                :last-total  (:total s)}))
                    "no eval suites"))

      :history
      (eval-history! ctx suite-runs suite by experiment)

      :save
      (let [spec (eval-suite-arg suite)]
        (emit! ctx (req! "eval save"
                         (api/post! :eval-suites {:name (:name spec) :spec spec})))
        nil)

      :cases
      (let [found (run-cases run)
            _     (when-not (or (:run found) (seq (:cases found)))
                    (throw (ex-info (str "no eval run " run " — a JOB id from `eval … --detach`"
                                         " is read with `eval job " run "`, the runs of a"
                                         " suite are listed by `eval history <suite>`")
                                    {:refused :unknown-run :id run})))
            body (cond-> (assoc found :run-id run)
                   failing? (update :cases #(filterv (fn [c] (false? (:passed c))) %)))]
        (emit! ctx body ::cases)
        {:kind :objects :rows (mapv #(eval-case-line (assoc % :id (:case-id %))) (:cases body))})

      :compare
      (last (mapv #(compare-stored! ctx suite-runs (first diff) %) (rest diff)))

      :job
      (let [j (req! "eval job" (api/get! :eval-job {:id run}))]
        (if (= "done" (:status j))
          (emit! ctx (:result j) ::run)
          (emit! ctx (merge {:id run} j) ::job)))

      (let [start   {:suite suite :iterations iterations :parallel parallel :model model
                     :version version :meta meta :grid grid :experiment experiment}
            request (cond-> {:suite (eval-suite-arg suite)}
                      iterations (assoc :iterations iterations)
                      parallel   (assoc :parallel parallel)
                      model      (assoc :model model)
                      version    (assoc :version version)
                      meta       (assoc :meta (kv-pairs meta))
                      grid       (assoc :experiment {:name (or experiment "grid")
                                                     :params (grid-params grid)}))]
        (if preview?
          (do (emit! ctx (req! "eval preview" (api/post! :eval-run (assoc request :preview true)))
                     ::preview)
              nil)
          (let [_    (dispatch/guard-restart! :eval start again?)
                job  (req! "eval" (api/post! :eval-run request))
                _    (dispatch/remember-start! :eval start (:id job))
                body (if detach? job (eval-job-result (:id job)))]
            (if detach?
              (emit! ctx (assoc job :job-id (:id job) :await (dispatch/await-line (:id job))
                                :read (str "eval job " (:id job)))
                     ::job-started)
              (emit! ctx body ::eval-result))
            {:kind :objects
             :rows (mapv eval-case-line
                         (remove #(true? (:passed %)) (:case-results body)))}))))))

(defmethod render ::preview [_ {:keys [experiment combinations runs answers]}]
  [(paint :bold (str "experiment " experiment " · " runs " run(s) · " answers " answer(s)"))
   (for [c combinations] (str "  " (pr-str c)))])

(defmethod render ::cases [_ {:keys [run cases run-id]}]
  [(when run (eval-header-lines (assoc run :suite (:suite-id run)
                                       :cases (get-in run [:summary :case-count])
                                       :run-id run-id)))
   (map #(case-line (eval-case-line (assoc % :id (:case-id %)))) cases)])

(defmethod render ::run [_ body] (eval-run-lines body))

(defmethod render ::job [_ {:keys [id status error]}]
  (str "  " id "  " status (when error (str " — " error))))

(defmethod render ::job-started [_ {:keys [id]}]
  (str "  " (paint :bold id) "  "
       (paint :dim (str "running — a JOB id, not a run: wait with `" (dispatch/await-line id)
                        "`, read with `eval job " id "`"))))

(defmethod render ::eval-result [_ body]
  (if-let [runs (:runs body)]
    [(paint :bold (str "experiment " (get-in body [:experiment :name])
                       " · " (count runs) " run(s)"))
     (for [r runs]
       [(str "  " (pr-str (get-in r [:summary :experiment :params]))
             "  " (get-in r [:summary :passed]) "/"
             (get-in r [:summary :total]) " cases pass"
             (when-let [p (get-in r [:summary :pass-rate])]
               (str " (" (eval-pct p) ")"))
             (when-let [c (get-in r [:summary :usage :cost])]
               (str "  " (fmt "%.4f" (double c)) " "
                    (get-in r [:summary :usage :currency])))
             "  " (paint :dim (str "run " (:run-id r))))
        (map #(str "  " (eval-metric-line %)) (:metrics r))])]
    (eval-run-lines body)))

(defmethod step! :agent-list [ctx [_ _]]
  (let [{:keys [agents]} (req! "list agents" (api/get! :agents))]
    (emit! ctx {:agents (vec (sort-by (comp str :agent-name) (vals agents)))}))
  nil)

(defmethod step! :agent-show [ctx [_ {nm :name}]]
  (let [{:keys [agents]} (req! "list agents" (api/get! :agents))
        spec (get agents nm)]
    (if (nil? spec)
      (warn! (mark :yellow "⏸" (str (i18n/t :cli/no-such-agent {:name (clojure.core/name nm)})
                                    " — try `agent list`")))
      (emit! ctx spec ::pprint)))
  nil)

(defmethod render ::pprint [_ v] (str/split-lines (with-out-str (pprint/pprint v))))

(defmethod step! :agent-run [ctx [_ {:keys [name input max-turns]}]]
  (when (str/blank? (str input))
    (throw (ex-info (i18n/t :cli/agent-run-needs-input) {:agent name})))
  (let [r (req! (str "run agent " (clojure.core/name name))
                (api/post! :agent-invoke {:name name}
                           (cond-> {:input input} max-turns (assoc :max-turns max-turns))))]
    (emit! ctx r ::agent-answer))
  nil)

(defmethod render ::agent-answer [_ r] [(show-lines (dissoc r :answer)) (str (:answer r))])

(defn- schema-llm-usage [body]
  (let [lm-models (fn [spec] (keep (fn [[_ in]] (when (= :language-model (:kind in)) (:model in)))
                                   (:inputs spec)))
        tx (for [[_ s] (:transforms body), model (lm-models s)]
             {:plane "batch"        :model model
              :where (str (if (:function-name s) "function:" "transform:")
                          (clojure.core/name (or (:function-name s) (:transform-name s))))})
        ag (for [[_ s] (:agents body) :when (:model s)]
             {:plane "interactive"  :model (:model s)
              :where (str "agent:" (clojure.core/name (:agent-name s)))})
        ss (for [[_ s] (:object-types body) :when (get-in s [:semantic-search :model])]
             {:plane "embedding"    :model (get-in s [:semantic-search :model])
              :where (str "object:" (clojure.core/name (:object-type-name s)))})]
    (sort-by (juxt :plane :where) (concat tx ag ss))))

(defmethod step! :llm-usage [ctx [_ _]]
  (emit! ctx {:usage (vec (schema-llm-usage (schema-sections :agents :object-types :transforms)))})
  nil)

(defmethod step! :llm-next [ctx [_ _]]
  (let [{:keys [schedules]} (req! "schedules" (api/get! :schedules))
        budget  (errors/optional "llm budget" #(api/get! :llm-budget))
        llm?    #(seq (:llm-transforms %))
        _       (when (and (seq schedules) (not-any? #(contains? % :llm-transforms) schedules))
                  (throw (ex-info (str "this instance's /api/schedules does not report build "
                                       "scopes yet (no :llm-transforms field) — `llm next` needs "
                                       "the backend deployed with it. `llm usage` + `schedule "
                                       "list` are the two halves in the meantime.")
                                  {:verb :llm-next})))
        touching (filter llm? schedules)
        armed   (->> touching (remove :paused) (sort-by #(or (:next-fire-at %) "9999")))
        paused  (->> touching (filter :paused) (sort-by :name))]
    (emit! ctx {:armed (vec armed) :paused (vec paused) :budget budget}))
  nil)

(defn- read-dataset-rows
  [ds-name]
  (let [rid  (->> (:datasets (req! "datasets" (api/get! :datasets)))
                  (some (fn [[rid d]] (when (= ds-name (:dataset-name d)) rid))))
        prev (when rid (api/get! :dataset-preview {:id rid}))]
    [(if (and prev (ok? prev)) (mapv :values (-> prev :body :rows)) [])
     rid]))

(defn- llm-cost!
  [ctx {:keys [per calls? since until today? caller run]}]
  (let [body (req! "llm cost" (api/get! :llm-cost (cond-> {}
                                                    per     (assoc :per per)
                                                    calls?  (assoc :calls true)
                                                    today?  (assoc :today true)
                                                    (seq since)  (assoc :since since)
                                                    (seq until)  (assoc :until until)
                                                    (seq caller) (assoc :caller caller)
                                                    (seq run)    (assoc :run run))))
        {:keys [unpriced-models unpriced-earliest]} body]
    (emit! ctx body)
    (when-not (json? ctx)
      (doseq [m (sort unpriced-models)
              :let [earliest (get unpriced-earliest m (get unpriced-earliest (keyword (str m))))]]
        (say! ctx (str "llm reprice " m " --in <$/1M> --out <$/1M>"
                       (when earliest (str " --effective " earliest))))))
    nil))

(defmethod step! :build-estimate [ctx [_ {:keys [only strategy from days calls-per-row]}]]
  (let [{:keys [transforms total] :as body}
        (req! "build estimate" (api/build-estimate {:only only :strategy strategy :from from
                                                    :days days :calls-per-row calls-per-row}))]
    (emit! ctx body)
    (when-not (json? ctx)
      (when-let [c (seq (remove nil? (:ceiling-only total)))]
        (say! ctx (mark :yellow "⚠"
                        (str "noch ohne Messung (OFFEN = Obergrenze des Anti-Joins): "
                             (str/join ", " c)))))
      (when-let [m (seq (remove nil?
                                (for [e transforms :when (:hash-mismatch? e)] (:transform e))))]
        (say! ctx
              (mark :red "✗"
                    (str "prompt-key-Hash nirgends im eigenen Output — Stempel-Spalte oder "
                         "prompt_hash(row) fehlt: "
                         (str/join ", " m)))))
      (when-let [d (seq (remove nil? (:delta-based total)))]
        (say! ctx
              (mark :yellow "⚠"
                    (str "ohne done_key — @transform(done_key=\"<input>.<spalte>\", max_per_run=N) "
                         "deklarieren: "
                         (str/join ", " d))))))
    nil))

(defn- audit-json-pretty
  [s]
  (if (str/blank? (str s))
    "(none)"
    (try (with-out-str (json/pprint (json/read-str (str s))))
         (catch Exception _ (str s)))))

(defn- llm-audit!
  [ctx {:keys [request caller model errors? since until today? limit]}]
  (if-not (str/blank? (str request))
    (let [resp (api/get! :llm-audit {:request request})
          r    (if (= 404 (:status resp)) {} (req! (str "llm audit " request) resp))]
      (if-let [row (:call r)]
        (let [v (select-keys row [:request-id :ts :caller :resource :run-id :source
                                  :model :prompt-tokens :completion-tokens :latency-ms
                                  :error :input-sha :input-json :output-json])]
          (emit! ctx v ::audit-call)
          {:kind :object :object row})
        (warn! (mark :red "✗"
                     (i18n/t :cli/no-audit-row {:tabelle "llm_call_log" :id (str request)})))))
    (do (emit! ctx (req! "llm audit" (api/get! :llm-audit (cond-> {}
                                                            today?       (assoc :today true)
                                                            (seq since)  (assoc :since since)
                                                            (seq until)  (assoc :until until)
                                                            (seq caller) (assoc :caller caller)
                                                            (seq model)  (assoc :model model)
                                                            errors?      (assoc :errors true)
                                                            limit        (assoc :limit limit)))))
        nil)))

(defmethod render ::audit-call [_ v]
  [(show-lines (dissoc v :input-json :output-json))
   (audit-json-pretty (:input-json v))
   (audit-json-pretty (:output-json v))])

(defmethod step! :audit [ctx [_ a]] (llm-audit! ctx a))

(defn- login-audit!
  [ctx {:keys [user outcome failed? provider limit]}]
  (let [params (cond-> {}
                 (not (str/blank? (str user)))     (assoc :user user)
                 (not (str/blank? (str provider))) (assoc :provider provider)
                 failed?                           (assoc :outcome "failure")
                 (not (str/blank? (str outcome)))  (assoc :outcome outcome)
                 limit                             (assoc :limit limit))
        events (vec (:events (req! "login audit" (api/get! :login-audit params))))]
    (emit! ctx {:count (count events) :events events}))
  nil)

(defmethod step! :login-audit [ctx [_ a]] (login-audit! ctx a))

(defn- live-register
  []
  (:llm-models (schema-sections :llm-models)))

(defn- llm-register!
  [ctx {:keys [name in out currency source provider window tool-calling?]
        provider-model :provider-model}]
  (let [m    {:id name :in in :out out :currency currency :source source :provider provider
              :provider-model provider-model :context-window window
              :capabilities (when tool-calling? [:tool-calling])}
        bad  (llm-reg/problems m)
        body (llm-reg/registration m)]
    (when (some #{:id} bad)
      (throw
       (ex-info "llm register <id> --in <$/1M> --out <$/1M> — id, --in and --out are required (no priceless registration)."
                {:name name})))
    (doseq [[k v] [[:in in] [:out out]] :when (some #{k} bad)]
      (throw (ex-info (str "llm: --" (clojure.core/name k) " must be a non-negative number "
                           "(currency per 1,000,000 tokens), got " (pr-str v)) {})))
    (req! "register" (api/mutate :create-llm-model body {:stage? true}))
    (req! "commit" (api/post! :staging))
    (emit! ctx {:registered (:name body) :in (:in body) :out (:out body)
                :currency (:currency body)})
    nil))

(defn- llm-sync-rates!
  [ctx {:keys [source apply?]}]
  (let [src    (some-> source str/trim not-empty)
        resp   (req! (str "llm catalog " (or src "(the server's)"))
                     (api/get! :llm-catalog (when src {:source src})))
        models (:models resp)
        reg    (live-register)
        rows   (for [{:keys [id in out currency]} (sort-by :id models)
                     :let [cur   (get reg (str id))
                           was   (when cur [(->num0 (:in cur)) (->num0 (:out cur))])
                           now   [(double in) (double out)]
                           state (cond (nil? cur)      :new
                                       (not= was now)  :changed
                                       :else           :same)]]
                 {:id id :in (double in) :out (double out)
                  :currency (or (not-empty (str currency)) llm-cost/default-currency)
                  :state state :was was})
        changed (filter #(#{:new :changed} (:state %)) rows)
        ccy     (or (:currency (first rows)) llm-cost/default-currency)
        catalog-ids (set (map (comp str :id) models))
        deprecate  (for [[mid spec] reg
                         :when (and (not (contains? catalog-ids (str mid)))
                                    (not (:deprecated spec)))]
                     {:id (clojure.core/name mid)
                      :in (->num0 (:in spec)) :out (->num0 (:out spec))
                      :currency (or (not-empty (str (:currency spec))) ccy)})
        undeprecate (for [{:keys [id] :as r} rows
                          :when (:deprecated (get reg (str id)))]
                      r)
        pending (concat changed undeprecate deprecate)
        entry  (fn [{:keys [id in out currency]} deprecated]
                 (llm-reg/registration
                  (merge (get reg (keyword (str id)) (get reg (str id)))
                         {:id (str id) :in in :out out :currency currency
                          :deprecated deprecated})))
        wrote?  (when (and apply? (seq pending))
                  (doseq [r (concat changed undeprecate)]
                    (req! (str "register " (:id r))
                          (api/mutate :create-llm-model (entry r false) {:stage? true})))
                  (doseq [r deprecate]
                    (req! (str "deprecate " (:id r))
                          (api/mutate :create-llm-model (entry r true) {:stage? true})))
                  (req! "commit" (api/post! :staging))
                  true)]
    (emit! ctx {:source src :apply (boolean apply?) :models (vec rows)
                :to-write (count changed)
                :written (if wrote? (count pending) 0)
                :deprecate (mapv :id deprecate)
                :already (count (filter #(= :same (:state %)) rows))})
    (when (and (seq pending) (not wrote?) (not (json? ctx)))
      (say! ctx (mark :yellow "⚠"
                      (str (i18n/t :cli/n-changes-pending {:n (count pending)}) " --apply "
                           (i18n/t :cli/to-commit-them)))))
    nil))

(defn- llm-reprice!
  [ctx {:keys [name in out currency effective reason]}]
  (when (or (str/blank? (str name)) (nil? in) (nil? out) (str/blank? (str effective)))
    (throw
     (ex-info "llm reprice <id> --in <$/1M> --out <$/1M> --effective <ISO-date> [--reason] — id, --in, --out and --effective are required."
              {:name name})))
  (emit! ctx (req! "reprice" (api/post! :llm-rates (cond-> {:model name :in in :out out
                                                            :effective-from (str/trim effective)}
                                                     currency (assoc :currency currency)
                                                     reason   (assoc :reason reason)))))
  nil)

(defn- llm-unregister!
  [ctx {:keys [name]}]
  (when (str/blank? (str name))
    (throw (ex-info (i18n/t :cli/unregister-needs-id) {})))
  (req! "unregister" (api/mutate :delete-llm-model {:name name} {:stage? true}))
  (req! "commit" (api/post! :staging))
  (emit! ctx {:unregistered name})
  nil)

(defn- llm-models!
  [ctx {:keys [history?]}]
  (emit! ctx (cond-> {:register (live-register)}
               history? (assoc :history (vec (first (read-dataset-rows :llm_model_rates))))))
  nil)

(defn- llm-budget!
  [ctx {s :set reserve :reserve project :project source :source quota :quota resume? :resume?}]
  (let [q    (when (some? quota) (when-not (#{"none" "-" ""} (str quota)) (->num0 quota)))
        body (cond-> {}
               (and source resume?) (assoc :source (name source) :resume true)
               (some? s)       (assoc :eur (->num0 s))
               (some? reserve) (assoc :reserve-pct (->num0 reserve))
               (and project (some? quota)) (assoc :project (name project) :quota q)
               (and source (some? quota))  (assoc :source (name source) :quota q))]
    (emit! ctx (if (seq body)
                 (req! "set llm budget" (api/post! :llm-budget body))
                 (req! "llm budget" (api/get! :llm-budget)))))
  nil)

(defmethod step! :llm [ctx [_ {:keys [sub] :as a}]]
  (case (some-> sub str/lower-case)
    ("models" "list" "ls" nil "") (llm-models! ctx a)
    "register"                    (llm-register! ctx a)
    "reprice"                     (llm-reprice! ctx a)
    "unregister"                  (llm-unregister! ctx a)
    "cost"                        (llm-cost! ctx a)
    "budget"                      (llm-budget! ctx a)
    ("sync-rates" "sync")         (llm-sync-rates! ctx a)
    (throw (ex-info (str "unknown `llm` subcommand " (pr-str sub)
                         " — one of: models | register | reprice | unregister | cost | sync-rates")
                    {:sub sub}))))
