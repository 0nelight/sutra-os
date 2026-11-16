(ns sutra.cli.verbs.diagnose
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.json :as json]
            [sutra.cli.out :refer [emit! paint render rpad say! warn!]]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defn- trace-squish
  [sql]
  (when sql
    (let [s (-> (str sql) (str/replace #"\s+" " ") str/trim)]
      (if (> (count s) 160) (str (subs s 0 160) "…") s))))

(defn- sql-size
  [n]
  (cond (< n 1000)    (str n "c")
        (< n 1000000) (str (quot n 1000) "kc")
        :else         (format "%.1fMc" (/ (double n) 1e6))))

(defn- trace-shape-tag
  [{:keys [chars folds] r :repeat}]
  (let [parts (cond-> []
                (> (long (or chars 0)) 160)  (conj (sql-size (long chars)))
                (pos? (long (or folds 0)))   (conj (str folds " folds"))
                (> (long (or r 1)) 1)        (conj (str "×" r " inlined")))]
    (when (seq parts) (str " [" (str/join " · " parts) "]"))))

(defn- trace-shape-paint
  [{:keys [chars] r :repeat}]
  (if (or (> (long (or r 1)) 1) (> (long (or chars 0)) 100000)) :red :dim))

(defn- trace-sql
  [{:keys [sql sql-shape]}]
  (str (trace-squish sql)
       (when-let [tag (trace-shape-tag sql-shape)]
         (paint (trace-shape-paint sql-shape) tag))))

(defn- say-lines!
  [ctx ls]
  (run! (partial say! ctx) (remove nil? (flatten [ls]))))

(defn- trace-event-line [e]
  (let [ms (:ms e)
        tail (cond (:failed e)              (paint :red (str "✗ " (:error e)))
                   (some? (:update-count e)) (paint :dim (str (:update-count e) "×"))
                   (= "resultset" (:rows e)) (paint :dim "→rows")
                   :else "")
        tail (cond-> tail
               (:explainable e) (str "  " (paint :yellow (str "--explain " (:seq e)))))]
    (str "  " (paint :cyan (rpad (str (or (:conn e) "?")) 22)) " "
         (paint (cond (:failed e) :red
                      (and ms (> ms 1000)) :yellow
                      :else :green)
                (rpad (str (or ms "?") "ms") 8))
         "  " (trace-sql e)
         (when (seq (str tail)) (str "  " tail)))))

(defn- trace-inflight-lines [live]
  (for [e live]
    (str "  " (paint :yellow (str "▶ " (i18n/t :cli/running) " "))
         (paint :yellow (rpad (str (:running-ms e) "ms") 8))
         " " (paint [:bold :yellow] (str "#" (:seq e)))
         " " (paint :cyan (str (:conn e))) "  "
         (trace-sql e))))

(defn- stuck-lines
  [{:keys [requests expired deadline-ms concurrency permits-free]}]
  [(str "  "
        (paint :dim
               (str "deadline " deadline-ms "ms · "
                    (- (long (or concurrency 0)) (long (or permits-free 0)))
                    "/" concurrency " slots busy")))
   (if (empty? requests)
     (str "  " (paint :dim "nothing in flight"))
     (for [{:keys [incident method uri running-ms state stack]} requests
           :let [slow? (> (long (or running-ms 0)) 5000)]]
       [(str "  " (paint (if slow? :yellow :green) (rpad (str running-ms "ms") 9))
             (paint :dim (rpad (str incident) 8))
             (paint :bold (str method " " uri))
             (when (= "expired" state) (paint :red "  EXPIRED")))
        (str "      " (paint :cyan (or (first stack) "no sutra frame")))
        (for [f (take 7 (rest stack))]
          (str "        " (paint :dim (str "<- " f))))]))
   (when (seq expired)
     [(str "  " (paint :dim "— past the deadline —"))
      (for [{:keys [incident method uri ran-ms stack]} (take 10 expired)]
        [(str "  " (paint :red (rpad (str ran-ms "ms") 9))
              (paint :dim (rpad (str incident) 8))
              (paint :bold (str method " " uri)))
         (str "      " (paint :cyan (or (first stack) "no sutra frame")))
         (for [f (take 5 (rest stack))]
           (str "        " (paint :dim (str "<- " f))))])])])

(defn- ttl-tag
  [expires-in-ms]
  (when-let [ms (some-> expires-in-ms long)]
    (paint :dim (str "  · stoppt in " (quot ms 60000) "min"))))

(defn- trace-coverage-lines
  [cov]
  (when-let [{:keys [covered window-ms busy-ms spans]} cov]
    (if (zero? (long (or spans 0)))
      (str "    "
           (paint :dim
                  (i18n/t :cli/no-traffic-in
                          {:sek (format "%.1f" (/ window-ms 1000.0))})))
      (let [pct  (* 100.0 covered)
            low? (< covered 0.60)]
        [(str "    " (paint :dim (str (i18n/t :cli/coverage) " "))
              (paint (if low? :yellow :green) (format "%.1f%%" pct))
              (paint :dim (str "  ("
                               (i18n/t :cli/coverage-detail
                                       {:busy (format "%.1f" (/ busy-ms 1000.0))
                                        :fenster (format "%.1f" (/ window-ms 1000.0))})
                               ")")))
         (when low?
           (paint :yellow (str "    ⚠ " (i18n/t :cli/coverage-low))))]))))

(defn- trace-status-lines [side label]
  (when side
    [(str "  " (paint :bold label) ": "
          (if (:armed side) (paint :green (i18n/t :cli/armed))
              (paint :dim (i18n/t :cli/off)))
          (when (:armed side) (ttl-tag (:expires-in-ms (:lifecycle side))))
          (when-let [f (:file side)] (paint :dim (str "  → " f)))
          (paint :dim
                 (str "  (" (i18n/t :cli/n-in-flight {:n (count (:inflight side))}) ")")))
     (trace-coverage-lines (:coverage side))]))

(defn- bytes-h
  [n]
  (let [n (long (or n 0))]
    (cond (< n 1024)    (str n "B")
          (< n 1048576) (format "%.0fkB" (/ (double n) 1024))
          :else         (format "%.1fMB" (/ (double n) 1048576)))))

(defn- profile-bar
  [pct]
  (let [n (int (Math/round (/ (double (or pct 0)) 5.0)))]
    (paint :dim (str (apply str (repeat n "█")) (apply str (repeat (- 20 n) "·"))))))

(defn- profile-cpu-lines
  [{:keys [cpu-ms by-layer by-self-frame by-thread]}]
  [(str "  " (paint :bold (str "CPU " (or cpu-ms 0) "ms"))
        (paint :dim " (sampled, on-CPU only)"))
   (when (seq by-layer)
     [(paint :dim "  — by domain —")
      (for [{:keys [category cpu-ms pct]} by-layer]
        (str "  " (profile-bar pct) " "
             (paint :cyan (rpad (str category) 11))
             (rpad (str cpu-ms "ms") 10) (paint :dim (str pct "%"))))])
   (when (seq by-self-frame)
     [(paint :dim "  — where the time was SPENT (self frame) —")
      (for [{:keys [label cpu-ms pct]} by-self-frame]
        (str "    " (rpad (str cpu-ms "ms") 9) (paint :dim (rpad (str pct "%") 7)) " "
             label))])
   (when (seq by-thread)
     [(paint :dim "  — by thread —")
      (for [{:keys [thread cpu-ms pct]} by-thread]
        (str "    " (rpad (str cpu-ms "ms") 9) (paint :dim (rpad (str pct "%") 7)) " "
             thread))])])

(defn- size-tag
  [[k v]]
  (let [n (long (or v 0))]
    (if (= :bytes k) (bytes-h n) (str n " " (name k)))))

(defn- profile-marker-lines
  [rows]
  (when (seq rows)
    [(paint :dim "  — markers (rows and bytes, which no stack carries) —")
     (for [{:keys [marker count total-ms sizes]} rows]
       (str "    " (paint :cyan (rpad (str marker) 22))
            (rpad (str count "×") 7)
            (rpad (str total-ms "ms") 10)
            (when (seq sizes)
              (paint :yellow (str/join " · " (map size-tag sizes))))))]))

(defn- profile-status-line
  [{:keys [armed interval-ms depth threads samples markers dropped-markers elapsed-ms
           cpu-time expires-in-ms]}]
  (str "  " (if armed (paint :green "scharf") (paint :dim "aus"))
       (when armed (ttl-tag expires-in-ms))
       (paint :dim (str "  " interval-ms "ms × " depth " frames"
                        (when-not cpu-time " (no per-thread CPU)")
                        " · " (or elapsed-ms 0) "ms · "
                        threads " threads · " samples " samples · "
                        markers " markers"
                        (when (pos? (long (or dropped-markers 0)))
                          (str " · " dropped-markers " dropped"))))))

(defn- profile-live-lines
  [requests]
  (when (seq requests)
    [(paint :dim "  — jetzt in Arbeit (ohne Armen gelesen) —")
     (for [{:keys [running-ms method uri stack]} (take 5 requests)]
       (str "    " (paint :yellow (rpad (str running-ms "ms") 9))
            (paint :bold (str method " " uri)) "  "
            (paint :cyan (or (first stack) ""))))]))

(defn- profile-shape-lines
  [{:keys [rows]}]
  (when (seq rows)
    [(paint :dim "  — Statement-Formen (derselbe Fold, der auch den Marker ergibt) —")
     (for [{:keys [calls total-ms sql-chars sql]} (take 8 rows)]
       (str "    " (rpad (str calls "×") 7)
            (rpad (str total-ms "ms") 10)
            (paint :dim (rpad (sql-size (long (or sql-chars 0))) 9))
            " " (trace-squish sql)))]))

(defn- scope-process-line
  [{:keys [process node answered armed statements in-report shapes report-age-ms]}]
  (str "    "
       (cond (not answered) (paint :red "✗ ")
             armed          (paint :green "✓ ")
             :else          (paint :dim "· "))
       (paint :bold (str process " " node)) "  "
       (paint :dim
              (str/join
               " · "
               (if answered
                 (cond-> [(if armed "scharf" "aus") (str statements " Statements")]
                   in-report     (conj (str in-report " im Bericht"))
                   (and armed (not shapes)) (conj "Shapes aus (--shapes)")
                   report-age-ms (conj (str "Bericht vor " (quot (long report-age-ms) 1000) "s")))
                 ["keine Meldung: dieser Prozess wird NICHT erfasst"])))))

(defn- scope-lines
  [{:keys [processes past-reports not-covered]}]
  (when (seq processes)
    [(paint :dim "  Scope dieses Trace:")
     (map scope-process-line processes)
     (when (seq past-reports)
       (paint :dim (str "    " (count past-reports)
                        " Berichte vergangener Prozesse ausgeblendet")))
     (for [n not-covered] (paint :dim (str "    nicht erfasst: " n)))]))

(defmethod step! :profile [ctx [_ {:keys [on? off? save interval depth label limit]}]]
  (cond
    (or on? off?)
    (let [r (req! "profile" (api/post! :profile (cond-> {}
                                                  on?      (assoc :on true)
                                                  off?     (assoc :off true)
                                                  interval (assoc :interval interval)
                                                  depth    (assoc :depth depth))))]
      (emit! ctx r (if on? ::profile-armed ::profile-status)))

    save
    (let [{:keys [gecko status nodes]} (req! "profile"
                                             (api/post! :profile
                                                        (cond-> {:gecko true}
                                                          label (assoc :label label))))
          f (io/file save)]
      (io/make-parents f)
      (spit f (json/write-str gecko))
      (emit! ctx {:saved (.getAbsolutePath f) :bytes (.length f) :status status :nodes nodes}
             ::profile-saved))

    :else
    (let [r (req! "profile" (api/post! :profile (cond-> {} limit (assoc :limit limit))))]
      (emit! ctx r ::profile-report)))
  nil)

(defmethod render ::profile-status [_ r] (profile-status-line r))

(defmethod render ::profile-armed [_ r]
  [(profile-status-line r)
   (scope-lines (:scope r))
   (paint :dim "  jetzt nachstellen, dann `profile` (Bericht) oder `profile --save <datei>`")])

(defmethod render ::profile-saved [_ {:keys [saved bytes status nodes]}]
  [(profile-status-line status)
   (for [{:keys [node role missing empty] st :status} nodes]
     (str "  " (paint :bold (str (some-> role name (str " ")) node)) "  "
          (cond missing (paint :yellow "keine Antwort in 10 s — fehlt in der Datei")
                empty   (paint :dim "nichts aufgezeichnet")
                :else   (paint :dim (str "+ " (:samples st) " samples · "
                                         (:markers st) " markers")))))
   (str "  " (paint :green "✓") " " saved (paint :dim (str "  (" (bytes-h bytes) ")")))
   (paint :dim "  → profiler.firefox.com · Load a profile from file")])

(defn- node-label
  [{:keys [role node age-ms]}]
  (str (paint :bold (str (some-> role name) " " node))
       (when age-ms (paint :dim (str "  · Bericht vor " (quot (long age-ms) 1000) "s")))))

(defmethod render ::profile-report [_ r]
  [(profile-status-line r)
   (profile-cpu-lines (:cpu r))
   (profile-marker-lines (:markers r))
   (profile-shape-lines (:shapes r))
   (profile-live-lines (get-in r [:live :requests]))
   (for [n (:nodes r)]
     [(str "  " (node-label n))
      (profile-status-line n)
      (profile-cpu-lines (:cpu n))
      (profile-marker-lines (:markers n))
      (profile-shape-lines (:shapes n))])])

(defmethod render ::stuck [_ r] (stuck-lines r))

(defmethod render ::trace-arm [_ {:keys [backend egress scope]}]
  [(trace-status-lines backend "backend")
   (for [[a side] egress] (trace-status-lines side (str "egress " a)))
   (scope-lines scope)])

(defn- node-events
  [nodes]
  (for [{:keys [role events]} nodes, e events]
    (update e :conn #(str (some-> role name) " " %))))

(defmethod render ::trace-status [_ {:keys [backend egress nodes recent scope]}]
  (let [sides (keep val egress)
        past  (set (:past-reports scope))
        nodes (remove (comp past :node) nodes)]
    [(trace-status-lines backend "backend")
     (for [[a side] egress] (trace-status-lines side (str "egress " a)))
     (for [n nodes]
       [(trace-status-lines n (node-label n))
        (profile-shape-lines (:shapes n))])
     (scope-lines scope)
     (when (seq recent) (paint :dim (str "  " (i18n/t :cli/recent) ":")))
     (map trace-event-line recent)
     (trace-inflight-lines (concat (:inflight backend) (mapcat :inflight sides)
                                   (mapcat :inflight nodes)))]))

(defn- fn-trace-lines
  [label {:keys [armed lifecycle wrapped fns rows error stale]}]
  [(str "  " label ": "
        (if armed (paint :green (i18n/t :cli/armed)) (paint :dim (i18n/t :cli/off)))
        (when armed (ttl-tag (:expires-in-ms lifecycle)))
        (paint :dim (str "  · " (or wrapped 0) " Vars umwickelt · " (or fns 0)
                         " aufgerufen")))
   (when error (paint :red (str "    abgelehnt: " error)))
   (when stale
     (paint :yellow "    nicht auf die Lesung geantwortet: Zahlen so alt wie der Bericht"))
   (when (and armed (zero? (long (or wrapped 0))))
     (paint :yellow "    kein Var passt: das Präfix ist in diesem Prozess nicht geladen"))
   (when (seq rows)
     (paint :dim (format "    %10s %10s %9s  %s" "self ms" "total ms" "Aufrufe" "Funktion")))
   (for [{:keys [self-ms total-ms calls] f :fn} rows]
     (format "    %10.1f %10.1f %9d  %s" (double self-ms) (double total-ms) (long calls) f))])

(defmethod render ::fn-trace [_ {:keys [nodes silent not-covered] :as own}]
  [(fn-trace-lines (paint :bold "backend") own)
   (for [n nodes] (fn-trace-lines (node-label n) n))
   (for [{:keys [role node]} silent]
     (paint :yellow (str "  " (some-> role name) " " node ": kein Bericht")))
   (for [n not-covered] (paint :dim (str "  nicht erfasst: " n)))])

(defmethod step! :trace
  [ctx [_ {:keys [on? off? follow? file egress-file no-params? kill kill-all? limit
                  fns fns-off? fns-report? sort shapes? shapes-report? stuck? slow-ms
                  explain kept? decisions decisions-off? decisions-report? decisions-save]}]]
  (let [cap (or limit 300)
        on? (or on? shapes?)]
    (when stuck?
      (emit! ctx (req! "trace" (api/get! :trace-stuck {})) ::stuck))
    (when shapes-report?
      (emit! ctx (req! "trace"
                       (api/post! :fn-trace
                                  (cond-> {:shapes true :limit (or limit 15)}
                                    sort (assoc :sort sort))))))
    (when (or fns fns-off? fns-report?)
      (emit! ctx
             (req! "trace" (api/post! :fn-trace
                                      (cond-> {:limit (or limit 25)}
                                        sort      (assoc :sort sort)
                                        fns       (assoc :on true
                                                         :prefixes (vec (str/split fns #"[, ]+")))
                                        slow-ms   (assoc :slow-ms slow-ms)
                                        fns-off?  (assoc :off true))))
             ::fn-trace))
    (when (or decisions decisions-off? decisions-report?)
      (emit! ctx (req! "trace"
                       (api/post! :decision-trace
                                  (cond-> {}
                                    decisions      (assoc :on true
                                                          :kinds (if (= "all" decisions)
                                                                   []
                                                                   (str/split decisions #"[, ]+")))
                                    decisions-off? (assoc :off true))))))
    (when decisions-save
      (let [{:keys [edn] :as r} (req! "trace" (api/post! :decision-trace {:save true}))
            f (io/file decisions-save)]
        (io/make-parents f)
        (spit f edn)
        (emit! ctx (-> (dissoc r :edn) (assoc :saved (.getAbsolutePath f) :bytes (.length f))))))
    (when (or kill kill-all?)
      (emit! ctx (req! "trace" (api/post! :trace-kill (if kill-all? {:all true} {:seq kill})))))
    (when (or explain kept?)
      (emit! ctx (req! "trace" (api/post! :trace-explain
                                          (cond-> {}
                                            explain (assoc :seq explain)
                                            limit   (assoc :limit limit))))))
    (when (or on? off?)
      (let [r (req! "trace"
                    (api/post! :trace-arm
                               (merge {:on (boolean on?)}
                                      (cond-> {:capture-params (not no-params?)}
                                        (and slow-ms (not fns)) (assoc :keep-ms slow-ms)
                                        shapes?     (assoc :shapes true)
                                        file        (assoc :file file)
                                        egress-file (assoc :egress-file egress-file)))))]
        (emit! ctx r ::trace-arm)
        (when (and on? (not (:egress-reachable r)))
          (warn! (paint :yellow (str "  " (i18n/t :cli/egress-unreachable)))))))
    (cond
      follow?
      (do (say! ctx (paint :dim (str "  " (i18n/t :cli/streaming-trace))))
          (loop [b 0, e "", seen {}, i 0]
            (let [{:keys [backend egress nodes]} (req! "trace" (api/get! :trace {:b b :e e}))
                  sides (keep val egress)
                  fresh (for [n nodes]
                          (update n :events
                                  (fn [es] (filter #(> (long (:seq %))
                                                       (long (get seen (:node n) 0)))
                                                   es))))
                  evs (->> (concat (:events backend) (mapcat :events sides) (node-events fresh))
                           (sort-by :t)
                           (take-last cap))]
              (say-lines! ctx (map trace-event-line evs))
              (say-lines! ctx (trace-inflight-lines (concat (:inflight backend)
                                                            (mapcat :inflight sides))))
              (when (and (zero? i) (not (:armed backend)))
                (say! ctx (paint :yellow (str "  (" (i18n/t :cli/tracer-off) ")"))))
              (Thread/sleep 1000)
              (recur (long (or (:cursor backend) b))
                     (str/join "," (for [[a side] egress :when side] (str a ":" (:cursor side))))
                     (into seen (for [{:keys [node events]} nodes :when (seq events)]
                                  [node (reduce max 0 (map :seq events))]))
                     (inc i)))))
      (not (or on? off? kill kill-all? fns fns-off? fns-report? shapes-report? stuck?
               explain kept? decisions decisions-off? decisions-report? decisions-save))
      (let [{:keys [backend egress nodes scope]} (req! "trace" (api/get! :trace {:b 0 :e ""}))
            sides (keep val egress)
            evs   (->> (concat (:events backend) (mapcat :events sides) (node-events nodes))
                       (sort-by :t) (take-last (or limit 40)))]
        (emit! ctx {:backend (dissoc backend :events)
                    :egress  (update-vals egress #(some-> % (dissoc :events)))
                    :nodes   (mapv #(dissoc % :events) nodes)
                    :scope   scope
                    :recent  (vec evs)}
               ::trace-status)))))
