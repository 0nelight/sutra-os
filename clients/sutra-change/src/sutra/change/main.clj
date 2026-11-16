(ns sutra.change.main
  (:require [sutra.change.ent :as ent]
            [sutra.change.findings :as f]
            [sutra.change.git :as git]
            [sutra.change.server :as srv]
            [sutra.i18n.core :as i18n :refer [t]]
            [sutra.change.author :as author]
            [cheshire.core :as json]
            [clojure.string :as str]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.project-export :as pe]
            [sutra.wire.transport :as tx]))

(def ^:private farbe?
  (delay (and (nil? (System/getenv "NO_COLOR"))
              (some? (System/console))
              (not= "dumb" (System/getenv "TERM")))))

(def ^:private sgr {:rot 31 :grün 32 :gelb 33 :blau 34 :cyan 36 :fett 1 :matt 2})

(defn- mal [k s] (if @farbe? (str "\033[" (sgr k) "m" s "\033[0m") (str s)))

(defn- anzeige-name
  [s]
  (str/replace (str s) #"(?i)sutra" "sutra"))

(defn- subjekt->text
  [{:keys [klasse name]}]
  (str klasse (when name (str "/" name))))

(defn- abhängigen-namen
  [kategorie xs]
  (for [x xs]
    (case kategorie
      :links      (str (:link x))
      :transforms (str (:transform x) (cond (:writes x) (str " (" (t :change/writes) ")")
                                            (:reads x)  (str " (" (t :change/reads) ")")
                                            :else ""))
      :actions    (str (:action x) (when (seq (:ops x)) (str " (" (str/join "+" (:ops x)) ")")))
      :apps       (str (:app x))
      (str x))))

(def ^:private zustand-schlüssel
  {:kein-changeset        :change/state-kein-changeset
   :offen                 :change/state-offen
   :eingereicht           :change/state-eingereicht
   :abgelehnt             :change/state-abgelehnt
   :wartet-auf-zustimmung :change/state-wartet-auf-zustimmung
   :landet                :change/state-landet
   :fertig                :change/state-fertig
   :unbekannt             :change/state-unbekannt})

(def ^:private art-schlüssel
  {:neu      :change/art-neu
   :geändert :change/art-geändert
   :entfernt :change/art-entfernt})

(def ^:private res-schlüssel
  {:bereit             :change/res-bereit
   :blockiert          :change/res-blockiert
   :konflikt           :change/res-konflikt
   :braucht-zustimmung :change/res-braucht-zustimmung})

(defn- zustand-text [z] (if-let [k (zustand-schlüssel z)] (t k) (name z)))

(defn- trunk-ref
  [dir]
  (let [t pe/trunk-branch]
    (or (git/rev dir (str "origin/" t)) (git/rev dir t))))

(defn- ressourcen-zustand
  [schlüssel {:keys [blockierte konflikte zustimmungspflichtige]}]
  (cond (blockierte schlüssel)             :blockiert
        (konflikte schlüssel)              :konflikt
        (zustimmungspflichtige schlüssel)  :braucht-zustimmung
        :else                              :bereit))

(defn- zustand
  [{:keys [wissen? änderungen? eigene-commits gepusht? check-status changeset
           gelandet? builds]}]
  (let [{:keys [status zustimmungen nötig ablehner]} changeset]
    (cond
      (not wissen?)                                       :unbekannt

      (= :merged status)          (if (seq builds) :landet :fertig)
      (seq ablehner)                                      :abgelehnt
      (= :failed check-status)                            :abgelehnt
      (and changeset gepusht? (pos? (or nötig 0)))
      :wartet-auf-zustimmung
      (and changeset (or änderungen? (not gepusht?)))     :offen
      changeset                                           :eingereicht

      (and (not änderungen?) (zero? (or eigene-commits 0))) :kein-changeset
      (and gelandet? (not änderungen?) (empty? builds))     :fertig
      (and gelandet? (not änderungen?))                     :landet
      (or änderungen? (not gepusht?))                       :offen
      (= :passed check-status)                              :landet
      :else                                                 :eingereicht)))

(defn- nächster-schritt
  [{:keys [zustand zustand-lokal befunde konflikte changeset basis-alt offline? fehler zweig
           verbindungsgrund]}]
  (let [fehler-b      (filter #(= :fehler (:schwere %)) befunde)
        erster-fehler (or (first (remove f/fremde-altlast? fehler-b)) (first fehler-b))
        beheben       (when erster-fehler
                        (str "sutra change explain check " (:regel erster-fehler)))
        anlegen (if (or (nil? zweig) (= zweig pe/trunk-branch))
                  "sutra change start <titel>"
                  (str "sutra propose --source " zweig))]
    (case zustand
      :unbekannt
      (let [verbinden (cond
                        fehler
                        {:befehl "cd <klon> && sutra change status" :warum fehler}

                        offline?
                        {:befehl "sutra change status"
                         :warum  (t :change/step-offline)}

                        :else
                        {:befehl "sutra login"
                         :warum  (str (or (some-> verbindungsgrund (str/split #" — ") first)
                                          (t :change/instance-silent))
                                      (when zustand-lokal
                                        (t :change/locally-looks-like
                                           {:zustand (zustand-text zustand-lokal)})))})]
        (if erster-fehler
          {:befehl  beheben
           :warum   (t :change/fix-first-also-unknown
                       {:subjekt (subjekt->text (:subjekt erster-fehler))})
           :daneben [(:befehl verbinden)]}
          verbinden))

      :kein-changeset
      {:befehl anlegen
       :warum  (t :change/step-no-changeset)
       :daneben ["git status"]}

      :offen
      (cond
        erster-fehler
        {:befehl beheben
         :warum  (t :change/fix-first {:subjekt (subjekt->text (:subjekt erster-fehler))})
         :daneben ["sutra change push -m \"…\""]}

        (seq konflikte)
        {:befehl "git pull --rebase"
         :warum  (t :change/trunk-touched-too {:n (count konflikte)})}

        basis-alt
        {:befehl "git pull --rebase"
         :warum  (t :change/base-behind {:n basis-alt})
         :daneben ["sutra change push"]}

        (nil? changeset)
        {:befehl anlegen
         :warum  (t :change/nothing-blocks-no-changeset)
         :daneben ["sutra change push"]}

        :else
        {:befehl "sutra change push" :warum (t :change/nothing-blocks)})

      :eingereicht
      (cond
        (:alleingang changeset)
        {:befehl "sutra change land"
         :warum  (t :change/green-solo)
         :daneben ["sutra change status"]}

        (= "preparing" (:stufe changeset))
        {:befehl "sutra change ask-review"
         :warum  (t :change/green-unasked)
         :daneben ["sutra change status"]}

        :else {:befehl "sutra change status" :warum (t :change/checks-running)})

      :abgelehnt
      {:befehl (or beheben "sutra change status")
       :warum  (if-let [wer (seq (:ablehner changeset))]
                 (t :change/rejected-by {:wer (str/join ", " wer)})
                 (t :change/rejected-inert))
       :daneben ["sutra change push -m \"…\""]}

      :wartet-auf-zustimmung
      {:befehl "sutra change status"
       :warum  (t :change/approval-pending {:haben (:zustimmungen changeset 0)
                                            :noetig (:nötig changeset 0)})}

      :landet
      {:befehl "sutra change status" :warum (t :change/landing)}

      :fertig
      {:befehl "git pull --ff-only" :warum (t :change/done)}

      {:befehl "sutra change status"
       :warum  (t :change/no-step {:zustand (zustand-text (or zustand :?))})})))

(defn- gesendet-ab
  [wurzel zweig basis]
  (let [tip (when zweig (git/rev wurzel (str "origin/" zweig)))]
    (if (and tip (git/ancestor? wurzel tip "HEAD")) tip basis)))

(defn erheben
  [{:keys [dir zeitraum offline? sql?]}]
  (let [wurzel  (git/root dir)]
    (if-not wurzel
      {:zustand :unbekannt
       :offline? (boolean offline?)
       :online?  false
       :fehler (t :change/not-a-checkout {:dir (or dir ".")})}
      (let [trunk   (trunk-ref wurzel)
            head    (git/head-sha wurzel)
            gemeinsam (when trunk (git/merge-base wurzel head trunk))
            wurzellos? (boolean (and trunk head (nil? gemeinsam)))
            basis   (or gemeinsam (when wurzellos? trunk) head)
            sess    (when-not offline? (srv/verbinden! wurzel))
            dateien-jetzt (git/ls-files wurzel)
            formate (pe/checkout-formats (or (srv/live-schema sess)
                                             {:projections standard/projections})
                                         dateien-jetzt
                                         #(let [f (java.io.File. (str wurzel) (str %))]
                                            (when (.isFile f) (slurp f))))
            inhalt? (partial ent/changeset-inhalt? formate)
            alle-p  (git/changed-paths wurzel basis)
            pfade   (filterv inhalt? alle-p)
            fremd   (filterv (complement inhalt?) alle-p)
            basis-p (set (when basis (git/ls-files wurzel basis)))
            alle-e  (ent/ent-paths formate (distinct (concat dateien-jetzt basis-p)))
            entp    (ent/ent-paths formate alle-p)
            kopf    (ent/tree-at formate wurzel nil entp)
            grund   (ent/tree-at formate wurzel basis entp)
            unt     (when trunk (ent/tree-at formate wurzel trunk entp))
            diff    (ent/diff (:resources grund) (:resources kopf))
            trunk-d (when unt (ent/diff (:resources grund) (:resources unt)))
            konf    (into #{} (filter (set (keys diff))) (keys trunk-d))
            dateien (into {}
                          (for [p entp
                                :let [txt (try (slurp (str wurzel "/" p)) (catch Exception _ nil))
                                      m   (when txt (ent/parse formate p txt))]
                                :when (and m (not (:kaputt m)))]
                            [p m]))
            dateien-basis (when basis
                            (into {} (for [p entp
                                           :let [txt (git/show wurzel basis p)
                                                 m   (when txt (ent/parse formate p txt))]
                                           :when (and m (not (:kaputt m)))]
                                       [p m])))
            ontol?  (some #(= pe/ontology-ent-path %) alle-e)
            befunde (f/sammeln {:ontologie? ontol? :dateien dateien
                                :kaputt (:kaputt kopf) :diff diff})
            sql-tx  (f/sql-transforms diff)
            sql-u   (srv/sql-urteil (or sess (when (and sql? (seq sql-tx)) (srv/verbinden! wurzel)))
                                    sql-tx)
            sql-offen? (boolean (and (seq sql-tx) (nil? sql-u)))
            befunde (into befunde (if sql-offen?
                                    [(f/sql-unbekannt-finding sql-tx)]
                                    (f/sql-urteil-findings sql-u)))
            zust    (srv/zustimmung wurzel (or trunk "HEAD") pfade)
            blockierte (into #{} (for [{:keys [schwere subjekt]} befunde
                                       :when (= :fehler schwere)
                                       :let [k [(:klasse subjekt) (:name subjekt)]]
                                       :when (contains? diff k)]
                                   k))
            zust-pfade (into #{} (mapcat :pfade) zust)
            quelle-von (merge (:quellen grund) (:quellen kopf))
            zust-res (into #{} (for [[k _] diff
                                     :when (contains? zust-pfade (quelle-von k))]
                                 k))
            gepusht? (and head (git/pushed? wurzel head))
            gelandet? (and head trunk (git/ancestor? wurzel head trunk))
            zweig   (git/branch wurzel)
            cs      (srv/changeset sess zweig)
            nach    (when gepusht? (srv/nachleben sess head))
            aufgaben (srv/tasks sess (:id (when (map? cs) cs)))
            wissen? (boolean (and sess (some? cs) (or (not gepusht?) (some? nach))
                                  (not sql-offen?)))
            cs      (when (map? cs) cs)
            schmutzig (filterv inhalt? (git/dirty-paths wurzel))
            dreckig? (boolean (seq schmutzig))
            lokal   (zustand {:wissen?        true
                              :änderungen?    dreckig?
                              :eigene-commits (git/ahead-count wurzel basis)
                              :gepusht?       gepusht?
                              :check-status   (:check-status nach)
                              :changeset      cs
                              :gelandet?      gelandet?
                              :builds         (:builds nach)})
            status      (if wissen? lokal :unbekannt)
            befunde (mapv (fn [{:keys [subjekt] :as bef}]
                            (cond-> bef
                              (and (:name subjekt)
                                   (not= :einreichung (:klasse subjekt))
                                   (not (contains? diff [(:klasse subjekt) (:name subjekt)])))
                              (assoc :fremd? true)))
                          befunde)
            befunde (f/markiere-geerbt befunde
                                       (f/sammeln {:ontologie? ontol?
                                                   :dateien dateien-basis
                                                   :kaputt (:kaputt grund)
                                                   :diff {}}))
            befunde (into befunde (f/check-findings (:checks nach)))
            basis-alt (when (and trunk (not wurzellos?))
                        (let [n (git/behind-count wurzel basis trunk)]
                          (when (and n (pos? n)) n)))
            befunde (into befunde (f/basis-findings {:hinterher basis-alt
                                                     :zweig zweig
                                                     :änderungen? dreckig?}))
            befunde (into (vec (f/ebenen-findings
                                {:pfade (ent/ent-paths formate
                                                       (git/committed-paths
                                                        wurzel (gesendet-ab wurzel zweig basis)))
                                 :schmutzig (ent/ent-paths formate schmutzig)
                                 :zweig zweig}))
                          befunde)
            ressourcen (vec (sort-by (comp subjekt->text :subjekt)
                                     (for [r (for [[[klasse nm] {:keys [art felder]}] diff]
                                               (cond-> {:subjekt {:klasse klasse :name nm} :art art}
                                                 (seq felder) (assoc :felder felder)))]
                                       (assoc r :zustand
                                              (ressourcen-zustand
                                               [(get-in r [:subjekt :klasse])
                                                (get-in r [:subjekt :name])]
                                               {:blockierte blockierte
                                                :konflikte konf
                                                :zustimmungspflichtige zust-res})))))]
        (cond-> {:zustand    status
                 :basis      (cond-> {:commit basis
                                      :trunk (when trunk (str "origin/" pe/trunk-branch))}
                               basis-alt (assoc :hinterher basis-alt)
                               wurzellos? (assoc :ohne-gemeinsame-geschichte true)
                               basis     (assoc :datum (git/commit-date wurzel basis)))
                 :zweig      zweig
                 :changeset  cs
                 :aufgaben   (vec aufgaben)
                 :befunde    befunde
                 :ressourcen ressourcen
                 :dateiebene
                 (vec (sort (for [[p r] (ent/attribute-paths formate (:resources kopf) pfade)
                                  :when (nil? r)] p)))
                 :nicht-inhalt fremd
                 :konflikte  (vec (sort-by str konf))
                 :zustimmung (vec zust)
                 :wirkung    (srv/wirkung sess diff (or zeitraum 30))
                 :nachleben  nach
                 :serverurteil (when gepusht? (:check-status nach))
                 :zeitraum   (or zeitraum 30)
                 :offline?   (boolean offline?)
                 :sql-offen? sql-offen?
                 :online?    (boolean sess)
                 :verbindungsgrund (when-not sess (srv/warum-nicht wurzel))}
          (not wissen?) (assoc :zustand-lokal lokal))))))

(def ^:private zustand-farbe
  {:kein-changeset :matt :offen :cyan :eingereicht :blau :abgelehnt :rot
   :wartet-auf-zustimmung :gelb :landet :blau :fertig :grün :unbekannt :gelb})

(def ^:private res-glyph
  {:bereit ["✓" :grün] :blockiert ["✗" :rot] :konflikt ["≠" :gelb]
   :braucht-zustimmung ["◷" :gelb]})

(defn- schritt-zeile! [{:keys [befehl warum daneben]}]
  (println)
  (println (str "  " (mal :fett "→ ") (mal :cyan befehl) "   " (mal :matt warum)))
  (when (seq daneben)
    (println (str "    " (mal :matt (t :change/also-possible {:was (str/join " · " daneben)}))))))

(defn render-text!
  [{:keys [zustand zustand-lokal basis zweig changeset aufgaben befunde ressourcen
           dateiebene nicht-inhalt zustimmung wirkung nachleben zeitraum offline?
           fehler verbindungsgrund serverurteil] :as alles}]
  (let [kopf! #(do (println)
                   (println (str "  " (mal (zustand-farbe zustand :cyan)
                                           (str/upper-case (zustand-text zustand)))
                                 "   "
                                 (mal :matt (str (t :change/branch {:zweig (or zweig "?")})
                                                 (when zustand-lokal
                                                   (t :change/locally-suffix
                                                      {:zustand (zustand-text zustand-lokal)}))))))
                   (if changeset
                     (println (str "  "
                                   (mal :matt
                                        (str (t :change/changeset-head
                                                {:id (subs (str (:id changeset)) 0
                                                           (min 8 (count (str (:id changeset)))))
                                                 :titel (:titel changeset)
                                                 :nach (:nach changeset)})
                                             (when-let [stufe (:stufe changeset)] (str " · " stufe))
                                             (when (seq (:gefragt changeset))
                                               (t :change/asked-suffix
                                                  {:wer (str/join ", " (:gefragt changeset))}))))))
                     (println (str "  " (mal :matt (t :change/no-changeset-on-branch))))))
        unbekannt! #(when (= :unbekannt zustand)
                      (println)
                      (println (str "  " (mal :gelb "?")
                                    " "
                                    (mal :matt
                                         (if offline?
                                           (t :change/unknown-offline)
                                           (t :change/unknown-online
                                              {:grund
                                               (or verbindungsgrund
                                                   (t :change/instance-silent))}))))))]
    (cond
      fehler
      (do (println)
          (println (mal :rot (str "  " fehler)))
          (schritt-zeile! (nächster-schritt alles)))

      (= :kein-changeset (or zustand-lokal zustand))
      (do (kopf!) (unbekannt!) (schritt-zeile! (nächster-schritt alles)))

      :else
      (do
        (kopf!)

        (let [eigen (remove f/fremde-altlast? befunde)
              fremd-b (filterv f/fremde-altlast? befunde)]
          (when (seq eigen)
            (println)
            (println (str "  " (mal :fett (t :change/section-reason))))
            (doseq [{:keys [subjekt regel ist soll behebung schwere]} eigen]
              (let [[g c] (if (= :fehler schwere) ["✗" :rot] ["!" :gelb])]
                (println (str "    " (mal c g) " " (mal :fett (subjekt->text subjekt))
                              "  " (mal :matt (str regel))))
                (when ist   (println (str "        " (format "%-6s" (t :change/label-is)) ist)))
                (when soll
                  (println (str "        " (format "%-6s" (t :change/label-should)) soll)))
                (println (str "        " (mal :cyan "→ ") behebung)))))
          (when (seq fremd-b)
            (when-not (seq eigen) (println))
            (println (str "    "
                          (mal :matt
                               (str (t :change/inherited-findings
                                       {:n (count fremd-b)
                                        :namen
                                        (str/join ", "
                                                  (take 3
                                                        (map (comp subjekt->text :subjekt)
                                                             fremd-b)))})
                                    (when (> (count fremd-b) 3)
                                      (t :change/more-suffix {:n (- (count fremd-b) 3)}))))))))

        (println)
        (println (str "  " (mal :fett (t :change/section-granularity))
                      "  "
                      (mal :matt
                           (str (t :change/base
                                   {:commit
                                    (some-> (:commit basis)
                                            (subs 0 (min 12 (count (:commit basis)))))})
                                (when-let [h (:hinterher basis)] (t :change/commits-behind {:n h}))
                                (when (:ohne-gemeinsame-geschichte basis)
                                  (t :change/no-shared-history))
                                (when-let [d (:datum basis)] (str " · " (subs d 0 10)))))))
        (if (seq ressourcen)
          (doseq [{:keys [subjekt art zustand felder]} ressourcen]
            (let [[g c] (res-glyph zustand ["·" :matt])]
              (println (str "    " (mal c g) " " (mal :fett (subjekt->text subjekt))
                            "  " (t (art-schlüssel art art))
                            (when (seq felder)
                              (mal :matt (str "  [" (str/join " " (map name felder)) "]")))
                            (when (not= :bereit zustand)
                              (mal c (str "  " (t (res-schlüssel zustand zustand)))))))))
          (println (str "    " (mal :matt (t :change/no-resource-touched)))))
        (when (and (not= :passed serverurteil)
                   (some #(= :bereit (:zustand %)) ressourcen))
          (println (str "    " (mal :matt (t :change/green-means-local)))))
        (when (seq dateiebene)
          (println (str "    "
                        (mal :matt (t :change/file-plane {:pfade (str/join ", " dateiebene)})))))
        (when (seq nicht-inhalt)
          (println (str "    " (mal :matt (str (t :change/not-changeset-content
                                                  {:pfade (str/join ", " (take 5 nicht-inhalt))})
                                               (when (> (count nicht-inhalt) 5)
                                                 (t :change/more-suffix
                                                    {:n (- (count nicht-inhalt) 5)})))))))
        (when (seq zustimmung)
          (doseq [{:keys [pfade reviewer]} zustimmung]
            (println (str "    " (mal :gelb "◷") " "
                          (t :change/approval-needed-for {:pfade (str/join ", " pfade)})
                          "  " (mal :matt (str/join ", " reviewer))))))

        (when (seq wirkung)
          (println)
          (println (str "  " (mal :fett (t :change/section-impact)) "  "
                        (mal :matt (t :change/usage-window {:tage zeitraum}))))
          (doseq [{:keys [subjekt objekte abhängige historie neubau]} wirkung]
            (println (str "    " (mal :fett (subjekt->text subjekt))
                          (when objekte (mal :matt (str "  " (t :change/objects {:n objekte}))))))
            (when neubau
              (println (str "        " (mal :gelb (t :change/forces-reindex))
                            (when objekte
                              (mal :matt (t :change/unavailable-meanwhile {:n objekte}))))))
            (doseq [[k v] (select-keys abhängige [:links :transforms :actions :apps])
                    :when (seq v)]
              (println (str "        " (name k) ": " (str/join ", " (abhängigen-namen k v)))))
            (when historie
              (println (str "        " (t :change/writeback
                                          {:n (str (when (:abgeschnitten historie) "≥ ")
                                                   (:objekte-mit-edits historie))})
                            (mal :matt (str "  (" (str/join "/" (:arten historie))
                                            (when-let [l (:letzter historie)]
                                              (t :change/last-suffix {:datum (subs l 0 10)}))
                                            ")"))))
              (when (pos? (:im-fenster historie 0))
                (println (str "        "
                              (t :change/within-window
                                 {:tage zeitraum :n (:im-fenster historie)})))))))

        (when (seq aufgaben)
          (println)
          (println (str "  " (mal :fett (t :change/section-approval)) "  "
                        (mal :matt (t :change/approval-subtitle))))
          (doseq [{:keys [task name status approvals stale rejections]} aufgaben]
            (let [[g c] (cond (seq rejections) ["✗" :rot]
                              (seq approvals)  ["✓" :grün]
                              :else            ["·" :matt])]
              (println (str "    " (mal c g) " " (or name task) "  " (mal :matt (str status))
                            (when (seq approvals)
                              (mal :grün (str "  ✓ " (str/join ", " approvals))))
                            (when (seq rejections)
                              (mal :rot (str "  ✗ " (str/join ", " rejections))))
                            (when (seq stale)
                              (mal :gelb
                                   (str "  ⧗ "
                                        (t :change/stale {:wer (str/join ", " stale)})))))))))

        (when (or (seq nachleben) (and changeset (pos? (:nötig changeset 0))))
          (println)
          (println (str "  " (mal :fett (t :change/section-afterlife))))
          (doseq [{:keys [check status detail]} (:checks nachleben)]
            (let [c (case status :passed :grün :failed :rot :gelb)]
              (println (str "    " (mal c (format "%-10s" (name (or status :?))))
                            " " (anzeige-name check)
                            (when detail (mal :matt (str "  " detail)))))))
          (let [{:keys [zustimmungen nötig zustimmer ablehner]} changeset]
            (when (pos? (or nötig 0))
              (do
                (println (str "    " (mal :gelb "◷")
                              " " (t :change/approvals {:haben (or zustimmungen 0) :noetig nötig})
                              (when (seq zustimmer)
                                (mal :matt (str "  " (str/join ", " zustimmer))))))))
            (doseq [wer ablehner]
              (println (str "    " (mal :rot "✗") " " (t :change/rejected-by-who {:wer wer})
                            (mal :matt (str "  — " (t :change/one-rejection-blocks)))))))
          (doseq [b (:builds nachleben)]
            (println (str "    " (mal :blau "⟳") " "
                          (t :change/build-running {:id (or (:id b) (:run b))})))))

        (unbekannt!)
        (schritt-zeile! (nächster-schritt (assoc alles :basis-alt (:hinterher basis))))))))

(defn render-json!
  [alles]
  (let [voll (assoc alles :nächster-schritt
                    (nächster-schritt (assoc alles :basis-alt (get-in alles [:basis :hinterher]))))]
    (println (json/generate-string
              (into {} (remove (fn [[_ v]] (or (nil? v) (and (coll? v) (empty? v))))) voll)
              {:pretty true}))))

(defn- explain-check! [regel json?]
  (let [k (keyword (str/replace (str regel) #"^:" ""))
        r (get f/regeln k)]
    (cond
      (and r json?) (do (println (json/generate-string {:regel k
                                                        :bedingung (t (:bedingung r))
                                                        :hinweis   (t (:hinweis r))}
                                                       {:pretty true})) 0)
      r (do (println)
            (println (str "  " (mal :fett (str k))))
            (println (str "    " (t (:bedingung r))))
            (println)
            (println (str "    " (mal :matt (t (:hinweis r)))))
            0)
      :else (do (println (str "  " (t :change/no-such-rule
                                      {:regel k
                                       :bekannt (str/join ", " (sort (map str (keys f/regeln))))})))
                1))))

(def ^:private wert-flags
  #{"--dir" "--zeitraum" "--message" "--lang" "--intervall" "--task" "--note"})

(defn- parse-args
  [args]
  (loop [[a & more] args, verb nil, pos [], opts {}]
    (cond
      (nil? a) {:verb verb :rest pos :opts opts}
      (wert-flags a) (recur (rest more) verb pos (assoc opts (keyword (subs a 2)) (first more)))
      (str/starts-with? a "--") (recur more verb pos (assoc opts (keyword (subs a 2)) true))
      (= "-h" a) (recur more verb pos (assoc opts :help true))
      (= "-m" a) (recur (rest more) verb pos (assoc opts :message (first more)))
      (= "change" a) (recur more verb pos opts)
      (nil? verb) (recur more a pos opts)
      :else (recur more verb (conj pos a) opts))))

(defn- vorpruefung!
  [opts]
  (when-not (or (:trotzdem opts)
                (some-> (git/root (or (:dir opts) ".")) git/laufender-vorgang))
    (let [vor (erheben {:dir (or (:dir opts) ".") :offline? true :sql? true})
          rot (filterv #(= :fehler (:schwere %)) (:befunde vor))]
      (when (seq rot)
        (println)
        (println (str "  " (mal :rot (t :change/not-sent)) "   "
                      (mal :matt (t :change/not-sent-detail {:n (count rot)}))))
        (doseq [{:keys [subjekt regel ist soll behebung]} rot]
          (println (str "    " (mal :rot "✗") " " (mal :fett (subjekt->text subjekt))
                        "  " (mal :matt (str regel))))
          (when ist  (println (str "        " (format "%-6s" (t :change/label-is)) ist)))
          (when soll (println (str "        " (format "%-6s" (t :change/label-should)) soll)))
          (println (str "        " (mal :cyan "→ ") behebung)))
        (schritt-zeile! {:befehl (t :change/cmd-fix-then-push)
                         :warum  (t :change/branch-untouched)
                         :daneben ["sutra change push --trotzdem"]})
        (System/exit 1))
      (when (:sql-offen? vor)
        (println)
        (println (str "  " (mal :gelb (t :change/sql-unbekannt)) "   "
                      (mal :matt (t :change/sql-nicht-kompiliert))))
        (schritt-zeile! {:befehl (t :change/cmd-fix-then-push)
                         :warum  (t :change/fix-sql-ungeprueft)
                         :daneben ["sutra change push --trotzdem"]})
        (System/exit 2)))))

(defn- folge-zeile!
  [wo {:keys [zustand zweig n vorgang meldung]}]
  (let [a {:wo wo :zweig zweig :n n :vorgang (some-> vorgang name) :grund meldung}
        [farbe k] (case zustand
                    :aktuell     [:matt :change/follow-current]
                    :nachgezogen [:grün :change/follow-pulled]
                    :waisen      [:gelb :change/follow-orphans]
                    :konflikt    [:gelb :change/follow-conflict]
                    :vorgang     [:gelb :change/follow-running]
                    :ungepusht   [:matt :change/follow-unpushed]
                    [:gelb :change/follow-no-trunk])]
    (println (str "  " (mal farbe (t k a))))
    (when (and meldung (= :nachgezogen zustand))
      (println (str "    " (mal :gelb meldung))))))

(defn- beobachten!
  [opts]
  (let [ziele (author/checkouts (or (:dir opts) "."))
        s     (max 10 (or (some-> (:intervall opts) parse-long) 30))]
    (when (empty? ziele)
      (println (mal :rot (str "  " (t :change/err-no-checkout {:dir (or (:dir opts) ".")}))))
      (System/exit 1))
    (println (str "  " (mal :matt (t :change/watch-started {:n (count ziele) :s s}))))
    (loop [stand {}]
      (let [neu (reduce (fn [acc w]
                          (let [f (author/fernstand w)]
                            (when (and f (not= f (get stand w)))
                              (let [r (author/folgen! w)]
                                (when (or (nil? (get stand w)) (not= :aktuell (:zustand r)))
                                  (folge-zeile! (.getName (java.io.File. ^String w)) r))))
                            (assoc acc w (or f (get stand w)))))
                        {} ziele)]
        (Thread/sleep (* 1000 s))
        (recur neu)))))

(defn- landen!
  [opts]
  (let [{:keys [zweig anker datei-ebene diff fehler
                heimkehr]} (author/land! {:dir (or (:dir opts) ".")})
        fp-fehler (:error datei-ebene)
        pro-repo  (when-not fp-fehler (vals datei-ebene))
        carried   (mapcat :carried pro-repo)
        removed   (mapcat :removed pro-repo)
        konflikte (mapcat :conflicts pro-repo)]
    (if fehler
      (do (println) (println (mal :rot (str "  " fehler)))
          (schritt-zeile! {:befehl "sutra change status" :warum (t :change/merge-gated)})
          1)
      (do (println)
          (println (str "  " (mal :grün (t :change/landed)) "   "
                        (mal :matt (str zweig " → " pe/trunk-branch))))
          (println (str "    " (mal :matt (if (seq diff)
                                            (t :change/landed-ontology-moved {:n (count diff)})
                                            (t :change/landed-ontology-unchanged)))))
          (when anker
            (println (str "    "
                          (mal :matt
                               (t :change/landed-anchor
                                  {:anker (subs (str anker) 0 (min 12 (count (str anker))))})))))
          (when (or (seq carried) (seq removed))
            (println (str "    "
                          (mal :matt (t :change/landed-file-plane {:uebernommen (count carried)
                                                                   :entfernt (count removed)})))))
          (when (seq konflikte)
            (println (str "  " (mal :gelb (t :change/not-carried)) "   "
                          (mal :matt
                               (t :change/not-carried-detail {:pfade (str/join ", " konflikte)})))))
          (when fp-fehler
            (println (str "  " (mal :gelb (t :change/file-plane-label)) "   "
                          (mal :matt (t :change/file-plane-failed {:grund fp-fehler})))))
          (when heimkehr (folge-zeile! "." heimkehr))
          (schritt-zeile! {:befehl (if (#{:aktuell :nachgezogen} (:zustand heimkehr))
                                     "sutra change start \"<titel>\""
                                     "git switch main && sutra change sync")
                           :warum  (t :change/merge-is-save)})
          0))))

(defn- durchziehen!
  [opts]
  (let [dir   (or (:dir opts) ".")
        stop! (fn [fehler weiter]
                (println (str "  " (mal :rot "✗") "   " fehler))
                (schritt-zeile! {:befehl weiter
                                 :warum  (t :change/ladder-stopped)})
                (System/exit 1))]
    (let [{:keys [stufe fehler]} (author/ask-review! {:dir dir})]
      (when fehler (stop! fehler "sutra change ask-review"))
      (println (str "  " (mal :grün (t :change/in-review)) "   "
                    (mal :matt (t :change/stage {:stufe (or stufe "in-review")})))))
    (let [{:keys [changeset fehler]} (author/review! {:dir dir :verdikt :zustimmen})]
      (when fehler (stop! fehler "sutra change review --approve"))
      (println (str "  " (mal :grün (t :change/approved)) "   "
                    (mal :matt (t :change/approvals {:haben (:approval-count changeset 0)
                                                     :noetig (count (:open-tasks changeset))})))))
    (System/exit (landen! opts))))

(defn -main [& args]
  (let [{:keys [verb rest opts]} (parse-args args)
        json? (boolean (:json opts))]
    (i18n/set-locale! (or (i18n/from-tag (:lang opts)) (i18n/from-env)))
    (reset! tx/on-protocol-mismatch
            (fn [e]
              (binding [*out* *err*] (println (mal :rot (str "  " (.getMessage ^Throwable e)))))
              (System/exit 3)))
    (cond
      (:help opts)
      (do (println (t :change/help)) (System/exit 0))

      (= "start" verb)
      (let [{:keys [zweig changeset warnung fehler fortgesetzt? schutz-neu? basis]}
            (author/start! {:dir (or (:dir opts) ".") :titel (str/join " " rest)})]
        (if fehler
          (do (println) (println (mal :rot (str "  " fehler))) (System/exit 1))
          (do (println)
              (println (str "  "
                            (mal :grün
                                 (t (if fortgesetzt? :change/changeset-continued
                                        :change/changeset-created))) "   "
                            (mal :matt
                                 (t :change/branch-to {:zweig zweig :nach (:nach changeset)}))))
              (println (str "  "
                            (mal :matt
                                 (t :change/changeset-named
                                    {:id (:id changeset) :titel (:titel changeset)}))))
              (when basis (println (str "  " (mal :matt (t :change/branched-from {:basis basis})))))
              (when warnung (println (str "  " (mal :gelb (str "! " warnung)))))
              (when schutz-neu?
                (println (str "  " (mal :matt (t :change/prepush-installed)))))
              (schritt-zeile! {:befehl (t :change/cmd-edit-then-push)
                               :warum  (t :change/save-judges-first)
                               :daneben ["sutra change status"]})
              (System/exit 0))))

      (= "push" verb)
      (let [_ (vorpruefung! opts)
            {:keys [zweig abgelehnt? gründe committet? dreckig ausgelassen fehler
                    nachricht-ungenutzt nachgezogen]}
            (author/push! {:dir (or (:dir opts) ".")
                           :alles? (boolean (:alles opts))
                           :message (or (:message opts)
                                        (when (seq rest) (str/join " " rest)))})]
        (println)
        (cond
          abgelehnt?
          (do (println (str "  " (mal :rot (t :change/refused)) "   "
                            (mal :matt (str (t :change/refused-detail {:zweig zweig})
                                            (when committet? (t :change/commit-stands-local))))))
              (doseq [g gründe] (println (str "    " (mal :rot "✗") " " g)))
              (schritt-zeile! {:befehl (t :change/cmd-fix-then-push-short)
                               :warum  (str (t (if committet?
                                                 :change/refused-why-committed
                                                 :change/refused-why-clean))
                                            (t :change/attempt-readable))})
              (System/exit 1))

          fehler
          (do (println (mal :rot (str "  " fehler)))
              (doseq [g gründe] (println (str "    " (mal :matt g))))
              (doseq [p (take 10 dreckig)] (println (str "    " (mal :matt (str "· " p)))))
              (System/exit 1))

          :else
          (do (doseq [g gründe] (println (str "    " (mal :matt g))))
              (println (str "  " (mal :grün (t :change/submitted)) "   "
                            (mal :matt (str (t :change/branch {:zweig zweig})
                                            (when committet? (t :change/committed-suffix))))))
              (when-let [{:keys [n sha grund]} nachgezogen]
                (println (str "  " (if grund (mal :gelb "! ") (mal :matt "↻ "))
                              (mal :matt (if grund
                                           (t :change/normalized-behind {:n n :grund grund})
                                           (t :change/normalized-pulled {:n n :sha sha}))))))
              (when nachricht-ungenutzt
                (println (str "  " (mal :gelb "! ")
                              (mal :matt
                                   (t :change/message-unused {:nachricht nachricht-ungenutzt})))))
              (when (seq ausgelassen)
                (println (str "  "
                              (mal :gelb
                                   (str "! " (t :change/paths-left {:n (count ausgelassen)})))))
                (doseq [p ausgelassen] (println (str "    " (mal :matt (str "· " p))))))
              (if (:land opts)
                (durchziehen! opts)
                (do (schritt-zeile! {:befehl "sutra change ask-review"
                                     :warum  (t :change/submit-asks-nobody)
                                     :daneben ["sutra change review --approve"
                                               "sutra change land"
                                               (t :change/cmd-push-land)]})
                    (System/exit 0))))))

      (= "ask-review" verb)
      (let [{:keys [zweig stufe reviewer fehler]}
            (author/ask-review! {:dir (or (:dir opts) ".")
                                 :reviewer (when-let [r (:reviewer opts)]
                                             (remove str/blank? (str/split (str r) #",")))})]
        (if fehler
          (do (println) (println (mal :rot (str "  " fehler))) (System/exit 1))
          (do (println)
              (println (str "  " (mal :grün (t :change/in-review)) "   "
                            (mal :matt (str (t :change/branch {:zweig zweig})
                                            (when (seq reviewer)
                                              (t :change/asked-suffix
                                                 {:wer (str/join ", " reviewer)}))))))
              (schritt-zeile! {:befehl "sutra change status"
                               :warum  (t :change/stage-now-per-resource
                                          {:stufe (or stufe "in-review")})})
              (System/exit 0))))

      (= "check" verb)
      (let [{:keys [zweig typen llm kosten gebaut wartet-auf-freigabe meldung fehler]}
            (author/check! {:dir (or (:dir opts) ".") :yes? (boolean (:yes opts))})]
        (cond
          fehler (do (println) (println (mal :rot (str "  " fehler))) (System/exit 1))

          wartet-auf-freigabe
          (do (println)
              (println (str "  " (mal :gelb (t :change/model-calls)) "   "
                            (mal :matt (str (t :change/model-calls-detail {:n (count llm)})
                                            (when kosten
                                              (t :change/estimated-suffix {:kosten kosten}))))))
              (doseq [t llm]
                (println (str "    " (mal :matt (str "· " (or (:transform t) (:name t))
                                                     "  " (or (:model t) "")
                                                     "  "
                                                     (t :change/rows-pending
                                                        {:n (or (:pending t) "?")}))))))
              (schritt-zeile! {:befehl "sutra change check --yes"
                               :warum  (t :change/compute-free-model-not)
                               :daneben ["sutra build estimate"]})
              (System/exit 0))

          :else
          (do (println)
              (println (str "  " (mal :grün "PREVIEW") "   "
                            (mal :matt (t :change/branch {:zweig zweig}))))
              (when meldung (println (str "  " (mal :matt meldung))))
              (doseq [{:keys [typ objekte]} typen]
                (let [[g c] (if (and objekte (pos? objekte)) ["✓" :grün] ["·" :matt])]
                  (println (str "    " (mal c g) " " typ "  "
                                (mal :matt (t :change/objects-on-branch {:n (or objekte 0)}))))))
              (schritt-zeile!
               (if gebaut
                 {:befehl "sutra change status" :warum (t :change/build-then-review)}
                 {:befehl "sutra change ask-review" :warum (t :change/reviewable-now)}))
              (System/exit 0))))

      (and (= "sync" verb) (:watch opts))
      (beobachten! opts)

      (= "sync" verb)
      (let [{:keys [zweig meldung konflikt ungepusht fehler gefolgt]}
            (author/sync! {:dir (or (:dir opts) ".")})]
        (cond
          fehler (do (println) (println (mal :rot (str "  " fehler))) (System/exit 1))

          gefolgt
          (do (println) (folge-zeile! "." gefolgt)
              (System/exit (if (#{:aktuell :nachgezogen} (:zustand gefolgt)) 0 1)))

          ungepusht
          (do (println)
              (println (str "  " (mal :cyan (t :change/not-pushed-yet)) "   "
                            (mal :matt (t :change/branch {:zweig zweig}))))
              (schritt-zeile! {:befehl "sutra change push"
                               :warum  (t :change/nothing-to-sync)})
              (System/exit 0))
          konflikt
          (do (println)
              (println (str "  " (mal :gelb (t :change/conflict)) "   "
                            (mal :matt (t :change/branch {:zweig zweig}))))
              (println (str "  " (mal :matt meldung)))
              (schritt-zeile! {:befehl "sutra changeset rebase --mine <ressource>"
                               :warum  (t :change/decide-per-resource)
                               :daneben ["git rebase --continue"]})
              (System/exit 1))
          :else
          (do (println)
              (println (str "  " (mal :grün (t :change/up-to-date)) "   "
                            (mal :matt (t :change/branch {:zweig zweig}))))
              (schritt-zeile! {:befehl "sutra change status" :warum (t :change/at-trunk)})
              (System/exit 0))))

      (= "review" verb)
      (let [verdikt (cond (:reject opts) :ablehnen (:approve opts) :zustimmen)]
        (if-not verdikt
          (let [alles (erheben {:dir (or (:dir opts) ".") :offline? false})]
            (render-text! alles)
            (System/exit (cond (f/blockiert? (:befunde alles)) 1
                               (= :unbekannt (:zustand alles)) 2
                               :else 0)))
          (let [{:keys [changeset task fehler]}
                (author/review! {:dir (or (:dir opts) ".") :verdikt verdikt
                                 :task (:task opts) :note (:note opts)})]
            (if fehler
              (do (println) (println (mal :rot (str "  " fehler))) (System/exit 1))
              (do (println)
                  (println (str "  " (mal (if (= :ablehnen verdikt) :gelb :grün)
                                          (t (if (= :ablehnen verdikt) :change/rejected
                                                 :change/approved)))
                                "   "
                                (mal :matt (str (or task (t :change/all-tasks))
                                                "  "
                                                (t :change/approvals
                                                   {:haben (:approval-count changeset 0)
                                                    :noetig (count (:open-tasks changeset))})))))
                  (schritt-zeile!
                   (if (= :ablehnen verdikt)
                     {:befehl "sutra change status" :warum (t :change/one-rejection-blocks)}
                     {:befehl "sutra change land" :warum (t :change/when-all-approved)}))
                  (System/exit 0))))))

      (= "land" verb)
      (System/exit (landen! opts))

      (= "explain" verb)
      (let [regel (last rest)]
        (if (and regel (not= "check" regel))
          (System/exit (explain-check! regel json?))
          (do (println "  sutra change explain check <regel>") (System/exit 1))))

      (or (nil? verb) (= "status" verb))
      (let [alles (erheben {:dir (or (:dir opts) ".")
                            :zeitraum (some-> (:zeitraum opts) parse-long)
                            :offline? (boolean (:offline opts))})]
        (if json? (render-json! alles) (render-text! alles))
        (System/exit (cond (f/blockiert? (:befunde alles))   1
                           (= :unbekannt (:zustand alles))   2
                           :else                             0)))

      :else
      (do (println (str "  " (t :change/unknown-verb {:verb verb}))) (println (t :change/help))
          (System/exit 1)))))
