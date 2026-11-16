(ns sutra.change.author
  (:require [sutra.change.git :as git]
            [sutra.change.server :as srv]
            [sutra.i18n.core :refer [t]]
            [clojure.string :as str]
            [babashka.process :as p]
            [sutra.parser.project-export :as pe]
            [sutra.tooling.git-hooks :as hooks]
            [sutra.tooling.platform-files :as pf]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn zweig-name
  [titel]
  (let [s (-> (str/lower-case (str titel))
              (str/replace "ä" "ae") (str/replace "ö" "oe") (str/replace "ü" "ue")
              (str/replace "ß" "ss")
              (str/replace #"[^a-z0-9]+" "-")
              (str/replace #"^-+|-+$" ""))]
    (when-not (str/blank? s) (subs s 0 (min 60 (count s))))))

(defn- schreib!
  [sess f]
  (try
    (binding [tx/*auth-token* (:token sess)]
      (let [{:keys [status body]} (f)]
        (if (<= 200 status 299)
          {:ok body}
          {:fehler (or (:error body) (str "HTTP " status))})))
    (catch Exception e {:fehler (or (ex-message e) "die Instanz antwortet nicht")})))

(defn- schutz-nachruesten!
  ([wurzel] (schutz-nachruesten! wurzel (srv/verbinden! wurzel)))
  ([wurzel sess]
   (try
     (when-let [bin (System/getenv "SUTRA_CHANGE_BIN")]
       (= :written (hooks/install! (git/hooks-dir wurzel) bin (srv/trunk-geschuetzt? sess))))
     (catch Exception _ nil))))

(defn- holen!
  [wurzel]
  (try (p/sh {:dir wurzel :out :string :err :string} "timeout" "30" "git" "fetch" "--quiet"
             "origin")
       (catch Exception _ nil)))

(defn- fork-basis
  [wurzel zweig trunk]
  (first (filter #(git/rev wurzel %)
                 [(str "origin/" zweig) (str "origin/" trunk) trunk])))

(defn- wechseln!
  [wurzel zweig trunk]
  (if (git/rev wurzel zweig)
    (p/sh {:dir wurzel :err :string} "git" "switch" zweig)
    (if-let [basis (fork-basis wurzel zweig trunk)]
      (p/sh {:dir wurzel :err :string} "git" "switch" "-c" zweig basis)
      (p/sh {:dir wurzel :err :string} "git" "switch" "-c" zweig))))

(defn- wurzellos?
  [wurzel zweig]
  (let [r (str "origin/" zweig)]
    (boolean (and (git/rev wurzel r) (nil? (git/merge-base wurzel "HEAD" r))))))

(defn- plattform-ablegen!
  [wurzel sess]
  (some->> (srv/plattform-dateien sess) (pf/lay-down! wurzel)))

(defn start!
  [{:keys [dir titel]}]
  (let [wurzel (git/root dir)
        zweig  (zweig-name titel)
        trunk  pe/trunk-branch]
    (cond
      (str/blank? (str titel))
      {:fehler (t :change/err-title-missing)}

      (nil? zweig)
      {:fehler (t :change/err-title-unusable {:titel titel})}

      (nil? wurzel)
      {:fehler (t :change/err-start-no-checkout {:dir (or dir ".")})}

      :else
      (if-let [sess (srv/verbinden! wurzel)]
        (let [_      (holen! wurzel)
              cs     (let [c (srv/changeset sess zweig)] (when (map? c) c))
              offen  (when (= :open (:status cs)) cs)
              steht? (srv/zweig-steht? sess zweig)
              da?    (boolean (or steht? offen (git/rev wurzel zweig)))
              b      (when-not (or steht? offen)
                       (schreib! sess #(api/post! :branches {:name zweig :parent trunk})))]
          (if (and (:fehler b) (not da?))
            {:fehler (t :change/err-branch-not-created {:grund (:fehler b)})}
            (let [p (if offen
                      {:ok {:id (:id offen)}}
                      (schreib! sess
                                #(api/post! :proposals
                                            {:source zweig :target trunk :title titel})))]
              (if (:fehler p)
                {:fehler (t :change/err-branch-without-changeset {:zweig zweig :grund (:fehler p)})
                 :zweig  zweig}
                (let [neu?  (nil? (git/rev wurzel zweig))
                      basis (when neu? (fork-basis wurzel zweig trunk))
                      {:keys [exit err]} (wechseln! wurzel zweig trunk)
                      schutz (schutz-nachruesten! wurzel sess)
                      _      (plattform-ablegen! wurzel sess)]
                  (cond-> {:zweig zweig
                           :fortgesetzt? (some? offen)
                           :schutz-neu?  (boolean schutz)
                           :basis basis
                           :changeset {:id (:id (:ok p)) :titel (or (:titel offen) titel)
                                       :nach trunk}}
                    (not (zero? exit))
                    (assoc :warnung (t :change/warn-branch-local-missing
                                       {:grund (str/trim (str err)) :zweig zweig}))

                    (and (zero? exit) (wurzellos? wurzel zweig))
                    (assoc :warnung (t :change/hint-foreign-root {:zweig zweig :trunk trunk}))))))))
        {:fehler (t :change/err-changeset-is-server-side {:grund (srv/warum-nicht wurzel)})}))))

(def ^:private grund-muster
  [#"^remote:\s*(.*?)\s*$"
   #"^\s*!\s+\[remote rejected\][^(]*\((.*)\)\s*$"
   #"^\s*!\s+\[rejected\][^(]*\((.*)\)\s*$"])

(defn gründe-aus
  [text]
  (->> (str/split-lines (str text))
       (keep (fn [l] (some #(second (re-find % l)) grund-muster)))
       (remove str/blank?)
       vec))

(defn- nachziehen!
  [wurzel zweig]
  (holen! wurzel)
  (let [r (str "origin/" zweig)]
    (when-let [n (and (git/rev wurzel r)
                      (git/ancestor? wurzel "HEAD" r)
                      (let [n (git/behind-count wurzel "HEAD" r)] (when (pos-int? n) n)))]
      (let [{:keys [exit err]} (p/sh {:dir wurzel :out :string :err :string}
                                     "git" "merge" "--ff-only" "--quiet" r)]
        (if (zero? exit)
          {:n n :sha (subs (str (git/rev wurzel "HEAD")) 0 12)}
          {:n n :grund (str/trim (str err))})))))

(defn push!
  [{:keys [dir message alles?]}]
  (let [wurzel (git/root dir)]
    (if (nil? wurzel)
      {:fehler (t :change/err-no-checkout {:dir (or dir ".")})}
      (let [zweig   (git/branch wurzel)
            vorgang (git/laufender-vorgang wurzel)
            sh      (fn [& args] (apply p/sh {:dir wurzel :out :string :err :string} args))
            vorgemerkt (->> (:out (sh "git" "diff" "--cached" "--name-only"))
                            str/split-lines (remove str/blank?) seq)
            dreckig    (seq (git/dirty-paths wurzel))
            ungesendet (if-let [r (first (filter #(git/rev wurzel %)
                                                 [(str "origin/" zweig) "@{upstream}"]))]
                         (pos? (or (git/ahead-count wurzel r) 0))
                         true)]
        (cond
          vorgang
          {:fehler (t :change/err-operation-in-progress {:vorgang (name vorgang)})
           :gründe [(t :change/hint-finish-operation {:vorgang (name vorgang)})]}

          (str/blank? (str zweig))
          {:fehler (t :change/err-no-branch)}

          (= :keiner (some-> (srv/verbinden! wurzel) (srv/changeset zweig)))
          {:fehler (t :change/err-no-open-changeset {:zweig zweig})
           :gründe [(t :change/hint-start-creates-both)
                    (t :change/hint-commits-survive {:trunk pe/trunk-branch :zweig zweig})]}

          (and (or vorgemerkt (and alles? dreckig)) (str/blank? (str message)))
          {:fehler (t :change/err-message-missing)}

          (and (not (str/blank? (str message))) (not vorgemerkt) (not alles?)
               (not ungesendet))
          {:fehler (t :change/err-nothing-staged)
           :dreckig (vec (take 10 (or dreckig [])))}

          :else
          (let [_ (when (or vorgemerkt (and alles? dreckig))
                    (when alles? (sh "git" "add" "-A"))
                    (sh "git" "commit" "-m" (str message)))
                {:keys [exit err out]} (sh "git" "push" "-u" "origin" zweig)
                text (str (or err "") (or out ""))
                gründe (gründe-aus text)
                abgelehnt? (boolean (re-find #"\[remote rejected\]|\[rejected\]" text))]
            (cond
              abgelehnt?
              (let [committet? (boolean (or vorgemerkt (and alles? dreckig)))
                    fremder-tip (when (re-find #"(?i)non-fast-forward|fetch first" text)
                                  (holen! wurzel)
                                  (let [r (str "origin/" zweig)]
                                    (when (and (git/rev wurzel r)
                                               (not (git/ancestor? wurzel r "HEAD")))
                                      (git/rev wurzel r))))]
                {:zweig zweig :abgelehnt? true
                 :committet? committet?
                 :gründe (cond-> (if (seq gründe)
                                   gründe
                                   (vec (remove str/blank? (str/split-lines text))))
                           fremder-tip
                           (conj (if (git/merge-base wurzel "HEAD" fremder-tip)
                                   (t :change/hint-foreign-tip
                                      {:zweig zweig
                                       :tip (subs fremder-tip 0 (min 12 (count fremder-tip)))})
                                   (t :change/hint-foreign-root
                                      {:zweig zweig :trunk pe/trunk-branch}))))
                 :fehler (if committet?
                           (t :change/err-refused-commit-local {:zweig zweig})
                           (t :change/err-refused-nothing-written))})

              (not (zero? exit))
              {:zweig zweig :gründe gründe
               :fehler (t :change/err-push-failed {:grund (str/trim (str err))})}

              :else
              {:zweig zweig :abgelehnt? false :gründe gründe
               :nachgezogen (nachziehen! wurzel zweig)
               :committet? (boolean (or vorgemerkt (and alles? dreckig)))
               :nachricht-ungenutzt (when (and (not (str/blank? (str message)))
                                               (not vorgemerkt) (not alles?))
                                      (str message))
               :ausgelassen (when (and dreckig (not vorgemerkt) (not alles?))
                              (vec (take 10 dreckig)))})))))))

(defn- changeset-von
  [dir]
  (let [wurzel (git/root dir)
        zweig  (git/branch wurzel)]
    (cond
      (nil? wurzel) {:fehler (t :change/err-no-checkout {:dir (or dir ".")})}
      :else
      (if-let [sess (srv/verbinden! wurzel)]
        (if-let [cs (let [c (srv/changeset sess zweig)] (when (map? c) c))]
          {:sess sess :zweig zweig :changeset cs}
          {:fehler (t :change/err-no-open-changeset-short {:zweig zweig})})
        {:fehler (srv/warum-nicht wurzel)}))))

(defn check!
  [{:keys [dir yes?]}]
  (let [{:keys [sess zweig changeset fehler]} (changeset-von dir)]
    (if fehler
      {:fehler fehler}
      (let [typen (->> (srv/tasks sess (:id changeset))
                       (filter #(str/starts-with? (str (:task %)) "object-types/"))
                       (keep :name)
                       distinct vec)]
        (if (empty? typen)
          {:zweig zweig :typen [] :meldung (t :change/no-object-types)}
          (let [b (srv/bereitschaft sess zweig typen)]
            (cond
              (nil? b) {:fehler (t :change/err-readiness-silent)}

              (and (seq (:llm b)) (not yes?))
              {:zweig zweig :typen (:typen b) :llm (:llm b) :kosten (:kosten b)
               :wartet-auf-freigabe true}

              :else
              (let [r (srv/bauen! sess zweig typen)]
                {:zweig zweig :typen (:typen b) :llm (:llm b) :gebaut (boolean r)
                 :run (or (:run-id r) (:id r))}))))))))

(defn ask-review!
  [{:keys [dir reviewer]}]
  (let [wurzel (git/root dir)
        zweig  (git/branch wurzel)]
    (cond
      (nil? wurzel)
      {:fehler (t :change/err-no-checkout {:dir (or dir ".")})}

      (= zweig pe/trunk-branch)
      {:fehler (t :change/err-nothing-to-review-on-trunk {:trunk pe/trunk-branch})}

      :else
      (if-let [sess (srv/verbinden! wurzel)]
        (if-let [cs (let [c (srv/changeset sess zweig)] (when (map? c) c))]
          (let [r (schreib! sess
                            #(api/post! :proposal-ask-review {:id (:id cs)}
                                        (cond-> {}
                                          (seq reviewer) (assoc :reviewers (vec reviewer)))))]
            (if (:fehler r)
              {:fehler (:fehler r)}
              {:zweig zweig :changeset (:ok r)
               :stufe (:stage (:ok r)) :reviewer (vec reviewer)}))
          {:fehler (t :change/err-no-open-changeset-short {:zweig zweig})})
        {:fehler (t :change/err-stage-is-server-side {:grund (srv/warum-nicht wurzel)})}))))

(defn- git! [wurzel & args]
  (let [{:keys [exit out err]} (apply p/sh {:dir wurzel :out :string :err :string} "git" args)]
    {:ok? (zero? exit) :text (str/trim (str out "\n" err))}))

(defn folgen!
  [wurzel]
  (let [trunk  pe/trunk-branch
        ziel   (str "origin/" trunk)
        zweig  (git/branch wurzel)
        _      (schutz-nachruesten! wurzel)
        vorgang (git/laufender-vorgang wurzel)]
    (if vorgang
      {:zustand :vorgang :zweig zweig :vorgang vorgang}
      (let [_      (holen! wurzel)
            vorher (git/head-sha wurzel)]
        (cond
          (or (= "HEAD" zweig) (= trunk zweig))
          (let [waisen (or (git/ahead-count wurzel ziel) 0)]
            (cond
              (nil? (git/rev wurzel ziel)) {:zustand :kein-trunk :zweig zweig}
              (pos? waisen)                {:zustand :waisen :zweig zweig :n waisen}
              (= vorher (git/rev wurzel ziel)) {:zustand :aktuell :zweig zweig}
              :else
              (let [n (git/behind-count wurzel "HEAD" ziel)
                    {:keys [ok?
                            text]} (git! wurzel "merge" "--ff-only" "--autostash" "--quiet" ziel)]
                (if ok?
                  {:zustand :nachgezogen :zweig zweig :n n
                   :meldung (when (str/includes? text "stash") text)}
                  {:zustand :konflikt :zweig zweig :meldung text}))))

          (nil? (git/upstream wurzel))
          {:zustand :ungepusht :zweig zweig}

          :else
          (let [up (git/upstream wurzel)
                n  (git/behind-count wurzel "HEAD" up)]
            (if (zero? (or n 0))
              {:zustand :aktuell :zweig zweig}
              (let [{:keys [ok? text]} (git! wurzel "-c" "rerere.enabled=true"
                                             "rebase" "--autostash" "--quiet" up)]
                (if ok?
                  {:zustand :nachgezogen :zweig zweig :n n
                   :meldung (when (str/includes? text "stash") text)}
                  (do (when (git/laufender-vorgang wurzel) (git! wurzel "rebase" "--abort"))
                      {:zustand :konflikt :zweig zweig
                       :meldung (or (some->> (str/split-lines text)
                                             (filter #(re-find #"(?i)konflikt|conflict" %))
                                             seq (str/join "; "))
                                    (first (str/split-lines text)))}))))))))))

(defn checkouts
  [dir]
  (when-let [wurzel (git/root dir)]
    (->> (.listFiles (java.io.File. ^String wurzel))
         (filter #(.isDirectory ^java.io.File %))
         (keep #(let [p (.getPath ^java.io.File %)] (when (= p (git/root p)) p)))
         sort
         (cons wurzel)
         vec)))

(defn fernstand
  [wurzel]
  (let [{:keys [exit out]} (p/sh {:dir wurzel :out :string :err :string}
                                 "timeout" "30" "git" "ls-remote" "--quiet" "origin")]
    (when (zero? exit) out)))

(defn sync!
  [{:keys [dir]}]
  (let [wurzel (git/root dir)]
    (cond
      (nil? wurzel) {:fehler (t :change/err-no-checkout {:dir (or dir ".")})}

      (contains? #{"HEAD" pe/trunk-branch} (git/branch wurzel))
      {:gefolgt (folgen! wurzel) :zweig (git/branch wurzel)}

      (nil? (git/upstream wurzel))
      (do (plattform-ablegen! wurzel (srv/verbinden! wurzel))
          {:ungepusht true :zweig (git/branch wurzel)})

      (wurzellos? wurzel (git/branch wurzel))
      {:fehler (t :change/hint-foreign-root {:zweig (git/branch wurzel) :trunk pe/trunk-branch})}

      :else
      (let [{:keys [exit out err]} (p/sh {:dir wurzel :err :string :out :string}
                                         "git" "pull" "--rebase" "--autostash")]
        (if (zero? exit)
          (do (plattform-ablegen! wurzel (srv/verbinden! wurzel))
              {:zweig (git/branch wurzel) :meldung (str/trim (str out))})
          {:konflikt true
           :zweig (git/branch wurzel)
           :meldung (str/trim (str err out))})))))

(defn review!
  [{:keys [dir verdikt task note]}]
  (let [{:keys [sess changeset fehler]} (changeset-von dir)]
    (if fehler
      {:fehler fehler}
      (let [r (schreib! sess #(if (= :ablehnen verdikt)
                                (api/post! :proposal-reject {:id (:id changeset)}
                                           (cond-> {} note (assoc :note note)
                                             task (assoc :task task)))
                                (api/post! :proposal-approve {:id (:id changeset)}
                                           (cond-> {} note (assoc :note note)
                                             task (assoc :task task)))))]
        (if (:fehler r)
          {:fehler (:fehler r)}
          {:changeset (:ok r) :verdikt verdikt :task task})))))

(defn land!
  [{:keys [dir]}]
  (let [{:keys [sess changeset zweig fehler]} (changeset-von dir)]
    (if fehler
      {:fehler fehler}
      (let [r (schreib! sess #(api/post! :proposal-merge {:id (:id changeset)} nil))]
        (if (:fehler r)
          {:fehler (:fehler r) :changeset changeset}
          {:zweig zweig :changeset (:ok r)
           :heimkehr (when-let [wurzel (git/root dir)]
                       (holen! wurzel)
                       (let [{:keys [ok? text]} (git! wurzel "switch" "--quiet" pe/trunk-branch)]
                         (if ok? (folgen! wurzel) {:zustand :konflikt :zweig zweig :meldung text})))
           :anker (:merge-commit (:ok r))
           :datei-ebene (:file-plane (:ok r))
           :diff (:diff (:ok r))})))))
