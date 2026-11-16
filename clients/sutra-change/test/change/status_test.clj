(ns change.status-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [sutra.change.ent :as ent]
            [sutra.change.findings :as f]
            [sutra.change.git :as git]
            [sutra.change.main :as main]
            [sutra.change.server :as srv]
            [sutra.i18n.catalog :as catalog]
            [sutra.i18n.core :as i18n :refer [t]]
            [sutra.change.author :as author]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- git! [dir & args]
  (apply p/sh {:dir (str dir) :err :string :out :string} "git" args))

(defn- repo!
  [files]
  (let [dir (str (fs/create-temp-dir {:prefix "change-test"}))]
    (git! dir "init" "-q" "-b" "main")
    (git! dir "config" "user.email" "t@example.com")
    (git! dir "config" "user.name" "T")
    (doseq [[p c] files]
      (let [f (str dir "/" p)]
        (fs/create-dirs (fs/parent f))
        (spit f c)))
    (git! dir "add" "-A")
    (git! dir "commit" "-qm" "basis")
    dir))

(defn- schreib! [dir p c] (spit (str dir "/" p) c))

(defn- status
  [dir] (main/erheben {:dir dir :offline? true}))

(defn- lokal [s] (or (:zustand-lokal s) (:zustand s)))

(def ^:private basis-ent
  (str "{:schema\n"
       " {:objects\n"
       "  {:lieferant {:attributes [[:name :string]] :match-keys [:name]}\n"
       "   :rechnung  {:attributes [[:betrag :double]] :match-keys [:beleg]}}}}\n"))

(deftest kein-changeset-ist-eine-zeile
  (let [dir (repo! {"ontology.ent" basis-ent})
        s   (status dir)]
    (is (= :kein-changeset (lokal s)))
    (is (empty? (:ressourcen s)))
    (is (empty? (:befunde s)))
    (testing "guarantee 3 — a next step exists even when there is nothing to do"
      (is (string? (:befehl (#'main/nächster-schritt s)))))))

(deftest eine-uncommittete-änderung-ist-offen
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent ":match-keys [:name]" ":match-keys [:name :nr]"))
    (let [s (status dir)]
      (is (= :offen (lokal s)))
      (testing "the working tree counts as the change — not only commits"
        (is (= [{:klasse :objects :name :lieferant}]
               (map :subjekt (:ressourcen s))))))))

(deftest ohne-instanz-ist-der-zustand-unbekannt
  (testing "guarantee 4b — absence of knowledge never renders as a green verdict"
    (let [dir (repo! {"ontology.ent" basis-ent})
          s   (status dir)]
      (is (= :unbekannt (:zustand s))
          "the lifecycle state is the SERVER's; without it the state is `unbekannt`, not `offen`")
      (is (= :kein-changeset (:zustand-lokal s))
          "what git alone could determine is carried beside it, not instead of it")
      (testing "and it still names a step — connecting IS the step"
        (is (string? (:befehl (#'main/nächster-schritt s))))))))

(deftest der-arbeitsbaum-verdeckt-keinen-serverzustand
  (testing "'ein abgelehnter Changeset bleibt abgelehnt, auch wenn nebenher eine Datei geändert ist'"
    (is (= :abgelehnt (#'main/zustand {:wissen? true :änderungen? true :eigene-commits 1
                                       :gepusht? true :check-status :failed})))
    (testing "while a local edit may still ADD `offen` where no server state contradicts it"
      (is (= :offen (#'main/zustand {:wissen? true :änderungen? true :eigene-commits 1
                                     :gepusht? true :check-status :passed}))))
    (testing "and an approval policy alone does not make the CHANGESET pending — that
              is a per-resource marker until a changeset carries the count"
      (is (= :offen (#'main/zustand {:wissen? true :änderungen? true :eigene-commits 0
                                     :gepusht? false}))))))

(deftest jeder-zustand-nennt-genau-einen-schritt
  (testing "guarantee 3, exhaustively — including a state nobody has written a step for yet"
    (doseq [z [:unbekannt :kein-changeset :offen :eingereicht :abgelehnt
               :wartet-auf-zustimmung :landet :fertig :ein-neuer-zustand]]
      (let [{:keys [befehl warum]} (#'main/nächster-schritt {:zustand z})]
        (is (string? befehl) (str z " has no command"))
        (is (string? warum) (str z " has no reason"))))))

(deftest granularität-ist-pro-ressource-nicht-pro-datei
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent "[[:betrag :double]]" "[[:betrag :double] [:datum :date]]"))
    (let [s (status dir)]
      (is (= 1 (count (:ressourcen s))))
      (is (= {:klasse :objects :name :rechnung} (:subjekt (first (:ressourcen s)))))
      (is (= :geändert (:art (first (:ressourcen s)))))
      (testing "the changed KEYS are carried — the impact axis is derived from them"
        (is (= [:attributes] (:felder (first (:ressourcen s)))))))))

(deftest neue-und-entfernte-ressourcen
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str "{:schema {:objects {:lieferant {:attributes [[:name :string]] :match-keys "
                   "[:name]}\n"
                   "                    :kunde {:attributes [[:nr :string]] :match-keys [:nr]}}}}\n"))
    (let [arten (into {} (for [r (:ressourcen (status dir))]
                           [(get-in r [:subjekt :name]) (:art r)]))]
      (is (= :neu (arten :kunde)))
      (is (= :entfernt (arten :rechnung))
          "a resource dropped from the file is REMOVED, not invisible"))))

(deftest dateiplane-verschiebt-den-zustand-nicht
  (testing "'Was als berührt zählt' — file-plane edits are not changeset content"
    (let [dir (repo! {"ontology.ent" basis-ent})]
      (schreib! dir "README.md" "hallo")
      (fs/create-dirs (str dir "/.clj-kondo"))
      (spit (str dir "/.clj-kondo/cache.edn") "{}")
      (let [s (status dir)]
        (is (empty? (:ressourcen s))
            "the platform interprets nothing in README.md — it carries no resource")
        (is (= :kein-changeset (lokal s))
            "and a file it does not interpret must not move the state off `kein-changeset`")
        (is (contains? (set (:nicht-inhalt s)) "README.md")
            "seen and deliberately not counted — silence would read as 'the verb missed my edit'")))))

(deftest ein-eingebetteter-klon-ist-kein-inhalt
  (testing "the code repositories are cloned INTO the ontology checkout, as
            repositories of their own. Nothing in this checkout's tree names
            them, so nothing here may report one as a resource — a status verb
            that did would put a neighbour's work into this author's changeset."
    (let [fremd (repo! {"a.txt" "1"})
          dir   (repo! {"ontology.ent" basis-ent})]
      (p/sh {:err :string} "git" "-c" "protocol.file.allow=always"
            "clone" "-q" fremd (str dir "/sub"))
      (schreib! (str dir "/sub") "a.txt" "2")
      (let [s (status dir)]
        (is (empty? (:ressourcen s)))
        (is (not (contains? (set (:dateiebene s)) "sub")))
        (is (= :kein-changeset (lokal s)))))))

(deftest jeder-befund-trägt-ein-subjekt
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str "{:schema {:objects {:lieferant {:attributes [[:name :string]] :quatsch 1}}\n"
                   "          :sources {:erp {:type :jdbc :config {} :secrets {:pw \"x\"}}}}}\n"))
    (let [befunde (:befunde (status dir))]
      (is (seq befunde))
      (testing "output guarantee 1 — a gate name alone is not output"
        (is (every? #(get-in % [:subjekt :klasse]) befunde))
        (is (every? :regel befunde))
        (is (every? :behebung befunde))
        (is (every? #{:fehler :warnung} (map :schwere befunde))))
      (testing "every rule that fires is lookupable"
        (is (every? #(contains? f/regeln (:regel %)) befunde))))))

(deftest grammatikfehler-blockiert-und-benennt-die-ressource
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent ":match-keys [:beleg]" ":match-keys [:beleg] :quatsch 1"))
    (let [s (status dir)
          b (first (filter #(= :grammatik/verletzt (:regel %)) (:befunde s)))]
      (is (= {:klasse :objects :name :rechnung} (dissoc (:subjekt b) :rest)))
      (is (= :fehler (:schwere b)))
      (is (f/blockiert? (:befunde s)))
      (testing "the resource carrying the error is the blocked one — and only it"
        (is (= {:rechnung :blockiert}
               (into {} (for [r (:ressourcen s)] [(get-in r [:subjekt :name]) (:zustand r)]))))))))

(deftest ein-unbekannter-schlüssel-nennt-sich-und-die-alternativen
  (testing "`disallowed key` names neither the key nor what would have been taken —
            both are in the malli error, so the refusal prints them"
    (let [dir (repo! {"ontology.ent" basis-ent})]
      (schreib! dir "ontology.ent"
                (str/replace basis-ent ":match-keys [:beleg]"
                             ":match-keys [:beleg] :display-name \"Rechnung\""))
      (let [b (first (filter #(= :grammatik/verletzt (:regel %)) (:befunde (status dir))))]
        (is (= :rechnung (get-in b [:subjekt :name])))
        (is (str/includes? (:ist b) ":display-name") "the offending KEY, not just its value")
        (is (str/includes? (:behebung b) "meintest du :name?")
            "`:display-name` is not within two edits of `:name` and is obviously it")
        (is (str/includes? (:behebung b) "Erlaubt sind:") "and what the grammar would take")
        (is (not (str/includes? (:behebung b) "Grammatik ist geschlossen"))
            "the sentence that told the author nothing is gone")))))

(deftest ein-fehlender-pflichtschlüssel-wird-nicht-als-falscher-wert-gemeldet
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent ":match-keys [:beleg]"
                           ":match-keys [:beleg] :backing {:primary-key :beleg}"))
    (let [b (first (filter #(= :grammatik/verletzt (:regel %)) (:befunde (status dir))))]
      (is (str/includes? (:behebung b) "Ergänze den Pflichtschlüssel :dataset"))
      (is (str/includes? (:behebung b) "backing") "…and where it belongs")
      (is (nil? (:ist b)) "there is no observed value to show — the key is absent"))))

(deftest ein-falscher-wert-sagt-die-bedingung-einmal
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent ":match-keys [:beleg]" ":match-keys [:beleg] :name :rechnung"))
    (let [b (first (filter #(= :grammatik/verletzt (:regel %)) (:befunde (status dir))))]
      (is (= "should be a string" (:soll b)) "the grammar's own message is the requirement")
      (is (str/includes? (:behebung b) "Korrigiere den Wert unter name"))
      (is (not (str/includes? (:behebung b) "should be a string"))
          "and it is not printed twice on a three-line finding"))))

(deftest geheimnis-im-baum-blockiert
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              "{:schema {:sources {:erp {:type :jdbc :config {} :secret-cipher \"abc\"}}}}\n")
    (let [b (first (filter #(= :quelle/klartext-geheimnis (:regel %)) (:befunde (status dir))))]
      (is (= :fehler (:schwere b)))
      (is (= :erp (get-in b [:subjekt :name]))))))

(deftest kaputte-datei-wird-ein-befund-kein-absturz
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent" "{:schema {:objects {:lieferant ")
    (let [s (status dir)
          b (first (filter #(= :datei/unlesbar (:regel %)) (:befunde s)))]
      (is (some? b) "an author mid-edit must still get a screen")
      (is (= "ontology.ent" (get-in b [:subjekt :name]))))))

(deftest schlüssel-migration-warnt-blockiert-aber-nicht
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent ":match-keys [:name]" ":match-keys [:name :nr]"))
    (let [s (status dir)
          b (first (filter #(= :neubau/schlüssel-migration (:regel %)) (:befunde s)))]
      (is (= :warnung (:schwere b)))
      (is (not (f/blockiert? (:befunde s))) "a warning appears and does not prevent submitting"))))

(defn- basis-befund [s]
  (first (filter #(= :basis/veralteter-zweig (:regel %)) (:befunde s))))

(defn- ebenen-befund [s]
  (first (filter #(= :ebene/ent-nicht-in-git (:regel %)) (:befunde s))))

(deftest ein-veralteter-zweig-meldet-sich-erst-mit-ungesicherter-arbeit
  (let [upstream (repo! {"ontology.ent" basis-ent})
        dir      (str (fs/create-temp-dir {:prefix "change-alt"}) "/k")]
    (p/sh {:err :string} "git" "clone" "-q" upstream dir)
    (git! dir "config" "user.email" "t@example.com")
    (git! dir "config" "user.name" "T")
    (schreib! upstream "ontology.ent"
              (str/replace basis-ent "[[:betrag :double]]" "[[:betrag :double] [:datum :date]]"))
    (git! upstream "commit" "-qam" "fremd")
    (git! dir "fetch" "-q" "origin")
    (testing "der Abstand allein ist kein Befund — ein sauberer Baum verliert nichts"
      (is (nil? (basis-befund (status dir)))))
    (schreib! dir "ontology.ent"
              (str/replace basis-ent "[[:name :string]]" "[[:name :string] [:ort :string]]"))
    (let [s (status dir)
          b (basis-befund s)]
      (testing "auf einem zurueckhaengenden Zweig editiert — das ist der Befund"
        (is (some? b))
        (is (= 1 (:hinterher (:basis s))))
        (is (contains? f/regeln (:regel b)))
        (is (= :warnung (:schwere b)) "sie mauert den Push nicht zu")
        (is (not (f/blockiert? (:befunde s)))
            "… und sie endet auch nicht rot: ein Push sendet Commits, und committet
             ist hier nichts. Rot wird es, sobald die .ent-Änderung committet ist —
             dann sagt :ebene/ent-nicht-in-git, dass git sie nicht trägt.")))))

(deftest konflikt-ist-semantisch
  (let [upstream (repo! {"ontology.ent" basis-ent})
        dir      (str (fs/create-temp-dir {:prefix "change-klon"}) "/k")]
    (p/sh {:err :string} "git" "clone" "-q" upstream dir)
    (git! dir "config" "user.email" "t@example.com")
    (git! dir "config" "user.name" "T")
    (schreib! upstream "ontology.ent"
              (str/replace basis-ent "[[:betrag :double]]" "[[:betrag :double] [:datum :date]]"))
    (git! upstream "commit" "-qam" "fremd")
    (git! dir "fetch" "-q" "origin")
    (schreib! dir "ontology.ent"
              (-> basis-ent
                  (str/replace ":match-keys [:beleg]" ":match-keys [:beleg :jahr]")
                  (str/replace "[[:name :string]]" "[[:name :string] [:ort :string]]")))
    (let [s (into {} (for [r (:ressourcen (status dir))]
                       [(get-in r [:subjekt :name]) (:zustand r)]))]
      (testing "same resource, different lines — a conflict all the same"
        (is (= :konflikt (s :rechnung))))
      (testing "same file, different resource — not a conflict"
        (is (= :bereit (s :lieferant)))))))

(deftest ausgabe-ist-deterministisch
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent ":match-keys [:name]" ":match-keys [:name :nr]"))
    (is (= (status dir) (status dir))
        "guarantee 6 — same tree + same base ⇒ same output")))

(deftest json-und-schirm-tragen-dieselben-felder
  (let [dir (repo! {"ontology.ent" basis-ent})]
    (schreib! dir "ontology.ent"
              (str/replace basis-ent ":match-keys [:beleg]" ":match-keys [:beleg] :quatsch 1"))
    (let [s (status dir)
          b (first (:befunde s))]
      (testing "guarantee 5 — the finding contract, in full, in both faces"
        (is (every? #(contains? b %) [:subjekt :regel :soll :behebung :schwere])))
      (testing "the next step is a field, not only a rendering"
        (is (:befehl (#'main/nächster-schritt s)))))))

(deftest kein-git-checkout-ist-eine-aussage-kein-stacktrace
  (let [dir (str (fs/create-temp-dir {:prefix "kein-repo"}))
        s   (status dir)]
    (is (string? (:fehler s)))
    (is (= :unbekannt (:zustand s)) "no ground to stand on is `unbekannt`, hence exit ≠ 0")
    (testing "and BOTH faces survive it — the json one has no screen to apologise on"
      (is (string? (:befehl (#'main/nächster-schritt s))))
      (is (string? (with-out-str (main/render-json! s))))
      (is (string? (with-out-str (main/render-text! s)))))))

(deftest jede-regel-ist-nachschlagbar
  (doseq [[id {:keys [bedingung hinweis]}] f/regeln
          sprache catalog/locales]
    (i18n/set-locale! sprache)
    (is (not (str/starts-with? (t bedingung) "\u27E6"))
        (str id " has no condition in plain words (" sprache ")"))
    (is (not (str/starts-with? (t hinweis) "\u27E6"))
        (str id " has no guidance (" sprache ")")))
  (i18n/set-locale! :de))

(deftest sektionen-kommen-aus-der-grammatik
  (testing "a section added to sutra.parser.ent is seen on the next run, not ignored"
    (is (contains? (set ent/sections) :objects))
    (is (contains? (set ent/sections) :transforms))
    (is (contains? (set ent/sections) :actions))))

(deftest der-changeset-bestimmt-den-zustand
  (let [cs (fn [m] (merge {:id "abc" :status :open :zustimmungen 0 :nötig 0} m))]
    (testing "merged with nothing running is done; with a build it is still landing"
      (is (= :fertig (#'main/zustand {:wissen? true :changeset (cs {:status :merged})})))
      (is (= :landet (#'main/zustand {:wissen? true :changeset (cs {:status :merged})
                                      :builds [{:id 1}]}))))
    (testing "one rejection blocks the merge — it outranks a green check board"
      (is (= :abgelehnt (#'main/zustand {:wissen? true :gepusht? true :check-status :passed
                                         :changeset (cs {:ablehner ["b"]})}))))
    (testing "short of its approvals, the change waits — the approval falls at the merge"
      (is (= :wartet-auf-zustimmung
             (#'main/zustand {:wissen? true :gepusht? true
                              :changeset (cs {:zustimmungen 1 :nötig 2})}))))
    (testing "and a changeset with local edits is `offen`, not `kein-changeset`"
      (is (= :offen (#'main/zustand {:wissen? true :änderungen? true :eigene-commits 0
                                     :changeset (cs {})}))))
    (testing "a branch WITHOUT one falls back to git alone"
      (is (= :kein-changeset (#'main/zustand {:wissen? true :eigene-commits 0}))))))

(deftest ein-zweigname-entsteht-aus-dem-titel
  (is (= "lieferant-um-ort-erweitern" (author/zweig-name "Lieferant um Ort erweitern")))
  (testing "umlauts are FOLDED, not stripped — `prfung` is a name nobody recognises"
    (is (= "pruefung-der-massnahme" (author/zweig-name "Prüfung der Maßnahme"))))
  (testing "a title with nothing to slug is refused rather than turned into an empty ref"
    (is (nil? (author/zweig-name "!!!")))
    (is (nil? (author/zweig-name "")))))

(deftest change-start-ohne-instanz-legt-nichts-an
  (testing "a branch whose changeset has to be guessed is the state this verb prevents"
    (let [dir (repo! {"ontology.ent" basis-ent})
          r   (author/start! {:dir dir :titel "Neuer Versuch"})]
      (is (string? (:fehler r)))
      (is (nil? (git/rev dir "neuer-versuch")) "and nothing was created locally either"))))

(deftest die-flaeche-bleibt-einstellig
  (testing "'die Fläche der Autoren-Persona bleibt einstellig' — a surface that
            grows by one verb per feature stops being a surface"
    (let [verben (set (re-seq #"(?m)^     change ([a-z-]+)" (t :change/help)))]
      (is (<= (count verben) 9) (str "zu viele Verben: " verben)))))

(deftest die-man-page-fuehrt-den-lebenszyklus
  (testing "a reader asks 'where am I and
            what comes next', never 'what can I type'"
    (let [txt (t :change/help)]
      (doseq [schritt ["1 ÄNDERN" "2 NACHZIEHEN" "3 PRÜFEN" "4 LANDEN"]]
        (is (str/includes? txt schritt) (str schritt " fehlt in der Man-Page")))
      (testing "and the two facts a git-shaped flow hides are stated there"
        (is (str/includes? txt "git push"))
        (is (re-find #"(?i)compute" txt))))))

(deftest jedes-verb-der-flaeche-ist-verdrahtet
  (testing "a verb in the man page that no branch answers is a promise the
            binary does not keep"
    (doseq [v ["start" "check" "sync" "ask-review" "review" "land" "status" "explain"]]
      (is (str/includes? (t :change/help) (str "change " v))
          (str v " fehlt in der Man-Page")))))

(deftest die-anmeldung-des-operator-cli-ist-auffindbar
  (let [paths (srv/cred-paths "/k" "/opt/sutra" "/home/u")]
    (testing "the operator CLI's own home is searched"
      (is (some #{"/opt/sutra/.run/cli-creds.edn"} paths)))
    (testing "but the CHECKOUT's own credential still wins"
      (is (= "/k/.run/cli-creds.edn" (first paths)))
      (is (< (.indexOf paths "/k/.run/cli-creds.edn")
             (.indexOf paths "/opt/sutra/.run/cli-creds.edn"))))
    (testing "an unrelated checkout is never hunted through"
      (is (= 4 (count paths)))))
  (testing "an absent environment simply drops that candidate"
    (is (= ["/k/.run/cli-creds.edn" ".run/cli-creds.edn"]
           (srv/cred-paths "/k" nil nil))))
  (testing "a credential IS read through that path"
    (let [home (str (fs/create-temp-dir))]
      (fs/create-dirs (str home "/.run"))
      (spit (str home "/.run/cli-creds.edn")
            (pr-str {:users {:admin {:username "admin" :password "geheim"}}}))
      (is (= "geheim"
             (:password (some (fn [p] (when (fs/regular-file? p)
                                        (:admin (:users (edn/read-string (slurp p))))))
                              (srv/cred-paths "/nirgends" home nil))))))))

(deftest die-ablehnung-nennt-ihren-grund
  (testing "a refused push never reaches post-receive: the gate's word arrives as
            the report-status `ng`, not as a `remote:` side-band line"
    (let [ausgabe (str "To http://git.example.invalid/git/ontology\n"
                       " ! [remote rejected] mein-zweig -> mein-zweig"
                       " (checks failed — {:duplicate-names {:lieferant 2}}"
                       " · NOTHING was written; fix and push again)\n"
                       "error: failed to push some refs\n")
          gründe  (author/gründe-aus ausgabe)]
      (is (= 1 (count gründe)))
      (is (str/includes? (first gründe) ":duplicate-names"))
      (is (str/includes? (first gründe) "NOTHING was written"))))

  (testing "the side-band shape still reads — an ACCEPTED push reports through it"
    (is (= ["sutra: ci/sutra-publish passed"]
           (author/gründe-aus "remote: sutra: ci/sutra-publish passed\nTo http://x\n"))))

  (testing "and git's own noise is not mistaken for a reason"
    (is (= [] (author/gründe-aus "To http://x\nEverything up-to-date\n")))))

(deftest ein-neuer-zweig-wird-vom-serverstand-abgezweigt
  (testing "a clone's `origin/main` is only as new as its last fetch, so `start`
            does that fetch rather than fork off whatever is lying around"
    (let [ferne (repo! {"ontology.ent" basis-ent})
          dir   (str (fs/create-temp-dir {:prefix "change-klon"}))]
      (fs/delete-tree dir)
      (git! "." "clone" "-q" (str ferne) dir)
      (git! dir "config" "user.email" "t@example.com")
      (git! dir "config" "user.name" "T")
      (schreib! ferne "ontology.ent" (str/replace basis-ent ":beleg" ":belegnr"))
      (git! ferne "add" "-A") (git! ferne "commit" "-qm" "weiter")
      (is (= "origin/main" (#'author/fork-basis dir "neu" "main"))
          "the remote-tracking ref is what a fork is cut from")
      (is (not= (git/rev ferne "main") (git/rev dir "origin/main"))
          "and without a fetch it is a commit behind")
      (#'author/holen! dir)
      (is (= (git/rev ferne "main") (git/rev dir "origin/main"))
          "the fetch is what makes the base the instance's state")))

  (testing "a checkout with no remote at all forks from the local trunk"
    (let [dir (repo! {"ontology.ent" basis-ent})]
      (is (= "main" (#'author/fork-basis dir "neu" "main"))))))

(deftest ein-abgelehnter-push-strandet-den-autor-nicht
  (testing "a refusal leaves the commit made and the index empty — refusing there
            for `nichts vorgemerkt` stranded the fix that was already committed"
    (let [bare (str (fs/create-temp-dir {:prefix "change-bare"}))
          dir  (repo! {"ontology.ent" basis-ent})]
      (git! bare "init" "-q" "--bare" "-b" "main")
      (git! dir "remote" "add" "origin" (str bare))
      (git! dir "push" "-q" "-u" "origin" "main")
      (schreib! dir "ontology.ent" (str/replace basis-ent ":beleg" ":belegnr"))
      (git! dir "add" "-A")
      (git! dir "commit" "-qm" "die behobene Fassung")
      (let [r (author/push! {:dir dir :message "die behobene Fassung"})]
        (is (nil? (:fehler r)) "the unsent commit is what makes the push meaningful")
        (is (false? (:abgelehnt? r)))
        (is (= "die behobene Fassung" (:nachricht-ungenutzt r))
            "and the sentence that found no commit is named, not swallowed")
        (is (= (git/rev dir "HEAD") (git/rev bare "main")))))

    (testing "with nothing staged AND nothing unsent it still refuses"
      (let [bare (str (fs/create-temp-dir {:prefix "change-bare2"}))
            dir  (repo! {"ontology.ent" basis-ent})]
        (git! bare "init" "-q" "--bare" "-b" "main")
        (git! dir "remote" "add" "origin" (str bare))
        (git! dir "push" "-q" "-u" "origin" "main")
        (schreib! dir "ontology.ent" (str basis-ent "\n;; unvorgemerkt\n"))
        (let [r (author/push! {:dir dir :message "geht so nicht"})]
          (is (str/includes? (str (:fehler r)) "nichts vorgemerkt"))
          (is (seq (:dreckig r))))))))

(deftest ein-fremder-befund-ist-als-fremd-markiert
  (testing "`sammeln` reads the whole tree — correctly, since a file the author
            never opened can break the push. But a finding about a resource this
            change does not touch is not the author's, and printed in one list it
            was indistinguishable from one that is."
    (let [kaputt (str "{:schema\n"
                      " {:objects\n"
                      "  {:lieferant {:attributes [[:name :string]] :match-keys [:name] :quatsch "
                      "1}\n"
                      "   :rechnung  {:attributes [[:betrag :double]] :match-keys [:beleg]}}}}\n")
          dir    (repo! {"ontology.ent" kaputt})]
      (schreib! dir "ontology.ent" (str/replace kaputt ":betrag :double" ":betrag :decimal"))
      (let [befunde (:befunde (status dir))
            von     (fn [nm] (first (filter #(= nm (get-in % [:subjekt :name])) befunde)))]
        (is (:fremd? (von :lieferant))
            "already broken before this change — marked")
        (is (seq befunde))
        (is (not-any? :fremd? (filter #(= :rechnung (get-in % [:subjekt :name])) befunde))
            "and a finding on a touched resource is never marked")))))

(deftest ein-geerbtes-problem-blockiert-nicht
  (testing "`:fremd?` says the resource is untouched; it does not say the problem
            was already there. A text's old errors blocked every change that
            merely shared the file with them — a stale projection in git made
            that every change there was."
    (let [kaputt (str "{:schema\n"
                      " {:objects\n"
                      "  {:lieferant {:attributes [[:name :string]] :match-keys [:name] :quatsch "
                      "1}\n"
                      "   :rechnung  {:attributes [[:betrag :double]] :match-keys [:beleg]}}}}\n")
          dir    (repo! {"ontology.ent" kaputt})]
      (schreib! dir "ontology.ent" (str/replace kaputt ":betrag :double" ":betrag :decimal"))
      (let [befunde (:befunde (status dir))
            von     (fn [nm] (first (filter #(= nm (get-in % [:subjekt :name])) befunde)))]
        (is (= :fehler (:schwere (von :lieferant))) "the old error is still reported")
        (is (:geerbt? (von :lieferant)) "… and named as one the base already carried")
        (is (not (f/blockiert? befunde))
            "so it no longer stops a change that did not cause it")))))

(deftest ein-verursachter-befund-blockiert-auch-an-fremder-ressource
  (testing "the counter-case the naive filter would have broken: a reference this
            change breaks sits on a resource it never touched, so it is `:fremd?`
            too — and it must still block. The base is what tells them apart."
    (let [vorher [{:regel :grammatik/verletzt :schwere :fehler
                   :subjekt {:klasse :objects :name :lieferant}}]
          jetzt  [{:regel :grammatik/verletzt :schwere :fehler :fremd? true
                   :subjekt {:klasse :objects :name :lieferant}}
                  {:regel :referenz/ungelöst :schwere :fehler :fremd? true
                   :subjekt {:klasse :actions :name :rechnung-buchen}}]
          m      (f/markiere-geerbt jetzt vorher)]
      (is (:geerbt? (first m)))
      (is (not (:geerbt? (second m))) "the base did not carry this one")
      (is (f/blockiert? m))
      (is (not (f/blockiert? (filterv :geerbt? m)))
          "and the inherited one alone does not block"))))

(deftest ein-ent-text-gehoert-nicht-git
  (testing "git carries Python. A committed `.ent` change cannot land through a
            push — it would install the whole tree — so the gate says that
            instead of judging the text's content."
    (let [upstream (repo! {"ontology.ent" basis-ent})
          dir      (str (fs/create-temp-dir {:prefix "change-ebene"}) "/k")]
      (p/sh {:err :string} "git" "clone" "-q" upstream dir)
      (git! dir "config" "user.email" "t@example.com")
      (git! dir "config" "user.name" "T")
      (schreib! dir "ontology.ent"
                (str/replace basis-ent ":match-keys [:name]" ":match-keys [:name :nr]"))
      (testing "in the working tree it is said, not refused — a push sends commits"
        (let [s (status dir)]
          (is (= :warnung (:schwere (ebenen-befund s))))
          (is (not (f/blockiert? (:befunde s))))))
      (git! dir "commit" "-qam" "ontologie per git")
      (testing "committed, it is the refusal — and it names the way that works"
        (let [s (status dir)
              b (ebenen-befund s)]
          (is (= :fehler (:schwere b)))
          (is (f/blockiert? (:befunde s)))
          (is (contains? f/regeln (:regel b)))
          (is (str/includes? (:behebung b) "projection push")))))))

(def ^:private repo-ent
  (str "{:schema\n"
       " {:transforms\n"
       "  {:zeilen {:language :python :code-file \"transforms/zeilen.py\"\n"
       "            :output {:kind :dataset :dataset :zeilen}}}}}\n"))

(deftest ein-python-koerper-bleibt-bei-git
  (testing "a transform body is changeset content — entity plane, so the diff sees
            it — and at the same time git's own plane. The rule about `.ent` texts
            must not judge it: it would refuse the one way code has."
    (let [upstream (repo! {"etl.ent" repo-ent "transforms/zeilen.py" "def t(i):\n    return []\n"})
          dir      (str (fs/create-temp-dir {:prefix "change-py"}) "/k")]
      (p/sh {:err :string} "git" "clone" "-q" upstream dir)
      (git! dir "config" "user.email" "t@example.com")
      (git! dir "config" "user.name" "T")
      (schreib! dir "transforms/zeilen.py" "def t(i):\n    return [{}]\n")
      (testing "uncommitted"
        (is (nil? (ebenen-befund (status dir)))))
      (git! dir "commit" "-qam" "python per git")
      (testing "committed — this is the documented way and stays open"
        (is (nil? (ebenen-befund (status dir)))))
      (testing "the `.ent` of the same checkout still is the ontology's plane"
        (schreib! dir "etl.ent" (str/replace repo-ent ":zeilen {" ":zeilen2 {"))
        (git! dir "commit" "-qam" "ent per git")
        (is (= :fehler (:schwere (ebenen-befund (status dir)))))))))

(deftest ein-zweig-ohne-changeset-wird-nicht-gepusht
  (testing "a branch made by hand is the way OUT of this flow: the push lands a
            ref nobody proposed and the author is back on propose/approve/merge.
            `:keiner` is a fact about the branch and refuses."
    (let [dir (repo! {"ontology.ent" basis-ent})]
      (schreib! dir "ontology.ent" (str basis-ent "\n;; x\n"))
      (git! dir "add" "-A")
      (with-redefs [srv/verbinden! (fn [_] {:basis "http://x" :token "t"})
                    srv/changeset  (fn [_ _] :keiner)]
        (let [r (author/push! {:dir dir :message "ohne changeset"})]
          (is (str/includes? (str (:fehler r)) "keinen offenen Changeset"))
          (is (some #(str/includes? % "change start") (:gründe r))
              "and it names the door rather than only the refusal")))))

  (testing "a FAILED read answers nil, and nil may not refuse — an unreachable
            instance is not an author without a changeset"
    (let [bare (str (fs/create-temp-dir {:prefix "change-bare3"}))
          dir  (repo! {"ontology.ent" basis-ent})]
      (git! bare "init" "-q" "--bare" "-b" "main")
      (git! dir "remote" "add" "origin" (str bare))
      (git! dir "push" "-q" "-u" "origin" "main")
      (schreib! dir "ontology.ent" (str basis-ent "\n;; y\n"))
      (git! dir "add" "-A")
      (with-redefs [srv/verbinden! (fn [_] {:basis "http://x" :token "t"})
                    srv/changeset  (fn [_ _] nil)]
        (let [r (author/push! {:dir dir :message "instanz stumm"})]
          (is (nil? (:fehler r)))
          (is (false? (:abgelehnt? r))))))))

(deftest eine-ablehnung-verschweigt-den-lokalen-commit-nicht
  (testing "the commit is made BEFORE the push is tried, so a refusal leaves it
            standing — `NICHTS wurde geschrieben` was true of the instance and
            false of the checkout, and the author re-staged committed work"
    (let [bare (str (fs/create-temp-dir {:prefix "change-bare4"}))
          dir  (repo! {"ontology.ent" basis-ent})]
      (git! bare "init" "-q" "--bare" "-b" "main")
      (git! dir "remote" "add" "origin" (str bare))
      (git! dir "push" "-q" "-u" "origin" "main")
      (let [andere (str (fs/create-temp-dir {:prefix "change-andere"}))]
        (fs/delete-tree andere)
        (git! "." "clone" "-q" (str bare) andere)
        (git! andere "config" "user.email" "a@example.com")
        (git! andere "config" "user.name" "A")
        (schreib! andere "ontology.ent" (str basis-ent "\n;; fremd\n"))
        (git! andere "add" "-A")
        (git! andere "commit" "-qm" "fremder stand")
        (git! andere "push" "-q" "origin" "main"))
      (schreib! dir "ontology.ent" (str basis-ent "\n;; meins\n"))
      (git! dir "add" "-A")
      (let [r (author/push! {:dir dir :message "meine fassung"})]
        (is (true? (:abgelehnt? r)))
        (is (true? (:committet? r)))
        (is (str/includes? (str (:fehler r)) "Commit steht lokal")
            "the checkout's half of the truth")
        (is (some #(str/includes? % "kein Vorfahr deines Zweigs") (:gründe r))
            "and `non-fast-forward` is diagnosed, not merely reproduced")))))

(deftest ein-unterbrochener-rebase-wird-nicht-gepusht
  (testing "a stopped rebase is named as one, before anything is committed —
            not reported as a grammar finding on its conflict markers"
    (let [dir (repo! {"ontology.ent" basis-ent})]
      (git! dir "switch" "-q" "-c" "zweig")
      (schreib! dir "ontology.ent" (str basis-ent ";; meins\n"))
      (git! dir "commit" "-qam" "meins")
      (git! dir "switch" "-q" "main")
      (schreib! dir "ontology.ent" (str basis-ent ";; fremd\n"))
      (git! dir "commit" "-qam" "fremd")
      (git! dir "switch" "-q" "zweig")
      (git! dir "rebase" "main")
      (is (= :rebase (git/laufender-vorgang dir)))
      (let [kopf (git/head-sha dir)
            r    (with-redefs [srv/verbinden! (fn [_] (throw (ex-info "kein Server" {})))]
                   (author/push! {:dir dir :alles? true :message "mitten drin"}))]
        (is (str/includes? (str (:fehler r)) "rebase"))
        (is (some #(str/includes? % "git rebase --continue") (:gründe r)))
        (is (= kopf (git/head-sha dir)) "nothing was committed"))
      (git! dir "rebase" "--abort")
      (is (nil? (git/laufender-vorgang dir))))))

(deftest ein-zweig-ohne-gemeinsame-geschichte-nennt-das-rezept
  (testing "the instance minted origin/<zweig> as its own root: same tree, no
            merge base. `pull --rebase` would replay the whole trunk, and
            `status` counted every trunk commit as 'behind'"
    (let [bare (str (fs/create-temp-dir {:prefix "change-bare5"}))
          dir  (repo! {"ontology.ent" basis-ent})]
      (git! bare "init" "-q" "--bare" "-b" "main")
      (git! dir "remote" "add" "origin" (str bare))
      (git! dir "commit" "-q" "--allow-empty" "-m" "zweite")
      (git! dir "push" "-q" "-u" "origin" "main")
      (let [andere (str (fs/create-temp-dir {:prefix "change-wurzel"}))]
        (git! andere "init" "-q" "-b" "zweig")
        (git! andere "config" "user.email" "a@example.com")
        (git! andere "config" "user.name" "A")
        (spit (str andere "/ontology.ent") basis-ent)
        (git! andere "add" "-A")
        (git! andere "commit" "-qm" "history anchored")
        (git! andere "push" "-q" (str bare) "zweig"))
      (git! dir "fetch" "-q" "origin")
      (git! dir "switch" "-q" "-c" "zweig" "main")
      (schreib! dir "ontology.ent"
                (str/replace basis-ent ":match-keys [:name]" ":match-keys [:name :nr]"))
      (git! dir "add" "-A")
      (let [r (author/push! {:dir dir :message "meins"})]
        (is (true? (:abgelehnt? r)))
        (is (some #(str/includes? % "git rebase --onto origin/zweig origin/main") (:gründe r))
            "the recipe that moves only the author's commits")
        (is (not-any? #(str/includes? % "git pull --rebase origin") (:gründe r))))
      (git! dir "rebase" "-q" "--onto" "origin/zweig" "origin/main")
      (git! dir "branch" "-q" "--set-upstream-to" "origin/zweig")
      (let [s (status dir)]
        (is (nil? (:hinterher (:basis s))) "no 'N commits behind' without a merge base")
        (is (true? (:ohne-gemeinsame-geschichte (:basis s))))
        (is (= [{:klasse :objects :name :lieferant}] (mapv :subjekt (:ressourcen s)))
            "…and the change is still the diff against the trunk's tree")))))

(deftest jede-oberflaechen-zeichenkette-steht-in-beiden-sprachen
  (testing "a key only one bundle carries ships a screen half in the wrong
            language — the bundles are one surface with two faces"
    (let [bündel   (into {} (for [l catalog/locales]
                              [l (edn/read-string
                                  (slurp (io/resource (str "i18n/change." (name l) ".edn"))))]))
          schlüssel (into #{} (mapcat keys) (vals bündel))]
      (doseq [[l m] bündel]
        (is (= schlüssel (set (keys m)))
            (str l " fehlt: " (sort (remove (set (keys m)) schlüssel)))))
      (testing "and every key the client asks for has a translation"
        (let [benutzt (into #{}
                            (comp (filter #(str/ends-with? (str %) ".clj"))
                                  (mapcat #(re-seq #":change/[a-zäöüß0-9-]+" (slurp %)))
                                  (map #(keyword (subs % 1))))
                            (file-seq (io/file "src")))]
          (is (seq benutzt))
          (is (empty? (remove schlüssel benutzt))
              (str "ohne Übersetzung: " (sort (remove schlüssel benutzt)))))))))

(deftest sql-char-ohne-laenge-blockt-lokal-und-ohne-instanz-ist-sql-unbekannt
  (let [dir (repo! {"ontology.ent" "{:schema {}}\n"})]
    (schreib! dir "ontology.ent"
              (str "{:schema {:transforms {:zeit-schluessel {:language :pg-sql\n"
                   "  :query \"SELECT CAST(z.jahr AS CHAR) AS k -- CAST(z.x AS CHAR)\\n FROM z\"\n"
                   "  :inputs {:z {:kind :dataset :dataset :zeit}}}\n"
                   "  :kommentar {:language :pg-sql :query \"SELECT 'CAST(a AS CHAR)' AS k FROM z -- "
                   "YEAR(x)\"\n"
                   "  :inputs {:z {:kind :dataset :dataset :zeit}}}}}}\n"))
    (let [r (status dir)
          sql (filter #(= "sql" (namespace (:regel %))) (:befunde r))
          rot (filter #(= :sql/char-ohne-laenge (:regel %)) sql)]
      (is (= [:zeit-schluessel] (map (comp :name :subjekt) rot)))
      (is (= "CAST(z.jahr AS CHAR)" (:ist (first rot))))
      (is (:sql-offen? r) "ohne Instanz ist das SQL ungeprueft, nie gruen")
      (is (= :unbekannt (:zustand r)))
      (is (some #(= :sql/ungeprueft (:regel %)) sql)))))
