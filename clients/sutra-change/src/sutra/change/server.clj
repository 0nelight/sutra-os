(ns sutra.change.server
  (:require [sutra.change.git :as git]
            [sutra.i18n.core :refer [t]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.parser.impact :as impact]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]
            [sutra.wire.creds :as creds]
            [sutra.wire.transport :as tx]))

(def ^:private frist-ms
  8000)

(defn basis-url
  [dir]
  (or (not-empty (str (System/getenv "SUTRA_HTTP_URL")))
      (when-let [url (git/remote-url dir)]
        (or (second (re-find #"^(https?://[^/]+)" url)) nil))))

(defn repo-rid
  [dir]
  (when-let [url (git/remote-url dir)]
    (or (second (re-find #"/git/r/([^/]+)\.git/?$" url))
        (when (re-find #"^https?://[^/]+/?$|/git/ontology\.git/?$" url) rid/ontology-repo-rid))))

(defn cred-paths
  [dir sutra-home home]
  (cond-> [(str dir "/.run/cli-creds.edn")
           ".run/cli-creds.edn"]
    sutra-home (conj (str sutra-home "/.run/cli-creds.edn"))
    home       (conj (str home "/.sutra/cli-creds.edn"))))

(defn- creds
  [dir base]
  (some (fn [p]
          (try (let [f (java.io.File. ^String p)]
                 (when (.isFile f)
                   (not-empty (creds/bound-user (edn/read-string (slurp f)) base :admin nil))))
               (catch Exception _ nil)))
        (cred-paths dir (System/getenv "SUTRA_HOME") (System/getenv "HOME"))))

(defn- anmelden!
  [dir base]
  (let [{:keys [username password token]} (:user (creds dir base))
        pw (or password (not-empty (str (System/getenv "SUTRA_ADMIN_PASSWORD"))))]
    (or (when pw
          (try (let [r (api/post! :login {:username (or username "admin") :password pw})]
                 (when (<= 200 (:status r) 299) (:token (:body r))))
               (catch Exception _ nil)))
        token)))

(defn- trunk-policy
  [tok]
  (try (binding [tx/*auth-token* tok]
         (let [r (api/get! :branch-policy {:name "main"})]
           (when (<= 200 (:status r) 299) (:body r))))
       (catch Exception _ nil)))

(defn verbinden!
  [dir]
  (when-let [base (basis-url dir)]
    (tx/set-base! base)
    (if-let [tok (anmelden! dir base)]
      {:basis base :token tok :rid (repo-rid dir)}
      (when-let [pol (trunk-policy nil)]
        {:basis base :token nil :rid (repo-rid dir) :trunk-policy pol}))))

(defn trunk-geschuetzt?
  [sess]
  (when sess
    (some-> (or (:trunk-policy sess) (trunk-policy (:token sess))) :protected boolean)))

(defn warum-nicht
  [dir]
  (let [base (basis-url dir)]
    (cond
      (nil? base)
      (t :change/err-no-instance-url)

      (:refused (creds dir base))
      (t :change/err-creds-other-instance (:refused (creds dir base)))

      (nil? (:user (creds dir base)))
      (t :change/err-no-credentials
         {:url base
          :orte (str (or dir ".") "/.run/cli-creds.edn, ./.run/cli-creds.edn"
                     (when-let [h (System/getenv "SUTRA_HOME")] (str ", " h "/.run/cli-creds.edn"))
                     ", " (System/getenv "HOME") "/.sutra/cli-creds.edn")})

      :else
      (t :change/err-login-refused {:url base}))))

(defn- lies
  [sess f]
  (try
    (binding [tx/*auth-token* (:token sess)]
      (let [r (f)] (when (<= 200 (:status r) 299) (:body r))))
    (catch Exception _ nil)))

(defn- innerhalb-der-frist
  [f]
  (let [fut (future (try (f) (catch Throwable _ nil)))
        v   (deref fut frist-ms ::frist)]
    (when (= v ::frist) (future-cancel fut))
    (when (not= v ::frist) v)))

(defn live-schema
  [sess]
  (when sess
    (innerhalb-der-frist #(lies sess api/current-schema))))

(defn- abhängige
  [schema nm]
  (impact/dependents-of schema nm))

(def ^:private historie-limit
  500)

(defn- schreib-historie
  [sess nm tage]
  (let [b    (lies sess #(api/edit-history (keyword nm) {:limit historie-limit}))
        rows (:provenance b)]
    (when (seq rows)
      (let [grenze (- (System/currentTimeMillis) (* (long tage) 86400000))
            ms     (fn [r] (try (.toEpochMilli (java.time.Instant/parse (str (:updated_at r))))
                                (catch Exception _ 0)))
            jung   (filter #(>= (ms %) grenze) rows)]
        {:objekte-mit-edits (count (distinct (keep :pk rows)))
         :zeilen (count rows)
         :abgeschnitten (>= (count rows) historie-limit)
         :im-fenster (count (distinct (keep :pk jung)))
         :arten (vec (sort (distinct (keep #(some-> (:edit_kind %) str) rows))))
         :letzter (some->> rows (map :updated_at) (remove nil?) sort last str)}))))

(defn wirkung
  [sess diff tage]
  (when sess
    (innerhalb-der-frist
     (fn []
       (let [schema (lies sess api/current-schema)
             counts (:by-type (lies sess api/object-counts))
             typen  (for [[[sek nm] {:keys [art felder]}] diff :when (= :objects sek)]
                      [nm art felder])]
         (when schema
           (vec (for [[nm art felder] typen
                      :let [dep (abhängige schema (name nm))
                            hist (schreib-historie sess nm tage)
                            neubau? (boolean (some #{:match-keys :primary-key} felder))]
                      :when (or dep hist)]
                  (cond-> {:subjekt {:klasse :objects :name nm} :art art}
                    dep    (assoc :abhängige (dissoc dep :rid :idx))
                    counts (assoc :objekte (get counts (keyword nm)))
                    hist   (assoc :historie hist)
                    neubau? (assoc :neubau true))))))))))

(defn changeset
  [sess zweig]
  (when (and sess zweig)
    (innerhalb-der-frist
     (fn []
       (when-let [ps (:proposals (lies sess #(api/list-proposals {:status "all" :source zweig})))]
         (let [meine (filter #(= (str zweig) (str (:source-branch %))) ps)
               p     (or (first (filter #(= "open" (str (:status %))) meine))
                         (first (filter #(= "merged" (str (:status %))) meine)))]
           (if-not p
             :keiner
             {:id     (:id p)
              :titel  (:title p)
              :status (keyword (str (:status p)))
              :von    (:source-branch p) :nach (:target-branch p)
              :autor  (:author p)
              :angelegt (:created-at p)
              :zustimmungen (or (:approval-count p) 0)
              :nötig        (count (:open-tasks p))
              :stufe        (:stage p)
              :alleingang   (boolean (:auto-land p))
              :gefragt      (vec (:reviewers p))
              :zustimmer    (vec (:approvals p))
              :ablehner     (vec (:rejections p))})))))))

(defn zweig-steht?
  [sess zweig]
  (when (and sess zweig)
    (innerhalb-der-frist
     (fn []
       (when-let [bs (:branches (lies sess #(api/list-branches nil)))]
         (boolean (some #(= (str zweig) (str (:name %))) bs)))))))

(defn tasks
  [sess changeset-id]
  (when (and sess changeset-id)
    (innerhalb-der-frist
     (fn [] (:tasks (lies sess #(api/get! :proposal-tasks {:id changeset-id})))))))

(defn bereitschaft
  [sess zweig typen]
  (when (and sess zweig (seq typen))
    (innerhalb-der-frist
     (fn []
       (tx/with-branch zweig
         (when-let [est (lies sess #(api/build-estimate {:only typen}))]
           (let [cnt (:by-type (lies sess api/object-counts))]
             {:zweig  zweig
              :llm    (vec (:transforms est))
              :kosten (:total est)
              :typen  (vec (for [t typen]
                             {:typ t :objekte (get cnt (keyword (name t)))}))})))))))

(defn bauen!
  [sess zweig typen]
  (when (and sess zweig (seq typen))
    (innerhalb-der-frist
     (fn []
       (tx/with-branch zweig
         (lies sess #(api/post! :import-run {:only (vec typen)})))))))

(defn nachleben
  [sess sha]
  (when (and sess sha)
    (innerhalb-der-frist
     (fn []
       (let [checks (when (:rid sess)
                      (lies sess #(api/get! :repo-checks {:rid (:rid sess) :commit sha})))
             builds (:builds (lies sess api/active-builds))]
         (cond-> {}
           checks (assoc :checks (:checks checks) :check-status (:status checks))
           (seq builds) (assoc :builds (vec (take 5 builds)))))))))

(defn zustimmung
  [dir trunk pfade]
  (when-let [txt (git/show dir trunk "sutra.repo.edn")]
    (let [policy (try (:approval-policy (edn/read-string txt)) (catch Exception _ nil))]
      (seq (for [{:keys [paths reviewers]} policy
                 :let [treffer (filterv (fn [p] (some #(str/starts-with? p (str %)) paths)) pfade)]
                 :when (seq treffer)]
             {:pfade treffer :reviewer (vec reviewers)})))))

(defn sql-urteil
  [sess sql-tx]
  (when (and sess (seq sql-tx))
    (innerhalb-der-frist
     #(let [b (lies sess (fn [] (api/post! :schema-sql-check {:transforms sql-tx})))]
        (when (:compiled? b) b)))))

(defn plattform-dateien
  [sess]
  (when (and sess (rid/ontology-repo? (:rid sess)))
    (innerhalb-der-frist
     (fn [] (:files (lies sess #(api/get! :repo-platform-files {:rid (:rid sess)})))))))
