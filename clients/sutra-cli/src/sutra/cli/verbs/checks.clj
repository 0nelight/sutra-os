(ns sutra.cli.verbs.checks
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :as errors :refer [req!]]
            [sutra.cli.live :refer [check-report live-schema rider-kinds]]
            [sutra.cli.local :refer [checkout-of checkout-root commit-home
                                     ent-merge-entity-schemas merge-view-files resolve-code-files
                                     vom-aufrufer]]
            [sutra.cli.out :refer [emit! grid mark paint render]]
            [sutra.i18n.core :as i18n]
            [clojure.edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.cli.view :refer
             [plural problem-line review-glyph row-class row-name row-verb verb-codes]]))

(declare diff-preset!)

(defn- authoring-file?          [& a] (apply @(requiring-resolve 'sutra.parser.authoring/file?) a))
(defn- pe-read-texts            [& a]
  (apply @(requiring-resolve 'sutra.parser.project-export/read-texts) a))
(defn- checkout-schema
  [live paths]
  (let [{:keys [rel formats]} (checkout-of live (first paths))
        by-rel (into (sorted-map) (map (fn [p] [(rel (io/file ^String p)) p])) paths)]
    (doseq [[r p] by-rel :when (not (authoring-file? formats r))]
      (throw (ex-info (str p ": no kind of text of this checkout's projection reads it")
                      {:file p})))
    (-> (pe-read-texts formats (keys by-rel) #(slurp (get by-rel %)))
        :schema
        (resolve-code-files (first paths)))))

(defn- candidate-schema
  [files live]
  (let [ps     (map vom-aufrufer files)
        groups (vals (group-by #(.getPath ^java.io.File (checkout-root %)) ps))
        own    (:schema (ent-merge-entity-schemas
                         (map (fn [g] {:schema (checkout-schema live g)}) groups)))]
    (reduce #(merge-view-files %1 (first %2) live) own groups)))

(defmethod step! :diff-schema [ctx [_ {:keys [preset presets]}]]
  (let [files (into (if preset [preset] []) (remove str/blank?) presets)]
    (if (empty? files)
      (step! ctx [:context {}])
      (diff-preset! ctx files))))

(defn- clip-to
  [n s]
  (let [s (str/trim (str/replace (str s) #"\s+" " "))]
    (if (> (clojure.core/count s) n) (str (subs s 0 n) "…") s)))

(defn- preflight-lines
  [problems]
  (let [failed (:relational-checks-failed problems)
        detail (dissoc problems :relational-checks-failed)
        n      (reduce + 0 (for [[_ v] detail] (if (sequential? v) (clojure.core/count v) 1)))]
    [(mark :red "✗" (i18n/t :cli/would-be-refused {:n n}))
     (for [[k items] detail
           :let [items (if (sequential? items) items [items])]]
       [(str "    " (paint :bold (clojure.core/name k)))
        (for [it (take 8 items)]
          (str "      " (paint :dim "·") " " (clip-to 180 (problem-line it))))
        (when (> (clojure.core/count items) 8)
          (str "      "
               (paint :dim (i18n/t :cli/n-more {:n (- (clojure.core/count items) 8)}))))])
     (when (seq failed)
       (str "    "
            (paint :dim
                   (i18n/t :cli/failing-checks
                           {:checks (str/join ", " (map #(clojure.core/name %) failed))}))))
     (mark :dim "•" (i18n/t :cli/gates-run-at-commit))]))

(defn- advisory-lines
  [zeilen]
  [(mark :yellow "!" (i18n/t :cli/advisories {:n (clojure.core/count zeilen)}))
   (for [z (take 8 zeilen)]
     (str "      " (paint :dim "·") " " (clip-to 220 z)))
   (when (> (clojure.core/count zeilen) 8)
     (str "      " (paint :dim (i18n/t :cli/n-more {:n (- (clojure.core/count zeilen) 8)}))))
   (str "    " (paint :dim (i18n/t :cli/advisories-full)))])

(defn- push-repository
  [file]
  (when-let [{:keys [root]} (commit-home (vom-aufrufer file))]
    (let [{:keys [rid repo]} (try ((requiring-resolve 'sutra.cli.git/repo-identity) root)
                                  (catch Exception _ nil))]
      (or rid repo))))

(defn- diff-files
  [files]
  (let [homes (map #(commit-home (vom-aufrufer %)) files)
        roots (into #{} (map :root) homes)
        root  (first roots)
        ents  (when (and root (= 1 (count roots)) (every? some? homes))
                (filter #(and (.isFile ^java.io.File %)
                              (str/ends-with? (.getName ^java.io.File %) ".ent"))
                        (.listFiles (io/file root))))]
    (if (seq ents)
      (vec (sort (map #(.getPath ^java.io.File %) ents)))
      (vec files))))

(defn- retiring-lines
  [{:keys [retiring orphaned strict-repository]}]
  [(when (seq orphaned)
     (mark :yellow "!" (i18n/t :cli/orphaned-producers {:namen (str/join ", " orphaned)})))
   (if (nil? strict-repository)
     (mark :dim "•" (i18n/t :cli/diff-not-strict))
     (when (seq retiring)
       [(str "    " (paint :red "−") " " (paint :red (str (count retiring))) " "
             (paint :bold (i18n/t :cli/retiring-by-omission)))
        (for [{:keys [kind entity status]} retiring]
          (str "      " (paint :dim "·") " " (clojure.core/name kind) " "
               (paint :bold (clojure.core/name entity))
               (paint :dim (str " (" (clojure.core/name (or status :experimental)) ")"))))]))])

(defn- riders-of-files
  [texts]
  (let [forms (keep #(try (clojure.edn/read-string %) (catch Exception _ nil)) texts)
        top   (apply merge (map #(dissoc % :schema) (filter map? forms)))]
    (into {} (for [k (keys rider-kinds)]
               [k (into {} (for [r (get top k) :when (:name r)] [(str (:name r)) r]))]))))

(defn- upstream-texts
  [files]
  (when-let [{:keys [root]} (commit-home (vom-aufrufer (first files)))]
    (let [git  #(apply sh/sh "git" "-C" (str root) %&)
          up   (let [{:keys [exit
                             out]} (git "rev-parse" "--abbrev-ref" "--symbolic-full-name"
                                        "@{upstream}")]
                 (if (zero? exit) (str/trim out) "origin/main"))
          base (let [{:keys [exit out]} (git "merge-base" "HEAD" up)]
                 (if (zero? exit) (str/trim out) up))]
      (keep (fn [f]
              (let [rel (str (.relativize (.toPath (.getCanonicalFile (io/file (str root))))
                                          (.toPath (.getCanonicalFile (io/file f)))))
                    {:keys [exit out]} (git "show" (str base ":" rel))]
                (when (zero? exit) out)))
            files))))

(defn- live-rider
  [kind row]
  (case kind
    :automations (let [rd #(when (seq %) (try (clojure.edn/read-string %) (catch Exception _ %)))]
                   (cond-> {:name (:name row) :on (keyword (str (:on row)))
                            :effect (rd (:effect-spec row))}
                     (seq (:when-spec row)) (assoc :when (rd (:when-spec row)))
                     (:every-ms row)        (assoc :every-ms (:every-ms row))
                     (:paused row)          (assoc :paused true)))
    nil))

(defn- rider-diff
  [files]
  (let [texts (keep #(try (slurp %) (catch Exception _ nil)) files)
        now   (riders-of-files texts)
        prior (riders-of-files (or (upstream-texts files) []))
        lists {:automations   #(:automations (:body (api/get! :automations)))
               :schedules     #(:schedules (:body (api/get! :schedules)))
               :health-checks #(:checks (:body (api/get! :health-checks)))}]
    (vec
     (for [[k {:keys [noun]}] (sort-by key rider-kinds)
           :let [decl   (get now k)
                 before (get prior k)]
           :when (or (seq decl) (seq before))
           :let [live (into {}
                            (for [r (errors/optional (str "live " (name k)) (lists k))]
                              [(str (:name r)) r]))
                 norm #(cond-> (into {} (remove (comp nil? val)) %)
                         (= false (:paused %)) (dissoc :paused)
                         (:on %) (update :on (comp keyword name)))
                 ops  (concat
                       (for [[nm r] (sort-by key decl)]
                         (cond
                           (not (contains? live nm)) ["+" nm]
                           (and (= k :automations)
                                (not= (norm r)
                                      (norm (live-rider k (get live nm))))) ["~" nm]
                           :else nil))
                       (for [nm (sort (keys before)) :when (not (contains? decl nm))]
                         ["−" nm]))]
           [op nm] (remove nil? ops)]
       {:op op :noun noun :name nm}))))

(def ^:private rider-codes {"+" :green "~" :yellow "−" :red})

(defn- diff-preset! [ctx files]
  (let [repo   (push-repository (first files))
        whole  (if repo (diff-files files) files)
        schema (candidate-schema whole (live-schema))
        tally  (req! "diff" (api/post! :install (cond-> {:schema schema :preview? true}
                                                  repo (assoc :repository repo))))
        rows   (if (:refused? tally) (:planned tally) (:staged tally))
        groups (vec (for [[[verb cls] rows] (->> rows
                                                 (group-by (juxt row-verb row-class))
                                                 (sort-by key))]
                      {:verb (str verb) :class (str cls) :count (count rows)
                       :names (->> rows (map row-name) (remove #{"" "?"}) distinct sort vec)}))
        riders (errors/optional "rider diff" #(rider-diff whole))]
    (emit! ctx (cond-> (merge {:files    (vec files)
                               :refused? (boolean (:refused? tally))
                               :changes  groups
                               :riders   (vec riders)}
                              (select-keys tally [:created :updated :skipped :deleted :retiring
                                                  :orphaned :strict-repository :problems]))
                 (not= whole files)      (assoc :whole (vec whole))
                 (seq (:advisories tally))
                 (assoc :advisories (mapv #(str (:entity %) " — " (:message %))
                                          (:advisories tally))))
           ::diff)
    (when (:problems tally) :abort)))

(defmethod render ::diff [_ {:keys [files whole refused? changes riders created updated skipped
                                    deleted advisories problems]
                             :as d}]
  [(when whole
     (mark :dim "•" (i18n/t :cli/diff-whole-checkout {:datei (str/join " + " whole)})))
   (paint :bold (str "  ── " (i18n/t :cli/vs-live-would-change {:dateien (str/join " + " files)})
                     " ──"))
   (cond
     refused?       (mark :red "✗" (i18n/t :cli/diff-planned-refused
                                           {:n (reduce + 0 (map :count changes))}))
     (empty? changes) (str "    " (paint :dim (i18n/t :cli/no-changes-vs-live))))
   (for [{:keys [verb class count names]} changes
         :let [codes (verb-codes verb :gray)]]
     [(str "    " (paint codes (review-glyph verb "•")) " " (paint codes (str count))
           " " (paint :bold (str verb " " (plural count class))))
      (when (seq names) (grid names 8 codes))])
   (str "    "
        (paint :dim (str "Σ "
                         (paint :green (i18n/t :cli/n-create {:n created})) ", "
                         (paint :yellow (i18n/t :cli/n-update {:n updated})) ", "
                         (paint :dim (i18n/t :cli/n-unchanged {:n skipped}))
                         (when deleted
                           (str ", " (paint :red (i18n/t :cli/n-delete {:n deleted})))))))
   (retiring-lines d)
   (for [{:keys [op noun name]} riders]
     (str "    " (paint (rider-codes op) op) " " (paint :bold noun) " " name))
   (mark :dim "•" (i18n/t :cli/diff-only))
   (when (seq advisories) (advisory-lines advisories))
   (when problems (preflight-lines problems))])

(defmethod step! :checkout [ctx [_ {:keys [name]}]]
  (let [b     (clojure.core/name name)
        names (into #{} (map (comp str :name))
                    (:branches (req! "branches" (api/list-branches {}))))]
    (when-not (contains? names b)
      (throw (ex-info (str "no branch " (pr-str b)) {:branch b})))
    (emit! ctx {:checked-out b})
    {:kind :branch :branch b}))

(defmethod step! :lint [ctx [_ _]]
  (emit! ctx {:findings (:findings (req! "lint" (api/get! :schema-lint)))})
  nil)

(defmethod step! :ontology-lint [ctx [_ {:keys [rule]}]]
  (emit! ctx {:findings (cond->> (:findings (req! "lint" (api/get! :schema-lint)))
                          rule (filterv #(= (name rule) (name (:rule %)))))})
  nil)

(defmethod step! :check [ctx [_ _]]
  (emit! ctx (check-report))
  nil)
