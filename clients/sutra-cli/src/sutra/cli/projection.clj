(ns sutra.cli.projection
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.cli.git :as git]
            [sutra.cli.out :as out]
            [sutra.cli.text-location :as loc]
            [sutra.kit.lifecycle :as lc]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.checkout :as ck]
            [sutra.cli.view :as view]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx])
  (:import (java.io File)
           (java.security MessageDigest)))

(defn sha [^String s]
  (apply str
         (map #(format "%02x" %)
              (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))

(defn- body! [what r]
  (if (<= 200 (:status r) 299)
    (:body r)
    (throw (ex-info (str what ": " (or (get-in r [:body :error]) (str "HTTP " (:status r))))
                    {:status (:status r) :body (:body r)}))))

(defn- projections []
  (into {} (map (juxt :name identity))
        (:projections (body! "projections" (api/get! :projections)))))

(defn in-branch [opts f]
  (let [run #(binding [tx/*branch*  (:branch opts)
                       tx/*project* (or (:project opts) tx/*project*)]
               (f))]
    (if (seq tx/*auth-token*) (run) (git/with-auth opts run))))

(defn- canonical ^File [x] (.getCanonicalFile (io/file x)))

(defn- sidecar-name
  [nm {:keys [texts project]}]
  (str "." (cond (= 1 (count texts)) (last (str/split (first texts) #"/"))
                 project             project
                 :else               nm)
       ".sutra"))

(def ^:private sidecar-reads (atom {}))

(defn- read-sidecar [^File f]
  (when (.isFile f)
    (let [k [(.getCanonicalPath f) (.lastModified f) (.length f)]]
      (or (get @sidecar-reads k)
          (let [c (assoc (edn/read-string (slurp f)) ::file f)]
            (swap! sidecar-reads assoc k c)
            c)))))

(defn- sidecars-in [^File dir]
  (keep #(when (and (.isFile ^File %) (str/starts-with? (.getName ^File %) ".")
                    (str/ends-with? (.getName ^File %) ".sutra"))
           (read-sidecar %))
        (.listFiles dir)))

(defn- root-of ^File [checkout] (.getParentFile ^File (::file checkout)))

(defn- checkout-of
  [path]
  (let [f (canonical path)]
    (some (fn [^File dir]
            (some (fn [c] (when (some #(= f (canonical (io/file dir %))) (:texts c)) c))
                  (sidecars-in dir)))
          (take 6 (take-while some? (iterate #(.getParentFile ^File %) (.getParentFile f)))))))

(defn- changed?
  [c]
  (let [root (root-of c)]
    (boolean (some (fn [[p h]] (let [f (io/file root p)]
                                 (or (not (.isFile f)) (not= h (sha (slurp f))))))
                   (:hashes c)))))

(defn- pick-query [nm {:keys [texts project]} rehome?]
  (cond-> {:name nm}
    (seq texts) (assoc :texts (str/join "," texts))
    project     (assoc :project project)
    rehome?     (assoc :rehome true)))

(defn- settle
  ([opts c] (settle opts c nil))
  ([opts c pulled]
   (if-not (= :staged (some-> (:state c) keyword))
     c
     (in-branch (cond-> opts (and (:branch c) (nil? (:branch opts))) (assoc :branch (:branch c)))
                (fn []
                  (if (seq (:staged (body! "staging" (api/get! :staging))))
                    c
                    (let [fresh #(or pulled
                                     (:base (body! "projection"
                                                   (api/get! :projection-files
                                                             (pick-query (:projection c) (:pulled c)
                                                                         false)))))
                          event (if (or (changed? c)
                                        (let [b (or (:pushed c) (:base c))]
                                          (= (select-keys (update-keys (fresh) name) (keys b)) b)))
                                  :commit
                                  :discard)
                          c'    (-> c
                                    (assoc :state (lc/decide ck/lifecycle
                                                             (.getName ^File (::file c))
                                                             event :staged c nil))
                                    (cond-> (and (= :commit event) (:pushed c))
                                      (-> (update :base #(merge (select-keys % (:texts c))
                                                                (:pushed c)))
                                          (update :base-hashes merge (:pushed-hashes c))))
                                    (dissoc :pushed :pushed-hashes))]
                      (reset! sidecar-reads {})
                      (spit (::file c) (pr-str (dissoc c' ::file)))
                      c')))))))

(defn- state-of
  ([opts c] (state-of opts c nil))
  ([opts c pulled]
   (let [c (when c (settle opts c pulled))]
     [c (ck/state c (and c (changed? c)))])))

(defn checkout-state
  [opts path]
  (second (state-of opts (checkout-of path))))

(defn- refused! [^Exception e]
  (if (= :refusal.lifecycle/transition-refused (:refusal/code (ex-data e)))
    (let [{:keys [from event]} (ex-data e)]
      (out/warn! (str "  ✗ " (ex-message e)))
      (case from
        (:edited :conflicted)
        (out/warn! "    push it first (--mine/--theirs on a conflict), or pull --force")
        :orphaned (out/warn! "    its staged change was discarded: pull --force starts over")
        nil)
      (when (and (= :absent from) (#{:push :preview} event))
        (out/warn! "    pull it first: projection pull <projection> <text>"))
      1)
    (throw e)))

(defn list!
  [opts]
  (in-branch opts
             (fn []
               (doseq [[nm p] (sort-by key (projections))]
                 (println (str "  " nm (when-let [d (:description p)] (str "  —  " d)))))
               0)))

(defn- edited
  [^File root paths]
  (distinct
   (for [c (sidecars-in root)
         [p h] (:hashes c)
         :when (contains? (set paths) p)
         :let [f (io/file root p)]
         :when (and (.isFile f) (not= h (sha (slurp f))))]
     p)))

(defn- unpulled
  [^File root paths]
  (let [known (set (mapcat :texts (sidecars-in root)))]
    (filter #(and (.isFile (io/file root %)) (not (known %))) paths)))

(defn pull!
  [opts nm {:keys [texts project to force? rehome?] :as pick}]
  (let [root (canonical (or to "."))]
    (in-branch opts
               (fn []
                 (let [{:keys [files base shape unreadable around]}
                       (body! "projection"
                              (api/get! :projection-files (pick-query nm pick rehome?)))
                       files (update-keys files name)
                       base  (update-keys base name)
                       paths (vec (sort (keys files)))
                       stop  (when-not force?
                               (seq (concat (edited root paths) (unpulled root paths))))
                       side  (io/file root (sidecar-name nm pick))
                       prior (read-sidecar side)
                       from  (cond
                               (nil? prior) :absent
                               force?       (ck/state prior false)
                               :else        (second
                                             (state-of opts prior
                                                       (when (and (not rehome?)
                                                                  (= (pick-query nm pick false)
                                                                     (pick-query (:projection prior)
                                                                                 (:pulled prior)
                                                                                 false)))
                                                         base))))
                       to    (try (lc/decide ck/lifecycle (.getName side)
                                             (if force? :pull-force :pull) from nil
                                             {:stop stop :rehome? rehome?})
                                  (catch clojure.lang.ExceptionInfo e e))]
                   (cond
                     (instance? Exception to)
                     (refused! to)
                     (empty? paths)
                     (out/fail! (str "  ✗ " nm
                                     (when project (str " has no text of project " project))
                                     " — nothing to pull"))
                     :else
                     (do
                       (doseq [[p t] files]
                         (let [f (io/file root p)]
                           (io/make-parents f)
                           (spit f t)))
                       (reset! sidecar-reads {})
                       (spit side
                             (pr-str {:projection nm :branch (:branch opts) :state to
                                      :pulled (cond (seq texts) {:texts texts}
                                                    project {:project project}
                                                    :else {:all true})
                                      :texts paths :base base :shape shape :around around
                                      :hashes (update-vals files sha)
                                      :base-hashes (update-vals files sha)}))
                       (println (str "  ✓ " nm " → " root "  (" (count paths) " text(s)"
                                     (when (:branch opts) (str ", branch " (:branch opts)))
                                     (when rehome? ", every node at its home")
                                     ") · checkout " (.getName side)))
                       (doseq [[p msg] (sort-by key unreadable)]
                         (out/warn! (str "  ⚠ " (name p) " — the grammar refuses it as stored;"
                                       " fix it and push it:\n      "
                                       (str/replace (str msg) "\n" "\n      "))))
                       0)))))))

(defn reach-lines
  [{:keys [inputs-changed readers-unaffected] {:keys [direct further]} :downstream}]
  (concat
   (when (seq inputs-changed)
     (concat
      ["  ! inputs changed since this pull — read them before you rely on them:"]
      (for [{:keys [node where what]} (take 20 inputs-changed)]
        (str "    ≠ " node " (" what ", on " where ")"))
      (when (< 20 (count inputs-changed))
        [(str "    … and " (- (count inputs-changed) 20) " more")])))
   (when (seq direct)
     (cons
      (str "  → contract changed, feeds " (count direct) " node(s) directly"
           (when (pos? (or further 0)) (str ", " further " more one step on")) ":")
      (for [[kind ids] (sort-by key (group-by #(first (str/split % #"/" 2)) direct))
            :let [names (map #(second (str/split % #"/" 2)) ids)]]
        (str "    " kind ": " (str/join ", " (take 8 names))
             (when (< 8 (count names)) (str " … +" (- (count names) 8)))))))
   (when (pos? (or readers-unaffected 0))
     [(str "  · " readers-unaffected " reader(s) of what changed: no contract moved")])))

(defn- row-name
  [{:keys [type type-name args target-id]}]
  (let [n (or type-name (get args (keyword (str type "-name"))) (:name args) target-id "?")]
    (if (keyword? n) (name n) (str n))))

(defn change-lines
  [rows]
  (for [[[verb kind] of-kind] (sort-by key (group-by (juxt :verb :type) rows))
        :let [names (sort (map row-name of-kind))]]
    (str "    " verb " " kind ": " (str/join ", " (take 12 names))
         (when (< 12 (count names)) (str " … +" (- (count names) 12))))))

(declare text-report!)

(defn report!
  [opts branch answer]
  (when (out/json? opts)
    (println (out/result-json
              (select-keys answer [:created :updated :deleted :skipped :preview? :staged :moved
                                   :landed :land-refused :instance :not-stored :refused?
                                   :problems]))))
  (text-report! opts branch answer))

(defn- text-report!
  [opts branch {:keys [created updated skipped deleted preview? staged moved landed land-refused
                  instance not-stored refused? problems] :as answer}]
  (out/say! opts (str "  " (cond refused? "✗ preview — the push would be refused, nothing staged"
                           preview? "◦ preview — nothing staged"
                           landed   "✓ landed"
                           :else    "✓ staged")
                (when branch (str " on " branch)) ": "
                (or created 0) " created, " (or updated 0) " updated, "
                (or deleted 0) " deleted, " (or skipped 0) " unchanged"
                (when (seq moved) (str ", " (count moved) " moved"))))
  (run! #(out/say! opts %) (change-lines staged))
  (when refused?
    (doseq [{:keys [error name]} (:plan problems)]
      (out/say! opts (str "      · " (or error name)))))
  (doseq [[section n from to] (take 20 moved)]
    (out/say! opts (str "    ↦ " section " " n ": " from " → " to)))
  (when (< 20 (count moved)) (out/say! opts (str "    … and " (- (count moved) 20) " more")))
  (run! #(out/say! opts %) (reach-lines answer))
  (when land-refused
    (out/say! opts (str "  ! not landed: " land-refused)))
  (when (seq not-stored)
    (out/say! opts (str "  ✗ read back, " (count not-stored)
                  " node(s) do not hold what the push sent — the platform dropped it:"))
    (doseq [{:keys [text node elements]} (take 20 not-stored)]
      (out/say! opts (str "    ≠ " text " " (pr-str node)
                    (when (sequential? elements) (str " " (pr-str elements)))))))
  (when instance
    (let [{:keys [created updated deleted requested land-refused branch]} instance]
      (out/say! opts (str "  " (cond requested    "? instance change requested (an admin decides)"
                               land-refused "! instance change not landed"
                               preview?     "◦ instance (unbranched) would change"
                               :else        "✓ instance (unbranched) landed")
                    (when branch (str " on " branch)) ": "
                    (or created 0) " created, " (or updated 0) " updated, "
                    (or deleted 0) " deleted"))
      (when land-refused (out/say! opts (str "    " land-refused)))))
  (when-not (or preview? landed)
    (out/say! opts (str "    commit it to land it on the branch: sutra"
                  (when branch (str " --branch " branch)) " commit"))))

(defn- reason-lines
  [errs]
  (cond
    (nil? errs) nil
    (and (sequential? errs) (every? string? errs)) errs
    :else (try (@(requiring-resolve 'sutra.parser.ent-errors/humanize->lines) errs)
               (catch Exception _ [(pr-str errs)]))))

(defn- key-reading-hint []
  (try @(requiring-resolve 'sutra.parser.ent-errors/key-reading-hint) (catch Exception _ "")))

(defn- conflicts! [{:keys [error data]}]
  (out/warn! (str "  ✗ " (or error "conflict")))
  (doseq [{[section n] :node f :field} (:conflicts data)]
    (out/warn! (str "    ≠ " (name section) " " n
                  (when (and f (not= "whole" (if (keyword? f) (name f) (str f))))
                    (str " " (pr-str f))))))
  (out/warn! "    decide: push --mine (yours win) or push --theirs (keep what changed since your pull), or pull --force to start over"))

(defn- save! [c c']
  (reset! sidecar-reads {})
  (spit (::file c) (pr-str (dissoc c' ::file))))

(defn- empty-push?
  [body]
  (zero? (reduce + (map #(or (get body %) 0) [:created :updated :deleted]))))

(defn- pushed-state
  [{:keys [c id step event]} body]
  (let [s (step (if (and (= :push event) (empty-push? body)) :move event))]
    (if (and (:landed body) (= :staged s))
      (lc/decide ck/lifecycle id :commit s c nil)
      s)))

(defn- pushed! [opts {:keys [c files preview?] :as push} branch body]
  (report! opts branch body)
  (doseq [{:keys [entity message]} (:advisories body)]
    (out/say! opts (str "  ! " entity " — " message)))
  (when-not preview?
    (let [kept   (vec (sort (keep (fn [[p t]] (when t p)) files)))
          hashes (into {} (for [p kept] [p (sha (get files p))]))
          base   (update-keys (:base body) name)
          state  (pushed-state push body)
          staged (= :staged (some-> state keyword))]
      (save! c (cond-> (assoc c
                              :state  state
                              :texts  kept
                              :base   (if staged
                                        (:base c)
                                        (merge (select-keys (:base c) kept) base))
                              :hashes hashes)
                 staged       (assoc :pushed base :pushed-hashes hashes)
                 (not staged) (-> (assoc :base-hashes hashes)
                                  (dissoc :pushed :pushed-hashes))))))
  (if (or (seq (:not-stored body)) (:refused? body)) 1 0))

(defn- conflicted! [{:keys [c step preview?]} body]
  (when-not preview? (save! c (assoc c :state (step :push-conflict))))
  (conflicts! body)
  1)

(defn- failed-source
  [error]
  (second (re-find #"^Invalid [^(]*\(([^)]+)\)" (str error))))

(defn- failed! [{:keys [status body]} files]
  (let [error  (:error body)
        ls     (reason-lines (get-in body [:data :errors]))
        source (failed-source error)
        text   (get files source)
        head   (if (seq ls)
                 (str/replace (first (str/split-lines (str error)))
                              #"\s*\d+ problem\(s\).*$" "")
                 error)]
    (out/warn! (str "  ✗ " (or head (str "HTTP " status))))
    (doseq [p (mapcat loc/problems-of ls)]
      (out/warn! (str "      · " (loc/line-for (when (some? text) source) text p))))
    (when (some #(str/includes? (str %) "disallowed key") ls)
      (out/warn! (str "        " (key-reading-hint)))))
  (doseq [{:keys [message rule entity]} (get-in body [:data :problems])]
    (out/warn! (str "      · " (or message (str rule " " entity)))))
  1)

(defn- text-kinds
  [nm]
  (authoring/formats {} (:filename (get (projections) nm))))

(defn- glob-dir
  [glob]
  (let [lit (first (str/split glob #"\*" 2))]
    (subs lit 0 (inc (or (str/last-index-of lit "/") -1)))))

(defn- rel-path [^File root ^File f]
  (str/replace (str (.relativize (.toPath root) (.toPath f))) File/separator "/"))

(defn- files-under [^File dir deep?]
  (if deep?
    (->> (tree-seq #(and (.isDirectory ^File %) (not (str/starts-with? (.getName ^File %) ".")))
                   #(.listFiles ^File %) dir)
         (filter #(.isFile ^File %)))
    (filter #(.isFile ^File %) (.listFiles dir))))

(defn unpulled-texts
  [c kinds]
  (let [root  (root-of c)
        globs (mapcat :name (vals kinds))
        dirs  (merge (zipmap (map #(glob-dir (str %)) (:texts c)) (repeat false))
                     (into {} (for [g globs] [(glob-dir g) (str/includes? g "**")])))]
    (->> (for [[d deep?] dirs
               :let [dir (io/file root d)]
               :when (.isDirectory dir)
               f (files-under dir deep?)
               :when (not (str/starts-with? (.getName ^File f) "."))]
           (rel-path root (canonical f)))
         distinct
         (filter #(authoring/file? kinds %))
         (remove #(checkout-of (io/file root %)))
         sort)))

(defn- differing
  [c files]
  (let [bh   (:base-hashes c)
        diff (when bh
               (into (sorted-map)
                     (filter (fn [[p t]] (or (nil? t) (not= (get bh p) (sha t)))))
                     files))]
    (if (seq diff) diff files)))

(defn- push-request [{:keys [files c preview?]} {:keys [resolve land?]}]
  (cond-> {:files (differing c files) :base (:base c) :shape (:shape c)
           :around (:around c) :preview? (boolean preview?)}
    resolve       (assoc :resolve resolve)
    (some? land?) (assoc :land? land?)))

(defn- held-texts
  [nm paths]
  (if (empty? paths)
    #{}
    (let [{:keys [base]} (body! "projection"
                                (api/get! :projection-files
                                          (pick-query nm {:texts (vec paths)} false)))]
      (set (keep (fn [[p b]] (when (seq b) (name p))) base)))))

(defn- unpulled-refusal!
  [nm files left deleted]
  (let [dropped (keep (fn [[p t]] (when-not t p)) files)]
    (out/warn! (str "  ✗ this push deletes " deleted " node(s) while " (str/join ", " left)
                  " sits in the checkout, written by no pull — it would not be sent"))
    (when (seq dropped)
      (out/warn! (str "    looks like a rename: " (str/join ", " dropped) " → "
                    (str/join ", " left))))
    (out/warn! "    carry it in this push: projection push <text> --adopt"
             "(what it holds moves, what is new is created)")
    (out/warn! (str "    or pull it by name: projection pull " nm " <old> <new>;"
                  " to really delete, move it out of the folder"))
    1))

(defn- push-checkout!
  [opts paths c from named {:keys [preview? resolve adopt?] :as flags}]
  (let [{nm :projection branch :branch :keys [texts]} c
        id    (.getName ^File (::file c))
        event (cond preview? :preview (= :conflicted from) :resolve :else :push)
        step  (fn [event] (lc/decide ck/lifecycle id event from c {:resolve resolve}))
        root  (root-of c)
        opts  (cond-> opts (and branch (nil? (:branch opts))) (assoc :branch branch))
        files (into (sorted-map) (for [p texts :let [f (io/file root p)]]
                                   [p (when (.isFile f) (slurp f))]))
        push  {:c c :id id :event event :step step :files files :preview? preview?}]
    (if-let [e (try (step event) nil (catch clojure.lang.ExceptionInfo e e))]
      (refused! e)
      (in-branch
       opts
       (fn []
         (let [strays  (set (unpulled-texts c (text-kinds nm)))
               unknown (seq (remove strays named))
               held    (held-texts nm strays)
               owned   (seq (filter held named))
               taken   (set (remove held (if adopt? strays named)))
               left    (sort (remove taken strays))
               files   (into files (for [p taken] [p (slurp (io/file root p))]))
               push    (assoc push :files files)
               send    #(api/post! :projection-files {:name nm}
                                   (push-request (assoc push :preview? %) flags))
               dry     (when (and (seq left) (not preview?)) (send true))
               deleted (fn [r] (or (get-in r [:body :deleted]) 0))]
           (cond
             unknown
             (out/fail! (str "  ✗ " (str/join ", " unknown) " is no text projection " nm
                             " reads"))
             owned
             (out/fail! (str "  ✗ " (str/join ", " owned) " holds nodes already —"
                             " pull it to change it: projection pull " nm " <text>"))
             (and dry (<= 200 (:status dry) 299) (pos? (deleted dry)))
             (unpulled-refusal! nm files left (deleted dry))
             :else
             (do
               (when (< (count (distinct paths)) (count texts))
                 (out/say! opts (str "  ◦ pushing checkout " id ": "
                               (count (differing c files)) " of its " (count texts)
                               " texts changed")))
               (doseq [p (sort taken)]
                 (out/say! opts (str "  ◦ carrying " p " — written by no pull")))
               (let [r (send preview?)]
                 (when (seq left)
                   (out/say! opts (str "  ◦ not sent: " (str/join ", " left)
                                 " — written by no pull; --adopt carries it")))
                 (cond
                   (and preview? (<= 200 (:status r) 299) (pos? (deleted r)))
                   (let [code (pushed! opts push (:branch opts) (:body r))]
                     (when (seq left) (unpulled-refusal! nm files left (deleted r)))
                     code)
                   (<= 200 (:status r) 299) (pushed! opts push (:branch opts) (:body r))
                   (= 409 (:status r))      (conflicted! push (:body r))
                   :else                    (failed! r files)))))))))))

(defn- enclosing-checkouts
  [path]
  (some (fn [^File dir] (seq (sidecars-in dir)))
        (take 6 (take-while some? (iterate #(.getParentFile ^File %)
                                           (.getParentFile (canonical path)))))))

(defn push!
  [opts paths flags]
  (let [found  (for [p paths]
                 (if-let [c (checkout-of p)]
                   [p c nil]
                   (let [cs (enclosing-checkouts p)]
                     (if (= 1 (count cs))
                       [p (first cs) (rel-path (root-of (first cs)) (canonical p))]
                       [p nil]))))
        orphan (seq (keep (fn [[p c]] (when-not c p)) found))
        cs     (distinct (keep second found))]
    (cond
      (empty? paths)
      (out/fail! "  ✗ name a text to push: projection push <text> …")
      orphan
      (out/fail! (str "  ✗ " (str/join ", " orphan)
                      " was written by no pull and lies in no one checkout's folder;"
                      " pull it first: projection pull <projection> <text>"))
      (< 1 (count cs))
      (out/fail! (str "  ✗ these texts belong to " (count cs) " checkouts ("
                      (str/join ", " (map #(.getName ^File (::file %)) cs))
                      ") — push each on its own"))
      :else
      (let [[c from] (state-of opts (first cs))]
        (push-checkout! opts paths c from (vec (keep #(get % 2) found)) flags)))))

(defn holders
  [base names]
  (let [want (set names)]
    (reduce-kv (fn [acc text nodes]
                 (reduce (fn [acc [section nm]]
                           (let [n (if (keyword? nm) (name nm) (str nm))]
                             (cond-> acc
                               (want n) (update n (fnil conj []) [(name text) (name section)]))))
                         acc (keys nodes)))
               {} base)))

(defn installed-in
  [schema names]
  (let [want (set names)]
    (reduce-kv (fn [acc section m]
                 (if (map? m)
                   (reduce (fn [acc n]
                             (cond-> acc (want n) (update n (fnil conj []) (name section))))
                           acc (view/entity-names m))
                   acc))
               {} schema)))

(defn where!
  [opts names {:keys [projection]}]
  (in-branch opts
             (fn []
               (let [nms    (if projection [projection] (sort (keys (projections))))
                     schema (body! "schema" (api/get! :schema))
                     views  (into {} (map (juxt identity #(view/object-views schema %))) names)
                     held   (apply merge-with into
                                   (for [nm nms
                                         :let [{:keys [base]}
                                               (body! (str "projection " nm)
                                                      (api/get! :projection-files {:name nm}))]]
                                     (update-vals (holders base (into (vec names)
                                                                      (mapcat val) views))
                                                  (fn [ps] (mapv #(into [nm] %) ps)))))
                     inst   (installed-in schema (remove held names))]
                 (doseq [n names]
                   (doseq [v (get views n)
                           [p t] (get held v)]
                     (println (str "  " n "  →  " p " · " t "  (view " v ")")))
                   (if-let [ps (seq (get held n))]
                     (doseq [[p t s] ps]
                       (println (str "  " n "  →  " p " · " t "  (" s ")")))
                     (println (str "  " n "  →  "
                                   (if-let [ss (seq (get inst n))]
                                     (str "in NO text — installed as " (str/join ", " ss)
                                          "; declare it in a text to change it through the CLI")
                                     "not installed, in no text")))))
                 0))))

(defn- leading-key
  [line]
  (when-let [[_ pre k] (re-find #"^(\s*[\[{(]*)(:[^\s,{}\[\]()\"]+)" line)]
    [(count pre) k]))

(defn- enclosing-keys
  [lines i]
  (let [line (nth lines i)
        own  (leading-key line)
        col  (or (first own) (inc (count (re-find #"^\s*" line))))]
    (loop [j (dec i) col col acc (if own (list (second own)) ())]
      (if (or (neg? j) (zero? col))
        (vec acc)
        (let [[c k] (leading-key (nth lines j))]
          (if (and c (< c col))
            (recur (dec j) c (conj acc k))
            (recur (dec j) col acc)))))))

(defn- snippet
  [^String line at]
  (let [from (max 0 (- at 40))
        to   (min (count line) (+ at 80))]
    (str (when (pos? from) "…") (str/trim (subs line from to))
         (when (< to (count line)) "…"))))

(defn text-hits
  [text needle nodes]
  (let [lines (str/split-lines text)
        low   (str/lower-case needle)
        known (set nodes)]
    (vec (for [[i line] (map-indexed vector lines)
               :let [at (str/index-of (str/lower-case line) low)]
               :when at
               :let [path (enclosing-keys lines i)]]
           {:line    (inc i)
            :node    (or (some #(when (known (subs % 1)) (subs % 1)) path)
                         (first path))
            :snippet (snippet line at)}))))

(def ^:private find-limit 40)

(defn find!
  [opts needle {:keys [projection]}]
  (in-branch opts
             (fn []
               (let [hits (vec
                           (for [nm (if projection [projection] (sort (keys (projections))))
                                 :let [{:keys [files base]}
                                       (body! (str "projection " nm)
                                              (api/get! :projection-files {:name nm}))]
                                 [path text] (sort-by (comp name key) files)
                                 :let [nodes (map (fn [[_ n]] (if (keyword? n) (name n) (str n)))
                                                  (keys (get base path)))]
                                 hit (text-hits text needle nodes)]
                             (assoc hit :projection nm :text (name path))))]
                 (doseq [{:keys [projection text line node snippet]} (take find-limit hits)]
                   (println (str "  " projection " · " text ":" line
                                 (when node (str "  " node)) "\n      " snippet)))
                 (when (< find-limit (count hits))
                   (println (str "  … and " (- (count hits) find-limit) " more")))
                 (if (seq hits)
                   0
                   (out/fail! (str "  ✗ no text holds " (pr-str needle))))))))
