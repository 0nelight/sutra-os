(ns sutra.repos.riders
  (:require [sutra.repos.ports :as ports]
            [sutra.repos.records :as git-rows]
            [sutra.store.access.scope :as scope]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.repos.edn-patch :as ep]
            [sutra.repos.projection :as gp]
            [sutra.repos.checks :as repo-checks]
            [sutra.repos.objects :as g]
            [sutra.types.opid :as opid]))

(set! *warn-on-reflection* true)

(def kinds [:health-checks :schedules :automations])

(def ^:private id-kind {:health-checks :health-check :schedules :schedule :automations :automation})

(defn- keeper [store k]
  (or (get (ports/riders store) k)
      (throw (ex-info (str "no module keeps " (name k) " riders") {:kind k}))))

(defn- upsert! [store k branch r] ((:upsert! (keeper store k)) store branch r))

(defn- delete! [store k branch nm]
  ((:delete! (keeper store k)) store branch (opid/name-id (id-kind k) nm)))

(defn declared
  [ent]
  (into {} (for [k kinds]
             [k (into {} (for [r (get ent k) :when (:name r)] [(str (:name r)) r]))])))

(defn apply!
  [store branch ent prior]
  (let [now    (declared ent)
        before (declared prior)
        n      (into {} (for [k kinds :let [rows (get ent k)] :when (seq rows)]
                          (do (doseq [r rows] (upsert! store k branch r))
                              [k (count rows)])))
        gone   (into {} (for [k kinds
                              :let [names (remove (set (keys (get now k))) (keys (get before k)))]
                              :when (seq names)]
                          (do (doseq [nm names] (delete! store k branch nm))
                              [k (vec (sort names))])))]
    (not-empty (cond-> n (seq gone) (assoc :removed gone)))))

(defn three-way
  ([base src tgt] (three-way base src tgt nil))
  ([base src tgt resolve]
   (reduce (fn [acc k]
             (let [b (get base k {}) s (get src k {}) t (get tgt k {})]
               (reduce (fn [acc nm]
                         (let [bv (get b nm) sv (get s nm) tv (get t nm)
                               conflict? (and (not= sv bv) (not= tv bv) (not= sv tv))
                               choice (get resolve [k nm])]
                           (cond
                             (not conflict?)   (assoc-in acc [:merged k nm] (if (= sv bv) tv sv))
                             (= :source choice) (assoc-in acc [:merged k nm] sv)
                             (= :target choice) (assoc-in acc [:merged k nm] tv)
                             (and (map? choice) (contains? choice :value))
                             (assoc-in acc [:merged k nm] (:value choice))
                             :else (-> (assoc-in acc [:merged k nm] tv)
                                       (update :conflicts (fnil conj []) [k nm])))))
                       acc (distinct (concat (keys b) (keys s) (keys t))))))
           {:merged {}} kinds)))

(defn- ordered
  [merged tgt-rows src-rows]
  (let [names (distinct (concat (map (comp str :name) tgt-rows) (map (comp str :name) src-rows)))]
    (vec (keep #(get merged %) names))))

(defn- holder
  [texts k]
  (some (fn [[path text]]
          (when (contains? (try (edn/read-string text) (catch RuntimeException _ {})) k) path))
        (sort-by key texts)))

(defn- rewrite
  [texts src-texts kinds->rows]
  (reduce (fn [acc [k rows]]
            (let [path (or (holder acc k) (holder src-texts k)
                           (when (contains? acc "ontology.ent") "ontology.ent")
                           (first (sort (keys acc))))
                  text (get acc path)]
              (if-not text
                acc
                (let [form   (edn/read-string text)
                      target (if (seq rows) (assoc form k rows) (dissoc form k))
                      same?  #(= (select-keys (edn/read-string %) [k]) (select-keys target [k]))
                      out    (or (if (contains? form k)
                                   (ep/patch text target [k] same?)
                                   (when (seq rows)
                                     (let [t (str/trimr text)]
                                       (when (str/ends-with? t "}")
                                         (str (subs t 0 (dec (count t)))
                                              "\n\n " (pr-str k) "\n " (pr-str rows) "}\n")))))
                                 (pr-str target))]
                  (assoc acc path out)))))
          texts kinds->rows))

(defn merge!
  ([store blobs repo-rid source target] (merge! store blobs repo-rid source target nil))
  ([store blobs repo-rid source target {:keys [resolve]}]
   (let [src-tip (:tip-sha (git-rows/repo-anchor store repo-rid source))
         tgt-tip (:tip-sha (git-rows/repo-anchor store repo-rid target))]
     (when (and src-tip tgt-tip (not= src-tip tgt-tip))
       (let [base    (g/merge-base (scope/repos) repo-rid src-tip tgt-tip)
             [base-ent src-ent
              tgt-ent] (map #(repo-checks/commit-ent store (scope/repos) repo-rid %)
                            [base src-tip tgt-tip])
             [b s t] (map declared [base-ent src-ent tgt-ent])
             {:keys [merged conflicts]} (three-way b s t resolve)
             changed (into {}
                           (for [k kinds
                                 :let [ch (into {} (filter (fn [[nm v]] (not= v (get-in t [k nm]))))
                                                (get merged k))]
                                 :when (seq ch)]
                             [k ch]))]
         (cond
           (seq conflicts)
           {:error     :conflict
            :conflicts conflicts
            :values    (into {} (for [[k nm :as c] conflicts]
                                  [c {:base   (get-in b [k nm]) :source (get-in s [k nm])
                                      :target (get-in t [k nm])}]))}

           (seq changed)
           (let [branch target]
             (doseq [[k ch] changed [nm v] ch]
               (if (some? v) (upsert! store k branch v) (delete! store k branch nm)))
             (let [rows  (into {} (for [k (keys changed)]
                                    [k (ordered (get merged k) (get tgt-ent k) (get src-ent k))]))
                   texts (rewrite (gp/root-texts store (scope/repos) repo-rid tgt-tip)
                                  (gp/root-texts store (scope/repos) repo-rid src-tip) rows)
                   sha   (gp/commit-files! store (scope/repos) blobs repo-rid target texts
                                           (str "riders merged from " source))]
               {:changed (into {} (for [[k ch] changed] [k (vec (sort (keys ch)))]))
                :sha     sha}))))))))
