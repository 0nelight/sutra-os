(ns sutra.store.core.replay
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.store.core.commit :refer [apply!]]
            [sutra.store.core.engine :as engine :refer [begin lock! rollback!]]
            [sutra.store.core.kv :refer [kv-fx kv-get kv-pages kv-parts kv-read kv-scan rev-key]]
            [sutra.store.core.reads :refer [fetch forget-heads! kept-since state-at world-at]]
            [sutra.store.core.resources :as res])
  (:import (java.security MessageDigest)
           (java.nio.charset StandardCharsets)
           (java.util HexFormat)))

(set! *warn-on-reflection* true)

(defn- commit!
  [db h]
  (let [r (engine/commit! db h)] (forget-heads! db) r))

(defn- par-map
  [n f xs]
  (letfn [(go [in-flight xs]
            (lazy-seq
             (when-let [[fut & more] (seq in-flight)]
               (let [more (vec more)]
                 (cons @fut
                       (if-let [[x] (seq xs)]
                         (go (conj more (future (f x))) (rest xs))
                         (go more nil)))))))]
    (let [xs   (seq xs)
          head (doall (take n xs))]
      (go (mapv (fn [x] (future (f x))) head) (drop n xs)))))

(defn- digest
  ^String [v]
  (let [md (MessageDigest/getInstance "MD5")]
    (.formatHex (HexFormat/of) (.digest md (.getBytes (pr-str v) StandardCharsets/UTF_8)))))

(defn- parsed
  [upgrade [r v]]
  (if (map? v)
    {:rev r :fork v}
    (let [es (res/entries (upgrade (edn/read-string v)))]
      {:rev r :es es :dg (persistent! (reduce-kv (fn [m id e] (assoc! m id (digest e)))
                                                 (transient {}) es))})))

(defn- versions
  [acc known vs]
  (reduce (fn [acc [id r v d]]
            (let [k [id r]
                  had (or (get (:vdg acc) k) (known k))]
              (cond
                (nil? had) (-> acc
                               (update :fx conj (kv-fx (res/node-k id r) [v]))
                               (update :vdg assoc k d)
                               (update :versions inc))
                (= had d)  acc
                :else      (throw (ex-info "a replayed version would overwrite another"
                                           {::collision k})))))
          acc vs))

(defn- step
  [{:keys [b state dg n] :as acc} {:keys [rev es fork] edg :dg} parent-at known]
  (if fork
    (let [{:keys [state dg es]} (parent-at fork)
          r                      (:rev fork)]
      (-> acc
          (assoc :state state :dg dg :n 0)
          (update :first #(or % rev))
          (versions known (for [[id v] es] [id r v (get dg id)]))
          (update :fx into (cond-> [(kv-fx (res/log-k b rev) {:fork fork})]
                             es (conj (kv-fx (res/state-k b rev) state))))
          (update :states assoc rev state)))
    (let [began? (nil? (:first acc))
          ids    (into [] (filter #(not= (get dg %) (get edg %)))
                       (into (set (keys dg)) (keys edg)))
          chs    (into {} (for [id ids] [id [(get state id) (when (contains? es id) rev)]]))
          state' (reduce-kv (fn [s id [_ to]] (if to (assoc s id to) (dissoc s id))) state chs)
          due?   (or began? (>= (inc n) res/checkpoint-every))]
      (if (and (empty? chs) (not began?))
        acc
        (-> acc
            (assoc :state state' :dg edg :n (if due? 0 (inc n)))
            (update :first #(or % rev))
            (versions known (for [[id [_ to]] chs :when to] [id rev (get es id) (get edg id)]))
            (update :fx into
                    (concat (for [[id ch] chs] (kv-fx (res/hist-k b id rev) ch))
                            [(kv-fx (res/log-k b rev)
                                    (cond-> {:changes chs} began? (assoc :converted true)))]
                            (when due? [(kv-fx (res/state-k b rev) state')])))
            (update :states assoc rev state'))))))

(defn- reconcile
  [{:keys [b state dg]} c dg-c]
  (let [ids (into (set (keys dg)) (keys dg-c))
        chs (into {} (for [id ids :when (not= (get dg id) (get dg-c id))]
                       [id [(get state id) (when (contains? dg-c id) c)]]))]
    (concat [(kv-fx (res/log-k b c) {:changes chs})]
            (for [id ids :when (and (contains? dg-c id) (not (contains? chs id)))]
              (kv-fx (res/hist-k b id c) nil))
            (for [[id ch] chs] (kv-fx (res/hist-k b id c) ch)))))

(defn- with-read [db f]
  (let [h (begin db :read nil)]
    (try (f h) (finally (rollback! db h)))))

(defn- snapshots
  [db h b]
  (into [] (keep (fn [[k]] (when-let [r (parse-long k)] [r k])))
        (kv-scan db h [:schema b] {:keys-only? true})))

(defn- events
  [{:keys [upgrade] :as db} h b snaps n]
  (par-map n (partial parsed (or upgrade identity))
           (mapcat (fn [window]
                     (let [got (kv-read db h (map (fn [[_ k]] [:schema b k]) window))]
                       (for [[r k] window] [r (get got [:schema b k])])))
                   (partition-all n snaps))))

(def ^:private ^:dynamic *seen*
  nil)

(defn- digests-of
  [db h state]
  (let [seen  (or *seen* (atom {}))
        known @seen
        miss  (into [] (remove #(contains? known %)) state)
        fresh (into {} (map (fn [[k v]] [k (digest v)])) (when (seq miss) (fetch db h miss)))]
    (swap! seen into fresh)
    (into {} (map (fn [[id _ :as k]] [id (or (get fresh k) (get known k))])) state)))

(defn- digests-at
  [db h vdg state]
  (let [have (into {} (keep (fn [[id r]] (when-let [d (get vdg [id r])] [id d]))) state)
        rest (into {} (remove (comp have key)) state)]
    (cond-> have (seq rest) (into (digests-of db h rest)))))

(defn- parent-reader
  [db h replayed vdg]
  (fn [{f :from r :rev}]
    (if-let [{:keys [states]} (get replayed f)]
      (let [[_ st] (first (rsubseq states <= r))
            st     (or st {})]
        {:state st :dg (digests-at db h vdg st)})
      (if-let [st (when-let [{:keys [since]} (kept-since db h f)]
                    (when (<= since r) (state-at db h f r)))]
        {:state st :dg (digests-of db h st)}
        (let [es (res/entries (world-at db h f r))]
          {:state (update-vals es (constantly r))
           :dg    (update-vals es digest)
           :es    es})))))

(defn- check
  [db b {:keys [states vdg]}]
  (with-read db
    (fn [h]
      (let [mine (filterv (fn [[[_ r]]] (contains? states r)) vdg)
            vs   (fetch db h (mapv first mine))]
        (into (into [] (for [[k d] mine :when (not= d (digest (get vs k)))]
                         {:problem :version :resource (first k) :rev (second k)}))
              (for [[r st] states :when (not= st (state-at db h b r))]
                {:problem :state :rev r}))))))

(defn verify!
  [db b {:keys [threads] :or {threads 4}}]
  (with-read db
    (fn [h]
      (let [since (:since (kept-since db h b))
            snaps (filterv (fn [[r]] (and since (>= r since))) (snapshots db h b))
            bad   (into []
                        (keep (fn [{:keys [rev dg]}]
                                (when dg
                                  (let [got (res/entries (world-at db h b rev))
                                        ids (into (set (keys dg)) (keys got))
                                        off (into [] (filter #(not= (get dg %)
                                                                    (some-> (get got %) digest)))
                                                  ids)]
                                    (when (seq off) {:rev rev :resources (take 10 off)})))))
                        (events db h b snaps threads))]
        {:branch b :checked (count snaps) :problems bad}))))

(def ^:private root "main")

(defn- known-in
  [db h vdg]
  (fn [[id r :as k]]
    (or (get vdg k)
        (get @*seen* k)
        (when-let [[v] (get (kv-read db h [(res/node-k id r)]) (res/node-k id r))]
          (let [d (digest v)] (swap! *seen* assoc k d) d)))))

(defn- lead
  [db h b snaps]
  (when-let [[r k] (first snaps)]
    (let [v (kv-get db h [:schema b k])]
      (cond
        (map? v)   {:marked v}
        (= root b) nil
        :else
        (let [{f :branch br :rev} (:base (kv-get db h [:branch "" b]))]
          (if (and f (not= f b) br)
            {:fork {:rev (dec r) :fork {:from f :rev (min br r)}}}
            {:guess r}))))))

(defn- best-parent
  [db h replayed vdg b {:keys [rev dg]}]
  (let [digests (fn [p]
                  (if-let [[_ st] (some-> (get-in replayed [p :states]) (rsubseq <= rev) first)]
                    (digests-at db h vdg st)
                    (when (= root p)
                      (when-let [{:keys [since]} (kept-since db h root)]
                        (when (<= since rev)
                          (some->> (state-at db h root rev) (digests-of db h)))))))
        diff    (fn [pd] (count (filter #(not= (get dg %) (get pd %))
                                        (into (set (keys dg)) (keys pd)))))]
    (->> (cons root (sort (remove #{root b} (keys replayed))))
         (keep (fn [p] (some->> (digests p) diff (vector p))))
         (reduce (fn [[_ n :as best] [_ m :as c]] (if (or (nil? best) (< m n)) c best)) nil)
         first)))

(defn- in-order
  [parents bs]
  (let [in? (set bs)]
    (loop [done [] seen #{} todo (vec bs)]
      (if (empty? todo)
        done
        (let [ready (filter #(let [p (parents %)] (or (nil? p) (not (in? p)) (seen p))) todo)
              ready (if (seq ready) ready [(first todo)])]
          (recur (into done ready) (into seen ready) (vec (remove (set ready) todo))))))))

(defn- same-head?
  [db h b snaps]
  (if-let [t (some (fn [[_ k]] (let [v (kv-get db h [:schema b k])] (when (string? v) v)))
                   (rseq snaps))]
    (= t (kv-get db h [:schema b "authored"]))
    true))

(defn- replay-branch
  [db b {:keys [dry-run? threads]} replayed vdg]
  (let [t0 (System/nanoTime)
        {:keys [kept snaps acc head?]}
        (with-read db
          (fn [h]
            (let [kept  (kept-since db h b)
                  snaps (cond->> (snapshots db h b)
                          kept (filterv (fn [[r]] (< r (:since kept)))))
                  pr    (parent-reader db h replayed vdg)]
              {:kept  kept
               :snaps snaps
               :head? (or kept (empty? snaps) (same-head? db h b snaps))
               :acc   (try
                        (reduce (fn [acc e] (step acc e pr (known-in db h vdg)))
                                {:b b :state {} :dg {} :fx [] :n 0 :states (sorted-map)
                                 :vdg {} :versions 0}
                                (let [l  (lead db h b snaps)
                                      es (events db h b snaps threads)
                                      p  (when (:guess l)
                                           (best-parent db h replayed vdg b (first es)))]
                                  (cond
                                    (:fork l) (cons (:fork l) es)
                                    p         (cons {:rev (dec (:guess l))
                                                     :fork {:from p :rev (:guess l)}}
                                                    es)
                                    :else     es)))
                        (catch clojure.lang.ExceptionInfo e
                          (if-let [k (::collision (ex-data e))]
                            {:collision k :versions 0 :fx []}
                            (throw e))))})))
        ms    #(long (/ (- (System/nanoTime) t0) 1e6))
        base  {:branch b :snapshots (count snaps) :versions (:versions acc)
               :writes (count (:fx acc))}]
    (cond
      (empty? snaps) (assoc base :status :nothing :ms (ms))
      (:collision acc) (assoc base :status :collision :resource (:collision acc) :ms (ms))
      (not head?)    (assoc base :status :head-differs :ms (ms))
      dry-run?       (assoc base :status :dry-run :ms (ms) :acc acc)
      :else
      (let [h (begin db :write nil)
            r (try
                (lock! db h [[:branch b]])
                (if (not= kept (kept-since db h b))
                  (do (rollback! db h) ::moved)
                  (let [c   (:since kept)
                        fmt (if kept
                              (assoc kept :since (:first acc))
                              {:since (:first acc) :n (:n acc)})
                        fx  (concat (:fx acc)
                                    (when kept
                                      (reconcile acc c (digests-of db h (state-at db h b c))))
                                    (when-not kept
                                      (for [[id rv] (:state acc)] (kv-fx (res/head-k b id) rv)))
                                    [(kv-fx (res/format-k b) fmt)])]
                    (apply! db h (vec fx))
                    (commit! db h)
                    :written))
                (catch Throwable t (rollback! db h) (throw t)))]
        (if (= ::moved r)
          (assoc base :status :moved :ms (ms))
          (let [problems (vec (check db b acc))]
            (cond-> (assoc base :status (if (seq problems) :mismatch :replayed) :ms (ms)
                           :acc acc)
              (seq problems) (assoc :problems (take 20 problems)))))))))

(declare replay*)

(defn replay!
  [db {:keys [dry-run? branches threads on-branch] :or {dry-run? true threads 4}}]
  (binding [*seen* (atom {})]
    (replay* db {:dry-run? dry-run? :branches branches :threads threads :on-branch on-branch})))

(defn- replay*
  [db {:keys [dry-run? branches threads on-branch]}]
  (let [t0      (System/nanoTime)
        bs      (with-read db
                  (fn [h]
                    (let [bs  (cond->> (kv-parts db h :schema) branches (filter (set branches)))
                          ls  (into {} (for [b bs] [b (lead db h b (snapshots db h b))]))
                          par (update-vals ls #(or (get-in % [:marked :from])
                                                   (get-in % [:fork :fork :from])))
                          gs  (filter (comp :guess ls) bs)]
                      (concat (in-order par (remove (comp :guess ls) bs))
                              (sort-by (comp :guess ls) gs)))))
        out     (reduce (fn [{:keys [replayed vdg] :as o} b]
                          (let [r (replay-branch db b {:dry-run? dry-run? :threads threads}
                                                 replayed vdg)]
                            (when on-branch (on-branch (dissoc r :acc)))
                            (cond-> (update o :report conj (dissoc r :acc))
                              (:acc r) (-> (assoc-in [:replayed b]
                                                     (select-keys (:acc r) [:states]))
                                           (update :vdg into (:vdg (:acc r)))))))
                        {:report [] :replayed {} :vdg {}} bs)]
    {:ok?      (not-any? #(#{:mismatch :collision} (:status %)) (:report out))
     :branches (:report out)
     :ms       (long (/ (- (System/nanoTime) t0) 1e6))}))

(defn verify-all!
  [db {:keys [on-branch] :as opts}]
  (let [t0   (System/nanoTime)
        rows (mapv (fn [b] (let [r (verify! db b opts)] (when on-branch (on-branch r)) r))
                   (with-read db (fn [h] (vec (kv-parts db h :schema)))))]
    {:ok?      (not-any? (comp seq :problems) rows)
     :branches rows
     :ms       (long (/ (- (System/nanoTime) t0) 1e6))}))

(defn- rev-of-hist [k] (res/read-rev (subs k (inc (str/last-index-of k "|")))))

(defn- reset-branch!
  [db b]
  (let [h (begin db :write nil)]
    (try
      (lock! db h [[:branch b]])
      (let [last-text (some-> (peek (snapshots db h b)) first)]
        (cond
          (= root b)       (do (rollback! db h) :root)
          (nil? last-text) (do (rollback! db h) :no-texts)
          :else
          (let [ks      (fn [space] (kv-scan db h [space b] {:keys-only? true}))
                mine?   #(<= % last-text)
                logs    (ks :wlog)
                own     (some (fn [[k]] (let [r (res/read-rev k)] (when-not (mine? r) r))) logs)
                fx      (concat
                         (for [[k] logs :when (mine? (res/read-rev k))] (kv-fx [:wlog b k] nil))
                         (for [[k] (ks :wstate) :when (mine? (res/read-rev k))]
                           (kv-fx [:wstate b k] nil))
                         (for [[k] (ks :whist) :when (mine? (rev-of-hist k))]
                           (kv-fx [:whist b k] nil))
                         (if own
                           [(kv-fx (res/format-k b)
                                   (assoc (kv-get db h (res/format-k b)) :since own))]
                           (cons (kv-fx (res/format-k b) nil)
                                 (for [[k] (ks :whead)] (kv-fx [:whead b k] nil)))))]
            (apply! db h (vec fx))
            (commit! db h)
            :reset)))
      (catch Throwable t (rollback! db h) (throw t)))))

(defn- compact-states!
  [db]
  (count
   (filter
    true?
    (for [b (with-read db (fn [h] (vec (kv-parts db h :wstate))))]
      (let [h (begin db :write nil)]
        (try
          (lock! db h [[:branch b]])
          (let [[k e]  (first (kv-scan db h [:wlog b] {:limit 1}))
                {f :from r :rev} (:fork e)
                own    (when f (kv-get db h [:wstate b k]))
                shared (when own (state-at db h f r))]
            (if (and own (= own shared))
              (do (apply! db h [(kv-fx [:wstate b k] nil)]) (commit! db h) true)
              (do (rollback! db h) false)))
          (catch Throwable t (rollback! db h) (throw t))))))))

(defn drop-texts!
  [db opts]
  (reduce
   (fn [acc b]
     (let [{:keys [checked problems]} (verify! db b opts)
           snaps (with-read db (fn [h] (snapshots db h b)))]
       (if (or (seq problems) (empty? snaps) (not= checked (count snaps)))
         (update acc :kept conj {:branch b :why (if (seq problems) :problems :not-covered)})
         (let [h (begin db :write nil)]
           (try
             (lock! db h [[:branch b]])
             (let [ks (kv-scan db h [:schema b] {:keys-only? true})]
               (apply! db h (mapv (fn [[k]] (kv-fx [:schema b k] nil)) ks))
               (commit! db h)
               (-> acc (update :dropped conj b) (update :rows + (count ks))))
             (catch Throwable t (rollback! db h) (throw t)))))))
   {:dropped [] :kept [] :rows 0}
   (with-read db (fn [h] (vec (kv-parts db h :schema))))))

(defn- referenced
  [db h]
  (let [s     (transient #{})
        add!  (fn [id r] (when r (conj! s (str (pr-str id) "|" (rev-key r)))))
        parts (fn [space] (kv-parts db h space))]
    (doseq [b (parts :whead), [k r] (kv-pages db h [:whead b] {})] (add! (res/read-id k) r))
    (doseq [b (parts :wstate), [_ st] (kv-pages db h [:wstate b] {}), [id r] st] (add! id r))
    (doseq [b (parts :whist), [k [from to]] (kv-pages db h [:whist b] {})]
      (let [id (res/read-id (subs k 0 (str/last-index-of k "|")))]
        (add! id from) (add! id to)))
    (doseq [b (parts :wlog), [_ {:keys [changes]}] (kv-pages db h [:wlog b] {}),
            [id [from to]] changes]
      (add! id from) (add! id to))
    (persistent! s)))

(defn gc-versions!
  [db]
  (let [{:keys [keep nodes]}
        (with-read db
          (fn [h]
            {:keep  (referenced db h)
             :nodes (into [] (for [part (kv-parts db h :wnode)
                                   [k] (kv-pages db h [:wnode part] {:keys-only? true})]
                               [part k]))}))
        drop (filterv (fn [[part k]] (not (contains? keep (str part "|" k)))) nodes)]
    (doseq [page (partition-all 2000 drop)]
      (let [h (begin db :write nil)]
        (try (apply! db h (mapv (fn [[part k]] (kv-fx [:wnode part k] nil)) page))
             (commit! db h)
             (catch Throwable t (rollback! db h) (throw t)))))
    {:versions (count nodes) :dropped (count drop)}))

(defn repair!
  [db opts]
  (let [bs     (with-read db (fn [h] (vec (remove #{root} (kv-parts db h :schema)))))
        resets (group-by #(reset-branch! db %) bs)
        rp     (replay! db (assoc opts :dry-run? false))
        shared (compact-states! db)
        gc     (gc-versions! db)
        vf     (verify-all! db (dissoc opts :on-branch))]
    {:ok?    (and (:ok? rp) (:ok? vf))
     :reset  (update-vals resets count)
     :kept   (vec (mapcat val (dissoc resets :reset)))
     :replay (select-keys rp [:ok? :ms])
     :states-shared shared
     :gc     gc
     :verify (assoc (select-keys vf [:ok? :ms])
                    :problems (vec (filter (comp seq :problems) (:branches vf))))}))

(defn- canon-map
  [db]
  (with-read db
    (fn [h]
      (let [vs (for [part (kv-parts db h :wnode)
                     :let [id (res/read-id part)]
                     [k [v]] (kv-pages db h [:wnode part] {})]
                 [id (res/read-rev k) (digest v)])]
        (reduce (fn [m [id grp]]
                  (into m (for [[_ rs] (group-by #(nth % 2) grp)
                                :let [r0 (apply min (map second rs))]
                                [_ r] rs :when (not= r r0)]
                            [[id r] [id r0]])))
                {} (group-by first vs))))))

(defn- dedupe-branch!
  [db b m]
  (let [h (begin db :write nil)]
    (try
      (lock! db h [[:branch b]])
      (let [to  (fn [id r] (if-let [[_ r0] (get m [id r])] r0 r))
            fx  (concat
                 (for [[k r] (kv-pages db h [:whead b] {})
                       :let [id (res/read-id k) r' (to id r)] :when (not= r r')]
                   (kv-fx [:whead b k] r'))
                 (for [[k st] (kv-pages db h [:wstate b] {})
                       :let [st' (into {} (map (fn [[id r]] [id (to id r)])) st)]
                       :when (not= st st')]
                   (kv-fx [:wstate b k] st'))
                 (for [[k [from t] :as row] (kv-pages db h [:whist b] {})
                       :let [id (res/read-id (subs k 0 (str/last-index-of k "|")))
                             ch [(some->> from (to id)) (some->> t (to id))]]
                       :when (not= (second row) ch)]
                   (kv-fx [:whist b k] (when (not= (first ch) (second ch)) ch)))
                 (for [[k e] (kv-pages db h [:wlog b] {})
                       :when (:changes e)
                       :let [chs (into {} (keep (fn [[id [from t]]]
                                                  (let [ch [(some->> from (to id))
                                                            (some->> t (to id))]]
                                                    (when (not= (first ch) (second ch))
                                                      [id ch]))))
                                       (:changes e))]
                       :when (not= chs (:changes e))]
                   (kv-fx [:wlog b k] (assoc e :changes chs))))
            fx  (vec fx)]
        (if (seq fx)
          (do (apply! db h fx) (commit! db h))
          (rollback! db h))
        (count fx))
      (catch Throwable t (rollback! db h) (throw t)))))

(defn- unheld-references
  [db]
  (with-read db
    (fn [h]
      (let [have (into #{} (for [part (kv-parts db h :wnode)
                                 [k] (kv-pages db h [:wnode part] {:keys-only? true})]
                             (str part "|" k)))]
        (into [] (remove have) (referenced db h))))))

(defn- worlds-digest
  [db]
  (with-read db
    (fn [h]
      (into {}
            (for [b    (kv-parts db h :wlog)
                  [k]  (kv-scan db h [:wlog b] {:keys-only? true})
                  :let [r (res/read-rev k) st (state-at db h b r)] :when st]
              [[b r] (digests-of db h st)])))))

(defn dedupe-versions!
  [db]
  (binding [*seen* (atom {})]
    (let [before (worlds-digest db)
          m      (canon-map db)
          bs     (with-read db (fn [h] (vec (distinct (mapcat #(kv-parts db h %)
                                                           [:whead :wstate :whist :wlog])))))
          n      (reduce + (map #(dedupe-branch! db % m) bs))
          gc     (gc-versions! db)
          after  (binding [*seen* (atom {})] (worlds-digest db))
          off    (into [] (comp (filter (fn [[k d]] (not= d (get after k)))) (map key)) before)
          unheld (unheld-references db)]
      {:ok?            (and (empty? off) (empty? unheld))
       :duplicates     (count m)
       :rewritten      n
       :gc             gc
       :unheld         (take 10 unheld)
       :changed-worlds (take 10 off)})))
