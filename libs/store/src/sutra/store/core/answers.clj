(ns sutra.store.core.answers
  (:require [sutra.store.api :as api]
            [sutra.store.lib :as lib]
            [sutra.store.memo :as memo]
            [sutra.store.plan :as plan]
            [sutra.store.scan :as scan]
            [sutra.store.semantics :as sem]
            [sutra.store.spi :as spi]
            [sutra.ref.objectset :as ros]
            [sutra.types.object-set :as os]
            [sutra.store.core :as-alias core]
            [sutra.store.core.branches :refer [ancestry branch-row live-root]]
            [sutra.store.core.effects :refer [->link ->object canon canonical-ir canonical-op
                                              canonical-step cascade deleted-by edits-of
                                              fold-targets read-rows read-touching rename-in step]]
            [sutra.store.core.engine :refer [*verify* candidates engine-can? question]]
            [sutra.store.core.kv :refer [branch-k change-k clock-k enc kv-get kv-pages kv-read
                                         kinded-through kv-scan log-indexed-k log-key
                                         log-kind-prefix log-kinded-k records-clock-k rev-k
                                         rev-key schema-logged-k]]
            [sutra.store.core.orakel :as orakel]
            [sutra.store.core.reads :refer [caches counted derived fetch head-world held-step
                                            link-count
                                            link-matches links read-cx resource-history rows
                                            changed-since through type-ticks world-at]]
            [sutra.store.core.resources :as res]
            [sutra.store.core.records :refer [in-engine? indexed! rec-list record-value
                                              wanted-index]]))

(set! *warn-on-reflection* true)

(defn overlay-of
  [db h {:keys [W sig] :as rc} ops]
  (let [ops    (mapv (partial canonical-op W) ops)
        stored (read-rows db h rc (fold-targets ops false))]
    (cascade W sig (read-touching db h rc (deleted-by ops))
             (reduce (partial step (fn [T id] (get stored [T (str id)]))) {} ops))))

(defn- overlaid
  [mine base wanted?]
  (if (empty? mine)
    base
    (into (into [] (remove #(contains? mine (:id %))) base)
          (for [[_ v] mine :when (and (map? v) (wanted? v))] v))))

(defn- rows*
  [db h rc ov T ids]
  (let [want (some->> ids (map str) set)]
    (overlaid (get-in ov [:rows T]) (rows db h rc T ids)
              #(or (nil? want) (contains? want (:id %))))))

(defn- links*
  [db h rc ov L where]
  (overlaid (get-in ov [:links L]) (links db h rc L where) (link-matches where)))

(defn- touches?
  [W ov ir]
  (let [types (set (keys (:rows ov)))
        ls    (set (keys (:links ov)))]
    (and (or (seq types) (seq ls))
         (or (not (:op ir))
             (some (fn [{:keys [op root link dir]}]
                     (or (and root (or (types root) (not (contains? (:object-types W) root))))
                         (and (#{:search-around :with} op) (or (nil? dir) (ls link) (seq ls)))))
                   (os/nodes ir))))))

(def capabilities #{:objects :schema :log :records :branches})

(defn- page
  [{:keys [after limit]} key-of xs]
  (cond->> xs
    (some? after) (drop-while #(<= (compare (key-of %) after) 0))
    (some? limit) (take limit)
    :always vec))

(defn readable-ir
  [W ir]
  (let [ir (os/assert-readable! (if (:op ir) ir (os/qt->ir ir)))]
    (some-> (first (filter #(= :page (:op %)) (os/nodes ir))) lib/check-paging!)
    (canonical-ir W ir)))

(defn- hidden-in
  [W authority]
  (into #{} (mapcat (fn [T] [T (canon W :object-types T)])) (:hidden-object-types authority)))

(defn- guarded-in
  [W authority]
  (if (lib/policy-exempt? authority)
    #{}
    (into #{} (keep (fn [[T spec]] (when (seq (:security spec)) T))) (:object-types W))))

(defn- reads-hidden?
  [W hidden ir]
  (and (seq hidden)
       (some (fn [{:keys [op root]}]
               (or (#{:search-around :with} op)
                   (and root (or (hidden root) (not (contains? (:object-types W) root))))))
             (os/nodes ir))))

(defn- pushdown?
  [{:keys [W base]} ov hidden ir]
  (and (nil? base) (not (touches? W ov ir)) (not (reads-hidden? W hidden ir))))

(defn- claiming
  [db h {:keys [W sig] :as rc} ov hidden denied ir]
  (when (pushdown? rc ov hidden ir)
    (let [p      (plan/fk-joins sig (lib/plan W ir))
          [q c'] (question db h rc :fragment)
          [x pr] (plan/executor-for (candidates db) q #(seq (plan/claims (:plan %) sig p)))]
      (when x
        (let [cl (plan/claims (:plan pr) sig p)]
          (assoc (plan/split p cl denied ir) :executor x :claims cl :cx c'))))))

(defn- same-rows?
  [ordered? a b]
  (let [by-id #(vec (sort-by :id sem/compare-values %))]
    (if ordered? (sem/same-data? (vec a) (vec b)) (sem/same-data? (by-id a) (by-id b)))))

(defn- select
  [db h {:keys [W] :as rc} ov authority ir]
  (let [ir     (readable-ir W ir)
        denied (lib/denied-markings authority)
        vis?   (partial lib/visible? denied)
        hidden (hidden-in W authority)
        ref    (fn [ir]
                 (let [ext   (memoize (fn [T]
                                        (if (hidden T)
                                          []
                                          (let [T (canon W :object-types T)]
                                            ((lib/guarded (lib/row-guard W authority T))
                                             (into [] (comp (map (partial ->object T))
                                                            (filter vis?))
                                                   (rows* db h rc ov T nil)))))))
                       edges (memoize (fn [L]
                                        (into [] (comp (filter vis?) (map (juxt :from :to)))
                                              (links* db h rc ov (canon W :link-types L) nil))))
                       sigma {:sigma {:extension (fn [_ T] (ext T)) :edges (fn [_ L] (edges L))}}]
                   (try (ros/read sigma W ir)
                        (catch clojure.lang.ExceptionInfo e
                          (if (:ref/kind (ex-data e))
                            (throw (api/refusal :malformed (ex-message e) (ex-data e)))
                            (throw e))))))
        {:keys [executor claims fragments twin cx]}
        (claiming db h rc ov (into hidden (guarded-in W authority)) denied ir)
        run    (fn [{k :ref p :plan :as fr}]
                 (let [rows (spi/-execute executor h cx (dissoc fr :ir))]
                   (when (and *verify* (not= spi/decline rows))
                     (let [r (ref (:ir fr))]
                       (when-not (same-rows? (and (plan/whole? claims) (plan/ordered? p)) rows r)
                         (throw
                          (api/refusal :unavailable
                                       "store engine answered a fragment otherwise than the core's reference"
                                       (scan/divergence
                                        :fragment (dissoc fr :ir) nil (vec rows) (vec r) 20))))))
                   [k rows]))
        answers (into {} (map run) fragments)]
    (cond
      (or (empty? fragments) (some #(= spi/decline %) (vals answers))) (ref ir)
      (plan/whole? claims) (vec (get answers 0))
      :else (ref (plan/assemble twin answers)))))

(defn as-of!
  [db as-of kind]
  (when (and as-of
             (or (#{:records/get :records/list :records/clock :objects/edited} kind)
                 (and (#{:objects :links :select} kind)
                      (or (not (engine-can? db :objects/history))
                          (= ::core/unknown (:root-rev as-of))))))
    (api/refuse! :as-of-unsupported
                 (str "revision " (:rev as-of)
                      " is not the head, and this store keeps no history of "
                      (if (#{:objects :links :select} kind) "objects" "records"))
                 {:rev (:rev as-of) :query kind})))

(defn committed-world
  [db h b as-of tick]
  (if (:rev as-of) (world-at db h b (:rev as-of)) (head-world db h b tick)))

(defn- head-derived
  [db h b as-of tick]
  (if-let [held (when-not (:rev as-of) (held-step db b tick))]
    (do (when orakel/on? (orakel/held! db (committed-world db h b as-of tick) (:W held)))
        [(:W held) (:sig (:st held))])
    [(derived db (committed-world db h b as-of tick)) nil]))

(defn snapshot-cx
  [db h b bs as-of tick lay wo]
  (let [[W0 sig0] (head-derived db h b as-of tick)
        rc        (if sig0
                    (read-cx db b W0 sig0 as-of bs lay)
                    (read-cx db b W0 as-of bs lay))]
    (if-let [{:keys [world migration]} wo]
      (let [W (derived db world)]
        (assoc (read-cx db b W as-of bs lay)
               :base {:rc rc :steps (mapv (partial canonical-step W0 W) migration)}))
      rc)))

(defn- renamed-entry
  [renames r e]
  (let [rs (for [[r' pairs] renames :when (> r' r) p pairs] p)
        nm (fn [x] (reduce (fn [x [a b]] (if (= x a) b x)) x rs))]
    (cond-> e
      (and (seq rs) (:attrs e))  (update :attrs (partial mapv nm))
      (and (seq rs) (:values e)) (update :values #(reduce (fn [m [a b]] (rename-in m a b)) % rs))
      (and (seq rs) (:before e)) (update :before #(reduce (fn [m [a b]] (rename-in m a b)) % rs)))))

(defn- log-indexed?
  [{:keys [memo] :as db} h]
  (or (some? (memo/hit memo ::core/flags ::core/log-indexed))
      (when (kv-get db h log-indexed-k)
        (memo/put! memo caches ::core/flags [[::core/log-indexed true]])
        true)))

(defmulti ^:private answer*
  (fn [_q [kind]] kind))

(defn answer
  [{:keys [db basis as-of] :as s} authority [kind :as query]]
  (as-of! db as-of kind)
  (answer* (assoc s :b (:branch basis) :rev (:rev basis) :authority authority) query))

(defn- visible [authority] (partial lib/visible? (lib/denied-markings authority)))

(defmethod answer* :default [_ [kind]]
  (api/refuse! :unsupported (str "this store does not answer " (pr-str kind))
               {:store/unsupported kind}))

(defmethod answer* :store/capabilities [{:keys [db]} _]
  (into capabilities (spi/capabilities (spi/-profile (:engine db)))))

(defmethod answer* :branch/list [{:keys [db h]} _]
  (mapv second (kv-scan db h [:branch ""] nil)))

(defmethod answer* :branch/get [{:keys [db h]} [_ x]]
  (branch-row db h x))

(defmethod answer* :branch/tips [{:keys [db h]} [_ bs]]
  (let [kv    (kv-read db h (mapcat (fn [b] [(branch-k b) (clock-k b [:schema])]) bs))
        own   #(get kv (clock-k % [:schema]))
        fork  #(get-in kv [(branch-k %) :base :rev])
        es    (kv-read db h (map rev-k (distinct (keep identity (mapcat (juxt own fork) bs)))))
        entry #(some->> % rev-k (get es))]
    (into {} (for [b bs
                   :let [last (entry (own b))]]
               [b {:last (when (= b (:branch last)) (select-keys last [:rev :at :actor]))
                   :fork (some-> (entry (fork b)) (select-keys [:rev :at]))}]))))

(defmethod answer* :schema/world [{:keys [db h b as-of wo tick]} [_ x]]
  (cond
    (:world wo)   (cond->> (:world wo) (:derived? x) (derived db))
    (:derived? x) (first (head-derived db h b as-of tick))
    :else         (committed-world db h b as-of tick)))

(defmethod answer* :schema/changed [{:keys [db h b as-of]} [_ {:keys [after]}]]
  (changed-since db h b after (:rev as-of)))

(defmethod answer* :schema/resource-history [{:keys [db h b as-of authority]} [_ [s k :as id]]]
  (if (and (= :object-types s) (lib/hidden-type? authority k))
    []
    (resource-history db h b (:rev as-of) id)))

(defn- newest-commits
  [db h rev limit keep?]
  (loop [upto rev wanted #{rev} out [] page (max 64 (* 4 limit))]
    (let [kvs (when (and (< (count out) limit) (seq wanted) (>= upto 0))
                (kv-scan db h [:rev ""] {:upto (rev-key upto) :reverse? true :limit page}))
          [wanted out] (reduce (fn [[w o :as acc] [_ e]]
                                 (cond
                                   (or (>= (count o) limit) (empty? w)) (reduced acc)
                                   (contains? w (:rev e)) [(into (disj w (:rev e)) (:parents e))
                                                           (cond-> o (keep? e) (conj e))]
                                   :else acc))
                               [wanted out] kvs)]
      (if (< (count kvs) page)
        out
        (recur (dec (:rev (second (last kvs)))) wanted out (min 8192 (* 2 page)))))))

(defn- history-all [{:keys [db h rev]} x]
  (let [{:keys [before after limit schema?]} x
        revs (->> (ancestry db h rev) (remove zero?)
                  (remove #(and before (>= % before)))
                  (remove #(and after (<= % after)))
                  (sort (if after < >)))
        es   (kv-read db h (map rev-k revs))]
    (cond->> (->> revs
                  (map #(get es (rev-k %)))
                  (filter #(or (not schema?) (:schema? %))))
      limit (take limit)
      :always vec)))

(defmethod answer* :schema/history [{:keys [db h rev] :as q} [_ x]]
  (if (and (:limit x) (nil? (:after x)))
    (let [{:keys [before limit schema?]} x]
      (newest-commits db h rev limit #(and (pos? (:rev %))
                                           (or (nil? before) (< (:rev %) before))
                                           (or (not schema?) (:schema? %)))))
    (history-all q x)))

(defn- logged-revs
  [db h b rev {:keys [after limit newest?]}]
  (let [tick-k  (clock-k b [:schema])
        kv      (kv-read db h [(schema-logged-k b) tick-k])
        through (get kv (schema-logged-k b))
        tick    (get kv tick-k)]
    (when (or (nil? through) (and tick (> tick through)))
      (api/refuse! :unindexed (str "the schema log of " b " has a gap after " through)
                   {:branch b :schema-log/gap true}))
    (mapv (comp parse-long first)
          (kv-scan db h [:schema-log b]
                   (cond-> {:upto (rev-key rev) :keys-only? true}
                     after   (assoc :after (rev-key after))
                     limit   (assoc :limit limit)
                     newest? (assoc :reverse? true))))))

(defmethod answer* :schema/commits [{:keys [db h b rev]} [_ opts]]
  (let [revs (logged-revs db h b rev opts)
        es   (kv-read db h (map rev-k revs))]
    (into [] (keep #(get es (rev-k %))) revs)))

(defn- window [db h b rev n]
  (let [revs  (logged-revs db h b rev {:limit n :newest? true})
        full  (= n (count revs))
        own   (kv-scan db h [:wlog b] (cond-> {:upto (rev-key rev) :reverse? true}
                                        full (assoc :after (rev-key (peek revs)))))
        start (when-not full (second (first (kv-scan db h [:wlog b] {:limit 1}))))
        since (when-not (or full (:fork start)) (:since (kv-get db h (res/format-k b))))
        older (when (pos? (or since 0)) (seq (logged-revs db h b (dec since) {:limit 1})))]
    (cond
      full           {:entries own :complete? true :reaches-back-to (peek revs)}
      (:fork start)  (let [{:keys [from rev]} (:fork start)]
                       (update (window db h from rev (- n (count revs))) :entries
                               #(concat own %)))
      (or older (and (nil? since) (seq revs)))
      {:entries own :complete? false :reaches-back-to since}
      :else          {:entries own :complete? true :reaches-back-to nil})))

(defmethod answer* :schema/removed [{:keys [db h b rev]} [_ {:keys [limit]}]]
  (let [{:keys [entries] :as w} (window db h b rev (or limit 100))
        gone (->> entries
                  (reduce (fn [[seen out] [k {:keys [changes]}]]
                            (reduce-kv (fn [[seen out] id [from to]]
                                         (cond
                                           (seen id) [seen out]
                                           (and (nil? to) from (not= id res/frame-id))
                                           [(conj seen id)
                                            (conj out {:id id :rev (parse-long k) :from from})]
                                           :else [(conj seen id) out]))
                                       [seen out] (into (sorted-map-by #(compare (str %1)
                                                                                 (str %2)))
                                                        changes)))
                          [#{} []])
                  second)
        vals (fetch db h (map (juxt :id :from) gone))]
    (assoc (dissoc w :entries)
           :removed (mapv (fn [{:keys [id from] :as g}]
                            (-> g (dissoc :from) (assoc :value (vals [id from]))))
                          gone))))

(defmethod answer* :schema/last-changed [{:keys [db h b rev]} [_ {:keys [limit after]}]]
  (let [{:keys [entries complete? reaches-back-to]}
        (if after
          {:entries (kv-scan db h [:wlog b] {:upto (rev-key rev) :reverse? true
                                             :after (rev-key after)})
           :complete? true}
          (window db h b rev (or limit 1000000)))
        newest (reduce (fn [out [k {:keys [changes]}]]
                         (reduce-kv (fn [out id _]
                                      (if (or (contains? out id) (= id res/frame-id))
                                        out
                                        (assoc out id (parse-long k))))
                                    out changes))
                       {} entries)
        commits (kv-read db h (map rev-k (cond-> (distinct (vals newest))
                                           reaches-back-to (conj reaches-back-to))))
        when-at #(:at (get commits (rev-k %)))]
    (cond-> {:complete? (boolean complete?)
             :changed   (into {}
                              (map (fn [[id r]]
                                     (let [{:keys [at actor]} (get commits (rev-k r))]
                                       [id {:rev r :at at :by (:principal actor)}])))
                              newest)}
      reaches-back-to (assoc :floor-at (when-at reaches-back-to)))))

(defn- laid-out
  [{:keys [memo engine]} h rc as-of b _tick layout]
  (let [cx  (or (:rc (:base @rc)) @rc)
        ask #(spi/-inspect engine h cx)]
    (if as-of (ask) (through memo ::core/laid-out [b layout (:ns cx) (:sig cx)] ask))))

(defmethod answer* :store/layout [{:keys [db h rc as-of b tick layout]} _]
  (laid-out db h rc as-of b tick layout))

(defmethod answer* :objects [{:keys [db h rc ov authority]} [_ x y]]
  (let [{:keys [W]} @rc T (canon W :object-types x)]
    (if (or (lib/hidden-type? authority x) (lib/hidden-type? authority T))
      []
      (let [ids   (some->> y (map str) distinct vec)
            found ((lib/guarded (lib/row-guard W authority T))
                   (into [] (comp (map (partial ->object T)) (filter (visible authority)))
                         (rows* db h @rc ov T ids)))]
        (if (some? ids)
          (let [by-id (into {} (map (juxt :id identity)) found)]
            (into [] (keep by-id) ids))
          (vec (sort-by (comp str :id) found)))))))

(defn- countable?
  [c ov authority L]
  (and (empty? (get-in ov [:links L]))
       (empty? (lib/denied-markings authority))
       (nil? (:base c))))

(defmethod answer* :links [{:keys [db h rc ov authority b as-of clocks]} [_ x y]]
  (let [{:keys [W]} @rc
        L     (canon W :link-types x)
        where (some-> (dissoc y :count?) not-empty (update-vals #(mapv str %)))]
    (if (and (:count? y) (countable? @rc ov authority L))
      (if (or where as-of)
        (link-count db h @rc L where)
        (counted db h @clocks @rc b L))
      (let [found (into [] (comp (map (partial ->link L)) (filter (visible authority)))
                        (links* db h @rc ov L where))]
        (if (:count? y) (count found) found)))))

(defmethod answer* :select [{:keys [db h rc ov authority]} [_ x]]
  (select db h @rc ov authority x))

(defmethod answer* :select/explain [{:keys [db h rc ov authority]} [_ x]]
  (let [{:keys [W]} @rc
        ir (readable-ir W x)
        {:keys [executor claims fragments cx]}
        (claiming db h @rc ov (into (hidden-in W authority) (guarded-in W authority))
                  (lib/denied-markings authority) ir)
        explains? (and executor
                       (:explain? (some (fn [[e p]] (when (= e executor) p)) (candidates db))))]
    (cond-> {:pushdown? (boolean (seq fragments)) :claims (vec claims)}
      explains? (assoc :engine (if (plan/whole? claims)
                                 (spi/-explain executor h cx (dissoc (first fragments) :ir))
                                 (into {}
                                       (for [fr fragments]
                                         [(:ref fr)
                                          (spi/-explain executor h cx (dissoc fr :ir))])))))))

(defn- guard-changes
  [g denied read-now es]
  (if (or g (seq denied))
    (let [{:keys [admit? redact] :or {admit? (constantly true) redact identity}} g
          ids (into #{} (map :id) es)
          ok  (into {} (comp (filter (partial lib/visible? denied))
                             (filter (comp admit? :values))
                             (map (juxt :id :values)))
                    (when (seq ids) (read-now (vec ids))))]
      (for [e es
            :let [row   (get ok (:id e))
                  blank (fn [vs] (select-keys vs (keys (redact (merge row vs)))))]
            :when (some? row)]
        (cond-> e
          (:values e) (update :values blank)
          (:before e) (update :before blank))))
    es))

(defmethod answer* :objects/changes [{:keys [db h rc rev authority]} [_ x y]]
  (let [{:keys [W]} @rc T (canon W :object-types (canon W :link-types x))
        {:keys [ids after upto limit]} y
        on   (ancestry db h rev)
        want (some->> ids (map str) set)
        renames (into [] (keep (fn [[k rs]] (let [r (parse-long k)] (when (on r) [r rs]))))
                      (kv-scan db h [:renames (enc T)] nil))]
    (if (or (lib/hidden-type? authority x) (lib/hidden-type? authority T))
      []
      (let [cs (for [[k {:keys [origin] :as c}] (kv-pages db h [:change (enc T)]
                                                          (cond-> {}
                                                            after (assoc :after (rev-key after))
                                                            upto  (assoc :upto (rev-key upto))))
                     :let [r (parse-long k)]
                     :when (and (on r) (or (nil? (:origin y)) (= (:origin y) origin)))]
                 [k r c])
            es (kv-read db h (for [[k] cs] [:rev "" k]))]
        (cond->> (for [[k r {:keys [origin entries]}] cs
                       :let [{:keys [actor at]} (get es [:rev "" k])]
                       e entries
                       :when (or (nil? want) (want (:id e)))]
                   (renamed-entry renames r (assoc e :rev r :origin origin :actor actor :at at)))
          true  (guard-changes (lib/row-guard W authority T)
                               (when (contains? (:object-types W) T)
                                 (lib/denied-markings authority))
                               #(rows* db h @rc nil T %))
          limit (take limit)
          true  vec)))))

(defn- type-clock
  [{:keys [memo] :as db} h T rev]
  (if-let [[v] (memo/hit memo ::core/clock [T rev])]
    v
    (let [entry (kv-get db h (rev-k rev))
          known (map #(memo/hit memo ::core/clock [T %]) (:parents entry))
          v     (cond
                  (some? (kv-get db h (change-k T rev))) rev
                  (and entry (seq known)
                       (every? some? known)) (some->> (seq (keep first known)) (apply max))
                  :else (let [on (ancestry db h rev)]
                          (some (fn [[k]] (let [r (parse-long k)] (when (on r) r)))
                                (kv-pages db h [:change (enc T)]
                                          {:upto (rev-key rev) :reverse? true
                                           :keys-only? true}))))]
      (when entry (memo/put! memo caches ::core/clock [[[T rev] v]]))
      v)))

(defmethod answer* :objects/clock [{:keys [db h rc rev authority]} [_ x]]
  (let [{:keys [W]} @rc T (canon W :object-types (canon W :link-types x))]
    (when-not (lib/hidden-type? authority T)
      (type-clock db h T rev))))

(defmethod answer* :objects/count [{:keys [db h rc ov authority b as-of clocks]} [_ x]]
  (let [{:keys [W sig] :as c} @rc
        T   (canon W :object-types (canon W :interfaces x))
        ask #(or (some-> (first (select db h c ov authority
                                        {:op :aggregate :aggs :count :in {:op :scan :root T}}))
                         :metrics :count long)
                 0)]
    (if (and (nil? as-of) (nil? ov) (nil? (:base c)) (empty? (lib/denied-markings authority))
             (not (lib/hidden-type? authority T)) (nil? (lib/row-guard W authority T)))
      (through (:memo db) ::core/rows-of
               [b T (mapv #(get @clocks (enc %)) (cons [:schema] (type-ticks sig T)))]
               ask)
      (ask))))

(defmethod answer* :objects/edited [{:keys [db h b rc authority]} [_ x y]]
  (let [{:keys [W]} @rc T (canon W :object-types (canon W :link-types x))]
    (if (lib/hidden-type? authority T)
      {}
      (edits-of db h b (live-root db h b) T y))))

(defn- windowed-log
  [db h part from keep? limit reverse?]
  (loop [window (max 128 (* 4 limit))]
    (let [rows (kv-scan db h part
                        (cond-> (assoc from :limit window) reverse? (assoc :reverse? true)))
          kept (into [] (comp (map second) (filter keep?) (take limit)) rows)]
      (if (or (= limit (count kept)) (< (count rows) window))
        kept
        (recur (* 4 window))))))

(defn- kind-mark
  [db h b]
  (let [kv (kv-read db h [(branch-k b) (log-kinded-k b)])]
    (kinded-through (get kv (branch-k b)) (get kv (log-kinded-k b)))))

(defn- log-spans
  [db h b rev {:keys [after kinds own?]}]
  (let [kind? #(or (nil? kinds) (contains? kinds (:kind %)))
        past  #(log-key % Integer/MAX_VALUE)
        own   (when (and own? (log-indexed? db h)) #(and (<= (:rev %) rev) (kind? %)))
        one   (when (and own (= 1 (count kinds))) (first kinds))
        m     (when one (kind-mark db h b))]
    (cond
      m    (let [pre (log-kind-prefix one)]
             (cond-> [[[:log-kind b]
                       (cond-> {:upto (str pre (past (min m rev)))}
                         after (assoc :after (str pre (past after))))
                       #(= one (:kind %))]]
               (> rev m) (conj [[:log-of b] {:after (past (max m (or after m)))} own])))
      own  [[[:log-of b] (when after {:after (past after)}) own]]
      :else
      (let [on (ancestry db h rev)]
        [[[:log ""] (when after {:after (past after)})
          #(and (on (:rev %)) (or (not own?) (= b (:branch %))) (kind? %))]]))))

(defmethod answer* :log/range [{:keys [db h b rev]} [_ x]]
  (let [{:keys [limit newest?] :as o} x
        spans (log-spans db h b rev x)]
    (if limit
      (reduce (fn [acc [part from keep?]]
                (let [n (- limit (count acc))]
                  (if (pos? n)
                    (into acc (windowed-log db h part from keep? n (boolean newest?)))
                    (reduced acc))))
              [] (cond-> spans newest? rseq))
      (->> spans
           (mapcat (fn [[part from keep?]] (filter keep? (map second (kv-scan db h part from)))))
           (page (dissoc o :after :own? :newest?) nil)))))

(defmethod answer* :records/get [{:keys [db h b]} [_ x y]]
  (let [v (record-value db h b x y)] (when-not (= api/absent v) v)))

(defmethod answer* :records/clock [{:keys [db h]} [_ coll]]
  (if (in-engine? db coll)
    (spi/-rec-clock (:engine db) h coll)
    (kv-get db h (records-clock-k coll))))

(defmethod answer* :records/list [{:keys [db h b]} [_ x y]]
  (indexed! db x (cond-> (wanted-index y) (:group-by y) (assoc (:group-by y) :eq)))
  (rec-list db h b x (dissoc y :project) (:project y) false))
