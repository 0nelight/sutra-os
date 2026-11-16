(ns sutra.store.core.commit
  (:require [sutra.kit.anomaly :as anomaly]
            [malli.core :as m]
            [sutra.world.delta :as d]
            [sutra.types.schema :as sch]
            [sutra.ref.err :as err]
            [sutra.store.api :as api]
            [sutra.store.lib :as lib]
            [sutra.store.spi :as spi]
            [sutra.store.core :as-alias core]
            [sutra.store.core.branches :refer [ancestor? bases-ks live! live-branch root-of]]
            [sutra.store.core.effects :refer [->object adopt-effects adopted-restores canon
                                              canonical-adoption canonical-fence canonical-op
                                              canonical-step cascade change-effects deleted-by
                                              edit-entries edits-of fk-link-refusal fold-refusal
                                              fold-targets index-ops read-holders read-rows
                                              read-touching rename-edit row-effects step ticks]]
            [sutra.store.core.engine :refer [lock!]]
            [sutra.store.core.kv :refer [branch-k clock-k detached-k edited-k enc kv-fx kv-read
                                         kv-writes lease-k log-k log-kind-k log-kinded-k
                                         log-of-k records-clock-k renames-k rev-k schema-log-k
                                         schema-logged-k
                                         with-layout-stamp]]
            [sutra.store.core.reads :refer [checked-entries! cold-step derived fixed? head-revs
                                            head-world held-step keep-step! kept-since
                                            read-cx sig-of sig-state stepped-sig]]
            [sutra.store.core.orakel :as orakel]
            [sutra.store.core.resources :as res]
            [sutra.store.core.records :refer [collection! in-engine? rec-list rec-ns record-effect
                                              record-key record-loc record-value unique-claims
                                              unique-holders unique-refusal]]))

(set! *warn-on-reflection* true)

(defn- fence-reads
  [db h b W sig root basis fences pre]
  (let [root    (when (:root-rev basis) root)
        clocked #{:schema :type :link-type}
        kv      (kv-read db h (concat (for [[k :as f] fences :when (clocked k)
                                            part (cond-> [b] root (conj root))]
                                        (clock-k part f))
                                      (for [[k x] fences :when (= :lease k)] (lease-k x))))
        rc      (assoc (read-cx db b W sig nil nil nil) :commit? true)
        stored  (read-rows db h rc
                           (into {}
                                 (for [[T
                                        fs]
                                       (group-by second (filter #(= :object (first %)) fences))]
                                   [T (into [] (comp (map #(str (nth % 2))) (distinct)) fs)])))]
    (into {}
          (for [[k x y :as f] fences]
            [f (case k
                 :head      nil
                 :branch-head (:rev (live-branch db h x))
                 (:schema :type :link-type)
                 {:b (get kv (clock-k b f)) :root (when root (get kv (clock-k root f)))}
                 :object    (api/object-version (some->> (get stored [x (str y)]) (->object x)))
                 :record    (record-value db h b x y pre)
                 :lease     (get kv (lease-k x)))]))))

(defn- fence-holds?
  [row basis reads [k _ y z :as f]]
  (let [r      (get reads f)
        moved? (fn [] (or (when-let [c (:b r)] (> c (:rev basis)))
                          (when-let [c (:root r)] (> c (:root-rev basis)))))]
    (case k
      :head      (= (:rev row) (:rev basis))
      :branch-head (= y r)
      (:schema :type :link-type) (not (moved?))
      :object    (= z r)
      :record    (= z r)
      :lease     (>= y (or r Long/MIN_VALUE)))))

(defn- not-yet!
  [{:keys [restore]}]
  (when restore
    (api/refuse! :unsupported "a :restore commit ( A7) is not implemented yet"
                 {:store/unsupported :restore})))

(def ^:private attribute-steps #{:rename-attribute :drop-attribute :clear-attribute})

(defn- edit-effects
  [{:keys [b rev root? W steps forget scans detached]}]
  (let [renames (reduce (fn [m [k T a a']]
                          (cond-> m (= :rename-attribute k) (update T (fnil conj []) [a a'])))
                        {} steps)
        changed (for [[k T] steps :when (attribute-steps k)] T)
        detach  (when root?
                  (into [] (comp (distinct) (remove detached)) (concat changed forget)))]
    {:detach  (for [T detach] [(if (contains? (:link-types W) T) :detach-link-type :detach-type) T])
     :effects (concat
               (for [T detach] (kv-fx (detached-k b T) rev))
               (for [[T rs] renames] (kv-fx (renames-k T rev) rs))
               (for [T (distinct (concat (keys renames) (remove (set forget) detach)))
                     [ek e] (get scans T)
                     :let [e' (reduce rename-edit e (get renames T))]
                     :when (or (some #{T} detach) (not= e e'))]
                 (kv-fx [:edited b ek] e'))
               (for [T forget [ek _] (get scans T)]
                 (kv-fx [:edited b ek] nil)))}))

(def ^:private dropped-tick
  [:unclaimed :dropped])
(def ^:private orphaned-tick
  [:unclaimed :orphaned])

(defn- same-at?
  [sig0 sig touched]
  (every? (fn [[sec ks]] (every? #(= (get-in sig0 [sec %]) (get-in sig [sec %])) ks))
          touched))

(defn- schema-effects
  [{:keys [b rev r0 W schema forget edit-scans detached sig0 sig sig-touched kept es0 es
           res-heads all-heads unclaimed]} wc]
  (let [{:keys [detach effects]} (edit-effects {:b b :rev rev :root? (some? r0) :W W
                                                :steps (:migration schema) :forget forget
                                                :scans edit-scans :detached detached})
        steps  (into (vec detach) (:migration schema))
        drops? (some #{[:drop-unclaimed]} steps)
        same?  (if sig-touched (same-at? sig0 sig sig-touched) (= sig0 sig))
        clean? (let [{:keys [dropped orphaned]} unclaimed]
                 (and dropped (<= (or orphaned 0) dropped)))
        lays?  (or (not same?) (seq (remove #{[:drop-unclaimed]} steps))
                   (and drops? (not clean?)))]
    (concat
     (when (and (or schema (seq detach)) lays?)
       [[:layout wc (cond-> {:old sig0 :new sig :steps steps}
                      (and sig-touched (or (not drops?) clean?))
                      (assoc :only sig-touched))]])
     (when schema
       (cond
         (and drops? lays?)   [(kv-fx (clock-k b dropped-tick) rev)]
         (and (not same?) (not drops?)) [(kv-fx (clock-k b orphaned-tick) rev)]))
     (when schema
       (checked-entries! (if kept (select-keys es (res/changed es0 es)) es))
       (res/commit-effects b rev {:kept kept :es0 es0 :es es :heads res-heads
                                  :all-heads all-heads}))
     effects)))

(defn- log-effects
  [{:keys [b rev]} {:keys [actor log]}]
  (for [[i {:keys [kind payload outbox?]}] (map-indexed vector log)
        :let [e {:rev rev :index i :branch b :kind kind :payload payload :outbox? (boolean outbox?)
                 :actor actor}]
        fx [(kv-fx (log-k rev i) e) (kv-fx (log-of-k b rev i) e)
            (kv-fx (log-kind-k b kind rev i) e)]]
    fx))

(defn- lease-effects
  [{:keys [fences]}]
  (for [[k n e] fences :when (= :lease k)] (kv-fx (lease-k n) e)))

(defn- clock-effects
  [{:keys [b rev W schema ops restores]} {:keys [touched]}]
  (for [tick (distinct (concat (ticks W schema (concat ops restores))
                               (map (partial canonical-fence W) touched)))]
    (kv-fx (clock-k b tick) rev)))

(defn- records-clock-effects
  [rev records recs]
  (for [coll (distinct (for [[w [[op]]] (map vector records recs) :when (= :kv op)] (:coll w)))]
    (kv-fx (records-clock-k coll) rev)))

(defn- ref-effects
  [{:keys [b row r0 rev time schema heads kinded] {:keys [tick through]} :schema-log}
   {:keys [actor message parents origin] :or {origin :edit}} reparent]
  (cond-> [(kv-fx (rev-k rev)
                  (cond-> {:rev rev :branch b
                           :parents (into [(:rev row)]
                                          (map #(or (:rev %) (get heads (:branch %))))
                                          parents)
                           :actor actor :at time :origin origin}
                    schema   (assoc :schema? true)
                    r0       (assoc :root-rev r0)
                    message  (assoc :message message)
                    reparent (assoc :reparent reparent)))
           (kv-fx (branch-k b) (cond-> (assoc row :rev rev) reparent (assoc :base reparent)))]
    schema (conj (kv-fx (schema-log-k b rev) rev))
    (and schema through (<= (or tick 0) through))
    (conj (kv-fx (schema-logged-k b) rev))
    (= kinded {:rev (:rev row) :base (:base row)})
    (conj (kv-fx (log-kinded-k b) {:rev rev :base (if reparent reparent (:base row))}))))

(defn- retire-effects
  [{:keys [retired]}]
  (mapcat (fn [{:keys [branch row expect-rev dropped]}]
            (when (not= expect-rev (:rev row))
              (api/refuse! :conflict (str "branch " branch " moved")
                           {:branch branch :rev (:rev row)}))
            (concat [[:ns-drop branch] (kv-fx (branch-k branch) (assoc row :retired? true))]
                    dropped))
          retired))

(defn- written-records
  [records recs]
  (mapv (fn [w [_ k]] {:coll (:coll w) :key k}) records recs))

(defn- schema-diff
  [{:keys [old schema es0 es]}]
  (if (:ids schema)
    (lib/diff-paths (res/world (dissoc es0 res/frame-id)) (res/world (dissoc es res/frame-id)))
    (lib/diff-paths old (:authored schema))))

(defn- plan-effects
  [{:keys [b r0 W0 W schema ops restores adopt stored touching heads rev time sig edited
           layout-stamp] :as in}
   {:keys [actor reparent records origin] :or {origin :edit} :as tx}
   recs]
  (let [wc       {:ns b :sig sig :rev rev :time time :actor actor}
        ov       (cascade W sig touching
                          (reduce (partial step (fn [T id] (get stored [T (str id)]))) {}
                                  (concat ops restores)))
        reparent (some-> reparent
                         (assoc :rev (or (:rev reparent) (get heads (:branch reparent)))))]
    {:effects (with-layout-stamp
                (concat (schema-effects in wc)
                        (row-effects wc ov)
                        (when (seq ops)
                          (change-effects b rev (inst-ms time) origin ops edited stored))
                        (adopt-effects b rev adopt)
                        (map first recs)
                        (log-effects in tx)
                        (lease-effects tx)
                        (clock-effects in tx)
                        (records-clock-effects rev records recs)
                        (ref-effects in tx reparent)
                        (retire-effects in))
                layout-stamp
                (some? r0))
     :result  (cond-> {:basis (cond-> {:branch b :rev rev} r0 (assoc :root-rev r0))}
                schema         (assoc :schema-diff (schema-diff in)
                                      :world W :prior-world W0 :prior-tick (:tick in))
                (seq records)  (assoc :records (written-records records recs)))}))

(defn- plan-commit
  [db {:keys [b W ops stored holders records-now rev sig] :as in}
   {:keys [records origin] :or {origin :edit} :as tx}]
  (or
   (fold-refusal W ops stored holders origin)
   (fk-link-refusal sig ops)
   (unique-refusal db records (:unique-taken in))
   (let [recs (vec (map-indexed (fn [i w] (record-effect db b rev i w (get records-now i)))
                                records))]
     (or (first (filter anomaly/refusal? recs))
         (plan-effects in tx recs)))))

(def ^:private effect-schema
  [:multi {:dispatch first}
   [:kv           [:tuple [:= :kv] [:tuple :keyword :string :string] any?]]
   [:layout       [:tuple [:= :layout] :map :map]]
   [:delete-rows  [:tuple [:= :delete-rows] :map some? [:sequential some?]]]
   [:write-rows   [:tuple [:= :write-rows] :map some? [:sequential :map]]]
   [:delete-links [:tuple [:= :delete-links] :map some? [:sequential some?]]]
   [:write-links  [:tuple [:= :write-links] :map some? [:sequential :map]]]
   [:rec-put      [:tuple [:= :rec-put] :map :keyword some? any? nat-int?]]
   [:rec-copy     [:tuple [:= :rec-copy] :keyword :string :string]]
   [:ns-fork      [:tuple [:= :ns-fork] :string :string :map]]
   [:ns-drop      [:tuple [:= :ns-drop] :string]]])

(def ^:private effect? (m/validator effect-schema))

(def ^:private engine-call
  {:layout       spi/-layout!
   :delete-rows  spi/-delete-rows!
   :write-rows   spi/-write-rows!
   :delete-links spi/-delete-links!
   :write-links  spi/-write-links!
   :rec-copy     spi/-rec-copy!
   :ns-fork      spi/-ns-fork!
   :ns-drop      spi/-ns-drop!})

(defn apply!
  [{:keys [engine check?]} h effects]
  (when check?
    (when-let [bad (first (remove effect? effects))]
      (throw (api/refusal :unavailable "sutra.store.core planned a malformed effect"
                          {:explain (m/explain effect-schema bad)}))))
  (spi/-kv-write! engine h (kv-writes check? effects))
  (doseq [[k & args] effects :when (not (#{:kv :rec-put} k))]
    (apply (engine-call k) engine h args))
  (into {}
        (mapcat (fn [[[c coll] puts]]
                  (map (fn [[_ _ _ _ _ i] k] [i k])
                       puts (spi/-rec-put! engine h c coll (mapv (fn [[_ _ _ k v]] [k v]) puts)))))
        (group-by (fn [[_ c coll]] [c coll]) (filter #(= :rec-put (first %)) effects))))

(def ^:private historyless
  #{:basis :lane :actor :records :fences})

(defn service-write?
  [db {:keys [records fences] :as tx}]
  (let [global? #(= :global (:scope (collection! db %)))]
    (boolean
     (and (seq records)
          (every? historyless (keys tx))
          (every? (comp global? :coll) records)
          (every? (fn [[k x]] (case k :lease true :record (global? x) false)) fences)))))

(defn- global-locks
  [db {:keys [fences records]}]
  (sort-by pr-str
           (distinct
            (concat (for [[k n] fences :when (= :lease k)] [:lease n])
                    (for [{:keys [coll key]} records
                          :when (and (= :global (:scope (collection! db coll)))
                                     (not= api/auto key))]
                      [:record coll key])
                    (for [{:keys [coll cols vals]} (unique-claims db records)]
                      [:unique coll cols vals])))))

(defn commit-locks
  [db {:keys [basis fences retires] :as tx}]
  (concat
   (when-not (service-write? db tx)
     (sort (distinct (cons [:branch (:branch basis)]
                           (concat (for [[k b] fences :when (= :branch-head k)] [:branch b])
                                   (for [{:keys [branch]} retires] [:branch branch]))))))
   (global-locks db tx)))

(defn- fence-refusal
  [[k :as f]]
  (case k
    :object (api/refused :stale-object "StaleObject: target changed since it was read"
                         {:store/fence f :sutra/stale-object true})
    :lease  (api/refused :fenced (str "lease " (pr-str (second f)) " moved past epoch "
                                      (nth f 2) ": a later holder wrote") {:store/fence f})
    (api/refused :conflict (str "fence " (pr-str f) " no longer holds") {:store/fence f})))

(defn- broken-fence
  [row basis freads fs]
  (some->> fs (remove #(fence-holds? row basis freads %)) first fence-refusal))

(defn- check-fences!
  [row basis freads fs]
  (some-> (broken-fence row basis freads fs) anomaly/ex throw))

(declare dropped-with)

(defn- commit-heads
  [parents reparent]
  (distinct (for [p (cond-> (vec parents) reparent (conj reparent))
                  :when (nil? (:rev p))]
              (:branch p))))

(defn- expected-record? [{:keys [key] :as w}]
  (and (contains? w :expect) (not= api/auto key)))

(defn- named-kv-keys
  [db b heads {:keys [records fences retires]}]
  (keep identity
        (concat [(branch-k b) (clock-k b [:schema]) (log-kinded-k b)]
                (bases-ks db b)
                (map branch-k heads)
                (for [{:keys [branch]} retires] (branch-k branch))
                (for [{:keys [coll key] :as w} records :when (expected-record? w)]
                  (record-key db b coll key))
                (for [[k coll key] fences :when (= :record k)]
                  (record-key db b coll key)))))

(defn- checked-basis!
  [db h b basis fences pre]
  (let [row             (live! b (get pre (branch-k b)))
        [root root-row] (root-of db h b row pre)]
    (when-not (ancestor? db h b (:rev row) (:rev basis))
      (api/refuse! :conflict (str "basis " (pr-str basis) " is not on branch " b)
                   {:store/fence [:basis]}))
    (let [tick (get pre (clock-k b [:schema]))
          old  (head-world db h b tick)
          held (or (held-step db b tick) (cold-step db h b old))
          _    (when (and held orakel/on?) (orakel/held! db old (:W held) (:st held)))
          W0   (or (:W held) (derived db old))
          sig0 (or (:sig (:st held)) (sig-of db W0))
          fs   (mapv (partial canonical-fence W0) fences)]
      (check-fences! row basis (fence-reads db h b W0 sig0 root basis fs pre) fs)
      {:row row :root root :root-row root-row :old old :W0 W0 :sig0 sig0 :tick tick
       :held held})))

(defn- applied
  [old changes]
  (try (d/apply-to old changes)
       (catch clojure.lang.ExceptionInfo e
         (if (= :stale-change (err/kind e))
           (api/refuse! :conflict (ex-message e)
                        (assoc (select-keys (ex-data e) [:change :holds :section])
                               :store/fence [:schema] :stale-change true))
           (throw e)))))

(defn- stated
  [db old {:keys [changes] :as schema}]
  (cond
    (nil? schema)       nil
    (contains? schema :changes)
    (-> schema (dissoc :changes)
        (assoc :authored (applied old changes)
               :ids (res/ids-of (d/nodes changes))))
    :else               (update schema :authored (partial (:mint db) old))))

(defn- stepped-world
  [db held old W0 A changes]
  (when-not (or held (fixed? db old))
    (throw (ex-info "the head is not its own derivation"
                    {:precondition :head-not-derived})))
  (let [W   (sch/indexed A)
        sv  (:sig-view db)
        st0 (when sv (or (:st held) (sig-state db W0)))
        st  (when sv ((:step sv) st0 W0 W (d/nodes changes)))]
    (when orakel/on? (orakel/derived! db A W (if st (:sig st) (sig-of db W))))
    {:W W :st st}))

(defn- commit-world
  [db held old W0 schema]
  (let [minted (stated db old schema)
        step   (when (contains? schema :changes)
                 (stepped-world db held old W0 (:authored minted) (:changes schema)))
        W      (if step (:W step) (derived db (if minted (:authored minted) old)))]
    {:W      W
     :step   step
     :schema (some-> minted
                     (update :migration (partial mapv (partial canonical-step W0 W))))}))

(defn- commit-ops
  [db h b root W rc origin ops]
  (let [ops0   (mapv (comp (partial canonical-op W) api/check-op!) (into [] ops))
        index? (= :index origin)
        stored (read-rows db h rc (fold-targets ops0 index?))]
    {:stored stored
     :ops    (if index?
               (index-ops W ops0 stored (into {} (for [T (distinct (map :type ops0))
                                                       [id e] (edits-of db h b root T nil)]
                                                   [[T id] e])))
               ops0)}))

(defn- edited-types
  [W migration forget-edits]
  (let [forget (mapv #(canon W :object-types (canon W :link-types %)) forget-edits)]
    {:forget     forget
     :edit-types (distinct (concat (for [[k T] migration :when (attribute-steps k)] T)
                                   forget))}))

(defn- adoptions
  [db h b root rc post adopt0]
  (let [live   (when root
                 (kv-read db h (for [{:keys [type id]} adopt0
                                     :when (not (contains? post (detached-k b type)))]
                                 (edited-k root type id))))
        edited? (fn [{:keys [type id]}]
                  (or (contains? post (edited-k b type id))
                      (contains? live (edited-k root type id))))
        adopt  (into [] (remove edited?) adopt0)
        a-rows (read-rows db h rc
                          (into {} (for [[T as] (group-by :type adopt)] [T (mapv :id as)])))]
    {:adopt adopt :a-rows a-rows :restores (adopted-restores adopt a-rows)}))

(defn- unique-taken
  [db h b records]
  (into {} (for [{:keys [i coll cols vals]} (unique-claims db records)]
             [[i cols] (unique-holders db h b coll cols vals)])))

(defn- records-now
  [db h b records pre]
  (into {} (for [[i {:keys [coll key] :as w}] (map-indexed vector records)
                 :when (expected-record? w)]
             [i (record-value db h b coll key pre)])))

(defn- retired
  [db h retires pre]
  (vec (for [{:keys [branch expect-rev]} retires]
         {:branch     branch
          :row        (live! branch (get pre (branch-k branch)))
          :expect-rev expect-rev
          :dropped    (dropped-with db h branch)})))

(defn- read-for-commit
  [{:keys [now] :as db} h
   {:keys [basis schema ops records fences parents reparent origin forget-edits adopt-edits
           retires]
    :or   {origin :edit}
    :as   tx}]
  (when (and (seq adopt-edits) (seq ops))
    (api/refuse! :malformed "a commit adopts edits or writes ops, not both" {}))
  (let [b          (:branch basis)
        heads      (commit-heads parents reparent)
        pre        (kv-read db h (named-kv-keys db b heads tx))
        {:keys [row root root-row old W0 sig0 tick held]}
        (checked-basis! db h b basis fences pre)
        {:keys [schema W step]} (commit-world db held old W0 schema)
        st         (:st step)
        sig        (if st (stepped-sig db st) (sig-of db W))
        rc         (assoc (read-cx db b W sig nil nil nil) :commit? true)
        {:keys [ops stored]} (commit-ops db h b root W rc origin ops)
        {:keys [forget edit-types]} (edited-types W (:migration schema) forget-edits)
        edited-ks  (when (= :edit origin)
                     (for [{:keys [type payload]} ops] (edited-k b type (:id payload))))
        adopt0     (mapv (partial canonical-adoption W) adopt-edits)
        post       (kv-read db h (concat edited-ks
                                         (map (partial detached-k b)
                                              (distinct (concat edit-types (map :type adopt0))))
                                         (for [{:keys [type id]} adopt0] (edited-k b type id))))
        {:keys [adopt a-rows restores]} (adoptions db h b root rc post adopt0)
        detached   (into #{} (filter #(contains? post (detached-k b %))) edit-types)
        holders    (read-holders db h rc W ops)
        touching   (read-touching db h rc (deleted-by (concat ops restores)))
        kept       (when schema (kept-since db h b))
        ids        (when kept (:ids schema))
        es0        (when kept (if ids (res/only old ids) (res/entries old)))
        es         (when schema
                     (if ids (res/only (:authored schema) ids) (res/entries (:authored schema))))
        res-heads  (when-let [ids (seq (when kept (res/changed es0 es)))]
                     (into {} (for [[[_ _ k] r] (kv-read db h (map #(res/head-k b %) ids))]
                                [(res/read-id k) r])))
        all-heads  (when (res/state-due? kept) (head-revs db h b))
        rev        (spi/-next-rev (:engine db) h)]
    {:b b :row row :r0 (:rev root-row) :old old :W0 W0 :W W :tick tick :step step
     :kinded (get pre (log-kinded-k b))
     :schema schema :ops ops :stored (merge stored a-rows)
     :kept kept :es0 es0 :es es :res-heads res-heads :all-heads all-heads
     :adopt adopt :restores restores
     :holders holders :touching touching :rev rev
     :edited (when (= :edit origin) (update-keys (select-keys post edited-ks) peek))
     :unique-taken (unique-taken db h b records)
     :records-now (records-now db h b records pre)
     :heads (into {} (for [p heads] [p (:rev (live! p (get pre (branch-k p))))]))
     :retired (retired db h retires pre)
     :forget forget
     :edit-scans (into {} (for [T edit-types]
                            [T (edit-entries db h b root (contains? detached T) T)]))
     :detached detached
     :sig0 sig0 :sig sig
     :sig-touched (when st
                    (let [t (:touched (meta st))]
                      (when orakel/on? (orakel/touched! sig0 sig t))
                      t))
     :unclaimed (when schema
                  (let [kv (kv-read db h [(clock-k b dropped-tick) (clock-k b orphaned-tick)])]
                    {:dropped  (get kv (clock-k b dropped-tick))
                     :orphaned (get kv (clock-k b orphaned-tick))}))
     :schema-log (when schema
                   (let [kv (kv-read db h [(clock-k b [:schema]) (schema-logged-k b)])]
                     {:tick    (get kv (clock-k b [:schema]))
                      :through (get kv (schema-logged-k b))}))
     :layout-stamp (str (random-uuid))
     :time (java.util.Date/from ^java.time.Instant (now))}))

(defn- read-for-service
  [db h {:keys [basis records fences]}]
  (let [b   (:branch basis)
        pre (kv-read db h (keep identity
                                (concat (for [{:keys [coll key] :as w} records
                                              :when (expected-record? w)]
                                          (record-key db b coll key))
                                        (for [[k coll key] fences :when (= :record k)]
                                          (record-key db b coll key))
                                        (for [[k n] fences :when (= :lease k)] (lease-k n)))))]
    {:b            b
     :rev          (spi/-next-rev (:engine db) h)
     :records-now  (records-now db h b records pre)
     :unique-taken (unique-taken db h b records)
     :fence-reads  (into {} (for [[k x y :as f] fences]
                              [f (case k
                                   :record (record-value db h b x y pre)
                                   :lease  (get pre (lease-k x)))]))}))

(defn- plan-service
  [db {:keys [b rev records-now unique-taken fence-reads]}
   {:keys [basis records fences] :as tx}]
  (or (broken-fence nil basis fence-reads fences)
      (unique-refusal db records unique-taken)
      (let [recs (vec (map-indexed (fn [i w] (record-effect db b rev i w (get records-now i)))
                                   records))]
        (or (first (filter anomaly/refusal? recs))
            {:effects (concat (map first recs)
                              (lease-effects tx)
                              (records-clock-effects rev records recs))
             :result  {:basis basis :records (written-records records recs)}}))))

(defn- with-assigned-keys
  [result assigned]
  (cond-> result
    (seq (:records result))
    (update :records #(into [] (map-indexed (fn [i {:keys [key] :as r}]
                                              (cond-> r
                                                (= ::core/engine key) (assoc :key (assigned i)))))
                            %))))

(defn commit-in!
  [db h tx]
  (not-yet! tx)
  (lock! db h (commit-locks db tx))
  (anomaly/then
   (if (service-write? db tx)
     (plan-service db (read-for-service db h tx) tx)
     (let [{:keys [b rev step] :as in} (read-for-commit db h tx)]
       (anomaly/then (plan-commit db in tx)
                     (fn [plan] (cond-> plan step (assoc ::kept [b rev step]))))))
   (fn [{:keys [effects result] kept ::kept}]
     (let [r (with-assigned-keys result (apply! db h effects))]
       (when-let [[b rev step] kept] (keep-step! db b rev step))
       r))))

(defn dropped-with
  [{:keys [collections] :as db} h name]
  (for [[coll {:keys [drops-with kept-under]}] collections
        :when (and drops-with (not (and kept-under (.startsWith ^String name ^String kept-under))))
        :let [engine? (in-engine? db coll)
              cx      {:ns (rec-ns db coll name) :sig {} :as-of nil}
              found   (rec-list db h name coll {:where {drops-with name}} #{} true)]
        [i [k _]] (map-indexed vector found)]
    (if engine?
      [:rec-put cx coll k nil i]
      (kv-fx (conj (record-loc db coll name) (enc k)) nil))))
