(ns sutra.store.core.reads
  (:require [clojure.edn :as edn]
            [malli.core :as m]
            [sutra.store.api :as api]
            [sutra.store.lib :as lib]
            [sutra.store.memo :as memo]
            [sutra.store.spi :as spi]
            [sutra.store.values :as values]
            [sutra.types.schema :as sch]
            [sutra.store.core :as-alias core]
            [sutra.store.core.engine :refer [scan]]
            [sutra.store.core.kv :refer [authored-k clock-k enc kv-get kv-pages kv-read kv-scan
                                         rev-key]]
            [sutra.store.core.resources :as res]))

(set! *warn-on-reflection* true)

(def ^:private signature? (m/validator spi/signature-schema))

(def caches
  {::core/head     {:cap 64}
   ::core/heads    {:cap 256}
   ::core/sig      {:cap 64}
   ::core/stepped  {:cap 64}
   ::core/worlds   {:cap 32 :clears #{::core/at}}
   ::core/at       {:cap 512}
   ::core/ancestry {:cap 8192 :newest true}
   ::core/bases    {:cap 4096}
   ::core/clock    {:cap 8192}
   ::core/counts   {:cap 4096}
   ::core/rows-of  {:cap 4096}
   ::core/laid-out {:cap 64}
   ::core/flags    {:cap 8}
   ::core/nodes    {:cap 65536}})

(def fixed-by-basis
  #{:schema/world :schema/history :schema/commits :schema/removed :schema/last-changed})

(defn through
  [memo c k f]
  (memo/through memo caches c k f))

(defn- interned
  [memo k f]
  (through memo ::core/worlds k f))

(defn sig-state
  [{:keys [sig-view memo]} W]
  (through memo ::core/sig [::state W] #((:init sig-view) W)))

(defn- checked
  [{:keys [check?]} sig]
  (when-let [e (and check? (not (signature? sig)) (m/explain spi/signature-schema sig))]
    (api/refuse! :malformed "the world has no storable signature" {:explain e}))
  sig)

(defn sig-of
  [{:keys [signature sig-view memo] :as db} W]
  (through memo ::core/sig W
           #(checked db (if sig-view (:sig (sig-state db W)) (signature W)))))

(defn stepped-sig
  [db st]
  (let [sig (:sig st)
        t   (:touched (meta st))]
    (checked db (if t
                  (into {} (for [[sec ks] t] [sec (select-keys (get sig sec) ks)]))
                  sig))
    sig))

(defn held-step
  [{:keys [memo]} b tick]
  (when-let [[e] (memo/hit memo ::core/stepped b)]
    (when (and (some? tick) (= tick (:tick e))) e)))

(defn keep-step!
  [{:keys [memo]} b rev step]
  (memo/put! memo caches ::core/stepped [[b (assoc step :tick rev)]]))

(defn read-cx
  ([db ns W as-of basis layout] (read-cx db ns W (sig-of db W) as-of basis layout))
  ([_db ns W sig as-of basis layout]
   (cond-> {:ns ns :W W :sig sig :as-of as-of}
     basis  (assoc :basis basis)
     layout (assoc :layout layout))))

(declare rows links)

(defn- migrated
  [steps T r]
  (reduce (fn [r [k T' a b]]
            (if (not= T T')
              r
              (case k
                :rename-attribute (if (contains? (:values r) a)
                                    (update r :values #(-> % (dissoc a) (assoc b (get % a))))
                                    r)
                (:drop-attribute :clear-attribute) (update r :values dissoc a)
                r)))
          r steps))

(defn- based
  [c section x f]
  (let [{rc :rc} (:base c)]
    (if (and (contains? (get (:W rc) section) x) (contains? (get (:W c) section) x))
      (f rc)
      [])))

(defn rows [{:keys [engine] :as db} h c T ids]
  (if-let [{:keys [steps]} (:base c)]
    (based c :object-types T #(mapv (partial migrated steps T) (rows db h % T ids)))
    (spi/-rows engine h c T ids)))

(defn link-matches
  [{:keys [ids from to ends]}]
  (let [in (fn [k vs] (let [want (into #{} (map str) vs)] #(contains? want (k %))))]
    (cond ids  (in :id ids)
          from (in :from from)
          to   (in :to to)
          ends (let [f (in :from ends) t (in :to ends)] #(or (f %) (t %)))
          :else (constantly true))))

(defn- link-where
  [{:keys [ids from to ends]}]
  (let [in (fn [a xs] [:in a (mapv str xs)])]
    (cond ids  [(in :db/id ids)]
          from [(in :from from)]
          to   [(in :to to)]
          ends [[:or (in :from ends) (in :to ends)]]
          :else [])))

(defn- link-entries
  [{:keys [engine]} h {:keys [sig] :as c} L]
  (if-let [{:keys [holder target]} (get-in sig [:links L :fk])]
    (fn [] (map (juxt :id #(dissoc % :id))
                (lib/fk-links sig L (spi/-rows engine h c holder nil)
                              (spi/-rows engine h c target nil))))
    (fn [] (map (juxt :id #(dissoc % :id)) (spi/-links engine h c L nil)))))

(defn links
  [{:keys [engine] :as db} h {:keys [sig] :as c} L where]
  (cond
    (:base c) (based c :link-types L #(links db h % L where))
    (not (get-in sig [:links L :fk])) (spi/-links engine h c L where)
    :else
    (mapv (fn [[id l]] (assoc l :id id))
          (scan db h c {:source [:links L] :where (link-where where)} (link-entries db h c L)))))

(defn link-count
  [db h c L where]
  (scan db h c {:source [:links L] :where (link-where where) :agg {:count? true}}
        (link-entries db h c L)))

(defn type-ticks
  [sig T]
  (if-let [impls (seq (get-in sig [:interfaces T :implementers]))]
    (mapv #(vector :type %) impls)
    (if (contains? (:interfaces sig) T) [] [[:type T]])))

(defn count-ticks
  [sig L]
  (let [{:keys [fk from to]} (get-in sig [:links L])]
    (cond-> [[:schema] [:link-type L]]
      fk (into (mapcat #(type-ticks sig %) [from to])))))

(defn counted
  [{:keys [memo] :as db} h clocks c b L]
  (through memo ::core/counts [b L (mapv #(get clocks (enc %)) (count-ticks (:sig c) L))]
           #(link-count db h c L nil)))

(defn checked-entries!
  [es]
  (doseq [[[s T] {:keys [attribute-list object-type-name]}] es
          :when (= :object-types s)
          {a :attribute-name} attribute-list
          :let [p (values/ident-problem a)] :when p]
    (api/refuse! :malformed (str (or object-type-name T) "." (name a) ": " (values/explain p))
                 {:type T :attribute a :value-class (:class p)}))
  (doseq [[id v] es
          :when (not= v (try (edn/read-string (pr-str v))
                             (catch RuntimeException _ ::core/unreadable)))]
    (api/refuse! :malformed (str (pr-str id) " is not plain EDN data, so it cannot be kept")
                 {:store/rule :world-verbatim :resource id})))

(defn- parse-world
  [{:keys [memo upgrade]} t]
  (interned memo [:text t] #(sch/indexed ((or upgrade identity) (edn/read-string t)))))

(defn kept-since
  [db h b]
  (kv-get db h (res/format-k b)))

(defn- began-anew? [[_ e]] (or (:fork e) (:converted e)))

(defn- carried-heads
  [db h b {:keys [at entry revs]}]
  (when (= entry (kv-get db h (res/log-k b at)))
    (let [es (vec (kv-pages db h [:wlog b] {:after (rev-key at)}))]
      (when-not (some began-anew? es)
        (if-let [[k e] (peek es)]
          {:at (res/read-rev k) :entry e :revs (reduce res/redo revs (map second es))}
          {:at at :entry entry :revs revs})))))

(defn remember-heads!
  [{:keys [memo]} b m]
  (memo/put! memo caches ::core/heads [[b m]]))

(defn forget-heads!
  [{:keys [memo]}]
  (swap! memo dissoc ::core/heads))

(defn head-revs
  [{:keys [memo] :as db} h b]
  (let [[old] (memo/hit memo ::core/heads b)]
    (if-let [m (some->> old (carried-heads db h b))]
      (do (when-not (identical? (:revs m) (:revs old)) (remember-heads! db b m))
          (:revs m))
      (let [revs  (into {} (for [[k r] (kv-pages db h [:whead b] {})] [(res/read-id k) r]))
            [k e] (first (kv-scan db h [:wlog b]
                                  {:upto (rev-key Long/MAX_VALUE) :reverse? true :limit 1}))]
        (when k (remember-heads! db b {:at (res/read-rev k) :entry e :revs revs}))
        revs))))

(defn fetch
  [{:keys [memo] :as db} h idrs]
  (let [known (into {} (keep (fn [x] (when-let [[v] (memo/hit memo ::core/nodes x)] [x v])))
                    idrs)
        miss  (into [] (remove known) idrs)
        got   (when (seq miss) (kv-read db h (map (fn [[id r]] (res/node-k id r)) miss)))
        fresh (for [[id r :as x] miss]
                (if-let [[v] (get got (res/node-k id r))]
                  [x v]
                  (api/refuse! :malformed (str "no version of " (pr-str id) " at " r)
                               {:resource id :rev r})))]
    (when (seq fresh) (memo/put! memo caches ::core/nodes fresh))
    (into known fresh)))

(defn- versions
  [db h revs]
  (into {} (map (fn [[[id _] v]] [id v])) (fetch db h (vec revs))))

(defn- kept-world
  [{:keys [memo upgrade] :as db} h revs]
  (interned memo [:revs revs]
            #(sch/indexed ((or upgrade identity) (res/world (versions db h revs))))))

(defn head-world
  [{:keys [memo head] :as db} h b tick]
  (let [own  #(cond
                (kept-since db h b) (kept-world db h (head-revs db h b))
                :else (if-let [t (kv-get db h (authored-k b))] (parse-world db t) {}))
        read (if head #(sch/indexed (head db h b own)) own)]
    (if (some? tick) (through memo ::core/head [b tick] read) (read))))

(declare world-at)

(defn- text-world-at
  [db h b rev]
  (if-let [[_
            v] (first (kv-scan db h [:schema b] {:upto (rev-key rev) :reverse? true :limit 1}))]
    (if (map? v) (world-at db h (:from v) (:rev v)) (parse-world db v))
    (let [[_
           oldest] (first (kv-scan db h [:schema b] {:upto (rev-key Long/MAX_VALUE) :limit 1}))]
      (cond
        (map? oldest) (world-at db h (:from oldest) rev)
        (and (nil? oldest) (kv-get db h (authored-k b)))
        (api/refuse! :as-of-unsupported
                     (str "branch " b
                          " keeps no world history from before revision history was kept")
                     {:branch b :rev rev})
        :else {}))))

(defn state-at
  [db h b rev]
  (if-let [[ck st] (first (kv-scan db h [:wstate b] {:upto (rev-key rev) :reverse? true
                                                    :limit 1}))]
    (reduce res/redo st (map second (kv-scan db h [:wlog b] {:after ck :upto (rev-key rev)})))
    (let [[k e] (first (kv-scan db h [:wlog b] {:limit 1}))]
      (when-let [{f :from r :rev} (:fork e)]
        (when (<= (res/read-rev k) rev)
          (when-let [st (state-at db h f r)]
            (reduce res/redo st
                    (map second (kv-scan db h [:wlog b] {:after k :upto (rev-key rev)})))))))))

(defn- kept-world-at
  [db h b rev]
  (if-let [st (state-at db h b rev)]
    (kept-world db h st)
    (let [[_ e] (first (kv-scan db h [:wlog b] {:limit 1}))]
      (if (:fork e)
        (world-at db h (get-in e [:fork :from]) rev)
        (text-world-at db h b rev)))))

(defn world-at
  ([db h b] (head-world db h b (kv-get db h (clock-k b [:schema]))))
  ([db h b rev]
   (cond
     (nil? rev)          (world-at db h b)
     (kept-since db h b) (kept-world-at db h b rev)
     :else               (text-world-at db h b rev))))

(defn changed-since
  [db h b rev upto]
  (when-let [{:keys [since]} (kept-since db h b)]
    (when (if rev
            (>= rev since)
            (:fork (second (first (kv-scan db h [:wlog b] {:limit 1})))))
      (into #{} (mapcat (comp keys :changes second))
            (kv-pages db h [:wlog b] (cond-> {:after (rev-key (or rev since))}
                                       upto (assoc :upto (rev-key upto))))))))

(defn resource-history
  [db h b rev id]
  (when (kept-since db h b)
    (let [pre    (str (enc id) "|")
          own    (for [[k [from to]] (kv-scan db h [:whist b]
                                              (cond-> (res/hist-range id)
                                                rev (assoc :upto (str pre (rev-key rev)))))]
                   {:rev (res/read-rev (subs k (count pre))) :from from :to to})
          vals   (fetch db h (for [{:keys [to]} own :when to] [id to]))
          [_ e]  (first (kv-scan db h [:wlog b] {:limit 1}))
          parent (when-let [{f :from r :rev} (:fork e)]
                   (resource-history db h f (if rev (min rev r) r) id))]
      (into (vec (reverse (for [{:keys [to] :as v} own]
                            (assoc v :value (when to (get vals [id to]))))))
            parent))))

(defn derived
  [{:keys [derive memo]} W]
  (interned memo [:derived W] #(sch/indexed (derive W))))

(defn fixed?
  [{:keys [memo] :as db} A]
  (interned memo [:fixed A] #(= A (derived db A))))

(defn cold-step
  [{:keys [held-head] :as db} h b old]
  (when held-head
    (held-head db h b old
               #(let [W (derived db old)]
                  {:W W :st (when (:sig-view db) (sig-state db W)) :fixed? (fixed? db old)}))))
