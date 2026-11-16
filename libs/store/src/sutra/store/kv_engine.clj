(ns sutra.store.kv-engine
  (:require [sutra.store.kv :as kv]
            [sutra.store.kv-plan :as plan]
            [sutra.store.spi :as spi]))

(set! *warn-on-reflection* true)

(defn- write! [s h writes] (when (seq writes) (kv/-write! s h writes)))

(defn- meta-of [s sp h ns]
  (let [k (plan/meta-key sp ns)] (get (kv/-read s h [k]) k)))

(defn- hist [s sp h sec ns K ids]
  (if (nil? ids)
    (into {} (kv/-scan s h (sec sp) (plan/part ns K) {}))
    (plan/by-key (kv/-read s h (plan/hist-keys sp sec ns K ids)))))

(defn- kinds [s sp h sec ns] (plan/own-kinds ns (kv/-parts s h (sec sp))))

(defn- keys-of [s sp h sec ns K]
  (map first (kv/-scan s h (sec sp) (plan/part ns K) {:keys-only? true})))

(defn- section [s sp h sec ns K as-of ids]
  (let [root (plan/root-of (meta-of s sp h ns) K as-of)]
    (plan/section (hist s sp h sec ns K ids) (when root (hist s sp h sec root K ids)) as-of)))

(defn- append! [s sp h sec ns rev K entries]
  (when (seq entries)
    (write! s h (plan/appended sp sec ns K rev (hist s sp h sec ns K (map first entries))
                               entries))))

(defn- clear! [s sp h sec ns K]
  (write! s h (plan/cleared sp sec ns K (keys-of s sp h sec ns K))))

(defn- detach! [s sp h ns rev sec K]
  (let [m (meta-of s sp h ns)]
    (write! s h (plan/detached sp sec ns K rev m (hist s sp h sec ns K nil)
                               (when (:root m) (hist s sp h sec (:root m) K nil))))))

(defn- layout! [s sp h {:keys [ns rev]} {:keys [new steps]}]
  (doseq [[k T :as st] steps]
    (case k
      :drop-unclaimed   (doseq [[sec held] [[:objects :types] [:links :links]]
                                K (kinds s sp h sec ns)
                                :when (not (contains? (get new held) K))]
                          (clear! s sp h sec ns K))
      :detach-type      (detach! s sp h ns rev :objects T)
      :detach-link-type (detach! s sp h ns rev :links T)
      (:rename-attribute :drop-attribute :clear-attribute)
      (write! s h (plan/revised sp ns rev T (hist s sp h :objects ns T nil) (plan/revision st)))
      :drop-type        (clear! s sp h :objects ns T)
      :drop-link-type   (clear! s sp h :links ns T)
      nil)))

(defn- fork! [s sp h from to]
  (let [m    (meta-of s sp h from)
        held (when (:root m)
               (into {} (for [sec [:objects :links] K (kinds s sp h sec from)]
                          [[sec K] (kv/-scan s h (sec sp) (plan/part from K) {})])))]
    (write! s h (plan/forked sp from to m held))))

(defn- drop! [s sp h ns]
  (write! s h (plan/dropped sp ns (into {} (for [sec [:objects :links] K (kinds s sp h sec ns)]
                                             [[sec K] (keys-of s sp h sec ns K)])))))

(defrecord KvEngine [s sp]
  spi/Engine
  (-profile [_]
    {:driver :kv :history? true :consistency (:consistency (kv/-profile s) :transactional)
     :reads #{:self} :semantics-overrides {}})
  (-begin [_ mode lane] (kv/-begin s mode lane))
  (-lock! [_ h ks] (kv/-lock! s h ks))
  (-next-rev [_ h] (kv/-next-rev s h))
  (-commit! [_ h] (kv/-commit! s h))
  (-rollback! [_ h] (kv/-rollback! s h))
  (-kv-read [_ h ks] (kv/-read s h ks))
  (-kv-write! [_ h writes] (kv/-write! s h writes))
  (-kv-scan [_ h space p opts] (kv/-scan s h space p opts))
  (-kv-parts [_ h space] (kv/-parts s h space))
  (-ns-exists? [_ h ns] (plan/exists? ns (meta-of s sp h ns)))
  (-ns-fork! [_ h from to _cx] (fork! s sp h from to))
  (-ns-drop! [_ h ns] (drop! s sp h ns))
  (-layout! [_ h cx change] (layout! s sp h cx change))
  (-inspect [_ h {:keys [ns sig]}]
    (plan/inspect (meta-of s sp h ns) {:objects (kinds s sp h :objects ns)
                                       :links   (kinds s sp h :links ns)} sig))
  (-rows [_ h {:keys [ns as-of sig]} T ids]
    (plan/rows sig T ids (section s sp h :objects ns T as-of ids)))
  (-links [_ h {:keys [ns as-of]} L where]
    (plan/links where (section s sp h :links ns L as-of (:ids where))))
  (-write-rows! [_ h {:keys [ns rev]} T rows]
    (append! s sp h :objects ns rev T (map (juxt :id identity) rows)))
  (-delete-rows! [_ h {:keys [ns rev]} T ids]
    (append! s sp h :objects ns rev T (map vector ids (repeat nil))))
  (-write-links! [_ h {:keys [ns rev]} L links]
    (append! s sp h :links ns rev L (map (juxt :id identity) links)))
  (-delete-links! [_ h {:keys [ns rev]} L ids]
    (append! s sp h :links ns rev L (map vector ids (repeat nil)))))

(defn engine
  ([s] (engine s "sutra.kv"))
  ([s prefix] (->KvEngine s (plan/spaces prefix))))
