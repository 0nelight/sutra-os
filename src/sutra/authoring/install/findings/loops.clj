(ns sutra.authoring.install.findings.loops
  (:require [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.world-columns :as world-columns]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.transforms :as transforms]
            [sutra.world.index :as ix]
            [sutra.types.schema :as sch]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(defn- cycle-of [core] (when (seq core) {:rule :transform-cycle :cycle (vec (sort-by str core))}))

(defn- own-reads [ts ks]
  (for [k ks :let [t (live/lookup ts k)] :when t r (transforms/own-output-reads k t)]
    (assoc r :rule :transform-reads-own-output)))

(defn- sides
  [d]
  (let [{:keys [names loops]} (diff/index d)
        cur   (get-in d [:current :transforms])
        tb    (get-in d [:before :transforms])
        ta    (get-in d [:after :transforms])
        cs    (diff/changed d :transform)
        st    (loops)
        nm    (fn [r] (or (:transform-name (live/lookup cur r)) r))
        rids  (fn [n] (distinct (concat (filter #(= n (:transform-name (live/lookup cur %)))
                                                (names :transforms (ix/nm n)))
                                        (when (and (live/holds? cur n)
                                                   (nil? (:transform-name (live/lookup cur n))))
                                          [n]))))
        rid   (fn [n] (last (sort-by (juxt str (comp str type)) (rids n))))
        shadow (volatile! false)
        named (fn [rs] (into #{} (keep (fn [r] (let [n (nm r) w (rid n)]
                                                  (if (or (= r n) (nil? w) (= r w))
                                                    n
                                                    (do (vreset! shadow true) nil)))))
                             rs))
        held  (fn [part sig]
                (let [[pre n] (world-columns/referent sig)]
                  (named (mapcat #(get-in st [:aux part (conj pre %)])
                                 (live/keys-named names n)))))
        emit  (fn [sig] (into (held :emit sig) cs))
        look  (fn [ts]
                {:producers         (fn [kind target]
                                      (for [u (sort-by str (emit [:out kind target]))
                                            :let [t (live/lookup ts u)] :when t
                                            o (outputs/outputs-of t)
                                            :when (and (= kind (:kind o))
                                                       (= target (get o kind)))]
                                        [u t o]))
                 :dataset-producers (fn [ds]
                                      (into #{}
                                            (filter #(let [t (live/lookup ts %)]
                                                       (some #{ds} (keep :dataset
                                                                         (cons t (outputs/outputs-of
                                                                                  t))))))
                                            (when (some? ds) (emit [:ds ds]))))
                 :media-producers   (fn [m]
                                      (filter #(some (fn [o] (= m (:media-set o)))
                                                     (vals (:media-outputs (live/lookup ts %))))
                                              (emit [:media m])))
                 :link              (constantly nil)})
        supp  (fn [ts]
                (fn [ot]
                  (into #{} (filter (fn [u] (some #(and (transforms/supplements? %)
                                                        (= ot (:object-type %)))
                                                  (outputs/outputs-of (live/lookup ts u)))))
                        (emit [:out :object-type ot]))))
        deps  (fn [ts k] (when-let [t (live/lookup ts k)]
                           (into #{} (filter #(some? (live/lookup ts %)))
                                 (transforms/deps-with (look ts) t))))
        sched (fn [ts k] (when-let [t (live/lookup ts k)]
                           (transforms/schedule-with (supp ts) k t (deps ts k))))
        lsn   (fn [t] (when t (mapcat #(held :listen %) (world-columns/emits t))))
        moved (into (set cs)
                    (mapcat (fn [c] (concat (lsn (live/lookup tb c)) (lsn (live/lookup ta c))
                                            (held :listen [:t c]))))
                    cs)
        s0m   (into {} (for [k moved] [k (sched tb k)]))
        s1m   (into {} (for [k moved] [k (sched ta k)]))
        d1m   (into {} (for [k moved] [k (deps ta k)]))
        hs    (fn [x] (some->> (rid x) (get (get-in st [:graph :schedule])) named))
        s0    (fn [x] (if (contains? s0m x) (s0m x) (hs x)))
        s1    (fn [x] (if (contains? s1m x) (s1m x) (hs x)))
        down  (fn [x] (concat (some->> (rid x) (get (get-in st [:graph :rdeps])) named)
                              (for [[k ds] d1m :when (contains? ds x)] k)))
        left0 (named (get-in st [:graph :cycle]))
        core0 (named (:core st))
        ss    (filter #(not= (s0 %) (s1 %)) moved)
        {:keys [gone corez]} (world-columns/core-moved s0 s1 down left0
                                                      #(some? (live/lookup ta %)) ss)
        core1 (into (reduce disj core0 gone) corez)
        side  (fn [ts core] (set (concat (own-reads ts cs) (some-> (cycle-of core) vector))))
        sides {:before (side tb core0) :after (side ta core1)}]
    (if @shadow
      (let [whole (fn [ts] (set (transforms/loop-problems (sch/indexed {:transforms (live/whole ts)}))))]
        {:before (whole tb) :after (whole ta)})
      sides)))

(defn- say [f] (apply refusal/message (transforms/loop-refusal (fn [_ ref] (name ref)) f)))

(def loops
  {:find (fn [d]
           (when (seq (diff/changed d :transform))
             (let [{:keys [before after]} (sides d)]
               (for [p (remove before after)]
                 (assoc p :entity-class :transforms
                        :entity (or (:transform p) (first (:cycle p))))))))
   :says {:transform-reads-own-output say
          :transform-cycle            say}})
