(ns ^:unit platform.merge-home-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.governance.merge :as merge]
            [sutra.world.gen :as g]
            [sutra.ref.install :as ri]
            [sutra.store.api :as api]
            [sutra.store.core.reads :as reads]
            [sutra.store.mem :as mem]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.types.entities :as ents]
            [support.merge-orakel :as orakel]))

(def ^:private actor {:principal "t"})

(defn- head [db b] (with-open [s (api/snapshot db {:branch b})] (api/basis s)))

(defn- commit! [db b W]
  (api/transact! db {:basis (head db b) :actor actor :schema {:authored W :migration []}}))

(defn- world-of [db b]
  (with-open [s (api/snapshot db {:branch b})]
    (with-meta (reads/world-at (:db s) (:h s) b) nil)))

(defn- rid [tag n] (str "ri.sutra.main." tag "." n))

(defn- sync-of [n ds]
  {:id (rid "transform" (str "s" n)) :transform-name (keyword (str "sync" n))
   :input {:kind :datasource :datasource (rid "source" (str "q" (mod n 2)))}
   :output {:kind :dataset :dataset ds}})

(defn- instanced
  [W n]
  (let [s  (rid "source" (str "q" (mod n 2)))
        t  (rid "transform" (str "s" (mod n 3)))
        ds (rid "dataset" (str "d" (mod n 4)))]
    (case (int (mod n 6))
      0 (assoc-in W [:sources s] {:id s :source-name (keyword (str "q" (mod n 2))) :type :jdbc})
      1 (update W :sources dissoc s)
      2 (-> W
            (assoc-in [:transforms t] (sync-of (mod n 3) ds))
            (assoc-in [:datasets ds] {:id ds :dataset-name (keyword (str "d" (mod n 4)))}))
      3 (cond-> W (get-in W [:transforms t]) (assoc-in [:transforms t :output :dataset] ds))
      4 (update W :transforms dissoc t)
      5 (if-let [k (first (keys (:object-types W)))]
          (assoc-in W [:object-types k :description] (str "d" (mod n 3)))
          (assoc-in W [:datasets ds] {:id ds :dataset-name (keyword (str "d" (mod n 4)))
                                      :description "x"})))))

(defn- grounded
  [W]
  (merge {:sources {} :transforms {} :datasets {} :object-types {}} W))

(def ^:private commit
  (gen/frequency [[1 (gen/fmap (fn [W] [:world (grounded W)]) g/world)]
                  [5 (gen/fmap (fn [n] [:instance n]) gen/nat)]]))

(defn- history!
  [W0 mains ps fs]
  (let [db    (mem/store mem/empty-seed {:upgrade ri/rekey-legacy})
        store {:db db :instance-keys ents/instance-keys
               :instance-home #(branch-kinds/instance-home "main" %)}
        on!   (fn [b [kind x]]
                (commit! db b (case kind
                                :world    x
                                :instance (instanced (world-of db b) x))))]
    (commit! db "main" (grounded W0))
    (api/branch! db {:op :fork :name "p" :from (head db "main")})
    (on! "p" [:instance 2])
    (api/branch! db {:op :fork :name "f" :from (head db "p")})
    (doseq [[b c] (interleave (map vector (repeat "main") mains)
                              (map vector (repeat "p") ps)
                              (map vector (repeat "f") fs))]
      (on! b c))
    store))

(defn- entries [W]
  (into {} (for [[s es] W :when (map? es) [k e] es] [[s k] e])))

(def ^:private history
  (gen/tuple g/world (gen/vector commit 1 3) (gen/vector commit 1 3) (gen/vector commit 1 4)))

(defn- check
  [store]
  (try (merge/staging-merge-check store "p" "f")
       (catch clojure.lang.ExceptionInfo e (when-not (:precondition (ex-data e)) (throw e)))))

(defspec the-worlds-of-homed-heads-are-those-of-the-store 100
  (prop/for-all [[W0 mains ps fs] history]
    (let [store (history! W0 mains ps fs)
          ws    #(update-vals (select-keys % [:base :mine-schema :theirs-schema]) entries)
          c     (check store)]
      (or (nil? c) (= (ws (orakel/worlds store "p" "f")) (ws c))))))

(defspec the-merge-on-homed-heads-is-the-merge-of-the-worlds 100
  (prop/for-all [[W0 mains ps fs] history]
    (let [store (history! W0 mains ps fs)
          c     (check store)
          {:keys [base mine-schema theirs-schema scope]} (when c (orakel/worlds store "p" "f"))]
      (or (nil? c)
          (= (entries (orakel/rebase base mine-schema theirs-schema {} scope))
             (entries (merge/rebase-schema (merge/heads-merge (:heads c) {}))))))))

(defspec the-check-and-preview-on-homed-heads-are-those-of-the-worlds 100
  (prop/for-all [[W0 mains ps fs] history]
    (let [store (history! W0 mains ps fs)
          c     (check store)
          {:keys [base mine-schema theirs-schema scope]} (when c (orakel/worlds store "p" "f"))
          rows  #(sort-by (juxt :table :status) %)]
      (or (nil? c)
          (let [chk (orakel/check base mine-schema theirs-schema scope)]
            (= {:check chk :rows (rows (orakel/rows (:mine chk) base mine-schema))}
               {:check (select-keys c [:status :conflicts :mine])
                :rows  (rows (:diff (merge/preview-of c)))}))))))

(deftest homed-merges-read-the-heads-and-the-instance
  (let [cs (for [[W0 mains ps fs] (gen/sample history 60)
                 :let [store (history! W0 mains ps fs)
                       c     (check store)]]
             c)]
    (is (< 15 (count (filter :heads cs))))
    (is (some (fn [c] (some #(ents/instance-section? (first %)) (get-in c [:heads :nodes])))
              cs))
    (is (some (fn [c] (some #(= :datasets (first %)) (get-in c [:heads :nodes]))) cs))))

(defn- judged
  [store]
  (let [{:keys [theirs-schema heads status]} (check store)]
    (when (and heads (= :clean status))
      (let [cut (fn [{:keys [ok error findings]}]
                  {:ok (some-> ok entries) :error? (some? error)
                   :findings (frequencies findings)})]
        [(cut (orakel/verdict (merge/rebase-schema (merge/heads-merge heads {})) theirs-schema))
         (cut (merge/judged (merge/heads-merge heads {})))]))))

(defspec the-verdict-on-homed-heads-is-the-verdict-of-the-world 100
  (prop/for-all [[W0 mains ps fs] history]
    (let [[old new] (judged (history! W0 mains ps fs))]
      (= old new))))
