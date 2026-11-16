(ns ^:unit platform.merge-heads-test
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
            [sutra.store.world :as world]
            [support.merge-orakel :as orakel]))

(def ^:private actor {:principal "t"})

(defn- head [db b] (with-open [s (api/snapshot db {:branch b})] (api/basis s)))

(defn- commit! [db b W]
  (api/transact! db {:basis (head db b) :actor actor :schema {:authored W :migration []}}))

(defn- world-of [db b]
  (with-open [s (api/snapshot db {:branch b})]
    (with-meta (reads/world-at (:db s) (:h s) b) nil)))

(def ^:private org "ri.sutra.main.organization.o")

(defn- grounded
  [W]
  (let [p "ri.sutra.main.project.pp" t "ri.sutra.main.object-type.tt"]
    (-> W
        (update :organizations #(or % {}))
        (assoc-in [:projects p] {:id p :project-name :pp :markings [:m0]})
        (assoc-in [:object-types t] {:id t :object-type-name :tt :project :pp
                                       :attribute-list []}))))

(defn- nth-key [W sec n] (first (drop (mod n 5) (keys (get W sec)))))

(defn- edited
  [W n]
  (if-let [k (nth-key W :object-types n)]
    (assoc-in W [:object-types k :description] (str "d" (mod n 3)))
    W))

(defn- guarded
  [W n]
  (let [p (nth-key W :projects n) t (nth-key W :object-types n) s (nth-key W :spaces n)]
    (case (int (mod n 6))
      0 (cond-> W p (assoc-in [:projects p :markings] [(keyword (str "m" (mod n 2)))]))
      1 (cond-> W p (update-in [:projects p] dissoc :markings))
      2 (cond-> W t (assoc-in [:object-types t :markings] [:m1]))
      3 (cond-> W p (assoc-in [:projects p :grants]
                              [{:principal (str "u" (mod n 2)) :role :ontology-editor}]))
      4 (assoc-in W [:organizations org]
                  {:id org :organization-name :o :members ["u1"] :apply ["u1"]
                   :expand-access ["u2"]})
      5 (cond-> (update W :organizations dissoc org)
          s (assoc-in [:spaces s :organization] :o)))))

(def ^:private commit
  (gen/frequency [[1 (gen/fmap (fn [W] [:world (grounded W)]) g/world)]
                  [3 (gen/fmap (fn [n] [:edit n]) gen/nat)]
                  [5 (gen/fmap (fn [n] [:access n]) gen/nat)]]))

(defn- history!
  [W0 mains fs]
  (let [db (mem/store mem/empty-seed {:upgrade ri/rekey-legacy})]
    (commit! db "main" (grounded W0))
    (api/branch! db {:op :fork :name "f" :from (head db "main")})
    (doseq [[b [kind x]] (interleave (map vector (repeat "main") mains)
                                     (map vector (repeat "f") fs))]
      (commit! db b (case kind
                      :world  x
                      :edit   (edited (world-of db b) x)
                      :access (guarded (world-of db b) x))))
    {:db db}))

(defn- entries [W]
  (into {} (for [[s es] W :when (map? es) [k e] es] [[s k] e])))

(defn- check
  [store]
  (try (merge/staging-merge-check store "main" "f")
       (catch clojure.lang.ExceptionInfo e (when-not (:precondition (ex-data e)) (throw e)))))

(defn- merged
  [store]
  (when-let [c (check store)]
    (let [{:keys [base mine-schema theirs-schema scope]} (orakel/worlds store "main" "f")]
      [(orakel/rebase base mine-schema theirs-schema {} scope)
       (merge/rebase-schema (merge/heads-merge (:heads c) {}))])))

(def ^:private history
  (gen/tuple g/world (gen/vector commit 0 3) (gen/vector commit 1 4)))

(defspec the-merge-on-the-heads-is-the-merge-of-the-worlds 100
  (prop/for-all [[W0 mains fs] history]
    (let [[old new] (merged (history! W0 mains fs))]
      (= (entries old) (entries new)))))

(defspec the-worlds-of-the-heads-are-those-of-the-store 100
  (prop/for-all [[W0 mains fs] history]
    (let [store (history! W0 mains fs)
          ws    #(update-vals (select-keys % [:base :mine-schema :theirs-schema]) entries)
          c     (check store)]
      (or (nil? c) (= (ws (orakel/worlds store "main" "f")) (ws c))))))

(deftest the-check-reads-no-world
  (let [cs (for [[W0 mains fs] (gen/sample history 40)
                 :let [store (history! W0 mains fs)]]
             (with-redefs [world/world (fn [& _] (throw (ex-info "a world was read" {})))]
               (check store)))]
    (is (some :heads cs))))

(defn- checked
  [store]
  (when-let [c (check store)]
    (let [{:keys [base mine-schema theirs-schema scope]} (orakel/worlds store "main" "f")]
     [(let [chk (orakel/check base mine-schema theirs-schema scope)]
        {:check chk
         :rows  (sort-by (juxt :table :status) (orakel/rows (:mine chk) base mine-schema))})
      {:check (select-keys c [:status :conflicts :mine])
       :rows  (sort-by (juxt :table :status) (:diff (merge/preview-of c)))}])))

(defspec the-check-and-preview-on-the-heads-are-those-of-the-worlds 100
  (prop/for-all [[W0 mains fs] history]
    (let [[old new] (checked (history! W0 mains fs))]
      (= old new))))

(defn- judged
  [store]
  (let [{:keys [heads status theirs-schema]} (check store)]
    (when (and heads (= :clean status))
      (let [cut (fn [{:keys [ok error findings]}]
                  {:ok (some-> ok entries) :error? (some? error)
                   :findings (frequencies findings)})]
        [(cut (orakel/verdict (merge/rebase-schema (merge/heads-merge heads {})) theirs-schema))
         (cut (merge/judged (merge/heads-merge heads {})))]))))

(defspec the-verdict-on-the-heads-is-the-verdict-of-the-world 100
  (prop/for-all [[W0 mains fs] history]
    (let [[old new] (judged (history! W0 mains fs))]
      (= old new))))
