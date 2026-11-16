(ns sutra.commits.branches-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.world.db :as kdb]
            [sutra.world.gen :as g]
            [sutra.ref.install :as ri]
            [sutra.commits.branches :as zweige]
            [sutra.store.api :as api]
            [sutra.store.core.reads :as reads]
            [sutra.store.mem :as mem]))

(def ^:private actor {:principal "t"})

(defn- store [] (mem/store mem/empty-seed {:upgrade ri/rekey-legacy}))

(defn- head [db b] (with-open [s (api/snapshot db {:branch b})] (api/basis s)))

(defn- commit! [db b W]
  (api/transact! db {:basis (head db b) :actor actor :schema {:authored W :migration []}}))

(def ^:private world
  (gen/let [W g/world
            legacy (gen/frequency [[4 (gen/return nil)] [1 (gen/return :old-a)]])
            gone (gen/elements [nil nil nil :projects])]
    (cond-> W
      legacy (assoc-in [:object-types legacy] {:object-type-name legacy})
      gone   (dissoc gone))))

(defn- edited
  [W n]
  (if-let [k (first (drop (mod n 5) (keys (:object-types W))))]
    (assoc-in W [:object-types k :description] (str "d" n))
    W))

(def ^:private commit
  (gen/frequency [[1 (gen/fmap (fn [W] [:world W]) world)]
                  [2 (gen/fmap (fn [n] [:edit n]) gen/nat)]]))

(defn- commit-on! [db b [kind x]]
  (let [W (with-open [s (api/snapshot db {:branch b})]
            (with-meta (reads/world-at (:db s) (:h s) b) nil))]
    (commit! db b (if (= :world kind) x (edited W x)))))

(defn- entries [W]
  (into {} (for [[s es] W :when (map? es) [k e] es] [[s k] e])))

(defn- states-or-unmet
  [d h rev]
  (try (zweige/states d h "f" "main" rev)
       (catch clojure.lang.ExceptionInfo e
         (when-not (:precondition (ex-data e)) (throw e)))))

(defn- holds? [db]
  (let [rev (get-in (with-open [s (api/snapshot db {:branch "main"})]
                      (api/read s nil [:branch/get "f"]))
                    [:base :rev])]
    (with-open [s (api/snapshot db {:branch "main"})]
      (let [[d h] [(:db s) (:h s)]
            read  #(entries (with-meta (reads/world-at d h %1 %2) nil))
            [B M T] [(read "main" rev) (read "f" nil) (read "main" nil)]]
        (if-let [{:keys [mine theirs base nodes]} (states-or-unmet d h rev)]
          (and (= M (entries (kdb/world mine)))
               (= T (entries (kdb/world theirs)))
               (every? #(= (get B %) (kdb/entry base %)) nodes)
               (every? (fn [n] (= (get B n) (get M n) (get T n)))
                       (remove (set nodes) (distinct (concat (keys B) (keys M) (keys T))))))
          ::unmet)))))

(defspec the-merge-reads-what-the-worlds-hold 200
  (prop/for-all [W0 world mains (gen/vector commit 0 3) fs (gen/vector commit 0 3)]
    (let [db (store)]
      (commit! db "main" W0)
      (api/branch! db {:op :fork :name "f" :from (head db "main")})
      (doseq [[b c] (interleave (map vector (repeat "main") mains)
                                (map vector (repeat "f") fs))]
        (commit-on! db b c))
      (let [r (holds? db)] (or (true? r) (= ::unmet r))))))

(deftest most-histories-are-read-by-their-nodes
  (let [db-of (fn [[W0 mains fs]]
                (let [db (store)]
                  (commit! db "main" W0)
                  (api/branch! db {:op :fork :name "f" :from (head db "main")})
                  (doseq [[b c] (interleave (map vector (repeat "main") mains)
                                            (map vector (repeat "f") fs))]
                    (commit-on! db b c))
                  db))
        rs (map (comp holds? db-of)
                (gen/sample (gen/tuple world (gen/vector commit 0 3) (gen/vector commit 0 3)) 100))]
    (is (every? #{true ::unmet} rs))
    (is (< 40 (count (filter true? rs))) "an unmet precondition stays the exception")))

(deftest a-fork-of-small-edits-is-read-by-its-nodes
  (let [db (store)
        W  (g/installed (for [n [:a :b :c]] [:create :object-type {:name n}]))
        [x y] (keys (:object-types W))]
    (commit! db "main" W)
    (api/branch! db {:op :fork :name "f" :from (head db "main")})
    (commit! db "f" (assoc-in W [:object-types x :description] "f"))
    (commit! db "main" (assoc-in W [:object-types y :description] "main"))
    (is (true? (holds? db)))
    (with-open [s (api/snapshot db {:branch "main"})]
      (let [rev (get-in (api/read s nil [:branch/get "f"]) [:base :rev])]
        (is (= #{[:object-types x] [:object-types y]}
               (:nodes (zweige/states (:db s) (:h s) "f" "main" rev))))))))
