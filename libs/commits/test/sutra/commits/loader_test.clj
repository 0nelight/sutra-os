(ns sutra.commits.loader-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.world.db :as kdb]
            [sutra.world.gen :as g]
            [sutra.world.install :as k]
            [sutra.ref.install :as ri]
            [sutra.commits.loader :as lader]
            [sutra.store.api :as api]
            [sutra.store.core.reads :as reads]
            [sutra.store.mem :as mem]))

(def ^:private actor {:principal "t"})

(defn- store [] (mem/store mem/empty-seed {:upgrade ri/rekey-legacy}))

(defn- head [db b] (with-open [s (api/snapshot db {:branch b})] (api/basis s)))

(defn- branches [db] (with-open [s (api/snapshot db {:branch "main"})]
                       (into #{} (map :name) (api/read s nil [:branch/list]))))

(defn- commit! [db b W]
  (api/transact! db {:basis (head db b) :actor actor :schema {:authored W :migration []}}))

(defn- read-both
  [db b]
  (with-open [s (api/snapshot db {:branch b})]
    [(lader/head-db (:db s) (:h s) b k/views)
     (with-meta (reads/world-at (:db s) (:h s) b) nil)]))

(defn- agrees? [kept W]
  (let [fresh (kdb/db W k/views)]
    (and (= W (kdb/world kept))
         (= (:idx fresh) (:idx kept))
         (= (:views fresh) (:views kept)))))

(def ^:private legacy
  (gen/let [n (gen/elements [:old-a :old-b]) d (gen/elements ["x" "y"])]
    [[:object-types n] {:object-type-name n :description d}]))

(def ^:private world
  (gen/let [W g/world
            ls (gen/vector legacy 0 2)
            frame (gen/elements [nil :empty :gone])]
    (cond-> (reduce (fn [W [node e]] (assoc-in W node e)) W ls)
      (= :empty frame) (assoc :value-types {})
      (= :gone frame)  (dissoc :projects))))

(def ^:private op
  (gen/frequency
   [[6 (gen/let [b (gen/elements ["main" "f" "g"]) W world] [:commit b W])]
    [2 (gen/let [b (gen/elements ["f" "g"]) from (gen/elements ["main" "f" "g"])]
         [:fork b from])]
    [1 (gen/let [b (gen/elements ["main" "f" "g"])] [:read b])]]))

(defn- step! [db [verb b x]]
  (let [bs (branches db)]
    (case verb
      :commit (when (bs b) (commit! db b x))
      :fork   (when (and (bs x) (not (bs b)))
                (api/branch! db {:op :fork :name b :from (head db x)}))
      :read   nil)))

(defspec a-carried-db-equals-the-world-read-whole 60
  (prop/for-all [ops (gen/vector op 1 12)]
    (let [db (store)]
      (every? (fn [o]
                (step! db o)
                (every? #(apply agrees? (read-both db %)) (branches db)))
              ops))))

(deftest a-read-after-one-commit-fetches-that-commit
  (let [db (store)
        W  (g/installed (for [n [:a :b :c]] [:create :object-type {:name n}]))
        ot (first (keys (:object-types W)))]
    (commit! db "main" W)
    (read-both db "main")
    (commit! db "main" (assoc-in W [:object-types ot :description] "changed"))
    (let [asked (atom [])
          fetch reads/fetch]
      (with-redefs [reads/fetch     (fn [s h idrs] (swap! asked into idrs) (fetch s h idrs))
                    reads/world-at  (fn [& _] (throw (ex-info "read whole" {})))]
        (with-open [s (api/snapshot db {:branch "main"})]
          (let [kept (lader/head-db (:db s) (:h s) "main" k/views)]
            (testing "only the changed entry's new version is fetched"
              (is (= [[:object-types ot]] (map first @asked))))
            (is (= "changed" (get-in (kdb/world kept) [:object-types ot :description])))))))))

(deftest a-fork-begins-at-its-parents-kept-db
  (let [db (store)
        W  (g/installed (for [n [:a :b]] [:create :object-type {:name n}]))
        ot (first (keys (:object-types W)))]
    (commit! db "main" W)
    (read-both db "main")
    (api/branch! db {:op :fork :name "f" :from (head db "main")})
    (commit! db "main" (assoc-in W [:object-types ot :description] "main moved on"))
    (let [asked (atom [])
          fetch reads/fetch]
      (with-redefs [reads/fetch    (fn [s h idrs] (swap! asked into idrs) (fetch s h idrs))
                    reads/world-at (fn [& _] (throw (ex-info "read whole" {})))]
        (with-open [s (api/snapshot db {:branch "f"})]
          (let [kept (lader/head-db (:db s) (:h s) "f" k/views)]
            (testing "nothing is fetched: the fork shares main's db at the fork"
              (is (empty? @asked)))
            (is (= W (kdb/world kept)))))))
    (commit! db "f" (assoc-in W [:object-types ot :description] "f's own"))
    (is (apply agrees? (read-both db "f")))))
