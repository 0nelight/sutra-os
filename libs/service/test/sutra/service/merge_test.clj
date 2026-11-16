(ns sutra.service.merge-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.service.merge :as m]
            [sutra.world.db :as db]
            [sutra.world.gen :as g]
            [sutra.types.entities :as ents]
            [sutra.types.shape :as tshape]
            [sutra.types.tree :as tree]))

(def ^:private opts {:cut tree/lists-of-maps :fold tshape/fold :first :theirs})

(defn- walked! [] (throw (ex-info "a section was walked" {::walked true})))

(deftype Guard [^clojure.lang.IPersistentMap m]
  clojure.lang.IPersistentMap
  (assoc [_ k v] (Guard. (.assoc m k v)))
  (assocEx [_ k v] (Guard. (.assocEx m k v)))
  (without [_ k] (Guard. (.without m k)))
  (containsKey [_ k] (.containsKey m k))
  (entryAt [_ k] (.entryAt m k))
  (count [_] (.count m))
  (cons [_ o] (Guard. (.cons m o)))
  (empty [_] (Guard. {}))
  (equiv [_ _] (walked!))
  (seq [_] (walked!))
  (valAt [_ k] (.valAt m k))
  (valAt [_ k nf] (.valAt m k nf))
  (iterator [_] (walked!))
  clojure.lang.IKVReduce
  (kvreduce [_ _ _] (walked!))
  clojure.lang.IFn
  (invoke [_ k] (.valAt m k))
  (invoke [_ k nf] (.valAt m k nf)))

(defn- guarded [W] (update (db/db W) :ents update-vals #(if (map? %) (Guard. %) %)))

(defn- entries
  [W]
  (into {} (for [[s es] (select-keys W ents/sections) :when (map? es) [k e] es] [[s k] e])))

(defn- old-merge
  [base mine theirs resolve]
  (let [cut #(select-keys % ents/sections)
        r   (tree/merge3 (cut base) (cut mine) (cut theirs) (assoc opts :resolve resolve))]
    {:entries (entries (merge theirs (:body r))) :conflicts (vec (:conflicts r))}))

(defn- new-merge [base mine theirs resolve]
  (let [{r :db cs :conflicts} (m/rebased (guarded base) (guarded mine) (guarded theirs)
                                         (g/diff base mine) (g/diff base theirs)
                                         (assoc opts :resolve resolve))]
    {:entries (entries (update-vals (db/world r) #(if (instance? Guard %) (.-m ^Guard %) %)))
     :conflicts (vec cs)}))

(def ^:private side
  (fn [W]
    (gen/let [steps (gen/vector g/step 0 4) raws (gen/vector g/raw-change 0 2)]
      (let [cx (g/sigma "s")
            W' (reduce (fn [W s] (or (g/install cx W s) W)) W steps)]
        (reduce g/apply-raw W' raws)))))

(def ^:private three
  (gen/let [base g/world mine (side base) theirs (side base)
            prefer (gen/elements [nil :mine :theirs])]
    [base mine theirs prefer]))

(defspec the-merge-of-touched-nodes-equals-the-whole-world-merge 300
  (prop/for-all [[base mine theirs prefer] three]
    (= (old-merge base mine theirs prefer) (new-merge base mine theirs prefer))))

(deftest attributes-both-sides-add-merge-element-by-element
  (let [base   (g/installed [[:create :object-type {:name :a :attributes [[:x :string]]}]])
        add    (fn [a] (g/install (g/sigma "u") base
                                  [:update :object-type
                                   {:name :a :attributes [[:x :string] [a :integer]]}]))
        mine   (add :y)
        theirs (add :z)
        r      (new-merge base mine theirs nil)]
    (is (empty? (:conflicts r)))
    (is (= (old-merge base mine theirs nil) r))))
