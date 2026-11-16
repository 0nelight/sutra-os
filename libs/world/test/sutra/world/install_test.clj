(ns sutra.world.install-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [sutra.world.db :as db]
            [sutra.world.gate :as gate]
            [sutra.world.gen :as g]
            [sutra.world.install :as k]
            [sutra.world.law-test :refer [norm-findings stated]]
            [sutra.world.schema :as schema]
            [sutra.ref.install :as ri]
            [sutra.types.entities :as ents]))

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

(defn- guarded [d] (update d :ents update-vals #(if (map? %) (Guard. %) %)))

(defn- unguarded [W] (update-vals W #(if (instance? Guard %) (.-m ^Guard %) %)))

(defn- outcome [f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (when (::walked (ex-data e)) (throw e))
         {:refused (or (:ref/kind (ex-data e)) (ex-message e))})))

(defn reference
  ([W body] (reference W :object-type body))
  ([W tag body]
   (outcome #(let [W' (:world (ri/install {:world W :installed W} (g/sigma "k")
                                          :create tag body))]
               {:world W' :findings (norm-findings (ri/report (g/sigma "r") W' W))}))))

(defn kernel
  ([W body] (kernel W :object-type body))
  ([W tag body]
   (outcome #(let [step (ri/build (g/sigma "b") :create tag body)
                   r    (k/run (g/sigma "k") (guarded (k/of-world W)) step)
                   db'  (update (:db r) :ents unguarded)]
               (assert (m/validate schema/Delta (:delta r)))
               {:world (db/world db') :findings (norm-findings (gate/report-of stated db'))}))))

(defspec create-object-type-equals-the-reference 300
  (prop/for-all [W g/world body g/object-type-body]
    (or (not (g/oracle-defined? W))
        (= (reference W body) (kernel W body)))))

(defn- reference-step [W [verb tag body]]
  (outcome #(let [st (ri/install {:world W :installed W} (g/sigma "k") verb tag body)
                  W' (:world st)]
              {:world W' :ddl (frequencies (:ddl st))
               :findings (norm-findings (ri/report (g/sigma "r") W' W))})))

(defn- kernel-step [W [verb tag body]]
  (outcome #(let [step (ri/build (g/sigma "b") verb tag body)
                  r    (k/run (g/sigma "k") (guarded (k/of-world W)) step)
                  db'  (update (:db r) :ents unguarded)]
              (assert (m/validate schema/Delta (:delta r)))
              (when (:step-findings r) (throw (ex-info "sql" {:ref/kind :sql-gate})))
              {:world (db/world db') :ddl (frequencies (:ddl r))
               :findings (norm-findings (gate/report-of stated db'))})))

(defspec every-verb-equals-the-reference 300
  (prop/for-all [W g/world s g/step]
    (or (not (g/oracle-defined? W))
        (= (reference-step W s) (kernel-step W s)))))

(defspec create-equals-the-reference 300
  (prop/for-all [W g/world [tag body] g/create]
    (or (not (g/oracle-defined? W))
        (= (reference W tag body) (kernel W tag body)))))

(deftest a-taken-name-is-refused-alike
  (let [W (g/installed [[:create :object-type {:name :a}]])]
    (is (= {:refused :name-taken} (reference W {:name :a}) (kernel W {:name :a})))))

(deftest a-new-type-resolves-the-names-that-waited-for-it
  (let [W  (g/installed [[:create :link-type {:name :l :from :a :to :b
                                              :cardinality :many-to-one}]])
        lt (fn [W] (first (vals (:link-types W))))]
    (is (= :a (:from-object-type (lt W))))
    (is (db/rid? (:from-object-type (lt (:world (kernel W {:name :a}))))))
    (is (= (reference W {:name :a}) (kernel W {:name :a})))))

(deftest the-kernel-takes-every-verb-of-every-mutable-section
  (doseq [{:keys [tag section]} ents/mutable
          verb [:create :update :delete]
          :let [n    (if (= :llm-model tag) "n" :n)
                step (ri/build (g/sigma "b") verb tag {:name n})
                r    (try (k/run (g/sigma "k") (k/of-world (g/installed [])) step)
                          (catch clojure.lang.ExceptionInfo e {:refused (ex-data e)}))]]
    (is (or (:db r) (:ref/kind (:refused r))) (pr-str [verb section r]))))

(deftest a-new-transform-closes-a-cycle-alike
  (let [in (fn [n] {:x {:kind :transform :transform n}})
        W  (g/installed [[:create :transform
                          {:name :a :language :sql :query "q" :inputs (in :b)}]])]
    (doseq [body [{:name :b :language :sql :query "q" :inputs (in :a)}
                  {:name :c :language :sql :query "q" :inputs (in :c)}]]
      (let [r (kernel W :transform body)]
        (is (some (comp :cycle? key) (:findings r)))
        (is (= (reference W :transform body) r))))))

(def ^:private world-and-delete
  (gen/bind g/world
            (fn [W]
              (let [named (for [[sec es] W :when (map? es)
                                :let [tag (ents/section->tag sec)]
                                :when tag
                                [_ e] es
                                :let [n (get e (ents/name-field sec))]
                                :when (keyword? n)]
                            [:delete tag {:name n}])]
                (if (seq named)
                  (gen/fmap (fn [s] [W s]) (gen/elements named))
                  (gen/return [W [:delete :object-type {:name :a}]]))))))

(defspec deleting-an-entry-equals-the-reference 300
  (prop/for-all [[W s] world-and-delete]
    (or (not (g/oracle-defined? W))
        (= (reference-step W s) (kernel-step W s)))))

(deftest a-live-definition-is-not-deleted-alike
  (let [W (g/installed [[:create :object-type {:name :a :status :active}]])
        s [:delete :object-type {:name :a}]]
    (is (= {:refused :protected-retire} (reference-step W s) (kernel-step W s)))))

(deftest the-sql-gate-findings-are-a-value
  (let [cx (assoc-in (g/sigma "k") [:sigma :sql-gate] (fn [_ _ _] [{:rule :sql/x}]))
        r  (k/run cx (k/of-world (g/installed []))
                  (ri/build (g/sigma "b") :create :transform {:name :t :language :sql :query "q"}))]
    (is (= [{:rule :sql/x}] (:step-findings r)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"SQL gate"
                          (ri/install {:world (g/installed []) :installed (g/installed [])} cx
                                      :create :transform {:name :t :language :sql :query "q"})))))

(def ^:private name-keyed
  (gen/let [tag  (gen/elements [:role :marking-definition :projection :eval-suite :llm-model])
            verb (gen/elements [:create :update :delete])
            n    (gen/elements [:a :b])
            d    (gen/elements [nil "x" "y"])]
    [verb tag (cond-> {:name (if (= :llm-model tag) (name n) n)} d (assoc :description d))]))

(defspec a-section-keyed-by-name-equals-the-reference 300
  (prop/for-all [W g/world pre (gen/vector name-keyed 0 3) s name-keyed]
    (let [W (reduce (fn [W [verb tag body]]
                      (or (:world (try (ri/install {:world W :installed W} (g/sigma "k")
                                                   verb tag body)
                                       (catch clojure.lang.ExceptionInfo _ nil)))
                          W))
                    W pre)]
      (or (not (g/oracle-defined? W))
          (= (reference-step W s) (kernel-step W s))))))

(deftest an-entry-stated-whole-drops-what-it-no-longer-holds
  (let [W    (g/installed [[:create :object-type {:name :a :description "alt"}]])
        [r e] (first (:object-types W))
        put  (fn [whole?]
               (let [st (with-meta (cond-> {:op :set :path [:object-types r]
                                            :value (dissoc e :description)}
                                     whole? (assoc :whole-entry? true))
                          {:verb :update})]
                 (get-in (db/world (:db (k/run (g/sigma "k") (k/of-world W) st)))
                         [:object-types r :description])))]
    (is (= "alt" (put false)))
    (is (nil? (put true)))))
