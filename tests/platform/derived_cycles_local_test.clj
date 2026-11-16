(ns ^:unit platform.derived-cycles-local-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.findings.derive :as derive]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.world-install :as world-install]
            [sutra.world.db :as kdb]
            [support.every-index :as every]))

(def ^:private types [:a :b :c :d])
(def ^:private attrs [:x :y :z])
(def ^:private handles [:p :q :r :s])

(def ^:private g-expr
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/fmap (fn [[a b]] [:+ a b]) (gen/tuple inner inner))
                  (gen/fmap (fn [[h a]] [:connected-sum h a])
                            (gen/tuple (gen/elements handles) (gen/elements attrs)))
                  (gen/fmap (fn [[h a]] [:connected-arg-max h a a])
                            (gen/tuple (gen/elements handles) (gen/elements attrs)))
                  (gen/fmap (fn [[h e]] [:connected? h e])
                            (gen/tuple (gen/elements handles) inner))]))
   (gen/elements attrs)))

(def ^:private g-attr
  (gen/let [n (gen/elements attrs)
            e (gen/frequency [[1 (gen/return nil)] [3 g-expr]])]
    (cond-> {:attribute-name n :attribute-type :decimal}
      e (assoc :opts {:derived {:expression e}}))))

(def ^:private g-type
  (gen/let [as   (gen/vector g-attr 0 3)
            impl (gen/elements [nil [:i]])]
    (cond-> {:attribute-list (vec (vals (into {} (map (juxt :attribute-name identity)) as)))}
      impl (assoc :implements impl))))

(def ^:private g-link
  (gen/let [from (gen/elements (conj types :i))
            to   (gen/elements (conj types :i))
            f    (gen/elements handles)
            r    (gen/elements handles)]
    {:from-object-type from :to-object-type to :forward-api-name f :reverse-api-name r}))

(def ^:private g-world
  (gen/let [ots (gen/map (gen/elements types) g-type {:max-elements 4})
            lts (gen/map (gen/elements [:l0 :l1 :l2 :l3]) g-link {:max-elements 4})]
    {:object-types (into {} (for [[k o] ots] [k (assoc o :object-type-name k)]))
     :interfaces   {:i {:interface-name :i}}
     :link-types   (into {} (for [[k l] lts] [k (assoc l :link-type-name k)]))}))

(def ^:private g-plan
  (gen/vector (gen/let [k  (gen/elements types)
                        oc (gen/elements [:created :updated :deleted])
                        t  g-type]
                {:kind :object-type :entity k :outcome oc :args t})
              1 3))

(def ^:private g-text-links
  (gen/vector (gen/let [k (gen/elements [:l1 :l4]) l g-link]
                [k (:from-object-type l) (:to-object-type l) :many-to-one
                 (select-keys l [:forward-api-name :reverse-api-name])])
              0 2))

(defn- outcome [f] (try {:ok (vec (f))} (catch Exception e {:thrown (ex-message e)})))

(defn- found [W text plan ix]
  (outcome #((:find derive/derived-cycles) (diff/diff text W plan nil ix))))

(defn- agree? [W text plan]
  (let [ix (live/index-of (kdb/db W world-install/views))]
    (= (found W text plan (every/index W)) (found W text plan ix))))

(defspec the-cycles-read-by-name-are-those-read-whole 1500
  (prop/for-all [W g-world plan g-plan links g-text-links]
    (agree? W {:links links} plan)))

(defn- rid [kind k] (str "ri.sutra.main." kind "." (name k)))

(defn- by-rid
  [W]
  (let [ot #(rid "object-type" %)
        end #(if (= :i %) (rid "interface" %) (ot %))]
    {:object-types (into {} (for [[k o] (:object-types W)]
                              [(ot k) (cond-> o (:implements o)
                                        (update :implements (partial mapv end)))]))
     :interfaces   {(rid "interface" :i) {:interface-name :i}}
     :link-types   (into {} (for [[k l] (:link-types W)]
                              [(rid "link-type" k) (-> l (update :from-object-type end)
                                                       (update :to-object-type end))]))}))

(defspec the-cycles-read-by-name-are-those-read-whole-by-rid 1500
  (prop/for-all [W g-world plan g-plan links g-text-links]
    (agree? (by-rid W) {:links links} plan)))

(deftest a-cycle-through-a-link-the-install-closes-is-found
  (let [W    {:object-types {:a {:object-type-name :a
                                 :attribute-list [{:attribute-name :x :attribute-type :decimal
                                                   :opts {:derived {:expression
                                                                    [:connected-sum :p :y]}}}]}
                             :b {:object-type-name :b
                                 :attribute-list [{:attribute-name :y :attribute-type :decimal}]}}
              :link-types   {:l0 {:link-type-name :l0 :from-object-type :a :to-object-type :b
                                  :forward-api-name :p :reverse-api-name :q}}}
        plan [{:kind :object-type :entity :b :outcome :updated
               :args {:attribute-list [{:attribute-name :y :attribute-type :decimal
                                        :opts {:derived {:expression [:connected-sum :q :x]}}}]}}]
        ix   (live/index-of (kdb/db W world-install/views))
        f    (fn [ix] (map (comp :chain :sutra.authoring.install.findings/ctx)
                           ((:find derive/derived-cycles) (diff/diff {} W plan nil ix))))]
    (is (= [[[:a :x] [:b :y]]] (f (every/index W)) (f ix)))))
