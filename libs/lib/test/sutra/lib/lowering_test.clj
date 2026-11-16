(ns sutra.lib.lowering-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.lib :as lib]
            [sutra.lib.dsl :refer [defobject deflink ns-decls ontology]]
            [sutra.lib.store :as store]
            [sutra.lib.test-store :as ts]
            [sutra.ref.expr :as rexpr]
            [sutra.ref.objectset :as ros]
            [sutra.store.api :as api]
            [sutra.store.derived :as derived]))

(defobject plane
  {:match-keys [:tail]}
  [:tail   :string]
  [:seats  :integer]
  [:weight [:decimal 12 2]]
  [:d      [:decimal 12 2] {:derived {:expression [:connected-count :parts]}}])

(defobject part
  {:match-keys [:part-id]}
  [:part-id :string]
  [:hours   :integer]
  [:status  :string]
  [:tail    :string])

(deflink part-plane part :many-to-one plane
  {:forward-api-name :plane :reverse-api-name :parts :fk :tail})

(def ^:private ent (ontology (ns-decls 'sutra.lib.lowering-test)))

(def ^:private planes
  [{:id "A" :values {:tail "A" :seats 3 :weight 2.5M}}
   {:id "B" :values {:tail "B" :seats 0}}
   {:id "C" :values {:tail "C" :seats 7 :weight 10M}}])

(def ^:private parts
  [{:id "a1" :values {:part-id "a1" :tail "A" :hours 4 :status "open"}}
   {:id "a2" :values {:part-id "a2" :tail "A" :status "closed"}}
   {:id "c1" :values {:part-id "c1" :tail "C" :hours 1 :status "open"}}
   {:id "c2" :values {:part-id "c2" :tail "C" :hours 2 :status "open"}}
   {:id "c3" :values {:part-id "c3" :tail "C" :hours 6 :status "closed"}}])

(def ^:private leaf
  (gen/elements [0 1 2 -3 0.25M 1.5M :seats :weight
                 [:connected-count :parts]
                 [:connected-count :parts [:= :status "open"]]
                 [:connected-sum :parts :hours]
                 [:connected-avg :parts :hours]
                 [:connected-min :parts :hours]
                 [:connected-max :parts :hours [:= :status "open"]]]))

(def ^:private expression
  (gen/such-that vector?
                 (gen/recursive-gen
                  (fn [inner]
                    (gen/one-of [(gen/fmap #(into [:+] %) (gen/tuple inner inner))
                                 (gen/fmap #(into [:-] %) (gen/tuple inner inner))
                                 (gen/fmap #(into [:*] %) (gen/tuple inner inner))
                                 (gen/fmap #(into [:div] %) (gen/tuple inner inner))
                                 (gen/fmap #(vector :abs %) inner)
                                 (gen/fmap #(into [:coalesce] %) (gen/tuple inner inner))]))
                  leaf)
                 100))

(defn- with-d
  [W T e]
  (update-in W [:object-types T :attribute-list]
             (fn [as]
               (mapv #(cond-> % (= :d (:attribute-name %))
                        (assoc-in [:opts :derived :expression] e)) as))))

(defn- world []
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower ent) {:clock clock})]
    (store/put! db (lib/type-rid W :plane) planes)
    (store/put! db (lib/type-rid W :part) parts)
    {:db db :W W :T (lib/type-rid W :plane)}))

(defn- evaluated-here
  [W T ask e]
  (into {}
        (for [row (ask [:objects T (mapv :id planes)])]
          [(:id row)
           (ros/cast-value
            (derived/cast-of W T :d)
            (derived/read-value T (:id row) :d
                               #(derived/evaluate
                                 (fn [V x]
                                   (rexpr/eval V x
                                               {:self row
                                                :traverse
                                                  (fn [self over] (lib/traverse W ask self over))}))
                                 (merge {:seats nil :weight nil} (:values row))
                                 e)))])))

(deftest a-stated-property-reads-as-it-evaluates
  (let [{:keys [db W T]} (world)
        r (tc/quick-check
           60
           (prop/for-all [e expression]
                         (with-open [s (api/snapshot db {:branch "main"})]
                           (let [W'   (with-d W T e)
                                 ask  #(api/read s store/system %)
                                 here (evaluated-here W' T ask e)
                                 read (into {} (map (juxt :id (comp :d :values)))
                                            (lib/select W' ask
                                                        {:op :static :root T
                                                         :ids (mapv :id planes)}))]
                             (and (derived/lower W' T :d)
                                  (= here read))))))]
    (is (:pass? r) (pr-str (select-keys r [:shrunk :fail])))))

(deftest a-filter-on-a-stated-property-is-the-stores
  (let [{:keys [db W T]} (world)
        r (tc/quick-check
           60
           (prop/for-all [e expression]
                         (with-open [s (api/snapshot db {:branch "main"})]
                           (let [W'      (with-d W T e)
                                 ask     #(api/read s store/system %)
                                 ir      {:op :filter :where [:> :d 0] :in {:op :scan :root T}}
                                 lowered (derived/plan W' ir)
                                 here    (evaluated-here W' T ask e)
                                 kept    (into #{} (map :id) (lib/select W' ask ir))
                                 e'      (api/read s store/system [:select/explain lowered])]
                             (and (nil? (derived/unstated W' lowered))
                                  (= kept
                                     (into #{}
                                           (keep (fn [[id v]] (when (and (number? v) (pos? v)) id)))
                                           here))
                                  (or (not ts/pg?)
                                      (and (:pushdown? e') (= [[]] (:claims e')))))))))]
    (is (:pass? r) (pr-str (select-keys r [:shrunk :fail])))))
