(ns ^:integration platform.read-path-parity
  (:require [support.system :as system]
            [support.install :as inst]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [sutra.store.seat :as seat]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.object-set :as os]
            [sutra.objects.planner :as planner]
            [sutra.store.core]
            [sutra.store.core.engine]
            [support.pod :as pod]
            [support.store :as store]
            [sutra.objects.plan-run :as plan-run]))

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(def ^:private widget-attrs
  [[:name :string] [:qty :integer] [:big :long] [:tiny :byte] [:small :short]
   [:ratio :float] [:precise :double] [:amount :number] [:flag :boolean]
   [:day :date] [:at :timestamp] [:ref :uuid] [:kind :keyword]
   [:tags [:array :string]]])

(defn- widget-values [i]
  {:name    (str "w-" (format "%03d" i))
   :qty     (int i)
   :big     (long (* i 1000000000))
   :tiny    (int (mod i 7))
   :small   (int (* i 3))
   :ratio   (float (/ i 4.0))
   :precise (double (/ i 3.0))
   :amount  (bigdec (str i ".2500000000"))
   :flag    (even? i)
   :day     (java.sql.Date/valueOf (java.time.LocalDate/of 2026 1 (inc (mod i 28))))
   :at      (java.sql.Timestamp/valueOf (java.time.LocalDateTime/of 2026 2 1 (mod i 24) 30 0))
   :ref     (uuid)
   :kind    (name (nth [:alpha :beta :gamma] (mod i 3)))
   :tags    [(str "t" (mod i 3)) "always"]})

(def ^:private world (atom nil))

(defn- join-table
  [ds]
  {:op :create-dataset :name ds :kind :standalone
   :columns [{:attribute-name :from_key :attribute-type :string}
             {:attribute-name :to_key :attribute-type :string}]})

(defn- join-producer [ds link]
  {:op :create-transform :name (keyword (str (name link) "-conn-rows"))
   :language :clojure :code "(fn [{:keys [kanten]}] kanten)"
   :inputs {:kanten {:kind :dataset :dataset ds}}
   :output {:kind :link-type :link-type link
            :from-key-column :from_key :to-key-column :to_key}})

(defn- seed! [branch]
  (let [run (fn [w rules] (inst/run-installing! branch w rules))
        w (run {:object-types {} :link-types {} :interfaces {}
                :transforms {} :actions {} :object-sets {}}
               [{:op :create-object-type :name :widget :attributes widget-attrs
                 :plural "Widgets" :primary-key :name}
                {:op :create-object-type :name :maker :attributes [[:name :string]]
                 :plural "Makers" :primary-key :name}
                {:op :create-object-type :name :country :attributes [[:name :string]]
                 :plural "Countries" :primary-key :name}
                {:op :create-link-type :name :made-by :from :widget :to :maker
                 :cardinality :many-to-many}
                {:op :create-link-type :name :based-in :from :maker :to :country
                 :cardinality :many-to-many}
                (join-table :widget_makers)
                (join-producer :widget_makers :made-by)
                (join-table :maker_countries)
                (join-producer :maker_countries :based-in)])
        widget (lookup/rid-of w :object-types :widget)
        maker  (lookup/rid-of w :object-types :maker)
        country (lookup/rid-of w :object-types :country)
        made-by (lookup/rid-of w :link-types :made-by)
        based-in (lookup/rid-of w :link-types :based-in)
        wid #(str "11111111-0000-0000-0000-" (format "%012d" %))
        mid #(str "22222222-0000-0000-0000-" (format "%012d" %))
        cid #(str "33333333-0000-0000-0000-" (format "%012d" %))
        b   branch]
    (doseq [i (range 24)]
      (store/insert-object! b widget {:id (wid i) :values (widget-values i)}))
    (doseq [i (range 6)]
      (store/insert-object! b maker {:id (mid i)
                                   :values {:name (if (= i 3) "m_3%odd" (str "m-" i))}}))
    (doseq [i (range 4)]
      (store/insert-object! b country (cond-> {:id (cid i) :values {:name (str "c-" i)}}
                                      (= i 2) (assoc :markings #{:secret}))))
    (doseq [i (range 24)]
      (store/insert-link! b made-by {:id (uuid) :from-id (wid i) :to-id (mid (mod i 6))}))
    (doseq [i (range 6)]
      (store/insert-link! b based-in {:id (uuid) :from-id (mid i) :to-id (cid (mod i 4))}))
    (reset! world {:w w :widget widget :maker maker :country country
                   :made-by made-by :based-in based-in :branch branch})))

(defn- with-seeded
  [f]
  (let [branch (str "engparity-" (subs (uuid) 0 8))]
    (store/fork! branch)
    (try
      (system/seat! branch)
      (seed! branch)
      (f)
      (finally
        (system/unseat!)
        (try (store/retire! branch) (catch Exception _ nil))))))

(use-fixtures :once pod/with-stores-only with-seeded)

(defn- scan [root] {:op :scan :root root})
(defn- hop [in link] {:op :search-around :link link :in in})

(defn- reads
  [{:keys [widget maker country made-by based-in]}]
  [["two hops"
    (-> (scan widget) (hop made-by) (hop based-in))]
   ["two hops, filtered start"
    (-> {:op :filter :where [:> :qty 10] :in (scan widget)}
        (hop made-by) (hop based-in))]
   ["two hops, ordered page"
    {:op :page :limit 3 :offset 1
     :in {:op :order :order-by :name :sort-dir :desc
          :in (-> (scan widget) (hop made-by) (hop based-in))}}]
   ["two hops, projected"
    {:op :project :select #{:name}
     :in (-> (scan widget) (hop made-by) (hop based-in))}]
   ["one hop, counted"
    {:op :aggregate :aggs :count :in (-> (scan widget) (hop made-by))}]
   ["two hops, counted"
    {:op :aggregate :aggs :count
     :in (-> (scan widget) (hop made-by) (hop based-in))}]
   ["hop from a LIKE filter carrying % and _"
    (-> {:op :filter :where [:contains :name "_3%"] :in (scan maker)}
        (hop based-in))]
   ["hop back the other way"
    (-> {:op :filter :where [:= :name "c-1"] :in (scan country)}
        (hop based-in) (hop made-by))]
   ["a hop reaching every column type"
    (-> {:op :filter :where [:= :name "m-1"] :in (scan maker)}
        (hop made-by))]
   ["union of two hops"
    {:op :union :in [(-> (scan widget) (hop made-by) (hop based-in))
                     (-> {:op :filter :where [:= :name "c-0"] :in (scan country)})]}]
   ["static start, two hops"
    (-> {:op :static :root widget
         :ids [(str "11111111-0000-0000-0000-" (format "%012d" 1))
               (str "11111111-0000-0000-0000-" (format "%012d" 7))]}
        (hop made-by) (hop based-in))]])

(defn- read-set [auth ir]
  (sub/query (ctx/store (seat/seated-branch) auth) {:ir ir}))

(defn- both-paths
  [auth ir]
  (let [can? @#'sutra.store.core.engine/engine-can?]
    [(read-set auth ir)
     (with-redefs [sutra.store.core.engine/engine-can?
                     (fn [db cap] (and (not= :select cap) (can? db cap)))]
       (read-set auth ir))]))

(defn- ordered?
  [ir]
  (boolean (some #(#{:order :page} (:op %)) (os/nodes ir))))

(defn- wire-unsafe
  [rows]
  (->> rows
       (mapcat #(vals (:values %)))
       (remove #(try (json/write-str %) (catch Exception _ false)))
       vec))

(deftest both-lowerings-answer-every-composed-read-identically
  (let [s @world]
    (doseq [[label ir] (reads s)]
      (testing label
        (let [[planned unplanned] (both-paths grants/system-authority ir)
                  as (if (ordered? ir) vec set)]
          (is (seq planned) "a read that returns nothing proves nothing")
          (is (= (as planned) (as unplanned))
              (str label " — the two lowerings disagree"))
          (is (= [] (wire-unsafe planned))
              (str label " — a value reaches the wire that data.json cannot write")))))

    (testing "and the marked row is hidden on BOTH, not just on the one that emits the clause"
      (let [ir (-> (scan (:widget s)) (hop (:made-by s)) (hop (:based-in s)))
            [planned unplanned] (both-paths {:denied-markings #{:secret}} ir)
            named (fn [rows] (set (map #(get-in % [:values :name]) rows)))]
        (is (= (named planned) (named unplanned)))
        (is (not (contains? (named planned) "c-2"))
            "the :secret country is denied — if it appears, the fixture is not testing anything")))))

(deftest the-planner-routes-what-it-says-it-routes
  (let [{:keys [w widget made-by based-in]} @world
        plan-of (fn [ir] (planner/plan w ir {:authority grants/system-authority}))]
    (testing "every shape routes to the one engine there is"
      (is (= :store (:engine (plan-of (-> (scan widget) (hop made-by) (hop based-in))))))
      (is (= :store (:engine (plan-of {:op :aggregate :aggs :count
                                      :in (-> (scan widget) (hop made-by))}))))
      (is (= :store (:engine (plan-of (scan widget)))))
      (is (= :store (:engine (plan-of (-> (scan widget) (hop made-by)))))))
    (testing "nothing is DECLINED, because nothing is weighed any more"
      (is (nil? (:engine-declined (plan-of (-> (scan widget) (hop made-by) (hop based-in)))))))
    (testing "and the planned read SURVIVES the trip to execution"
      (let [ir   (-> (scan widget) (hop made-by) (hop based-in))
            p    (planner/plan w ir {:authority grants/system-authority})
            rows (plan-run/execute (sub/reader (ctx/store (seat/seated-branch)
                                                          grants/system-authority))
                                   p)]
        (is (= :store (:engine p)))
        (is (seq rows))
        (is (= (set (read-set grants/system-authority ir)) (set rows))
            "the plan is a specialization, never a second semantics")))))
