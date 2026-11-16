(ns ^:integration grammar.schema-delta-diff
  (:require [support.system :as tsys]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.interfaces :as interfaces]
            [sutra.catalog.signature :as signature]
            [clojure.string :as str]
            [clojure.test :refer [deftest use-fixtures]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.world :as world]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-stores-only)

(defn- uuid8 [] (subs (str (java.util.UUID/randomUUID)) 0 8))

(defn- ot [nm attrs implements]
  (cond-> {:object-type-name nm :id nm
           :attribute-list (mapv (fn [[n t]] {:attribute-name n :attribute-type t}) attrs)}
    (seq implements) (assoc :implements (vec implements))))

(defn- iface [nm props extends]
  (cond-> {:interface-name nm :id nm
           :property-list (mapv (fn [[n t]] {:attribute-name n :attribute-type t}) props)}
    (seq extends) (assoc :extends (vec extends))))

(defn- contribute-all
  [schema]
  (update schema :object-types
          (fn [ots]
            (into {} (for [[k ot] ots]
                       [k (assoc ot :attribute-list
                                 (interfaces/contribute-interface-props
                                  schema (:implements ot) (:attribute-list ot)))])))))

(defn- q [sql & params]
  (jdbc/execute! (tsys/ds) (into [sql] params) {:builder-fn rs/as-unqualified-maps}))

(defn- unqualified
  [b text]
  (-> (str text)
      (str/replace (pgns/own-schema b) "<own>")
      (str/replace (pgns/read-schema b) "<read>")))

(defn- catalog
  [b]
  (let [own (pgns/own-schema b)]
    {:tables   (->> (q "SELECT table_name AS t, column_name AS c, data_type AS ty
                        FROM information_schema.columns WHERE table_schema = ?" own)
                    (remove #(re-find #"_[0-9a-f]{6}__s\d+$" (str (:t %))))
                    (reduce (fn [m {:keys [t c ty]}] (assoc-in m [t c] ty)) {}))
     :triggers (->> (q "SELECT trigger_name AS n, event_object_table AS t, action_timing AS ti,
                               event_manipulation AS e, action_statement AS s
                        FROM information_schema.triggers WHERE trigger_schema = ?" own)
                    (map #(update % :s (partial unqualified b)))
                    set)
     :views    (->> (q "SELECT c.table_name AS t, c.column_name AS c, c.data_type AS ty
                        FROM information_schema.columns c
                        JOIN information_schema.views v
                          ON v.table_schema = c.table_schema AND v.table_name = c.table_name
                        WHERE c.table_schema = ?" (pgns/read-schema b))
                    (reduce (fn [m {:keys [t c ty]}] (assoc-in m [t c] ty)) {}))}))

(def ^:private actor {:principal "test"})

(defn- install!
  [b W]
  (world/install! (ctx/store b) W {:dataset-decls signature/datasets-of :actor actor
                                  :steps [[:drop-unclaimed]] :drop-unclaimed? true}))

(defn- timeline []
  (let [base {:link-types {} :transforms {} :shared-properties {}}]
    [     (assoc base :interfaces {} :object-types {:hangar (ot :hangar [[:hid :string]] nil)})
     (assoc base :interfaces {:locatable (iface :locatable [[:geo :string]] nil)}
            :object-types {:hangar (ot :hangar [[:hid :string]] [:locatable])})
     (assoc base :interfaces {:locatable (iface :locatable [[:geo :string]] nil)}
            :object-types {:hangar (ot :hangar [[:hid :string]] [:locatable])
                           :tower  (ot :tower  [[:tid :string]] [:locatable])})
     (assoc base :interfaces {:locatable (iface :locatable [[:geo :string]] nil)}
            :shared-properties {:ip {:shared-property-name :ip}}
            :object-types {:hangar (ot :hangar [[:hid :string] [:ip :string]] [:locatable])
                           :tower  (ot :tower  [[:tid :string] [:ip :string]] [:locatable])})
     (assoc base :interfaces {:locatable (iface :locatable [[:geo :string]] nil)
                              :premium   (iface :premium [[:value :number]] [:locatable])}
            :shared-properties {:ip {:shared-property-name :ip}}
            :object-types {:hangar (ot :hangar [[:hid :string] [:ip :string]] [:locatable :premium])
                           :tower  (ot :tower  [[:tid :string] [:ip :string]] [:locatable])})
     (assoc base :interfaces {:locatable (iface :locatable [[:geo :string]] nil)
                              :premium   (iface :premium [[:value :number]] [:locatable])}
            :shared-properties {:ip {:shared-property-name :ip}}
            :object-types {:hangar (ot :hangar [[:hid :string] [:ip :string]]
                                       [:locatable :premium])})
     (assoc base :interfaces {:locatable (iface :locatable [[:geo :string]] nil)
                              :premium   (iface :premium [[:value :number]] [:locatable])}
            :shared-properties {:ip {:shared-property-name :ip}}
            :object-types {:hangar (ot :hangar [[:hid :string] [:ip :string] [:name :string]]
                                       [:locatable :premium])})]))

(deftest delta-equiv-clean-install
  (let [run (uuid8)
        a (str "dda-" run)
        worlds (mapv (comp (partial ri/derived {:sigma clock/system}) contribute-all) (timeline))]
    (try
      (store/fork! a)
      (doseq [[i W] (map-indexed vector worlds)]
        (section (str "step S" i))
        (install! a W)
        (let [b (str "ddb-" run "-" i)]
          (store/fork! b)
          (install! b W)
          (let [ca (catalog a) cb (catalog b)]
          (check (str "S" i " tables and columns match") (= (:tables ca) (:tables cb)))
          (check (str "S" i " views match")              (= (:views ca) (:views cb)))
          (check (str "S" i " triggers match")           (= (:triggers ca) (:triggers cb)))
          (when (not= ca cb)
            (println "    A:" ca)
            (println "    B:" cb)))
          (store/retire! b)))
      (finally
        (try (store/retire! a) (catch Exception _ nil))))))
