(ns ^:unit platform.world-external-source-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]
            [sutra.world.db :as db]
            [sutra.world.install :as k]
            [sutra.ref.install :as ri]))

(defn- cx []
  (let [n (atom 0)]
    (assoc-in kernel/install-cx [:sigma :fresh-uuid] #(str "u-" (swap! n inc)))))

(defn- installed [steps]
  (reduce (fn [W [verb tag body]]
            (:world (ri/install {:world W :installed W} (cx) verb tag body)))
          {} steps))

(defn- outcome [f]
  (try (f) (catch clojure.lang.ExceptionInfo e {:refused (:ref/kind (ex-data e))})))

(defn- reference [W [verb tag body]]
  (outcome #(:world (ri/install {:world W :installed W} (cx) verb tag body))))

(defn- kernel [W [verb tag body]]
  (let [c (cx)]
    (outcome #(db/world (:db (k/run c (k/of-world W) (ri/build c verb tag body)))))))

(def ^:private base
  [[:create :project {:name :werft}]
   [:create :object-type {:name :rechnung :project :werft
                          :attributes [{:attribute-name :id :attribute-type :string}]}]])

(def ^:private steps
  {"an object type"
   [:update :object-type {:name :rechnung :project :werft
                          :attributes [{:attribute-name :id :attribute-type :string}]
                          :external-source {:datasource :mro :table "R" :primary-key :id}}]
   "a dataset"
   [:create :dataset {:name :zeit_raw :project :werft
                      :external-source {:datasource :mro :table "ZV"}}]
   "a link type"
   [:create :link-type {:name :anrechnung :from :rechnung :to :rechnung
                        :cardinality :many-to-many :project :werft
                        :external-source {:datasource :mro :table "A"
                                          :from-key-column :from_id
                                          :to-key-column :to_id}}]})

(deftest a-step-with-an-external-source-equals-the-reference
  (let [W (installed base)]
    (doseq [[what s] steps]
      (testing what
        (let [r (kernel W s)]
          (is (not (:refused r)) (pr-str r))
          (is (some #(re-find #"-(rows|conn-rows|sync)$" (name (:transform-name %)))
                    (vals (:transforms r))))
          (is (= (reference W s) r)))))))

(deftest a-lifted-transform-replaces-the-one-of-its-name
  (let [W (installed (conj base [:create :dataset {:name :zeit_raw :project :werft
                                                   :external-source {:datasource :mro
                                                                     :table "ZV"}}]))
        s [:update :dataset {:name :zeit_raw :project :werft
                             :external-source {:datasource :mro :table "ZV2"
                                               :primary-key :id}}]
        r (kernel W s)]
    (is (= [:id] (keep :primary-key (vals (:transforms r)))))
    (is (= (reference W s) r))))
