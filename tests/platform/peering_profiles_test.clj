(ns ^:unit platform.peering-profiles-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.federation :as fed]
            [sutra.peering.http :as http]
            [sutra.peering.jobs :as jobs]
            [sutra.peering.objects :as objects]
            [sutra.peering.profiles :as profiles]
            [sutra.peering.rows :as rows]
            [sutra.peering.wire :as wire]
            [sutra.store.control :as ctl]
            [sutra.store.control.collections :as collections]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world]
            [sutra.types.peering :as peering]))

(def ^:private T "ri.sutra.main.object-type.8f4c36f9-3ea7-4e54-b12d-17ea707983bf")
(def ^:private P "ri.sutra.main.project.5b1d4573-c08a-45c9-800f-da8508608b74")

(defn- attrs [& as] (mapv (fn [a] {:attribute-name a :attribute-type :string}) as))

(def ^:private schema
  {:projects     {P {:project-name :vertrag}}
   :object-types {T {:object-type-name :vertrag :project P :primary-key :nr
                     :attribute-list (attrs :nr :titel) :plural "Verträge"}}})

(defn- instance []
  (let [s (world/store (store-mem/store {:world (ri/derived {:sigma clock/system} schema)}
                                        {:collections collections/all}) "main"
                       {:clock #(java.util.Date.)})]
    (ctl/upsert! s :product-installs {:install-id (str (random-uuid)) :product "vertraege"
                                      :version "1.0.0" :branch "main" :projects ["vertrag"]})
    s))

(defn- trust! [s nm other]
  (fed/declare-trust! s "admin" {:name nm :url (str "https://" nm ".example")
                                 :public-key (:public-key (fed/identity-of other))
                                 :inbound? true :outbound? true}))

(defn- in-process [instances]
  (fn [store trust _path payload]
    (let [resp (http/peer-handler {:body-params (wire/envelope store payload)
                                   :sutra/store (get instances trust)
                                   :remote-addr "10.0.0.7"})]
      (if (= 200 (:status resp))
        (edn/read-string (get-in resp [:body :payload]))
        (throw (ex-info (str "peer refused: " (:status resp) " " (pr-str (:body resp)))
                        {:status (:status resp)}))))))

(defn- titles [s] (into {} (for [[_ {:keys [values]}] (objects/objects s T)]
                             [(:nr values) (:titel values)])))

(deftest a-profile-pair-sets-up-the-connection-by-itself
  (let [hub (instance) spoke (instance) other (instance)]
    (trust! hub "spoke" spoke)
    (trust! hub "other" other)
    (trust! spoke "hub" hub)
    (rows/put-mesh-node! hub "spoke" ["edge"])
    (rows/put-mesh-node! hub "other" ["central"])
    (binding [wire/*transport* (in-process {"spoke" spoke "hub" hub "other" other})]
      (let [{:keys [profile counterpart]}
            (profiles/create-pair! hub "admin"
                                   {:name "verträge" :strategy "labels:edge" :auto-connect true
                                    :data-types {:source-data :export :action-data :both}
                                    :types (pr-str [{:local-type "vertrag"
                                                     :remote-type "vertrag"
                                                     :properties {"nr" "nr" "titel" "titel"}
                                                     :source-data :export
                                                     :action-data :both}])})]
        (testing "the counterpart mirrors the hub's profile"
          (is (= :spoke (:side counterpart)))
          (is (= {:source-data :import :action-data :both :media :none :datasets :none}
                 (:data-types counterpart)))
          (is (= [{:local-type "vertrag" :remote-type "vertrag"
                   :properties {"nr" "nr" "titel" "titel"}
                   :source-data :import :action-data :both}]
                 (:types counterpart))))
        (testing "the strategy picks the mesh's nodes by label"
          (is (= ["spoke"] (:targets (profiles/view hub profile)))))
        (profiles/install! spoke "admin" {:profile (pr-str counterpart)
                                          :strategy "instance:hub" :auto-connect "true"})
        (testing "the hub's round connects and hands over the invite"
          (profiles/round! hub)
          (let [[c :as cs] (rows/connections hub)]
            (is (= 1 (count cs)))
            (is (= ["spoke" :requested] [(:trust c) (:status c)]))
            (is (= 1 (count (rows/offers spoke))))))
        (testing "the spoke's round takes it in, the hub's activates and maps"
          (profiles/round! spoke)
          (is (empty? (rows/offers spoke)))
          (profiles/round! hub)
          (profiles/round! spoke)
          (let [id (:connection-id (first (rows/connections hub)))]
            (is (= :active (:status (rows/connection hub id))))
            (is (= :active (:status (rows/connection spoke id))))
            (is (some? (rows/mapping hub id "vertrag")))
            (is (some? (rows/mapping spoke id "vertrag")))))
        (testing "a round more changes nothing"
          (profiles/round! hub)
          (profiles/round! spoke)
          (is (= 1 (count (rows/connections hub))))
          (is (= 1 (count (rows/connections spoke)))))
        (testing "data peers over it"
          (world/index! (world/at hub "main")
                        [{:op :create-object :type T
                          :payload {:id "h1" :values {:nr "1" :titel "Miete"}}}])
          (jobs/tick! hub)
          (is (= {"1" "Miete"} (titles spoke))))))))

(deftest a-profile-is-checked-and-picks-its-targets
  (is (= [{:problem :strategy-names-no-label}]
         (peering/profile-problems {:pair "p" :name "n" :side :hub
                                    :strategy {:kind :labels}})))
  (is (= [:unknown-side]
         (mapv :problem (peering/profile-problems {:pair "p" :name "n" :side :middle
                                                   :strategy {:kind :all}}))))
  (let [mesh {"a" #{"edge"} "b" #{"central"} "c" #{"edge"}}]
    (is (= ["a" "b" "x"] (peering/targets {:strategy {:kind :all}} ["a" "b" "x"] mesh)))
    (is (= ["a" "b"] (peering/targets {:strategy {:kind :mesh}} ["a" "b" "x"] mesh)))
    (is (= ["a"] (peering/targets {:strategy {:kind :labels :labels ["edge"]}} ["a" "b"] mesh)))
    (is (= ["b"] (peering/targets {:strategy {:kind :instance :instance "b"}} ["a" "b"] mesh))))
  (is (= {:kind :labels :labels ["a" "b"]} (profiles/strategy-of "labels:a, b")))
  (is (= {:kind :instance :instance "hub"} (profiles/strategy-of "instance:hub"))))
