(ns ^:unit platform.peering-realtime-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.federation :as fed]
            [sutra.peering.connections :as conns]
            [sutra.peering.http :as http]
            [sutra.peering.jobs :as jobs]
            [sutra.peering.objects :as objects]
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

(def ^:private schema
  {:projects     {P {:project-name :vertrag}}
   :object-types {T {:object-type-name :vertrag :project P :primary-key :nr
                     :attribute-list [{:attribute-name :nr :attribute-type :string}
                                      {:attribute-name :titel :attribute-type :string}]
                     :plural "Verträge"}}})

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
        (throw (ex-info (str "peer refused: " (:status resp)) {:status (:status resp)}))))))

(defn- create! [s id values]
  (world/index! (world/at s "main")
                [{:op :create-object :type T :payload {:id id :values values}}]))

(defn- edit! [s id values]
  (world/commit! (world/at s "main")
                 {:ops [{:op :update-object :type T :payload {:id id :values values}}]
                  :origin :edit :at-head? true :actor {:principal "u"}}))

(defn- titles [s] (into {} (for [[_ {:keys [values]}] (objects/objects s T)]
                             [(:nr values) (:titel values)])))

(def ^:private hour 3600000)

(deftest notices-before-a-run-poke-it-once
  (jobs/tick! (instance))
  (let [pokes (atom 0)]
    (dotimes [_ 3] (jobs/noticed! "main" (keyword T) #(swap! pokes inc)))
    (jobs/noticed! "feature" (keyword T) #(swap! pokes inc))
    (is (= 1 @pokes) "three notices on main, one poke; another branch none")
    (jobs/tick! (instance) hour nil)
    (jobs/noticed! "main" (keyword T) #(swap! pokes inc))
    (is (= 2 @pokes) "after a run took them, the next notice pokes again")
    (jobs/tick! (instance))))

(deftest a-write-is-sent-at-once
  (let [home (instance) away (instance)]
    (trust! home "away" away)
    (trust! away "home" home)
    (binding [wire/*transport* (in-process {"away" away "home" home})]
      (let [{:keys [invite connection-id]}
            (conns/connect! home "admin" {:name "fern" :trust "away"
                                          :source-data "export" :action-data "both"})]
        (conns/accept! away "admin" {:invite invite})
        (conns/activate! home connection-id)
        (doseq [[s dir] [[home "export"] [away "import"]]]
          (conns/map-type! s "admin" connection-id
                           {:local-type "vertrag" :remote-type "vertrag"
                            :properties "nr=nr,titel=titel" :source-data dir
                            :action-data "both"}))
        (create! home "h1" {:nr "1" :titel "Miete"})
        (jobs/tick! home)
        (is (= {"1" "Miete"} (titles away)) "the full round still sends")

        (testing "an edit noticed is sent by the next run, though the round is not due"
          (edit! home "h1" {:titel "Miete neu"})
          (jobs/noticed! "main" (keyword T) (constantly nil))
          (jobs/tick! home hour nil)
          (is (= {"1" "Miete neu"} (titles away)))
          (is (empty? (rows/outbox home connection-id)) "acknowledged, so off the queue"))

        (testing "a run without a notice and before the round is due sends nothing"
          (edit! home "h1" {:titel "unbemerkt"})
          (jobs/tick! home hour nil)
          (is (= {"1" "Miete neu"} (titles away))))

        (testing "the lag from change to acknowledgement is kept on the job"
          (let [j (first (filter #(and (= :action-data (:data-type %)) (= :out (:way %))
                                       (= :ok (:state %)))
                                 (rows/jobs home connection-id)))]
            (is (nat-int? (:latency-ms j)))
            (is (contains? (get-in (peering/health [j]) [:jobs "action-data/out"])
                           :latency-ms))))
        (jobs/tick! home)))))

(deftest a-stream-backed-type-does-not-peer
  (is (= :stream (peering/datasource-kind {:dataset :d1} {:stream {:partitions 1}})))
  (is (= :direct (peering/datasource-kind {:dataset :d1 :direct true} {:stream {}})))
  (is (= :direct (peering/datasource-kind {:kind :direct} nil)))
  (is (= :dataset (peering/datasource-kind {:dataset :d1} {})))
  (is (= #{:source-data :action-data}
         (set (map :data-type (peering/datasource-problems
                               {:source-data :export :action-data :both} :stream)))))
  (is (empty? (peering/datasource-problems {:source-data :export :action-data :both}
                                           :direct)))
  (is (= [:dataset-source-data]
         (map :note (peering/datasource-notes {:source-data :export} :dataset))))
  (is (empty? (peering/datasource-notes {:source-data :none} :dataset)))
  (testing "the mapping check refuses it"
    (is (some #(= :stream-backed (:problem %))
              (peering/mapping-problems {:properties {"nr" "nr"} :action-data :both}
                                        {:local-attrs {:nr :string} :remote-attrs {:nr :string}
                                         :local-product "p" :remote-product "p"
                                         :datasource :stream})))))

(deftest lag-is-from-the-oldest-known-change
  (is (= 30 (peering/lag (peering/oldest-change [0 100 70 nil]) 100)))
  (is (nil? (peering/lag (peering/oldest-change [0 0]) 100)))
  (is (= 0 (peering/lag 200 100))))

(deftest the-kind-follows-the-types-backing
  (let [s {:datasets     {"ri.ds.calls" {:dataset-name :calls :stream {:partitions 1}}
                          "ri.ds.plain" {:dataset-name :plain}}
           :object-types {"ri.ot.call"  {:object-type-name :call
                                         :backing {:dataset :calls :primary-key :id}}
                          "ri.ot.plain" {:object-type-name :plain
                                         :backing {:dataset :plain :primary-key :id}}}}]
    (is (= :stream (objects/datasource-kind s "ri.ot.call")))
    (is (= :direct (objects/datasource-kind
                    (assoc-in s [:object-types "ri.ot.call" :backing :direct] true)
                    "ri.ot.call")))
    (is (= :dataset (objects/datasource-kind s "ri.ot.plain")))
    (is (= [:index] (objects/source-origins (instance) T))
        "without a direct datasource source data is read from the builds' commits")))

(deftest revisions-compare
  (is (= 12 (peering/later-rev 10 12)))
  (is (= 12 (peering/later-rev 12 nil)))
  (is (nil? (peering/later-rev nil nil))))
