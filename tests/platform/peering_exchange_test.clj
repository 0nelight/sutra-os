(ns ^:unit platform.peering-exchange-test
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
            [sutra.store.world :as world]))

(def ^:private T "ri.sutra.main.object-type.8f4c36f9-3ea7-4e54-b12d-17ea707983bf")
(def ^:private M "ri.sutra.main.object-type.01ad4573-c08a-45c9-800f-da8508608b74")
(def ^:private P "ri.sutra.main.project.5b1d4573-c08a-45c9-800f-da8508608b74")

(defn- attrs [& as] (mapv (fn [a] {:attribute-name a :attribute-type :string}) as))

(def ^:private schema
  {:projects     {P {:project-name :vertrag}}
   :object-types {T {:object-type-name :vertrag :project P :primary-key :nr
                     :attribute-list (attrs :nr :titel :intern) :plural "Verträge"}
                  M {:object-type-name :marking :primary-key :name
                     :attribute-list (attrs :name) :plural "Marken"}}})

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

(defn- in-process
  [instances]
  (fn [store trust _path payload]
    (let [resp (http/peer-handler {:body-params (wire/envelope store payload)
                                   :sutra/store (get instances trust)
                                   :remote-addr "10.0.0.7"})]
      (if (= 200 (:status resp))
        (edn/read-string (get-in resp [:body :payload]))
        (throw (ex-info (str "peer refused: " (:status resp) " " (pr-str (:body resp)))
                        {:status (:status resp)}))))))

(defn- create! [s id values]
  (world/index! (world/at s "main")
                [{:op :create-object :type T :payload {:id id :values values}}]))

(defn- edit! [s id values]
  (world/commit! (world/at s "main")
                 {:ops [{:op :update-object :type T :payload {:id id :values values}}]
                  :origin :edit :at-head? true :actor {:principal "u"}}))

(defn- sent
  [s id k]
  (reduce + (keep #(when (and (= :source-data (:data-type %)) (= :out (:way %))) (k %))
                  (rows/jobs s id))))

(defn- titles [s] (into {} (for [[_ {:keys [values]}] (objects/objects s T)]
                             [(:nr values) (:titel values)])))

(deftest two-instances-peer-a-type
  (let [home (instance) away (instance)
        _    (trust! home "away" away)
        _    (trust! away "home" home)]
    (binding [wire/*transport* (in-process {"away" away "home" home})]
      (let [{:keys [invite connection-id]}
            (conns/connect! home "admin" {:name "fern" :trust "away"
                                          :source-data "export" :action-data "both"
                                          :markings "intern"})]
        (testing "an invite is taken in only from a trusted instance"
          (is (thrown? clojure.lang.ExceptionInfo
                       (conns/accept! (instance) "admin" {:invite invite}))))
        (conns/accept! away "admin" {:invite invite})
        (is (= :accepted (:status (rows/connection home connection-id))))
        (is (= {:source-data :import :action-data :both :media :none :datasets :none}
               (:data-types (rows/connection away connection-id))))
        (conns/activate! home connection-id)
        (is (= :active (:status (rows/connection away connection-id))))

        (testing "a mapping is checked against the other side's type"
          (is (thrown? clojure.lang.ExceptionInfo
                       (conns/map-type! home "admin" connection-id
                                        {:local-type "vertrag" :remote-type "vertrag"
                                         :properties "nr=nr,titel=gibtsnicht"}))))
        (conns/map-type! home "admin" connection-id
                         {:local-type "vertrag" :remote-type "vertrag"
                          :properties "nr=nr,titel=titel" :source-data "export"
                          :action-data "both"})
        (conns/map-type! away "admin" connection-id
                         {:local-type "vertrag" :remote-type "vertrag"
                          :properties "nr=nr,titel=titel" :source-data "import"
                          :action-data "both"})

        (testing "source data travels one way, only the mapped properties"
          (create! home "h1" {:nr "1" :titel "Miete" :intern "geheim"})
          (jobs/tick! home)
          (is (= {"1" "Miete"} (titles away)))
          (is (nil? (some :intern (map :values (vals (objects/objects away T)))))))

        (testing "only changed objects travel again"
          (create! home "h2" {:nr "2" :titel "Strom"})
          (jobs/tick! home)
          (is (= {"1" "Miete" "2" "Strom"} (titles away)))
          (is (= 2 (sent home connection-id :items)) "h1 once, h2 once"))

        (testing "an edit made on the other side comes back to the object it came from"
          (let [away-id (key (first (filter #(= "1" (get-in (val %) [:values :nr]))
                                            (objects/objects away T))))]
            (jobs/tick! away)
            (edit! away away-id {:titel "Miete 2027"})
            (jobs/tick! away)
            (is (= "Miete 2027" (get-in (objects/objects home T ["h1"]) ["h1" :values :titel])))
            (testing "and is not sent back again"
              (jobs/tick! home)
              (is (empty? (rows/outbox home connection-id))))))

        (testing "the ceiling keeps a marked object back"
          (world/index! (world/at home "main")
                        [{:op :create-object :type T
                          :payload {:id "h3" :values {:nr "3" :titel "Gehalt"}
                                    :markings [:personal]}}])
          (jobs/tick! home)
          (is (not (contains? (titles away) "3")))
          (is (= 1 (sent home connection-id :held))))

        (testing "a ceiling change waits for the other side"
          (conns/propose-ceiling! home connection-id {:markings "intern,personal"})
          (is (= {:classification nil :markings ["intern"]}
                 (:ceiling (rows/connection away connection-id))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (conns/approve-ceiling! home connection-id)))
          (conns/approve-ceiling! away connection-id)
          (is (= ["intern" "personal"] (:markings (:ceiling (rows/connection home
                                                                              connection-id))))))

        (testing "what the old ceiling kept back passes the new one; a marking unknown on
                  the other side is created there with nobody cleared for it"
          (jobs/tick! home)
          (is (= "Gehalt" (get (titles away) "3")))
          (let [local (get (:marking-map (rows/connection away connection-id)) "personal")]
            (is (some? local))
            (is (contains? (objects/objects away M) local))
            (is (= [(keyword local)]
                   (:markings (val (first (filter #(= "3" (get-in (val %) [:values :nr]))
                                                  (objects/objects away T)))))))))

        (testing "disabled by either side, nothing peers"
          (conns/set-disabled! away connection-id true)
          (is (true? (:disabled-remote (rows/connection home connection-id))))
          (create! home "h5" {:nr "5" :titel "Wasser"})
          (jobs/tick! home)
          (is (not (contains? (titles away) "5")))
          (conns/set-disabled! away connection-id false)
          (jobs/tick! home)
          (is (= "Wasser" (get (titles away) "5"))))))))

(deftest a-batch-is-resent-until-acknowledged
  (let [home (instance) away (instance)
        _    (trust! home "away" away)
        _    (trust! away "home" home)
        good (in-process {"away" away "home" home})
        down (fn [& _] (throw (ex-info "peer away unreachable" {})))]
    (binding [wire/*transport* good]
      (let [{:keys [invite connection-id]}
            (conns/connect! home "admin" {:name "x" :trust "away" :source-data "export"
                                          :action-data "both"})]
        (conns/accept! away "admin" {:invite invite})
        (conns/activate! home connection-id)
        (doseq [[s sd] [[home "export"] [away "import"]]]
          (conns/map-type! s "admin" connection-id
                           {:local-type "vertrag" :remote-type "vertrag" :source-data sd
                            :properties "nr=nr,titel=titel" :action-data "both"}))
        (create! home "h1" {:nr "1" :titel "a"})
        (jobs/tick! home)
        (is (= "a" (get (titles away) "1")))
        (edit! home "h1" {:titel "b"})
        (binding [wire/*transport* down]
          (jobs/run-connection! home (rows/connection home connection-id) 0))
        (is (= 1 (count (rows/outbox home connection-id))) "kept for the next attempt")
        (is (pos? (:next-at (first (rows/outbox home connection-id)))))
        (jobs/run-connection! home (rows/connection home connection-id) Long/MAX_VALUE)
        (is (empty? (rows/outbox home connection-id)) "acknowledged, gone")
        (is (= "b" (get (titles away) "1")))))))
