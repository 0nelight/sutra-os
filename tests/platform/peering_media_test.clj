(ns ^:unit platform.peering-media-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.federation :as fed]
            [sutra.blobs.api :as blobs]
            [sutra.blobs.mem :as blobs-mem]
            [sutra.media.api :as media-api]
            [sutra.peering.connections :as conns]
            [sutra.peering.http :as http]
            [sutra.peering.jobs :as jobs]
            [sutra.peering.media :as media]
            [sutra.peering.rows :as rows]
            [sutra.peering.wire :as wire]
            [sutra.store.control.collections :as collections]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world])
  (:import (java.nio.charset StandardCharsets)))

(def ^:private P "ri.sutra.main.project.5b1d4573-c08a-45c9-800f-da8508608b74")
(def ^:private S "ri.sutra.main.media-set.7a1d4573-c08a-45c9-800f-da8508608b74")

(defn- instance [set-spec]
  (world/store (store-mem/store {:world (ri/derived {:sigma clock/system}
                                            {:projects   {P {:project-name :ablage}}
                                             :media-sets {S (merge {:media-set-name :plaene
                                                                    :project P}
                                                                   set-spec)}})}
                                {:collections collections/all})
               "main" {:clock #(java.util.Date.)}))

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

(defn- land! [s b path text]
  (let [bs (.getBytes ^String text StandardCharsets/UTF_8)
        {:keys [hash size]} (blobs/put! b bs)]
    (media-api/record! (assoc s :branch "main")
                       {:media-set :plaene :path path :address hash :size size
                        :mime "text/plain"})
    hash))

(defn- paths [s] (set (map :path (media-api/items (assoc s :branch "main") :plaene))))

(defn- connect! [home away]
  (let [{:keys [invite connection-id]}
        (conns/connect! home "admin" {:name "plaene" :trust "away" :media "export"})]
    (conns/accept! away "admin" {:invite invite})
    (conns/activate! home connection-id)
    (media/map-set! home "admin" connection-id {:local-set "plaene" :direction "export"})
    (media/map-set! away "admin" connection-id {:local-set "plaene" :direction "import"})
    connection-id))

(deftest a-media-set-peers-its-items
  (let [b (blobs-mem/blobs) home (instance {}) away (instance {})]
    (trust! home "away" away)
    (trust! away "home" home)
    (with-redefs [media/blob-store (constantly b)]
      (binding [wire/*transport* (in-process {"away" away "home" home})]
        (let [id (connect! home away)]
          (testing "a set peers one way, as the connection allows"
            (is (thrown? clojure.lang.ExceptionInfo
                         (media/map-set! away "admin" id {:local-set "plaene"
                                                          :direction "export"}))))
          (testing "an item travels with its bytes"
            (let [h (land! home b "a/plan.txt" "Grundriss EG")]
              (jobs/tick! home)
              (is (= #{"a/plan.txt"} (paths away)))
              (is (= h (:address (first (media-api/items (assoc away :branch "main")
                                                          :plaene)))))))
          (testing "only what changed travels again"
            (land! home b "a/plan2.txt" "Grundriss OG")
            (jobs/tick! home)
            (is (= #{"a/plan.txt" "a/plan2.txt"} (paths away)))
            (is (= [1 1] (->> (rows/jobs home id)
                              (filter #(= :media (:data-type %)))
                              (map :items)
                              (take 2)
                              (sort >)
                              vec)))))))))

(deftest an-item-is-not-landed-where-more-may-read-it
  (let [b (blobs-mem/blobs) home (instance {:markings [:intern]}) away (instance {})]
    (trust! home "away" away)
    (trust! away "home" home)
    (with-redefs [media/blob-store (constantly b)]
      (binding [wire/*transport* (in-process {"away" away "home" home})]
        (let [id (connect! home away)]
          (testing "above the ceiling, the sending side holds it"
            (land! home b "geheim.txt" "Gehalt")
            (jobs/tick! home)
            (is (empty? (paths away)))
            (is (= 1 (:held (first (filter #(= :media (:data-type %)) (rows/jobs home id)))))))
          (testing "through the ceiling, the receiving set that lacks the marking holds it"
            (conns/propose-ceiling! home id {:markings "intern"})
            (conns/approve-ceiling! away id)
            (rows/put-media-mapping! home (assoc (first (rows/media-mappings home id)) :sent {}))
            (jobs/tick! home)
            (is (empty? (paths away)))
            (is (re-find #"does not carry"
                         (str (:error (first (filter #(= :media (:data-type %))
                                                     (rows/jobs away id)))))))))))))

(deftest an-item-must-hash-to-its-address
  (let [b (blobs-mem/blobs) home (instance {}) away (instance {})]
    (trust! home "away" away)
    (trust! away "home" home)
    (with-redefs [media/blob-store (constantly b)]
      (binding [wire/*transport* (in-process {"away" away "home" home})]
        (let [id (connect! home away)
              c  (rows/connection away id)]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not hash"
                                (media/receive! away c {:type "plaene" :markings []
                                                        :items [{:path "x" :mime "text/plain"
                                                                 :address (apply str
                                                                                 (repeat 64 "0"))
                                                                 :bytes "aGFsbG8="}]}))))))))
