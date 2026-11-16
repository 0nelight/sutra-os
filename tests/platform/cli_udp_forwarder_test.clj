(ns ^:unit platform.cli-udp-forwarder-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.udp :as udp])
  (:import (java.net DatagramPacket DatagramSocket InetAddress)
           (java.time Instant)))

(defn- send-all! [port payloads]
  (with-open [s (DatagramSocket.)]
    (doseq [^bytes b payloads]
      (.send s (DatagramPacket. b (alength b) (InetAddress/getLoopbackAddress) (int port))))))

(defn- run-until
  [publish payloads done?]
  (let [seen  (atom [])
        stop  (atom false)
        out   (future
                (udp/forward! {:port 0 :bind "127.0.0.1" :flush-ms 50
                               :publish (fn [b] (swap! seen conj b) (publish @seen))
                               :stop? #(deref stop)
                               :on-ready #(send-all! % payloads)}))
        limit (+ (System/currentTimeMillis) 5000)]
    (while (and (not (done? @seen)) (not (realized? out))
                (< (System/currentTimeMillis) limit))
      (Thread/sleep 10))
    (reset! stop true)
    {:batches @seen :result (deref out 5000 :timed-out)}))

(deftest ein-datagramm-wird-ein-unveraenderter-datensatz
  (let [at (Instant/parse "2026-11-06T10:00:00Z")]
    (is (= {:message "<13>link down" :encoding "utf-8" :source_ip "10.0.0.1" :source_port 514
            :received_at "2026-11-06T10:00:00Z"}
           (udp/record-of (.getBytes "<13>link down" "UTF-8") "10.0.0.1" 514 at)))
    (testing "Binaeres wie sFlow bleibt als base64 verlustfrei"
      (is (= {:message "/4A=" :encoding "base64"}
             (select-keys (udp/record-of (byte-array [-1 -128]) "10.0.0.1" 514 at)
                          [:message :encoding]))))))

(deftest datagramme-kommen-ueber-einen-echten-socket-an
  (let [{:keys [batches result]}
        (run-until (constantly {:status 200})
                   [(.getBytes "a" "UTF-8") (.getBytes "b" "UTF-8")]
                   #(= 2 (count (mapcat identity %))))]
    (is (= ["a" "b"] (sort (map :message (mapcat identity batches)))))
    (is (every? #(= "127.0.0.1" (:source_ip %)) (mapcat identity batches)))
    (is (= {:received 2 :published 2 :dropped 0} result))))

(deftest ein-503-haelt-den-batch-und-sendet-ihn-erneut
  (let [{:keys [batches result]}
        (run-until #(if (= 1 (count %)) {:status 503} {:status 200})
                   [(.getBytes "a" "UTF-8")]
                   #(<= 2 (count %)))]
    (is (= 2 (count batches)))
    (is (= (first batches) (second batches)))
    (is (= 1 (:published result)))))

(deftest eine-verweigerung-beendet-den-forwarder
  (is (thrown-with-msg? Exception #"refused the batch"
                        (run-until (constantly {:status 400 :body {:error "no column"}})
                                   [(.getBytes "a" "UTF-8")]
                                   (constantly false)))))
