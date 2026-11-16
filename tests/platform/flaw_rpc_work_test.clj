(ns ^:integration platform.flaw-rpc-work-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.foundation.rpc :as rpc]
            [sutra.kit.effects :as effects]
            [support.pod :as pod]))

(def ^:private runs (atom 0))
(def ^:private calls (atom 0))

(def ^:private routes
  {"/slow"   (fn [_] (swap! runs inc) (Thread/sleep 800) :done)
   "/zombie" (fn [_] (loop [] (effects/call! #(swap! calls inc)) (Thread/sleep 50) (recur)))})

(defonce ^:private server* (atom nil))

(defn- with-server [f]
  (let [port (pod/free-port)]
    (rpc/serve! server* (fn [req] (rpc/handler routes req)) {:port port :ip "127.0.0.1"})
    (try (with-redefs [rpc/*grace-ms* 300]
           (f (str "http://127.0.0.1:" port)))
         (finally (rpc/stop-server! server*)))))

(deftest overlapping-run
  (with-server
    (fn [base]
      (reset! runs 0)
      (is (= :done
             (rpc/call! {:url (str base "/slow") :peer "test" :timeout 500 :retries 3
                         :backoff-ms 150} {})))
      (is (= 1 @runs) "the retry attached to the running work instead of starting it again"))))

(deftest unowned-effect
  (with-server
    (fn [base]
      (reset! calls 0)
      (is (thrown? Exception (rpc/call! {:url (str base "/zombie") :peer "test" :timeout 200} {})))
      (Thread/sleep 1200)
      (let [n @calls]
        (Thread/sleep 400)
        (is (= n @calls) "nobody waits for the work: it stopped at its next outside call")))))
