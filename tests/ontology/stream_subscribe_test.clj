(ns ^:api ontology.stream-subscribe-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api])
  (:import [java.io BufferedReader InputStreamReader]
           [java.net URL HttpURLConnection]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:schema
   {:projects {:streams {}}
    :datasets
    {:ticks
     {:project :streams
      :kind    :standalone
      :columns [{:attribute-name :tick_id :attribute-type :string}
                {:attribute-name :value   :attribute-type :integer}]
      :stream  {:partitions 1 :partition-keys [:tick_id]}}}}})

(defn- open-subscription
  [stream-name]
  (let [url  (URL. (str (pod/base-url) (api/stream-subscribe-url stream-name)))
        conn ^HttpURLConnection (.openConnection url)]
    (.setRequestProperty conn "Accept" "text/event-stream")
    (.setConnectTimeout conn 5000)
    (.setReadTimeout conn 15000)
    [conn (BufferedReader. (InputStreamReader. (.getInputStream conn) "UTF-8"))]))

(defn- read-event
  [^BufferedReader rdr]
  (loop [n 0]
    (when (< n 200)
      (if-let [line (.readLine rdr)]
        (if (str/starts-with? line "data: ")
          (subs line 6)
          (recur (inc n)))
        nil))))

(deftest a-subscriber-receives-records-as-they-are-published
  (is (ok? (api/post! :install schema)) "install stages")
  (is (ok? (api/post! :staging)) "commit")

  (let [[conn rdr] (open-subscription "ticks")]
    (try
      (testing "the connection opens as an event stream"
        (is (= 200 (.getResponseCode ^HttpURLConnection conn)))
        (is (str/starts-with? (str (.getHeaderField ^HttpURLConnection conn "Content-Type"))
                              "text/event-stream")
            "not a transit response — a held connection has to say so in its type,
             or the JSON encoder in front of it would try to render the channel"))

      (testing "the first frame confirms which stream is being watched"
        (let [open (read-event rdr)]
          (is (str/includes? (str open) "\"stream\":\"ticks\""))))

      (testing "a published record arrives without anyone asking for it"
        (let [received (future (read-event rdr))]
          (Thread/sleep 200)
          (is (ok? (api/post! :stream-records {:name (name :ticks)}
                              {:records (vec [{:tick_id "t1" :value 7}])})))
          (let [frame (deref received 10000 ::timeout)]
            (is (not= ::timeout frame) "the record reached the subscriber")
            (is (str/includes? (str frame) "\"tick_id\":\"t1\""))
            (is (str/includes? (str frame) "\"offset\":0")
                "and it carries the offset it took, so a reader that drops off
                 knows where to resume by offset"))))

      (testing "the server counts the live subscriber"
        (is (= 1 (:subscribers (:body (api/get! :stream-by {:name (name :ticks)}))))))

      (finally (.disconnect ^HttpURLConnection conn)))))
