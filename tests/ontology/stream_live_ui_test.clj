(ns ^:api ontology.stream-live-ui-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest do-report is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api])
  (:import [java.io BufferedReader InputStreamReader]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private harness (io/file (System/getProperty "user.dir") "tests/frontend"))
(def ^:private script (io/file harness "src" "sutra_e2e" "stream_watch.js"))
(def ^:private bundle (io/file (System/getProperty "user.dir")
                               "resources" "public" "js" "main.js"))

(def ^:private schema
  {:schema
   {:projects {:streams {}}
    :datasets
    {:ticks
     {:project :streams
      :kind    :standalone
      :columns [{:attribute-name :tick_id :attribute-type :string}
                {:attribute-name :value   :attribute-type :integer}]
      :stream  {:partitions 1 :primary-key {:columns [:tick_id]}}}}}})

(defn- line!
  [^BufferedReader rdr ms]
  (let [f (future (.readLine rdr))
        v (deref f ms ::timeout)]
    (when (= v ::timeout) (future-cancel f))
    (when-not (keyword? v) (println (str "  [browser] " v)) v)))

(defn- skip!
  [why]
  (println (str "  (stream-live-ui SKIPPED — " why ")"))
  (do-report {:type :kaocha/pending :message why}))

(deftest the-streams-tab-updates-while-it-is-watched
  (cond
    (not (.exists (io/file harness "node_modules")))
    (skip! "run: cd tests/frontend && npm install")

    (not (.exists bundle))
    (skip! "no compiled frontend; run: cd clients/frontend && npx shadow-cljs compile app")

    :else
    (do
      (is (ok? (api/post! :install schema)) "install stages")
      (is (ok? (api/post! :staging)) "commit")

      (let [pb (doto (ProcessBuilder. ["node" (.getPath script) (pod/base-url) "ticks"])
                 (.directory harness)
                 (.redirectErrorStream true))
            _  (.put (.environment pb) "NODE_PATH"
                     (.getPath (io/file harness "node_modules")))
            proc (.start pb)
            out  (BufferedReader. (InputStreamReader. (.getInputStream proc) "UTF-8"))]
        (try
          (testing "the page opens the tab, selects the stream and reports connected"
            (let [l (loop [n 0]
                      (when (< n 40)
                        (let [l (line! out 40000)]
                          (cond (nil? l) nil
                                (str/starts-with? l "WATCHING") l
                                (str/starts-with? l "ERROR") l
                                :else (recur (inc n))))))]
              (is (= "WATCHING" l)
                  "the pane reached 'connected' — an EventSource that never opened
                   would leave the pill in its reconnecting state")))

          (testing "a record published now shows up without a reload"
            (is (ok? (api/post! :stream-records {:name (name :ticks)}
                                {:records (vec [{:tick_id "t1" :value 42}])})))
            (let [l (loop [n 0]
                      (when (< n 40)
                        (let [l (line! out 40000)]
                          (cond (nil? l) nil
                                (or (str/starts-with? l "SAW")
                                    (str/starts-with? l "MISSED")
                                    (str/starts-with? l "ERROR")) l
                                :else (recur (inc n))))))]
              (is (some? l) "the browser answered")
              (is (str/starts-with? (str l) "SAW") (str "browser said: " l))
              (let [[tail view] (str/split (str l) #"\"view\":" 2)]
                (is (str/includes? tail "0:0")
                    "the arrived record is in the tail, at the offset it took")
                (is (str/includes? tail "tick_id")
                    "and the row carries the record, not just its address")
                (is (str/includes? (str view) "42")
                    "the CURRENT VIEW re-read off the same event — a derived read
                     that only refreshes on reload is the frozen table this pane
                     exists to replace"))))

          (finally (.destroy proc)))))))
