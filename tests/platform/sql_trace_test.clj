(ns ^:unit platform.sql-trace-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.foundation.fn-trace :as fntrace]
            [sutra.foundation.outbound :as outbound]
            [sutra.kit.effects :as effects]
            [sutra.foundation.profile.scope :as scope]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.foundation.wire :as wire]
            [sutra.kit.pubsub]
            [sutra.serve.observe :as observe]
            [sutra.serve.platform]
            [sutra.serve.ports :as ports]
            [sutra.store.storage.node :as node])
  (:import [java.sql Connection Statement PreparedStatement]
           [java.lang.reflect Proxy InvocationHandler]))

(use-fixtures :each (fn [f] (sqltrace/disarm!) (sqltrace/clear!) (f) (sqltrace/disarm!)))

(defn- fake-statement ^Statement [take-ms]
  (let [h (reify InvocationHandler
            (invoke [_ _ m _]
              (case (.getName m)
                "execute" (do (Thread/sleep (long take-ms)) true)
                "getUpdateCount" (int 1)
                "close" nil
                nil)))]
    (Proxy/newProxyInstance (clojure.lang.RT/baseLoader)
                            (into-array Class [PreparedStatement]) h)))

(defn- fake-connection ^Connection [take-ms]
  (let [h (reify InvocationHandler
            (invoke [_ _ m _]
              (case (.getName m)
                ("prepareStatement" "createStatement" "prepareCall") (fake-statement take-ms)
                "close" nil
                nil)))]
    (Proxy/newProxyInstance (clojure.lang.RT/baseLoader)
                            (into-array Class [Connection]) h)))

(defn- fake-datasource [take-ms]
  (reify javax.sql.DataSource
    (getConnection [_] (fake-connection take-ms))
    (getConnection [_ _ _] (fake-connection take-ms))))

(defn- run-one! [^Connection c sql]
  (with-open [stmt (.prepareStatement c sql)] (.execute stmt)))

(defn- recorded-sql []
  (->> (:events (sqltrace/since 0)) (keep :sql) vec))

(deftest a-temp-tables-name-does-not-split-its-shape
  (let [c (sqltrace/wrap-connection (fake-connection 0) "test")]
    (sqltrace/arm! {:shapes? true})
    (doseq [id ["4b82d4b7456244fdb052aa3e4a13e8dd" "563d7c7f15b943b3af89c786e4506971"]]
      (run-one! c (str "SELECT 1 FROM _probe_" id)))
    (is (= [[2 "SELECT 1 FROM _probe_<id>"]]
           (->> (sqltrace/shapes-report {:limit 5}) :rows (map (juxt :calls :sql))))
        (str "one shape, counted twice"))))

(deftest arming-reaches-a-connection-opened-before-it
  (let [ds (sqltrace/wrap-datasource (fake-datasource 0) "test")
        c  (.getConnection ds)]
    (testing "before arming, the wrapper is inert — nothing is recorded"
      (run-one! c "SELECT 'before'")
      (is (empty? (recorded-sql)) "disarmed records nothing, as before"))

    (testing "arming NOW reaches the connection that was already open"
      (sqltrace/arm!)
      (run-one! c "SELECT 'after'")
      (is (= ["SELECT 'after'"] (recorded-sql))
          "the long-held connection is traced — this is the whole fix"))

    (testing "and a connection opened after arming is traced too (unchanged)"
      (run-one! (.getConnection ds) "SELECT 'fresh'")
      (is (= #{"SELECT 'after'" "SELECT 'fresh'"} (set (recorded-sql)))))))

(deftest a-directly-opened-connection-is-reachable-too
  (let [c (sqltrace/wrap-connection (fake-connection 0) "lake")]
    (run-one! c "SELECT 'unarmed'")
    (is (empty? (recorded-sql)))
    (sqltrace/arm!)
    (run-one! c "SELECT 'armed'")
    (is (= ["SELECT 'armed'"] (recorded-sql)))))

(defn- profiled! [c f]
  (let [reports (atom [])]
    (scope/run-scoped {:label "sql scope" :on-report #(swap! reports conj %)} #(f c))
    (last @reports)))

(deftest a-scope-counts-its-own-statements-and-returns-the-tracer
  (let [c (sqltrace/wrap-connection (fake-connection 0) "test")
        r (profiled! c (fn [c]
                         (run-one! c "SELECT 'mine'")
                         @(future (run-one! c "SELECT 'elsewhere'"))))]
    (is (= ["SELECT 'mine'"] (map :sql (get-in r [:sql-shapes :rows])))
        "a statement on a thread outside the scope is not the run's")
    (is (not (sqltrace/armed?)) "armed for the scope, so disarmed with it")))

(deftest a-scope-leaves-a-tracer-armed-by-hand-alone
  (sqltrace/arm! {:shapes? true})
  (let [c (sqltrace/wrap-connection (fake-connection 0) "test")]
    (run-one! c "SELECT 'before'")
    (let [r (profiled! c #(run-one! % "SELECT 'during'"))]
      (is (= ["SELECT 'during'"] (map :sql (get-in r [:sql-shapes :rows])))))
    (is (sqltrace/armed?))
    (is (= 2 (:shapes (sqltrace/shapes-report {}))) "what it had gathered is still there")))

(deftest coverage-counts-parallel-work-once
  (let [busy #'sqltrace/merge-spans]
    (is (= 0    (busy [])))
    (is (= 10   (busy [[0 10]])))
    (is (= 10   (busy [[0 10] [0 10] [0 10] [0 10]])) "four in parallel = one window")
    (is (= 10   (busy [[0 6] [4 10]]))                "overlapping, merged")
    (is (= 10   (busy [[0 10] [3 5]]))                "nested counts once")
    (is (= 8    (busy [[0 4] [6 10]]))                "a real gap is a real gap")
    (is (= 8    (busy [[6 10] [0 4]]))                "order does not matter")))

(deftest coverage-reports-the-gap-it-used-to-hide
  (testing "never armed ⇒ nil, not a fake 0% — 'never looked' is not 'saw nothing'"
    (sqltrace/disarm!)
    (is (nil? (sqltrace/coverage)) "no window to be honest about"))

  (testing "a window spent mostly IN statements reads high"
    (sqltrace/arm!) (sqltrace/clear!)
    (let [c (sqltrace/wrap-connection (fake-connection 60) "test")]
      (dotimes [_ 3] (run-one! c "SELECT 1"))
      (let [{:keys [covered busy-ms window-ms spans]} (sqltrace/coverage)]
        (is (= 3 spans))
        (is (>= busy-ms 150) "three 60ms statements, sequential")
        (is (pos? window-ms))
        (is (> covered 0.5)
            (str "most of the window was inside a traced statement, got " covered)))))

  (testing "a window spent mostly OUTSIDE statements reads low — the alarm"
    (sqltrace/arm!) (sqltrace/clear!)
    (let [c (sqltrace/wrap-connection (fake-connection 0) "test")]
      (run-one! c "SELECT 1")
      (Thread/sleep 250)
      (let [{:keys [covered]} (sqltrace/coverage)]
        (is (< covered 0.5)
            (str "the untraced stretch has to show, got " covered))))))

(deftest a-report-keeps-the-slow-beside-the-newest
  (let [events (mapv (fn [i] {:seq i :ms (if (= 5 i) 9000 1)}) (range 1000))
        kept   (observe/kept-events events)]
    (is (= 350 (count kept)))
    (is (some #(= 5 (:seq %)) kept) "a slow statement outlives the polling after it")
    (is (= 999 (:seq (last kept))))
    (is (= 3 (count (observe/kept-events (subvec events 0 3)))))))

(deftest a-worker-obeys-the-fn-tracer-and-reports-it
  (with-redefs [node/role :worker]
    (let [arm!  (fn [on opts] (#'observe/arm-fn-heard! {:on on :opts opts}))
          fns   #(:fns (observe/report))]
      (try
        (arm! true {:prefixes ["sutra.kit.pubsub"] :request "r1"})
        (is (= "r1" (:request (fns))))
        (is (true? (:armed (fns))))
        (is (pos? (:wrapped (fns))))
        (testing "a prefix not loaded here says so instead of looking idle"
          (arm! true {:prefixes ["gibt.es.nicht"] :request "r2"})
          (is (= [true 0 "r2"] ((juxt :armed :wrapped :request) (fns)))))
        (testing "a refusal on the bus's thread comes back in the report, and leaves it off"
          (with-redefs [fntrace/wrap-all! (fn [_] (throw (ex-info "narrow them" {})))]
            (arm! true {:prefixes ["sutra.kit.pubsub"] :request "r3"}))
          (is (= "r3" (:request (fns))))
          (is (re-find #"narrow them" (:error (fns))))
          (is (false? (:armed (fns))))
          (is (false? (fntrace/armed?))))
        (finally (arm! false {:request "off"})))
      (is (zero? (:wrapped (fns)))))))

(deftest what-the-fn-tracer-sends-passes-the-bus
  (let [sent (atom [])]
    (with-redefs [ports/cluster-broadcast (constantly (fn [t m] (swap! sent conj [t m])))]
      (try
        (observe/switch-fns! true {:prefixes ["gibt.es.nicht"] :slow-ms 5})
        (observe/refresh-fns!)
        (observe/switch-fns! false {})
        (finally (fntrace/disarm!))))
    (is (= [:observe/arm-fn :observe/report-now :observe/arm-fn] (map first @sent)))
    (doseq [[topic message] @sent]
      (is (nil? (wire/problem [:bus topic] message)) (pr-str topic message)))))

(deftest a-new-arming-counts-from-zero
  (let [probe (intern (create-ns 'fn-trace-probe) 'f (fn [] :ok))
        calls #(->> (fntrace/report {:limit 5}) :rows (keep (fn [r] (:calls r))) (reduce + 0))]
    (try
      (fntrace/arm! {:prefixes ["fn-trace-probe"]})
      (dotimes [_ 3] (@probe))
      (is (= 3 (calls)))
      (fntrace/arm! {:prefixes ["fn-trace-probe"]})
      (is (= 0 (calls)))
      (@probe)
      (is (= 1 (calls)))
      (finally (fntrace/disarm!)))))

(deftest a-report-answers-a-read-at-once
  (with-redefs [node/role :worker]
    (#'observe/report-now! {:request "lesung"})
    (is (= "lesung" (:read (:fns (observe/report)))))))

(deftest fn-scope-answers-for-the-request-and-names-the-silent
  (let [rows   [{:fn "a" :self-ms 9.0 :total-ms 1.0 :calls 1}
                {:fn "b" :self-ms 1.0 :total-ms 9.0 :calls 2}]
        report (fn [node request]
                 {:node node :role "worker" :age-ms 1000
                  :fns {:armed true :wrapped 2 :fns 2 :rows rows :request request}})
        alive  {"w1" "worker" "w2" "worker" "a2" "api"}
        scope  (observe/fn-scope {:reports [(report "w1" "r") (report "w2" "alt")
                                            (report "past" "r")]
                                  :alive alive :request "r" :limit 1 :sort-key "total"})]
    (is (= ["w1"] (map :node (:nodes scope))) "an older answer and a past colour do not count")
    (is (= ["b"] (map :fn (:rows (first (:nodes scope))))) "the reader sorts and cuts")
    (is (= [{:node "w2" :role "worker"}] (:silent scope)) "another api arms itself")
    (testing "a refresh marks who did not answer it, and each process stays its own entry"
      (let [nodes (:nodes (observe/fn-scope
                           {:reports [(assoc-in (report "w1" "r") [:fns :read] "jetzt")
                                      (report "w2" "r")]
                            :alive alive :refresh "jetzt"}))]
        (is (= [["w1" nil] ["w2" true]] (map (juxt :node :stale) nodes)))
        (is (= [2 2] (map (comp count :rows) nodes)))))
    (testing "a plain read takes every live process's last report"
      (is (= ["w1" "w2"] (map :node (:nodes (observe/fn-scope {:reports [(report "w1" "r")
                                                                         (report "w2" "alt")]
                                                               :alive alive}))))))))

(deftest a-report-keeps-the-top-of-each-sort
  (let [rows (mapv (fn [i] {:fn (str "f" i) :self-ms (double i) :total-ms (double (- 1000 i))
                            :calls (if (= i 500) 99999 1)})
                   (range 1000))
        kept (fntrace/kept-rows rows)]
    (is (<= (count kept) 150))
    (is (= "f999" (:fn (first (fntrace/sorted-rows kept :self 1)))))
    (is (= "f0" (:fn (first (fntrace/sorted-rows kept :total 1)))))
    (is (= "f500" (:fn (first (fntrace/sorted-rows kept :calls 1)))))))

(deftest a-scope-names-who-did-not-answer
  (let [{:keys [processes past-reports not-covered]}
        (observe/scope {:own     {:node "a" :armed true :seen 7 :events [{}]}
                        :egress  {:erp nil}
                        :reports [{:node "w1" :role "worker" :armed true :seen 1013
                                   :events (repeat 350 {}) :age-ms 2000}
                                  {:node "old" :role "worker" :armed false :seen 9 :events []}]
                        :alive   {"a2" "api" "w1" "worker" "c1" "conductor"}})
        by (into {} (map (juxt :node identity)) processes)]
    (is (= {:process "api" :node "a" :answered true :armed true :statements 7 :shapes false}
           (by "a")))
    (is (false? (:answered (by "erp"))))
    (is (= [1013 350] ((juxt :statements :in-report) (by "w1"))))
    (is (false? (:answered (by "c1"))) "alive and silent is said, not skipped")
    (is (nil? (by "a2")) "another api arms itself, never over the bus")
    (is (= ["old"] past-reports))
    (is (seq not-covered))))

(deftest an-outside-call-is-traced-like-a-statement
  (testing "off: nothing is recorded"
    (is (= {:status 200} (effects/call! nil "GET http://x/y" (constantly {:status 200}))))
    (is (empty? (:events (sqltrace/since 0)))))
  (sqltrace/arm!)
  (is (= {:status 503} (effects/call! nil "POST http://h/p" (constantly {:status 503}))))
  (is (thrown? Exception (effects/call! nil "GET http://h/q" #(throw (Exception. "down")))))
  (effects/call! nil (constantly :untargeted))
  (let [evs (filter :http (:events (sqltrace/since 0)))]
    (is (= ["POST http://h/p" "GET http://h/q"] (mapv :sql evs)) "an untargeted call is not")
    (is (= [503 nil] (mapv :status evs)))
    (is (every? :ms evs))
    (is (= [nil true] (mapv :failed evs)))
    (is (= [:HTTP :HTTP] (mapv :kind (sqltrace/events->bus "a" evs)))))
  (testing "an answer that comes later is timed at its callback"
    (let [p (effects/call-async! nil "GET http://h/later"
                                 (fn [done] (future (done {:status 204}))))]
      (is (= {:status 204} @p))
      (Thread/sleep 50)
      (is (= 204 (:status (last (filter :http (:events (sqltrace/since 0)))))))))
  (sqltrace/disarm!)
  (is (nil? (effects/call! nil "GET http://h/off" (constantly nil))))
  (is (not-any? #(= "GET http://h/off" (:sql %)) (:events (sqltrace/since 0)))))

(deftest a-trace-shows-no-query-of-a-url
  (is (= "POST https://api.x:8443/v1/chat"
         (outbound/target {:method :post :url "https://u:pw@api.x:8443/v1/chat?key=s"})))
  (is (= "GET http://h/p" (outbound/target {:url "http://h/p"}))))

(deftest a-subprocess-is-traced-with-its-exit
  (sqltrace/arm!)
  (is (= {:exit 3} (effects/observed "EXEC python3" (constantly {:exit 3}))))
  (is (= [["EXEC python3" 3]]
         (mapv (juxt :sql :status) (filter :http (:events (sqltrace/since 0))))))
  (sqltrace/disarm!))
