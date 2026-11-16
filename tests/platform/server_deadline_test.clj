(ns ^:unit platform.server-deadline-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.serve.deadline :as deadline]
            [sutra.types.paths :as paths]))

(def ^:private wrap #'deadline/wrap-deadline)

(defn- routed
  [id uri method]
  {:uri uri :request-method method :sutra/endpoint id})

(defn- slow-handler [ms]
  (fn [_] (Thread/sleep ms) {:status 200 :body :finished}))

(deftest a-handler-that-answers-in-time-is-untouched
  (let [h (wrap (fn [_] {:status 200 :body :ok}))]
    (is (= {:status 200 :body :ok} (h {:uri "/api/x" :request-method :get})))))

(deftest a-handler-that-does-not-answer-gets-answered-for
  (with-redefs [deadline/default-deadline-ms 120]
    (let [h   (wrap (slow-handler 5000))
          t0  (System/currentTimeMillis)
          res (h {:uri "/api/transforms/foo" :request-method :delete})
          dt  (- (System/currentTimeMillis) t0)]
      (testing "it answers, and soon"
        (is (= 503 (:status res)))
        (is (< dt 3000) "the caller is not held for the handler's whole run"))
      (testing "503, not 500 — the request was fine, the server was not"
        (is (= 503 (:status res))))
      (testing "the answer names what was being done"
        (let [err (str (get-in res [:body :error]))]
          (is (str/includes? err "/api/transforms/foo"))
          (is (str/includes? err "DELETE"))))
      (testing "…and says only what is KNOWN of the work"
        (let [err (str (get-in res [:body :error]))]
          (is (not (str/includes? err "cancelled"))
              "the watchdog interrupts; it rolls nothing back, so it may not claim it did")
          (is (str/includes? err "read the state back")
              "a write that may have committed is not offered as safe to repeat"))))))

(deftest a-read-past-its-deadline-is-safe-to-repeat-and-says-so
  (with-redefs [deadline/default-deadline-ms 100]
    (let [h   (wrap (slow-handler 5000))
          err (str (get-in (h (routed :branches "/api/branches" :get)) [:body :error]))]
      (is (str/includes? err "asking again is safe")
          "a read changes nothing — only a write needs the state read back"))))

(deftest work-that-finished-late-is-served-not-replaced
  (with-redefs [deadline/default-deadline-ms 100]
    (let [h (wrap (fn [_] (try (Thread/sleep 400) (catch InterruptedException _ nil))
                    {:status 200 :body {:merged true}}))]
      (is (= {:status 200 :body {:merged true}}
             (h {:uri "/api/proposals/7/merge" :request-method :post}))
          "the work is done and said so; the deadline's report is the log, not a lie to the caller"))))

(deftest the-work-is-interrupted-so-its-connections-come-back
  (with-redefs [deadline/default-deadline-ms 100]
    (let [finished (promise)
          h        (wrap (fn [_]
                           (try (Thread/sleep 4000) (deliver finished :ran)
                                (catch InterruptedException _ (deliver finished :interrupted)))
                           {:status 200}))]
      (h {:uri "/api/x" :request-method :post})
      (is (= :interrupted (deref finished 3000 :never-ran))
          "past its deadline the handler is interrupted, not left running on a held connection"))))

(deftest an-interrupted-handler-that-throws-is-the-one-that-gets-a-503
  (with-redefs [deadline/default-deadline-ms 100]
    (let [h (wrap (fn [_] (Thread/sleep 4000) {:status 200}))]
      (is (= 503 (:status (h {:uri "/api/x" :request-method :post})))))))

(deftest long-running-routes-are-exempt
  (with-redefs [deadline/default-deadline-ms 80]
    (let [h (wrap (slow-handler 300))]
      (is (= 200 (:status (h (routed :import-run "/api/import/run" :post))))
          "a build is allowed to take as long as it takes")
      (is (= 503 (:status (h (routed :schema-lint "/api/schema/lint" :get))))
          "…and nothing else inherits that licence"))))

(deftest the-deadline-is-the-routes
  (testing "a row names :unbounded, the rest have the contract's default"
    (is (nil? (paths/deadline-ms :llm-chat)))
    (is (nil? (paths/deadline-ms :stream-subscribe)))
    (is (= paths/default-deadline-ms (paths/deadline-ms :branches))))
  (testing "a request the router did not match has the default"
    (is (= paths/default-deadline-ms (paths/deadline-ms nil)))))

(deftest work-that-swallows-an-interrupt-still-stops-at-its-next-round-trip
  (with-redefs [deadline/default-deadline-ms 100]
    (let [stopped  (promise)
          spin     (fn [ms]
                     (let [end (+ (System/nanoTime) (* ms 1000000))]
                       (while (< (System/nanoTime) end))))
          h        (wrap (fn [_]
                           (loop [i 0]
                             (cond
                               (= i 200) (deliver stopped :ran-to-the-end)
                               :else (do (try (Thread/sleep (if (zero? i) 400 2))
                                           (catch InterruptedException _ nil))
                                         (spin 20)
                                         (if (.isInterrupted (Thread/currentThread))
                                           (deliver stopped :stopped)
                                           (recur (inc i))))))
                           {:status 200}))]
      (is (= 200 (:status (h (routed :branches "/api/branches" :get)))))
      (is (= :stopped (deref stopped 1000 :never))
          "the watchdog interrupts it again until it stops")
      (Thread/sleep 500)
      (is (not (.isInterrupted (Thread/currentThread)))
          "and once it answered, the thread carries no interrupt into its next request"))))
