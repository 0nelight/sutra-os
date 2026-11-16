(ns ^:unit ontology.schedule-triggers-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [sutra.types.naming :as naming]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.substrate :as sub]
            [sutra.schedules.schedules :as schedules]
            [sutra.schedules.triggers :as triggers])
  (:import (java.time LocalDateTime)))

(def ^:private schema
  {:object-types {"ri.email" {:object-type-name :email :plural "Emails"}}
   :datasets     {"ri.raw"   {:dataset-name :raw_messages}}
   :transforms   {"ri.pull"  {:transform-name :pull}}})

(deftest watched-parents-test
  (testing "a :parent-updated trigger yields its watched parent name keywords"
    (is (= [:email] (triggers/watched-parents
                     {:trigger {:kind :parent-updated :of :email}})))
    (is (= [:email :raw_messages]
           (triggers/watched-parents
            {:trigger {:kind :parent-updated :of [:email "raw_messages"]}}))
        "a vector :of is supported and string names are coerced to keywords")
    (is (= [:email] (triggers/watched-parents
                     {:trigger {:kind :parent-updated :of [:email :email nil]}}))
        "nils dropped, duplicates collapsed"))
  (testing "a schedule with no parent-updated trigger watches nothing"
    (is (nil? (triggers/watched-parents {:trigger nil})))
    (is (nil? (triggers/watched-parents {:trigger {:kind :cadence :every-ms 1000}})))
    (is (nil? (triggers/watched-parents {:trigger {:kind :logic-updated}}))
        "logic-updated is a distinct (not-yet-wired) event kind, not parent-updated"))
  (testing "a parent nested inside a compound is still a watched parent"
    (is (= [:raw_messages]
           (triggers/watched-parents
            {:trigger {:kind :all-of
                       :triggers [{:kind :cadence :every-ms 3600000}
                                  {:kind :parent-updated :of :raw_messages}]}})))
    (is (= [:a :b]
           (triggers/watched-parents
            {:trigger {:kind :any-of
                       :triggers [{:kind :parent-updated :of :a}
                                  {:kind :all-of
                                   :triggers [{:kind :parent-updated :of [:b :a]}]}]}}))
        "arbitrary nesting, de-duplicated across arms")))

(deftest leaves-are-identified-by-path-not-by-content
  (let [t {:kind :any-of
           :triggers [{:kind :parent-updated :of :x}
                      {:kind :all-of :triggers [{:kind :cadence :cron "0 2 * * *"}
                                                {:kind :parent-updated :of :x}]}]}]
    (is (= [[0] [1 0] [1 1]] (mapv first (triggers/leaves t))))
    (is (= [:parent-updated :cadence :parent-updated]
           (mapv (comp :kind second) (triggers/leaves t)))))
  (testing "a bare leaf is the root itself"
    (is (= [[[] {:kind :cadence :every-ms 5}]]
           (triggers/leaves {:kind :cadence :every-ms 5}))))
  (testing "nil has no leaves"
    (is (= [] (triggers/leaves nil)))))

(deftest satisfied-implements-and-or
  (let [cadence {:kind :cadence :every-ms 1}
        parent  {:kind :parent-updated :of :x}]
    (testing "AND needs every arm; OR needs one"
      (let [t {:kind :all-of :triggers [cadence parent]}]
        (is (true?  (triggers/satisfied? t {[0] true  [1] true})))
        (is (false? (triggers/satisfied? t {[0] true  [1] false})))
        (is (false? (triggers/satisfied? t {[0] false [1] true}))))
      (let [t {:kind :any-of :triggers [cadence parent]}]
        (is (true?  (triggers/satisfied? t {[0] false [1] true})))
        (is (false? (triggers/satisfied? t {[0] false [1] false})))))

    (testing "arbitrary nesting"
      (let [t {:kind :all-of
               :triggers [cadence {:kind :any-of :triggers [parent parent]}]}]
        (is (true?  (triggers/satisfied? t {[0] true [1 0] false [1 1] true})))
        (is (false? (triggers/satisfied? t {[0] true [1 0] false [1 1] false})))
        (is (false? (triggers/satisfied? t {[0] false [1 0] true [1 1] true})))))

    (testing "a bare leaf is its own truth"
      (is (true?  (triggers/satisfied? parent {[] true})))
      (is (false? (triggers/satisfied? parent {}))
          "an unknown leaf is NOT satisfied — missing evidence is not evidence"))

    (testing "no trigger is not satisfied"
      (is (false? (triggers/satisfied? nil {}))))))

(deftest a-trigger-that-cannot-fire-is-refused-at-the-write-door
  (let [refused (fn [t] (try (triggers/assert-evaluatable! t) nil
                             (catch clojure.lang.ExceptionInfo e e)))]
    (testing "an undecidable leaf is refused, wherever it is nested"
      (is (some? (refused {:kind :logic-updated :of :x})))
      (is (some? (refused {:kind :all-of :triggers [{:kind :cadence :every-ms 1}
                                                    {:kind :logic-updated}]}))))
    (testing "the refusal is a CLIENT error, so the membrane can render it 400"
      (is (anomaly/client-error? (ex-data (refused {:kind :logic-updated})))))
    (testing "an empty compound is refused in both directions"
      (is (some? (refused {:kind :all-of :triggers []})))
      (is (some? (refused {:kind :any-of :triggers []}))))
    (testing "the decidable shapes pass, nil included"
      (is (nil? (refused nil)))
      (is (nil? (refused {:kind :parent-updated :of :x})))
      (is (nil? (refused {:kind :all-of
                          :triggers [{:kind :cadence :cron "0 2 * * *"}
                                     {:kind :any-of
                                      :triggers [{:kind :parent-updated :of :x}]}]}))))))

(deftest a-cadence-leaf-decides-like-the-columns-it-mirrors
  (let [now (LocalDateTime/of 2026 8 6 9 0 0)
        ago (fn [mins] (java.sql.Timestamp/valueOf (.minusMinutes now mins)))]
    (testing ":every-ms is an INTERVAL since the last fire"
      (is (true?  (triggers/time-leaf-due? {:kind :cadence :every-ms 3600000} (ago 61) now)))
      (is (false? (triggers/time-leaf-due? {:kind :cadence :every-ms 3600000} (ago 59) now)))
      (is (true?  (triggers/time-leaf-due? {:kind :cadence :every-ms 3600000} nil now))
          "never fired ⇒ due, matching the column query's COALESCE(last_fired_at, created_at)"))
    (testing ":cron is WALL-CLOCK, and every-ms wins when both are present"
      (is (true?  (triggers/time-leaf-due? {:kind :cadence :cron "0 9 * * *"} (ago 600) now)))
      (is (true?  (triggers/time-leaf-due? {:kind :cadence :cron "0 2 * * *"} (ago 600) now))
          "02:00 passed since the last fire at 23:00 — a missed slot is caught up")
      (is (false? (triggers/time-leaf-due? {:kind :cadence :cron "0 2 * * *"} (ago 300) now))
          "last fired at 04:00, after today's 02:00 ⇒ nothing pending")
      (is (false? (triggers/time-leaf-due? {:kind :cadence :every-ms 3600000 :cron "0 9 * * *"}
                                           (ago 5) now))
          "the interval is not up, and the cron must not override it — same precedence as list-due"))
    (testing "a cadence naming no instant is never due"
      (is (false? (triggers/time-leaf-due? {:kind :cadence} nil now)))
      (is (false? (triggers/time-leaf-due? {:kind :cadence :every-ms 0} nil now))))))

(deftest parent-signature-resolution-test
  (let [parent-signature #'sutra.schedules.triggers/parent-signature]
    (with-redefs [sub/type-clock          (fn [_ ot] (str "clock of " ot))
                  dsets/content-signature (fn [_ t] (str "content of " t))]
      (testing "an object type stands on its store clock, never on the store's own table"
        (is (= [:clock "clock of ri.email"] (parent-signature {} schema :email))))
      (testing "a dataset or a transform's output stands on its d_ table's content"
        (is (= (str "content of " (naming/dataset-table "ri.raw"))
               (parent-signature {} schema :raw_messages)))
        (is (= (str "content of " (naming/dataset-table "ri.pull"))
               (parent-signature {} schema :pull))))
      (testing "an unknown parent has no signature (never throws) ⇒ the schedule just never fires"
        (is (nil? (parent-signature {} schema :does-not-exist)))))))

(deftest watching-names-the-armed-schedules-on-a-dataset
  (with-redefs [schedules/list-all
                (constantly [{:name "b" :trigger {:kind :parent-updated :of :uploads}}
                             {:name "a" :trigger {:kind :any-of
                                                  :triggers [{:kind :cadence :every-ms 1000}
                                                             {:kind :parent-updated
                                                              :of ["uploads" :x]}]}}
                             {:name "pausiert" :paused true
                              :trigger {:kind :parent-updated :of :uploads}}
                             {:name "fremd" :trigger {:kind :parent-updated :of :andere}}
                             {:name "uhr" :cron "0 * * * *"}])]
    (is (= ["a" "b"] (schedules/watching nil nil "uploads")))
    (is (= [] (schedules/watching nil nil :nichts)))))
