(ns ^:unit ontology.schedule-must-be-targeted-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.schedules.schedules :as schedules]
            [sutra.parser.ent :as ent]))

(defn- armable [spec] (try (schedules/assert-armable! spec) :ok
                           (catch clojure.lang.ExceptionInfo e e)))
(defn- fireable [spec] (try (schedules/assert-fireable! spec) :ok
                            (catch clojure.lang.ExceptionInfo e e)))

(deftest an-untargeted-schedule-cannot-be-armed
  (testing "creating one is refused"
    (let [e (armable {:name "ontology-reindex" :every-ms 3600000})]
      (is (instance? clojure.lang.ExceptionInfo e))
      nil))

  (testing "a blank string is the same as absent — not a loophole"
    (is (instance? clojure.lang.ExceptionInfo (armable {:name "x" :target ""})))
    (is (instance? clojure.lang.ExceptionInfo (armable {:name "x" :target "   "}))))

  (testing "the refusal says what to do instead, including the discovery verb"
    (let [m (ex-message (armable {:name "x"}))]
      (is (re-find #":scope" m))
      (is (re-find #"parent-updated" m))
      nil))

  (testing "a targeted schedule passes both doors"
    (is (= :ok (armable  {:name "mro-sync" :target "mro_lieferantenrechnung_raw"})))
    (is (= :ok (fireable {:name "mro-sync" :target "mro_lieferantenrechnung_raw"})))))

(deftest off-is-always-allowed-on-never-is
  (testing "an untargeted schedule may be PAUSED — the write door lets it through"
    (is (= :ok (armable {:name "ontology-reindex" :paused true}))))

  (testing "…but pausing does not make it runnable: the ACT door refuses anyway"
    (let [e (fireable {:name "ontology-reindex" :paused true})]
      (is (instance? clojure.lang.ExceptionInfo e))
      nil))

  (testing "and un-pausing it is refused — there is no way back"
    (is (instance? clojure.lang.ExceptionInfo
                   (armable {:name "ontology-reindex" :paused false :every-ms 3600000})))))

(deftest the-ent-door-refuses-it-too
  (let [ent-of (fn [sched] {:schema {:objects {}} :schedules [sched]})
        bad?   (fn [sched] (some? (ent/explain (ent-of sched))))]
    (testing "no :target is a grammar error, not a default"
      (is (bad? {:name "ontology-reindex" :every-ms 3600000})))
    (testing "a blank :target is refused by the same rule"
      (is (bad? {:name "x" :target "" :every-ms 1000})))
    (testing "a named target is accepted"
      (is (not (bad? {:name "mro-sync" :target "mro_lieferantenrechnung_raw"
                      :every-ms 3600000}))))))
