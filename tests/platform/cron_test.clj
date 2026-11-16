(ns ^:unit platform.cron-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.cron :as cron])
  (:import (java.time LocalDateTime)))

(defn- at [y mo d h mi] (LocalDateTime/of y mo d h mi 0))
(def ^:private mon-2145 (at 2026 7 6 21 45))
(defn- due? [c last-fired now] (cron/due? c last-fired now java.time.ZoneOffset/UTC))

(deftest matches-every-15
  (testing "*/15 fires on the quarter-hour only"
    (is ((cron/matcher "*/15 * * * *") (at 2026 7 6 21 45)))
    (is ((cron/matcher "*/15 * * * *") (at 2026 7 6 22 0)))
    (is ((cron/matcher "*/15 * * * *") (at 2026 7 6 9 30)))
    (is (not ((cron/matcher "*/15 * * * *") (at 2026 7 6 21 47))))
    (is (not ((cron/matcher "*/15 * * * *") (at 2026 7 6 21 44))))))

(deftest matches-daily-and-fields
  (testing "0 2 * * * = 02:00 exactly"
    (is ((cron/matcher "0 2 * * *") (at 2026 7 6 2 0)))
    (is (not ((cron/matcher "0 2 * * *") (at 2026 7 6 3 0))))
    (is (not ((cron/matcher "0 2 * * *") (at 2026 7 6 2 1)))))
  (testing "lists + ranges"
    (is ((cron/matcher "0,30 * * * *") (at 2026 7 6 9 30)))
    (is ((cron/matcher "0 9-17 * * *") (at 2026 7 6 12 0)))
    (is (not ((cron/matcher "0 9-17 * * *") (at 2026 7 6 18 0)))))
  (testing "day-of-week (0=Sun; Mon=1)"
    (is ((cron/matcher "30 8 * * 1") (at 2026 7 6 8 30)))
    (is (not ((cron/matcher "30 8 * * 1") (at 2026 7 7 8 30))))
    (is ((cron/matcher "0 0 * * 7") (at 2026 7 5 0 0))))
  (testing "malformed cron never throws, never matches"
    (is (not ((cron/matcher "not a cron") mon-2145)))
    (is (not ((cron/matcher "* * *") mon-2145)))))

(deftest due-fires-once-per-slot
  (testing "due? matches the minute AND hasn't already fired it"
    (is (due? "*/15 * * * *" nil mon-2145) "never fired ⇒ due")
    (is (due? "*/15 * * * *" (at 2026 7 6 21 30) mon-2145) "last fired a prior slot ⇒ due")
    (is (not (due? "*/15 * * * *" (LocalDateTime/of 2026 7 6 21 45 20) mon-2145))
        "already fired THIS minute ⇒ not due")
    (is (not (due? "*/15 * * * *" nil (at 2026 7 6 21 47)))
        "off-slot minute ⇒ not due even if never fired")))

(deftest a-missed-minute-is-caught-up-once
  (testing "the node was down (or a build ran) at 02:00: the next pass still fires, once"
    (is (due? "0 2 * * *" (at 2026 7 5 2 0) (at 2026 7 6 9 0))
        "02:00 passed since the last fire ⇒ due")
    (is (not (due? "0 2 * * *" (at 2026 7 6 9 0) (at 2026 7 6 9 30)))
        "stamped with the catch-up fire ⇒ not due again until the next 02:00")
    (is (due? "0 2 * * *" (at 2026 7 6 9 0) (at 2026 7 7 2 0)))))
