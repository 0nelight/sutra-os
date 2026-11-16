(ns ^:unit platform.live-trace-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.live-trace :as live]))

(defn- fresh! [] (live/reset-ring!))

(deftest a-poller-neither-repeats-nor-skips
  (fresh!)
  (live/emit! {:kind :build/start :run-id "r1"})
  (live/emit! {:kind :llm/request :caller "t1"})
  (let [{:keys [seq events]} (live/since nil)]
    (is (= 2 seq))
    (is (= [1 2] (mapv :seq events)) "the whole window, oldest first")

    (testing "asking again from the head yields nothing — no repeats"
      (is (= [] (:events (live/since seq)))))

    (testing "and an event emitted meanwhile is delivered exactly once"
      (live/emit! {:kind :llm/response :caller "t1"})
      (let [nx (live/since seq)]
        (is (= [3] (mapv :seq (:events nx))))
        (is (= [] (:events (live/since (:seq nx)))))))))

(deftest every-event-is-stamped-with-a-time-the-operator-can-read
  (fresh!)
  (let [before (System/currentTimeMillis)
        _      (live/emit! {:kind :build/phase :phase "transforms"})
        ev     (first (:events (live/since nil)))]
    (is (<= before (:at ev) (System/currentTimeMillis)))
    (is (= :build/phase (:kind ev)) "the caller's own keys survive untouched")))

(deftest a-poller-that-falls-behind-is-told-so
  (fresh!)
  (dotimes [_ (+ live/capacity 10)] (live/emit! {:kind :llm/request}))
  (let [{:keys [events dropped seq]} (live/since 1)]
    (is (= live/capacity (count events)) "the window is bounded, not the stream")
    (is (= (+ live/capacity 10) seq) "seq keeps counting past what is retained")
    (is (pos? dropped) "…and the gap is named")
    (is (= 9 dropped)
        "exactly the events between the caller's cursor and the oldest one held"))

  (testing "a caller that is keeping up is told nothing was dropped"
    (fresh!)
    (live/emit! {:kind :build/start})
    (is (zero? (:dropped (live/since 1))))
    (is (zero? (:dropped (live/since nil))) "nor on a first, cursorless poll")))

(deftest free-text-is-bounded-because-a-prompt-is-an-email-body
  (is (nil? (live/clip nil)))
  (is (= "a b c" (live/clip "a\n  b\t\nc")))
  (let [long-s (apply str (repeat 5000 \x))
        out    (live/clip long-s)]
    (is (= (inc live/preview-chars) (count out)) "capped, plus the ellipsis")
    (is (.endsWith ^String out "…") "and visibly truncated, not silently cut")))

(deftest the-ring-does-no-io
  (let [deps (->> (ns-refers 'sutra.foundation.live-trace)
                  vals
                  (keep #(some-> % meta :ns str))
                  set)]
    (is (empty? (filter #(re-find #"^(storage|access|catalog|indexer)\." %) deps))
        "the live channel must not be able to touch the control plane")))
