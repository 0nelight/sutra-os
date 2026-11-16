(ns sutra-ui.state.machine-smoke-test
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.debug :as debug]
            [sutra-ui.state.machine :as machine]))

(defonce ^:private seen (atom []))

(machine/reg-fx ::seen (fn [_ x] (swap! seen conj x)))

(defn- reg [id f] (machine/reg-event id (fn [{:keys [db]} [_ x]] (f db x))))

(reg ::ask (fn [db x] {:db (assoc db :asked x)
                       :fx [[:call {:call [(fn [v] (js/Promise.resolve (* 2 v))) x]
                                    :ok [::answer] :err [::failed]}]]}))
(reg ::fail (fn [_ _] {:fx [[:call {:call [(fn [] (js/Promise.reject (js/Error. "weg")))]
                                    :ok [::answer] :err [::failed]}]]}))
(reg ::wait (fn [_ x] {:fx [[:call {:call [(fn [] x)] :ok [::answer]}]]}))
(reg ::later (fn [_ _] {:fx [[:after {:key :t :ms 5 :event [::answer :late]}]]}))
(reg ::answer (fn [db x] {:db (assoc db :answer x) :fx [[:dispatch [::own x]]]}))
(reg ::failed (fn [db x] {:db (assoc db :failed x)}))
(reg ::own (fn [_ x] {:fx [[::seen [:own x]]]}))
(reg ::go (fn [db _] {:db (assoc db :n 1)
                      :fx [[::seen :first] [:dispatch [::inner]] [::seen :last]]}))
(reg ::inner (fn [db _] {:db (update db :n inc) :fx [[::seen [:inner (:n db)]]]}))
(reg ::bump (fn [db _] {:db (update db :n inc)}))
(reg ::same (fn [_ _] nil))
(reg ::kept (fn [db _] {:db db}))
(reg ::odd (fn [_ _] {:fx [[:no-such-effect]]}))

(defn- send! [f event] (machine/dispatch event {:frame f}))

(deftest a-call-answers-with-an-event-through-a-registered-effect
  (async done
    (let [state (atom {})
          f     (machine/frame state {:name :t})]
      (reset! seen [])
      (send! f [::ask 21])
      (is (= 21 (:asked @state)))
      (js/setTimeout (fn []
                       (is (= 42 (:answer @state)))
                       (is (= [[:own 42]] @seen))
                       (done))
                     0))))

(deftest a-failure-answers-with-its-sentence
  (async done
    (let [state (atom {})
          f     (machine/frame state {:name :t})]
      (send! f [::fail])
      (js/setTimeout (fn [] (is (= "weg" (:failed @state))) (done)) 0))))

(deftest an-unchanged-db-is-not-reset-and-unknown-ids-throw
  (let [state  (atom {:a 1})
        resets (atom 0)
        f      (machine/frame state {:name :t})]
    (add-watch state ::n (fn [& _] (swap! resets inc)))
    (send! f [::same])
    (send! f [::kept])
    (is (= 0 @resets))
    (is (thrown? js/Error (send! f [::odd])))
    (send! f [::same])
    (is (= 0 @resets) "a throwing event leaves the queue free for the next")
    (is (thrown-with-msg? js/Error #"::nope|/nope" (send! f [::nope])))
    (is (thrown? js/Error (machine/dispatch [::same] {})) "an event needs a frame")))

(deftest an-event-dispatched-by-an-effect-runs-after-the-remaining-effects
  (let [state (atom {})
        f     (machine/frame state {:name :t})]
    (reset! seen [])
    (send! f [::go])
    (is (= [:first :last [:inner 1]] @seen))
    (is (= 2 (:n @state)) "outside dispatch returns once all it caused has run")))

(deftest a-late-answer-is-dropped-after-the-view-left-a-normal-one-is-not
  (async done
    (let [state  (atom {})
          f      (machine/frame state {:name :t :lifetime :view})
          app    (machine/frame (atom {}) {:name :t})
          before (js/Promise.resolve 1)
          gate   (atom nil)
          late   (js/Promise. (fn [ok] (reset! gate ok)))]
      (send! f [::wait before])
      (js/setTimeout
       (fn []
         (is (= 1 (:answer @state)) "an answer before any view mounted arrives")
         (send! f [::wait late])
         (send! app [::wait late])
         (machine/leave! f)
         (machine/leave! app)
         (@gate 2)
         (js/setTimeout (fn []
                          (is (= 1 (:answer @state)) "the answer that crossed the leave is gone")
                          (is (= 2 (:answer @(:db app))) "an :app frame outlives its views")
                          (done))
                        0))
       0))))

(deftest leaving-cancels-pending-timers
  (async done
    (let [state (atom {})
          f     (machine/frame state {:name :t :lifetime :view})]
      (send! f [::later])
      (machine/leave! f)
      (is (empty? @(:timers f)))
      (js/setTimeout (fn [] (is (nil? (:answer @state))) (done)) 20))))

(machine/reg-sub ::doubled (fn [db [_ k]] (* 2 (get db k))))

(defonce ^:private computed (atom 0))

(machine/reg-sub ::counted (fn [db _] (swap! computed inc) (:n db)))

(machine/reg-sub ::sum {:inputs [[::counted] [::doubled :n]]} (fn [[n d] _] (+ n d)))

(deftest a-sub-is-computed-once-per-db-change-for-all-its-readers
  (let [f   (machine/frame (r/atom {:n 1}) {:name :t})
        sub #(machine/subscribe % {:frame f})
        a   (r/track! #(deref (sub [::counted])))
        b   (r/track! #(deref (sub [::counted])))]
    (reset! computed 0)
    (r/flush)
    (is (= [1 1] [@a @b]))
    (is (= 2 @(sub [::doubled :n])))
    (send! f [::bump])
    (r/flush)
    (is (= [2 2] [@a @b]))
    (is (= 1 @computed) "one computation serves both readers")
    (send! f [::same])
    (r/flush)
    (is (= 1 @computed) "an unchanged db computes nothing")
    (is (= 6 @(sub [::sum])) "a derived sub reads its :inputs")
    (r/dispose! a)
    (r/dispose! b)))

(defonce ^:private lines (atom []))

(defonce sink
  (debug/add-sink! #(when (= :state (:level %)) (swap! lines conj %))))

(deftest every-event-writes-one-trace-line-without-its-args
  (let [f (machine/frame (atom {}) {:name :probe})]
    (reset! lines [])
    (send! f [::go])
    (let [mine (filter #(= ":probe" (:src %)) @lines)]
      (is (= 2 (count mine)))
      (is (str/starts-with? (:msg (first mine))
                            ":sutra-ui.state.machine-smoke-test/go [:sutra-ui.state.machine"))
      (is (str/includes? (:msg (second mine)) "/inner"))
      (is (not-any? #(str/includes? (:msg %) "secret") mine)))
    (reset! lines [])
    (send! f [::own "secret"])
    (is (not-any? #(str/includes? (:msg %) "secret") @lines))))
