(ns sutra-ui.apphost.events-smoke-test
  (:require [cljs.test :refer [deftest is testing async]]
            [reagent.core :as r]
            [sutra.capps.warn :as warn]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.events :as events]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.state :as state]))

(deftest set-writes-the-variable-with-and-without-a-frame
  (testing "without a frame the handle is reset at once"
    (let [h (r/atom nil)]
      (events/run-effects! {:do :set :var h :to 5})
      (is (= 5 @h))))
  (testing "with a frame a cursor of its db is written through its events"
    (let [f (frame/capp-frame {:v 1})
          c (frame/cursor f [:v])]
      (events/run-effects! [{:do :set :var c :to {:row :n}}] {:n 7} {:frame f})
      (is (= 7 (:v @(:db f))))))
  (testing "a value that is no variable is left alone"
    (is (nil? (events/run-effects! {:do :set :var 3 :to 5} nil {:frame (frame/capp-frame)})))))

(deftest open-and-close-an-overlay-in-the-frame
  (let [f  (frame/capp-frame)
        ov (state/overlays)]
    (events/run-effects! {:do :open :overlay :d :arg 4} nil {:frame f :overlays ov})
    (is (= 4 (state/overlay-arg ov :d)))
    (events/run-effects! {:do :close :overlay :d} nil {:frame f :overlays ov})
    (is (not (state/open? ov :d)))))

(defn- held
  []
  (let [calls (atom [])]
    [calls (fn [_ body]
             (js/Promise. (fn [ok fail] (swap! calls conj {:body body :ok ok :fail fail}))))]))

(defn- later
  [f]
  (js/Promise. (fn [ok] (js/setTimeout #(ok (f)) 0))))

(def ^:private doc-effect {:do :document :template "beleg"})

(defn- document-run
  [f settle check]
  (let [[calls stub] (held)
        h            (r/atom nil)
        warned       (atom [])
        real-doc     client/document!
        real-warn    warn/warn!]
    (set! client/document! stub)
    (set! warn/warn! #(swap! warned conj %))
    (events/run-effects! (assoc doc-effect :var h) nil {:descriptor {:id "a"} :frame f})
    (is (= "beleg" (get-in @calls [0 :body :template])))
    (settle (first @calls))
    (later #(do (set! client/document! real-doc)
                (set! warn/warn! real-warn)
                (check h warned)))))

(deftest document-answers-into-the-variable
  (async done
    (-> (document-run nil #((:ok %) {:url "blob:x"})
                      (fn [h _] (is (= "blob:x" @h) "without a frame: the answer lands in :var")))
        (.then #(document-run (frame/capp-frame) (fn [c] ((:ok c) {:url "blob:y"}))
                              (fn [h _] (is (= "blob:y" @h) "with a frame"))))
        (.then #(document-run (frame/capp-frame) (fn [c] ((:fail c) (js/Error. "kaputt")))
                              (fn [_ warned]
                                (is (= ["app/document: kaputt"] @warned)
                                    "a refusal is said, not swallowed"))))
        (.then #(done)))))

(deftest an-answer-after-the-app-left-is-dropped
  (async done
    (let [f (frame/capp-frame)]
      (-> (document-run f (fn [c] (frame/leave! f) ((:ok c) {:url "blob:late"}))
                        (fn [h _] (is (nil? @h))))
          (.then #(done))))))
