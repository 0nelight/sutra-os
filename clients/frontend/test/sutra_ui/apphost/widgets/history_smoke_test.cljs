(ns sutra-ui.apphost.widgets.history-smoke-test
  (:require [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [reagent.ratom :as ratom]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.widgets.history :as hw]))

(defn- setup
  []
  (let [calls (atom [])
        f     (frame/capp-frame)]
    {:f     f
     :live  (ratom/make-reaction (fn [] nil))
     :calls calls
     :env   {:frame      f
             :descriptor {:edit-history-fn
                          (fn [_ spec & _]
                            (js/Promise.
                             (fn [ok fail] (swap! calls conj [spec {:ok ok :fail fail}]))))}}}))

(defn- html
  [{:keys [env live]} cfg]
  (binding [ratom/*ratom-context* live]
    (rds/render-to-string ((:edit-history hw/widgets) cfg env))))

(defn- answer! [calls i res] ((:ok (second (nth @calls i))) res))

(defn- later
  [& fs]
  (reduce (fn [p f] (.then p (fn [_] (js/Promise. (fn [ok] (js/setTimeout #(ok (f)) 0))))))
          (js/Promise.resolve nil) fs))

(deftest edit-history-reads-once-per-key
  (async done
    (let [{:keys [f env calls] :as t} (setup)
          cfg {:object "R-1" :type :rechnung :limit 20}]
      (is (str/includes? (html t cfg) "sutra-loading"))
      (html t cfg)
      (is (= 1 (count @calls)) "the same key is not asked again")
      (is (= {:type "rechnung" :pk "R-1" :limit 20} (first (first @calls))))
      (answer! calls 0 {:history [{:action "Freigabe"}] :next-page-token "t2" :total 2})
      (later
       #(let [h (html t cfg)]
          (is (str/includes? h "Freigabe"))
          (is (= 1 (count @calls)))
          (let [[slot {:keys [key]}] (first (:reads @(:db f)))
                desc (:descriptor env)]
            (frame/send! f [::hw/more slot key
                            [(:edit-history-fn desc) desc {:pk "R-1" :token "t2"}] false])
            (answer! calls 1 {:history [{:action "Zweitens"}] :total 2})))
       #(let [h (html t cfg)]
          (is (str/includes? h "Freigabe"))
          (is (str/includes? h "Zweitens"))
          (done))))))

(deftest an-answer-to-an-older-key-is-dropped
  (async done
    (let [{:keys [calls] :as t} (setup)]
      (html t {:object "R-1" :type :rechnung :rev 1})
      (html t {:object "R-1" :type :rechnung :rev 2})
      (is (= 2 (count @calls)) "a new revision asks again")
      (answer! calls 1 {:history [{:action "Neu"}]})
      (answer! calls 0 {:history [{:action "Alt"}]})
      (later
       #(let [h (html t {:object "R-1" :type :rechnung :rev 2})]
          (is (str/includes? h "Neu"))
          (is (not (str/includes? h "Alt")) "the late answer to rev 1 is dropped")
          (testing "another object starts empty"
            (is (str/includes? (html t {:object "R-2" :type :rechnung :rev 2})
                               "sutra-loading"))
            (is (= "R-2" (:pk (first (nth @calls 2))))))
          (done))))))
