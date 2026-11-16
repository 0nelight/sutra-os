(ns sutra-ui.apphost.recompute-smoke-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [reagent.core :as r]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.scope-handles :as handles]))

(def ^:private handle #'handles/recomputing-handle)

(defn- seite []
  (let [f (frame/capp-frame {:generation 0})]
    {:f f :at {:frame f :sid :root} :desc {:app/generation (frame/cursor f [:generation])}}))

(defn- aufbau
  [mode]
  (let [{:keys [at desc] :as s} (seite)
        fragen (atom 0)
        h      (handle at :k desc mode (fn [] [:args])
                       (fn [_] {:ask [(fn [] (swap! fragen inc)
                                           (js/Promise.resolve {:rows []}))]}))]
    (assoc s :fragen fragen :h h)))

(defn- neu-laden! [{:keys [f]}] (frame/send! f [:capp/update [:generation] inc []]) (r/flush))

(deftest automatisch-rechnet-bei-jedem-neuladen
  (async done
    (let [{:keys [fragen h] :as a} (aufbau nil)]
      (is (= 1 @fragen))
      (js/setTimeout
       (fn []
         (is (= {:rows [] :loading? false} @h))
         (neu-laden! a)
         (is (= 2 @fragen) "Refresh data in module: alle Daten, auch eine Kennzahl")
         (done))
       0))))

(deftest nur-durch-ein-ereignis-rechnet-erst-auf-das-ereignis
  (let [{:keys [fragen h] :as a} (aufbau :event)]
    (is (= 0 @fragen))
    (neu-laden! a)
    (is (= 0 @fragen))
    ((:recompute! (meta h)))
    (r/flush)
    (is (= 1 @fragen))))

(deftest beim-laden-und-durch-ein-ereignis
  (let [{:keys [fragen h] :as a} (aufbau :load-and-event)]
    (is (= 1 @fragen))
    (neu-laden! a)
    (is (= 1 @fragen))
    ((:recompute! (meta h)))
    (r/flush)
    (is (= 2 @fragen))))

(deftest das-verlassen-beendet-das-nachrechnen
  (let [{:keys [f fragen] :as a} (aufbau nil)]
    (frame/leave! f)
    (neu-laden! a)
    (is (= 1 @fragen) "die Reaktion lief frueher ueber das Ende der App hinaus weiter")))

(def ^:private function-handle #'handles/make-function-handle)

(deftest eine-variable-in-args-geht-als-wert-und-rechnet-neu
  (let [gesendet (atom [])
        suche    (r/atom "a")
        {:keys [at desc]} (seite)]
    (with-redefs [client/function! (fn [_ spec & _]
                                      (swap! gesendet conj (:args spec))
                                      (js/Promise. (fn [_ _])))]
      (function-handle at :f desc {:function :f :args {:suche {:var :q} :fest 1}}
                       {:q suche} {} nil nil)
      (is (= [{:suche "a" :fest 1}] @gesendet))
      (reset! suche "b")
      (r/flush)
      (is (= {:suche "b" :fest 1} (last @gesendet))))))
