(ns sutra-ui.apphost.sci-sandbox-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.apphost.sci-runtime :as sci-rt]))

(defn- run
  [expr]
  (let [c (sci-rt/compile-view (str "(fn [_ _] " expr ")"))
        v (c nil nil)]
    (if (and (vector? v) (= {:class "capp capp--error"} (second v))) ::refused v)))

(defn- blows-up? [expr]
  (try (run expr) false (catch :default _ true)))

(deftest an-app-keeps-the-harmless-globals
  (is (= 2 (run "(js/Math.max 1 2)")))
  (is (= 3 (run "(Math/floor 3.7)")))
  (is (= "A" (run "(.toUpperCase \"a\")")))
  (is (= 2020 (run "(.getFullYear (js/Date. 2020 1 1))")))
  (is (= "{\"a\":1}" (run "(js/JSON.stringify (clj->js {:a 1}))")))
  (is (string? (run "(.format (js/Intl.NumberFormat. \"de\") 1.5)")))
  (is (true? (run "(instance? js/Error (js/Error. \"x\"))")))
  (is (= 12 (run "(js/parseInt \"12\")"))))

(deftest an-app-reaches-no-way-out
  (testing "globals that are not handed out"
    (doseq [g ["js/fetch" "js/document" "js/window" "js/localStorage" "js/eval"
               "js/Function" "js/globalThis" "js/XMLHttpRequest" "js/Object"]]
      (is (nil? (run g)) g)))
  (testing "the constructor chain back to Function"
    (is (nil? (run "js/Math.max.constructor")))
    (is (nil? (run "js/Date.prototype")))
    (is (blows-up? "(.-constructor (.-constructor \"x\"))"))
    (is (blows-up? "(.-foo (js-obj))")))
  (testing "a timer takes no string to eval"
    (is (nil? (run "(js/setTimeout \"globalThis.pwned=1\" 0)"))))
  (testing "raw access is refused at compile time"
    (doseq [e ["(aget \"\" \"constructor\")" "(type 1)" "(set! (.-x (js-obj)) 1)"
               "(js-delete (js-obj) \"x\")"]]
      (is (= ::refused (run e)) e))))
