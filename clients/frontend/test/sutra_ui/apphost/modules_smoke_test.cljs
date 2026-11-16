(ns sutra-ui.apphost.modules-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [reagent.dom.server :as rds]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.modules :as mod]
            [sutra-ui.apphost.sci-runtime :as sci-rt]))

(deftest module-interface-is-typed-and-loud
  (let [m (app/module {:inputs {:region :keyword} :outputs {:sel :int}}
                      (fn [_] [:div]))]
    (is (mod/module? m) "app/module yields a Module value")
    (testing "inputs"
      (is (= {:region :ost} (mod/validate-inputs m {:region :ost})) "valid input passes")
      (is (thrown? js/Error (mod/validate-inputs m {:region "ost"})) "wrong type is rejected")
      (is (thrown? js/Error (mod/validate-inputs m {:nope 1})) "an undeclared input is rejected"))
    (testing "outputs"
      (is (= [:sel 7] (mod/validate-output m :sel 7)) "valid output passes")
      (is (thrown? js/Error (mod/validate-output m :sel "7")) "wrong type is rejected")
      (is (thrown? js/Error (mod/validate-output m :other 1)) "an undeclared output is rejected"))))

(deftest embed-pairs-host-input-and-child-output
  (let [got   (atom nil)
        child (app/module {:inputs {:region :keyword} :outputs {:sel :int}}
                          (fn [{:keys [in emit]}]
                            (emit :sel (count (name (:region in))))
                            [:div "region=" (name (:region in))]))
        html  (rds/render-to-string
               [(fn [] [app/embed {:module    child
                                   :inputs    {:region :ost}
                                   :on-output (fn [k v] (reset! got [k v]))}])])]
    (is (re-find #"region=(<!-- -->)?ost" html) "the host input reaches the child view")
    (is (= [:sel 3] @got)
        "the child's emit flows through the validated interface to the host's on-output")))

(deftest embed-validates-a-bad-host-input-at-the-boundary
  (let [child (app/module {:inputs {:region :keyword}} (fn [_] [:div]))]
    (is (thrown? js/Error
                 (rds/render-to-string
                  [(fn [] [app/embed {:module child :inputs {:region "ost"}}])]))
        "a wrong-typed input from the host throws where it's caught, not deep inside the child")))

(deftest embed-without-a-loaded-module-shows-a-placeholder
  (let [html (rds/render-to-string
              [(fn [] [app/embed {:module nil :inputs {}}])])]
    (is (re-find #"capp-error" html) "nothing-loaded-yet renders an error box, not a crash")))

(deftest module-source-compiles-and-renders-standalone
  (let [src  (str "(app/module\n"
                  "  {:inputs {:region :keyword} :outputs {:sel :int}}\n"
                  "  (fn [{:keys [in emit]}]\n"
                  "    [:div {:class \"capp\"} \"r=\" (name (:region in))]))\n")
        comp (sci-rt/compile-view src)]
    (is (mod/module? comp) "an app/module final form compiles to a Module")
    (let [html (rds/render-to-string
                (sci-rt/render {:source src :layout {:module-inputs {:region :nord}}} {}))]
      (is (re-find #"r=(<!-- -->)?nord" html)
          "opened standalone, inputs come from :layout :module-inputs (validated)"))))

(deftest plain-fn-view-still-renders-backward-compatible
  (let [src  "(fn [descriptor data] [:div {:class \"capp\"} \"plain\"])"
        html (rds/render-to-string (sci-rt/render {:source src} {}))]
    (is (re-find #"plain" html) "a non-module view compiles and renders exactly as before")))
