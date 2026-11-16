(ns sutra-ui.apps.ontology.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.ontology.events :as ev]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.sync :as sync]
            [sutra.types.scaffold :as scaffold]))

(defn- calls [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(deftest a-run-input-searches-after-a-pause-from-two-letters-on
  (let [[s fx] (step {} [:ontology.run/value "ac" :flugzeug " d1 "])]
    (is (= " d1 " (get-in s [:fn-run :values "ac"])))
    (is (= [[:after {:key [:run-hits "ac"] :ms 250
                     :event [:ontology.run/search "ac" :flugzeug "d1"]}]]
           fx)))
  (is (= [[:cancel [:run-hits "ac"]]] (second (step {} [:ontology.run/value "ac" :x "d"]))))
  (let [[{:keys [call ok err]}] (calls (step {} [:ontology.run/search "ac" :flugzeug "d1"]))]
    (is (= [api/post! :ontology-query {:find :flugzeug :keywords "d1" :limit 20}] call))
    (is (= [{:id 1}] (get-in (run {} (conj ok {:rows [{:id 1}]})) [:fn-run :hits "ac"])))
    (is (= [] (get-in (run {} (conj err "weg")) [:fn-run :hits "ac"])))))

(deftest a-run-takes-the-edn-text-before-the-fields
  (is (= {:ac :d1 :n "3"}
         (ev/run-inputs {:values {"ac" " :d1 " "n" "3" "leer" " "} :inputs ""})))
  (is (= {:x 1} (ev/run-inputs {:values {"ac" "d"} :inputs "{:x 1}"})))
  (is (uuid? (:id (ev/run-inputs {:values {"id" "0f8fad5b-d9cb-469f-a165-70867728950e"}}))))
  (let [[s :as r] (step {:fn-run {:inputs "{:x 1}" :error "alt"}} [:ontology.run/start :f])
        [{[_ nm inputs] :call :keys [ok err]}] (calls r)]
    (is (nil? (get-in s [:fn-run :error])))
    (is (= [:f {:x 1}] [nm inputs]))
    (is (= {:decisions []} (get-in (run s (conj ok {:decisions []})) [:fn-run :result])))
    (is (some #{[:invoke [sync/refresh-snapshot!]]} (second (step s (conj ok {})))))
    (is (= "nein" (get-in (run s (conj err "nein")) [:fn-run :error]))))
  (testing "text that is no EDN is shown, nothing is sent"
    (let [[s fx] (step {:fn-run {:inputs "{:x"}} [:ontology.run/start :f])]
      (is (string? (get-in s [:fn-run :error])))
      (is (= [] fx)))))

(deftest a-package-is-previewed-then-staged-then-committed
  (let [[s fx] (step {:package {:staged {"p" {}}}} [:ontology.packages/enter])]
    (is (= {} (:package s)))
    (is (= [[:dispatch [:ontology.packages/load]]] fx)))
  (let [[s :as r] (step {:package {:error "alt"}} [:ontology.package/preview "p"])
        [{:keys [call ok err]}] (calls r)]
    (is (= {:busy "p" :error nil} (:package s)))
    (is (= ["p" true] (rest call)))
    (is (= {:error nil :preview {"p" {:created 1}}}
           (:package (run s (conj ok {:created 1})))))
    (is (= {:error "kaputt"} (:package (run s (conj err "kaputt"))))))
  (let [[s :as r] (step {} [:ontology.package/install "p"])
        [{:keys [ok]}] (calls r)
        [s' fx] (step s (conj ok {:staged []}))]
    (is (= {"p" {:staged []}} (get-in s' [:package :staged])))
    (is (= [[:dispatch [:ontology.packages/load {:staged []}]]
            [:invoke [sync/refresh-after-staging-change!]]]
           (remove nil? fx))))
  (let [[s fx] (step {:package {:staged {"p" {} "q" {}}}} [:ontology.package/committed "p"])]
    (is (= {"q" {}} (get-in s [:package :staged])))
    (is (= [[:dispatch [:ontology.packages/load]]] fx))))

(deftest a-delete-runs-its-effects-only-once-agreed
  (let [fx [[:dispatch [:ontology/mutate :delete-interface {:name :i}]]]
        [_ [[k {:keys [ask ok]}]]] (step {} [:ontology/confirm {:title "?"} fx])]
    (is (= :confirm k))
    (is (= {:title "?"} ask))
    (is (= fx (second (step {} ok)))))
  (is (= [[:invoke [ontology/mutate! :delete-interface {:name :i}]]]
         (second (step {} [:ontology/mutate :delete-interface {:name :i}])))))

(deftest a-function-is-specced-from-its-form
  (is (= {:name :f :target-type :flug :language :clojure
          :source scaffold/edit-function-stub
          :param-types {:ac :flugzeug}}
         (ev/function-spec {:name "f" :target-type "flug"
                            :param-types [{:role "ac" :type "flugzeug"} {:role "" :type "x"}]})))
  (let [close #(do nil)
        [{:keys [call ok]}] (calls (step {} [:ontology.function/delete :f close]))]
    (is (= [ontology/mutate! :delete-function {:name :f}] call))
    (is (= [[:invoke [close]]] (remove nil? (second (step {} (conj ok :answer))))))))

(deftest usage-is-loading-then-there-or-failed
  (let [[s :as r] (step {:usage {:flug {:writes 1}}} [:ontology.usage/load :flug])
        [{:keys [call ok err]}] (calls r)]
    (is (nil? (get-in s [:usage :flug])))
    (is (= [api/get! :object-type-usage {:type "flug"}] call))
    (is (= {:writes 2} (get-in (run s (conj ok {:writes 2})) [:usage :flug])))
    (is (= :failed (get-in (run s (conj err "weg")) [:usage :flug])))))

(deftest the-object-type-view-takes-what-it-loaded-as-baseline
  (is (= [[:ontology/load-object-form [:flug true]]]
         (second (step {} [:ontology.otv/load :flug true]))))
  (let [[s] (step {:otv {:sel #{"a"} :review? true :page :editor}}
                  [:ontology.otv/loaded :flug {:loaded-as :flug} true])]
    (is (= {:loaded-as :flug} (:otv-baseline s)))
    (is (= {:sel #{} :checked #{} :review? false :menu? false :page :editor :type :flug
            :known? true}
           (:otv s)))))

(deftest saving-stages-then-commits-or-shows-the-errors
  (let [[{:keys [call ok err]}] (calls (step {} [:ontology.otv/save :flug true]))]
    (is (= [:flug true] (rest call)))
    (is (= [[:dispatch [:ontology.otv/staged :flug nil]]]
           (remove nil? (second (step {} (conj ok nil))))))
    (is (= "nein" (get-in (run {} (conj err "nein")) [:otv :save-error]))))
  (is (= :errors (get-in (first (step {} [:ontology.otv/staged :flug ::ev/invalid]))
                         [:otv :review-tab])))
  (is (= [[sync/commit-staged!]]
         (map :call (calls (step {} [:ontology.otv/staged :flug nil])))))
  (let [[{:keys [ok]}] (calls (step {} [:ontology.otv/staged :flug nil]))
        s0 {:otv {:review? true :save-error "x"}}]
    (is (= {:review? false :save-error nil} (:otv (run s0 (conj ok {})))))
    (is (some #{[:dispatch [:ontology.otv/load :flug]]} (second (step s0 (conj ok {})))))))

(deftest the-create-wizard-needs-a-title-and-then-adds-groups-and-actions
  (is (string? (get-in (first (step {:otv-wizard {}} [:ontology.otv/create {:name "x"} []]))
                       [:otv-wizard :error])))
  (let [[{:keys [ok]}] (calls (step {} [:ontology.otv/create {:name " flug " :title "a"}
                                        [{:name :a}]]))]
    (is (= [[:dispatch [:ontology.otv/created :flug [{:name :a}] nil]]]
           (remove nil? (second (step {} (conj ok nil)))))))
  (let [[s fx] (step {:otv-wizard {:groups #{:g}} :otv {:page :security}}
                     [:ontology.otv/created :flug [{:name :a}] nil])]
    (is (nil? (:otv-wizard s)))
    (is (= :overview (get-in s [:otv :page])))
    (is (= [[:invoke [ontology/set-object-type-groups! :flug #{:g}]]
            [:invoke [ontology/mutate! :create-action {:name :a}]]
            [:invoke [object-forms/open-object-detail! :flug]]]
           fx)))
  (is (= [{:otv-wizard {:step :x}} []]
         (step {:otv-wizard {:step :x}} [:ontology.otv/created :flug [] ::ev/invalid]))))
