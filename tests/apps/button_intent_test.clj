(ns ^:unit apps.button-intent-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.intent :as ci]
            [sutra.parser.ent :as ent]))

(deftest only-a-purely-destructive-action-is-danger
  (is (= :danger (ci/action-intent {:rules [{:op :delete-object}]})))
  (is (= :danger (ci/action-intent {:rules [{:op :if :then [{:op :delete-object}]}]})))
  (is (nil? (ci/action-intent {:rules [{:op :delete-object} {:op :create}]})))
  (is (nil? (ci/action-intent {:rules [{:op :update}]})))
  (is (nil? (ci/action-intent {}))))

(deftest a-group-keeps-one-primary
  (let [btn  (fn [& kv] (apply hash-map :widget :action-button kv))
        prim #(= :action-button (:widget %))]
    (is (= [nil :none :danger :none]
           (mapv :intent (ci/one-primary prim [(btn) (btn) (btn :intent :danger) (btn)]))))
    (testing "an authored primary claims the slot first"
      (is (= [:primary :none]
             (mapv :intent (ci/one-primary prim [(btn :intent :primary) (btn)])))))))

(deftest no-intent-is-the-primary-button
  (is (= {:intent :primary} (ci/btn-props :primary)))
  (is (= {:intent :primary} (ci/btn-props nil)))
  (is (= {} (ci/btn-props :none)))
  (is (= {:intent :danger} (ci/btn-props "danger"))))

(defn- app [layout]
  (ent/read-text :app "t.enta"
                 (pr-str {:name :t :layout (assoc layout :vars {:x {:default nil}})})))

(deftest the-door-knows-the-new-keys
  (is (map? (app {:kind :section :title "T"
                  :tag {:widget :kpi :display :tag :label "n" :value 1
                        :on-click {:do :set :var {:var :x} :to 1}}
                  :body {:kind :loop :items [] :arrange :grid :max-columns 3
                         :min-card-width 240
                         :body {:widget :button :label "b" :intent :none
                                :on-click {:do :set :var {:var :x} :to 1}}}})))
  (is (map? (app {:widget :kpi-row :display :card
                  :items [{:label "a" :value 1 :on-click {:do :set :var {:var :x} :to 1}}]})))
  (testing "and refuses what is not an Intent"
    (is (thrown? Exception (app {:widget :button :label "b" :intent :blue
                                 :on-click {:do :set :var {:var :x} :to 1}})))
    (is (thrown? Exception (app {:widget :kpi :label "a" :value 1 :display :list})))))
