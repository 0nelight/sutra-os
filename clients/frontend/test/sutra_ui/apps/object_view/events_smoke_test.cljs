(ns sutra-ui.apps.object-view.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.object-view.events :as ev]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.machine-testing :refer [requests step]]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]))

(defn- asks [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (asks r)))

(deftest a-key-opens-its-one-object-or-says-why-not
  (is (= [[api/post! :ontology-query {:find :kunde :where {:nr {:= "7"}} :limit 2}]]
         (calls (step {} [:object-view/by-key :kunde [:nr "7"]]))))
  (is (= [[:invoke [shell/enter! :object-view {:object-type :kunde :instance-id "1"}]]]
         (second (step {} [:object-view/by-key-found :kunde :nr "7" {:rows [{:id 1}]}]))))
  (is (= [[:invoke [shell/fail! "kunde: mehrere Objekte mit nr = 7"]]]
         (second (step {} [:object-view/by-key-found :kunde :nr "7"
                           {:rows [{:id 1} {:id 2}]}]))))
  (testing "a failed query is said with its type"
    (let [[{:keys [err]}] (asks (step {} [:object-view/by-key :kunde [:nr "7"]]))]
      (is (= [[:invoke [shell/fail! "kunde" "weg"]]]
             (keep identity (second (step {} (conj err "weg")))))))))

(deftest a-bound-object-fills-the-parameter-of-its-type
  (let [spec {:params {:k {:type :kunde} :n {:type :string}}}
        [s] (step {} [:object-view.action/open {:action :a :spec spec :bindings {:kunde 5}}])]
    (is (= {:k "5"} (get-in s [:action-launcher :values])))
    (is (= #{:kunde} (get-in s [:action-launcher :bound-types]))))
  (testing "several ids go only to a parameter that takes many"
    (is (= {:ks ["1" "2"]}
           (ev/seed-values {} {:params {:k {:type :kunde} :ks {:type :kunde :multiple true}}}
                           {:kunde [1 2]})))))

(deftest a-property-default-starts-at-the-opened-objects-value
  (let [spec {:params {:ding  {:type :computer :prefill :current-object}
                       :fach  {:type :string :default {:from :ding :property :im-schrank}}
                       :ohne  {:type :string :default {:property :im-schrank}}
                       :fest  {:type :string :default "x"}}}
        rows {:computer {:id 5 :values {:im-schrank "Büro"}}}]
    (is (= {:fach "Büro" :ohne "Büro"}
           (ev/property-defaults {} spec {:computer 5} rows)))
    (is (= {} (ev/property-defaults {} spec {:computer 5} {}))))
  (let [spec {:params {:k {:type :kunde :prefill :current-object}
                       :n {:type :string :default {:property :nr}}}}
        [s] (step {} [:object-view.action/open {:action :a :spec spec :bindings {:kunde 5}
                                                :rows {:kunde {:id 5 :nr "7"}}}])]
    (is (= {:n "7"} (get-in s [:action-launcher :defaults])))))

(deftest a-submit-sends-only-what-is-filled-and-closes-once-applied
  (let [r (step {:action-launcher {:on-applied :applied}}
                [:object-view.action/submit :a {:x "" :y 1}])]
    (is (= [[ev/mutate! :a {:y 1} {:publish? true}]] (calls r))))
  (let [[s fx] (step {:action-launcher {}} [:object-view.action/applied :applied {}])]
    (is (nil? (:action-launcher s)))
    (is (= [[:invoke [sync/refresh-snapshot!]] [:invoke [instances/refresh-instance-state!]]
            [:invoke [:applied]]]
           fx))))

(deftest a-preview-after-the-launcher-closed-opens-nothing
  (let [[s r] (step {:action-launcher {}} [:object-view.action/preview :a {}])
        [{:keys [ok]}] (requests [s r])
        planned {:plan [{:op :update}]}]
    (is (true? (get-in s [:action-launcher :busy?])))
    (is (= {} (first (step {} (conj ok planned)))))
    (is (= {:busy? false :error nil :preview [{:op :update}]}
           (:action-launcher (first (step s (conj ok planned))))))))

(deftest the-layout-editor-moves-hides-and-drops-items
  (let [db {:configure-dashboard {:draft [{:widget :a} {:widget :b}]}}
        d  #(get-in (first (step db %)) [:configure-dashboard :draft])]
    (is (= [{:widget :b} {:widget :a}] (d [:object-view.layout/move 0 1])))
    (is (= [{:widget :a :hidden? true} {:widget :b}] (d [:object-view.layout/toggle-hidden 0])))
    (is (= [{:widget :b}] (d [:object-view.layout/drop 0]))))
  (let [[s [[k c]]] (step {:configure-dashboard {} :object-view {:instance-id 3}}
                          [:object-view.layout/save :kunde :full {:kind :rows}
                           :configure-dashboard])]
    (is (nil? (:configure-dashboard s)))
    (is (= :call k))
    (is (= [[:invoke [@#'ev/refresh-if-shown! :kunde "3"]]] (:fx (second (:ok c)))))))

(deftest a-new-placement-is-linked-to-its-floorplan
  (let [[_ fx] (step {} [:object-view.floorplan/placed "fp" :connect-depicts-device "d1"
                         {:id "p1"}])]
    (is (= [[:invoke [ontology/mutate! :connect-on-floorplan {:from-id "p1" :to-id "fp"}]]
            [:invoke [ontology/mutate! :connect-depicts-device {:from-id "p1" :to-id "d1"}]]]
           fx)))
  (is (= [] (second (step {} [:object-view.floorplan/placed "fp" :x "d1" {}])))))
