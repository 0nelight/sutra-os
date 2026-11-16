(ns sutra-ui.apps.enta-editor.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.enta-editor.events :as ev]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.staging :as staging]))

(defn- calls [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- kinds [[_ fx]] (mapv first (remove nil? fx)))

(def ^:private tree {:kind :rows :items [{:widget :kpi :label "Summe"}]})

(def ^:private loaded
  {:draft (assoc ev/blank-draft :app :kosten :file {:name :kosten :layout tree} :tree tree
                 :cursor [])})

(deftest saving-stages-the-draft-and-leaves-publishing-to-the-install
  (let [[s :as r] (step loaded [:enta/stage])
        [{:keys [call ok err]}] (calls r)]
    (is (:loading? (:draft s)))
    (is (= [api/post! :application-file {:name "kosten"}] (take 3 call)))
    (is (= [[:enta/staged]] (:then (second ok))))
    (let [s' (run s (conj ok {:created 1}))
          [_ fx] (step s' [:enta/staged {:created 1}])]
      (is (false? (:loading? (:draft s'))))
      (is (= {:created 1} (:staged (:draft s'))))
      (is (= [(:file (:draft s')) tree] (:saved (:draft s'))))
      (is (= [[:invoke [staging/refresh-staged!]]] fx)))
    (is (= [false "nein"] ((juxt :loading? :error) (:draft (run s (conj err "nein"))))))))

(deftest a-change-asks-for-the-preview-only-while-it-is-shown
  (is (= [] (kinds (step loaded [:enta/intend :put-param {:path [:items 0] :key :label
                                                          :value "Neu"} {}]))))
  (let [shown (assoc loaded :preview-shown? true)
        [s fx] (step shown [:enta/intend :put-param {:path [:items 0] :key :label :value "Neu"}
                            {:rid 7}])]
    (is (= "Neu" (get-in s [:draft :tree :items 0 :label])))
    (is (= [[:after {:key :preview :ms ev/preview-ms :event [:enta.preview/refresh 7]}]]
           (remove nil? fx))))
  (testing "a select changes no tree and asks for nothing"
    (let [shown (assoc loaded :preview-shown? true :read-for (ev/preview-key (:draft loaded)))]
      (is (= [] (kinds (step shown [:enta/intend :select {:path [:items 0]} {}])))))))

(deftest a-stale-preview-answer-is-dropped-and-a-refusal-falls-back
  (let [[s :as r] (step loaded [:enta.preview/refresh 9])
        [{:keys [ok err]}] (calls r)]
    (is (= :loading (get-in s [:draft :preview-source])))
    (is (= [[:enta.preview/staged 1 9]] (:then (second ok))))
    (let [s2 (assoc s :preview-seq 2)]
      (is (= s2 (run s2 (conj ok {:id "x"}))) "a stale answer asks nothing further")
      (is (= s2 (run s2 (conj err "weg")))))
    (let [[{:keys [call ok err]}] (calls (step s [:enta.preview/refused 1 9 "no edit access"]))]
      (is (= [api/get! :my-application {:rid "9"}] call))
      (is (= [[:enta.preview/read :published "no edit access"]] (:then (second ok))))
      (is (= [:none "no edit access"]
             ((juxt :preview-source :preview-error) (:draft (run s (conj err "weg")))))
          "the published app missing too, the first reason shows"))
    (let [[s'] (step s [:enta.preview/refused 1 nil "weg"])]
      (is (= [:none "weg"] ((juxt :preview-source :preview-error) (:draft s')))))
    (let [s' (run s [:enta.preview/read :draft nil {:data {:a 1} :application {:id 3}}])]
      (is (= [:draft {:a 1} {:id 3}] ((juxt :preview-source :data :consumer) (:draft s')))))))

(deftest a-template-that-needs-a-type-asks-for-it-first
  (is (= [[:dispatch [:enta.ui/set {:asking {:id :liste-detail}}]]]
         (second (step {} [:enta.new/start :liste-detail]))))
  (is (= [[:dispatch [:enta/new :leer nil]]] (second (step {} [:enta.new/start :leer]))))
  (let [[s] (step {:pane :enta} [:enta/new :leer nil])]
    (is (= :preview (:pane s)))
    (is (= :neue-app (get-in s [:draft :file :name])))))

(deftest unbinding-a-used-type-asks-first
  (let [d (assoc-in loaded [:draft :file :object-types] [:rechnung])
        [s] (step d [:enta.types/unbind [] {} [] {}])]
    (is (empty? (get-in s [:draft :file :object-types]))))
  (let [[_ [[k {:keys [ok]}]]] (step loaded [:enta.types/unbind [{:label "Tabelle"}] {:title "?"}
                                             [] {}])]
    (is (= :confirm k))
    (is (= [:enta/intend :put-file-key {:key :object-types :value []} {}] ok))))

(deftest preview-objects-load-once-and-sort-by-fill
  (let [[s :as r] (step {} [:enta.objects/need :rechnung])]
    (is (= {:loading? true} (get-in s [:candidates :rechnung])))
    (is (= [api/list-objects :rechnung {:limit 200 :offset 0}] (:call (first (calls r)))))
    (is (= [s []] (step s [:enta.objects/need :rechnung])) "asked twice, loaded once")
    (let [{:keys [ok err]} (first (calls r))
          s' (run s (conj ok {:rows [{:id 1 :a "x" :b 2} {:id 2}] :total 2}))]
      (is (= [2 1] (map :id (get-in s' [:candidates :rechnung :rows]))))
      (is (= {:loading? false :error "weg"} (get-in (run s (conj err "weg"))
                                                    [:candidates :rechnung]))))))
