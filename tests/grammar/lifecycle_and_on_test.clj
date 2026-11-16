(ns ^:unit grammar.lifecycle-and-on-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.lifecycle :as lifecycle]
            [sutra.types.submission :as csub]))

(def ^:private arbeitsauftrag
  {:name       "Arbeitsauftrag"
   :title      :bezeichnung
   :subtitle   :auftrag-nr
   :row        [:auftrag-nr :bezeichnung :stand]
   :lifecycle  {:property    :stand
                :states      [:angelegt :durchfuehrbar :abgeschlossen]
                :transitions [{:action :freigeben :from [:angelegt] :to :durchfuehrbar}]}
   :attributes [[:auftrag-nr :string] [:bezeichnung :string] [:stand :string]]})

(def ^:private freigeben
  {:on     :arbeitsauftrag
   :params {:arbeitsauftrag {:type :arbeitsauftrag :required true}
            :notiz   {:type :string}}
   :rules  [{:op :update :object-type :arbeitsauftrag :target [:get :arbeitsauftrag :id]
             :values {:notiz [:get :notiz]}}]})

(deftest a-transition-gates-and-moves-the-state
  (let [tr  (lifecycle/transition-for {:arbeitsauftrag arbeitsauftrag} :freigeben)
        act (lifecycle/expand-action freigeben tr #(= :arbeitsauftrag %))]
    (is (= {:object-type :arbeitsauftrag :property :stand :from ["angelegt"] :to "durchfuehrbar"}
           tr))
    (testing "the object must stand in :from"
      (is (= [:in :stand "angelegt"] (:pred (first (:submission-criteria act)))))
      (is (= {:stand [:get :arbeitsauftrag :stand]} (:test (first (:submission-criteria act))))))
    (testing "and a rule moves it to :to, since no rule of the Action writes :stand"
      (is (= {:stand "durchfuehrbar"} (:values (last (:rules act))))))
    (testing "an Action that writes the state itself keeps its own move"
      (let [own (update-in freigeben [:rules 0 :values] assoc :stand "durchfuehrbar")]
        (is (= 1 (count (:rules (lifecycle/expand-action own tr #(= :arbeitsauftrag %))))))))))

(deftest lifecycle-problems-are-said
  (let [ms (map :message
                (lifecycle/problems {:arbeitsauftrag
                                       (assoc-in arbeitsauftrag [:lifecycle :transitions 0 :to]
                                                 :fertig)}
                             {:freigeben (update freigeben :params dissoc :arbeitsauftrag)}
                             (constantly [:stand])))]
    (is (some #(re-find #"unknown states" %) ms))
    (is (some #(re-find #"exactly one parameter" %) ms))))

(deftest on-binds-the-viewed-object
  (is (= :current-object
         (get-in (csub/with-on-prefill freigeben) [:params :arbeitsauftrag :prefill])))
  (is (nil? (get-in (csub/with-on-prefill (dissoc freigeben :on))
                    [:params :arbeitsauftrag :prefill])))
  (testing "an explicit prefill wins"
    (is (= :uuid
           (get-in
            (csub/with-on-prefill (assoc-in freigeben [:params :arbeitsauftrag :prefill] :uuid))
            [:params :arbeitsauftrag :prefill])))))

(deftest on-binds-across-rid-and-name-spellings
  (let [rid "ri.sutra.main.object-type.1e9dbe22-b9a8-4327-b383-04039da39f6f"
        act {:on (keyword rid) :params {:arbeitsauftrag {:type :arbeitsauftrag :required true}}}
        ids {rid rid :arbeitsauftrag rid}
        id  (fn [t] (get ids (if (keyword? t) (let [s (subs (str t) 1)] (if (= s rid) s t)) t)))]
    (is (= :current-object
           (get-in (csub/with-on-prefill act id) [:params :arbeitsauftrag :prefill]))
        ":on stored as a keyword RID still finds the param typed by NAME")))
