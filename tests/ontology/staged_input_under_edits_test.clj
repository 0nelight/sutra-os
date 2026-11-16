(ns ^:api ontology.staged-input-under-edits-test
  (:require [support.wire :refer [ok?]]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private py-echo
  "from sutra_transforms import transform, Input, Output
@transform(inputs={'g': Input(kind='object-type', object_type='gadget')},
           output=Output(kind='object-type', object_type='pyecho', primary_key='pk'))
def compute(g):
    return [{'pk': 'p-' + r['pk'], 'seen': str(r.get('code_norm'))} for r in g]")

(def ^:private schema
  {:schema
   {:objects {:gadget {:project :wb :symbol "⚙️" :primary-key :pk
                       :attributes [[:pk :string] [:label :string] [:code :string]
                                    [:code-norm :string]]
                       :plural "Gadgets"}
              :echo   {:project :wb :symbol "🔁" :primary-key :pk
                       :attributes [[:pk :string] [:seen :string]]
                       :plural "Echos"}
              :pyecho {:project :wb :symbol "🐍" :primary-key :pk
                       :attributes [[:pk :string] [:seen :string]]
                       :plural "Pyechos"}}
    :transforms
    {:mk-gadget {:project :wb :language :clojure
                 :code "(fn [{:keys [src]}]
                          (for [r src] {:pk (:tid r) :label (:label r) :code (:code r)
                                       :code-norm (:code r)}))"
                 :inputs {:src {:kind :dataset :dataset :src}}
                 :primary-key :pk
                 :output {:kind :object-type :object-type :gadget}}
     :echo-gadget {:project :wb :language :clojure
                   :code "(fn [{:keys [g]}]
                            (for [r g] {:pk (str \"e-\" (:pk r))
                                        :seen (pr-str [(:label r) (contains? r :code)])}))"
                   :inputs {:g {:kind :object-type :object-type :gadget}}
                   :primary-key :pk
                   :output {:kind :object-type :object-type :echo}}
     :py-echo {:project :wb :language :python :code py-echo
               :inputs {:g {:kind :object-type :object-type :gadget}}
               :primary-key :pk
               :output {:kind :object-type :object-type :pyecho}}}}})

(defn- echoes []
  (into {}
        (map (fn [o] [(get-in o [:values :pk]) (edn/read-string (get-in o [:values :seen]))]))
        (:rows (:body (api/post! :ontology-query {:find :echo :limit 100})))))

(defn- pyechoes []
  (into {} (map (fn [o] [(get-in o [:values :pk]) (get-in o [:values :seen])]))
        (:rows (:body (api/post! :ontology-query {:find :pyecho :limit 100})))))

(deftest a-type-imported-in-the-run-is-read-under-its-edits
  (is (ok? (api/mutate :create-project {:name :wb} {:stage? true})))
  (is (ok? (api/mutate :create-dataset
                       {:name :src :project :wb
                        :columns [(col :tid :string) (col :label :string) (col :code :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :dataset-write {:name "src"}
                      {:rows [{:values {:tid "t1" :label "alpha" :code "A"}}]
                       :mode :snapshot :message "seed"})))
  (is (ok? (api/post! :import-run)))
  (is (ok? (api/mutate :create-action
                       {:name :gadget-anlegen :project :wb
                        :params {:pk    {:name "Pk" :type :string :required true}
                                 :label {:name "Label" :type :string :required true}}
                        :rules [{:op :create :object-type :gadget
                                 :values {:pk [:get :arg0 :pk] :label [:get :arg0 :label]}}]
                        :edits [{:object-type :gadget}]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :gadget-anlegen {:pk "w1" :label "user-made"})))
  (is (ok? (api/post! :staging)) "commit the action's edit")
  (testing "the run imports :gadget again and reads it downstream"
    (is (ok? (api/post! :dataset-write {:name "src"}
                        {:rows [{:values {:tid "t1" :label "alpha2" :code "A"}}]
                         :mode :snapshot :message "touch"})))
    (is (ok? (api/post! :import-run)))
    (let [e (echoes)]
      (prn :echoes e)
      (is (= "alpha2" (first (get e "e-t1"))) "this run's source row")
      (is (= "user-made" (first (get e "e-w1"))) "the object an edit created"))
    (let [p (pyechoes)]
      (prn :pyechoes p)
      (is (= "A" (get p "p-t1")) "python reads this run's source row")
      (is (= "None" (get p "p-w1"))
          "python reads the edit-created object, a column it never got as None"))))
