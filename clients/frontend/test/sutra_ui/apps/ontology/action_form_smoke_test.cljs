(ns sutra-ui.apps.ontology.action-form-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.ontology.model :as ont-model]))

(def ^:private notiz-rid "ri.sutra.main.object-type.notiz")

(defn- with-schema! []
  (reset! catalog/canonical-schema
          {:object-types {:notiz {:id notiz-rid
                                  :attribute-list
                                    [{:attribute-name :notiz-id :attribute-type :string}
                                     {:attribute-name :menge :attribute-type :integer}
                                     {:attribute-name :tag :attribute-type :date}
                                     {:attribute-name :am :attribute-type :timestamp}
                                     {:attribute-name :von :attribute-type :string}
                                     {:attribute-name :stand :attribute-type :string}]}}
           :link-types {} :actions {}})
  (reset! catalog/name->rid {:object-types {:notiz notiz-rid}})
  (reset! catalog/rid->name {notiz-rid :notiz}))

(def ^:private form
  {:name "vermerken"
   :params [{:pname "id" :type "string" :prefill "uuid" :hidden true}
            {:pname "quelle" :type "notiz" :label "Quelle" :required true :prefill "current-object"}
            {:pname "menge" :type "number" :default-src "prop" :default-from "quelle"
             :default-prop "menge"}
            {:pname "stand" :type "string" :default-src "static" :default "entwurf"}]
   :rules [{:op "create" :type "notiz" :bind {:id "id"}
            :vals [{:prop "notiz-id" :src "param" :param "id"}
                   {:prop "menge" :src "static" :value "3"}
                   {:prop "tag" :src "now"}
                   {:prop "am" :src "now"}
                   {:prop "von" :src "user"}
                   {:prop "stand" :src "prop" :param "quelle" :oprop "stand"}]}]
   :submission-criteria [{:left-src "prop" :param "quelle" :prop "stand" :op "="
                          :right-src "static" :value "entwurf" :message "Nur Entwuerfe."}
                         {:left-src "param" :param "menge" :op "<="
                          :right-src "prop" :rparam "quelle" :rprop "menge"}
                         {:left-src "user" :op "=" :right-src "static" :value "admin"}
                         {:left-src "param" :param "stand" :op "=" :right-src "none"}]})

(deftest rule-values-take-every-source-kind
  (with-schema!)
  (let [spec (ont-model/->action-spec form)
        r    (first (:rules spec))]
    (is (= {:notiz-id [:get :arg0 :id]
            :menge    3
            :tag      [:get :regel-heute]
            :am       [:get :regel-jetzt]
            :von      [:get :regel-benutzer]
            :stand    [:get :quelle :stand]}
           (:values r)))
    (testing "current user/time become hidden prefilled params, typed by the property"
      (is (= {:type :date :prefill :now :exposure :hidden} (get-in spec [:params :regel-heute])))
      (is (= {:type :timestamp :prefill :now :exposure :hidden}
             (get-in spec [:params :regel-jetzt])))
      (is (= {:type :string :prefill :current-user :exposure :hidden}
             (get-in spec [:params :regel-benutzer]))))))

(deftest parameters-carry-their-configuration
  (with-schema!)
  (let [ps (:params (ont-model/->action-spec form))]
    (is (= {:type :string :prefill :uuid :exposure :hidden} (:id ps)))
    (is (= {:type :notiz :name "Quelle" :required true :prefill :current-object} (:quelle ps)))
    (is (= {:type :number :default {:from :quelle :property :menge}} (:menge ps)))
    (is (= {:type :string :default "entwurf"} (:stand ps)))))

(deftest criteria-use-the-grammars-binding-shape
  (with-schema!)
  (let [[c1 c2 c3 c4] (:submission-criteria (ont-model/->action-spec form))]
    (is (= {:test {:l [:get :quelle :stand]} :pred [:= :l "entwurf"] :message "Nur Entwuerfe."} c1))
    (is (= {:test {:l [:get :menge] :r [:get :quelle :menge]} :pred [:<= :l :r]} c2))
    (is (= {:test {:l [:get :regel-benutzer]} :pred [:= :l "admin"]} c3))
    (is (= {:test {:l [:get :stand]} :pred [:= :l nil]} c4))))

(deftest a-loaded-action-saves-back-unchanged
  (with-schema!)
  (let [spec  (ont-model/->action-spec form)
        again (ont-model/->action-spec (ont-model/action->form :vermerken spec))]
    (is (= spec again)))
  (testing "what the form cannot show survives a save — nested criteria, computed
            values, parameter constraints, a legacy whole-map binding"
    (let [spec {:name :x
                :params {:a {:type :number :constraints {:range {:min 0}}}
                         :v {:type :string}}
                :rules [{:op :create :object-type notiz-rid :values {:menge [:+ [:get :a] 1]}}
                        {:op :update :target [:get :a] :values [:get :arg0 :v]}]
                :edits [{:object-type notiz-rid}]
                :submission-criteria [{:test {:a [:get :a]} :pred [:and [:> :a 0] [:< :a 9]]}]}
          back
            (ont-model/->action-spec
             (ont-model/action->form :x (assoc spec :display-name "X" :action-log true)))]
      (is (= ["X" true] [(:display-name back) (:action-log back)]) "action-level keys survive")
      (is (= (:params spec) (:params back)))
      (is (= (:rules spec) (:rules back)))
      (is (= (:submission-criteria spec) (:submission-criteria back))))))

(deftest a-runtime-binding-is-an-expression-not-a-parameter
  (with-schema!)
  (let [spec {:name :x :params {:q {:type :notiz}}
              :rules [{:op :update :target [:get :q] :object-type notiz-rid
                       :values {:stand [:get :webhook-result :body] :von [:get :q :von]}}]
              :edits [{:object-type notiz-rid}]}
        vals (:vals (first (:rules (ont-model/action->form :x spec))))]
    (is (= #{{:prop "stand" :src "expr" :raw [:get :webhook-result :body]}
             {:prop "von" :src "prop" :param "q" :oprop "von"}}
           (set (map #(dissoc % :orig) vals))))
    (is (= (:rules spec) (:rules (ont-model/->action-spec (ont-model/action->form :x spec)))))))

(deftest a-modify-rule-targets-the-object-parameter
  (with-schema!)
  (let [form {:name "x" :params [{:pname "q" :type "notiz"}]
              :rules [{:op "update" :bind {:target "q"}
                       :vals [{:prop "stand" :src "static" :value "gesendet"}]}]}]
    (is (= [:get :q] (:target (first (:rules (ont-model/->action-spec form)))))))
  (testing "an old [:get :arg0 p] target is repaired when the action is saved again"
    (let [spec {:name :x :params {:q {:type :notiz}}
                :rules [{:op :update :target [:get :arg0 :q] :values {:stand "gesendet"}}]}]
      (is (= [:get :q]
             (:target
              (first (:rules (ont-model/->action-spec (ont-model/action->form :x spec))))))))))

(deftest a-legacy-ui-criterion-is-repaired-on-save
  (with-schema!)
  (let [legacy {:name :x :params {:menge {:type :number}}
                :submission-criteria [{:test {} :pred [:> [:get :arg0 :menge] 3]}]}]
    (is (= [{:test {:l [:get :menge]} :pred [:> :l 3]}]
           (:submission-criteria (ont-model/->action-spec (ont-model/action->form :x legacy)))))))
