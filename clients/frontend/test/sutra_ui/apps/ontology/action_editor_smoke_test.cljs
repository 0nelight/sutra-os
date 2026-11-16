(ns sutra-ui.apps.ontology.action-editor-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.ontology.model :as ont-model]))

(def ^:private ue-rid "ri.sutra.main.object-type.ueberweisung")
(def ^:private log-rid "ri.sutra.main.object-type.ueberweisung-senden-log")

(defn- with-schema! []
  (reset! catalog/canonical-schema
          {:object-types {:ueberweisung {:id ue-rid :plural "Ueberweisungen"
                                         :attribute-list
                                           [{:attribute-name :ueberweisung-id
                                             :attribute-type :string}
                                            {:attribute-name :auftragsstand :attribute-type :string}
                                            {:attribute-name :betrag :attribute-type :decimal}
                                            {:attribute-name :freigegeben-am
                                             :attribute-type :timestamp}]}
                          :ueberweisung-senden-log {:id log-rid :plural "[LOG] ueberweisung-senden"
                                                    :attribute-list
                                                      [{:attribute-name :log-id
                                                        :attribute-type :string}]}}
           :sources
             {:zahlungsdienst
                {:webhooks {:zahlung-einreichen {:params [:auftrag-id :betrag :waehrung]}}}}
           :link-types {} :actions {:vorhanden {:name :vorhanden}}})
  (reset! catalog/name->rid {:object-types {:ueberweisung ue-rid :ueberweisung-senden-log log-rid}})
  (reset! catalog/rid->name {ue-rid :ueberweisung log-rid :ueberweisung-senden-log}))

(defn- errors [form]
  (set (keep #(when (= :error (:level %)) (:key %)) (ont-model/action-form-problems form))))

(deftest the-wizard-builds-a-complete-action
  (with-schema!)
  (testing "Modify: the object parameter the rule targets, one parameter per chosen property"
    (let [f    (ont-model/wizard->form {:display-name "Überweisung freigeben" :kind "update"
                                    :object-type "ueberweisung" :props ["auftragsstand" "betrag"]})
          spec (ont-model/->action-spec f)]
      (is (= :ueberweisung-freigeben (:name spec)) "the API name follows the display name")
      (is (= "Überweisung freigeben" (:display-name spec)))
      (is (= {:ueberweisung  {:type :ueberweisung :required true :prefill :current-object}
              :auftragsstand {:type :string}
              :betrag        {:type :number}}
             (:params spec)))
      (is (= [{:op :update :object-type ue-rid :target [:get :ueberweisung]
               :values {:auftragsstand [:get :arg0 :auftragsstand] :betrag [:get :arg0 :betrag]}}]
             (:rules spec)))
      (is (= [{:object-type ue-rid}] (:edits spec))
          "Modify names its type, so the edit is provenance")
      (is (empty? (errors f)))))
  (testing "Create: the key property is filled by a hidden, UUID-prefilled parameter"
    (let [spec (ont-model/->action-spec (ont-model/wizard->form {:display-name "Neu" :kind "create"
                                                         :object-type "ueberweisung"
                                                         :props ["betrag"]}))]
      (is (= {:type :string :exposure :hidden :prefill :uuid}
             (get-in spec [:params :ueberweisung-id])))
      (is (= [:get :arg0 :ueberweisung-id] (get-in spec [:rules 0 :id])))
      (is (= {:ueberweisung-id [:get :arg0 :ueberweisung-id] :betrag [:get :arg0 :betrag]}
             (get-in spec [:rules 0 :values])))))
  (testing "Delete: only the object parameter"
    (let [spec
            (ont-model/->action-spec
             (ont-model/wizard->form {:display-name "Weg" :kind "delete"
                                      :object-type "ueberweisung"}))]
      (is (= [:get :arg0 :ueberweisung] (get-in spec [:rules 0 :target])))
      (is (= #{:ueberweisung} (set (keys (:params spec)))))))
  (testing "action log types are never offered as authorable types"
    (is (= [:ueberweisung] (vec (ont-model/authorable-object-types))))))

(deftest a-mapped-property-creates-its-parameter
  (with-schema!)
  (let [f0 {:name "x" :params [{:pname "u" :type "ueberweisung"}]
            :rules [{:op "update" :type "ueberweisung" :bind {:target "u"}
                     :vals [{:prop "" :src "param" :param ""}]}]}
        f1 (ont-model/set-value-prop f0 0 0 "freigegeben-am")]
    (is (= {:pname "freigegeben-am" :type "timestamp" :label "" :prefill ""} (last (:params f1))))
    (is (= "freigegeben-am" (get-in f1 [:rules 0 :vals 0 :param])))
    (testing "an existing parameter of the same type is reused, one of another type is not retyped"
      (let [f2 (ont-model/set-value-prop (update f0 :params conj {:pname "betrag" :type "number"}) 0
                                         0
                                     "betrag")
            f3 (ont-model/set-value-prop (update f0 :params conj {:pname "betrag" :type "string"}) 0
                                         0
                                     "betrag")]
        (is (= 2 (count (:params f2))))
        (is (= "betrag" (get-in f2 [:rules 0 :vals 0 :param])))
        (is (= "betrag-2" (get-in f3 [:rules 0 :vals 0 :param])))))
    (testing "renaming a parameter carries every reference with it"
      (let [f (ont-model/rename-param f1 0 "ueb")]
        (is (= "ueb" (get-in f [:rules 0 :bind :target])))))))

(deftest criteria-nest-all-any-none
  (with-schema!)
  (let [cnd  (fn [p v] {:left-src "prop" :param "u" :prop p :op "=" :right-src "static" :value v})
        form {:name "x" :params [{:pname "u" :type "ueberweisung"} {:pname "b" :type "number"}]
              :submission-criteria
              [{:group "all" :message "Nur freigegebene, bezahlbare Entwürfe."
                :children [(cnd "auftragsstand" "freigegeben")
                           {:group "any"
                            :children [{:left-src "param" :param "b" :op "<=" :right-src "prop"
                                        :rparam "u" :rprop "betrag"}
                                       {:left-src "param" :param "b" :op "=" :right-src "none"}]}
                           {:group "none" :children [(cnd "auftragsstand" "gesendet")
                                                     (cnd "auftragsstand" "abgelehnt")]}]}
               {:left-src "param" :param "b" :op "in" :right-src "static" :value "1, 2,3"}]}
        [c1 c2] (:submission-criteria (ont-model/->action-spec form))]
    (is (= {:test {:l [:get :u :auftragsstand] :l2 [:get :b] :r [:get :u :betrag]}
            :pred [:and [:= :l "freigegeben"]
                   [:or [:<= :l2 :r] [:= :l2 nil]]
                   [:not [:or [:= :l "gesendet"] [:= :l "abgelehnt"]]]]
            :message "Nur freigegebene, bezahlbare Entwürfe."}
           c1)
        "every side bound once in :test; one failure message at the root")
    (is (= {:test {:l [:get :b]} :pred [:in :l 1 2 3]} c2) "\"is included in\" takes a static list")
    (testing "a nested criterion loads into the tree and saves back unchanged"
      (let [spec {:name :x :params {:u {:type :ueberweisung} :b {:type :number}}
                  :submission-criteria [c1 c2
                                        {:test {:s [:get :u :auftragsstand]}
                                         :pred [:not [:= :s "x"]]}]}
            form (ont-model/action->form :x spec)]
        (is (= "all" (get-in form [:submission-criteria 0 :group])))
        (is (not-any? :raw (:submission-criteria form)) "nothing falls back to read-only")
        (is (= (:submission-criteria spec)
               (:submission-criteria (ont-model/->action-spec form))))))
    (testing "what the tree cannot say stays raw, untouched"
      (let [odd {:test {:a [:get :b]} :pred [:and [:> [:+ :a 1] 0]]}
            f   (ont-model/action->form :x
                                    {:name :x :params {:b {:type :number}}
                                     :submission-criteria [odd]})]
        (is (= {:raw odd} (first (:submission-criteria f))))))))

(deftest webhook-inputs-map-from-parameters
  (with-schema!)
  (is (= ["auftrag-id" "betrag" "waehrung"]
         (ont-model/webhook-inputs "zahlungsdienst" "zahlung-einreichen")))
  (let [form {:name "senden" :params [{:pname "u" :type "ueberweisung"} {:pname "w" :type "string"}]
              :rules [{:op "invoke-webhook" :pick {:source "zahlungsdienst"}
                       :free {:webhook "zahlung-einreichen"}
                       :wvals [{:prop "auftrag-id" :src "prop" :param "u" :oprop "ueberweisung-id"}
                               {:prop "betrag" :src "prop" :param "u" :oprop "betrag"}
                               {:prop "waehrung" :src "static" :value "EUR"}]}]}
        rule (first (:rules (ont-model/->action-spec form)))]
    (is (= {:op :invoke-webhook :source :zahlungsdienst :webhook :zahlung-einreichen
            :params {:auftrag-id [:get :u :ueberweisung-id] :betrag [:get :u :betrag]
                     :waehrung "EUR"}}
           rule))
    (testing "an input left unmapped is flagged before the save"
      (let [f (update-in form [:rules 0 :wvals] pop)]
        (is (some #(= :ui/prob-webhook-unmapped (:key %)) (ont-model/action-form-problems f)))))
    (testing "a loaded webhook rule round-trips, an expression input stays as written"
      (let [spec {:name :senden :params {:u {:type :ueberweisung}}
                  :rules [{:op :invoke-webhook :source :zahlungsdienst :webhook :zahlung-einreichen
                           :params {:auftrag-id [:get :u :ueberweisung-id] :waehrung "EUR"
                                    :betrag [:get :webhook-result :x]}}]}
            f    (ont-model/action->form :senden spec)]
        (is (= #{"prop" "static" "expr"} (set (map :src (get-in f [:rules 0 :wvals])))))
        (is (= (:rules spec) (:rules (ont-model/->action-spec f))))))))

(deftest allowed-values-are-a-parameter-constraint
  (with-schema!)
  (let [ps (:params (ont-model/->action-spec
                     {:name "x" :params [{:pname "kanal" :type "string" :one-of "x, mastodon"}
                                         {:pname "stufe" :type "number" :one-of "1,2,3"
                                          :constraints {:min 1}}]}))]
    (is (= {:type :string :constraints {:one-of ["x" "mastodon"]}} (:kanal ps)))
    (is (= {:type :number :constraints {:min 1 :one-of [1 2 3]}} (:stufe ps))))
  (let [spec {:name :x :params {:k {:type :string :constraints {:one-of ["x"] :max-length 3}}}}]
    (is (= (:params spec) (:params (ont-model/->action-spec (ont-model/action->form :x spec)))))))

(deftest platform-parameters-and-the-log-rule-stay-out-of-the-form
  (with-schema!)
  (let [spec {:name :ueberweisung-senden :display-name "Senden" :description "Reicht ein."
              :params {:log-id {:type :string :prefill :uuid :exposure :hidden}
                       :log-at {:type :string :prefill :now :exposure :hidden}
                       :log-by {:type :string :prefill :current-user :exposure :hidden}
                       :regel-2-id {:type :string :prefill :uuid :exposure :hidden}
                       :regel-jetzt {:type :timestamp :prefill :now :exposure :hidden}
                       :ueberweisung {:type :ueberweisung :name "Ueberweisung" :required true}}
              :rules [{:op :update :object-type ue-rid :target [:get :ueberweisung :id]
                       :values {:auftragsstand "gesendet" :freigegeben-am [:get :regel-jetzt]}}
                      {:op :create :object-type log-rid :id [:get :regel-2-id]
                       :values {:log-id [:get :log-id] :action "ueberweisung-senden"
                                :at [:get :log-at]
                                :user-id [:get :log-by]}}]
              :edits [{:object-type ue-rid} {:object-type log-rid}]
              :status :experimental}
        form (ont-model/action->form :ueberweisung-senden spec)
        back (ont-model/->action-spec form)]
    (is (= ["ueberweisung"] (map :pname (:params form))) "only the author's parameter is listed")
    (is (= 1 (count (:rules form))) "the log rule is not an editable rule")
    (is (true? (ont-model/hidden-action-param? spec :log-id)))
    (is (= (dissoc spec :edits) (dissoc back :edits)) "saved back unchanged")
    (is (= [{:object-type ue-rid}] (:edits back)))))

(deftest an-unknown-rule-op-is-kept-whole
  (with-schema!)
  (let [spec {:name :x :params {:p {:type :string}}
              :rules [{:op :update-of-interface :interface :ausgehend :target [:get :p]
                       :values {:a 1}}]}]
    (is (= (:rules spec) (:rules (ont-model/->action-spec (ont-model/action->form :x spec)))))))

(deftest the-editor-says-what-a-save-would-be-refused-for
  (with-schema!)
  (is (= #{:ui/prob-no-name} (errors {:name "" :params [] :rules []})))
  (is (contains? (errors {:name "vorhanden"}) :ui/prob-name-taken))
  (is (contains? (errors {:name "x" :params [{:pname "a" :type ""}]}) :ui/prob-param-incomplete))
  (is (contains? (errors {:name "x"
                          :params [{:pname "a" :type "string"} {:pname "a" :type "string"}]})
                 :ui/prob-param-dup))
  (is (contains? (errors {:name "x" :rules [{:op "create" :type ""}]}) :ui/prob-rule-type))
  (is (contains? (errors {:name "x" :rules [{:op "update" :type "ueberweisung" :bind {}}]})
                 :ui/prob-rule-target))
  (is (contains? (errors {:name "x" :rules [{:op "create" :type "ueberweisung"
                                             :vals [{:prop "betrag" :src "param"
                                                     :param "fehlt"}]}]})
                 :ui/prob-unknown-param))
  (is (contains? (errors {:name "x" :submission-criteria [{:group "all" :children []}]})
                 :ui/prob-group-empty))
  (is (= "rechnung-freigeben-ae" (ont-model/api-name-of "Rechnung freigeben (ä)"))))

(deftest a-source-switched-away-from-a-parameter-no-longer-reads-it
  (with-schema!)
  (let [f {:name "x" :params [{:pname "u" :type "ueberweisung"}]
           :rules [{:op "update" :type "ueberweisung" :bind {:target "u"}
                    :vals [{:prop "auftragsstand" :src "static" :param "weg" :value "entwurf"}]}]
           :submission-criteria [{:left-src "user" :param "weg" :op "=" :right-src "static"
                                  :value "a"}]}]
    (is (empty? (errors f))
        "a stale :param behind a static value or the current user is no reference")))
