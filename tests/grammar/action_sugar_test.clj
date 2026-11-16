(ns ^:unit grammar.action-sugar-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-emit :as pemit]))

(def ^:private halter
  {:match-keys [:halter-id]
   :attributes [[:halter-id :string {:name "Halter"}]
                [:email :string {:name "E-Mail"}]
                [:plz :string {:name "PLZ"}]]})

(defn- back [schema]
  (-> (select-keys schema [:objects :links :actions])
      (update :actions update-vals sugar/raise-create-ids)
      sugar/raise-actions))

(deftest set-expands-to-identity-values-and-implied-params
  (let [a (get-in (sugar/lower-schema
                   {:objects {:halter halter}
                    :actions {:kontakt-aendern {:params {:halter {:type :halter}}
                                        :rules [{:op :update :object-type :halter
                                                 :target [:get :halter]
                                                 :set [:email :plz]}]}}})
                  [:actions :kontakt-aendern])]
    (is (= {:email [:get :email] :plz [:get :plz]} (:values (first (:rules a)))))
    (is (nil? (:set (first (:rules a)))))
    (testing "the parameter takes type AND label from the property it fills"
      (is (= {:type :string :name "E-Mail"} (get-in a [:params :email]))))))

(deftest an-explicit-value-wins-over-set
  (let [r (first (get-in (sugar/lower-schema
                          {:objects {:halter halter}
                           :actions {:x {:params {:p {:type :string}}
                                         :rules [{:op :create :object-type :halter
                                                  :set [:email] :values {:email [:get :p]}}]}}})
                         [:actions :x :rules]))]
    (is (= {:email [:get :p]} (:values r)))))

(deftest a-malformed-set-is-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":set takes a vector"
                        (sugar/lower-schema
                         {:actions {:x {:rules [{:op :create :object-type :a :set :email}]}}}))))

(deftest required-false-is-the-absence-of-required
  (is (= {:type :string}
         (get-in (sugar/lower-schema {:actions {:x {:params {:p {:type :string :required false}}}}})
                 [:actions :x :params :p]))))

(deftest hidden-params-take-no-label
  (is (nil? (get-in (sugar/lower-schema
                     {:objects {:halter halter}
                      :actions {:x {:params {:id {:prefill :uuid :exposure :hidden}}
                                    :rules [{:op :create :object-type :halter
                                             :values {:halter-id [:get :id]}}]}}})
                    [:actions :x :params :id :name]))))

(def ^:private langform
  {:objects {:halter halter
             :kontakt-aendern-log {:plural "[LOG] kontakt-aendern" :symbol "🧾" :title :log-id
                           :match-keys [:log-id] :primary-key :log-id
                           :project :p :status :active
                           :attributes [[:log-id :string
                                         {:name "Submission" :render-hints [:identifier]}]
                                        [:action :string {:name "Action"}]
                                        [:at :string {:name "Zeitpunkt" :render-hints [:sortable]}]
                                        [:user-id :string
                                         {:name "Benutzer" :render-hints [:searchable]}]]}}
   :actions {:kontakt-aendern
             {:params {:halter {:type :halter :name "Halter" :required true}
                       :email   {:type :string :name "E-Mail" :required false}
                       :plz     {:type :string :name "Postleitzahl"}
                       :log-id  {:type :string :prefill :uuid :exposure :hidden}
                       :log-at  {:type :string :prefill :now :exposure :hidden}
                       :log-by  {:type :string :prefill :current-user :exposure :hidden}}
              :rules [{:op :update :object-type :halter :target [:get :halter]
                       :values {:email [:get :email] :plz [:get :plz]}}
                      {:op :create :object-type :kontakt-aendern-log
                       :values {:log-id [:get :log-id] :action "kontakt-aendern"
                                :at [:get :log-at] :user-id [:get :log-by]}}]}}})

(deftest the-export-writes-the-short-form
  (let [out (back (sugar/lower-schema langform))
        a   (get-in out [:actions :kontakt-aendern])]
    (testing "the hand-written log type folds into the toggle"
      (is (nil? (get-in out [:objects :kontakt-aendern-log])))
      (is (= {:project :p :status :active :edited false} (:action-log a))))
    (testing "identity values fold away"
      (is (= [{:op :update :object-type :halter :target [:get :halter] :set [:email :plz]}]
             (:rules a))))
    (testing "only what the property cannot say stays on the parameter"
      (is (= {:halter {:type :halter :name "Halter" :required true}
              :plz     {:name "Postleitzahl"}}
             (:params a))))
    (testing "and the short form lowers to exactly what the long form lowers to"
      (let [stamp? #{:action-rid :action-type-rid :action-type-version :timestamp}
            strip #(-> %
                       (update :actions update-vals (fn [x] (dissoc x :action-log)))
                       (update-in [:objects :kontakt-aendern-log]
                                  (fn [o]
                                    (-> (dissoc o :visibility)
                                        (update :attributes
                                                (fn [as] (vec (remove (comp stamp? first) as)))))))
                       )]
        (is (= (strip (sugar/lower-schema langform)) (strip (sugar/lower-schema out))))))
    (testing "— except that the folded log type is now a hidden system object"
      (is (= :hidden
             (get-in (sugar/lower-schema out) [:objects :kontakt-aendern-log :visibility]))))))

(deftest the-exporter-runs-the-raise
  (let [authored {:actions {:a {:params {:titel {:type :string}}
                                :rules  [{:op :create :object-type :arbeitskarte :id [:get :titel]
                                          :values {:titel [:get :titel]}}]}}}]
    (is (= [{:op :create :object-type :arbeitskarte :id [:get :titel] :set [:titel]}]
           (get-in (:ent (pemit/export (sugar/lower-schema authored)
                                    {:all-projects? true :inline? true}))
                   [:schema :actions :a :rules])))))

(deftest a-log-type-the-generator-would-not-produce-stays-written-out
  (let [ohne-key (update-in langform [:objects :kontakt-aendern-log] dissoc :match-keys)
        out      (back (sugar/lower-schema ohne-key))]
    (is (some? (get-in out [:objects :kontakt-aendern-log])))
    (is (nil? (get-in out [:actions :kontakt-aendern :action-log])))))

(deftest a-logged-create-still-derives-its-key-parameter
  (let [lowered (sugar/lower-schema
                 {:objects {:karte {:match-keys [:karte-id]
                                    :attributes [[:karte-id :string {:name "Karte"}]]}}
                  :actions {:karte-anlegen
                            {:action-log true
                             :params {:karte-id {:prefill :uuid}}
                             :rules  [{:op :create :object-type :karte
                                       :values {:karte-id [:get :karte-id]}}]}}})
        a       (get-in lowered [:actions :karte-anlegen])]
    (is (= :string (get-in a [:params :karte-id :type])))
    (is (= [:get :karte-id]
           (some #(when (= :karte-anlegen-log (:object-type %)) (get-in % [:values :edited]))
                 (:rules a))))))

(deftest a-partial-text-takes-the-edited-key-from-the-live-type
  (let [lowered (sugar/lower-schema-against
                 {:objects {:rechnung {:attributes [[:notiz :string {}]]}}
                  :actions {:verwerfen
                            {:action-log true
                             :params {:rechnung {:type :rechnung :required true}}
                             :rules  [{:op :delete-object :object-type :rechnung
                                       :target [:get :rechnung]}]}}}
                 {:rechnung {:primary-key :rechnungs-id :match-keys [] :title :doc-number
                             :attributes [[:rechnungs-id :string {}]
                                          [:doc-number :string {}]]}})]
    (is (nil? (:sutra.parser.sugar/live lowered)))
    (is (= [:get :rechnung :rechnungs-id]
           (some #(get-in % [:values :edited]) (get-in lowered [:actions :verwerfen :rules]))))
    (is (some #{:verwerfen-log-edited} (map first (:links lowered))))))

(deftest a-log-stores-the-values-beside-the-edit
  (let [lowered (sugar/lower-schema
                 {:objects {:position {:match-keys [:pos-id]
                                       :attributes [[:pos-id :string {}]
                                                    [:betrag [:decimal 18 4] {}]]}
                            :karte    {:match-keys [:karte-id]
                                       :attributes [[:karte-id :string {}]
                                                    [:notiz :string {}]]}}
                  :actions {:buchen
                            {:action-log {:values {:betrag [:get :position :betrag]
                                                   :notiz-damals [:get :notiz]
                                                   :detail "gebucht"}}
                             :params {:position {:type :position :required true}
                                      :karte    {:type :karte :required true}
                                      :notiz    {}}
                             :rules  [{:op :update :object-type :karte :target [:get :karte]
                                       :values {:notiz [:get :notiz]}}]}}})
        log     (get-in lowered [:objects :buchen-log])
        rule    (some #(when (= :buchen-log (:object-type %)) %)
                      (get-in lowered [:actions :buchen :rules]))
        types   (into {} (map (juxt first second)) (:attributes log))]
    (is (= [:decimal 18 4] (:betrag types)))
    (is (= :string (:notiz-damals types) (:detail types)))
    (is (= :string (get-in lowered [:actions :buchen :params :notiz :type])))
    (is (= {:betrag [:get :position :betrag] :notiz-damals [:get :notiz] :detail "gebucht"}
           (select-keys (:values rule) [:betrag :notiz-damals :detail])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (sugar/lower-schema
                  {:actions {:b {:action-log {:values {:x [:get :fehlt]}} :rules []}}})))))

(deftest stored-values-fold-back-into-the-toggle
  (let [written {:objects {:karte {:match-keys [:karte-id]
                                   :attributes [[:karte-id :string {}] [:notiz :string {}]]}}
                 :actions {:notieren
                           {:action-log {:project :p :values {:notiz [:get :karte :notiz]}}
                            :params {:karte {:type :karte :required true}}
                            :rules  [{:op :update :object-type :karte :target [:get :karte]
                                      :values {:notiz "neu"}}]}}}
        out     (:schema (:ent (pemit/export (sugar/lower-schema written)
                                          {:all-projects? true :inline? true})))]
    (is (= {:notiz [:get :karte :notiz]} (get-in out [:actions :notieren :action-log :values])))
    (is (nil? (get-in out [:objects :notieren-log])))))

(deftest a-generated-log-carries-counterparts-identifying-fields
  (let [a (-> (sugar/lower-schema
               {:objects {:karte {:match-keys [:karte-id]
                                  :attributes [[:karte-id :string {}] [:notiz :string {}]]}}
                :actions {:notieren {:action-log true
                                     :params {:karte {:type :karte :required true}}
                                     :rules [{:op :update :object-type :karte
                                              :target [:get :karte] :values {:notiz "x"}}]}}})
              (get-in [:objects :notieren-log :attributes]))
        types (into {} (map (juxt first second)) a)]
    (is (= {:action-rid :string :action-type-rid :string
            :action-type-version :integer :timestamp :timestamp}
           (select-keys types [:action-rid :action-type-rid :action-type-version :timestamp])))))

(deftest an-array-link-stands-beside-a-hand-declared-edited-link
  (let [lowered (sugar/lower-schema
                 {:objects {:karte {:match-keys [:karte-id]
                                    :attributes [[:karte-id :string {}] [:notiz :string {}]]}}
                  :links   [[:notieren-log-edited :notieren-log :karte :many-to-one
                             {:fk {:attribute :edited :on-unresolved :skip}}]]
                  :actions {:notieren {:action-log true
                                       :params {:karte {:type :karte :required true}}
                                       :rules [{:op :update :object-type :karte
                                                :target [:get :karte] :values {:notiz "x"}}]}}})
        fk (fn [link] (some (fn [[_ t]]
                                 (when (= link (get-in t [:output :link-type]))
                                   (get-in t [:output :fk-attribute])))
                             (:transforms lowered)))]
    (is (= :edited (fk :notieren-log-edited)))
    (is (= :edited-objects (fk :notieren-log-edited-objects)))))

(deftest a-live-log-keeps-the-type-of-its-edited-property
  (let [lowered (sugar/lower-schema-against
                 {:actions {:unterordnen
                            {:action-log true
                             :params {:projekt {:type :projekt :required true}}
                             :rules [{:op :update :object-type :projekt :target [:get :projekt]
                                      :values {:notiz "x"}}]}}}
                 {:projekt {:primary-key :projekt-id :match-keys []
                            :attributes [[:projekt-id :integer {}] [:notiz :string {}]]}
                  :unterordnen-log {:primary-key :log-id
                                    :attributes [[:log-id :string {}] [:edited :string {}]]}})]
    (is (= :string
           (some (fn [[n t]] (when (= :edited n) t))
                 (get-in lowered [:objects :unterordnen-log :attributes]))))))
