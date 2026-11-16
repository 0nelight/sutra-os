(ns sutra-ui.link-type-form-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.forms.base :as forms]
            [sutra-ui.forms.external :as ext-forms]
            [sutra-ui.forms.modals :as form-modals]
            [sutra-ui.forms.link-type :as ltf]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.ontology :as ontology]))

(def ^:private canon
  {:object-types
   {:bankkonto {:implements [:geldkonto]}
    :person    {:implements [:partei]}
    :organisation {:implements [:partei]}
    :ort       {}
    :kunde     {}
    :auftrag   {}}
   :interfaces
   {:geldkonto {:link-constraints [{:link-name :inhaber :cardinality :one
                                    :to-interface :partei}]}
    :partei {}}
   :link-types
   {:bankkonto-inhaber-person
    {:link-type-name :bankkonto-inhaber-person
     :from-object-type :bankkonto :to-object-type :person :cardinality :many-to-one
     :forward-api-name :inhaber-person :reverse-api-name :bankkonten
     :satisfies [{:interface :geldkonto :constraint :inhaber}]
     :id "ri.sutra.main.link-type.1"}
    :ort-teil-von
    {:link-type-name :ort-teil-von :from-object-type :ort :to-object-type :ort
     :cardinality :many-to-one :forward-api-name :teil-von :reverse-api-name :teile
     :forward-name "Teil von" :reverse-name "Teile" :id "ri.sutra.main.link-type.2"}
    :auftrag-kunde
    {:link-type-name :auftrag-kunde :from-object-type :auftrag :to-object-type :kunde
     :cardinality :many-to-one :id "ri.sutra.main.link-type.3"}}
   :transforms
   {:link-bankkonto-inhaber-person
    {:transform-name :link-bankkonto-inhaber-person
     :input {:kind :object-type :object-type :bankkonto}
     :output {:kind :link-type :link-type :bankkonto-inhaber-person
              :fk-attribute :inhaber-person-id :on-null :skip :on-unresolved :skip
              :dataset :link-bankkonto-inhaber-person}}
    :link-auftrag-kunde
    {:transform-name :link-auftrag-kunde
     :input {:kind :object-type :object-type :auftrag}
     :output {:kind :link-type :link-type :auftrag-kunde}
     :fk-attribute :kunden-nr}
    :kunde-sync
    {:transform-name :kunde-sync
     :output {:kind :object-type :object-type :kunde :primary-key :kunden-nr}}}})

(def ^:private sch
  {:object-types
   {:bankkonto {:fields [{:name :bankkonto-id :type :string}
                         {:name :inhaber-person-id :type :string}
                         {:name :person-id :type :string}]
                :implements [:geldkonto]}
    :person {:fields [{:name :person-id :type :string} {:name :bezeichnung :type :string}]
             :implements [:partei]}
    :organisation {:fields [{:name :organisation-id :type :string}] :implements [:partei]}
    :ort {:fields [{:name :ort-id :type :string}]}
    :kunde {:fields [{:name :kunden-nr :type :integer}]}
    :auftrag {:fields [{:name :auftrag-id :type :string}
                       {:name :kunden-nr :type :integer}
                       {:name :kunde :type :string}]}}
   :interfaces {:geldkonto {:link-constraints [{:link-name :inhaber :cardinality :one
                                                :to-interface :partei}]}
                :partei {}}
   :links [{:name :bankkonto-inhaber-person :from :bankkonto :to :person
            :cardinality :many-to-one :id "ri.sutra.main.link-type.1"
            :forward-api-name :inhaber-person :reverse-api-name :bankkonten}
           {:name :ort-teil-von :from :ort :to :ort :cardinality :many-to-one
            :id "ri.sutra.main.link-type.2" :forward-api-name :teil-von
            :reverse-api-name :teile :forward-name "Teil von" :reverse-name "Teile"}
           {:name :auftrag-kunde :from :auftrag :to :kunde :cardinality :many-to-one
            :id "ri.sutra.main.link-type.3"}]})

(defn- spec-of [form] (ltf/form->spec form canon ext-forms/ext->link-external-source))

(deftest the-fk-is-read-off-the-producer
  (is (= {:kind :fk :producer :link-bankkonto-inhaber-person :fk-side :from
          :fk-attribute :inhaber-person-id :on-null :skip :on-unresolved :skip}
         (ltf/backing-of canon :bankkonto-inhaber-person)))
  (testing "the lifted :endpoint-fk shape carries the key beside :output"
    (is (= :kunden-nr (:fk-attribute (ltf/backing-of canon :auftrag-kunde)))))
  (testing "no producer: links kept through Actions"
    (is (nil? (ltf/backing-of canon :ort-teil-von)))))

(deftest saving-an-installed-link-unchanged-sends-no-backing
  (let [form (ltf/load-form sch canon :bankkonto-inhaber-person)]
    (is (= :synced (:backing-mode form)))
    (is (= :inhaber-person-id (get-in form [:external :fk-attribute])))
    (is (= #{[:geldkonto :inhaber]} (:satisfies form)))
    (is (= {:spec {:name "bankkonto-inhaber-person" :from "bankkonto" :to "person"
                   :cardinality "many-to-one"
                   :forward-api-name "inhaber-person" :reverse-api-name "bankkonten"}}
           (spec-of form))))
  (testing "a link kept through Actions stays so, names and all"
    (is (= {:spec {:name "ort-teil-von" :from "ort" :to "ort" :cardinality "many-to-one"
                   :forward-api-name "teil-von" :reverse-api-name "teile"
                   :forward-name "Teil von" :reverse-name "Teile"}}
           (spec-of (ltf/load-form sch canon :ort-teil-von))))))

(deftest a-changed-fk-rewrites-the-producer
  (let [form (-> (ltf/load-form sch canon :bankkonto-inhaber-person)
                 (assoc-in [:external :fk-attribute] :person-id))]
    (is (= {:strategy :endpoint-fk :fk-side :from :fk-attribute :person-id
            :on-null :skip :on-unresolved :skip}
           (get-in (spec-of form) [:spec :external-source])))))

(deftest every-link-needs-a-backing
  (let [new {:name "x" :from "auftrag" :to "kunde" :cardinality "many-to-one"}]
    (is (= :refusal.scaffold/link-needs-fk (:error (spec-of (assoc new :backing-mode :synced)))))
    (is (= :refusal.scaffold/link-needs-fk
           (:error (spec-of (assoc new :backing-mode :in-system)))))
    (is (= {:strategy :endpoint-fk :fk-side :from :fk-attribute :kunden-nr}
           (get-in (spec-of (assoc new :backing-mode :synced
                                   :external {:fk-side :from :fk-attribute :kunden-nr}))
                   [:spec :external-source])))
    (testing "a new many-to-many needs its join table"
      (is (= :refusal.scaffold/link-needs-join
             (:error (spec-of (assoc new :cardinality "many-to-many"
                                     :backing-mode :in-system))))))))

(deftest the-pk-side-is-selected-for-you
  (is (= :kunden-nr (ltf/primary-key-of canon :kunde)))
  (is (nil? (ltf/primary-key-of canon :person)))
  (testing "FK pre-selected when name and type match the PK"
    (is (= :kunden-nr (ltf/suggest-fk sch canon :auftrag :kunde))))
  (testing "a key-less PK side: the <type>-id convention"
    (is (= :person-id (ltf/suggest-fk sch canon :bankkonto :person))))
  (testing "a self-link is never keyed on the type's own id"
    (is (nil? (ltf/suggest-fk sch canon :ort :ort))))
  (testing "every stored property can carry the key"
    (is (= [:auftrag-id :kunden-nr :kunde] (map :name (ltf/fk-candidates sch :auftrag))))))

(deftest interface-link-constraints
  (let [opts (ltf/constraint-options sch :bankkonto :person :many-to-one)]
    (is (= [{:interface :geldkonto :constraint :inhaber :target :partei
             :cardinality :one :required false :fits? true :why nil}]
           opts)))
  (is (= :target (:why (first (ltf/constraint-options sch :bankkonto :ort :many-to-one)))))
  (is (= :cardinality (:why (first (ltf/constraint-options sch :bankkonto :person :one-to-many)))))
  (testing "ticking one is sent; unticking all clears"
    (let [form (assoc (ltf/load-form sch canon :auftrag-kunde) :satisfies #{[:geldkonto :inhaber]})]
      (is (= [{:interface :geldkonto :constraint :inhaber}]
             (get-in (spec-of form) [:spec :satisfies]))))
    (let [form (assoc (ltf/load-form sch canon :bankkonto-inhaber-person) :satisfies #{})]
      (is (= [] (get-in (spec-of form) [:spec :satisfies]))))))

(deftest api-names-are-unique-per-object-type
  (let [new {:name "zweit" :from "bankkonto" :to "organisation" :cardinality "many-to-one"
             :backing-mode :synced :external {:fk-side :from :fk-attribute :person-id}}]
    (is (= {:error :ui/link-api-name-taken :args {:type "bankkonto" :api-name "inhaber-person"}}
           (spec-of (assoc new :forward-api-name "inhaber-person"))))
    (is (:spec (spec-of (assoc new :forward-api-name "inhaber-organisation"))))
    (testing "the link's own names do not collide with themselves"
      (is (:spec (spec-of (ltf/load-form sch canon :bankkonto-inhaber-person)))))))

(deftest satisfies-reaches-the-server
  (let [seen (atom nil)]
    (with-redefs [ontology/mutate! (fn [verb spec] (reset! seen [verb spec]))]
      (ontology/update-link-type! {:name "a" :from "b" :to "c" :cardinality "many-to-one"
                                   :satisfies [{:interface :geldkonto :constraint :inhaber}]}))
    (is (= [:update-link-type {:name :a :from :b :to :c :cardinality :many-to-one
                               :satisfies [{:interface :geldkonto :constraint :inhaber}]}]
           @seen))))

(deftest the-connect-modal-keeps-its-backing
  (let [seen (atom nil)
        on-create (some #(when (vector? %) (:on-create (second %)))
                        (tree-seq vector? seq (form-modals/connect-modal)))]
    (reset! catalog/canonical-schema canon)
    (fs/put! forms/connect-modal-form
             {:name "auftrag-kunde2" :from "auftrag" :to "kunde"
              :cardinality "many-to-one" :backing-mode :synced
              :external {:fk-side :from :fk-attribute :kunden-nr}})
    (with-redefs [ontology/add-link-type! (fn [spec] (reset! seen spec) (js/Promise.resolve nil))
                  ontology/mutate! (fn [_ spec] (reset! seen spec) (js/Promise.resolve nil))]
      (on-create))
    (is (= {:strategy :endpoint-fk :fk-side :from :fk-attribute :kunden-nr}
           (:external-source @seen)))))
