(ns ^:integration ontology.object-key-door-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.authoring.install.world :as onto-install]
            [sutra.actions.operations :as ops]
            [sutra.actions.execution :as x]
            [sutra.actions.action :as action]
            [sutra.objects.views :as views]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(def ^:private schema
  {:object-types
   {:kunde    {:object-type-name :kunde :match-keys [:kunde-id]
               :attribute-list [{:attribute-name :kunde-id :attribute-type :string}
                                {:attribute-name :name :attribute-type :string}]
               :plural "Kunden"}
    :svs-jahr {:object-type-name :svs-jahr :match-keys [:jahr]
               :attribute-list [{:attribute-name :jahr :attribute-type :integer}]
               :plural "Svs jahre"}
    :notiz    {:object-type-name :notiz
               :attribute-list [{:attribute-name :text :attribute-type :string}
                                {:attribute-name :kunde-name :attribute-type :string}]
               :plural "Notizen"}}
   :link-types {}
   :actions {:notieren
             {:params {:kunde {:name "Kunde" :type :kunde :required true}
                       :text  {:name "Text" :type :string}}
              :rules [{:op :create :object-type :notiz
                       :values {:text [:get :text] :kunde-name [:get :kunde :name]}}]}}})

(def ^:private world (@#'onto-install/normalize-schema schema))

(defn- T [k] (lookup/rid-of world :object-types k))

(defn- submit! [branch txv]
  (store/submit-tx-vector! (ctx/store branch grants/system-authority) txv))

(defn- act! [branch args]
  (let [r   (ops/execute-action world {}
                                {:reads? true :actor "tester" :branch branch
                                 :authority grants/system-authority}
                                (x/io (action/store-io (ctx/store branch grants/system-authority)
                                                       (ctx/code)))
                                :notieren {:arg0 args})]
    (submit! branch (into (vec (:ops r)) (:side-effects r)))
    r))

(defn- create [type id values]
  {:op :create-object :type type :payload {:id id :values values}})

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest the-key-names-the-object-and-is-taken-once
  (let [branch (str "okey-" (subs (uuid) 0 8))
        k1     (uuid)]
    (try
      (store/fork! branch)
      (store/install! branch world)
      (submit! branch [(create (T :kunde) k1 {:kunde-id "k-airline" :name "Alpenflug"})
                       (create (T :svs-jahr) (uuid) {:jahr 2022})])
      (submit! branch [(create (T :kunde) (uuid) {:kunde-id "k-schlosser" :name "Schlosser"})])

      (testing "31: a create whose key exists is refused with a 409 naming the key"
        (let [e (refusal #(submit! branch
                                   [(create (T :kunde) (uuid)
                                            {:kunde-id "k-airline" :name "zweite"})]))]
          (is (some? e))
          (is (= 409 (anomaly/status (ex-data e))))
          (is (= :object-key-taken (:rule (ex-data e))))
          (is (= {:kunde-id "k-airline"} (:key (ex-data e))))
          (is (= k1 (:taken-by (ex-data e)))))
        (is (= 2 (count (store/objects branch (T :kunde))))
            "and nothing was written"))
      (testing "an integer key, and a key created twice in ONE submission"
        (is (some? (refusal #(submit! branch [(create (T :svs-jahr) (uuid) {:jahr 2022})]))))
        (let [e (refusal #(submit! branch [(create (T :svs-jahr) (uuid) {:jahr 2023})
                                           (create (T :svs-jahr) (uuid) {:jahr 2023})]))]
          (is (= :same-submission (:taken-by (ex-data e))))))
      (testing "a fresh key and a type without one pass"
        (submit! branch [(create (T :kunde) (uuid) {:kunde-id "k-maler" :name "Maler"})
                         (create (T :notiz) (uuid) {:text "a"})
                         (create (T :notiz) (uuid) {:text "a"})])
        (is (= 3 (count (store/objects branch (T :kunde))))))

      (testing "34: an action's object parameter takes the key as well as the object id"
        (let [r (act! branch {:kunde "k-airline" :text "per Schluessel"})]
          (is (some #(= "Alpenflug" (get-in % [:after :kunde-name])) (:side-effects r))))
        (act! branch {:kunde k1 :text "per Id"})
        (is (= #{"per Schluessel" "per Id"}
               (into #{}
                     (keep #(when (= "Alpenflug" (get-in % [:values :kunde-name]))
                              (get-in % [:values :text])))
                     (store/objects branch (T :notiz)))))
        (let [e (refusal #(act! branch {:kunde "k-gibtsnicht" :text "x"}))]
          (is (= 422 (anomaly/status (ex-data e))))
          (is (re-find #"weder als kunde-id" (str (ex-message e))))))

      (testing "51: the template door resolves the same text through the same resolver"
        (do
          (is (= k1
                 (str
                  (:id
                   (:object
                    (views/resolve-object-ref (ctx/store branch grants/system-authority) world
                                              (T :kunde) "k-airline"))))))
          (is (= k1
                 (str
                  (:id
                   (:object
                    (views/resolve-object-ref (ctx/store branch grants/system-authority) world
                                              (T :kunde) k1))))))
          (is (= 2022
                 (get-in
                  (views/resolve-object-ref (ctx/store branch grants/system-authority) world
                                            (T :svs-jahr) "2022") [:object :values :jahr])))
          (is (= :unresolved
                 (get-in
                  (views/resolve-object-ref (ctx/store branch grants/system-authority) world
                                            (T :svs-jahr) "zweitausend")
                  [:problem :reason])))))
      (finally
        (try (store/retire! branch) (catch Exception _ nil))))))
