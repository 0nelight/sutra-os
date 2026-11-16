(ns ^:unit ontology.edit-surface-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.mem :as mem]
            [sutra.store.world :as world]))

(def ^:private konto-rid   "ri.sutra.main.object-type.8f4c36f9-3ea7-4e54-b12d-17ea707983bf")
(def ^:private buchung-rid "ri.sutra.main.object-type.01ad4573-c08a-45c9-800f-da8508608b74")

(defn- attrs [& as] (mapv (fn [a] {:attribute-name a :attribute-type :string}) as))

(def ^:private schema
  {:object-types {konto-rid   {:object-type-name :konto
                               :attribute-list (attrs :bezeichnung :gruppe :name)
                               :plural "Konten"}
                  buchung-rid {:object-type-name :buchungssatz :attribute-list (attrs :netto)
                               :plural "Buchungssätze"}}})

(def ^:private actor {:principal "test"})

(defn- store []
  {:db (mem/store {:world schema}) :clock #(java.util.Date.)})

(defn- edit! [s ops]
  (world/commit! (world/at s "main") {:ops (vec ops) :actor actor :at-head? true}))

(defn- update-op [T id values] {:op :update-object :type T :payload {:id id :values values}})
(defn- create-op [T id values] {:op :create-object :type T :payload {:id id :values values}})
(defn- delete-op [T id] {:op :delete-object :type T :payload {:id id}})

(defn- surface
  ([ops] (surface ops schema))
  ([ops W] (let [s (store)]
             (when (seq ops) (edit! s ops))
             (world/edit-surface (world/at s "main") W))))

(defn- edited [ops] (:properties (surface ops)))

(defn- seeded
  [ops]
  (let [s (store)]
    (world/index! (world/at s "main") [(create-op konto-rid "1" {:bezeichnung "a"})
                            (create-op konto-rid "2" {:bezeichnung "b"})
                            (create-op buchung-rid "3" {:netto "1"})])
    (when (seq ops) (edit! s ops))
    (world/edit-surface (world/at s "main") schema)))

(deftest the-edits-name-the-edited-columns
  (testing "each edited column becomes one [object-type property] pair"
    (is (= #{[:konto :bezeichnung] [:konto :gruppe]}
           (:properties (seeded [(update-op konto-rid "1" {:bezeichnung "x" :gruppe "y"})])))))

  (testing "pairs are unioned across objects and types"
    (is (= #{[:konto :bezeichnung] [:buchungssatz :netto]}
           (:properties (seeded [(update-op konto-rid "1" {:bezeichnung "x"})
                                 (update-op konto-rid "2" {:bezeichnung "z"})
                                 (update-op buchung-rid "3" {:netto "1"})])))))

  (testing "no edits at all is an EMPTY set, never nil"
    (is (= #{} (edited [])))
    (is (= {:types #{} :properties #{}} (surface [])))))

(deftest what-the-index-wrote-is-no-edit
  (is (= {:types #{} :properties #{}} (seeded []))
      "objects the index created leave the surface empty"))

(deftest an-edit-on-a-type-no-longer-in-the-schema-is-dropped
  (is (= {:types #{:konto} :properties #{[:konto :name]}}
         (surface [(create-op konto-rid "1" {:name "x"}) (create-op buchung-rid "2" {:netto "1"})]
                  {:object-types {konto-rid (get-in schema [:object-types konto-rid])}}))
      "a retired type's edits cannot break a property that no longer exists"))

(deftest the-lookup-finds-the-type-however-the-writer-spelled-it
  (testing "an edit written under the RID resolves to its type"
    (is (= #{[:konto :name]} (edited [(create-op konto-rid "1" {:name "oja.at"})]))))
  (testing "an edit written under the NAME resolves to the same type"
    (is (= #{[:konto :name]} (edited [(create-op :konto "1" {:name "oja.at"})])))))

(deftest the-types-grain-counts-the-object-not-the-values
  (testing "a deletion contributes a TYPE and no property"
    (is (= {:properties #{} :types #{:konto}}
           (seeded [(delete-op konto-rid "1")]))))

  (testing "an update contributes both"
    (is (= {:properties #{[:konto :name]} :types #{:konto}}
           (seeded [(update-op konto-rid "1" {:name "x"})]))))

  (testing "a change of markings contributes the TYPE, not a property"
    (is (= {:properties #{} :types #{:konto}}
           (seeded [{:op :update-markings :type konto-rid :payload {:id "1" :add [:secret]}}])))))

(deftest jede-umstufung-bleibt-als-eigene-zeile-stehen
  (let [s     (store)
        stufe (fn [id v] (update-op konto-rid id {:gruppe v}))]
    (world/index! (world/at s "main") [(create-op konto-rid "a1" {:gruppe "NEU"})])
    (edit! s [(stufe "a1" "AKT25") (stufe "a1" "AKT50")])
    (testing "zwei Umstufungen desselben Objekts sind ZWEI Verlaufszeilen, neueste zuerst"
      (let [rows (world/edit-history (world/at s "main") konto-rid {:pk "a1"})]
        (is (= 2 (count rows)))
        (is (= ["AKT50" "AKT25"] (map #(get-in % [:edited-values :gruppe]) rows)))
        (is (= ["update" "update"] (map :edit-kind rows)))))
    (testing "Anlegen und Loeschen tragen ihre eigene Art"
      (edit! s [(create-op konto-rid "a2" {:gruppe "NEU"}) (delete-op konto-rid "a2")])
      (is (= ["delete" "create"]
             (map :edit-kind (world/edit-history (world/at s "main") konto-rid {:pk "a2"})))))))
