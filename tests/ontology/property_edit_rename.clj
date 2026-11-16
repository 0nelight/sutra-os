(ns ^:integration ontology.property-edit-rename
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.signature :as signature]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.world :as world]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defn- W [T a & more]
  {:object-types {T {:object-type-name T
                     :attribute-list (mapv (fn [x] {:attribute-name x :attribute-type :string})
                                           (cons a more))
                     :plural "Objekte"}}
   :link-types {}})

(defn- onto
  [W W']
  (reduce-kv (fn [W t o]
               (let [r (or (some (fn [[r e]] (when (= t (:object-type-name e)) r))
                                 (:object-types W))
                           t)]
                 (update-in W [:object-types r] merge o)))
             W (:object-types W')))

(defn- rename! [b W' T from to]
  (world/install! (ctx/store b)
                  (ri/derived {:sigma clock/system} (onto (world/authored (ctx/store b)) W'))
                  {:steps         [[:rename-attribute T from to]]
                   :dataset-decls signature/datasets-of :actor {:principal "test"}}))

(defn- edited [b T] (mapv :values (world/edit-provenance (ctx/store b) T {})))

(defn- edited-attrs [b T] (into #{} (mapcat (comp :attrs val)) (world/edited (ctx/store b) T)))

(defn- history [b T] (mapv :edited-values (world/edit-history (ctx/store b) T {})))

(deftest ein-umbenannter-edit-wandert-mit
  (let [t      (keyword (str "konto-" (subs (uuid) 0 8)))
        trunk  "main"
        branch (str "umb-" (subs (uuid) 0 8))
        pk     (uuid)
        old-w  (W t :währung :andere)
        new-w  (W t :waehrung :andere)]
    (store/install! trunk (merge-with merge (world/authored (ctx/store trunk)) old-w))
    (store/insert-object! trunk t {:id pk :values {:währung "ATS"}})
    (store/update-object! trunk t pk {:währung "EUR"})
    (store/fork! trunk branch)
    (try
      (testing "auf dem Zweig: eine eigene Kopie, der Stamm bleibt"
        (rename! branch new-w t :währung :waehrung)
        (is (= "EUR" (get-in (store/object branch t pk) [:values :waehrung])))
        (is (= #{:waehrung} (edited-attrs branch t)))
        (is (= "EUR" (get-in (first (edited branch t)) [:waehrung :v])))
        (is (not (contains? (first (edited branch t)) :währung)))
        (is (= #{:währung} (edited-attrs trunk t)) "der Stamm ist unberuehrt")
        (is (= "EUR" (get-in (first (edited trunk t)) [:währung :v])) "der Stamm ist unberuehrt"))
      (testing "zweimal ist wie einmal"
        (rename! branch new-w t :währung :waehrung)
        (is (= 1 (count (edited branch t))))
        (is (= #{:waehrung} (edited-attrs branch t))))
      (testing "auf dem Stamm: an Ort und Stelle, samt Historie"
        (rename! trunk new-w t :währung :waehrung)
        (is (= #{:waehrung} (edited-attrs trunk t)))
        (is (= "EUR" (get-in (first (edited trunk t)) [:waehrung :v])))
        (is (= [{:waehrung "EUR"} {:waehrung "ATS"}] (history trunk t))))
      (testing "eine unbeteiligte Eigenschaft bleibt, wie sie ist"
        (rename! trunk (W t :waehrung :anders) t :andere :anders)
        (is (= #{:waehrung} (edited-attrs trunk t)))
        (is (= [{:waehrung "EUR"} {:waehrung "ATS"}] (history trunk t))))
      (finally
        (try (store/retire! branch) (catch Exception _ nil))))))
