(ns sutra.store.tx-vector
  (:require [malli.core :as m]
            [sutra.store.api :as api]))

(set! *warn-on-reflection* true)

(defn event?
  [e]
  (contains? e :target))

(defn store-op?
  [e]
  (and (not (event? e)) (contains? api/op-descriptors (:op e))))

(def ^:private event-schema
  [:map [:op :keyword] [:target [:maybe map?]]])

(def tx-vector-schema
  [:sequential (-> (into [:multi {:dispatch (fn [e] (if (event? e) ::event (:op e)))}]
                         (for [[op {:keys [payload]}] api/op-descriptors]
                           [op [:map [:op [:= op]] [:type some?] [:payload payload]
                                [:expected-version {:optional true} any?]]]))
                   (conj [::event event-schema])
                   (conj [::m/default [:map [:op :keyword]]]))])

(defn explain-tx-vector
  [txv]
  (m/explain tx-vector-schema txv))

(defn store-write?
  [op]
  (and (store-op? op) (contains? op :payload)))

(defn object-write?
  [op]
  (and (store-write? op) (boolean (get-in api/op-descriptors [(:op op) :object?]))))

(defn link-write?
  [op]
  (and (store-write? op) (boolean (get-in api/op-descriptors [(:op op) :link?]))))

(defn edit-kind
  [op]
  (get-in api/op-descriptors [op :edit-kind]))

(defn authorized-as
  [{:keys [op payload]}]
  (if (= :update-markings op)
    (cond-> [] (seq (:add payload)) (conj :apply-marking)
      (seq (:remove payload)) (conj :remove-marking))
    [(get-in api/op-descriptors [op :authorized-as])]))

(defn ledger-type
  [ops]
  (let [obj (filter #(get-in api/op-descriptors [(:op %) :object?]) ops)]
    (cond
      (empty? obj) nil
      (some #(= :DELETE (get-in api/op-descriptors [(:op %) :ledger])) obj) :DELETE
      :else :APPEND)))
