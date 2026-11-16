(ns sutra-ui.state.forms
  (:require [sutra-ui.state.db :as db]))

(def form-defaults
  {:new-object      {:name "" :plural "" :fields [] :implements [] :external nil}
   :new-link  {:name "" :from "" :to "" :cardinality "many-to-one"
               :external nil}
   :connect-modal   {:name "" :from "" :to "" :cardinality "many-to-one"}
   :edit-object     {:name "" :plural "" :fields [] :implements [] :external nil}
   :edit-link {:name "" :from "" :to "" :cardinality ""
               :external nil}
   :create-entity   {:type nil :values {}}
   :new-interface   {:name "" :display-name "" :description "" :status "experimental"
                     :searchable true :extends [] :properties [] :links []}
   :edit-interface  {:name "" :display-name "" :description "" :status "experimental"
                     :searchable true :extends [] :properties [] :links []}
   :new-object-type-group  {:name "" :description "" :members []}
   :edit-object-type-group {:name "" :description "" :members []}
   :new-function    {:name "" :target-type "" :language "clojure"
                     :source ""
                     :param-types []}
   :edit-function   {:name "" :target-type "" :language "clojure"
                     :source ""
                     :param-types []}})

(defonce forms (db/cursor [:forms :forms] form-defaults))

(deftype Form [id]
  IDeref
  (-deref [_] (get @forms id))
  IEquiv
  (-equiv [_ o] (and (instance? Form o) (= id (.-id ^Form o))))
  IHash
  (-hash [_] (hash [::form id])))

(defn form
  [id]
  (Form. id))

(db/reg :sutra.forms/edit
        (fn [s [_ id f args]] {:db (update-in s [:forms :forms id] #(apply f % args))}))

(db/reg :sutra.forms/put
        (fn [s [_ id v]] {:db (assoc-in s [:forms :forms id] v)}))

(db/reg :sutra.forms/reset
        (fn [s [_ id]] {:db (assoc-in s [:forms :forms id] (get form-defaults id))}))

(defn edit!
  [form f & args]
  (if (instance? Form form)
    (db/send! [:sutra.forms/edit (.-id ^Form form) f args])
    (apply swap! form f args)))

(defn put!
  [form v]
  (if (instance? Form form)
    (db/send! [:sutra.forms/put (.-id ^Form form) v])
    (reset! form v)))

(deftype At [form path]
  IDeref
  (-deref [_] (get-in @form path))
  IReset
  (-reset! [_ v] (edit! form assoc-in path v) v)
  IEquiv
  (-equiv [_ o] (and (instance? At o) (= form (.-form ^At o)) (= path (.-path ^At o))))
  IHash
  (-hash [_] (hash [::at form path])))

(defn at
  [form path]
  (At. form (if (sequential? path) (vec path) [path])))

(defn reset-form! [form-id] (db/send! [:sutra.forms/reset form-id]))

(defn set-form! [form-id value] (db/send! [:sutra.forms/put form-id value]))
