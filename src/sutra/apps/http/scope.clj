(ns sutra.apps.http.scope
  (:require [sutra.kit.coll :as coll]
            [sutra.types.versions :as vers]
            [clojure.string :as str]
            [sutra.capps.catalog :as cat]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.apps.http.standard-view :refer [app-type-rid]]
            [sutra.access.api :as access]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]))

(set! *warn-on-reflection* true)

(defn- project-ops
  [auth schema spec]
  (let [granted (grants/project-operations auth)]
    (if (grants/all-projects? auth)
      ::full
      (let [p   (:project spec)
            rid (when p (if (string? p) p (lookup/rid-of schema :projects p)))]
        (get granted rid #{})))))

(defn resource-access
  [auth schema a]
  (let [ops (project-ops auth schema a)]
    (cond (or (= ::full ops) (contains? ops :read)) :open
          (contains? ops :discover)                 :locked)))

(defn workspace-rights
  [auth schema spec]
  (let [ops (project-ops auth schema spec)
        has (fn [op] (or (= ::full ops) (contains? ops op)))]
    {:view    (boolean (or (has :read) (has :discover)))
     :edit    (boolean (has :update))
     :promote (boolean (has :manage-permissions))}))

(defn editable-projects
  [auth schema]
  (vec (sort-by :name
                (for [[rid p] (:projects schema)
                      :let  [ops (project-ops auth schema {:project rid})]
                      :when (or (= ::full ops) (contains? ops :update))]
                  {:id rid :name (:project-name p)}))))

(defn readable-app
  [auth schema app-rid]
  (let [as (:applications schema)
        a  (or (get as app-rid) (get as (lookup/rid-of schema :applications app-rid)))]
    (when (and a (access/granted? auth schema a))
      a)))

(defn granted-app
  [auth schema app-rid]
  (let [a (readable-app auth schema app-rid)]
    (when (vers/published? a)
      a)))

(defn app-type-name
  [schema rid]
  (or (get-in schema [:object-types rid :object-type-name])
      (get-in schema [:interfaces rid :interface-name])))

(def ^:private scope-axes
  [{:axis :bound-type :noun "object type" :->id #'app-type-rid :paths :bound-type}
   {:axis :type :noun "object type" :->id #'app-type-rid :hides-hidden? true :paths :type}
   {:axis :object-type :noun "object type" :->id #(lookup/->type-rid %1 :object-types %2)
    :hides-hidden? true :paths :type}
   {:axis :link :noun "link" :->id #(lookup/->type-rid %1 :link-types %2) :paths :link}
   {:axis :function :noun "function" :->id (fn [_ t] (->kw t)) :paths :function}])

(def ^:private axis-of
  (coll/index-by :axis scope-axes))

(defn app-scope
  [schema a]
  (into {}
        (map (fn [{:keys [axis paths] f :->id}]
               [axis (into #{} (keep #(f schema %)) (cat/scope-tokens a paths))]))
        scope-axes))

(defn in-scope
  [auth schema scope axis token]
  (let [{:keys [hides-hidden?] f :->id} (axis-of axis)
        id (f schema token)]
    (when (and id
               (contains? (get scope axis) id)
               (not (and hides-hidden? (contains? (grants/hidden-object-types auth) id))))
      id)))

(defn not-in-app
  [axis & tokens]
  {:error (str "no " (:noun (axis-of axis)) " " (str/join ", " (map pr-str tokens))
               " in this application")})

(defn rid-str
  [x]
  (if (keyword? x) (subs (str x) 1) x))
