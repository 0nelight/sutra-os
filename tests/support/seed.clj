(ns support.seed
  (:require [sutra.catalog.changes :as changes]
            [sutra.actions.publish :as onto-publish]
            [sutra.store.seat :as seat]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn identity-model!
  []
  (api/mutate :create-object-type {:name :user    :attributes [[:rolle :string]] :plural "Users"}
              {:stage? true})
  (api/mutate :create-object-type {:name :marking :attributes [[:name :string]] :plural "Markings"}
              {:stage? true})
  (api/mutate :create-link-type {:name :cleared-for :from :user :to :marking
                                 :cardinality :many-to-many}
              {:stage? true})
  (api/post! :staging))

(defn- seeding-store
  []
  (let [b (or tx/*branch* (seat/seated-branch))]
    (ctx/at (ctx/store b grants/system-authority) b)))

(defn- rid-of [kind type-kw]
  (or (lookup/rid-of (seat/branch-schema (seeding-store)) kind type-kw)
      (throw (ex-info (str "seed: unknown " (name kind) " " type-kw
                           " — stage + commit the type first")
                      {:kind kind :type type-kw}))))

(defn objects!
  [type-kw rows]
  (let [rid (rid-of :object-types type-kw)]
    (onto-publish/publish! (seeding-store) (seat/branch-schema (seeding-store))
                           (mapv (fn [{:keys [id values]}] (changes/create-object id rid values))
                                 rows)
                           {:kind :objects :verb :create :type rid}
                           nil)))

(defn object!
  ([type-kw values] (object! type-kw nil values))
  ([type-kw id values] (objects! type-kw [{:id id :values values}])))

(defn update!
  [type-kw id values]
  (let [rid (rid-of :object-types type-kw)]
    (onto-publish/publish! (seeding-store) (seat/branch-schema (seeding-store))
                           [(changes/update-object id rid values)]
                           {:kind :objects :verb :update :type rid}
                           nil)))

(defn delete!
  [type-kw id]
  (let [rid (rid-of :object-types type-kw)]
    (onto-publish/publish! (seeding-store) (seat/branch-schema (seeding-store))
                           [(changes/delete-object id rid)]
                           {:kind :objects :verb :delete :type rid}
                           nil)))

(defn links!
  [link-kw pairs]
  (let [rid (rid-of :link-types link-kw)]
    (onto-publish/publish! (seeding-store) (seat/branch-schema (seeding-store))
                           (mapv (fn [{:keys [from-id to-id]}] (changes/connect rid from-id to-id))
                                 pairs)
                           {:kind :links :verb :create :type rid}
                           nil)))

(defn link!
  [link-kw from-id to-id]
  (links! link-kw [{:from-id from-id :to-id to-id}]))
