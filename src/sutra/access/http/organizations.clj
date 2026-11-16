(ns sutra.access.http.organizations
  (:require [sutra.kit.refusal :as refusal]
            [sutra.catalog.grants :as grants]
            [sutra.http.responses :refer [ok]]
            [sutra.types.authority :as authority]
            [sutra.types.ontologies :as ontologies]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- admin? [auth] (= grants/admin-username (:username auth)))

(defn- viewer [auth] (set (get-in auth [:subject :organizations])))

(defn- org-name [schema o]
  (or (some-> (get-in schema [:organizations o :organization-name]) rid/->name) (str o)))

(defn- discoverable? [auth schema o aspect]
  (or (admin? auth) (ontologies/discoverable? schema (viewer auth) o aspect)))

(defn- blocked? [auth x]
  (and (not (admin? auth)) (contains? (set (:blocked-resources auth)) x)))

(defn- unseen [what nm]
  (refusal/refused :refusal.access/no-named-you-may-see {} {:what what :nm nm}))

(defn- spaces-of [schema o]
  (vec (sort (for [s (keys (:spaces schema))
                   :when (contains? (authority/space-organizations schema s) o)]
               (ontologies/ontology-name schema s)))))

(defn- org-row [schema o]
  (let [spec (get-in schema [:organizations o])]
    {:organization (str o)
     :name         (org-name schema o)
     :members      (count (:members spec))
     :guests       (count (:guests spec))
     :spaces       (spaces-of schema o)}))

(defn organizations-handler
  [{schema :sutra/schema auth :sutra/auth}]
  (ok {:organizations (vec (for [o (sort-by #(org-name schema %) (keys (:organizations schema)))
                                 :when (discoverable? auth schema o :name)]
                             (org-row schema o)))}))

(defn organization-handler
  [{{nm :name} :path-params schema :sutra/schema auth :sutra/auth}]
  (let [o (authority/organization-rid schema nm)]
    (if (or (nil? o) (not (discoverable? auth schema o :name)))
      (unseen "organization" nm)
      (let [spec   (get-in schema [:organizations o])
            users? (discoverable? auth schema o :users)]
        (ok (merge (org-row schema o)
                   {:description       (:description spec)
                    :home-space        (some->> (ontologies/home-space schema o)
                                                (ontologies/ontology-name schema))
                    :collaborators     (vec (sort (map #(org-name schema %)
                                                       (ontologies/collaborators schema o))))
                    :discovery         (merge {:name :everyone :users :everyone
                                               :groups :everyone}
                                              (:discovery spec))
                    :apply             (vec (:apply spec))
                    :expand-access     (vec (:expand-access spec))}
                   (when users?
                     {:member-ids (vec (:members spec)) :guest-ids (vec (:guests spec))})))))))

(defn- space-row [schema s]
  (let [projects (filter #(= s (sch/project-space schema %)) (keys (:projects schema)))]
    {:space         (str s)
     :name          (ontologies/ontology-name schema s)
     :organizations (vec (sort (map #(org-name schema %)
                                    (authority/space-organizations schema s))))
     :ontology      (ontologies/kind schema s)
     :projects      (count projects)}))

(defn- spaces [schema]
  (cons sch/default-space (sort-by str (keys (:spaces schema)))))

(defn spaces-handler
  [{schema :sutra/schema auth :sutra/auth}]
  (ok {:spaces (vec (for [s (spaces schema) :when (not (blocked? auth s))]
                      (space-row schema s)))}))

(defn space-handler
  [{{nm :name} :path-params schema :sutra/schema auth :sutra/auth}]
  (let [s (ontologies/resolve-ontology schema nm)]
    (if (or (nil? s) (blocked? auth s))
      (unseen "space" nm)
      (ok (assoc (space-row schema s)
                 :project-names
                 (vec (sort (for [p (keys (:projects schema))
                                  :when (= s (sch/project-space schema p))]
                              (or (some-> (get-in schema [:projects p :project-name]) rid/->name)
                                  (str p))))))))))

(defn ontologies-handler
  [{schema :sutra/schema auth :sutra/auth}]
  (ok {:ontologies (vec (remove #(blocked? auth (ontologies/resolve-ontology schema (:ontology %)))
                                (ontologies/ontologies schema)))}))

(defn ontology-handler
  [{{nm :name} :path-params schema :sutra/schema auth :sutra/auth}]
  (let [s (ontologies/resolve-ontology schema nm)]
    (if (or (nil? s) (blocked? auth s))
      (unseen "ontology" nm)
      (let [o (ontologies/in-ontology schema s)]
        (ok (merge (some #(when (= (str s) (:ontology %)) %) (ontologies/ontologies schema))
                   {:resources
                    (into (sorted-map)
                          (for [section ontologies/sections
                                :let [nf (rid/name-field-of section)
                                      ns (sort (keep #(some-> (get (val %) nf) rid/->name)
                                                     (get o section)))]
                                :when (seq ns)]
                            [section (vec ns)]))}))))))

(def module
  {:name        :organizations
   :contributes {:http/routes
                 {:organizations {:get organizations-handler}
                  :organization  {:get organization-handler}
                  :spaces        {:get spaces-handler}
                  :space         {:get space-handler}
                  :ontologies    {:get ontologies-handler}
                  :ontology      {:get ontology-handler}}}})
