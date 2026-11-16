(ns sutra.serve.instance
  (:require [clojure.string :as str]
            [sutra.kit.system :as sys]
            [sutra.parser.spec :as spec]
            [sutra.http.responses :refer [ok]]
            [sutra.catalog.branch-policy :as policy]
            [sutra.types.source :as source]))

(set! *warn-on-reflection* true)

(defn describe
  [{:keys [routes identity offers catalogues layouts source-types profiles policy-fields]}]
  {:routes             (vec (sort routes))
   :identity-providers (vec (for [[id p] (sort-by key identity)]
                              {:id       id
                               :label    (:label p)
                               :source   (:source p)
                               :can      (vec (filter #(get p %) [:password :negotiate :directory]))
                               :settings (into (sorted-map)
                                               (for [[f s] (:settings p)]
                                                 [f (select-keys s [:type :secret? :input])]))}))
   :product-offers     (vec (for [[id o] (sort-by (fn [[id o]] [(:order o 100) (name id)]) offers)]
                              (assoc (select-keys o [:name :description :requires]) :id id)))
   :llm-catalogues     (vec (for [[id c] (sort-by key catalogues)] {:id id :host (:host c)}))
   :graph-layouts      (vec (for [[id l] (sort-by key layouts)]
                              (cond-> {:id id :label (:label l)}
                                (:doc l) (assoc :description (str/replace (:doc l) #"\s+" " ")))))
   :source-types       source-types
   :connector-profiles (into (sorted-map)
                             (for [[t p] profiles]
                               [t (select-keys p [:capabilities :endpoint :route :needs])]))
   :policy-fields      (mapv #(select-keys % [:key :kind :default]) policy-fields)})

(defn system-handler
  [req]
  (let [s (sys/of (:sutra/store req))]
    (ok (describe {:routes        (keys (sys/table s :http/routes))
                   :identity      (sys/table s :identity/providers)
                   :offers        (sys/table s :products/offers)
                   :catalogues    (sys/table s :llm/catalogues)
                   :layouts       (sys/table s :viz/layouts)
                   :source-types  (spec/source-types (source/connectors))
                   :profiles      (source/all-profiles)
                   :policy-fields policy/policy-fields}))))

(def module
  {:name        :instance
   :contributes {:http/routes {:system {:get system-handler}}}})
