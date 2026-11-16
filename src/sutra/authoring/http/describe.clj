(ns sutra.authoring.http.describe
  (:require [clojure.string :as str]
            [sutra.cli.view :as view]
            [sutra.objects.api :as objects]
            [sutra.http.endpoint :as ep]
            [sutra.authoring.http.oms :as oms]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn- count-of
  [store branch]
  (fn [ot]
    (let [st (ctx/at store branch)]
      (:count (objects/run-object-query st (seat/rebuild-schema-from-storage st)
                                      {:find ot :count? true :limit 0})))))

(def schema-describe
  (ep/handler :schema-describe
              {:call (fn [{:keys [store branch schema names project no-count]}]
                       (view/described schema
                                       (mapv keyword (remove str/blank?
                                                             (str/split (str names) #",")))
                                       (not-empty project)
                                       {:backing  (:body (oms/schema-backing
                                                          {:sutra/schema schema}))
                                        :count-of (if no-count
                                                    (constantly nil)
                                                    (count-of store branch))}))}))

(def schema-sections
  (ep/handler :schema-sections
              {:call (fn [{:keys [schema sections]}]
                       (select-keys schema (map keyword (remove str/blank?
                                                                (str/split (str sections)
                                                                           #",")))))}))

(def module
  {:name        :describe
   :contributes {:http/routes {:schema-describe {:get schema-describe}
                               :schema-sections {:get schema-sections}}}})
