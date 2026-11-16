(ns sutra.checkouts.http
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.authoring.http.mutation :as mutation]
            [sutra.catalog.grants :as grants]
            [sutra.checkouts.rename :as rename]
            [sutra.checkouts.slice :as slice]
            [sutra.http.responses :refer [ok]]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- ctx-of
  [{store :sutra/store schema :sutra/schema branch :sutra/branch e :sutra/edge
    auth :sutra/auth :as req}]
  (let [user (or (ep/signed-in-user req) "sutra")]
    {:store store :schema schema :branch branch :edge e :auth auth
     :admin? (and (= user grants/admin-username) (not (:sutra/agent? req)))
     :author {:name user :email (str user "@sutra")}}))

(defn pull-slice
  [{:keys [query-params] :as req}]
  (let [nodes (get query-params "nodes")]
    (if (str/blank? nodes)
      (refusal/refused :refusal.checkouts/a-slice-needs-nodes {})
      (ok (slice/pull (ctx-of req) nodes)))))

(defn push-slice
  [{:keys [body-params] :as req}]
  (ok (slice/push! (ctx-of req) body-params)))

(defn- rename-of
  [kind]
  (let [kinds-key (rid/tag->kinds-key kind)]
    (fn [{{nm :name} :path-params {to :to} :body-params :as req}]
      (mutation/answer (rename/rename! (ctx-of req) kinds-key (keyword nm) (keyword to))))))

(def module
  {:name        :checkouts-http
   :contributes {:http/routes
                 {:slices             {:get pull-slice :post push-slice}
                  :dataset-rename     {:post (rename-of :dataset)}
                  :object-type-rename {:post (rename-of :object-type)}
                  :conn-type-rename   {:post (rename-of :link-type)}}}})
