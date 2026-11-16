(ns sutra-ui.forms.events
  (:require [sutra-ui.credentials :as creds]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.requests :as rq]))

(db/reg :shared.forms/stage-link
        (fn [s [_ spec create? on-done]]
          (rq/request s {:ask [(if create? ontology/add-link-type! ontology/update-link-type!)
                               spec]
                         :fx  [[:invoke [on-done]]]})))

(db/reg :shared.forms/stage-object
        (fn [s [_ save on-done]]
          (rq/request s {:ask  [#(or (save) (js/Promise.resolve ::invalid))]
                         :then [[:shared.forms/staged-object on-done]]})))

(db/reg :shared.forms/staged-object
        (fn [_ [_ on-done r]] (when-not (= ::invalid r) {:fx [[:invoke [on-done]]]})))

(db/reg :shared.forms/save-datasource
        (fn [s [_ base cred source on-done]]
          (if (seq cred)
            (rq/request s {:ask    [creds/seal cred {:source source}]
                           :then   [[:shared.forms/sealed base on-done]]
                           :report "seal"})
            {:fx [[:dispatch [:shared.forms/sealed base on-done nil]]]})))

(db/reg :shared.forms/sealed
        (fn [s [_ base on-done cipher]]
          (rq/request s {:ask [data/upsert-datasource!
                               (cond-> base cipher (assoc :secret-cipher cipher))]
                         :fx  [[:invoke [on-done]]]})))

(db/reg :shared.forms/create-table
        (fn [s [_ ds-key body err on-done]]
          (update (rq/request s {:ask  [data/create-datasource-table! ds-key body]
                                 :then [[:shared.forms/table-created ds-key err on-done]]
                                 :else [[:shared.forms/table-failed err]]})
                  :fx #(into [[:invoke [reset! err nil]]] %))))

(db/reg :shared.forms/table-created
        (fn [s [_ ds-key err on-done]]
          (rq/request s {:ask  [data/reload-ds-tables! ds-key]
                         :fx   [[:invoke [on-done]]]
                         :else [[:shared.forms/table-failed err]]})))

(db/reg :shared.forms/table-failed
        (fn [_ [_ err sentence]] {:fx [[:invoke [reset! err sentence]]]}))
