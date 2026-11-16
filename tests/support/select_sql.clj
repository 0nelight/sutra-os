(ns support.select-sql
  (:require [sutra.types.world-rules :as rules]
            [sutra.store.core.answers]
            [sutra.store.lib :as lib]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.select :as select]
            [sutra.store.pg.sql :as sql]
            [sutra.types.sql-ident :as sqli]))

(defn cx
  ([W] (cx W {}))
  ([W {:keys [denied seen]}]
   (cond-> {:sig    (rules/signature W)
            :denied (or denied #{})
            :rel    (comp sql/frag sqli/quote-ident)
            :edges  (fn [L]
                      (some-> seen (swap! conj (layout/link-table L)))
                      {:from (sql/frag (sqli/quote-ident (layout/link-table L))) :derived? false})}
     seen (assoc :seen seen))))

(defn plan
  [W ir]
  (lib/plan W (#'sutra.store.core.answers/readable-ir W ir)))

(defn compile-sql
  ([W ir] (compile-sql W ir {}))
  ([W ir opts]
   (let [{:keys [sql params]} (select/compile-plan (cx W opts) (plan W ir))]
     [sql params])))

(defn compile-read
  [W ir]
  (let [seen (atom #{})
        [sql params] (compile-sql W ir {:seen seen})]
    {:sql sql :params (vec params) :tables @seen}))
