(ns ^:integration store.pg-cost-test
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [sutra.store.conformance :as store-conf]
            [sutra.store.core :as core]
            [sutra.store.core.engine]
            [sutra.store.pg :as store-pg]
            [support.pod :as pod]))

(defn- driver
  [handles]
  {:engine (fn []
             (let [h  (pod/start-empty-pg!)
                   ds (jdbc/get-datasource {:jdbcUrl (:url h) :user (:user h)
                                            :password (:password h)})]
               (swap! handles conj h)
               (store-pg/engine {:datasource ds :records {}})))
   :db     (fn [engine collections]
             (doto (core/db engine {:collections collections}) core/adopt!))})

(deftest pg-keeps-the-cost-laws
  (let [handles (atom [])]
    (try
      (binding [sutra.store.core.engine/*one-handle-per-lane* false]
        (let [measured (store-conf/check-cost (driver handles))]
          (testing "the measurements come back, so the run also reports"
            (is (seq measured))
            (doseq [{:keys [what small big skipped]} measured]
              (println (format "  %-44s %-24s %s"
                               (subs what 0 (min 43 (count what)))
                               (if skipped (str "— " skipped) (str small))
                               (if skipped "" (str big))))))))
      (finally
        (doseq [h @handles] (pod/stop-pg! h))))))
