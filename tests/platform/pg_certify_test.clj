(ns ^:integration platform.pg-certify-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.store.certify :as certify]
            [sutra.store.core :as core]
            [sutra.store.core.engine :as core-engine]
            [sutra.store.pg :as store-pg]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(defonce ^:private handles (atom []))

(def ^:private record-tables
  ["CREATE TABLE certify_notes (k text COLLATE \"C\" PRIMARY KEY, v bigint)"
   "CREATE TABLE certify_counters (n bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY, v bigint)"])

(def ^:private records
  {:certify/notes    {:table "certify_notes" :key-cols [:k] :text #{:k}}
   :certify/counters {:table "certify_counters" :key-cols [:n] :auto :n}})

(defn- fresh-engine []
  (let [{:keys [url user password] :as h} (pod/start-empty-pg!)
        ds (jdbc/get-datasource {:jdbcUrl url :user user :password password})]
    (swap! handles conj h)
    (run! #(jdbc/execute! ds [%]) record-tables)
    (store-pg/engine {:datasource ds :records records})))

(defn- db [engine collections]
  (doto (core/db engine {:collections collections}) core/adopt!))

(defn certify-pg []
  (try (certify/certify {:make fresh-engine :db db :steps 80 :seed 11
                         :records #{:certify/notes} :auto-records :certify/counters})
       (finally (run! pod/stop-pg! @handles) (reset! handles []))))

(deftest ^{:stall-s 240} pg-is-certified
  (let [r (certify-pg)]
    (is (:passed? r)
        (pr-str (into {} (remove #(= :proven (:status (val %)))) (:results r))))
    (is (not= :none (get-in r [:results :certify/records :records]))
        "pg's records are certified, not skipped")
    (is (= (:pg core-engine/certificates) (:claims r))
        "certificates.edn :pg is what certifying pg proves")))
