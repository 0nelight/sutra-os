(ns ^:unit platform.layout-retype-test
  (:require [sutra.store.pg.layout]
            [clojure.test :refer [deftest testing is]]))

(def ^:private clause-sql #'sutra.store.pg.layout/clause-sql)

(deftest a-column-becoming-json-keeps-every-value
  (testing "into json: JSON text stays JSON, other text becomes a JSON string, NULL stays NULL"
    (is (= (str "ALTER COLUMN \"gruppen\" TYPE json USING CASE WHEN \"gruppen\" IS NULL THEN NULL"
                " WHEN \"gruppen\"::text IS JSON THEN \"gruppen\"::text::json"
                " ELSE to_json(\"gruppen\") END")
           (clause-sql [:retype "gruppen" "json" nil]))))
  (testing "into any other type the bare cast as before"
    (is (= "ALTER COLUMN \"n\" TYPE bigint USING \"n\"::bigint"
           (clause-sql [:retype "n" "bigint" nil])))))
