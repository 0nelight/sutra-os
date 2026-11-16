(ns ^:unit ontology.fk-hop-swap-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.pg.select :as select]
            [sutra.store.pg.sql :as sql]))

(def ^:private hop-clause #'select/hop-clause)
(def ^:private edge-source #'select/edge-source)

(def ^:private derived-edges
  (sql/frag "(SELECT f.from_id, p.to_id FROM fk f JOIN pk p ON f.k = p.k WHERE NOT (from_id = ? AND to_id = ?))"
               ["a" "b"]))

(defn- cx
  ([] (cx #{}))
  ([denied]
   {:denied denied
    :edges  (fn [L]
              (case L
                :buchung-import {:from derived-edges :derived? true}
                :tagged         {:from (sql/frag "`c_tagged`") :derived? false}))}))

(deftest an-fk-link-hops-through-its-relation-a-join-table-link-through-its-table
  (testing "the FK link reads the relation its edges came from"
    (let [{:keys [sql params]} (hop-clause (cx) :buchung-import :to [:ids ["x"]])]
      (is (re-find #"JOIN" sql))
      (is (re-find #"\) e WHERE" sql)
          "the subquery carries an alias — one serving arm requires it")
      (is (re-find #"SELECT from_id FROM" sql) "dir :to still walks from_id")
      (is (= ["a" "b" (sql/text-array ["x"])] (vec params))
          "relation parameters bind BEFORE the membership ids — a swap mis-binds every edge")))

  (testing "…while a join-table link still reads c_<link>: it owns real rows"
    (let [{:keys [sql params]} (hop-clause (cx) :tagged :to [:ids ["x"]])]
      (is (re-find #"FROM `c_tagged` e WHERE" sql))
      (is (= [(sql/text-array ["x"])] (vec params))))))

(deftest a-derived-hop-drops-the-edge-marking-filter
  (let [derived (:sql (hop-clause (cx #{:secret}) :buchung-import :to [:ids ["x"]]))
        plain   (:sql (hop-clause (cx #{:secret}) :tagged :to [:ids ["x"]]))]
    (is (not (re-find #"_markings" derived))
        "no column to filter, so no clause")
    (is (re-find #"_markings" plain)
        "…while the table-backed hop is filtered — markings live on its rows")))

(deftest edge-source-reports-which-of-the-two-it-chose
  (is (false? (:derived? (edge-source (cx) :tagged "e"))))
  (is (true? (:derived? (edge-source (cx) :buchung-import "e"))))
  (is (= ["a" "b"] (:params (:from (edge-source (cx) :buchung-import "e"))))))
