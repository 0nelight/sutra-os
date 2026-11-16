(ns ^:unit platform.sql-trap-gate-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.authoring.install.findings.sql :as sql-findings]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.types.sql-traps :as traps]))

(defn- schema
  [query]
  {:projects {:werft {}}
   :datasets {:zeit {:project :werft :kind :standalone
                     :columns [{:attribute-name :jahr :attribute-type :long}]}}
   :transforms {:zeit-schluessel {:project :werft
                                  :language :pg-sql
                                  :query query
                                  :inputs {:z {:kind :dataset :dataset :zeit}}
                                  :output-columns [[:k :string]]}}})

(defn- gate
  ([desired] (gate desired {}))
  ([desired current]
   (->> (install-plan/plan-problems desired current (install/plan-schema desired current {}) {})
        (filter #(= :sql-char-without-length (:rule %))))))

(deftest cast-as-char-ohne-laenge-blockt
  (let [[f] (gate (schema "SELECT CAST(z.jahr AS CHAR) || '-2026' AS k FROM z"))]
    (is (= :error (:severity f)))
    (is (= :zeit-schluessel (:entity f)))
    (is (re-find #"AS VARCHAR" (:message f))))
  (is (seq (gate (schema "SELECT z.jahr::char AS k FROM z")))))

(deftest mit-laenge-oder-varchar-geht-durch
  (is (empty? (gate (schema "SELECT CAST(z.jahr AS VARCHAR) AS k FROM z"))))
  (is
   (empty?
    (gate (schema "SELECT CAST(z.jahr AS CHAR(4)) AS k, z.jahr::character varying FROM z")))))

(deftest kommentare-und-literale-schlagen-nicht-an
  (is (empty? (gate (schema (str "SELECT 'CAST(x AS CHAR)' AS k -- CAST(z.jahr AS CHAR)\n"
                                 "/* z.jahr::char */ FROM z"))))))

(deftest external-source-wird-nicht-geprueft
  (is
   (empty?
    (gate
     {:projects {:werft {}}
      :datasets {:zv {:project :werft
                      :external-source
                      {:datasource :mro
                       :query "SELECT ISNULL(CAST(a AS CHAR), '') AS a, CAST(b AS varchar(12)) AS b FROM ZV"}}}}))))

(deftest nur-was-der-push-anfasst
  (let [live {:datasets {"ri.ds.zeit" {:id "ri.ds.zeit" :dataset-name :zeit :project "ri.pr.c"
                                       :kind :standalone
                                       :columns [{:attribute-name :jahr :attribute-type :long}]}}
              :projects {"ri.pr.c" {:id "ri.pr.c" :project-name :werft}}
              :transforms {"ri.tx.alt" {:id "ri.tx.alt" :transform-name :alt :language :pg-sql
                                        :query "SELECT CAST(z.jahr AS CHAR) AS k FROM z"
                                        :inputs {:z {:kind :dataset :dataset "ri.ds.zeit"}}}}}]
    (is (empty? (gate (schema "SELECT CAST(z.jahr AS TEXT) AS k FROM z") live))
        "ein Altbestand blockiert einen Push nicht, der ihn nicht beruehrt")))

(deftest der-plan-ausgang-erreicht-den-compiler-mit-rids
  (let [seen (atom nil)
        live {:datasets {"ri.ds.zeit" {:id "ri.ds.zeit" :dataset-name :zeit :project "ri.pr.c"
                                       :kind :standalone
                                       :columns [{:attribute-name :jahr :attribute-type :long}]}}
              :projects {"ri.pr.c" {:id "ri.pr.c" :project-name :werft}}}
        desired (schema "SELECT YEAR(z.jahr) AS k FROM z")
        plan (install/plan-schema desired live {})
        code {:sql-explain (fn [_ _ spec]
                             (reset! seen spec)
                             {:state :refused
                              :detail "function year(bigint) does not exist"})}
        [f]  (sql-findings/sql-compile-findings
              (sql-findings/staged-sql-verdicts {} code
                                                (diff/diff desired live plan {}
                                                           (live/world-index live))))]
    (is (= "ri.ds.zeit" (get-in @seen [:inputs :z :dataset])))
    (is (= :sql-refused (:rule f)))
    (is (= :error (:severity f)))
    (is (re-find #"year\(bigint\)" (:message f)))))

(deftest traps-einzeln
  (is (= ["CAST(CAST(a AS INT) AS CHAR)"]
         (traps/char-casts "SELECT CAST(CAST(a AS INT) AS CHAR) FROM t")))
  (is (empty? (traps/char-casts "SELECT podcast(b as char), broadcast(a) AS char_x FROM t"))))
