(ns ^:unit platform.composite-key-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.sources.datasources]
            [sutra.sources.policy :as policy]
            [sutra.sources.dialect :as dialect]
            [sutra.parser.spec :as spec]))

(def ^:private t0 (java.time.LocalDateTime/of 2026 10 7 9 30 5))

(def ^:private pk-clause @#'dialect/pk-clause)

(def ^:private av-kre
  {"AV_KRE"
   {:pk "KRE_SCHLUESSEL" :identity "KRE_SCHLUESSEL"
    :pk-columns ["KRE_ID" "KRE_ART" "KRE_REIHENFOLGE"]
    :writable-columns ["KRE_ID" "KRE_ART" "KRE_BETRAG_E"]
    :insertable-columns ["KRE_ID" "KRE_ART" "KRE_REIHENFOLGE" "KRE_BETRAG_E"]
    :sequence {:column "KRE_REIHENFOLGE" :scope "KRE_ID"}
    :column-map {"schluessel" "KRE_SCHLUESSEL" "betrag" "KRE_BETRAG_E"}
    :log :none}})

(def ^:private src
  {:type :mssql :access :read-write :writeback av-kre})

(deftest spec-accepts-a-composite-key
  (is (spec/writeback-valid? av-kre))
  (testing "one column is no composite key"
    (is (not (spec/writeback-valid? (assoc-in av-kre ["AV_KRE" :pk-columns] ["KRE_ID"])))))
  (testing "the virtual :pk may not be one of its own columns"
    (is (not (spec/writeback-valid? (assoc-in av-kre ["AV_KRE" :pk] "KRE_ID")))))
  (testing ":identity must read back the virtual key, not a single column"
    (is (not (spec/writeback-valid? (assoc-in av-kre ["AV_KRE" :identity] "KRE_REIHENFOLGE"))))))

(deftest key-splits-back-into-its-columns
  (is (= ["4430" "N" "5"] (policy/pk-parts ["KRE_ID" "KRE_ART" "KRE_REIHENFOLGE"] "4430_|_N_|_5")))
  (is (thrown? clojure.lang.ExceptionInfo
               (policy/pk-parts ["KRE_ID" "KRE_ART" "KRE_REIHENFOLGE"] "4430_|_5")))
  (is (= ["[KRE_ID] = ? AND [KRE_ART] = ? AND [KRE_REIHENFOLGE] = ?" ["4430" "N" "5"]]
         (pk-clause :mssql "KRE_SCHLUESSEL" ["KRE_ID" "KRE_ART" "KRE_REIHENFOLGE"] "4430_|_N_|_5")))
  (is (= ["[ADR_ID] = ?" [7]] (pk-clause :mssql "ADR_ID" nil 7))))

(deftest insert-reads-back-the-joined-key
  (let [plan (policy/write-plan t0 src {:table "AV_KRE"
                                           :insert {"KRE_ID" 4430 "KRE_ART" "N" "KRE_BETRAG_E" 1.0}
                                           :returning "KRE_SCHLUESSEL"})]
    (is (= ["KRE_ID" "KRE_ART" "KRE_REIHENFOLGE"] (:returning-columns plan)))
    (let [sql (dialect/insert-sql :mssql plan "[A]" "?" :output)]
      (is
       (re-find
        #"OUTPUT CONCAT\(RTRIM\(CONVERT\(nvarchar\(4000\), INSERTED\.\[KRE_ID\]\)\), N'_\|_', "
        sql))
      (is (re-find #"INTO @sutra_out \(id\)" sql)))
    (is
     (re-find
      #"RETURNING CONCAT\(\"KRE_ID\", '_\|_', \"KRE_ART\", '_\|_', \"KRE_REIHENFOLGE\"\) AS id"
      (dialect/insert-sql :postgres plan "a" "?" :returning))))
  (testing "a single-column identity is read back as before"
    (is (nil? (:returning-columns
               (policy/write-plan t0
                (assoc src :writeback {"T" {:pk "ID" :identity "ID" :writable-columns ["A"]
                                            :insertable-columns ["A"] :log :none}})
                {:table "T" :insert {"A" 1} :returning "ID"}))))))

(deftest update-carries-the-key-columns
  (let [plan (policy/write-plan t0 src {:table "AV_KRE" :pk-value "4430_|_N_|_5"
                                           :set {"KRE_BETRAG_E" 2.0}
                                           :expect {"KRE_BETRAG_E" 1.0}})]
    (is (= ["KRE_ID" "KRE_ART" "KRE_REIHENFOLGE"] (:pk-columns plan)))))
