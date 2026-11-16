(ns ^:unit platform.store-landing-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.import.reconcile :as reconcile]
            [sutra.datasets.landing :as landing]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.layout :as layout]))

(deftest a-replace-sweeps-inline-only-with-one-shape
  (is (landing/sweeps-inline? true [[["k"] [{"k" 1}]]]))
  (is (not (landing/sweeps-inline? true [[["k"] [{"k" 1}]] [["k" "v"] [{"k" 2 "v" 1}]]])))
  (is (not (landing/sweeps-inline? false [[["k"] [{"k" 1}]]]))))

(defn- steps [plan] (mapv first plan))

(deftest a-landing-is-a-plan-of-steps
  (testing "an append forks and merges, and sweeps nothing"
    (is (= [:fork :ensure-columns :merge]
           (steps (landing/plan {:fork? true :groups [[["k"] [{"k" 1}]]]})))))
  (testing "one shape replaces inline"
    (let [plan (landing/plan {:replace? true :groups [[["k"] [{"k" 1}]]]})]
      (is (= [:retract-inherited :ensure-columns :merge] (steps plan)))
      (is (= {:full-refresh? true} (nth (last plan) 3)))))
  (testing "several shapes replace by a sweep after all of them"
    (let [plan (landing/plan
                {:replace? true :groups [[["k"] [{"k" 1}]] [["k" "v"] [{"k" 2 "v" 1}]]]})]
      (is (= [:retract-inherited :ensure-columns :merge :merge :retain] (steps plan)))
      (is (= {:full-refresh? false} (nth (nth plan 2) 3))))))

(deftest only-the-same-rows-in-the-same-mode-are-already-landed
  (let [rows [{:id "1"}]]
    (is (reconcile/already-landed? {:mode :append :rows rows} :append rows))
    (is (not (reconcile/already-landed? {:mode :append :rows rows} :snapshot rows)))
    (is (not (reconcile/already-landed? {:mode :append :rows rows} :append [{:id "1"}]))
        "rows folded since the landing are another value")
    (is (not (reconcile/already-landed? nil :append rows)))))

(deftest a-known-removal-holds-only-while-the-dataset-is-unwritten
  (with-redefs [dsets/content-signature (constantly "5")]
    (let [known {:signature "5" :removed ["gone"]}]
      (is (= ["gone"] (reconcile/still-known-removed nil "ds" known)))
      (is (nil? (reconcile/still-known-removed nil "ds" (assoc known :signature "4"))))
      (is (nil? (reconcile/still-known-removed nil "ds" (assoc known :signature nil))))
      (is (nil? (reconcile/still-known-removed nil "ds" nil))))))

(deftest a-remembering-catalog-asks-once-per-relation
  (let [asked (atom [])
        cat   (layout/remembering-cat
               (reify layout/Catalog
                 (table? [_ s t] (swap! asked conj [:table s t]) true)
                 (columns [_ s t] (swap! asked conj [:columns s t]) [])
                 (sealed [_ s t] (swap! asked conj [:sealed s t]) [])))]
    (dotimes [_ 3]
      (is (true? (layout/table? cat "s" "t")))
      (layout/columns cat "s" "t")
      (layout/sealed cat "s" "t"))
    (is (true? (layout/table? cat "s" "u")))
    (is (= [[:table "s" "t"] [:columns "s" "t"] [:sealed "s" "t"] [:table "s" "u"]]
           @asked))))

(deftest the-write-follows-the-counts
  (let [merge-target {:schema "s" :table "t" :tgt "\"s\".\"t\"" :pk "\"k\""
                      :all-cols ["k" "sys_from" "sys_to" "v"] :delta "d" :gone "g"}
        ops-at       (fn [changed went]
                       (mapv first (pgds/merge-write-ops merge-target
                                                         {:changed changed :went went :v 7
                                                          :lits {"sys_from" "7"}})))]
    (is (= [] (ops-at 0 0)))
    (is (= [:displace :exec] (ops-at 2 0)))
    (is (= [:displace] (ops-at 0 1)))
    (is (= [:displace :exec :displace] (ops-at 2 1)))
    (testing "the version columns take their literals"
      (let [[_ [_ insert]] (pgds/merge-write-ops merge-target
                                                 {:changed 1 :went 0 :v 7
                                                  :lits {"sys_from" "7" "sys_to" "NULL"}})]
        (is (= (str "INSERT INTO \"s\".\"t\" (\"k\",\"sys_from\",\"sys_to\",\"v\")"
                    " SELECT \"k\",7,NULL,\"v\" FROM \"d\"")
               insert))))))

(deftest copy-is-only-taken-where-it-reads-values-as-an-insert-does
  (let [types  {"s" "character varying" "i" "bigint" "n" "numeric" "b" "boolean"}
        ready? (fn [col v] (pgds/copy-ready? types [col] [{col v}]))]
    (is (ready? "s" "x"))
    (is (ready? "i" "12") "a string goes through the column's input function either way")
    (is (ready? "i" nil))
    (is (ready? "i" 12))
    (is (ready? "n" 1.5))
    (is (ready? "b" true))
    (is (ready? "s" :kw) "a keyword is bound as its name")
    (is (not (ready? "s" 12)) "an INSERT casts a typed number to its own text")
    (is (not (ready? "i" 1.5)) "and rounded by its assignment cast")
    (is (not (ready? "s" true)))
    (is (not (ready? "s" (java.sql.Timestamp. 0))) "a time is written with the session's zone")
    (testing "a date or a local time only into its own type, as text that reads back alike"
      (let [types  {"d" "date" "t" "timestamp without time zone" "z" "timestamp with time zone"
                    "s" "character varying"}
            ready? (fn [col v] (pgds/copy-ready? types [col] [{col v}]))
            day    (java.time.LocalDate/of 2026 9 24)
            at     (java.time.LocalDateTime/of 2026 9 24 10 15)]
        (is (ready? "d" day))
        (is (ready? "t" at))
        (is (not (ready? "s" day)))
        (is (not (ready? "t" day)))
        (is (ready? "d" at) "a date column keeps its day, by COPY as by the INSERT's cast")
        (is (not (ready? "z" at)) "a zone column reads it with the session's zone")
        (is (not (ready? "d" (java.time.LocalDate/of 10000 1 1))) "the text has no plain year")
        (is (not (ready? "t" (.withNano at 1))) "finer than the column keeps")))))

(deftest a-copy-refusal-names-the-first-value-it-cannot-read-alike
  (let [types {"s" "character varying(20)" "i" "bigint"}]
    (is (nil? (pgds/copy-refusal types ["s" "i"] [{"s" "x" "i" 1}])))
    (is (= {:column "s" :type "character varying(20)" :class "java.lang.Long"}
           (pgds/copy-refusal types ["s" "i"] [{"s" "x" "i" 1} {"s" 12 "i" 1.5}])))))

(deftest copy-text-escapes-what-the-format-reads
  (is (= "a\\tb\t\\N\n" (pgds/copy-text ["x" "y"] [{"x" "a\tb"}])))
  (is (= "a\\nb\\\\c\\rd\n" (pgds/copy-text ["x"] [{"x" "a\nb\\c\rd"}])))
  (is (= "\t\n" (pgds/copy-text ["x" "y"] [{"x" "" "y" ""}])) "an empty string is no NULL"))

(deftest a-merge-is-planned-as-sql-values
  (let [decl {:tgt "\"ds\".\"t\"" :src "\"ds\".\"t\"" :pk "\"id\"" :pk-col "id"
              :cols ["id" "v"] :all-cols ["id" "v" "w"] :uniq "u1"}
        plan (pgds/plan-merge decl)]
    (testing "a stage like the target, a delta of new or differing rows, both dropped"
      (is (= "_stage_u1" (:stage plan)))
      (is (= ["_delta_u1" "_stage_u1"] (:drop plan)))
      (let [[[_ create]] (:create plan), [[_ derive]] (:derive plan)]
        (is (re-find #"LIMIT 0" create))
        (is (re-find #"t\.\"v\" IS DISTINCT FROM s\.\"v\"" derive))
        (is (re-find #"t\.\"w\" AS \"w\"" derive) "a column not carried keeps its value")))
    (testing "only a full refresh counts what went"
      (is (= [:changed] (keys (:counts plan))))
      (is (= #{:changed :went}
             (set (keys (:counts (pgds/plan-merge (assoc decl :full-refresh? true))))))))
    (testing "the write follows the counts"
      (is (= [] (pgds/merge-write-ops (merge decl {:schema "ds" :table "t"} plan)
                                      {:changed 0 :went 0 :v 7 :lits {}})))
      (is (= [:displace :exec :displace]
             (mapv first (pgds/merge-write-ops (merge decl {:schema "ds" :table "t"} plan)
                                               {:changed 2 :went 1 :v 7 :lits {}})))))))
