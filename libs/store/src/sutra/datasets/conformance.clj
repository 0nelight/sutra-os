(ns sutra.datasets.conformance
  (:require [clojure.test :refer [is testing]]
            [sutra.datasets.api :as datasets]
            [sutra.kit.time :as time]))

(set! *warn-on-reflection* true)

(def ^:private a {:branch "main" :dataset "d_conf_a"})
(def ^:private b {:branch "main" :dataset "d_conf_b"})

(declare refusal by-id)

(defn- ids [rows] (set (map (comp str :id) rows)))

(defn check-writes [make]
  (testing "writes and reads (transaction kinds)"
    (let [db (make)
          v1 (:version (datasets/write! db a
                                        {:kind :snapshot :pk "id"
                                         :rows [{:id "1" :n 1} {:id "2" :n 2}]}))
          v2 (:version (datasets/write! db a
                                        {:kind :update :pk "id"
                                         :rows [{:id "2" :n 20} {:id "3" :n 3}]}))
          v3 (:version (datasets/write! db a {:kind :delete :pk "id" :ids ["1"]}))]
      (is (< v1 v2 v3) "versions rise")
      (is (= #{"2" "3"} (ids (datasets/read db a))))
      (is (= 20 (:n (first (datasets/read db a {:ids ["2"]})))))
      (is (= 2 (datasets/count db a)))
      (testing "a read :at a version answers the dataset as it was"
        (is (= #{"1" "2"} (ids (datasets/read db a {:at v1}))))
        (is (= 2 (:n (first (datasets/read db a {:at v1 :ids ["2"]})))))
        (is (= 2 (datasets/count db a {:at v1}))))
      (testing ":columns projects"
        (is (= [{:id "2"}] (datasets/read db a {:ids ["2"] :columns ["id"]}))))
      (testing "columns names what a read gives, and nothing for a dataset never written"
        (is (= #{"id" "n"} (set (keys (datasets/columns db a)))))
        (is (= {} (datasets/columns db (assoc a :dataset "d_conf_never")))))
      (testing "the watermark covers every committed write"
        (is (<= v3 (datasets/version db a))))
      (testing "list names the datasets a branch reads"
        (is (some #{(:dataset a)} (datasets/list-datasets db "main")))
        (is (= [] (datasets/list-datasets db "main" {:prefix "nothing-"}))))
      (testing "truncate"
        (datasets/write! db a {:kind :delete :pk "id" :all? true})
        (is (zero? (datasets/count db a)))))))

(defn check-delta [make]
  (testing "delta, replaced?, history"
    (let [db (make)
          v1 (:version (datasets/write! db b
                                        {:kind :snapshot :pk "id"
                                         :rows [{:id "1" :n 1} {:id "2" :n 2}]}))
          v2 (:version (datasets/write! db b
                                        {:kind :update :pk "id"
                                         :rows [{:id "2" :n 9} {:id "3" :n 3}]
                                         :meta {:run "r1"}}))
          v3 (:version (datasets/write! db b {:kind :delete :pk "id" :ids ["1"]}))]
      (let [{:keys [added removed changed replaced?]} (datasets/delta db b v1 v3)]
        (is (= #{"3"} (set (map str added))))
        (is (= #{"1"} (set (map str removed))))
        (is (contains? (set (map str changed)) "2"))
        (is (false? replaced?)))
      (let [h (datasets/history db b)]
        (is (= [v3 v2 v1] (take 3 (mapv :version h))) "newest first")
        (is (= [v3] (mapv :version (datasets/history db b {:limit 1})))))
      (when (:history-kinds? (datasets/profile db))
        (is (= {:run "r1"} (:meta (second (datasets/history db b))))))
      (is (<= (datasets/horizon db b) v1))
      (let [v4 (:version (datasets/write! db b {:kind :snapshot :pk "id" :rows [{:id "2" :n 9}]}))]
        (is (true? (:replaced? (datasets/delta db b v3 v4)))
            "a :snapshot in the window replaces")))))

(defn check-restore [make]
  (testing "restore! makes the dataset what it was, as a new version"
    (let [db (make)
          v1 (:version (datasets/write! db a {:kind :snapshot :pk "id" :rows [{:id "1" :n 1}]}))
          _  (datasets/write! db a {:kind :snapshot :pk "id" :rows [{:id "9" :n 9}]})
          {v :version} (datasets/restore! db a v1)]
      (is (> v v1))
      (is (= #{"1"} (ids (datasets/read db a))))))
  (testing "restore! refuses a version below the horizon: its past may be partly swept"
    (let [db (make)
          r  {:branch "main" :dataset "d_conf_restore_horizon"}
          v1 (:version (datasets/write! db r {:kind :snapshot :pk "id" :rows [{:id "1" :n 1}]}))
          v2 (:version (datasets/write! db r {:kind :update :pk "id" :rows [{:id "1" :n 2}]}))]
      (datasets/retire-below! db r v2)
      (is (= :as-of-unsupported (:store/refusal (refusal #(datasets/restore! db r v1)))))
      (is (= {"1" 2} (by-id (datasets/read db r))) "and the dataset stays as it was")
      (is (some? (:version (datasets/restore! db r v2))) "the horizon itself still answers")
      (datasets/drop-dataset! db r))))

(defn check-branches [make]
  (testing "a fork reads its parent's datasets live under its own writes"
    (let [db (make)
          f  (assoc a :branch "feature")]
      (datasets/write! db a {:kind :snapshot :pk "id" :rows [{:id "1" :n 1}]})
      (datasets/fork! db "main" "feature")
      (is (= #{"1"} (ids (datasets/read db f))))
      (datasets/write! db f {:kind :update :pk "id" :rows [{:id "2" :n 2}]})
      (is (= #{"1" "2"} (ids (datasets/read db f))))
      (is (= #{"1"} (ids (datasets/read db a))) "the parent does not see the fork's write")
      (datasets/drop! db "feature")
      (is (= #{"1"} (ids (datasets/read db a)))))))

(defn check-inspect [make]
  (testing "inspect: missing declared datasets, orphans it laid out, never scratch"
    (let [db (make)
          s  {:branch "main" :dataset "d_scratch_1" :scratch? true}]
      (datasets/write! db a {:kind :snapshot :pk "id" :rows [{:id "1"}]})
      (datasets/write! db s {:kind :snapshot :pk "id" :rows [{:id "1"}]})
      (let [{:keys [missing
                    orphans]}
            (datasets/inspect db "main"
                              {(:dataset b) [{:attribute-name :x :attribute-type :string}]})]
        (is (= [(:dataset b)] missing))
        (is (= [(:dataset a)] (mapv :name orphans))
            "a is not declared; the scratch one is never an orphan")))))

(def ^:private q {:branch "main" :dataset "d_conf_q"})
(def ^:private r {:branch "main" :dataset "d_conf_r"})

(defn- id-of [s] (str "`" s "`"))

(defn check-query
  [make]
  (testing "query: the transform language over named inputs"
    (let [db  (make)
          num #(some-> % long)]
      (datasets/write! db q {:kind :snapshot :pk "id"
                             :rows [{:id "1" :grp "a" :n 1} {:id "2" :grp "a" :n 2}
                                    {:id "3" :grp "b" :n 5}]})
      (datasets/write! db r {:kind :snapshot :pk "id"
                             :rows [{:id "a" :label "Alpha"} {:id "b" :label "Beta"}]})
      (testing "an input is read by its alias, filtered with a param"
        (is (= #{"2" "3"}
               (ids (datasets/query db {:inputs {"src" q}
                                        :sql (str "SELECT " (id-of "id") " FROM " (id-of "src")
                                                  " WHERE " (id-of "n") " > ?")
                                        :params [1]})))))
      (testing "two inputs join by their aliases"
        (is (= #{["1" "Alpha"] ["3" "Beta"]}
               (set (map (juxt :id :label)
                         (datasets/query db
                                         {:inputs {"x" q "y" r}
                                          :sql (str "SELECT x." (id-of "id") ", y." (id-of "label")
                                                    " FROM " (id-of "x") " x JOIN " (id-of "y") " y"
                                                    " ON x." (id-of "grp") " = y." (id-of "id")
                                                    " WHERE x." (id-of "n") " <> 2")}))))))
      (testing "an aggregate groups"
        (is (= {"a" 3 "b" 5}
               (into {} (map (juxt :grp (comp num :total)))
                     (datasets/query db
                                     {:inputs {"src" q}
                                      :sql (str "SELECT " (id-of "grp") ", SUM(" (id-of "n") ") AS "
                                                (id-of "total") " FROM " (id-of "src")
                                                " GROUP BY " (id-of "grp"))})))))
      (testing "an input bounded by :ids holds only those rows (an incremental read)"
        (is (= #{"3"}
               (ids (datasets/query db
                                    {:inputs {"src" (assoc q :ids ["3"])}
                                     :sql (str "SELECT " (id-of "id") " FROM " (id-of "src"))})))))
      (testing "text compares exactly and orders by code point"
        (datasets/write! db r {:kind :update :pk "id"
                               :rows (map-indexed (fn [i t] {:id (str "o" i) :label t})
                                                  ["😀" "\uFFFD" "ä" "a" "Z" "B" "a " "A"])})
        (is (= ["A" "B" "Z" "a" "a " "ä" "\uFFFD" "😀"]
               (mapv :label
                     (datasets/query db {:inputs {"y" r}
                                         :sql (str "SELECT " (id-of "label") " FROM " (id-of "y")
                                                   " WHERE " (id-of "id") " LIKE 'o%' ORDER BY "
                                                   (id-of "label"))}))))
        (is (= ["o3"]
               (mapv :id (datasets/query db {:inputs {"y" r} :params ["a"]
                                             :sql (str "SELECT " (id-of "id") " FROM " (id-of "y")
                                                       " WHERE " (id-of "label") " = ?")})))
            "\"a\" is neither \"A\" nor \"a \""))
      (testing ":explain? plans without reading"
        (is (= [] (datasets/query db {:inputs {"src" q} :explain? true
                                      :sql (str "SELECT " (id-of "id") " FROM " (id-of "src"))}))))
      (testing "a missing input is refused by its alias"
        (is (= "gone"
               (try (datasets/query db {:inputs {"gone" {:branch "main" :dataset "d_conf_absent"}}
                                        :sql (str "SELECT 1 FROM " (id-of "gone"))})
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:datasets/missing-input (ex-data e))))))))))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn check-refusals
  [make]
  (testing "refusals the API owns"
    (let [db (make)]
      (datasets/write! db a {:kind :snapshot :pk "id" :rows [{:id "1"}]})
      (is (= :as-of-unsupported (:store/refusal (refusal #(datasets/read db a {:at -1}))))
          "a read below the horizon")
      (is (= :as-of-unsupported (:store/refusal (refusal #(datasets/count db (assoc a :at -1))))))
      (let [never (assoc a :dataset "d_conf_never")]
        (is (= :as-of-unsupported (:store/refusal (refusal #(datasets/read db never {:at 1}))))
            "a version of a dataset never written")
        (is (= [] (datasets/read db never)) "its head is the empty answer"))
      (let [r (refusal #(datasets/query db {:inputs {"t" (assoc a :at 1)} :sql "SELECT 1"}))]
        (if (:query (datasets/profile db))
          (is (= :as-of-unsupported (:store/refusal r)) "a query input at a version")
          (is (= :query (:datasets/unsupported r)) "a query, by a driver without an engine"))))))

(def ^:private v {:branch "main" :dataset "d_conf_v"})

(defn- instant-of
  [t]
  (if (instance? java.sql.Timestamp t) (time/stored-timestamp->instant t) t))

(defn check-values
  [make]
  (testing "values: a driver hands back what it was given, or the API refuses it"
    (let [db    (make)
          nano  (java.time.Instant/parse "2024-02-29T23:59:59.123456789Z")
          micro (java.time.Instant/parse "2024-02-29T23:59:59.123456Z")
          rows  [{:id "Müller" :t "Müller 😀 ≥ ½" :x -0.0    :b true  :n 1 :q 3/4 :at nano :e ""}
                 {:id "muller" :t "muller"        :x 1.0E300 :b false :n 2}
                 {:id "a"      :t "a"             :x 5.0E-324}
                 {:id "a "     :t "a "}]
          back  #(into {} (map (juxt :id identity)) (datasets/read db v))]
      (datasets/write! db v {:kind :snapshot :pk "id" :rows rows})
      (let [got (back)]
        (is (= #{"Müller" "muller" "a" "a "} (set (keys got)))
            "keys that differ only in case, accent or a trailing blank are four rows")
        (doseq [row rows, [k x] (dissoc row :q :at)]
          (is (= (pr-str x) (pr-str (get-in got [(:id row) k])))
              (str k " of " (pr-str (:id row)) " reads back unchanged")))
        (is (nil? (get-in got ["muller" :e])) "nil stays nil beside \"\"")
        (is (== 0.75M (get-in got ["Müller" :q])) "a ratio with a finite decimal is that decimal")
        (is (= micro (instant-of (get-in got ["Müller" :at]))) "an instant is its microsecond"))
      (testing "what not every driver keeps is refused before any driver sees it, by column, key and producer"
        (doseq [[col x] {:t "a\u0000b" :u (str "x" (char 0xD800)) :x ##NaN :y ##Inf :q 1/3
                         (keyword (apply str (repeat 32 "ü"))) "too long a name"}]
          (let [r (refusal #(datasets/write! db v
                                             {:kind :update :pk "id" :meta {:producer "conformance"}
                                              :rows [{:id "z" col x}]}))]
            (is (= :malformed (:store/refusal r)) (pr-str [col x]))
            (is (= [(name col) "z" "conformance" (:dataset v)]
                   ((juxt :column :pk :producer :dataset) r)))))
        (let [k (apply str (repeat 451 "ü"))]
          (is (= :malformed
                 (:store/refusal
                  (refusal #(datasets/write! db v
                                             {:kind :update :pk "id" :rows [{:id k :t "x"}]}))))
              "a key longer than every store can index")
          (is (= 4 (datasets/count db v)) "nothing was written")
          (datasets/write! db v {:kind :update :pk "id" :rows [{:id (subs k 1) :t "x"}]})
          (is (= [(subs k 1)] (map :id (datasets/read db v {:ids [(subs k 1)]})))
              "900 bytes are a key"))))))

(def ^:private w {:branch "main" :dataset "d_conf_w"})

(defn check-bulk
  [make]
  (testing "a write larger than any statement"
    (let [db (make)
          n  7000]
      (datasets/write! db w {:kind :snapshot :pk "id"
                             :rows (for [i (range n)]
                                     (into {:id (str i)}
                                           (for [c (range 9)] [(keyword (str "c" c)) (+ i c)])))})
      (is (= n (datasets/count db w)))
      (is (= 6999 (:c0 (first (datasets/read db w {:ids ["6999"]}))))))))

(def ^:private ret {:branch "main" :dataset "d_conf_ret"})

(defn- by-id [rows] (into {} (map (juxt (comp str :id) (comp long :n))) rows))

(defn check-retention [make]
  (testing "retention: a mark refuses below it, a sweep keeps the state at the horizon"
    (let [db (make)
          v1 (:version (datasets/write! db ret {:kind :snapshot :pk "id"
                                                :rows [{:id "1" :n 1} {:id "2" :n 2}]}))
          v2 (:version (datasets/write! db ret {:kind :update :pk "id" :rows [{:id "1" :n 10}]}))
          v3 (:version (datasets/write! db ret {:kind :update :pk "id" :rows [{:id "2" :n 20}]}))]
      (is (= #{v1 v2 v3} (into #{} (keep #(when (:kind %) (:version %)))
                               (datasets/history db ret {:typed? true})))
          "typed history holds every write, with its kind")
      (is (= {:horizon v2} (datasets/retire-below! db ret v2)))
      (is (= v2 (datasets/horizon db ret)))
      (is (= :as-of-unsupported (:store/refusal (refusal #(datasets/read db ret {:at v1})))))
      (is (= :as-of-unsupported (:store/refusal (refusal #(datasets/delta db ret v1 v3))))
          "a delta from below the horizon")
      (is (= {:horizon v2} (datasets/retire-below! db ret v1)) "a mark never lowers it")
      (is (zero? (:swept (datasets/sweep! db ret {:grace-ms 3600000}))) "not due yet")
      (let [{:keys [swept rows]} (datasets/sweep! db ret {:grace-ms 0})]
        (is (= v2 swept))
        (is (pos? rows) "the past below the horizon is gone"))
      (is (= {"1" 10 "2" 2} (by-id (datasets/read db ret {:at v2}))) "the state at the horizon")
      (is (= {"1" 10 "2" 20} (by-id (datasets/read db ret))) "the dataset as it is")
      (is (= #{"2"} (set (map str (:changed (datasets/delta db ret v2 v3))))))
      (is (zero? (:swept (datasets/sweep! db ret {:grace-ms 0}))) "a sweep is done once")))
  (testing "expire! drops the current rows last written at or below a version"
    (let [db (make)
          x  {:branch "main" :dataset "d_conf_expire"}
          v1 (:version (datasets/write! db x {:kind :snapshot :pk "id"
                                              :rows [{:id "1" :n 1} {:id "2" :n 2}]}))
          _  (datasets/write! db x {:kind :update :pk "id" :rows [{:id "1" :n 10}]})
          {:keys [version rows]} (datasets/expire! db x v1)]
      (is (= 1 rows) "row 1 was written again above the cut")
      (is (= {"1" 10} (by-id (datasets/read db x))))
      (is (= #{"2"} (set (map str (:removed (datasets/delta db x v1 version))))))
      (is (zero? (:rows (datasets/expire! db x v1))) "nothing left to expire")
      (datasets/drop-dataset! db x))))

(def ^:private sp {:branch "main" :dataset "d_conf_sparse"})

(defn- row-of [db addr id] (first (datasets/read db addr {:ids [id]})))

(defn check-sparse
  [make]
  (testing "sparse writes"
    (let [db (make)]
      (datasets/write! db sp {:kind :snapshot :pk "id"
                              :rows [{:id "1" :n 1 :m "a"} {:id "2" :n 2 :m "b"}]})
      (datasets/write! db sp {:kind :update :pk "id" :rows [{:id "1" :n 9}]})
      (is (= [9 "a"] ((juxt (comp long :n) :m) (row-of db sp "1")))
          "an :update keeps what the row does not name")
      (datasets/write! db sp {:kind :update :pk "id" :rows [{:id "1" :m nil}]})
      (is (nil? (:m (row-of db sp "1"))) "an :update clears what the row names as nil")
      (let [before (mapv :version (datasets/history db sp))]
        (datasets/write! db sp {:kind :snapshot :pk "id" :whole-row? true
                                :rows [{:id "1" :n 9} {:id "2" :n 2 :m "b"}]})
        (is (= before (mapv :version (datasets/history db sp)))
            "a nil and a column not named are the same to a write that changes nothing"))
      (datasets/write! db sp {:kind :snapshot :pk "id" :rows [{:id "2" :n 20}]})
      (is (= #{"2"} (ids (datasets/read db sp))) "a :snapshot retires the rows it omits")
      (is (= "b" (:m (row-of db sp "2"))) "a :snapshot keeps what a surviving row does not name")
      (datasets/write! db sp {:kind :snapshot :pk "id" :whole-row? true :rows [{:id "2" :n 21}]})
      (is (nil? (:m (row-of db sp "2"))) ":whole-row? clears what the row does not name"))))

(def ^:private same {:branch "main" :dataset "d_conf_same"})

(defn check-unchanged
  [make]
  (testing "a write that changes nothing"
    (let [db   (make)
          rows [{:id "1" :n 1} {:id "2" :n 2}]
          v1   (:version (datasets/write! db same {:kind :snapshot :pk "id" :rows rows}))
          hist #(mapv :version (datasets/history db same))]
      (datasets/write! db a {:kind :snapshot :pk "id" :rows [{:id "elsewhere"}]})
      (let [before (hist)]
        (testing "an identical :update"
          (datasets/write! db same {:kind :update :pk "id" :rows [{:id "1" :n 1}]})
          (is (= before (hist))))
        (testing "an identical :snapshot"
          (let [{:keys [version rows]} (datasets/write! db same {:kind :snapshot :pk "id"
                                                                 :rows rows})]
            (is (= before (hist)))
            (is (= 2 rows) "it still answers the rows it was given")
            (is (<= v1 version))
            (let [{:keys [added removed changed replaced?]}
                  (datasets/delta db same v1 (datasets/version db same))]
              (is (= [#{} #{} #{} false]
                     [(set added) (set removed) (set changed) replaced?])))))))))

(defn check-all [make]
  (check-refusals make)
  (check-writes make)
  (check-delta make)
  (check-restore make)
  (check-branches make)
  (check-inspect make)
  (check-sparse make)
  (check-unchanged make)
  (check-values make)
  (check-bulk make)
  (check-retention make))
