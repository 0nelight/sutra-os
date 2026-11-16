(ns ^:integration platform.catalog-for-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [support.pod :as pod]
            [support.system :as tsys]
            [sutra.store.pg.ident :as ident]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pgns]))

(use-fixtures :once pod/with-stores-only)

(defn- q [s] (str "\"" (str/replace (str s) "\"" "\"\"") "\""))
(defn- at [s t] (str (q s) "." (q t)))
(defn- lit [s] (str "'" (str/replace (str s) "'" "''") "'"))

(defn- table-ddl
  [s t {:keys [key? owned]}]
  (cond-> [(str "CREATE TABLE IF NOT EXISTS " (at s t) " (id varchar" (when key? " NOT NULL")
                ", n integer, txt text COLLATE \"C\", sys_from bigint, sys_to bigint)")]
    key?  (conj (str "CREATE UNIQUE INDEX IF NOT EXISTS " (q (ident/fit (str t "_key_id")))
                     " ON " (at s t) " (id)"))
    owned (conj (str "COMMENT ON TABLE " (at s t) " IS " (lit owned)))))

(defn- view-ddl [s t] (str "CREATE OR REPLACE VIEW " (at s t) " AS SELECT 1 AS x"))

(def ^:private own "bo_b1")

(defn- relations
  [t]
  (let [tomb (layout/tomb-of t)]
    [t (layout/history-of t) tomb (layout/history-of tomb)]))

(defn- state-ddl
  [t {:keys [main? own? history? tomb? tomb-history? view? other?] :as st}]
  (let [tomb (layout/tomb-of t)]
    (concat
     (when main?    (table-ddl "main" t st))
     (when own?     (table-ddl own t st))
     (when history?
       (concat (when main? (table-ddl "main" (layout/history-of t) {}))
               (when own?  (table-ddl own (layout/history-of t) {}))))
     (when tomb?         (table-ddl own tomb {:key? true}))
     (when tomb-history? (table-ddl own (layout/history-of tomb) {}))
     (when view?         [(view-ddl "br_b1" t)])
     (when other?        (conj (vec (table-ddl "bo_other" t st)) (view-ddl "br_other" t))))))

(def ^:private flags [:main? :own? :history? :tomb? :tomb-history? :view? :other? :key?])

(def ^:private states
  (vec (for [bits (range (bit-shift-left 1 (count flags)))]
         (assoc (into {} (map-indexed (fn [i f] [f (bit-test bits i)])) flags)
                :t (format "cf_s%03d" bits)
                :owned (case (mod bits 3) 0 nil 1 "sutra:store" 2 "sutra:datasets")))))

(def ^:private near-limit (apply str "cf_lang_" (repeat 52 "x")))
(def ^:private over-limit (ident/fit (apply str "cf_ueber_" (repeat 90 "y"))))

(def ^:private odd-names
  ["Mixed_Case" "FALL" "fall" "a_b" "axb" "a%b" "quo\"te" "o'brien" "größe" "mit leer"
   "bind-strich" "x__history" "x__tomb" "x__history__history" "x__tomb__history"
   "k" "k_key_x" near-limit over-limit])

(def ^:private noise
  ["cf_s001x" "cf_s001__history2" "cf_s001__tomb_x" "xcf_s001" "aXb" "x__historyx" "_detached"])

(def ^:private absent "gibt_es_nicht")

(defn- odd-ddl
  [t]
  (let [st {:key? true :owned "sutra:store"}
        tomb (layout/tomb-of t)]
    (concat (table-ddl "main" t st)
            (table-ddl "main" (layout/history-of t) {})
            (table-ddl own t st)
            (table-ddl own (layout/history-of t) {})
            (table-ddl own tomb {:key? true})
            (table-ddl own (layout/history-of tomb) {})
            [(view-ddl "br_b1" t)])))

(defn- special-ddl
  []
  [(str "CREATE TABLE " (at "main" "cf_wide") " (id varchar NOT NULL, gone int, n int, "
        "sys_from bigint, sys_to bigint)")
   (str "ALTER TABLE " (at "main" "cf_wide") " DROP COLUMN gone")
   (str "CREATE UNIQUE INDEX " (q "cf_wide_key_id_n") " ON " (at "main" "cf_wide") " (id, n)")
   (str "CREATE TABLE " (at "main" "cf_part") " (id varchar NOT NULL, n int) "
        "PARTITION BY RANGE (n)")
   (str "CREATE TABLE " (at "main" "cf_part_p0") " PARTITION OF " (at "main" "cf_part")
        " FOR VALUES FROM (0) TO (10)")
   (str "CREATE TABLE " (at own "_detached") " (table_name text NOT NULL, sys_from bigint, "
        "sys_to bigint)")
   (view-ddl "br_b1" "cf_nur_view")])

(def ^:private ddl
  (delay
    (concat
     (for [s ["bo_b1" "br_b1" "bo_b2" "br_b2" "bo_other" "br_other"]]
       (str "CREATE SCHEMA IF NOT EXISTS " (q s)))
     (mapcat #(state-ddl (:t %) %) states)
     (mapcat odd-ddl odd-names)
     (mapcat #(table-ddl "main" % {}) noise)
     (special-ddl))))

(def ^:private world
  (delay (jdbc/with-transaction [tx (tsys/ds)]
           (doseq [s @ddl] (jdbc/execute! tx [s])))))

(defn- ensure-world! [] @world)

(def ^:private names
  (vec (concat (map :t states) odd-names noise
               ["cf_wide" "cf_part" "cf_part_p0" "cf_nur_view" absent])))

(defn- whole
  [ns all vs t]
  (let [ss (set (layout/schemas-for vs ns [t]))
        by-schema #(into {} (filter (comp ss key)) %)]
    (-> all
        (update :tables by-schema)
        (update :owned by-schema)
        (update :columns #(into {} (filter (comp ss first key)) %))
        (update :key-idx #(into {} (filter (comp ss first key)) %)))))

(defn- narrowed
  [K wanted]
  (let [drop-empty #(into {} (filter (comp seq val)) %)]
    (-> K
        (update :views #(select-keys % wanted))
        (update :tables #(drop-empty (update-vals % (fn [ts] (set (filter wanted ts))))))
        (update :owned #(drop-empty (update-vals % (fn [m] (select-keys m wanted)))))
        (update :columns #(into {} (filter (comp wanted second key)) %))
        (update :key-idx #(into {} (filter (comp wanted second key)) %)))))

(defn- wanted-of [ts] (into #{"_detached"} (mapcat relations) ts))

(def ^:private columns-kept
  [{:name "id" :type "character varying"} {:name "n" :type "integer"}])

(defn- plans
  [ns t]
  (let [s (pgns/own-schema ns)]
    (for [[nm f] {:table-same (fn [K] (layout/table-plan K ns t columns-kept "sutra:store"))
                  :table-more (fn [K] (layout/table-plan
                                       K ns t
                                       (conj columns-kept {:name "neu" :type "text"})
                                       "sutra:datasets"))
                  :table-type (fn [K] (layout/table-plan
                                       K ns t
                                       [{:name "id" :type "character varying"}
                                        {:name "n" :type "bigint"}]
                                       "sutra:store"))
                  :view       (fn [K] (layout/view-plan K ns t))
                  :own        (fn [K] (layout/own-plan K ns t))
                  :drop       (fn [K] (layout/drop-plan K ns t))
                  :detach     (fn [K] (layout/detach-plan K ns 7 t false))
                  :materialize (fn [K] (layout/detach-plan K ns 7 t true))
                  :create     (fn [K] (layout/create-plan K s t columns-kept))
                  :version    (fn [K] (layout/version-plan K s t))
                  :add        (fn [K] (layout/alter-plan K s t [[:add "neu" "text"]]))
                  :rename     (fn [K] (layout/alter-plan K s t [[:rename "n" "m"]]))
                  :retype     (fn [K] (layout/alter-plan K s t [[:retype "txt" "varchar"]]))
                  :drop-col   (fn [K] (layout/alter-plan K s t [[:drop "n"]]))
                  :main-holds (fn [K] (layout/satisfied-by-main? K ns t columns-kept))
                  :columns    (fn [K] (layout/catalog-columns K s t))}]
      [nm #(try (f %) (catch Exception e [:threw (ex-message e)]))])))

(defn- catalogs
  [c]
  (let [vs (#'layout/views c)]
    (into {}
          (for [ns ["main" "b1" "b2" "ohne"]]
            [ns {:views vs
                 :all (layout/catalog c (layout/schemas-for vs ns names) vs)}]))))

(deftest it-holds-what-the-whole-catalog-holds-of-those-relations
  (ensure-world!)
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (let [ks (catalogs c)
          bad (vec (for [[ns {:keys [all views]}] ks
                    t names
                    :let [got  (layout/catalog-for c ns [t])
                          want (narrowed (whole ns all views t) (wanted-of [t]))]
                    k [:tables :owned :columns :key-idx :views]
                    :when (not= (k got) (k want))]
                [ns t k]))]
      (is (empty? bad) (pr-str (take 10 bad))))))

(deftest several-tables-and-none
  (ensure-world!)
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (let [ks (catalogs c)]
      (testing "several at once hold each of them"
        (let [ts ["cf_s255" "größe" over-limit absent]
              {:keys [all views]} (ks "b1")
              ss (set (layout/schemas-for views "b1" ts))
              want (narrowed (-> all
                                 (update :tables #(into {} (filter (comp ss key)) %))
                                 (update :columns #(into {} (filter (comp ss first key)) %)))
                             (wanted-of ts))
              got  (layout/catalog-for c "b1" ts)]
          (is (= (:tables want) (:tables got)))
          (is (= (:columns want) (:columns got)))))
      (testing "none reads only what a branch always has"
        (is (= {own #{"_detached"} "main" #{"_detached"}}
               (:tables (layout/catalog-for c "b1" []))))))))

(deftest every-plan-comes-out-the-same-from-either-catalog
  (ensure-world!)
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (let [ks  (catalogs c)
          bad (vec (for [[ns {:keys [all views]}] ks
                    t names
                    :let [full (whole ns all views t)
                          part (layout/catalog-for c ns [t])]
                    [nm plan] (plans ns t)
                    :let [a (plan full) b (plan part)]
                    :when (not= a b)]
                {:ns ns :t t :plan nm :whole a :narrow b}))]
      (is (empty? bad) (pr-str (take 3 bad))))))

(deftest every-name-it-reads-is-needed
  (ensure-world!)
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (let [{:keys [all views]} ((catalogs c) "b1")
          leave-out {:the-table    (fn [t] t)
                     :history      layout/history-of
                     :tomb         layout/tomb-of
                     :tomb-history (comp layout/history-of layout/tomb-of)
                     :detached     (constantly "_detached")}]
      (doseq [[what name-of] leave-out]
        (testing (str "a plan changes without " what)
          (is (some (fn [{:keys [t]}]
                      (let [full (whole "b1" all views t)
                            less (narrowed full (disj (wanted-of [t]) (name-of t)))]
                        (some (fn [[_ plan]] (not= (plan full) (plan less))) (plans "b1" t))))
                    states)))))))
