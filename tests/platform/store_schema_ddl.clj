(ns ^:integration platform.store-schema-ddl
  (:require [support.system :as tsys]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [support.pod :as pod]
            [sutra.store.pg.tables :as pg-tables]
            [sutra.store.access.scope :as ctx]
            [sutra.types.naming :as naming]
            [sutra.catalog.signature :as signature]
            [next.jdbc :as jdbc]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [sutra.store.pg.dataset-store :as dstore]
            [sutra.types.coltype :as ctype]
            [sutra.types.rid :as rid]))

(use-fixtures :once pod/with-dataset-store)
(use-fixtures :each (fn [f] (pod/reset-dataset-store!) (f)))

(def ^:private br "scenario-ddl")
(def ^:private br-ns (pgns/read-schema br))

(defn- col-types-of
  [s tbl]
  (dstore/with-store* (tsys/ds)
    (fn [store]
      (into {}
            (map (fn [[col t]] [(str col) (some-> (if (map? t) t (ctype/parse t)) ctype/label)]))
            (dstore/column-types store s tbl)))))

(defn- col-types
  ([d] (col-types "main" d))
  ([s d] (col-types-of s (naming/dataset-table d))))

(defn- pks [s d]
  (with-open [c (pgds/open (tsys/ds))]
    (mapv (comp str first vals)
          (jdbc/execute! c [(str "SELECT " (sql/dq naming/pk-col) " FROM "
                                 (sql/dq s) "." (sql/dq (naming/dataset-table d))
                                 " ORDER BY 1")]))))

(defn- ensure-on!
  [b d columns]
  (pgds/with-conn [c (tsys/ds)] (pg-tables/ensure-table! c b (naming/dataset-table d) columns)))

(defn- ensure! [d columns] (ensure-on! "main" d columns))

(defn- cols [& pairs]
  (mapv (fn [[n t]] {:attribute-name n :attribute-type t}) (partition 2 pairs)))

(deftest declared-columns-land-with-their-declared-types
  (let [d :ddl-types]
    (ensure! d (cols :qty :long, :price :decimal, :when :date,
                                   :ok :boolean, :note :string))
    (is (= {"internal_uuid" "varchar"
            "qty"   "bigint"
            "price" "decimal(38,10)"
            "when"  "date"
            "ok"    "boolean"
            "note"  "varchar"}
           (col-types d)))))

(deftest an-empty-producer-still-gets-its-types
  (let [declared :ddl-declared
        inferred :ddl-inferred]
    (ensure! declared (cols :qty :long))
    (dsets/write-rows! (ctx/system-store) inferred :snapshot [{:id "a" :values {:qty nil}}])
    (is (= "bigint"  (get (col-types declared) "qty")))
    (is (= "varchar" (get (col-types inferred) "qty"))
        "the inference this seam replaces, asserted so the difference is not theoretical")))

(deftest widening-is-additive-and-idempotent
  (let [d :ddl-widen]
    (ensure! d (cols :a :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:a "x"}}])
    (ensure! d (cols :a :string, :b :long))
    (is (= #{"internal_uuid" "a" "b"} (set (keys (col-types d)))))
    (is (= ["r1"] (pks "main" d)) "the existing rows are untouched")
    (testing "re-running is a no-op"
      (ensure! d (cols :a :string, :b :long))
      (is (= #{"internal_uuid" "a" "b"} (set (keys (col-types d)))))
      (is (= ["r1"] (pks "main" d))))
    (testing "a column no longer declared is NOT dropped — a prior output was written against it"
      (ensure! d (cols :a :string))
      (is (contains? (set (keys (col-types d))) "b")))))

(defn- values-of [s d col]
  (with-open [c (pgds/open (tsys/ds))]
    (mapv (comp first vals)
          (jdbc/execute! c [(str "SELECT " (sql/dq (name col)) " FROM "
                                 (sql/dq s) "." (sql/dq (naming/dataset-table d))
                                 " ORDER BY " (sql/dq naming/pk-col))]))))

(deftest a-declared-type-change-retypes-the-column-and-keeps-the-rows
  (let [d :ddl-retype]
    (ensure! d (cols :betrag :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:betrag "12.50"}}
                                     {:id "r2" :values {:betrag "7"}}])
    (ensure! d (cols :betrag [:decimal 38 10]))
    (is (= "decimal(38,10)" (get (col-types d) "betrag")))
    (is (= [12.50M 7M] (map bigdec (values-of "main" d :betrag)))
        "the stored values came across, cast — not dropped and not re-emitted")
    (is (= ["r1" "r2"] (pks "main" d)))
    (testing "and no temp column is left standing"
      (is (= #{"internal_uuid" "betrag"} (set (keys (col-types d))))))
    (testing "re-running is a no-op"
      (ensure! d (cols :betrag [:decimal 38 10]))
      (is (= "decimal(38,10)" (get (col-types d) "betrag"))))))

(deftest an-earlier-snapshot-still-reads-under-its-OWN-schema
  (let [d :ddl-retype-history]
    (ensure! d (cols :betrag :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:betrag "12.50"}}])
    (let [before (with-open [c (pgds/open (tsys/ds))] (pgds/current-snapshot c))]
      (ensure! d (cols :betrag [:decimal 38 10]))
      (is (= "decimal(38,10)" (get (col-types d) "betrag")))
      (with-open [c (pgds/open (tsys/ds))]
        (is (= [12.50M]
               (mapv (comp bigdec first vals)
                     (jdbc/execute! c
                                    (let [{rel :sql
                                           ps :params}
                                            (pgds/as-of c "main" (naming/dataset-table d) before)]
                                      (into [(str "SELECT betrag FROM " rel)] ps)))))
            "the older view keeps the VALUES; the type is today's")))))

(deftest a-value-that-will-not-cast-BLOCKS-instead-of-nulling
  (let [d :ddl-retype-dirty]
    (ensure! d (cols :betrag :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:betrag "12.50"}}
                                     {:id "r2" :values {:betrag "n/a"}}])
    (let [e (is (thrown? clojure.lang.ExceptionInfo
                         (ensure! d (cols :betrag [:decimal 38 10]))))]
      (is (= :schema-mismatch (:error (ex-data e))))
      (is (= 1 (:uncastable (ex-data e)))))
    (is (= "varchar" (get (col-types d) "betrag")) "and the column is untouched")
    (is (= ["12.50" "n/a"] (values-of "main" d :betrag)))))

(deftest a-branch-retypes-its-OWN-copy
  (let [d :ddl-retype-branch]
    (ensure! d (cols :betrag :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:betrag "12.50"}}])
    (ensure-on! br d (cols :betrag [:decimal 38 10]))
    (is (= "decimal(38,10)" (get (col-types br-ns d) "betrag")))
    (is (= [12.50M] (map bigdec (values-of br-ns d :betrag))))
    (is (= "varchar" (get (col-types "main" d) "betrag"))
        "and the trunk kept its type — a retype is not something the fallback can un-see")
    (is (= ["12.50"] (values-of "main" d :betrag)))))

(deftest a-widening-within-a-type-family-is-a-promotion-and-a-family-change-is-not
  (with-open [c (pgds/open (tsys/ds))]
    (is (pgds/promotable? "DECIMAL(18,2)" "DECIMAL(38,10)") "wider precision, same family")
    (is (not (pgds/promotable? "INTEGER" "BIGINT"))          "a family change is a rewrite")
    (is (not (pgds/promotable? "VARCHAR" "DECIMAL(38,10)")))
    (is (not (pgds/promotable? "BIGINT" "VARCHAR")))
    (pgds/create-table! c "main" "promo" [{:name "v" :type "INTEGER"}])
    (is (= :rewritten (pgds/retype-column! c "main" "promo" "v" "INTEGER" "BIGINT")))
    (is (nil? (pgds/retype-column! c "main" "promo" "v" "BIGINT" "BIGINT"))
        "…and the same type twice is a NO-OP, which is the answer that matters:
         a retype that is not a change must never cost a rewrite")))

(deftest a-retype-decides-on-the-TYPE-not-on-its-spelling
  (testing "`norm-type` was upper-case + whitespace-strip over type STRINGS — the
            sixth site of what sutra.types.coltype absorbed. Postgres answers
            `numeric(38,10)` for the column the store was told to create as
            `DECIMAL(38,10)`, and comparing the two as text called a no-op a
            rewrite: O(all rows) through the change feed, for nothing."
    (with-open [c (pgds/open (tsys/ds))]
      (is (pgds/promotable? "numeric(38,10)" "DECIMAL(38,10)"))
      (is (pgds/promotable? "int4" "INTEGER"))
      (is (not (pgds/promotable? "BIGINT" "int4")))
      (pgds/create-table! c "main" "spelling" [{:name "v" :type "DECIMAL(38,10)"}])
      (is (nil? (pgds/retype-column! c "main" "spelling" "v" "numeric(38,10)" "DECIMAL(38,10)")))
      (is (= (ctype/col-type :decimal {:precision 38 :scale 10})
             (ctype/parse (get (pgds/column-types c "main" "spelling") "v")))
          "and the column was left alone rather than rewritten into itself"))))

(deftest a-column-outside-the-ident-set-still-reconciles-when-nothing-changed
  (testing "`währung` is a live attribute on :rechnung and :bestellung. Every
            statement here QUOTES it, and the ident guard belongs to the rewrite
            arm's spliced temp name alone — demanding it up front made an
            UNCHANGED column throw, so every :update-object-type on those two
            types failed and no push could re-install the ontology."
    (with-open [c (pgds/open (tsys/ds))]
      (pgds/create-table! c "main" "umlaut" [{:name "währung" :type "VARCHAR"}])
      (is (nil? (pgds/retype-column! c "main" "umlaut" "währung" "VARCHAR" "VARCHAR")))
      (pgds/create-table! c "main" "umlaut2" [{:name "grösse" :type "INTEGER"}])
      (is (= :rewritten (pgds/retype-column! c "main" "umlaut2" "grösse" "INTEGER" "BIGINT"))
          "and a rewrite still only quotes, so it needs no safe ident either"))))

(deftest a-value-the-stored-column-cannot-hold-is-a-NAMED-mismatch
  (let [d :ddl-write-mismatch]
    (ensure! d (cols :qty :long))
    (dsets/write-rows! (ctx/system-store) d :append [{:id "r1" :values {:qty 12}}])
    (let [e (is (thrown? clojure.lang.ExceptionInfo
                         (dsets/write-rows! (ctx/system-store) d :append
                                         [{:id "r2" :values {:qty "n/a"}}])))]
      (is (= :schema-mismatch (:error (ex-data e))))
      (is (= [{:column "qty" :stored (ctype/col-type :bigint) :emitted (ctype/col-type :varchar)}]
             (:columns (ex-data e)))))
    (testing "a value that DOES cast is not a mismatch — the store's own coercion stands"
      (dsets/write-rows! (ctx/system-store) d :append [{:id "r3" :values {:qty "12"}}])
      (is (= #{"r1" "r3"} (set (pks "main" d)))))
    (testing "and a column this batch leaves empty is never named — inference calls it VARCHAR"
      (dsets/write-rows! (ctx/system-store) d :append [{:id "r4" :values {:qty nil}}])
      (is (= #{"r1" "r3" "r4"} (set (pks "main" d)))))))

(deftest an-inferred-decimal-column-takes-the-scale-its-VALUE-carries
  (let [d :ddl-infer-integral]
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:betrag 40M}}])
    (is (= "decimal(38,0)" (get (col-types d) "betrag")))
    (is (= ["40"] (mapv str (values-of "main" d :betrag)))
        "no scale nobody declared — the ten zeros the client used to receive")))

(deftest more-than-ten-decimals-survive-the-write-path
  (let [d :ddl-infer-scale]
    (dsets/write-rows! (ctx/system-store) d :snapshot
                        [{:id "r1" :values {:betrag 0.123456789012M}}])
    (is (= "decimal(38,12)" (get (col-types d) "betrag")))
    (is (= ["0.123456789012"] (mapv str (values-of "main" d :betrag)))
        "under the constant the eleventh and twelfth digit were gone before any
         wire saw the value, and nothing anywhere read as an error")))

(deftest the-FIRST-row-does-not-nail-the-scale-down-for-the-rest
  (let [d :ddl-infer-widen]
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:betrag 40M}}
                                     {:id "r2" :values {:betrag 40.125M}}])
    (is (= "decimal(38,3)" (get (col-types d) "betrag")))
    (is (= ["40.000" "40.125"] (mapv str (values-of "main" d :betrag))))))

(deftest a-wider-stored-decimal-is-not-reported-as-a-MISMATCH
  (let [d :ddl-infer-vs-declared]
    (ensure! d (cols :betrag [:decimal 38 10], :qty :long))
    (dsets/write-rows! (ctx/system-store) d :append [{:id "r1" :values {:betrag 12.5M :qty 1}}])
    (is (= "decimal(38,10)" (get (col-types d) "betrag")) "the declared type stands")
    (let [e (is (thrown? clojure.lang.ExceptionInfo
                         (dsets/write-rows! (ctx/system-store) d :append
                                         [{:id "r2" :values {:betrag 12.5M :qty "n/a"}}])))]
      (is (= [{:column "qty" :stored (ctype/col-type :bigint) :emitted (ctype/col-type :varchar)}]
             (:columns (ex-data e)))
          "only the column that actually cannot hold its value"))))

(deftest an-unmappable-value-type-fails-loudly
  (is (thrown? clojure.lang.ExceptionInfo
               (ensure! :ddl-bogus (cols :x :no-such-value-type)))))

(deftest a-branch-install-forks-rather-than-shadowing
  (let [d :ddl-branch]
    (ensure! d (cols :a :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:a "x"}}
                                     {:id "r2" :values {:a "y"}}])
    (ensure-on! br d (cols :a :string, :b :long))
(is (= #{"r1" "r2"} (set (map :id (dsets/rows (ctx/system-store br) d))))
     "the branch still reads the trunk's rows")
    (is (= ["r1" "r2"] (pks br-ns d)) "which it holds itself, because the install forked")
    (is (contains? (set (keys (col-types br-ns d))) "b"))
    (is (not (contains? (set (keys (col-types "main" d))) "b"))
        "and the trunk did not grow the branch's column")))

(deftest a-dataset-declared-only-on-a-branch-stays-there
  (let [d :ddl-branch-native]
    (ensure-on! br d (cols :a :string))
    (is (contains? (col-types br-ns d) "a"))
    (with-open [c (pgds/open (tsys/ds))]
      (is (not (pgds/table-exists? c "main" (naming/dataset-table d)))))))

(deftest an-edge-dataset-is-keyed-on-internal-uuid
  (dsets/write-edges! (ctx/system-store) :srv-edge-ds [{:id "e1" :from-id "a" :to-id "b"}])
  (is (contains? (col-types :srv-edge-ds) "internal_uuid")))

(deftest a-transforms-datasets-mirror-its-outputs
  (let [W (ri/derived {:sigma clock/system}
           {:object-types {:widget {:object-type-name :widget
                                    :attribute-list (cols :sku :string, :qty :long)
                                    :plural "Widgets"}}
            :transforms   {:xf-mirror {:language :pg-sql :query "SELECT 1"
                                       :outputs [{:kind :object-type :object-type :widget
                                                  :dataset (rid/dataset-rid-for :xf-mirror)}]}}})
        tables (signature/datasets-of W)]
    (is (= [(naming/dataset-table (rid/dataset-rid-for :xf-mirror))] (keys tables)))
    (is (= #{:sku :qty} (set (map :attribute-name (val (first tables)))))))
  (testing "an intermediate transform owns one table, keyed by its own locator"
    (let [r (signature/datasets-of {:transforms {:xf-inter {:output-columns (cols :a :string)}}})]
      (is (= [(naming/dataset-table (rid/dataset-rid-for :xf-inter))] (keys r))))))

(defn- rename-raw! [s d from to]
  (with-open [c (pgds/open (tsys/ds))]
    (pgds/rename-column! c s (naming/dataset-table d) from to)))

(defn- add-raw! [s d col type]
  (with-open [c (pgds/open (tsys/ds))]
    (pgds/add-column! c s (naming/dataset-table d) col type)))

(deftest a-pre-fold-column-is-renamed-and-keeps-its-rows
  (let [d :ddl-fold]
    (ensure! d (cols :stueck-preis :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:stueck-preis "12.50"}}])
    (rename-raw! "main" d "stueck_preis" "stück_preis")
    (is (= #{"internal_uuid" "stück_preis"} (set (keys (col-types d))))
        "the state prod is in: the column the old rule minted")

    (ensure! d (cols :stück-preis :string))
    (is (= #{"internal_uuid" "stueck_preis"} (set (keys (col-types d))))
        "renamed, not added beside")
    (is (= ["12.50"] (values-of "main" d :stueck_preis)) "the values came across")
    (is (= ["r1"] (pks "main" d)))
    (testing "re-running is a no-op"
      (ensure! d (cols :stück-preis :string))
      (is (= #{"internal_uuid" "stueck_preis"} (set (keys (col-types d))))))))

(deftest an-empty-pre-fold-twin-is-dropped
  (let [d :ddl-fold-twin]
    (ensure! d (cols :stueck-preis :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:stueck-preis "12.50"}}])
    (add-raw! "main" d "stück_preis" "VARCHAR")
    (ensure! d (cols :stück-preis :string))
    (is (= #{"internal_uuid" "stueck_preis"} (set (keys (col-types d)))))
    (is (= ["12.50"] (values-of "main" d :stueck_preis)) "the FILLED twin is the survivor")))

(deftest a-pre-fold-twin-that-holds-values-is-refused
  (let [d :ddl-fold-collision]
    (ensure! d (cols :stueck-preis :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:stueck-preis "12.50"}}])
    (add-raw! "main" d "stück_preis" "VARCHAR")
    (with-open [c (pgds/open (tsys/ds))]
      (jdbc/execute! c [(str "UPDATE " (sql/dq "main") "."
                             (sql/dq (naming/dataset-table d))
                             " SET " (sql/dq "stück_preis") " = '9.99'")]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"FOLD-COLLISION"
                          (ensure! d (cols :stück-preis :string))))
    (is (= #{"internal_uuid" "stueck_preis" "stück_preis"} (set (keys (col-types d))))
        "and nothing was dropped on the way out")))

(deftest das-aufraeumen-eines-schnappschusses-verdeckt-den-echten-fehler-nicht
  (let [c (pgds/open (tsys/ds))
          e (is (thrown? clojure.lang.ExceptionInfo
                         (pgds/in-one-snapshot c
                                               (.close c)
                                               (throw (ex-info "die DDL selbst" {})))))]
      (is (= "die DDL selbst" (ex-message e))
          "stirbt die Verbindung, dann scheitern Rollback UND das Zuruecksetzen von
           autoCommit — beide duerfen den Fehler, der die Arbeit wirklich gekippt
           hat, nicht durch \"Connection is closed\" ersetzen")))

(deftest ein-retype-nimmt-die-sichten-ALLER-zweige-mit
  (let [d :ddl-retype-fremde-sicht
        zweig "scenario-fremd"]
    (ensure! d (cols :betrag :string))
    (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "r1" :values {:betrag "12.50"}}])
    (doseq [b [br zweig]]
      (ensure-on! b d (cols :betrag :string, :notiz :string)))
    (ensure! d (cols :betrag [:decimal 38 10]))
    (is (= "decimal(38,10)" (get (col-types d) "betrag"))
        "eine Overlay-Sicht eines FREMDEN Zweigs haengt an derselben Spalte des
         Trunks — wird nur die eigene gedroppt, weist Postgres den Alter ab
         (\"cannot alter type of a column used by a view or rule\")")
    (doseq [b [br zweig]]
      (is (= [12.50M]
             (with-open [c (pgds/open (tsys/ds))]
                 (mapv (comp bigdec first vals)
                       (jdbc/execute! c [(str "SELECT betrag FROM "
                                              (sql/dq (pgns/read-schema b)) "."
                                              (sql/dq (naming/dataset-table d)))]))))
          "und die Sicht des Zweigs steht danach wieder — und liest den Trunk in
           seinem NEUEN Typ, denn die eigene Kopie des Zweigs hat keine Zeile"))))
