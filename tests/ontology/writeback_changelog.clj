(ns ^:integration ontology.writeback-changelog
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.sources.datasources]
            [sutra.sources.policy :as policy]
            [sutra.sources.egress :as egress]
            [sutra.sources.jdbc :as fjdbc]
            [sutra.parser.spec :as spec]
            [support.helpers :refer [check section expect-error]]))

(def ^:private t0 (java.time.LocalDateTime/of 2026 10 7 9 30 5))

(def ^:private log-cfg
  {:table "SYS_NOTICE_CHANGES"
   :columns {:table "TABLE_NAME" :pk "PK_FIELDVALUE" :column "COLUMN_NAME"
             :previous "PREVIOUS_VALUE" :next "SET_VALUE"
             :actor ["CURRENTUSER" "OPEN_USER"]
             :date "EROEFFNUNGSDATUM" :time "EROEFFNUNGSTIME"
             :correlation "POSITION"}
   :constants {"NOTICE_TYPE_ID" 1 "CLIENT_SYSTEM" 0 "PROGRAM_NAME" "SUTRA"}})

(def ^:private src
  {:type :mssql :access :read-write
   :jdbc-url "jdbc:sqlserver://erp:1433;databaseName=mro"
   :egress #{"erp:1433"}
   :writeback
   {"WO_STAMM"
    {:pk "WO_ID"
     :writable-columns ["WO_STATUS_ID" "WO_TERMIN_VERMERK"]
     :column-map {"status_id" "WO_STATUS_ID" "termin_vermerk" "WO_TERMIN_VERMERK"}
     :column-values {"WO_STATUS_ID" [1 3 5 6 10 11 12 13 14]}
     :object-type :arbeitsauftrag
     :log log-cfg}
    "ADR_STAMM"
    {:pk "ADR_ID" :writable-columns ["NAME_FIRMA"] :log :none}
    "ANGEBOT"
    {:pk "ANG_ID"
     :identity "ANG_ID"
     :writable-columns ["ANGEBOTSNR"]
     :insertable-columns ["ANGEBOTSNR" "BEZEICHNUNG"]
     :autonumber {:table "SYS_AUTONR"
                  :where {"TABELLENNAME" "ANGEBOT" "FELDNAME" "ANGEBOTSNR_EX"}
                  :prefix-column "VORWAHL"
                  :counter-column "STARTNUMMER"
                  :width-column "STELLENANZAHL"
                  :into "ANGEBOTSNR"}
     :log (assoc log-cfg :insert-constants {"NOTICE_TYPE_ID" 4})}}})

(defn- plan-of [write-spec] (policy/write-plan t0 src write-spec))

(def ^:private base
  {:table "WO_STAMM" :pk-value 4711
   :set {"WO_STATUS_ID" 3} :expect {"WO_STATUS_ID" 12}
   :actor "milo" :correlation "sub-0042"})

(defn- gate-tests []
  (section "assert-write-allowed! — value allowlist")
  (check "an allowlisted status passes"
         (= :update (:op (plan-of base))))
  (check "a status NOT on :column-values is refused (4 = abgeschlossen)"
         (some? (expect-error #(plan-of (assoc base :set {"WO_STATUS_ID" 4})))))
  (check "the refusal names the column and the value"
         (re-find #"WO_STAMM.WO_STATUS_ID"
                  (str (expect-error #(plan-of (assoc base :set {"WO_STATUS_ID" 8}))))))
  (check "a column with no :column-values entry is unconstrained"
         (= :update (:op (plan-of (assoc base :set {"WO_TERMIN_VERMERK" "neu"}
                                         :expect {"WO_TERMIN_VERMERK" "alt"})))))

  (section "assert-write-allowed! — a sync alias is translated to the foreign column")
  (let [aliased (assoc base :set {"status_id" 3} :expect {"status_id" 12})
        {:keys [log] :as p} (plan-of aliased)]
    (check "an incoming alias is accepted" (= :update (:op p)))
    (check "the SQL is written in the foreign system's column name"
           (= {"WO_STATUS_ID" 3} (:set p)))
    (check "so is the guard" (= {"WO_STATUS_ID" 12} (:expect p)))
    (check "and so is the log row"
           (= "WO_STATUS_ID" (get (first (:rows log)) "COLUMN_NAME")))
    (check "the value allowlist judges the translated column, not the alias"
           (some? (expect-error #(plan-of (assoc aliased :set {"status_id" 4}))))))
  (check "an alias with no mapping and no allowlisted name is still refused"
         (some? (expect-error #(plan-of (assoc base :set {"status_id_alt" 3}
                                               :expect {"status_id_alt" 12})))))

  (section "assert-write-allowed! — the previous value is not optional")
  (check "a :set without a matching :expect is refused"
         (some? (expect-error #(plan-of (dissoc base :expect)))))
  (check "a partial :expect is refused"
         (some? (expect-error
                 #(plan-of (assoc base :set {"WO_STATUS_ID" 3 "WO_TERMIN_VERMERK" "x"})))))
  (check "a write that changes nothing is refused"
         (some? (expect-error #(plan-of (assoc base :expect {"WO_STATUS_ID" 3})))))

  (section "assert-write-allowed! — only the CHANGED columns are written and guarded")
  (let [p (plan-of (assoc base
                          :set    {"WO_STATUS_ID" 3 "WO_TERMIN_VERMERK" "gleich"}
                          :expect {"WO_STATUS_ID" 12 "WO_TERMIN_VERMERK" "gleich"}))]
    (check "an unchanged column is not written"
           (= {"WO_STATUS_ID" 3} (:set p)))
    (check "and is not carried into the guard — the WHERE renders `col = value`, and a
            large-text column (MSSQL text, Oracle CLOB) cannot be compared with = at all,
            so an untouched note column used to fail every status change"
           (= {"WO_STATUS_ID" 12} (:expect p)))
    (check "the log still records exactly the change"
           (= 1 (count (:rows (:log p))))))
  (check "an UNCHANGED column off :column-values does not block the write"
         (= :update (:op (plan-of (assoc base
                                         :set    {"WO_STATUS_ID" 4 "WO_TERMIN_VERMERK" "neu"}
                                         :expect {"WO_STATUS_ID" 4 "WO_TERMIN_VERMERK" "alt"})))))
  (check "an UNCHANGED column that is not writable does not block it either"
         (= :update (:op (plan-of (assoc base
                                         :set    {"GEHEIM" "x" "WO_STATUS_ID" 3}
                                         :expect {"GEHEIM" "x" "WO_STATUS_ID" 12})))))
  (check "the plan carries :expect through to the executor"
         (= {"WO_STATUS_ID" 12} (:expect (plan-of base))))

  (section "assert-write-allowed! — no log, no writeback")
  (check "a logging table refuses a blank actor"
         (some? (expect-error #(plan-of (assoc base :actor "  ")))))
  (check "a logging table refuses a missing actor"
         (some? (expect-error #(plan-of (dissoc base :actor)))))
  (check "the refusal says there is no service-account fallback"
         (re-find #"service-account" (str (expect-error #(plan-of (dissoc base :actor))))))
  (check "an explicitly unlogged table needs no actor"
         (= :update (:op (plan-of {:table "ADR_STAMM" :pk-value 1
                                   :set {"NAME_FIRMA" "NEU"} :expect {"NAME_FIRMA" "ALT"}}))))
  (check "an explicitly unlogged table plans no log"
         (nil? (:log (plan-of {:table "ADR_STAMM" :pk-value 1
                               :set {"NAME_FIRMA" "NEU"} :expect {"NAME_FIRMA" "ALT"}}))))

  (section "assert-write-allowed! — :actor-columns stamp the data row, not the log")
  (let [stamping (fn [ac]
                   (policy/write-plan t0
                                      (assoc-in src [:writeback "ADR_STAMM" :actor-columns] ac)
                                      {:table "ADR_STAMM" :pk-value 1 :actor " edda "
                                       :set {"NAME_FIRMA" "NEU"} :expect {"NAME_FIRMA" "ALT"}}))
        p        (stamping {:update ["CURRENTUSER"]})]
    (check "the update sets the actor into the row's own column"
           (= {"NAME_FIRMA" "NEU" "CURRENTUSER" "EDDA"} (:set p)))
    (check "the stamp is not guarded — its previous value is nobody's edit"
           (= {"NAME_FIRMA" "ALT"} (:expect p)))
    (check "an :insert-only stamp leaves an update alone"
           (= {"NAME_FIRMA" "NEU"} (:set (stamping {:insert ["OPEN_USER"]}))))
    (check "a stamping table refuses a write without an actor"
           (some? (expect-error
                   #(policy/write-plan t0
                                       (assoc-in src [:writeback "ADR_STAMM" :actor-columns]
                                                 {:update ["CURRENTUSER"]})
                                       {:table "ADR_STAMM" :pk-value 1
                                        :set {"NAME_FIRMA" "NEU"} :expect {"NAME_FIRMA" "ALT"}})))))
  (let [p (policy/write-plan t0
                             (assoc-in src [:writeback "ANGEBOT" :actor-columns]
                                       {:insert ["OPEN_USER" "CURRENTUSER"]})
                             {:table "ANGEBOT" :insert {"BEZEICHNUNG" "Fahrwerk"} :actor "milo"})]
    (check "the insert carries both stamps"
           (= {"BEZEICHNUNG" "Fahrwerk" "OPEN_USER" "MILO" "CURRENTUSER" "MILO"} (:insert p)))
    (check "and names them, so the create log leaves them out"
           (= #{"OPEN_USER" "CURRENTUSER"} (set (:stamped p)))))
  (check "the validator takes :actor-columns"
         (spec/writeback-valid? (assoc-in (:writeback src) ["ADR_STAMM" :actor-columns]
                                          {:insert ["OPEN_USER"] :update ["CURRENTUSER"]})))
  (check "and refuses an op it does not know"
         (not (spec/writeback-valid? (assoc-in (:writeback src) ["ADR_STAMM" :actor-columns]
                                               {:delete ["CURRENTUSER"]}))))

  (section "the change-log row the plan carries")
  (let [{:keys [log]} (plan-of base)
        row (first (:rows log))]
    (check "one row per column that moves" (= 1 (count (:rows log))))
    (check "the plan states how many rows must land" (= 1 (:expect-count log)))
    (check "it targets the log table" (= "SYS_NOTICE_CHANGES" (:table log)))
    (check "it names the changed table" (= "WO_STAMM" (get row "TABLE_NAME")))
    (check "the key rides as text" (= "4711" (get row "PK_FIELDVALUE")))
    (check "it names the column" (= "WO_STATUS_ID" (get row "COLUMN_NAME")))
    (check "it carries the previous value" (= "12" (get row "PREVIOUS_VALUE")))
    (check "it carries the new value" (= "3" (get row "SET_VALUE")))
    (check "the human is in CURRENTUSER, in the foreign system's spelling"
           (= "MILO" (get row "CURRENTUSER")))
    (check "OPEN_USER repeats it, as the measured rows do"
           (= "MILO" (get row "OPEN_USER")))
    (check "the constants ride along"
           (= [1 0 "SUTRA"] [(get row "NOTICE_TYPE_ID") (get row "CLIENT_SYSTEM")
                             (get row "PROGRAM_NAME")]))
    (check "the submission is correlatable" (= "sub-0042" (get row "POSITION")))
    (check "the time is char(8), or the change cannot be sorted into the day"
           (re-matches #"\d{2}:\d{2}:\d{2}" (get row "EROEFFNUNGSTIME")))
    (check "the date is a plain date"
           (re-matches #"\d{4}-\d{2}-\d{2}" (get row "EROEFFNUNGSDATUM"))))

  (let [{:keys [log]} (plan-of (assoc base
                                      :set {"WO_STATUS_ID" 3 "WO_TERMIN_VERMERK" "neu"}
                                      :expect {"WO_STATUS_ID" 12 "WO_TERMIN_VERMERK" "alt"}))]
    (check "two changed columns mean two log rows" (= 2 (count (:rows log))))
    (check "and the plan demands both" (= 2 (:expect-count log))))

  (let [{:keys [log]} (plan-of (assoc base
                                      :set {"WO_STATUS_ID" 3 "WO_TERMIN_VERMERK" "gleich"}
                                      :expect {"WO_STATUS_ID" 12 "WO_TERMIN_VERMERK" "gleich"}))]
    (check "a column that does not move gets no log row" (= 1 (count (:rows log))))))

(defn- validator-tests []
  (section "writeback allowlist validator — the log is declared, not assumed")
  (check "an allowlist with a change-log descriptor is valid"
         (true? (boolean (spec/writeback-valid? (:writeback src)))))
  (check "a table with NO :log at all is refused"
         (false? (boolean (spec/writeback-valid?
                           (update (:writeback src) "WO_STAMM" dissoc :log)))))
  (check "opting out is possible, but it has to be written down"
         (true? (boolean (spec/writeback-valid?
                          (assoc-in (:writeback src) ["WO_STAMM" :log] :none)))))
  (check "a log descriptor without an actor column is refused"
         (false? (boolean (spec/writeback-valid?
                           (update-in (:writeback src) ["WO_STAMM" :log :columns]
                                      dissoc :actor)))))
  (check "an unknown key in the log descriptor is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["WO_STAMM" :log :tabelle] "x")))))
  (check ":column-values on a non-writable column is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["WO_STAMM" :column-values]
                                     {"GEHEIM" [1]})))))
  (check ":column-map pointing at a non-writable column is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["WO_STAMM" :column-map]
                                     {"x" "GEHEIM"})))))
  (check "an empty value set is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["WO_STAMM" :column-values]
                                     {"WO_STATUS_ID" []}))))))

(defn- sqlite-conn! []
  (let [f (java.io.File/createTempFile "wb-changelog" ".db")]
    (.deleteOnExit f)
    (.delete f)
    (let [c (jdbc/get-connection (jdbc/get-datasource {:dbtype "sqlite" :dbname (.getPath f)}))]
      (jdbc/execute! c
                     ["CREATE TABLE WO_STAMM (WO_ID INTEGER PRIMARY KEY, WO_STATUS_ID INTEGER, WO_TERMIN_VERMERK TEXT)"])
      (jdbc/execute! c [(str "CREATE TABLE SYS_NOTICE_CHANGES ("
                             "NOTICE_CHANGES_ID INTEGER PRIMARY KEY AUTOINCREMENT, "
                             "TABLE_NAME TEXT, PK_FIELDVALUE TEXT, COLUMN_NAME TEXT, "
                             "PREVIOUS_VALUE TEXT, SET_VALUE TEXT, NOTICE_TYPE_ID INTEGER, "
                             "CURRENTUSER TEXT, OPEN_USER TEXT, CLIENT_SYSTEM INTEGER, "
                             "PROGRAM_NAME TEXT, POSITION TEXT, "
                             "EROEFFNUNGSDATUM TEXT, EROEFFNUNGSTIME TEXT)")])
      (jdbc/execute! c
                     ["INSERT INTO WO_STAMM (WO_ID, WO_STATUS_ID, WO_TERMIN_VERMERK) VALUES (4711, 12, NULL)"])
      c)))

(defn- one [c sql]
  (jdbc/execute-one! c [sql] {:builder-fn rs/as-unqualified-lower-maps}))

(defn- status-of [c] (:wo_status_id (one c "SELECT WO_STATUS_ID FROM WO_STAMM WHERE WO_ID = 4711")))
(defn- log-count [c] (:n (one c "SELECT COUNT(*) AS n FROM SYS_NOTICE_CHANGES")))

(defn- run-update! [c plan] (fjdbc/apply-plan! c false :sqlite plan))

(defn- executor-tests []
  (section "executor — the row and its log row move together")
  (let [c (sqlite-conn!)
        res (run-update! c (plan-of base))]
    (check "the row moved" (= 3 (status-of c)))
    (check "exactly one row was updated" (= 1 (:update-count res)))
    (check "the change-log carries it" (= 1 (log-count c)))
    (check "the result says where it was logged" (= "SYS_NOTICE_CHANGES" (:log-table res)))
    (let [row (one c "SELECT * FROM SYS_NOTICE_CHANGES")]
      (check "the logged previous value is the one the update was guarded on"
             (= "12" (:previous_value row)))
      (check "the logged new value is what landed" (= "3" (:set_value row)))
      (check "the human is named" (= "MILO" (:currentuser row))))
    (.close c))

  (section "executor — the MSSQL guard survives a large-text column")
  (let [sql (atom nil)
        n   (with-redefs [jdbc/execute!     (fn [& _] [{:c "WO_AUSFUEHRUNG"}])
                          jdbc/execute-one! (fn [_ q & _] (reset! sql (first q)) {:n 1})]
              (fjdbc/execute-update! nil :mssql
                                     {:table "WO_STAMM" :pk "WO_NR" :pk-value "WO15.0003"
                                      :set {"WO_AUSFUEHRUNG" "neu"
                                            "KONSOL_DATUM" #inst "2026-09-24"}
                                      :expect {"WO_AUSFUEHRUNG" "alt"
                                               "KONSOL_DATUM" #inst "2025-12-22T20:18:01.207Z"}}))]
    (check "the guard casts a text column, because text/ntext cannot be compared with = at all"
           (re-find #"CAST\(\[WO_AUSFUEHRUNG\] AS varchar\(max\)\) = \?" @sql))
    (check "a datetime is compared as a datetime — cast to text it lost its seconds"
           (and (re-find #"\(\[KONSOL_DATUM\] = \?" @sql)
                (not (re-find #"CAST\(\[KONSOL_DATUM\]" @sql))))
    (check "the IS NULL arm stays uncast — that comparison is legal on text"
           (re-find #"\[WO_AUSFUEHRUNG\] IS NULL" @sql))
    (check "the assignment itself is never cast"
           (re-find #"SET (\[\w+\] = \?, )*\[WO_AUSFUEHRUNG\] = \?" @sql))
    (check "the count is the statement's own rows, not a trigger's"
           (and (re-find #"^SET NOCOUNT ON;" @sql)
                (re-find #"OUTPUT INSERTED\.\[WO_NR\] INTO @sutra_out \(id\) WHERE" @sql)
                (re-find #"SELECT COUNT\(\*\) AS n FROM @sutra_out;$" @sql)
                (= 1 n))))
  (let [sql (atom nil)]
    (with-redefs [jdbc/execute!     (fn [& _] [])
                  jdbc/execute-one! (fn [_ q & _] (reset! sql (first q)) {:n 1})]
      (fjdbc/execute-delete! nil :mssql
                             {:table "ZV_ZEITEN_KONSOL" :pk "ZV_ZEITEN_ID" :pk-value 142960
                              :expect {"ZEITEN_ID" 1 "MITARBEITER_ID" 2}}))
    (check "a delete counts its own rows the same way"
           (re-find #"DELETE FROM \[ZV_ZEITEN_KONSOL\] OUTPUT DELETED\.\[ZV_ZEITEN_ID\] INTO @sutra_out \(id\) WHERE" @sql)))
  (let [sql (atom nil)]
    (with-redefs [jdbc/execute-one! (fn [_ q & _] (reset! sql (first q))
                                      {:next.jdbc/update-count 1})]
      (fjdbc/execute-update! nil :postgres {:table "t" :pk "id" :pk-value 1
                                            :set {"note" "neu"} :expect {"note" "alt"}}))
    (check "a dialect that can compare text keeps the plain form"
           (and (re-find #"\"note\" = \?" @sql) (not (re-find #"CAST" @sql)))))

  (section "executor — a row someone else moved first is a conflict, not an overwrite")
  (let [c (sqlite-conn!)
        _ (jdbc/execute! c ["UPDATE WO_STAMM SET WO_STATUS_ID = 5 WHERE WO_ID = 4711"])
        err (expect-error #(run-update! c (plan-of base)))]
    (check "it refuses" (some? err))
    (check "it says the row no longer holds what the edit was based on"
           (re-find #"no longer holds" (str err)))
    (check "the other writer's value stands" (= 5 (status-of c)))
    (check "and nothing was logged" (= 0 (log-count c)))
    (.close c))

  (section "executor — no log table, no change: the rollback has teeth")
  (let [c (sqlite-conn!)
        _ (jdbc/execute! c ["ALTER TABLE SYS_NOTICE_CHANGES RENAME TO SYS_NOTICE_CHANGES_WEG"])
        err (expect-error #(run-update! c (plan-of base)))]
    (check "the write fails" (some? err))
    (check "and the row is UNCHANGED — the UPDATE rolled back with the log"
           (= 12 (status-of c)))
    (.close c))

  (section "executor — a key that is not unique in the source stops the write")
  (let [c (sqlite-conn!)
        _ (jdbc/execute! c ["CREATE TABLE DOPPELT (WO_ID INTEGER, WO_STATUS_ID INTEGER)"])
        _ (jdbc/execute! c ["INSERT INTO DOPPELT VALUES (4711, 12), (4711, 12)"])
        plan (assoc (plan-of base) :table "DOPPELT")
        err (expect-error #(run-update! c plan))]
    (check "two matching rows are refused" (some? err))
    (check "it says the key is not unique" (re-find #"not unique" (str err)))
    (check "and neither row moved"
           (zero? (:n (one c "SELECT COUNT(*) AS n FROM DOPPELT WHERE WO_STATUS_ID = 3"))))
    (check "and nothing was logged" (= 0 (log-count c)))
    (.close c))

  (section "executor — a short log is a failed writeback")
  (let [c (sqlite-conn!)
        plan (assoc-in (plan-of base) [:log :expect-count] 2)
        err (expect-error #(run-update! c plan))]
    (check "landing fewer log rows than the plan demands fails the write" (some? err))
    (check "it says so" (re-find #"change-log incomplete" (str err)))
    (check "and the row is unchanged" (= 12 (status-of c)))
    (.close c))

  (section "executor — a NULL column compares NULL-safely")
  (let [c (sqlite-conn!)
        plan (plan-of (assoc base :set {"WO_TERMIN_VERMERK" "gesetzt"}
                             :expect {"WO_TERMIN_VERMERK" nil}))
        res (run-update! c plan)]
    (check "expecting NULL against a NULL column matches the row" (= 1 (:update-count res)))
    (check "and it is logged" (= 1 (log-count c)))
    (let [row (one c "SELECT PREVIOUS_VALUE FROM SYS_NOTICE_CHANGES")]
      (check "the previous value is recorded as NULL" (nil? (:previous_value row))))
    (.close c))

  (section "executor — an unlogged table still writes")
  (let [c (sqlite-conn!)
        _ (jdbc/execute! c ["CREATE TABLE ADR_STAMM (ADR_ID INTEGER PRIMARY KEY, NAME_FIRMA TEXT)"])
        _ (jdbc/execute! c ["INSERT INTO ADR_STAMM VALUES (1, 'ALT')"])
        res (run-update! c (plan-of {:table "ADR_STAMM" :pk-value 1
                                     :set {"NAME_FIRMA" "NEU"} :expect {"NAME_FIRMA" "ALT"}}))]
    (check "it lands" (= 1 (:update-count res)))
    (check "and reports that nothing was logged" (= 0 (:logged res)))
    (.close c)))

(def ^:private ins
  {:table "ANGEBOT" :insert {"BEZEICHNUNG" "Fahrwerksüberholung"} :returning "ANG_ID"
   :actor "milo" :correlation "sub-0099"})

(defn- insert-gate-tests []
  (section "assert-write-allowed! — a creation owes a log row too")
  (check "a well-formed insert plans" (= :insert (:op (plan-of ins))))
  (check "a logging table refuses an insert with no actor"
         (some? (expect-error #(plan-of (dissoc ins :actor)))))
  (check "the minted column may not be supplied by the caller"
         (some? (expect-error #(plan-of (assoc-in ins [:insert "ANGEBOTSNR"] "AN26.0001")))))
  (check "the plan carries the number source"
         (= "SYS_AUTONR" (get-in (plan-of ins) [:autonumber :table])))
  (check "the plan defers the log to after the insert"
         (= "SYS_NOTICE_CHANGES" (get-in (plan-of ins) [:log :table])))

  (section "the creation's log rows")
  (let [rows (policy/insert-log-rows (:log (plan-of ins)) "ANGEBOT" 77
                                     {"ANGEBOTSNR" "AN26.0194" "BEZEICHNUNG" "Fahrwerksüberholung"})
        by   (into {} (map (juxt #(get % "COLUMN_NAME") identity)) rows)]
    (check "one row per inserted column" (= 2 (count rows)))
    (check "the creation is typed as a creation, not a field change"
           (= [4 4] (mapv #(get % "NOTICE_TYPE_ID") rows)))
    (check "the new key is on every row" (= ["77" "77"] (mapv #(get % "PK_FIELDVALUE") rows)))
    (check "a creation has no previous value"
           (every? nil? (map #(get % "PREVIOUS_VALUE") rows)))
    (check "the minted number is in the log"
           (= "AN26.0194" (get-in by ["ANGEBOTSNR" "SET_VALUE"])))
    (check "the human is named" (= "MILO" (get-in by ["BEZEICHNUNG" "CURRENTUSER"]))))

  (section "validator — an insertable logging table must name its create marker")
  (check "without :insert-constants it is refused"
         (false? (boolean (spec/writeback-valid?
                           (update-in (:writeback src) ["ANGEBOT" :log]
                                      dissoc :insert-constants)))))
  (check ":autonumber writing into a non-insertable column is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["ANGEBOT" :autonumber :into]
                                     "GEHEIM")))))
  (check ":autonumber with no width anywhere is refused"
         (false? (boolean (spec/writeback-valid?
                           (update-in (:writeback src) ["ANGEBOT" :autonumber]
                                      dissoc :width-column))))))

(defn- autonumber-conn! []
  (let [c (sqlite-conn!)]
    (jdbc/execute! c
                   ["CREATE TABLE ANGEBOT (ANG_ID INTEGER PRIMARY KEY AUTOINCREMENT, ANGEBOTSNR TEXT, BEZEICHNUNG TEXT)"])
    (jdbc/execute! c
                   ["CREATE TABLE SYS_AUTONR (TABELLENNAME TEXT, FELDNAME TEXT, VORWAHL TEXT, STARTNUMMER INTEGER, STELLENANZAHL INTEGER)"])
    (jdbc/execute! c ["INSERT INTO SYS_AUTONR VALUES ('ANGEBOT','ANGEBOTSNR_EX','AN26.',193,4)"])
    c))

(defn- counter-of [c] (:startnummer (one c "SELECT STARTNUMMER FROM SYS_AUTONR")))

(defn- insert-executor-tests []
  (section "executor — the number is the LAST issued, so the next one is +1")
  (let [c (autonumber-conn!)
        res (fjdbc/apply-plan! c false :sqlite (plan-of ins))]
    (check "the counter held 193, so the row gets 194 — not 193"
           (= "AN26.0194" (:number res)))
    (check "zero-padded to the declared width" (= 9 (count (:number res))))
    (check "the counter advanced" (= 194 (counter-of c)))
    (check "the row carries the number"
           (= "AN26.0194" (:angebotsnr (one c "SELECT ANGEBOTSNR FROM ANGEBOT"))))
    (check "the creation is logged" (= 2 (:logged res)))
    (.close c))

  (section "executor — two creations never share a number")
  (let [c (autonumber-conn!)
        a (fjdbc/apply-plan! c false :sqlite (plan-of ins))
        b (fjdbc/apply-plan! c false :sqlite (plan-of ins))]
    (check "consecutive numbers" (= ["AN26.0194" "AN26.0195"] [(:number a) (:number b)]))
    (check "and the counter matches" (= 195 (counter-of c)))
    (.close c))

  (section "executor — no log, no creation, and the number is handed back")
  (let [c (autonumber-conn!)
        _ (jdbc/execute! c ["ALTER TABLE SYS_NOTICE_CHANGES RENAME TO SYS_NOTICE_CHANGES_WEG"])
        err (expect-error #(fjdbc/apply-plan! c false :sqlite (plan-of ins)))]
    (check "the creation fails" (some? err))
    (check "no row was created"
           (zero? (:n (one c "SELECT COUNT(*) AS n FROM ANGEBOT"))))
    (check "and the number was NOT burned — the counter is back at 193"
           (= 193 (counter-of c)))
    (.close c))

  (section "executor — a counter that names no single row is refused")
  (let [c (autonumber-conn!)
        _ (jdbc/execute! c
                         ["INSERT INTO SYS_AUTONR VALUES ('ANGEBOT','ANGEBOTSNR_EX','AN26.',500,4)"])
        err (expect-error #(fjdbc/apply-plan! c false :sqlite (plan-of ins)))]
    (check "two counter rows are refused" (some? err))
    (check "it says a number source must be exactly one row"
           (re-find #"exactly one row" (str err)))
    (check "and nothing was created"
           (zero? (:n (one c "SELECT COUNT(*) AS n FROM ANGEBOT"))))
    (.close c))

  (section "executor — an exhausted number range stops rather than silently widening")
  (let [c (autonumber-conn!)
        _ (jdbc/execute! c ["UPDATE SYS_AUTONR SET STARTNUMMER = 9999"])
        err (expect-error #(fjdbc/apply-plan! c false :sqlite (plan-of ins)))]
    (check "10000 does not fit 4 digits" (some? err))
    (check "it says the range is exhausted" (re-find #"exhausted" (str err)))
    (check "and nothing was created"
           (zero? (:n (one c "SELECT COUNT(*) AS n FROM ANGEBOT"))))
    (.close c)))

(defn- session-tests []
  (section "write session — several writes, ONE transaction")
  (let [f    (doto (java.io.File/createTempFile "wb-session" ".db") (.deleteOnExit) (.delete))
        open #(jdbc/get-connection (jdbc/get-datasource {:dbtype "sqlite" :dbname (.getPath f)}))
        _    (with-open [c (open)]
               (jdbc/execute! c
                              ["CREATE TABLE WO_STAMM (WO_ID INTEGER PRIMARY KEY, WO_STATUS_ID INTEGER, WO_TERMIN_VERMERK TEXT)"])
               (jdbc/execute! c
                              ["CREATE TABLE ADR_STAMM (ADR_ID INTEGER PRIMARY KEY, NAME_FIRMA TEXT)"])
               (jdbc/execute! c ["INSERT INTO WO_STAMM VALUES (4711, 12, NULL)"])
               (jdbc/execute! c ["INSERT INTO ADR_STAMM VALUES (1, 'ALT')"]))
        spec (-> src (assoc :type :sqlite) (update :writeback dissoc "ANGEBOT")
                 (assoc-in [:writeback "WO_STAMM" :log] :none))
        sess @#'egress/sessions
        begin! (fn [id] (let [c (open)]
                          (.setAutoCommit c false)
                          (swap! sess assoc id {:conn c :spec spec :source "erp"})))
        write! (fn [id ws]
                 (#'egress/handle-write {:tx id :spec {:source-name "erp"} :write-spec ws}))
        now    #(with-open [c (open)]
                  [(:wo_status_id (one c "SELECT WO_STATUS_ID FROM WO_STAMM"))
                   (:name_firma (one c "SELECT NAME_FIRMA FROM ADR_STAMM"))])
        adr    {:table "ADR_STAMM" :pk-value 1 :set {"NAME_FIRMA" "NEU"}
                :expect {"NAME_FIRMA" "ALT"}}
        wo     (dissoc base :actor)]
    (begin! "a")
    (write! "a" wo)
    (write! "a" adr)
    (check "nothing is visible before the commit" (= [12 "ALT"] (now)))
    (#'egress/end-session! "a" true)
    (check "the commit lands both" (= [3 "NEU"] (now)))

    (with-open [c (open)]
      (jdbc/execute! c ["UPDATE WO_STAMM SET WO_STATUS_ID = 12"])
      (jdbc/execute! c ["UPDATE ADR_STAMM SET NAME_FIRMA = 'ALT'"]))
    (begin! "b")
    (write! "b" wo)
    (check "a failing second write refuses"
           (some? (expect-error #(write! "b" (assoc adr :expect {"NAME_FIRMA" "ANDERS"})))))
    (check "and takes the first one with it" (= [12 "ALT"] (now)))
    (check "the session is gone, not left open" (nil? (get @sess "b")))
    (check "a write into a session of another source is refused"
           (do (begin! "c")
               (some? (expect-error #(#'egress/handle-write {:tx "c" :spec {:source-name "crm"}
                                                             :write-spec wo})))))
    (#'egress/end-session! "c" false)))

(defn run-tests []
  (gate-tests)
  (session-tests)
  (validator-tests)
  (executor-tests)
  (insert-gate-tests)
  (insert-executor-tests)
  (println "\n  writeback-changelog: all checks passed"))
