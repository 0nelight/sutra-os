(ns ^:integration ontology.writeback-create
  (:require [sutra.sources.datasources]
            [sutra.sources.policy :as policy]
            [sutra.sources.writeback :as wb]
            [sutra.parser.spec :as spec]
            [support.helpers :refer [check section expect-error]]))

(def ^:private t0 (java.time.LocalDateTime/of 2026 10 7 9 30 5))

(def ^:private src
  {:type :mssql :access :read-write
   :jdbc-url "jdbc:sqlserver://erp:1433;databaseName=mro"
   :egress #{"erp:1433"}
   :writeback
   {"ADR_STAMM"
    {:pk "ADR_ID" :identity "ADR_ID"
     :writable-columns  #{"NAME_FIRMA" "STRASSE" "PLZ" "ORT" "E_MAIL_ADR" "UID_NUMMER"}
     :insertable-columns #{"NAME_FIRMA" "STRASSE" "PLZ" "ORT" "E_MAIL_ADR" "UID_NUMMER"}
     :object-type :lieferant
     :match-key "UID_NUMMER"
     :source-of-record :apply-most-recent
     :timestamp-attr :aenderungsdatum
     :log :none}}})

(def ^:private tgt
  {:table "ADR_STAMM"
   :pk-attr :adr-id
   :identity-col "ADR_ID"
   :insertable #{"NAME_FIRMA" "STRASSE" "PLZ" "ORT" "E_MAIL_ADR" "UID_NUMMER"}
   :match-key "UID_NUMMER"
   :column-map {:adr-id "ADR_ID" :name "NAME_FIRMA" :strasse "STRASSE"
                :plz "PLZ" :ort "ORT" :email "E_MAIL_ADR" :uid-nummer "UID_NUMMER"}})

(defn run-tests []
  (section "assert-write-allowed! — INSERT shape (agent boundary)")
  (let [ok {:table "ADR_STAMM"
            :insert {"NAME_FIRMA" "ACME GmbH" "ORT" "Wien" "UID_NUMMER" "ATU123"}
            :returning "ADR_ID"}]
    (check "a well-formed insert-spec yields an :insert plan"
           (= :insert (:op (policy/write-plan t0 src ok))))
    (check "the plan carries the returning identity column"
           (= "ADR_ID" (:returning (policy/write-plan t0 src ok))))
    (check "the plan carries only the insert columns"
           (= (:insert ok) (:insert (policy/write-plan t0 src ok))))

    (check "a column NOT on :insertable-columns is refused"
           (some? (expect-error
                   #(policy/write-plan t0 src (assoc-in ok [:insert "GEHEIM"] "x")))))
    (check ":returning that is not the table's :identity column is refused"
           (some? (expect-error
                   #(policy/write-plan t0 src (assoc ok :returning "ORT")))))
    (check "an empty :insert map is refused"
           (some? (expect-error #(policy/write-plan t0 src (assoc ok :insert {})))))
    (check "a :read-only source refuses an insert (fail-closed)"
           (some? (expect-error
                   #(policy/write-plan t0 (assoc src :access :read-only) ok))))
    (check "a table not on the :writeback allowlist is refused"
           (some? (expect-error #(policy/write-plan t0 src (assoc ok :table "SECRETS")))))
    (check "a table with no :insertable-columns allowlist refuses insert"
           (some? (expect-error
                   #(policy/write-plan t0
                     (update-in src [:writeback "ADR_STAMM"] dissoc :insertable-columns)
                     ok))))
    (check "an insert with no :returning still validates (identity optional)"
           (= :insert (:op (policy/write-plan t0 src (dissoc ok :returning))))))

  (section "assert-write-allowed! — UPDATE shape unchanged")
  (let [upd {:table "ADR_STAMM" :pk-value 42
             :set {"NAME_FIRMA" "NEU"} :expect {"NAME_FIRMA" "ALT"}}]
    (check "an update-spec still yields an :update plan"
           (= :update (:op (policy/write-plan t0 src upd))))
    (check "the update plan takes its pk column from the allowlist"
           (= "ADR_ID" (:pk (policy/write-plan t0 src upd))))
    (check "a non-:writable column is still refused on update"
           (some? (expect-error
                   #(policy/write-plan t0 src (assoc-in upd [:set "GEHEIM"] "x"))))))

  (section "reshape-insert / marshal-insert — object → INSERT write-spec")
  (let [values {:adr-id nil :name "ACME GmbH" :ort "Wien" :uid-nummer "ATU123" :plz "1010"}
        r      (wb/reshape-insert tgt values)]
    (check "reshape targets the right table" (= "ADR_STAMM" (:table r)))
    (check "reshape names the identity column as :returning" (= "ADR_ID" (:returning r)))
    (check "reshape maps ontology attrs to their columns"
           (= {"NAME_FIRMA" "ACME GmbH" "ORT" "Wien" "UID_NUMMER" "ATU123" "PLZ" "1010"}
              (:insert r)))
    (check "reshape OMITS the DB-assigned identity column"
           (not (contains? (:insert r) "ADR_ID")))
    (check "reshape drops attrs whose column is not insertable"
           (= (:insert r) (:insert (wb/reshape-insert tgt (assoc values :not-mapped "z")))))
    (check "reshape with no insertable value present is refused"
           (some? (expect-error #(wb/reshape-insert tgt {:adr-id 1})))))

  (section "find-by-match-key — probe is a no-op without a key/value")
  (check "no :match-key ⇒ nil (dedup disabled)"
         (nil? (wb/find-by-match-key (dissoc tgt :match-key) "ATU123")))
  (check "no probe value ⇒ nil"
         (nil? (wb/find-by-match-key tgt nil)))

  (section "writeback allowlist validator — CREATE keys")
  (check "a create-enabled writeback allowlist is valid"
         (true? (boolean (spec/writeback-valid? (:writeback src)))))
  (check "a bogus :mode is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["ADR_STAMM" :mode] :carrier-pigeon)))))
  (check "a non-string :identity is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["ADR_STAMM" :identity] 7)))))
  (check "an empty :insertable-columns is refused"
         (false? (boolean (spec/writeback-valid?
                           (assoc-in (:writeback src) ["ADR_STAMM" :insertable-columns] [])))))

  (println "\n  writeback-create: all checks passed"))
