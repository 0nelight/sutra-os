(ns ^:integration ontology.writeback-target-declared
  (:require [support.helpers :refer [check section expect-error]]
            [sutra.sources.writeback-dataset]))

(def ^:private target #'sutra.sources.writeback-dataset/writeback-target)

(def ^:private cfg
  {:pk "ANG_ID"
   :identity "ANG_ID"
   :object-type :angebot
   :match-key "ANGEBOTSNR"
   :timestamp-attr :geaendert
   :source-of-record :apply-most-recent
   :autonumber {:table "SYS_AUTONR" :into "ANGEBOTSNR"}
   :column-map {"angebot_nr" "ANGEBOTSNR"
                "bezeichnung" "ANGEBOTSBEZ"
                "status_id" "ANG_STATUS_ID"}
   :writable-columns ["ANG_STATUS_ID" "ANGEBOTSBEZ"]
   :insertable-columns ["ANGEBOTSBEZ" "ANG_STATUS_ID" "ANGEBOTSNR"]})

(defn- schema-with
  [wb]
  {:sources
   {:src/mro {:source-name :mro :access :read-write :type :mssql :writeback wb}}
   :datasets
   {:ds/raw {:dataset-name :angebot_raw}}
   :transforms
   {:tf/sync  {:transform-name :angebot_raw-sync
               :input  {:kind :datasource :datasource :mro}
               :source {:query "SELECT a.ANG_ID AS ang_id FROM ANGEBOT a"}
               :output {:kind :dataset :dataset :ds/raw}}
    :tf/shape {:transform-name :shape-angebot
               :input  {:kind :dataset :dataset :ds/raw}
               :output {:kind :object-type :object-type :ot/angebot
                        :primary-key :ang-id}}}
   :object-types
   {:ot/angebot
    {:object-type-name :angebot
     :attribute-list [{:attribute-name :ang-id :attribute-type :string}
                      {:attribute-name :angebot-nr :attribute-type :string}
                      {:attribute-name :bezeichnung :attribute-type :string}
                      {:attribute-name :status-id :attribute-type :integer}
                      {:attribute-name :geaendert :attribute-type :date}]
     :plural "Angebote"}}})

(defn run-tests []
  (section "writeback target — DECLARED, not derived from the read lineage")
  (let [schema (schema-with {"ANGEBOT" cfg})
        tgt    (target schema :angebot nil)]
    (check "a type behind a shaping hop resolves — the read chain is not consulted"
           (= "ANGEBOT" (:table tgt)))
    (check "the source comes from the allowlist that names the type"
           (= :mro (:source-id tgt)))
    (check "the pk ATTRIBUTE resolves though :column-map cannot name the pk column"
           (= :ang-id (:pk-attr tgt)))
    (check "the live re-read addresses the declared table, keyed by the foreign pk"
           (and (= "ANG_ID" (:pk-result tgt)) (nil? (:query tgt))))

    (section "column names — attribute → fold → the allowlist's rename")
    (check "an attribute maps to the FOREIGN column, not the dataset spelling"
           (= "ANG_STATUS_ID" (get-in tgt [:column-map :status-id])))
    (check "an attribute the allowlist does not rename keeps its folded spelling"
           (= "geaendert" (get-in tgt [:column-map :geaendert])))
    (check "writable-attrs are the attributes whose FOREIGN column is writable"
           (= #{:status-id :bezeichnung} (:writable-attrs tgt)))
    (check "the pk is never writable, even when its column is listed"
           (not (contains? (:writable-attrs tgt) :ang-id)))
    (check "match-key and autonumber resolve back to attributes"
           (and (= [:angebot-nr] (:match-attrs tgt))
                (= :angebot-nr (:number-attr tgt))))

    (section "refusals")
    (check "a type no allowlist names is refused, and says so"
           (re-find #"not writeback-enabled"
                    (or (expect-error #(target schema :bestellung nil)) "")))
    (check "an allowlist :pk naming no column of the type is refused"
           (re-find #"names no column"
                    (or (expect-error
                         #(target (schema-with {"ANGEBOT" (assoc cfg :pk "NOPE")})
                                  :angebot nil))
                        "")))))
