(ns sutra.authoring.install.findings.exports
  (:require [sutra.kit.coerce :refer [->name-kw]]
            [sutra.types.export :as export]
            [sutra.authoring.install.findings :as fnd]))

(set! *warn-on-reflection* true)

(defn- allowlist-entry
  [src table]
  (let [wb (:writeback src)]
    (or (get wb table) (get wb (keyword table)) (get wb (str table)))))

(defn- by-own-name
  [m name-key]
  (into {} (map (fn [[k v]] [(->name-kw (or (name-key v) k)) v])) m))

(defn- export-finding
  [sources dsets [exp-kw spec]]
  (let [src     (get sources (->name-kw (:source spec)))
        ds      (get dsets (->name-kw (:dataset spec)))
        table   (get-in spec [:destination :table])
        mode    (or (:mode spec) export/default-mode)
        allow   (when src (allowlist-entry src table))
        leaking (when src (remove (set (:export-markings src)) (:markings ds)))
        rule    (cond
                  (nil? (export/mode-row mode))                :export-mode-unknown
                  (nil? src)                                   nil
                  (not= :read-write (->name-kw (:access src))) :export-source-read-only
                  (nil? allow)                                 :export-destination-not-allowlisted
                  (and (contains? export/truncating-modes mode)
                       (not (:truncatable? allow)))            :export-truncate-not-granted
                  (seq leaking)                                :export-markings-not-exportable)]
    (when rule
      {:rule rule :entity-class :exports :entity (or (:export-name spec) exp-kw)
       ::fnd/ctx {:source (some-> (:source spec) name) :table table :mode mode
                  :leaking (vec leaking)
                  :allowed (vec (keys (:writeback src)))}})))

(def exports
  {:find (fn [{:keys [text]}]
           (keep (partial export-finding
                          (by-own-name (:sources text) :source-name)
                          (by-own-name (:datasets text) :dataset-name))
                 (:exports text)))
   :says
   {:export-mode-unknown
    (fn [{nm :entity {:keys [mode]} ::fnd/ctx}]
      (str "Export " (name nm) " runs in mode " (pr-str mode) " — known: "
           (pr-str (vec (sort (keys export/modes))))
           ". These are the table-export modes; there is deliberately no upsert among"
           " them — a row-level change to a foreign system goes through a webhook."))

    :export-source-read-only
    (fn [{nm :entity {:keys [source]} ::fnd/ctx}]
      (str "Export " (name nm) " writes to source " source ", which is not :read-write."
           " An export is the source's own resource: it may only do what the source grants."))

    :export-destination-not-allowlisted
    (fn [{nm :entity {:keys [source table allowed]} ::fnd/ctx}]
      (str "Export " (name nm) " writes table " (pr-str table) " of source " source
           ", which is not on its :writeback allowlist (" (pr-str allowed) ")."
           " The destination must also already exist in the source system — an export"
           " never creates it."))

    :export-truncate-not-granted
    (fn [{nm :entity {:keys [source table mode]} ::fnd/ctx}]
      (str "Export " (name nm) " runs in mode " mode ", which clears the destination first,"
           " but " (pr-str table) " on source " source " is not :truncatable?."
           " That grant is its own: the per-row :deletable? names the row it removes and"
           " carries a guard, a truncate names none."))

    :export-markings-not-exportable
    (fn [{nm :entity {:keys [source leaking]} ::fnd/ctx}]
      (str "Export " (name nm) " would carry the markings " (pr-str leaking)
           " out to source " source ", which does not list them under :export-markings."
           " What leaves is judged by the markings of the DATA, not of the source alone."))}})
