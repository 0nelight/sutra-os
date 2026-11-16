(ns sutra.sources.api
  (:refer-clojure :exclude [read])
  (:require [sutra.sources.egress-policies :as egress-policies]
            [sutra.sources.datasources :as ds]
            [sutra.sources.dialect :as dialect]
            [sutra.sources.policy :as policy]
            [sutra.sources.protocols :as protocols]
            [sutra.sources.writeback-dataset :as wb]
            [sutra.types.source :as source]))

(set! *warn-on-reflection* true)

(defn assert-source-write-cleared! [auth spec] (ds/assert-source-write-cleared! auth spec))

(defn end-write-session! [session commit?] (ds/end-write-session! session commit?))

(defn held-log [session] (ds/held-log session))

(defn egress-policies [store] (egress-policies/all store))

(defn egress-effective [policies entries] (egress-policies/effective policies entries))

(defn proxy-agents [policies entry] (egress-policies/proxy-agents policies entry))

(defn connection [spec] (policy/connection spec))

(defn assert-egress-permitted! [store spec] (egress-policies/assert-permitted! store spec))

(defn invoke-webhook!
  ([spec webhook-id params] (ds/invoke-webhook! spec webhook-id params))
  ([spec webhook-id params opts] (ds/invoke-webhook! spec webhook-id params opts)))

(defn invoke!
  ([s op params] (protocols/invoke! s op params))
  ([s op params opts] (protocols/invoke! s op params opts)))

(defn listen! [spec req] (ds/listen! spec req))

(defn write-session [] (ds/write-session))

(defn assert-export-allowed! [spec] (policy/assert-export-allowed! spec))

(defn spec-health [spec] (policy/spec-health spec))

(defn speaks-sql? [s] (protocols/speaks-sql? s))

(defn columns [s table] (protocols/columns s table))

(def max-rows protocols/max-rows)

(defmacro with-source [& args] `(protocols/with-source ~@args))

(defn confirm!
  ([schema obj-kw obj-id] (wb/confirm! schema obj-kw obj-id))
  ([schema obj-kw obj-id opts] (wb/confirm! schema obj-kw obj-id opts)))

(defn preview
  ([schema obj-kw obj-id] (wb/preview schema obj-kw obj-id))
  ([schema obj-kw obj-id opts] (wb/preview schema obj-kw obj-id opts)))

(defn read
  ([s sql] (protocols/read s sql))
  ([s sql params] (protocols/read s sql params)))

(defn scan
  ([s table] (protocols/scan s table))
  ([s table sc] (protocols/scan s table sc)))

(defn entries
  ([s table] (protocols/entries s table))
  ([s table sc] (protocols/entries s table sc))
  ([s table sc key] (protocols/entries s table sc key)))

(defn token [s table] (protocols/token s table))

(defn close! [s] (protocols/close! s))

(defn capable? [spec c] (policy/capable? spec c))

(defn file-source? [spec] (source/file-source? spec))

(defn one-file? [spec] (source/one-file? spec))

(defn scan-key [spec table] (policy/scan-key spec table))

(defn agent-routed? [spec] (policy/agent-routed? spec))

(defn runtime-health [spec agent-up?] (policy/runtime-health spec agent-up?))

(defn probe-sql [type table column] (dialect/probe-sql type table column))

(defn cdc-capable? [type] (dialect/cdc-capable? type))

(defn cdc-instance [cdc] (dialect/cdc-instance cdc))

(defn cdc-bounds-stmt [instance] (dialect/cdc-bounds-stmt instance))

(defn cdc-snapshot-stmt [cdc cols deletion] (dialect/cdc-snapshot-stmt cdc cols deletion))

(defn cdc-changes-stmt
  [instance cols deletion from to]
  (dialect/cdc-changes-stmt instance cols deletion from to))
