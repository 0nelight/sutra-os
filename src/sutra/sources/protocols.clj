(ns sutra.sources.protocols
  (:refer-clojure :exclude [read])
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.read-only-sql :as read-only]
            [sutra.sources.policy :as policy]
            [sutra.store.scan :as scan]))

(set! *warn-on-reflection* true)

(defprotocol Source
  (-profile [s])
  (-probe [s])
  (-close [s]))

(defprotocol Catalog
  (-tables [s])
  (-columns [s table])
  (-relations [s]))

(defprotocol Scan
  (-scan-profile [s table])
  (-scan [s table scan]))

(defprotocol Sql
  (-select [s sql params limit]))

(defprotocol Land
  (-reach! [s]))

(defprotocol Write
  (-write! [s write-spec]))

(defprotocol Invoke
  (-invoke! [s webhook-id params opts]))

(defprotocol Token
  (-token [s table]))

(defn unsupported [op spec]
  (refusal/refuse! :refusal.foreign/unsupported {:unsupported op :type (:type spec)}
                   {:op (name op) :type (some-> (:type spec) name)}))

(defn- spec-of [s] (or (:spec s) {}))

(defmacro ^:private via
  [p s op & body]
  `(if (satisfies? ~p ~s) (do ~@body) (unsupported ~op (spec-of ~s))))

(def max-rows 1000)

(defn- clamp [n default] (max 1 (min max-rows (or n default))))

(defn tables [s] (via Catalog s :tables (-tables s)))

(defn columns [s table] (via Catalog s :columns (-columns s table)))

(defn relations [s] (if (satisfies? Catalog s) (-relations s) []))

(defn offer
  [p s]
  (scan/offer (cond-> p (some? (:after s)) (assoc :page? false)) s))

(defn- check-scan! [{:keys [after order agg] :as s} key]
  (when agg
    (refusal/refuse! :refusal.sources/foreign-scan-agg {:scan s}))
  (when (and (nil? key) (or (some? after) (seq order)))
    (refusal/refuse! :refusal.sources/foreign-scan-keyless {:scan s}))
  s)

(defn- indexed [rows] (map-indexed vector rows))

(defn run-local
  [rows s]
  (mapv second (scan/run (indexed rows) (dissoc s :page))))

(defn- keyed [key rows]
  (if key (mapv (fn [r] [(policy/row-key key r) r]) rows) (vec (indexed rows))))

(defn- scanned
  [s table sc need-key? declared]
  (let [table (if (:paths? (-profile s)) table (policy/safe-ident table))
        key   (or (seq declared) (policy/scan-key (spec-of s) table))
        sc    (into {} (remove (comp nil? val)) sc)]
    (when (and need-key? (nil? key))
      (refusal/refuse! :refusal.sources/table-keyless {:table table}))
    (check-scan! sc key)
    (via Scan s :scan
         (let [o   (offer (-scan-profile s table) sc)
               p   (cond-> (scan/pushed sc o) (and key (:project o)) (update :project into key))
               got (keyed key (-scan s table p))
               r   (scan/residual sc o)]
           (cond->> (if (and (= [:where] (keys r)) (empty? (:where r))) got (scan/run got r))
             (:project sc) (mapv (fn [[k v]] [k (select-keys v (:project sc))])))))))

(defn scan
  ([s table] (scan s table {}))
  ([s table sc] (mapv second (scanned s table sc false nil))))

(defn entries
  ([s table] (entries s table {}))
  ([s table sc] (scanned s table sc true nil))
  ([s table sc key] (scanned s table sc true key)))

(defn read
  ([s sql] (read s sql nil))
  ([s sql params] (via Sql s :select (-select s (read-only/assert-read-only-sql! sql) params nil))))

(defn query
  [s sql limit]
  (via Sql s :select (-select s (read-only/assert-read-only-sql! sql) nil (clamp limit 50))))

(defn sample [s table limit] (scan s table {:limit (clamp limit 20)}))

(defn probe
  [s]
  (-probe s)
  true)

(defn token? [s] (satisfies? Token s))

(defn token [s table] (via Token s :token (-token s (policy/safe-ident table))))

(defn reach! [s] (via Land s :reach! (-reach! s)))

(defn write! [s write-spec] (via Write s :write! (-write! s write-spec)))

(defn invoke!
  ([s webhook-id params] (invoke! s webhook-id params nil))
  ([s webhook-id params opts] (via Invoke s :invoke! (-invoke! s webhook-id params opts))))

(defn close! [s] (-close s))

(defmacro with-source
  [[sym connect spec] & body]
  `(let [s# (~connect ~spec)]
     (try (let [~sym s#] ~@body)
          (finally (close! s#)))))

(defn speaks-sql? [s] (satisfies? Sql s))
