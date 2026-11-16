(ns sutra.types.kinds
  (:require [clojure.set :as set]
            [sutra.types.entities :as ents]
            [sutra.types.table :as tbl]))

#?(:clj (set! *warn-on-reflection* true))

(def kinds
  {:object-type    {:source-input? true  :compute-input? true  :source-rows? false
                    :output? true  :ddl :object
                    :whole-rows? true :ends-chain? true :typed-output? true :watchable? true
                    :freshness :clock
                    :input-ref {:at :object-type :names :object-types}
                    :output-ref {:at :object-type :names :object-types}
                    :physical :landed :link-backing :endpoint-fk}
   :link-type      {:source-input? false :compute-input? false :source-rows? false
                    :output? true  :ddl :link :typed-output? true
                    :output-ref {:at :link-type :names :link-types}}
   :dataset        {:source-input? true  :compute-input? true  :source-rows? true
                    :output? true  :ddl :dataset
                    :whole-rows? true :ends-chain? true :watchable? true
                    :input-ref {:at :dataset :names :datasets}
                    :output-ref {:at :dataset :names :datasets}
                    :added-read? true :physical :dataset-table}
   :media-set      {:source-input? false :compute-input? true  :source-rows? true
                    :output? true :ddl nil
                    :input-ref {:at :dataset :names :datasets}
                    :output-ref {:at :media-set :names :media-sets}
                    :added-read? true :lineage-node :dataset :physical :dataset-table}
   :media-set-items {:source-input? false :compute-input? true :source-rows? false
                     :output? false :ddl nil :freshness :items
                     :input-ref {:at :media-set :names :media-sets}}
   :media          {:source-input? false :compute-input? true  :source-rows? false
                    :output? false :ddl nil :call-input? true
                    :input-ref {:at :dataset :names :datasets}}
   :language-model {:source-input? true  :compute-input? true  :source-rows? false
                    :output? false :ddl nil :passed-as-is? true
                    :input-ref {:at :model :shape :model}}
   :transform      {:source-input? true  :compute-input? true  :source-rows? true
                    :output? false :ddl nil :whole-rows? true
                    :input-ref {:at :transform :names :transforms} :physical :transform-table
                    :link-backing :join-table}
   :datasource     {:source-input? true  :compute-input? true  :source-rows? true
                    :output? false :ddl nil :ends-chain? true
                    :input-ref {:at :datasource :shape :keyword} :lineage-node :source
                    :physical :staged :link-backing :join-table}
   :export         {:source-input? false :compute-input? false :source-rows? false
                    :output? true  :ddl nil
                    :output-ref {:at :export :names :exports}}
   :scalar         {:source-input? false :compute-input? true  :source-rows? false
                    :output? false :ddl nil :valueless-input? true
                    :call-input? true :passed-as-is? true}})

(def facets
  #{:whole-rows? :ends-chain? :typed-output? :watchable? :call-input? :passed-as-is?})

(defn is?
  [facet k]
  (boolean (get-in kinds [(keyword (if (keyword? k) (name k) (str k))) facet])))

(def output-ref-keys
  (into #{} (keep (comp :at :output-ref val)) kinds))

(def source-input-kinds
  (tbl/where kinds :source-input?))

(def compute-input-kinds
  (tbl/where kinds :compute-input?))

(def output-kinds
  (tbl/where kinds :output?))

(def source-rows-kinds
  (tbl/where kinds :source-rows?))

(def valueless-input-kinds
  (tbl/where kinds :valueless-input?))

(defn valueless-input? [k] (contains? valueless-input-kinds k))
(defn source-input? [k] (contains? source-input-kinds k))
(defn compute-input? [k] (contains? compute-input-kinds k))
(defn output? [k] (contains? output-kinds k))
(defn source-rows? [k] (contains? source-rows-kinds k))

(defn physical
  [k]
  (get-in kinds [k :physical]))

(defn link-backing
  [k]
  (get-in kinds [k :link-backing]))

(defn freshness
  [k]
  (get-in kinds [k :freshness]))

(defn ddl-family
  [k]
  (get-in kinds [k :ddl]))

(defn input-ref
  [k]
  (get-in kinds [k :input-ref]))

(defn input-ref-val
  [in]
  (when-let [{:keys [at]} (input-ref (:kind in))]
    (get in at)))

(defn input-names-section
  [in]
  (:names (input-ref (:kind in))))

(defn- target
  [ref-key m]
  (let [{:keys [at names]} (get-in kinds [(:kind m) ref-key])]
    (when-some [v (and names (get m at))]
      [names v])))

(defn input-target
  [in]
  (target :input-ref in))

(defn output-target
  [out]
  (target :output-ref out))

(defn output-slot
  [out]
  (let [{:keys [at names]} (get-in kinds [(:kind out) :output-ref])]
    (when names [names (get out at)])))

(defn lineage-node
  [k]
  (get-in kinds [k :lineage-node] k))

(def added-read-kinds
  (tbl/where kinds :added-read?))

(defn added-read-kind?
  [k]
  (contains? added-read-kinds k))

(tbl/holds! "an input kind that carries a value says where its ref lives; a valueless one does not"
            (let [want (set/difference (into source-input-kinds compute-input-kinds)
                                       valueless-input-kinds)
                  have (tbl/where kinds :input-ref)]
              (concat (set/difference want have) (set/difference have want))))

(tbl/holds! "an input ref names a RID section"
            (for [[k {{s :names} :input-ref}] kinds :when (and s (not (ents/rid-sections s)))]
              [k s]))

(tbl/holds! "only a compute input takes the added read"
            (set/difference added-read-kinds compute-input-kinds))
