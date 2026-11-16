(ns sutra.catalog.outputs
  (:require [sutra.types.column :as column]
            [sutra.types.schema :as sch]
            [sutra.types.kinds :as kinds]
            [sutra.types.transform-io :as tio]
            [sutra.parser.spec :as spec]
            [sutra.parser.board :as board]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.actions :as actions]))

(set! *warn-on-reflection* true)

(def card-limits
  {:one-to-one [1 1] :one-to-many [nil 1] :many-to-one [1 nil] :many-to-many [nil nil]})

(def valid-cardinalities-all (set (rest spec/Cardinality)))

(def outputs-of
  tio/outputs-of)

(def producer-index
  (comp :producers sch/index))

(defn outputs-for
  [index kind target-kw]
  (get-in index [:by-output [kind target-kw]] []))

(defn find-outputs-for
  [schema kind target-kw]
  (sch/outputs-for (sch/index schema) kind target-kw))

(defn find-output-for
  [schema kind target-kw]
  (first (find-outputs-for schema kind target-kw)))

(defn- supplement-output?
  [[_ _ out]] (= :supplement (:role out)))

(defn primary-outputs
  [outs]
  (into [] (remove supplement-output?) outs))

(defn primary-outputs-for
  [schema kind target-kw]
  (primary-outputs (find-outputs-for schema kind target-kw)))

(defn primary-output-for
  [schema kind target-kw]
  (let [outs (find-outputs-for schema kind target-kw)]
    (or (first (remove supplement-output? outs)) (first outs))))

(defn dataset-producers
  [schema ds-rid]
  (sch/producers-of-dataset (sch/index schema) ds-rid))

(defn transform-input-key
  [schema input-entry]
  (let [trid (:transform input-entry)
        t    (get-in schema [:transforms trid])]
    (if (:output-columns t)
      trid
      (let [outs (outputs-of t)
            out  (if-let [on (:output input-entry)]
                   (some #(when (= on (:name %)) %) outs)
                   (first outs))]
        (or (:dataset out) trid)))))

(defn input-dataset-rid
  [schema in]
  (if (= :transform (:kind in))
    (transform-input-key schema in)
    (when-let [section (kinds/input-names-section in)]
      (lookup/rid-of schema section (kinds/input-ref-val in)))))

(defn board-input-cols
  [schema in-spec]
  (let [attrs #(mapv (fn [a] {:name (column/attr->col (:attribute-name a))
                              :type (:attribute-type a)})
                     %)]
    (case (kinds/physical (:kind in-spec))
      :landed
      (let [rid (lookup/rid-of schema :object-types (:object-type in-spec))]
        (attrs (:attribute-list (get-in schema [:object-types (or rid (:object-type in-spec))]))))
      :dataset-table
      (let [rid (lookup/rid-of schema :datasets (kinds/input-ref-val in-spec))]
        (attrs (:columns (get-in schema [:datasets (or rid (:dataset in-spec))]))))
      :transform-table
      (let [rid (lookup/rid-of schema :transforms (:transform in-spec))]
        (attrs (:output-columns (get-in schema [:transforms (or rid (:transform in-spec))]))))
      [])))

(defn recompile-board
  [schema body]
  (if-let [b (:board body)]
    (try
      (let [compiled (board/board->sql b (fn [in] (board-input-cols schema in)))
            out      (cond-> (assoc body :query (:query compiled))
                       (and (empty? (:inputs body)) (seq (:inputs b)))
                       (assoc :inputs (:inputs b))
                       (and (nil? (:output body)) (nil? (:outputs body)))
                       (assoc :output-columns (:output-columns compiled)))]
        (cond-> out
          (map? (:inputs out)) (update :inputs board/with-output-removals b)))
      (catch clojure.lang.ExceptionInfo _ body))
    body))
