(ns sutra.calx.spec
  (:require [clojure.spec.alpha :as s]
            [sutra.calx.value :as v]
            [sutra.calx.address :as addr]))

(set! *warn-on-reflection* true)

(s/def :calx/error v/error?)
(s/def :calx/scalar
  (s/nilable (s/or :number double? :text string? :bool boolean? :error :calx/error)))
(s/def :calx/array
  (s/and vector? (s/coll-of (s/coll-of :calx/scalar :kind vector? :min-count 1) :min-count 1)
         #(apply = (map count %))))
(s/def :calx/value (s/or :scalar :calx/scalar :array :calx/array))

(s/def :calx/sheet-id pos-int?)
(s/def :calx/row (s/int-in 0 addr/max-rows))
(s/def :calx/col (s/int-in 0 addr/max-cols))
(s/def :calx/key (s/tuple :calx/sheet-id :calx/row :calx/col))
(s/def :calx.area/r1 :calx/row)
(s/def :calx.area/r2 :calx/row)
(s/def :calx.area/c1 :calx/col)
(s/def :calx.area/c2 :calx/col)
(s/def :calx/area (s/and (s/keys :req-un [:calx.area/r1 :calx.area/c1 :calx.area/r2 :calx.area/c2])
                         #(<= (:r1 %) (:r2 %)) #(<= (:c1 %) (:c2 %))))

(defmulti ast-node :kind)
(s/def :calx/ast (s/multi-spec ast-node :kind))

(s/def :calx.ast/kind #{:lit :ref :name :call :invoke :op :array :spill :paren :missing})
(s/def :calx.ast/args (s/coll-of :calx/ast :kind vector?))
(s/def :calx.ast/fn (s/or :name string? :node :calx/ast))
(s/def :calx.ast/op
  #{:add :sub :mul :div :pow :concat :eq :ne :lt :gt :le :ge :range :neg :pos :pct})
(s/def :calx.ast/shape #{:cell :area :cols :rows})
(s/def :calx.ast/abs (s/tuple boolean? boolean? boolean? boolean?))
(s/def :calx.ast/sheet (s/nilable :calx/sheet-id))
(s/def :calx.ast/sheet-name string?)
(s/def :calx.ast/name string?)
(s/def :calx.ast/value :calx/scalar)
(s/def :calx.ast/rows (s/coll-of (s/coll-of :calx/ast :kind vector?) :kind vector?))
(s/def :calx.ast/expr :calx/ast)
(s/def :calx.ast/ref :calx/ast)
(s/def :calx.ast/r1 (s/nilable :calx/row))
(s/def :calx.ast/r2 (s/nilable :calx/row))
(s/def :calx.ast/c1 (s/nilable :calx/col))
(s/def :calx.ast/c2 (s/nilable :calx/col))

(defmethod ast-node :lit [_] (s/keys :req-un [:calx.ast/kind :calx.ast/value]))
(defmethod ast-node :ref [_] (s/keys :req-un [:calx.ast/kind :calx.ast/shape :calx.ast/abs
                                              :calx.ast/r1 :calx.ast/c1 :calx.ast/r2 :calx.ast/c2]
                                     :opt-un [:calx.ast/sheet :calx.ast/sheet-name]))
(defmethod ast-node :name [_]
  (s/keys :req-un [:calx.ast/kind :calx.ast/name] :opt-un [:calx.ast/sheet :calx.ast/sheet-name]))
(defmethod ast-node :call [_] (s/keys :req-un [:calx.ast/kind :calx.ast/fn :calx.ast/args]))
(defmethod ast-node :invoke [_] (s/keys :req-un [:calx.ast/kind :calx.ast/fn :calx.ast/args]))
(defmethod ast-node :op [_] (s/keys :req-un [:calx.ast/kind :calx.ast/op :calx.ast/args]))
(defmethod ast-node :array [_] (s/keys :req-un [:calx.ast/kind :calx.ast/rows]))
(defmethod ast-node :spill [_] (s/keys :req-un [:calx.ast/kind :calx.ast/ref]))
(defmethod ast-node :paren [_] (s/keys :req-un [:calx.ast/kind :calx.ast/expr]))
(defmethod ast-node :missing [_] (s/keys :req-un [:calx.ast/kind]))

(s/def :calx.cell/ast :calx/ast)
(s/def :calx.cell/value (s/nilable (s/or :scalar :calx/scalar :array :calx/array)))
(s/def :calx.cell/invalid string?)
(s/def :calx.cell/style (s/nilable map?))
(s/def :calx/cell
  (s/keys :opt-un [:calx.cell/ast :calx.cell/value :calx.cell/invalid :calx.cell/style]))

(s/def :calx.sheet/id :calx/sheet-id)
(s/def :calx.sheet/name (s/and string? #(<= 1 (count %) 31)))
(s/def :calx.sheet/cells (s/map-of (s/tuple :calx/row :calx/col) :calx/cell))
(s/def :calx.sheet/extent (s/tuple nat-int? nat-int?))
(s/def :calx.sheet/rows (s/map-of :calx/row map?))
(s/def :calx.sheet/cols (s/map-of :calx/col map?))
(s/def :calx/sheet
  (s/keys :req-un [:calx.sheet/id :calx.sheet/name :calx.sheet/cells :calx.sheet/extent]
          :opt-un [:calx.sheet/rows :calx.sheet/cols]))

(s/def :calx.wb/sheets (s/map-of :calx/sheet-id :calx/sheet))
(s/def :calx.wb/order (s/coll-of :calx/sheet-id :kind vector? :distinct true :min-count 1))
(s/def :calx.wb/next-id pos-int?)
(s/def :calx.name/name string?)
(s/def :calx.name/scope (s/nilable :calx/sheet-id))
(s/def :calx.name/ast :calx/ast)
(s/def :calx.wb/names (s/map-of (s/tuple (s/nilable :calx/sheet-id) string?)
                                (s/keys :req-un [:calx.name/name :calx.name/scope :calx.name/ast])))
(s/def :calx.wb/functions (s/map-of string? map?))
(s/def :calx.wb/config map?)
(s/def :calx.wb/spills (s/map-of :calx/key (s/tuple pos-int? pos-int?)))
(s/def :calx.wb/blocked (s/map-of :calx/key (s/tuple pos-int? pos-int?)))
(s/def :calx.wb/owner (s/map-of :calx/key :calx/key))
(s/def :calx.wb/log vector?)

(s/def :calx/workbook
  (s/and (s/keys :req-un [:calx.wb/sheets :calx.wb/order :calx.wb/next-id :calx.wb/names
                          :calx.wb/functions :calx.wb/config :calx.wb/spills :calx.wb/owner
                          :calx.wb/blocked])
         #(= (set (:order %)) (set (keys (:sheets %))))
         #(every? (fn [[id sh]] (= id (:id sh))) (:sheets %))))

(s/def :calx/sheet-ref (s/or :id :calx/sheet-id :name string?))
(s/def :calx/key-ref (s/or :key (s/tuple :calx/sheet-ref :calx/row :calx/col) :address string?))
(s/def :calx/input
  (s/nilable (s/or :text string? :number number? :bool boolean? :error :calx/error)))

(defmulti op-spec first)
(defmethod op-spec :set-cell [_] (s/cat :op #{:set-cell} :key :calx/key-ref :input :calx/input))
(defmethod op-spec :set-value [_] (s/cat :op #{:set-value} :key :calx/key-ref :value :calx/input))
(defmethod op-spec :set-style [_]
  (s/cat :op #{:set-style} :key :calx/key-ref :style (s/nilable map?)))
(defmethod op-spec :clear [_] (s/cat :op #{:clear} :sheet :calx/sheet-ref :area :calx/area))
(defmethod op-spec :add-sheet [_] (s/cat :op #{:add-sheet} :name :calx.sheet/name))
(defmethod op-spec :remove-sheet [_] (s/cat :op #{:remove-sheet} :sheet :calx/sheet-ref))
(defmethod op-spec :rename-sheet [_]
  (s/cat :op #{:rename-sheet} :sheet :calx/sheet-ref :name :calx.sheet/name))
(defmethod op-spec :move-sheet [_]
  (s/cat :op #{:move-sheet} :sheet :calx/sheet-ref :index nat-int?))
(doseq [op [:insert-rows :delete-rows :insert-cols :delete-cols]]
  (defmethod op-spec op [_]
    (s/cat :op keyword? :sheet :calx/sheet-ref :at nat-int? :n (s/? pos-int?))))
(defmethod op-spec :move [_]
  (s/cat :op #{:move} :sheet :calx/sheet-ref :area :calx/area :to :calx/sheet-ref :row :calx/row
         :col :calx/col))
(defmethod op-spec :copy [_]
  (s/cat :op #{:copy} :sheet :calx/sheet-ref :area :calx/area :to :calx/sheet-ref :row :calx/row
         :col :calx/col))
(defmethod op-spec :fill [_]
  (s/cat :op #{:fill} :sheet :calx/sheet-ref :source :calx/area :target :calx/area))
(s/def :calx.sort/col :calx/col)
(s/def :calx.sort/ascending? boolean?)
(defmethod op-spec :sort [_]
  (s/cat :op #{:sort} :sheet :calx/sheet-ref :area :calx/area
         :keys
         (s/nilable (s/coll-of (s/keys :req-un [:calx.sort/col] :opt-un [:calx.sort/ascending?])))))
(defmethod op-spec :define-name [_]
  (s/cat :op #{:define-name} :name string? :input :calx/input :scope
         (s/? (s/nilable :calx/sheet-ref))))
(defmethod op-spec :remove-name [_]
  (s/cat :op #{:remove-name} :name string? :scope (s/? (s/nilable :calx/sheet-ref))))
(defmethod op-spec :set-row-meta [_]
  (s/cat :op #{:set-row-meta} :sheet :calx/sheet-ref :row :calx/row :meta map?))
(defmethod op-spec :set-col-meta [_]
  (s/cat :op #{:set-col-meta} :sheet :calx/sheet-ref :col :calx/col :meta map?))
(s/def :calx/op (s/and vector? (s/multi-spec op-spec (fn [v _] v))))
(s/def :calx/batch (s/coll-of :calx/op :kind vector?))
(s/def :calx/log (s/coll-of (s/or :batch :calx/batch :undo #{:undo} :redo #{:redo}) :kind vector?))

(s/def :calx.doc/formula string?)
(s/def :calx.doc/cell (s/or :formula (s/keys :req-un [:calx.doc/formula]) :value :calx/scalar))
(s/def :calx.doc/cells (s/map-of string? :calx.doc/cell))
(s/def :calx.doc/styles (s/map-of string? map?))
(s/def :calx.doc/sheet
  (s/keys :req-un [:calx.sheet/name :calx.doc/cells] :opt-un
          [:calx.doc/styles :calx.sheet/rows :calx.sheet/cols]))
(s/def :calx.doc/sheets (s/coll-of :calx.doc/sheet :kind vector? :min-count 1))
(s/def :calx.doc.name/scope (s/nilable string?))
(s/def :calx.doc/names
  (s/coll-of (s/keys :req-un [:calx.name/name :calx.doc.name/scope :calx.doc/formula]) :kind
             vector?))
(s/def :calx/format #{1})
(s/def :calx/document
  (s/keys :req [:calx/format] :req-un [:calx.doc/sheets] :opt-un [:calx.doc/names :calx.wb/config]))
