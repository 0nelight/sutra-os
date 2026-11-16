(ns sutra.lib.dsl-kondo)

(defmacro defobject [sym & body] `(def ~sym [~@body]))

(defmacro deflink [sym & body] `(def ~sym [~@body]))

(defmacro defaction [sym & body]
  (let [[head [params & clauses]] (split-with (complement vector?) body)]
    `(def ~sym [~@head ~@(take-nth 2 (rest params)) ~@clauses])))

(defmacro defobjectset [sym & body] `(def ~sym [~@body]))
