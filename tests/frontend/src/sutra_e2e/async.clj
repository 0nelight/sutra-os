(ns sutra-e2e.async)

(defmacro plet
  [bindings & body]
  (if (empty? bindings)
    `(js/Promise.resolve (do ~@body))
    (let [[sym expr & more] bindings]
      `(.then (js/Promise.resolve ~expr) (fn [~sym] (plet ~(vec more) ~@body))))))
