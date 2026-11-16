(ns sutra-e2e.async
  (:require-macros [sutra-e2e.async]))

(defn fang
  [p f]
  (.catch (js/Promise.resolve p) f))

(defn nacheinander
  [xs f halt?]
  (let [go (fn go [xs acc]
             (if-let [[x & more] (seq xs)]
               (.then (js/Promise.resolve (f x))
                      (fn [r] (if (halt? r x) (conj acc r) (go more (conj acc r)))))
               (js/Promise.resolve acc)))]
    (go xs [])))
