(ns sutra.capps.warn)

(defn warn! [s]
  #?(:cljs (js/console.warn s)
     :clj  (binding [*out* *err*] (println s))))

(defonce ^:private warned (atom #{}))

(defn warn-once!
  [k s]
  (when-not (contains? @warned k)
    (swap! warned conj k)
    (warn! s)))
