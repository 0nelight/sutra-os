(ns sutra.types.protection
  (:require [clojure.string :as str]
            [sutra.kit.remote :as rd]))

(defn ref-state
  [protections ref]
  (rd/fold protections
           {:loading (constantly :loading)
            :absent  (constantly :failed)
            :failed  (constantly :failed)
            :value   (fn [{:keys [sections effective]}]
                       (cond
                         (not (contains? (set sections)
                                         (first (str/split (str ref) #"/" 2))))
                         :unprotectable
                         (contains? (set effective) ref) :protected
                         :else                           :open))}))

(defn trunk-view
  [protections]
  (rd/show protections
           {:value (fn [p] {:show (if (:trunk-protected p) :protected :open)})}))
