(ns support.registry
  (:require [clojure.set :as set]
            [clojure.test :refer [is testing]]))

(defn names
  [r]
  (set (if (map? r) (keys r) r)))

(defn covers
  [label registry table]
  (let [want (names registry) have (names table)]
    (testing label
      (is (= {:fehlt #{} :zu-viel #{}}
             {:fehlt (set/difference want have) :zu-viel (set/difference have want)})))))

(defn complete
  [registry required]
  (doseq [[k entry] registry]
    (testing k
      (is (= #{} (set (remove #(some? (get entry %)) required)))))))
