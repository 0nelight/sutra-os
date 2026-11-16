(ns sutra.foundation.contracts.a
  (:require #?@(:clj [[clojure.edn :as edn] [clojure.java.io :as io]])))

(def schemas
  #?(:clj  (edn/read-string (slurp (io/resource "sutra/foundation/contracts/a.edn")))
     :cljs {}))
