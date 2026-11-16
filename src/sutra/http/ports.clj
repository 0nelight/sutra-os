(ns sutra.http.ports
  (:require [sutra.foundation.ops :as op]))

(set! *warn-on-reflection* true)

(defn ended-branches [store] (op/query {:store store} :branch/ended {}))
