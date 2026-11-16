(ns sutra.types.object
  (:require [sutra.ref.value :as rv]))

(def binding-ref-ops (disj rv/binding-ref-ops :get))
