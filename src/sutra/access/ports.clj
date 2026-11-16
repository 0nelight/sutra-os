(ns sutra.access.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn auth-app-guard [store] (sys/provided (sys/of store) :auth/app-guard))

(defn install-world! [store branch world opts]
  ((sys/required (sys/of store) :world/install) store branch world opts))

(defn identity-providers [store] (sys/table (sys/of store) :identity/providers))

(defn throttle-buckets [store] (sys/required (sys/of store) :access/throttle))
