(ns sutra.store.storage.pool
  (:require [next.jdbc.prepare :as prepare]
            [sutra.store.storage.pg :as pg])
  (:import (java.sql PreparedStatement)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(extend-protocol prepare/SettableParameter
  UUID
  (set-parameter [^UUID v ^PreparedStatement stmt ^long i]
    (.setString stmt i (.toString v))))

(def module
  {:name :pg-ds :opens {:pg/ds (fn [config _] (pg/pools (pg/settings config)))}})
