(ns sutra.kit.attempt
  (:require [sutra.kit.err :as err]))

(set! *warn-on-reflection* true)

(defn interruption?
  [t]
  (boolean (err/cause #(or (instance? InterruptedException %)
                           (= "java.nio.channels.ClosedByInterruptException"
                              (.getName (class %))))
                      t)))

(defn rethrow-fatal!
  [t]
  (cond (interruption? t)                 (do (.interrupt (Thread/currentThread)) (throw t))
        (instance? VirtualMachineError t) (throw t))
  nil)

(defmacro or-answer
  [alt & body]
  `(try ~@body
        (catch Throwable t#
          (rethrow-fatal! t#)
          ~alt)))

(defmacro silently
  [& body]
  `(or-answer nil ~@body))
