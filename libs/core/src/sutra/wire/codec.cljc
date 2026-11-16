(ns sutra.wire.codec
  (:require [cognitect.transit :as t]
            #?@(:cljs [[clojure.string :as str]
                       [sutra.types.decimal :as d]
                       [clojure.walk :as walk]
                       [goog.math.Long]]))
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

(def ^:private read-handlers
  #?(:cljs {"f" d/carry
            "n" d/carry}
     :clj  {}))

#?(:cljs
   (defn- exactify-longs
     [x]
     (walk/postwalk #(if (instance? goog.math.Long %) (d/carry (.toString %)) %) x)))

#?(:cljs
   (defn- big-int-tagged?
     [s]
     (str/includes? s "\"~i")))

(defn encode
  [v]
  #?(:clj  (let [out (ByteArrayOutputStream.)]
             (t/write (t/writer out :json) v)
             (.toByteArray out))
     :cljs (t/write (t/writer :json) v)))

(defn decode
  [bs-or-str]
  #?(:clj  (when (and bs-or-str (pos? (alength ^bytes bs-or-str)))
             (t/read (t/reader (ByteArrayInputStream. bs-or-str) :json
                               {:handlers read-handlers})))
     :cljs (when (seq bs-or-str)
             (let [v (t/read (t/reader :json {:handlers read-handlers}) bs-or-str)]
               (cond-> v (big-int-tagged? bs-or-str) exactify-longs)))))
