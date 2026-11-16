(ns conformance.json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn parse [s] (json/read-str s))

(defn parse-or-nil [s] (try (parse s) (catch Exception _ nil)))

(defn write [x] (json/write-str x))

(defn read-file [path] (with-open [r (io/reader path)] (json/read r)))
