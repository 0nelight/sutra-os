(ns sutra.solo.port
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io File)
           (java.net InetAddress ServerSocket)))

(def range-start 18030)
(def range-size 1000)

(defn derived
  [^File dir]
  (+ range-start (Math/floorMod (.hashCode (.getCanonicalPath dir)) (int range-size))))

(defn free?
  [port]
  (try (with-open [_ (ServerSocket. (int port) 1 (InetAddress/getByName "127.0.0.1"))] true)
       (catch Exception _ false)))

(defn instance-file ^File [^File dir] (io/file dir "instance.edn"))

(defn stored
  [^File dir]
  (let [f (instance-file dir)]
    (when (.exists f)
      (try (:http-port (edn/read-string (slurp f))) (catch Exception _ nil)))))

(defn choose
  [^File dir {:keys [env-port stored free?] :or {free? free?}}]
  (cond
    env-port {:port (parse-long (str env-port)) :via :env}
    stored   {:port stored :via :stored}
    :else    (let [d (derived dir)]
               {:port (->> (range range-size)
                           (map #(+ range-start (mod (+ (- d range-start) %) range-size)))
                           (filter free?)
                           first)
                :via :derived})))

(defn persist!
  [^File dir port]
  (let [f   (instance-file dir)
        cur (or (try (when (.exists f) (edn/read-string (slurp f))) (catch Exception _ nil)) {})]
    (spit f (pr-str (assoc cur :http-port port)))))
