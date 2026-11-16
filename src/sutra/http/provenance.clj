(ns sutra.http.provenance
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.http.security-log :as seclog]
            [sutra.kit.anomaly :as anomaly]
            [sutra.types.feedback :as b])
  (:import (java.io File)
           (java.net URLEncoder)))

(set! *warn-on-reflection* true)

(defn asked?
  [req]
  (boolean (:herkunft? (seclog/seen req))))

(def ^:private header-max 3000)
(def frames-max 8)
(def ^:private text-max 120)

(def ^:private framework
  #"^sutra\.(serve|http|kit)\.")

(defn repo-path
  [classpath]
  (let [u    (io/resource classpath)
        root (str (System/getProperty "user.dir") File/separator)]
    (if (and u (= "file" (.getProtocol u)) (str/starts-with? (.getPath u) root))
      (subs (.getPath u) (count root))
      classpath)))

(defn- frame [^StackTraceElement f]
  (let [[ns-name fn-name] (str/split (.getClassName f) #"\$" 3)
        file (.getFileName f)
        line (.getLineNumber f)]
    (when (and file (pos? line) (str/starts-with? ns-name "sutra.")
               (not (re-find framework ns-name)))
      (let [dir (-> ns-name (str/replace "." "/") (str/replace #"/[^/]+$" ""))]
        (cond-> {:datei (repo-path (str dir "/" file)) :zeile line}
          fn-name (assoc :fn (clojure.lang.Compiler/demunge fn-name)))))))

(defn frames
  [^Throwable t]
  (->> (take-while some? (iterate #(.getCause ^Throwable %) t))
       reverse
       (mapcat #(keep frame (.getStackTrace ^Throwable %)))
       (reduce (fn [acc s]
                 (if (some #(= (select-keys s [:datei :fn]) (select-keys % [:datei :fn])) acc)
                   acc
                   (conj acc s)))
               [])
       (take frames-max)
       vec))

(defn- name-of [x]
  (when (some? x) (if (keyword? x) (name x) (str x))))

(defn- url-encoded
  [^String s]
  (str/replace (URLEncoder/encode s "UTF-8") "+" "%20"))

(defn- thrown
  ^Throwable [x]
  (if (instance? Throwable x) x (anomaly/cause x)))

(defn header
  [x]
  (let [t    (thrown x)
        d    (if (instance? Throwable x) (ex-data x) (anomaly/data x))
        text (if (instance? Throwable x) (ex-message x) (anomaly/message x))
        form (cond-> {:server (if t (frames t) [])}
               (:action d)  (assoc :aktion (name-of (:action d)))
               (seq (:missing d)) (assoc :fehlt (mapv name-of (:missing d)))
               (:ref/kind d) (assoc :ref-art (name-of (:ref/kind d)))
               (seq (::sent d)) (assoc :gesendet (::sent d))
               text (assoc :text (subs text 0 (min (count text) text-max))))]
    (or (some #(let [raw (url-encoded (json/write-str %))] (when (< (count raw) header-max) raw))
              [form (dissoc form :text)])
        (url-encoded (json/write-str (dissoc form :text :gesendet))))))

(defn with-sent
  [req r param-specs args]
  (if (and (asked? req) (anomaly/refusal? r) (map? args))
    (assoc r ::sent (b/gesendet (param-specs) args))
    r))

(defn without-sent [d] (dissoc d ::sent))

(defn with-origin
  [req res x]
  (if (and (asked? req) x (map? res))
    (try (assoc-in res [:headers "X-Sutra-Herkunft"] (header x))
         (catch Exception e
           (println "[feedback] origin not attached:" (ex-message e))
           res))
    res))
