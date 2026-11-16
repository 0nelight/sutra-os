(ns ent-lsp.jsonrpc
  (:require [cheshire.core :as json])
  (:import [java.io InputStream OutputStream ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]))

(defn- read-line-ascii [^InputStream in]
  (let [buf (ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (cond (= -1 b) (when (pos? (.size buf)) (.toString buf "US-ASCII"))
              (= 10 b) (let [s (.toString buf "US-ASCII")]
                         (if (.endsWith s "\r") (subs s 0 (dec (count s))) s))
              :else (do (.write buf b) (recur)))))))

(defn read-message
  [^InputStream in]
  (loop [headers {}]
    (when-let [line (read-line-ascii in)]
      (if (= "" line)
        (when-let [n (some-> (get headers "content-length") parse-long)]
          (let [bytes (.readNBytes in (int n))]
            (json/parse-string (String. bytes StandardCharsets/UTF_8) true)))
        (let [[k v] (.split ^String line ":" 2)]
          (recur (assoc headers (.toLowerCase (.trim ^String k)) (some-> v .trim))))))))

(def ^:private locks (java.util.WeakHashMap.))

(defn- lock-of
  [out]
  (locking locks
    (or (.get locks out) (let [l (Object.)] (.put locks out l) l))))

(defn write-message [^OutputStream out msg]
  (let [body (.getBytes ^String (json/generate-string (assoc msg :jsonrpc "2.0"))
                        StandardCharsets/UTF_8)
        head (.getBytes (str "Content-Length: " (alength body) "\r\n\r\n")
                        StandardCharsets/US_ASCII)
        lock (lock-of out)]
    (locking lock
      (.write out head)
      (.write out body)
      (.flush out))))
