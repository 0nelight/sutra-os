(ns sutra.foundation.media-type
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- starts? [^bytes bs & prefix]
  (and (>= (alength bs) (count prefix))
       (every? true? (map-indexed (fn [i b] (= (unchecked-byte b) (aget bs i))) prefix))))

(defn sniff
  [^bytes bs]
  (cond
    (starts? bs 0x25 0x50 0x44 0x46)                 :pdf
    (starts? bs 0x89 0x50 0x4E 0x47)                 "image/png"
    (starts? bs 0xFF 0xD8 0xFF)                      "image/jpeg"
    (starts? bs 0x47 0x49 0x46 0x38)                 "image/gif"
    (and (starts? bs 0x52 0x49 0x46 0x46) (>= (alength bs) 12)
         (= "WEBP" (String. bs 8 4 "US-ASCII")))     "image/webp"
    :else (let [dec (.newDecoder java.nio.charset.StandardCharsets/UTF_8)]
            (try (.decode dec (java.nio.ByteBuffer/wrap bs)) :text
                 (catch java.nio.charset.CharacterCodingException _ nil)))))

(defn essence
  [mime]
  (some-> mime str (str/split #";" 2) first str/trim str/lower-case not-empty))

(def ^:private inline-types
  #{"image/png" "image/jpeg" "image/gif" "image/webp" "image/avif" "image/bmp"
    "application/pdf" "text/plain"})

(defn inline-safe?
  [mime]
  (let [m (essence mime)]
    (boolean (or (contains? inline-types m)
                 (some-> m (str/starts-with? "audio/"))
                 (some-> m (str/starts-with? "video/"))))))

(def ^:private magic-types
  #{"image/png" "image/jpeg" "image/gif" "image/webp" "application/pdf"})

(defn checked
  [claimed ^bytes bs]
  (let [k (sniff (java.util.Arrays/copyOf bs (int (min 12 (alength bs)))))]
    (cond
      (= :pdf k)                                "application/pdf"
      (string? k)                               k
      (contains? magic-types (essence claimed)) "application/octet-stream"
      :else                                     claimed)))
