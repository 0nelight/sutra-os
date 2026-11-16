(ns sutra.llm.media
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [sutra.foundation.media-type :as media-type]
            [clojure.string :as str]
            [sutra.blobs.api :as blobs]
            [sutra.types.run-limits :as limits])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.util Base64)
           (javax.imageio ImageIO)
           (org.apache.pdfbox Loader)
           (org.apache.pdfbox.pdmodel PDDocument)
           (org.apache.pdfbox.rendering ImageType PDFRenderer)
           (org.apache.pdfbox.text PDFTextStripper)))

(set! *warn-on-reflection* true)

(def modes #{:auto :text :image})

(defn- fail [msg data]
  (refusal/refuse! :refusal.llm/media-invalid data {:why msg}))

(defn hashes-of
  [v]
  (cond
    (nil? v)        []
    (string? v)     (if (str/blank? v) [] [(str/trim v)])
    (sequential? v) (vec (mapcat hashes-of v))
    (map? v)        (hashes-of (or (:hash v) (:media-reference v) (get v "hash")))
    :else           (fail (str "not a media reference: " (pr-str v)) {:value v})))

(defn- read-bytes ^bytes [store hash]
  (when-not (blobs/valid-address? hash)
    (fail (str "not a media reference (a sha256 content address): " (pr-str hash)) {:hash hash}))
  (let [^InputStream in (or (blobs/open store hash)
                            (fail (str "media " hash " is not in the blob store") {:hash hash}))]
    (with-open [in in]
      (let [bs (.readNBytes in (int (inc limits/media-blob-bytes-max)))]
        (when (> (alength bs) limits/media-blob-bytes-max)
          (fail (str "media " hash " is larger than " limits/media-blob-bytes-max " bytes")
                {:hash hash}))
        bs))))
(defn- b64 [^bytes bs] (.encodeToString (Base64/getEncoder) bs))

(defn- image-part [mime ^bytes bs]
  {"type" "image_url" "image_url" {"url" (str "data:" mime ";base64," (b64 bs))}})

(defn- text-part [label text]
  {"type" "text" "text" (str "[" label "]\n" text)})

(defn- page-text [^PDDocument doc from to]
  (-> (doto (PDFTextStripper.) (.setSortByPosition true) (.setStartPage (int from))
        (.setEndPage (int to)))
      (.getText doc)))

(defn- substantial? [s]
  (>= (count (str/replace (str s) #"\s" "")) limits/media-text-min-chars))

(defn- render-page ^bytes [^PDFRenderer r page]
  (System/setProperty "java.awt.headless" "true")
  (let [img (.renderImageWithDPI r (int page) (float limits/media-render-dpi) ImageType/RGB)
        out (ByteArrayOutputStream.)]
    (ImageIO/write img "jpeg" out)
    (.toByteArray out)))

(defn- pdf-parts
  [i hash ^bytes bs mode pages]
  (with-open [^PDDocument doc (try (Loader/loadPDF bs)
                                   (catch Exception e
                                     (attempt/rethrow-fatal! e)
                                     (fail (str "media " hash " is not a readable PDF: "
                                                (ex-message e))
                                           {:hash hash})))]
    (let [n     (.getNumberOfPages doc)
          k     (min n pages)
          label (fn [what p] (str "attachment " i ": PDF page " p " of " n ", " what))
          rend  (delay (PDFRenderer. doc))
          per   (mapv (fn [p]
                        (let [t (when-not (= :image mode) (page-text doc p p))]
                          (cond
                            (or (substantial? t) (and (= :text mode) (not (str/blank? t))))
                            {:page p :as :text :part (text-part (label "text layer" p) t)}
                            (= :text mode) {:page p :as :none}
                            :else {:page p :as :image :bytes (render-page @rend (dec p))})))
                      (range 1 (inc k)))]
      {:parts   (vec (keep (fn [{:keys [as part bytes]}]
                             (case as :text part :image (image-part "image/jpeg" bytes) nil))
                           per))
       :image-bytes (reduce + 0 (keep #(some-> ^bytes (:bytes %) alength) per))
       :described {:hash hash :kind "application/pdf" :pages n :read k
                   :as (vec (distinct (map :as per)))}})))

(defn parts
  [store hash-list {:keys [as pages]}]
  (let [mode  (or as :auto)
        pages (-> (or pages limits/media-pages-default) (max 1) (min limits/media-pages-max))]
    (when-not (modes mode)
      (fail (str "unknown :media-as " (pr-str as) " — :auto, :text or :image") {:media-as as}))
    (let [r (reduce
             (fn [acc [i hash]]
               (let [bs   (read-bytes store hash)
                     kind (media-type/sniff bs)
                     one  (cond
                            (= :pdf kind)  (pdf-parts i hash bs mode pages)
                            (string? kind) {:parts [(image-part kind bs)] :image-bytes (alength bs)
                                            :described {:hash hash :kind kind :as [:image]}}
                            (= :text
                               kind)
                              {:parts
                                 [(text-part (str "attachment " i ": text") (String. bs "UTF-8"))]
                                     :described {:hash hash :kind "text/plain" :as [:text]}}
                            :else (fail (str "media " hash " is neither a PDF, an image nor text")
                                        {:hash hash}))
                     acc  (-> acc
                              (update :parts into (:parts one))
                              (update :media conj (:described one))
                              (update :image-bytes + (or (:image-bytes one) 0)))]
                 (when (> (:image-bytes acc) limits/media-image-bytes-max)
                   (fail
                    (str "the images of this step exceed " limits/media-image-bytes-max
                         " bytes — lower :media-pages or read the text layer (:media-as :text)")
                    {:hash hash :sutra/run-limit :media}))
                 acc))
             {:parts [] :media [] :image-bytes 0}
             (map-indexed (fn [i h] [(inc i) h]) hash-list))]
      (select-keys r [:parts :media]))))

(defn elide
  [x]
  (cond
    (and (string? x) (str/starts-with? x "data:")) (str "<" (count x) " bytes of media>")
    (map? x)        (into {} (map (fn [[k v]] [k (elide v)])) x)
    (sequential? x) (mapv elide x)
    :else x))
