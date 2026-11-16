(ns sutra.types.file-sync
  (:require [clojure.string :as str])
  (:import (java.time Instant LocalDate ZoneOffset)))

(set! *warn-on-reflection* true)

(def Filters
  [:map {:closed true}
   [:path-matches {:optional true} :string]
   [:path-not-matches {:optional true} :string]
   [:modified-after {:optional true} :string]
   [:size-between {:optional true} [:tuple :int :int]]
   [:any-file-matches {:optional true} :string]
   [:at-least {:optional true} :int]
   [:limit {:optional true} :int]])

(def SyncFilters
  (into Filters
        [[:exclude-synced {:optional true}
          [:or :boolean [:vector [:enum :modified :size]]]]
         [:ignore-unmatched-schema {:optional true} :boolean]]))

(defn normal-path
  [path]
  (str/join "/" (remove str/blank? (str/split (str path) #"[/\\]+"))))

(defn relative-path
  [subfolder path]
  (let [root (normal-path subfolder)
        p    (normal-path path)]
    (if (and (seq root) (str/starts-with? p (str root "/")))
      (subs p (inc (count root)))
      p)))

(defn- instant
  ^Instant [s]
  (if (re-matches #"\d{4}-\d{2}-\d{2}" (str s))
    (.toInstant (.atStartOfDay (LocalDate/parse s) ZoneOffset/UTC))
    (Instant/parse (str s))))

(defn- per-file-predicate
  [{:keys [path-matches path-not-matches modified-after size-between]}]
  (let [in  (some-> path-matches re-pattern)
        out (some-> path-not-matches re-pattern)
        at  (some-> modified-after instant)
        [lo hi] size-between]
    (fn [{:keys [path modified size]}]
      (and (or (nil? in) (re-find in path))
           (or (nil? out) (not (re-find out path)))
           (or (nil? at) (and modified (.isAfter (instant modified) at)))
           (or (nil? lo) (and size (<= lo size hi)))))))

(defn- already-synced?
  [exclude earlier {:keys [modified size]}]
  (and earlier
       (or (true? exclude)
           (every? (fn [k]
                     (case k
                       :modified (= (:modified earlier) modified)
                       :size     (= (:size earlier) size)))
                   exclude))))

(defn select
  ([filters files] (select filters files nil))
  ([{:keys [exclude-synced any-file-matches at-least limit] :as filters} files synced]
   (let [kept   (filter (per-file-predicate filters) files)
         fresh  (if exclude-synced
                  (remove #(already-synced? exclude-synced (get synced (:path %)) %) kept)
                  kept)
         fresh  (sort-by :path fresh)
         any    (some-> any-file-matches re-pattern)
         base   {:listed (count files) :excluded (- (count files) (count fresh))}]
     (cond
       (and any (not-any? #(re-find any (:path %)) files))
       (assoc base :files [] :held-by :any-file-matches)

       (and at-least (< (count fresh) at-least))
       (assoc base :files [] :held-by :at-least)

       :else
       (assoc base :files (vec (if limit (take limit fresh) fresh)))))))

(def ^:private mime-of-extension
  {"pdf" "application/pdf" "png" "image/png" "jpg" "image/jpeg" "jpeg" "image/jpeg"
   "gif" "image/gif" "webp" "image/webp" "tif" "image/tiff" "tiff" "image/tiff"
   "bmp" "image/bmp" "svg" "image/svg+xml" "txt" "text/plain" "csv" "text/csv"
   "html" "text/html" "xml" "application/xml" "json" "application/json"
   "doc" "application/msword" "xls" "application/vnd.ms-excel"
   "docx" "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
   "xlsx" "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
   "mp3" "audio/mpeg" "mp4" "video/mp4" "wav" "audio/wav"})

(defn mime-of
  [path]
  (let [ext (some-> (re-find #"\.([^./]+)$" (str path)) second str/lower-case)]
    (cond
      (nil? ext) "application/octet-stream"
      :else      (or (mime-of-extension ext) (str "application/x-" ext)))))
