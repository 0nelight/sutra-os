(ns sutra.media.http.blobs
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [sutra.foundation.media-type :as media-type]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [clojure.string :as str]
            [sutra.catalog.lookup :as lookup]
            [sutra.blobs.api :as blobs]
            [sutra.media.ports :as ports]
            [sutra.media.http.sets :as media]
            [sutra.media.virtual :as virtual]
            [sutra.types.media :as mt]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.grants :as grants]
            [sutra.media.sets :as msets]
            [sutra.http.responses :refer [ok too-many]]
            [sutra.kit.rate-limit :as rate]
            [sutra.kit.digest :as digest]))

(set! *warn-on-reflection* true)

(defn- blob-meta
  [store hash]
  (let [schema (seat/rebuild-schema-from-storage store)]
    (when (lookup/rid-of schema :object-types :blob)
      (-> ((:query (ports/object-reads store)) store schema
                                         {:find :blob :where {:hash {:= hash}} :limit 1})
          :rows first :values))))

(defn- parse-range
  [hdr total]
  (when (and hdr (str/starts-with? hdr "bytes="))
    (let [[a b] (str/split (subs hdr 6) #"-" 2)
          a (some-> a str/trim not-empty)
          b (some-> b str/trim not-empty)]
      (try
        (cond
          (nil? a) (when b [(max 0 (- total (Long/parseLong b))) (dec total)])
          :else    (let [s (Long/parseLong a)
                         e (if b (Long/parseLong b) (dec total))]
                     (when (and (<= 0 s) (<= s e) (< s total))
                       [s (min e (dec total))])))
        (catch NumberFormatException _ nil)))))

(defn- sniff-mime
  [store hash]
  (try
    (with-open [is (blobs/open store hash)]
      (let [k (media-type/sniff (.readNBytes is 12))]
        (case k :pdf "application/pdf" (:text "image/webp") nil k)))
    (catch java.io.IOException _ nil)))

(defn- serving
  [ctype]
  (if (media-type/inline-safe? ctype)
    {"Content-Type"           ctype
     "Content-Disposition"    "inline"
     "X-Content-Type-Options" "nosniff"}
    {"Content-Type"            ctype
     "Content-Disposition"     "attachment"
     "X-Content-Type-Options"  "nosniff"
     "Content-Security-Policy" "sandbox; default-src 'none'"}))

(defn- stream-virtual
  [store address headers]
  (when-let [item (msets/virtual-item store address)]
    (let [rng (atom nil)
          opened (virtual/open! store item
                                (fn [{:keys [size]}]
                                  (let [r (parse-range (get headers "range") size)]
                                    (reset! rng r)
                                    (when r {:offset (first r)
                                             :length (inc (- (second r) (first r)))}))))]
      (if-let [{:keys [size modified stream]} opened]
        (let [base (merge (serving (or (:mime item) "application/octet-stream"))
                          {"Accept-Ranges" "bytes"
                           "Cache-Control" "private, no-cache"
                           "ETag" (str \" size \- (digest/sha256-hex (str modified)) \")})]
          (if-let [[s e] @rng]
            {:status  206
             :headers (assoc base
                             "Content-Range" (str "bytes " s "-" e "/" size)
                             "Content-Length" (str (inc (- e s))))
             :body    stream}
            {:status 200 :headers (assoc base "Content-Length" (str size)) :body stream}))
        (refusal/refused :refusal.http/no-such-blob {:hash hash})))))

(defn stream-blob
  ([store hash headers]
   (let [blob-store (ctx/blobs)
         meta  (try (blob-meta store hash) (catch clojure.lang.ExceptionInfo _ nil))]
     (if (blobs/exists? blob-store hash)
       (let [total (long (or (:size meta) (blobs/size-of blob-store hash) 0))
             ctype (or (:mime meta) (sniff-mime blob-store hash) "application/octet-stream")
             etag  (str \" hash \")
             rng   (parse-range (get headers "range") total)]
         (if rng
           (let [[s e] rng len (inc (- e s))]
             {:status  206
              :headers (merge (serving ctype)
                              {"Content-Range"  (str "bytes " s "-" e "/" total)
                               "Content-Length" (str len)
                               "Accept-Ranges"  "bytes"
                               "ETag"           etag
                               "Cache-Control"  "private, max-age=31536000, immutable"})
              :body    (blobs/open-range blob-store hash s len)})
           {:status  200
            :headers (merge (serving ctype)
                            {"Content-Length" (str total)
                             "Accept-Ranges"  "bytes"
                             "ETag"           etag
                             "Cache-Control"  "private, max-age=31536000, immutable"})
            :body    (blobs/open blob-store hash)}))
       (or (stream-virtual store hash headers)
           (refusal/refused :refusal.http/no-such-blob {:hash hash}))))))

(defn- admin?
  [req]
  (= (ep/signed-in-user req) grants/admin-username))

(defn- readable-through?
  [store schema ot id hash]
  (boolean
   (when-let [rid (lookup/rid-of schema :object-types (keyword ot))]
     (try
       (grants/authorize-read! (grants/read-rights store) (get-in schema [:object-types rid]) rid)
       (let [attrs (mt/attribute-names (lookup/attr-type-map schema rid))]
         (and (seq attrs)
              (some #(contains? (mt/row-references attrs %) hash)
                    (sub/read-objects store rid [id]))))
       (catch clojure.lang.ExceptionInfo _ false)))))

(defn- readable-in-set?
  [store schema media-set hash]
  (and (msets/readable? store schema media-set)
       (msets/holds? store media-set hash)))

(defn get-blob
  [{:keys [path-params query-params headers] store :sutra/store schema :sutra/schema
    branch :sutra/branch :as req}]
  (let [hash (:hash path-params)
        branch-store  (ctx/at store branch)
        ot   (not-empty (str (get query-params "type")))
        id   (not-empty (str (get query-params "id")))
        mset (not-empty (str (get query-params "media-set")))]
    (cond
      (not (blobs/valid-address? hash)) (refusal/refused :refusal.http/bad-blob-hash {:hash hash})
      (and ot id)                       (if (readable-through? branch-store schema ot id hash)
                                          (stream-blob branch-store hash headers)
                                          (refusal/refused :refusal.http/no-such-blob {:hash hash}))
      mset                              (if (readable-in-set? branch-store schema mset hash)
                                          (stream-blob branch-store hash headers)
                                          (refusal/refused :refusal.http/no-such-blob {:hash hash}))
      (admin? req)                      (stream-blob branch-store hash headers)
      :else                             (refusal/refused :refusal.http/no-such-blob {:hash hash}))))

(def upload-quota
  {:capacity (* 4 1024 1024 1024) :per-ms (* 60 60 1000)})

(defonce ^:private uploads (rate/limiter upload-quota #(System/currentTimeMillis)))

(defn- declared-length
  [{:keys [content-length headers]}]
  (or content-length (some-> (get headers "content-length") parse-long)))

(defn put-blob
  [{:keys [body] :as req}]
  (let [who   (str (ep/signed-in-user req))
        free? (admin? req)
        n     (declared-length req)
        wait  (when-not free? (rate/spend! uploads who (or n 1)))]
    (cond
      (nil? body) (refusal/refused :refusal.media/empty-upload {})
      wait        (too-many "upload quota for this hour is used up" wait)
      :else       (let [r (blobs/put! (ctx/blobs) body)]
                    (when-not (or free? n) (rate/charge! uploads who (dec (:size r))))
                    (ok r)))))

(defn b64-bytes
  [^String s]
  (when-let [^bytes bs (try (some->> s (.decode (java.util.Base64/getDecoder)))
                            (catch IllegalArgumentException _ nil))]
    (when (pos? (alength bs)) bs)))

(defn land-media
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch
    auth :sutra/auth}]
  (let [{:keys [dataset media-column values filename mime message bytes-b64]} body-params
        bytes (b64-bytes bytes-b64)
        rid   (some->> dataset name keyword (lookup/rid-of schema :datasets))]
    (grants/authorize! auth {:op :update :project (get-in schema [:datasets rid :project])
                             :resource rid}
                       dataset)
    (cond
      (not (and dataset media-column))
      (refusal/refused :refusal.media/media-land-braucht-dataset-und {:dataset dataset})

      (nil? bytes)
      (refusal/refused :refusal.media/leerer-oder-ungueltiger-upload-bytes {})

      (> (alength ^bytes bytes) media/max-land-bytes)
      (refusal/refused :refusal.media/datei-zu-gross {:max media/max-land-bytes})

      :else
      (let [{:keys [hash size write triggered-by] mset :media-set}
            (media/land! store schema branch
                         {:bytes        bytes
                          :dataset      dataset
                          :media-column media-column
                          :values       values
                          :filename     filename
                          :mime         mime
                          :message      (or message (str "upload " (or filename (name dataset))))})]
        (if (= 200 (:status write))
          (ok {:hash hash :size size :dataset (name dataset) :media-set mset
               :media-column (name media-column) :filename filename
               :written (get-in write [:body :written])
               :branch  (get-in write [:body :branch])
               :triggered-by (vec triggered-by)})
          write)))))

(defn get-xfer-media
  [{:keys [path-params] store :sutra/store}]
  (let [hash       (:hash path-params)
        blob-store (ctx/blobs)]
    (cond
      (not (blobs/valid-address? hash)) (refusal/refused :refusal.http/bad-blob-hash {:hash hash})

      (not (blobs/exists? blob-store hash))
      (or (stream-virtual store hash nil) (refusal/refused :refusal.http/no-such-blob {:hash hash}))

      :else
      {:status  200
       :headers {"Content-Type"           "application/octet-stream"
                 "X-Content-Type-Options" "nosniff"
                 "Cache-Control"          "private, no-store"
                 "Content-Length"         (str (blobs/size-of blob-store hash))}
       :body    (blobs/open blob-store hash)})))

(defn media-gc
  [{:keys [body-params] store :sutra/store}]
  (ok (media/sweep! store
                    {:apply?     (boolean (:apply? body-params))
                     :grace-days (:grace-days body-params)})))

(def module
  {:name        :blobs
   :contributes {:http/routes
                 {:blob        {:get get-blob}
                  :xfer-media  {:get get-xfer-media}
                  :blob-upload {:post put-blob}
                  :media-gc    {:post media-gc}
                  :media-land  {:post land-media}}}})
