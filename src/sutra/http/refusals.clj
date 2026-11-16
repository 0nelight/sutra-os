(ns sutra.http.refusals
  (:require [sutra.kit.noted :as noted]
            [clojure.string :as str]
            [sutra.i18n.catalog :as catalog]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.err :as err]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.constraints :as cstr]
            [sutra.http.provenance :as provenance]
            [sutra.http.ports :as ports]
            [sutra.http.security-log :as seclog]
            [sutra.kit.redact :as redact]
            [sutra.store.world :as world]
            [sutra.http.responses
             :refer [negotiated transit-write-handlers]]))

(set! *warn-on-reflection* true)

(defn- branches-declaring
  [store ended obj-type]
  (vec (for [{b :name} (world/branches store)
             :when (and (not= "main" b) (not (contains? ended b)))
             :let [ots (:object-types (world/authored (world/at store b)))]
             :when (or (contains? ots obj-type)
                       (some #(= (keyword (str obj-type)) (keyword (str (:object-type-name %))))
                             (vals ots)))]
         b)))

(defn transit-safe
  [v]
  (cond
    (instance? Throwable v) {:class (str (class v))
                             :message (.getMessage ^Throwable v)}
    (map? v)        (into {} (map (fn [[k vv]] [k (transit-safe vv)])) v)
    (sequential? v) (mapv transit-safe v)
    (set? v)        (into #{} (map transit-safe) v)
    (or (nil? v) (string? v) (number? v) (keyword? v) (symbol? v) (boolean? v) (char? v)
        (uuid? v) (some #(instance? % v) (keys transit-write-handlers)))
    v
    :else           (pr-str v)))

(defn store-unavailable
  [^Throwable t]
  (err/cause #(instance? java.sql.SQLTransientConnectionException %) t))

(defn unavailable
  [^Throwable e req]
  (binding [*out* *err*]
    (println (str "[503] " (:request-method req) " " (seclog/encode (:uri req)) " — "
                  (seclog/encode (redact/scrub (str (.getMessage e)))))))
  {:status 503
   :body {:error "the control plane is temporarily unavailable"
          :data {:retry true
                 :hint (str "a build may be holding the control-plane connections — retry,"
                            " or wait for it to finish")}}})

(defn- locale
  [req]
  (some (set catalog/locales)
        (for [tag (str/split (str (get-in req [:headers "accept-language"])) #",")
              :let [lang (first (str/split (str/trim tag) #"[-;]"))]
              :when (seq lang)]
          (keyword (str/lower-case lang)))))

(defn- message
  [req r]
  (let [code (:refusal/code r)
        l    (locale req)
        msg  (if (and code l (not= :en l))
               (catalog/text l code (merge r (:refusal/params r)))
               (anomaly/message r))]
    (if-let [T (:unknown-type r)]
      (str msg (noted/or-else (str "the branches declaring " T) nil
                 (cstr/elsewhere-hint
                  (branches-declaring (ctx/store) (ports/ended-branches (ctx/store)) T))))
      msg)))

(defn- body
  [msg category d]
  (let [data (transit-safe (redact/redact d))]
    (merge (into {} (remove (comp qualified-keyword? key)) data)
           {:error    (redact/scrub msg)
            :category category
            :data     data})))

(defn response
  [req r]
  (provenance/with-origin
   req
   (if-let [u (some-> (anomaly/cause r) store-unavailable)]
     (unavailable u req)
     (negotiated req (anomaly/status r)
                 (body (message req r) (anomaly/category r)
                       (provenance/without-sent (anomaly/data r)))))
   r))
