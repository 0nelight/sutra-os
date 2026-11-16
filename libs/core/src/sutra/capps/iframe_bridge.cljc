(ns sutra.capps.iframe-bridge
  (:require [clojure.string :as str]
            [sutra.kit.url :as url]))

(def max-objects 10000)
(def set-output "APPHOST//SET_OUTPUT_VALUE")
(def trigger-event "APPHOST//TRIGGER_APPHOST_EVENT")
(def resize "APPHOST//RESIZE")
(def variable-values "APPHOST//VARIABLE_VALUES")

(defn youtube-embed
  [url]
  (if-let [[_ id] (re-find #"(?i)^https?://(?:www\.|m\.)?youtube\.com/watch\?(?:[^#]*&)?v=([\w-]+)"
                           (str url))]
    (str "https://www.youtube.com/embed/" id)
    url))

(defn safe-src
  [src]
  (url/safe-url src url/web-schemes))

(defn- encode [x]
  #?(:clj  (java.net.URLEncoder/encode (str x) "UTF-8")
     :cljs (js/encodeURIComponent (str x))))

(defn with-params
  [url params]
  (let [[base fragment] (str/split (str url) #"#" 2)
        pairs (for [[k v] params :when (some? v)]
                (str (encode (if (keyword? k) (name k) k)) "=" (encode v)))]
    (cond-> base
      (seq pairs) (str (if (str/includes? base "?") "&" "?") (str/join "&" pairs))
      fragment    (str "#" fragment))))

(defn frame-url
  [{:keys [src url-params embedded?]}]
  (when-let [s (safe-src (youtube-embed src))]
    (with-params s (cond-> (or url-params {}) embedded? (assoc :embedded "true")))))

(defn origin-of
  [url]
  (some-> (re-find #"(?i)^https?://[^/?#]+" (str url)) str/lower-case))

(def ^:private default-sandbox "allow-scripts allow-forms allow-popups")

(defn- without-same-origin
  [sandbox]
  (->> (str/split (str sandbox) #"\s+")
       (remove #{"allow-same-origin"})
       (str/join " ")))

(defn frame-policy
  [{:keys [src sandbox]} host-origin]
  (let [own      (origin-of src)
        foreign? (and own (not= own (str/lower-case (str host-origin))))
        base     (if (string? sandbox) sandbox default-sandbox)]
    (if foreign?
      {:sandbox (if (false? sandbox)
                  nil
                  (cond-> base
                    (not (str/includes? base "allow-same-origin")) (str " allow-same-origin")))
       :target  own
       :accept  own}
      {:sandbox (without-same-origin base)
       :target  "*"
       :accept  "null"})))

(defn accept?
  [{:keys [accept]} origin]
  (= accept origin))

(defn- capped
  [v]
  (if (and (sequential? v) (> (count v) max-objects)) (vec (take max-objects v)) v))

(defn- as-key [k] (when (some? k) (keyword (if (keyword? k) (name k) (str k)))))

(defn- writable [{:keys [mode output-params variables]}]
  (if (= :bidirectional mode) variables output-params))

(defn inbound
  [{:keys [events] :as cfg} {:keys [type outputParameterKey eventKey height value undefined?]}]
  (condp = type
    set-output    (let [k (as-key outputParameterKey)]
                    (when (contains? (or (writable cfg) {}) k)
                      (if undefined? [:reset k] [:set k (capped value)])))
    trigger-event (let [k (as-key eventKey)]
                    (when (contains? (or events {}) k) [:event k]))
    resize        (when (and (number? height) (pos? height)) [:resize height])
    nil))

(defn outbound
  [{:keys [mode input-params]} variable-values*]
  (if (= :bidirectional mode)
    {:type   variable-values
     :values (into {} (map (fn [[k v]] [k {:value (capped v) :loading false}])) variable-values*)}
    (into {} (map (fn [[k v]] [k (capped v)])) input-params)))
