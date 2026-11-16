(ns sutra.http.origin
  (:require [clojure.string :as str]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(defn- parsed
  [s]
  (try (let [u (java.net.URI. (str/trim (str s)))]
         (when (and (#{"http" "https"} (some-> (.getScheme u) str/lower-case))
                    (not-empty (.getHost u)))
           {:origin (str (str/lower-case (.getScheme u)) "://" (str/lower-case (.getHost u))
                         (when (not= -1 (.getPort u)) (str ":" (.getPort u))))
            :host   (str/lower-case (.getHost u))
            :port   (.getPort u)}))
       (catch java.net.URISyntaxException _ nil)))

(defn- host-and-port
  [host-header]
  (let [h (str/lower-case (str/trim (str host-header)))]
    (if-let [[_ host port] (re-matches #"(\[[^\]]+\]|[^:]+):(\d+)" h)]
      [host (parse-long port)]
      [h nil])))

(defn same-host?
  [o host-header]
  (let [[host port]                 (host-and-port host-header)
        {oh :host op :port oo :origin} (parsed o)
        op                          (if (= -1 op) (if (str/starts-with? oo "https:") 443 80) op)]
    (boolean (and oh (seq host) (= oh host) (or (nil? port) (= port op))))))

(defn allowlist
  ([] (allowlist (env/getenv "SUTRA_WS_ORIGINS")))
  ([s] (into #{} (keep (comp :origin parsed)) (str/split (str s) #","))))

(defn allowed?
  [allowed o host-header]
  (or (str/blank? o)
      (same-host? o host-header)
      (contains? allowed (:origin (parsed o)))))

(defn wrap-ws-origin
  [handler]
  (let [allowed (allowlist)]
    (fn [{:keys [headers] :as req}]
      (if (and (:websocket? req)
               (not (allowed? allowed (get headers "origin") (get headers "host"))))
        {:status  403
         :headers {"Content-Type" "text/plain; charset=utf-8"}
         :body    "WebSocket origin not allowed\n"}
        (handler req)))))
