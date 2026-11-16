(ns sutra.sources.sidecar
  (:require [clojure.data.json :as json]
            [sutra.foundation.encoding :as enc]
            [clojure.string :as str]
            [sutra.foundation.outbound :as out]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(defn parse-sidecars
  [s]
  (into {} (for [pair (str/split (str/trim (str s)) #"[,\s]+")
                 :when (seq pair)
                 :let [[t u] (str/split pair #"=" 2)]]
             (if (and (re-matches #"[a-z][a-z0-9-]*" (str t)) (seq u))
               [(keyword t) (str/replace u #"/+$" "")]
               (throw (ex-info (str "SUTRA_CONNECTOR_SIDECARS: " (pr-str pair) " is not type=url")
                               {:pair pair}))))))

(defn- kw-set [xs] (into #{} (map keyword) xs))

(defn wire->scan
  [scan]
  (cond-> {:preds (into {} (for [[op how] (:preds scan)]
                             [(keyword (name op)) (keyword (name how))]))}
    (contains? scan :project?)  (assoc :project? (boolean (:project? scan)))
    (contains? scan :page?)     (assoc :page? (boolean (:page? scan)))
    (contains? scan :compound?) (assoc :compound? (boolean (:compound? scan)))))

(defn wire->profile
  [{:keys [capabilities endpoint needs scan]}]
  (cond-> {:capabilities (kw-set capabilities)
           :route        #{:agent}}
    endpoint (assoc :endpoint (keyword endpoint))
    needs    (assoc :needs (mapv keyword needs))
    scan     (assoc :scan (wire->scan scan))))

(defn- post! [url token body timeout-ms]
  (let [{:keys [status body error]}
        (out/post! url {:headers (cond-> {"Content-Type" "application/json"}
                                   token (assoc "Authorization" (str "Bearer " token)))
                        :body    (json/write-str body)
                        :timeout timeout-ms})]
    (cond
      error (refusal/refuse! :refusal.foreign/connector-unreachable {:url url :error (str error)}
                             {:url url})
      :else (let [m (enc/json-or-nil (cond-> body (and body (not (string? body))) slurp)
                                       :key-fn keyword)]
              (cond
                (:error m)
                (refusal/refuse! :refusal.foreign/connector-failed
                                 {:url url :status status :error (:error m)}
                                 {:code (str (get-in m [:error :code]))
                                  :message (str (get-in m [:error :message]))})
                (or (not= 200 status) (not (map? m)))
                (refusal/refuse! :refusal.foreign/connector-failed {:url url :status status}
                                 {:code (str "http-" status) :message ""})
                :else m)))))

(defn describe!
  [url token timeout-ms]
  (into {}
        (for [[t {:keys [profile form]}] (:types (post! (str url "/describe") token {} timeout-ms))]
          [(keyword (name t))
           (cond-> {:profile (wire->profile profile)} form (assoc :form form))])))

(defn- result-of [op r]
  (case op
    :scan-profile (wire->scan r)
    :tables  (vec r)
    :columns (mapv #(update % :ontology-type (fn [t] (some-> t keyword))) r)
    :scan    (vec r)
    r))

(defn op!
  [url token timeout-ms spec {:keys [op table scan webhook-id params opts]} plan]
  (let [body (cond-> {:op op :type (:type spec) :spec spec}
               table      (assoc :table table)
               scan       (assoc :scan scan)
               plan       (assoc :plan plan)
               webhook-id (assoc :webhook-id webhook-id)
               params     (assoc :params params)
               opts       (assoc :opts opts))]
    (result-of op (:result (post! (str url "/op") token body timeout-ms)))))
