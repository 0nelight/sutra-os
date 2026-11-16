(ns sutra.foundation.rpc
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.digest :as digest]
            [sutra.kit.effects :as effects]
            [sutra.kit.redact :as redact]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.foundation.outbound :as out]
            [org.httpkit.server :as http-kit])
  (:import (java.security MessageDigest)
           (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)))

(set! *warn-on-reflection* true)

(def ^:private edn-headers {"Content-Type" "application/edn"})

(defn- respond [status body]
  {:status status :headers edn-headers :body (pr-str body)})

(defn token-for
  [^String master ^String peer]
  (let [mac (doto (Mac/getInstance "HmacSHA256")
              (.init (SecretKeySpec. (.getBytes master "UTF-8") "HmacSHA256")))]
    (digest/hex (.doFinal mac (.getBytes peer "UTF-8")))))

(defn- bearer [req]
  (some-> (get-in req [:headers "authorization"]) (str/replace #"^Bearer\s+" "")))

(defn same-token? [^String a ^String b]
  (and (seq a) (seq b)
       (MessageDigest/isEqual (.getBytes a "UTF-8") (.getBytes b "UTF-8"))))

(defn authorized? [token req]
  (same-token? token (bearer req)))

(defn query-params
  [req]
  (into {} (for [kv (str/split (str (:query-string req)) #"&")
                 :let [[k v] (str/split kv #"=" 2)]
                 :when (seq k)]
             [(keyword k) (java.net.URLDecoder/decode (str v) "UTF-8")])))

(def request-key-header "x-sutra-request-key")

(def wait-header "x-sutra-wait-ms")

(def ^:dynamic *grace-ms* 5000)

(def ^:private answer-ttl-ms (* 10 60 1000))

(defonce ^:private works (atom {}))

(defn- prune! []
  (let [now (System/currentTimeMillis)]
    (swap! works
           (fn [m]
             (into {}
                   (remove (fn [[_ {:keys [done-at]}]]
                             (and done-at (> (- now done-at) answer-ttl-ms)))) m)))))

(defn- answer [f body]
  (try (respond 200 {:ok true :result (f body)})
       (catch Exception e
         (attempt/rethrow-fatal! e)
         (respond 400 {:ok false :error (.getMessage e) :data (redact/redact (ex-data e))}))))

(defn- abandon!
  [k w]
  (let [[old _] (swap-vals! works
                            (fn [m] (let [x (get m k)]
                                       (if (and x (identical? (:answer x) (:answer w))
                                                (not (realized? (:answer x))))
                                         (dissoc m k) m))))]
    (when (identical? (:answer w) (:answer (get old k)))
      (effects/cancel! (:token w)))))

(defn- watch!
  [k answer]
  (future
    (loop []
      (Thread/sleep 100)
      (let [w (get @works k)]
        (when (and w (identical? answer (:answer w)) (not (realized? answer)))
          (let [now (System/currentTimeMillis)]
            (if (or (> now (+ (:until w Long/MAX_VALUE) *grace-ms*))
                    (and (:left-at w) (> now (+ (:left-at w) *grace-ms*))))
              (abandon! k w)
              (recur))))))))

(defn- attach!
  [k wait-ms run]
  (let [mine  (promise)
        until (when wait-ms (+ (System/currentTimeMillis) (long wait-ms)))
        wait  (fn [w] (cond-> (-> w (update :waiters inc) (dissoc :left-at))
                        until (update :until #(max (or % 0) until))))
        [old new] (swap-vals! works
                              (fn [m]
                                (if (get m k)
                                  (update m k wait)
                                  (assoc m
                                         k (cond-> {:answer mine :token (effects/work-token)
                                                    :waiters 1}
                                             until (assoc :until until))))))
        w (get new k)]
    (when-not (get old k)
      (watch! k mine)
      (future
        (deliver mine (binding [effects/*work* (:token w)] (run)))
        (swap! works
               (fn [m]
                 (if (identical? mine (get-in m [k :answer]))
                   (assoc-in m [k :done-at] (System/currentTimeMillis)) m)))))
    w))

(defn- detach!
  [k]
  (swap! works
         (fn [m] (if (get m k)
                   (update m k (fn [w] (let [w (update w :waiters dec)]
                                         (cond-> w
                                           (<= (:waiters w)
                                               0) (assoc :left-at (System/currentTimeMillis))))))
                   m))))

(defn handler
  ([routes req] (handler routes req nil))
  ([routes req {:keys [token open] :as auth}]
   (try
     (let [f (get routes (:uri req))]
       (cond
         (nil? f)
         (respond 404 {:ok false :error "not found"})

         (and auth (not (contains? open (:uri req))) (not (authorized? token req)))
         (respond 401 {:ok false :error "unauthorized"})

         :else
         (let [body (some-> (:body req) slurp edn/read-string)
               k    (get-in req [:headers request-key-header])]
           (prune!)
           (if (and k (:async-channel req))
             (let [w (attach! [(:uri req) k] (some-> (get-in req [:headers wait-header]) parse-long)
                              #(answer f body))]
               (http-kit/as-channel req
                                    {:on-open
                                       (fn [ch] (future (http-kit/send! ch (deref (:answer w)))))
                                     :on-close (fn [_ _] (detach! [(:uri req) k]))}))
             (answer f body)))))
     (catch Exception e
       (attempt/rethrow-fatal! e)
       (respond 400 {:ok false :error (.getMessage e) :data (redact/redact (ex-data e))})))))

(defn serve!
  [server* handler opts]
  (reset! server* (http-kit/run-server handler (assoc opts :legacy-return-value? false))))

(defn stop-server!
  [server*]
  (when-let [s @server*] (http-kit/server-stop! s) (reset! server* nil)))

(defn- answer-of
  [p work]
  (loop []
    (let [r (deref p 250 ::waiting)]
      (if (identical? ::waiting r)
        (do (effects/check! work) (recur))
        r))))

(defn call!
  [{:keys [url peer timeout retries backoff-ms token request-key work]
    :or {retries 0 backoff-ms 0}}
   payload]
  (let [work     (or work effects/*work*)
        body-str (pr-str payload)
        headers  (cond-> (assoc edn-headers request-key-header (str (or request-key (random-uuid))))
                   timeout (assoc wait-header (str timeout))
                   token   (assoc "Authorization" (str "Bearer " token)))]
    (loop [attempt 0]
      (let [{:keys [status body error]}
            (answer-of (out/request work (cond-> {:method :post :url url :headers headers
                                                  :body body-str :keepalive -1}
                                           timeout (assoc :timeout timeout)))
                       work)]
        (cond
          (and error (< attempt retries))
          (do (Thread/sleep (long (* backoff-ms (long (inc attempt)))))
              (recur (inc attempt)))

          error
          (refusal/refuse! :refusal.foundation/peer-unreachable
                           {:cause error :url url :attempts (inc attempt)} {:peer peer})

          :else
          (let [resp (edn/read-string (if (string? body) body (slurp body)))]
            (if (and (= 200 status) (:ok resp))
              (:result resp)
              (let [d (assoc (or (:data resp) {}) :status status)]
                (refusal/refuse! :refusal.foundation/peer-refused
                                 (cond-> (assoc d ::anomaly/category
                                                (or (anomaly/category d) :upstream))
                                   (:refusal/code d) (assoc :remote-code (:refusal/code d)))
                                 {:peer peer :why (:error resp)})))))))))
