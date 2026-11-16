(ns conformance.kits.events
  (:require [clojure.string :as str]
            [conformance.report :as r]
            [conformance.schema :as schema])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(defn- pct-decode
  [^String s]
  (let [bs (java.io.ByteArrayOutputStream.)]
    (loop [i 0]
      (if (< i (count s))
        (if (and (= \% (.charAt s i)) (<= (+ i 3) (count s)))
          (do (.write bs (Integer/parseInt (subs s (inc i) (+ i 3)) 16)) (recur (+ i 3)))
          (do (.write bs (.getBytes (str (.charAt s i)) "UTF-8")) (recur (inc i))))
        (String. (.toByteArray bs) "UTF-8")))))

(defn attributes
  [headers]
  (into {} (for [[k v] headers :when (str/starts-with? k "ce-")]
             [(subs k 3) (pct-decode v)])))

(defn delivery-problems
  [root {:keys [method headers]}]
  (let [attrs (attributes headers)
        errs  (schema/errors root attrs)
        idem  (get headers "idempotency-key")]
    (cond
      (not= "POST" method) (str "method " method)
      (seq errs) (str/join "; " errs)
      (and idem (not= idem (get attrs "id")))
      (str "Idempotency-Key " (pr-str idem) " differs from ce-id " (pr-str (get attrs "id"))))))

(defn listen!
  [port]
  (let [q (LinkedBlockingQueue.)
        s (HttpServer/create (InetSocketAddress. (int port)) 0)]
    (.createContext s "/"
                    (reify HttpHandler
                      (handle [_ ex]
                        (let [^HttpExchange ex ex
                              hs (into {} (for [[k vs] (.getRequestHeaders ex)]
                                            [(str/lower-case k) (first vs)]))]
                          (.put q {:method  (.getRequestMethod ex)
                                   :path    (str (.getRequestURI ex))
                                   :headers hs
                                   :body    (slurp (.getRequestBody ex))})
                          (.sendResponseHeaders ex 200 -1)
                          (.close ex)))))
    (.start s)
    {:server s :queue q}))

(defn checks
  [{:keys [port root count timeout-s] :or {count 1 timeout-s 300}} & [on-listen]]
  (let [{:keys [server queue]} (listen! port)
        deadline (+ (System/currentTimeMillis) (* 1000 timeout-s))]
    (try
      (when on-listen (on-listen))
      (let [got (loop [acc []]
                  (let [left (- deadline (System/currentTimeMillis))
                        d    (when (and (< (clojure.core/count acc) count) (pos? left))
                               (.poll queue left TimeUnit/MILLISECONDS))]
                    (if d (recur (conj acc d)) acc)))]
        (into [(r/check "received" (str "at least " count " deliveries arrived")
                        #(when (< (clojure.core/count got) count)
                           (str (clojure.core/count got) " arrived before the timeout")))]
              (map-indexed (fn [i d]
                             (r/check (str "delivery-" (inc i))
                                      (str "delivery to " (:path d) " is a conforming CloudEvent")
                                      #(delivery-problems root d)))
                           got)))
      (finally (.stop ^HttpServer server 0)))))
