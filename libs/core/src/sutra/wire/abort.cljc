(ns sutra.wire.abort
  (:require [sutra.types.paths :as paths]))

(def max-retries
  2)

(defn reads?
  [method path]
  (let [method (keyword (.toLowerCase (name method)))
        id     (paths/endpoint-of path)
        w      (when id (paths/writes? id method))]
    (if (some? w)
      (not w)
      (= :get method))))

(defn aborted?
  [{:keys [status error-name hidden?]}]
  (boolean (and (nil? status)
                (or (= "AbortError" error-name) hidden?))))

(defn next-step
  [{:keys [reads? attempt] :or {attempt 0} :as failure}]
  (cond
    (not (aborted? failure))                 :failed
    (and reads? (< attempt max-retries))     :retry
    :else                                    :aborted))

(defn aborted-error?
  [e]
  (boolean (::aborted (ex-data e))))

#?(:cljs
   (defonce ^:private hidden-at
     (let [a (atom nil)]
       (when (exists? js/document)
         (let [mark! #(when (.-hidden js/document) (reset! a (js/performance.now)))]
           (.addEventListener js/document "visibilitychange" mark!)
           (.addEventListener js/window "pagehide" #(reset! a (js/performance.now)))))
       a)))

#?(:cljs
   (defn- hidden-since?
     [t0]
     (or (and (exists? js/document) (.-hidden js/document))
         (some-> @hidden-at (>= t0)))))

#?(:cljs
   (defn- when-visible
     []
     (js/Promise.
      (fn [resolve _]
        (if-not (and (exists? js/document) (.-hidden js/document))
          (resolve nil)
          (letfn [(on-change []
                    (when-not (.-hidden js/document)
                      (.removeEventListener js/document "visibilitychange" on-change)
                      (resolve nil)))]
            (.addEventListener js/document "visibilitychange" on-change)))))))

#?(:cljs
   (defn fetch!
     ([start method path] (fetch! start (reads? method path) 0 method path))
     ([start reads? attempt method path]
      (let [t0 (js/performance.now)]
        (.catch (start)
                (fn [e]
                  (case (next-step {:status     nil
                                    :error-name (some-> e .-name)
                                    :hidden?    (hidden-since? t0)
                                    :reads?     reads?
                                    :attempt    attempt})
                    :retry   (.then (when-visible)
                                    #(fetch! start reads? (inc attempt) method path))
                    :aborted (throw (ex-info (str "Abruf abgebrochen: "
                                                  (.toUpperCase (name method)) " " path)
                                             {::aborted true :status 499
                                              :method method :path path}
                                             e))
                    (throw e))))))))

#?(:cljs
   (defn fetch-url
     ([url] (fetch-url url nil))
     ([url opts]
      (fetch! #(if opts (js/fetch url opts) (js/fetch url))
              (or (some-> opts .-method) "GET") url))))
