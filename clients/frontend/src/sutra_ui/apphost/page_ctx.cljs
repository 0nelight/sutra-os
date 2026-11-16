(ns sutra-ui.apphost.page-ctx
  (:require [clojure.string :as str]
            [sutra.capps.leaf-props :as lp]
            [sutra-ui.render-ctx :as rctx]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.url :as url]
            [sutra-ui.apphost.client :as client]))

(defn ev-ctx
  ([app] (ev-ctx app (:overlays app)))
  ([app ov] {:overlays ov :descriptor (:descriptor app) :open-object (:open-object app)
             :frame (:frame app)}))

(defn submitted
  ([app] (submitted app nil))
  ([{:keys [refresh]} then]
   (fn [d] (when (and refresh d) (refresh d)) (when then (then d)))))

(defn page
  [{:keys [app scope]}]
  {:descriptor (:descriptor app)
   :data       (some-> (:data* scope) deref)
   :ctx        (rctx/current)})

(def ^:private media-schemes #{"http" "https" "blob"})

(defn media-src
  [desc v]
  (let [s (when-not (or (coll? v) (fn? v)) (str v))]
    (cond
      (str/blank? s) nil
      (re-find #"(?i)^\s*data:" s) (url/image-src s)
      (or (str/starts-with? s "/") (str/includes? s ":"))
      (url/safe-url s media-schemes)
      :else (client/blob-url desc s))))

(defn sourced [env x tone-key value-key] (lp/sourced (page env) x tone-key value-key))

(defn rows-display
  [desc rows]
  (when-let [t (some-> (first rows) :object-type-ref)]
    (lp/type-entry desc :property-display t)))

(defn row-cols
  [desc rows]
  (when-let [t (some-> (first rows) :object-type-ref)]
    (some->> (lp/type-entry desc :object-rows t)
             seq
             (mapv (fn [p] {:prop (->kw p)})))))
