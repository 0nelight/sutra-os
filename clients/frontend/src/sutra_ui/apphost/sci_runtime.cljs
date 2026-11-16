(ns sutra-ui.apphost.sci-runtime
  (:require [sutra-ui.ui.core :as ui]
            [sci.core :as sci]
            [reagent.core :as r]
            [sutra-ui.apphost.sdk :as app-sdk]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apphost.modules :as mod]
            [sutra-ui.ui.vendor :as vendor]
            [clojure.string :as str]))

(def ^:private reagent-ns
  {'atom         r/atom
   'cursor       r/cursor
   'create-class r/create-class
   'as-element   r/as-element
   'track        r/track
   'track!       r/track!})

(def ^:private hidden
  #{"constructor" "__proto__" "prototype" "caller" "arguments" "__defineGetter__"
    "__defineSetter__" "__lookupGetter__" "__lookupSetter__"})

(defonce ^:private reals (js/WeakMap.))

(declare guard)

(defn- wrap
  [real stand-in]
  (let [p (js/Proxy.
           stand-in
           #js {:get            (fn [_ k]
                                  (cond
                                    (identical? k (.-hasInstance js/Symbol))
                                    (fn [o] (instance? real o))
                                    (and (string? k) (contains? hidden k)) js/undefined
                                    :else (guard (unchecked-get real k))))
                :has            (fn [_ k] (js-in k real))
                :apply          (fn [_ this args]
                                  (.apply js/Reflect real (or (.get reals this) this) args))
                :construct      (fn [_ args] (.construct js/Reflect real args))
                :set            (constantly false)
                :defineProperty (constantly false)
                :deleteProperty (constantly false)
                :setPrototypeOf (constantly false)
                :getPrototypeOf (constantly nil)})]
    (.set reals p real)
    p))

(defn- guard
  [x]
  (cond
    (fn? x)                                               (wrap x (fn []))
    (and (some? x) (identical? "object" (goog/typeOf x))) (wrap x #js {})
    :else                                                 x))

(defn- timer
  [^js f]
  (fn [cb & more]
    (when (fn? cb) (.apply f js/globalThis (to-array (cons cb more))))))

(defn- on-window [^js f] (fn [& args] (.apply f js/globalThis (to-array args))))

(def ^:private regexp
  (.-constructor #""))

(defn- globals
  []
  (let [fixed {"Math" js/Math "JSON" js/JSON "Date" js/Date "Intl" js/Intl
               "Number" js/Number "String" js/String "Boolean" js/Boolean
               "Array" js/Array "Map" js/Map "Set" js/Set "RegExp" regexp
               "Error" js/Error "Promise" js/Promise "console" js/console
               "parseInt" js/parseInt "parseFloat" js/parseFloat "isNaN" js/isNaN
               "isFinite" js/isFinite "encodeURIComponent" js/encodeURIComponent
               "decodeURIComponent" js/decodeURIComponent "NaN" js/NaN "Infinity" js/Infinity
               "setTimeout" (timer js/setTimeout) "setInterval" (timer js/setInterval)
               "clearTimeout" (on-window js/clearTimeout)
               "clearInterval" (on-window js/clearInterval)}]
    (js/Proxy. #js {}
               #js {:get            (fn [_ k]
                                      (cond
                                        (not (string? k)) nil
                                        (= "Plotly" k)
                                        (guard (unchecked-get js/globalThis "Plotly"))
                                        (contains? fixed k) (guard (get fixed k))))
                    :set            (constantly false)
                    :defineProperty (constantly false)
                    :deleteProperty (constantly false)})))

(defn- instance-classes
  []
  (into {}
        (keep (fn [[sym c]] (when c [sym (guard c)])))
        {'String js/String 'Number js/Number 'Boolean js/Boolean 'Date js/Date
         'RegExp regexp 'Promise js/Promise 'Error js/Error 'Array js/Array
         'Map js/Map 'Set js/Set 'Math js/Math 'JSON js/JSON 'Intl js/Intl
         'NumberFormat (.-NumberFormat js/Intl) 'DateTimeFormat (.-DateTimeFormat js/Intl)
         'PluralRules (.-PluralRules js/Intl) 'Collator (.-Collator js/Intl)
         'RelativeTimeFormat (.-RelativeTimeFormat js/Intl)}))

(def ^:private denied
  '[aget aset js-delete js-invoke unchecked-get unchecked-set set! type])

(defn- make-ctx
  []
  (sci/init
   {:namespaces {'reagent.core   reagent-ns
                 'clojure.string  (sci/copy-ns clojure.string
                                               (sci/create-ns 'clojure.string))
                 'app             (sci/copy-ns sutra-ui.apphost.sdk
                                               (sci/create-ns 'app))}
    :classes    (assoc (instance-classes) 'js (globals))
    :deny       denied}))

(defonce ^:private cache (atom {}))

(defn- err-component
  [msg]
  (js/console.error "app/source:" (str msg))
  (fn [_ _]
    [:div {:class "capp capp--error"}
     [ui/problem (i18n/t :ui/capp-app-failed) (str msg)]]))

(defn error-trace
  [e]
  (some->> (sci/stacktrace e) seq sci/format-stacktrace (clojure.string/join "\n")))

(defn compile-view
  [source]
  (or (get @cache source)
      (let [c (try
                (let [v (sci/eval-string* (make-ctx) source)]
                  (if (or (fn? v) (mod/module? v))
                    v
                    (err-component
                     (str "Quelltext ergibt keine Komponente (fn) oder Modul, sondern: "
                          (pr-str (type v))))))
                (catch :default e
                  (err-component (or (.-message e) e))))]
        (when (str/includes? source "Plotly") (vendor/load! :plotly))
        (swap! cache assoc source c)
        c)))

(mod/set-compiler! compile-view)

(defn render
  [descriptor data]
  (let [c (compile-view (:source descriptor))]
    [(if (mod/module? c) (mod/standalone c) c) descriptor data]))

(mod/set-sci! {:render render :error-trace error-trace})
