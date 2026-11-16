(ns sutra.parser.lint.source
  (:require [sutra.kit.coll :as coll]
            [sutra.parser.lint.support :refer [->name]]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- code-in-data?
  [v]
  (and (list? v) (seq v) (symbol? (first v))))

(defn- numeric-config? [v]
  (or (number? v)
      (and (string? v) (re-matches #"\s*-?\d+(\.\d+)?\s*" v))))

(defn lint-source-config
  [source-types sources]
  (vec
   (for [{:keys [config] :as src} sources
         :let [nm    (->name (or (:name src) (:source-name src) (:type src) "source"))
               typ   (some-> (:type src) keyword)
               descr (get source-types typ)
               fields-by (coll/index-by :key (:fields descr))
               given (merge (dissoc src :config) config)]
         :when (map? config)
         finding
         (concat
          (for [[k v] config :when (code-in-data? v)]
            {:entity (str nm "." (->name k)) :entity-class :sources :severity :error
             :message (str "Config value " (pr-str k) " is " (pr-str v)
                           " — a `.ent` is DATA, not evaluated code, so this stored a"
                           " literal list, not a string. Write a plain literal"
                           " (e.g. \"a,b\"); the transform will otherwise fail at run time.")})
          (for [f (:fields descr)
                :when (and (:required f) (not= :secret (:kind f))
                           (not (seq (str (get given (:key f))))))]
            {:entity (str nm "." (->name (:key f))) :entity-class :sources :severity :warn
             :message (str "Required " (name (or (:kind f) :string)) " field "
                           (pr-str (:key f)) " (" (:label f) ") is missing/blank"
                           " — the source will not connect.")})
          (for [[k v] config
                :let [f (get fields-by k)]
                :when (and f (= :number (:kind f)) (some? v) (not (numeric-config? v)))]
            {:entity (str nm "." (->name k)) :entity-class :sources :severity :warn
             :message (str "Field " (pr-str k) " (" (:label f) ") is declared numeric"
                           " but holds " (pr-str v) " — a non-numeric value.")}))]
     (assoc finding :doc nil))))

(def ^:private app-bypass-rules
  [[#"/api/my-application"
    "constructs the app API path by hand — a view must call app/submit! / app/upload! / app/export! / app/blob-url"]
   [#"js/(?:fetch|XMLHttpRequest|WebSocket|EventSource)"
    "hand-rolls a network/platform call — go through the SDK client verbs (app/submit! / upload! / export!)"]
   [#"\(defn-?\s+(?:->js|->clj|jval)\b"
    "re-implements the SDK JS-interop bridge — use app/->js / app/->clj"]
   [#"(?<![\w-]):style\b"
    "hand-rolls inline styling — compose from capp-* classes and SDK widgets (app/kpi / app/bars / app/modal / app/object-table); pass dynamic values as widget params or via app/num-class"]
   [(re-pattern (str "js/(?:window|document|globalThis|self|top|parent|location|localStorage"
                     "|sessionStorage|indexedDB|navigator|eval|Function|Object|Reflect"
                     "|Proxy)\\b"))
    (str "reaches a browser global the app sandbox does not hand out — js/ holds Math, Date,"
         " JSON, Intl, Number, String, Array, Map, Set, Promise, console and timers; read an"
         " input with app/event-value")
    :warn]
   [#"dangerouslySetInnerHTML|:src-?[dD]oc\b|\(\s*(?:aget|aset|set!|type)\s"
    (str "writes raw HTML or touches a JS object directly — render with SDK widgets"
         " (app/markdown) and read values with app/->clj / app/event-value")
    :warn]])

(defn- strip-cljs-comments
  [src]
  (->> (str/split-lines (or src ""))
       (map #(first (str/split % #";")))
       (str/join "\n")))

(defn lint-app-source
  [apps]
  (vec
   (for [app apps
         :let  [source (:source app)]
         :when (and (string? source) (seq source))
         :let  [nm   (->name (or (:name app) (:application-name app) "application"))
                code (strip-cljs-comments source)]
         [re why severity] app-bypass-rules
         :when (re-find re code)]
     {:entity nm :entity-class :applications :severity (or severity :error)
      :message (str "Consumer-app view " (pr-str nm) " " why
                    " (the SDK is injected as ns `app`; see apphost/sdk.cljs).")
      :doc nil})))
