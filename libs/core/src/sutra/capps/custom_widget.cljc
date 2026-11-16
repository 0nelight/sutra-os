(ns sutra.capps.custom-widget
  (:require [clojure.string :as str]
            [sutra.capps.iframe-bridge :as bridge]))

(def manifest-file "widgets.edn")
(def runtime-url "/js/widget/runtime.js")
(def max-release-bytes (* 5 1024 1024))

(def sandbox "allow-scripts allow-downloads allow-modals")

(def scalar-types #{:boolean :date :number :string :timestamp})
(def value-types (into scalar-types #{:object-set :any}))

(def ^:private csp-directives #{:connect-src :img-src :media-src :font-src :frame-src})

(defn version?
  [v]
  (boolean (and (string? v) (re-matches #"\d+\.\d+\.\d+" v))))

(defn set-name?
  [s]
  (boolean (and (string? s) (re-matches #"[a-z][a-z0-9-]*" s))))

(defn- version-key
  [v]
  (mapv #?(:clj #(Long/parseLong %) :cljs js/parseInt) (str/split v #"\.")))

(defn latest
  [versions]
  (last (sort-by version-key (filter version? versions))))

(defn resolve-version
  [versions pin]
  (let [pin (some-> pin str str/trim not-empty)]
    (if (or (nil? pin) (= "latest" pin))
      (latest versions)
      (some #{pin} versions))))

(defn- type-ok?
  [t]
  (or (contains? value-types t)
      (and (vector? t) (= 2 (count t)) (= :array (first t)) (contains? scalar-types (second t)))))

(defn source-of
  [files ns-sym]
  (let [base (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/"))]
    (some (fn [p] (when (contains? files p) p))
          (for [dir ["src/" ""] ext [".cljs" ".cljc"]] (str dir base ext)))))

(defn- widget-problems
  [id {:keys [main css parameters events]} files]
  (let [at (str "widget " (name id) ": ")]
    (concat
     (cond
       (not (and (symbol? main) (namespace main)))
       [(str at ":main names the view as namespace/var, got " (pr-str main))]

       (nil? (source-of files (symbol (namespace main))))
       [(str at "no source for namespace " (namespace main))])
     (when (and css (not (contains? files css)))
       [(str at ":css names a file this release does not hold: " css)])
     (for [[p {:keys [type]}] parameters :when (not (type-ok? type))]
       (str at "parameter " (name p) " has the unknown type " (pr-str type)))
     (for [[e {:keys [updates]}] events
           u updates :when (not (contains? parameters u))]
       (str at "event " (name e) " updates the undeclared parameter " (name u))))))

(defn- csp-problems
  [csp]
  (concat
   (for [k (keys csp) :when (not (contains? csp-directives k))]
     (str ":csp does not open " (pr-str k) " — it opens " (str/join " " (sort csp-directives))))
   (for [vs (vals csp) v vs :when (not (re-matches #"[\w:/.*'-]+" (str v)))]
     (str ":csp holds an unusable source: " (pr-str v)))))

(defn manifest-problems
  [{:keys [widgets csp] :as manifest} files]
  (cond
    (not (map? manifest)) [(str manifest-file " is not a map")]
    (empty? widgets)      [(str manifest-file " declares no :widgets")]
    :else (vec (concat (csp-problems csp)
                       (mapcat (fn [[id spec]] (widget-problems id spec files)) widgets)))))

(defn- dev-origin?
  [origin]
  (boolean (re-matches #"http://(localhost|127\.0\.0\.1)(:\d+)?" (str origin))))

(defn- csp-value
  [origin {:keys [connect-src img-src media-src font-src frame-src]}]
  (str/join
   "; "
   (cond-> ["default-src 'none'" "form-action 'none'" "base-uri 'none'"
            (if (dev-origin? origin)
              (str "script-src " origin " 'unsafe-eval'")
              (str "script-src " origin runtime-url))
            "style-src 'unsafe-inline'"
            (str/join " " (concat ["img-src" origin "data:" "blob:"] img-src))
            (str/join " " (concat ["font-src" "data:"] font-src))
            (str/join " " (concat ["media-src" origin "data:" "blob:"] media-src))]
     (seq connect-src) (conj (str/join " " (cons "connect-src" connect-src)))
     (seq frame-src)   (conj (str/join " " (cons "frame-src" frame-src))))))

(defn frame-document
  [origin manifest]
  (str "<!doctype html><html><head><meta charset=\"utf-8\">"
       "<meta http-equiv=\"Content-Security-Policy\" content=\""
       (csp-value origin (:csp manifest)) "\">"
       "<script src=\"" origin runtime-url "\"></script>"
       "</head><body><div id=\"root\"></div></body></html>"))

(defn frame-values
  [values]
  (update-vals values
               #(if (and (sequential? %) (> (count %) bridge/max-objects))
                  (vec (take bridge/max-objects %))
                  %)))

(defn emit-effects
  [widget bindings event updates]
  (let [declared (get-in widget [:events event])
        allowed  (set (:updates declared))]
    (cond
      (nil? declared)
      {:refused (str "the widget declares no event " (pr-str event))}

      (seq (remove allowed (keys updates)))
      {:refused (str "event " (pr-str event) " does not update "
                     (str/join " " (map pr-str (remove allowed (keys updates)))))}

      :else
      {:effects (cond-> (vec (for [[k v] updates :when (contains? (:variables bindings) k)]
                               [:set k v]))
                  (contains? (:events bindings) event) (conj [:event event]))})))

(defn binding-problems
  [widget {:keys [variables events]}]
  (concat
   (for [k (keys variables) :when (not (contains? (:parameters widget) k))]
     (str "unknown parameter " (name k)))
   (for [k (keys events) :when (not (contains? (:events widget) k))]
     (str "unknown event " (name k)))))
