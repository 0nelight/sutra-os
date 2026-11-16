(ns sutra.i18n.inline
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.types.table :as tbl]))

(set! *warn-on-reflection* true)

(def locales [:de :en])

(def required-surfaces ["change" "cli" "refusal"])

(def ^:private optional-surfaces ["ui"])

(defn- bundle-path [surface locale] (str "i18n/" surface "." (name locale) ".edn"))

(def ^:private app-root "sutra_ui/apps")

(defn- app-bundles
  []
  (when-let [u (io/resource app-root)]
    (when (= "file" (.getProtocol u))
      (for [d (sort-by #(.getName ^java.io.File %) (.listFiles (io/file (.toURI u))))
            :when (.isDirectory ^java.io.File d)
            :let [prefix (str app-root "/" (.getName ^java.io.File d) "/i18n")]
            :when (every? #(io/resource (str prefix "." (name %) ".edn")) locales)]
        prefix))))

(defn surfaces []
  (into required-surfaces
        (filter (fn [s] (every? #(io/resource (bundle-path s %)) locales)))
        optional-surfaces))

(defn- bundle-paths [locale]
  (concat (for [s (surfaces)] (bundle-path s locale))
          (for [p (app-bundles)] (str p "." (name locale) ".edn"))))

(defn- read-bundle [slurp-path path]
  (let [_    (when-not (io/resource path) (throw (ex-info "i18n bundle missing" {:path path})))
        m    (edn/read-string (slurp-path path))]
    (when-not (and (map? m)
                   (every? qualified-keyword? (keys m))
                   (every? string? (vals m)))
      (throw (ex-info "i18n bundle must map qualified keywords to strings" {:path path})))
    m))

(defn- placeholders [s]
  (set (re-seq #"\{([a-zA-Z0-9-]+)(?::pr)?\}" s)))

(defn- agreed!
  [by-locale]
  (let [all (into #{} (mapcat keys) (vals by-locale))
        [m0 & ms] (vals by-locale)]
    (tbl/holds! "every locale translates every key"
                (for [[l m] by-locale :let [missing (remove (set (keys m)) all)]
                      :when (seq missing)]
                  [l (sort missing)]))
    (tbl/holds! "a placeholder stands in every locale"
                (for [[k v] m0 m ms :when (not= (placeholders v) (placeholders (get m k)))] k))
    (tbl/holds! "a placeholder name is ASCII, or `t` never fills it"
                (for [m (vals by-locale) [k v] m
                      :when (re-find #"\{[A-Za-z0-9_-]*[^\x00-\x7F][A-Za-z0-9_-]{2,}\}" v)]
                  k))
    by-locale))

(defn load-all
  ([] (load-all (comp slurp io/resource)))
  ([slurp-path]
   (agreed! (into {} (for [l locales
                           :let [bundles (map #(read-bundle slurp-path %) (bundle-paths l))]]
                       (do (tbl/holds! "a key lives in one bundle"
                                       (for [[k n] (frequencies (mapcat keys bundles))
                                             :when (> n 1)]
                                         k))
                           [l (apply merge bundles)]))))))

(def ^:private app-index "sutra_ui/apps/all.cljs")

(defn- shadow-slurp
  []
  (try (requiring-resolve 'shadow.resource/slurp-resource)
       (catch java.io.FileNotFoundException _ nil)))

(defmacro bundles []
  (if-let [slurp* (shadow-slurp)]
    (let [env &env]
      (when (io/resource app-index) (slurp* env app-index))
      (load-all #(slurp* env %)))
    (load-all)))

(defmacro refusal-categories
  []
  (let [path "refusal/category.edn"]
    (edn/read-string
     (if-let [slurp* (shadow-slurp)]
       (slurp* &env path)
       (slurp (io/resource path))))))
