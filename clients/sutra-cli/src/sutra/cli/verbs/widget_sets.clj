(ns sutra.cli.verbs.widget-sets
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.out :refer [emit! mark say! warn!]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.wire.api :as api])
  (:import [java.io File]))

(defn- beim-aufrufer
  [p]
  (let [f   (io/file ^String p)
        pwd (System/getenv "SUTRA_CALLER_PWD")]
    (if (or (.isAbsolute f) (not (seq pwd))) (.getPath f) (.getPath (io/file pwd ^String p)))))

(defn- release-file?
  [^File root ^File f]
  (let [rel (str/replace (.getPath f) (str (.getPath root) "/") "")]
    (and (.isFile f)
         (not-any? #(or (str/starts-with? % ".") (#{"target" "node_modules"} %))
                   (str/split rel #"/")))))

(defn- files-of
  [dir]
  (let [root (io/file (beim-aufrufer dir))]
    (when-not (.isDirectory root)
      (throw (ex-info (str "kein Ordner: " (.getPath root)) {:dir dir})))
    (into {}
          (comp (filter #(release-file? root %))
                (map (fn [^File f]
                       [(str/replace (.getPath f) (str (.getPath root) "/") "") (slurp f)])))
          (file-seq root))))

(defn- stamp-of
  [dir]
  (let [root (io/file (beim-aufrufer dir))]
    (into #{}
          (comp (filter #(release-file? root %))
                (map (fn [^File f] [(.getPath f) (.lastModified f)])))
          (file-seq root))))

(defmethod step! :widget-set-create [ctx [_ {:keys [name project description]}]]
  (emit! ctx (req! "widget-set create"
                   (api/post! :widget-sets {:name name :project project :description description})))
  nil)

(defmethod step! :widget-set-delete [ctx [_ {:keys [name]}]]
  (req! "widget-set delete" (api/delete! :widget-set {:name name}))
  (emit! ctx {:deleted name})
  nil)

(defmethod step! :widget-set-publish [ctx [_ {:keys [name dir version notes]}]]
  (emit! ctx (req! "widget-set publish"
                   (api/post! :widget-set-releases {:name name}
                              {:version version :notes notes :files (files-of dir)})))
  nil)

(defn- push-draft!
  [ctx nm dir]
  (try
    (req! "widget-set dev" (api/post! :widget-set-releases {:name nm}
                                      {:dev? true :files (files-of dir)}))
    (say! ctx (mark :green "✓" (str "Entwurf aktiv: " (java.time.LocalTime/now))))
    (catch Exception e
      (warn! (mark :red "✗" (ex-message e))))))

(defmethod step! :widget-set-dev [ctx [_ {:keys [name dir stop]}]]
  (if stop
    (do (req! "widget-set dev --stop" (api/delete! :widget-set-dev {:name name}))
        (emit! ctx {:dev-mode :off :widget-set name}))
    (do
      (when-not dir
        (throw (ex-info "widget-set dev <name> <ordner> — oder --stop" {:name name})))
      (say! ctx (str "Dev Mode: jede Verwendung von " name " zeigt dir diesen Ordner."
                     " Beenden: Strg-C, dann widget-set dev " name " --stop"))
      (loop [seen nil]
        (let [now (stamp-of dir)]
          (when (not= now seen) (push-draft! ctx name dir))
          (Thread/sleep 700)
          (recur now)))))
  nil)

(defmethod step! :widget-set-pull [ctx [_ {:keys [name dir version]}]]
  (let [v (or version
              (:latest (:widget-set (req! "widget-set pull"
                                          (api/get! :widget-set {:name name})))))
        _ (when-not v
            (throw (ex-info (str name " hat noch kein Release") {:name name})))
        r (:release (req! "widget-set pull"
                          (api/get! :widget-set-release {:name name :version v})))
        root (io/file (beim-aufrufer dir))]
    (doseq [[k text] (:files r)
            :let [f (io/file root (if (keyword? k) (subs (str k) 1) (str k)))]]
      (io/make-parents f)
      (spit f text))
    (emit! ctx {:widget-set name :version v :dir (.getPath root) :files (count (:files r))}))
  nil)

(def ^:private template-manifest
  "{:widgets
 {:counter
  {:name \"Zähler\"
   :description \"Zählt hoch; der Stand liegt in einer Variablen der App.\"
   :main hello.counter/view
   :parameters {:count {:name \"Stand\" :type :number}}
   :events {:increment {:name \"Hochzählen\" :updates [:count]}}}}}
")

(def ^:private template-source
  "(ns hello.counter
  (:require [sutra.widget :as w]))

(defn view []
  (let [n (w/param :count 0)]
    [:button {:on-click #(w/emit! :increment {:count (inc n)})} n]))
")

(defmethod step! :widget-set-init [ctx [_ {:keys [dir]}]]
  (let [root (io/file (beim-aufrufer dir))
        man  (io/file root "widgets.edn")
        src  (io/file root "src/hello/counter.cljs")]
    (when (.exists man)
      (throw (ex-info (str "hier liegt schon ein Widget Set: " (.getPath man)) {:dir dir})))
    (io/make-parents src)
    (spit man template-manifest)
    (spit src template-source)
    (emit! ctx {:created [(.getPath man) (.getPath src)]}))
  nil)
