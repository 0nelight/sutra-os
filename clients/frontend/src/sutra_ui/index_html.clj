(ns sutra-ui.index-html
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private marker
  "<!-- sutra:modules -->")

(defn- eager-modules
  [manifest]
  (let [by-id (into {} (map (juxt :module-id identity)) manifest)
        keep  (loop [seen #{} [id & more] [:main]]
                (if-not id
                  seen
                  (recur (conj seen id)
                         (into (vec more)
                               (remove seen)
                               (:depends-on (get by-id id))))))]
    (filter #(contains? keep (:module-id %)) manifest)))

(defn- script-tags
  [modules]
  (->> modules
       (map
        (fn [{:keys [output-name]}]
          (str "  <script src=\"/js/" output-name "\""
               " onerror=\"window.__sutraBundleFailed && window.__sutraBundleFailed()\"></script>")))
       (str/join "\n")))

(defn render
  [output-dir]
  (let [manifest (io/file output-dir "manifest.edn")
        template (io/file "index.html")]
    (when (and (.exists manifest) (.exists template))
      (let [tpl (slurp template)]
        (when-not (str/includes? tpl marker)
          (throw (ex-info (str "index.html template has no " marker " line — "
                               "nothing would load the bundle")
                          {:template (.getPath template)})))
        (str/replace tpl marker
                     (script-tags (eager-modules (edn/read-string (slurp manifest)))))))))

(defn write!
  [output-dir]
  (when-let [html (render output-dir)]
    (let [dst (io/file (.getParentFile (io/file output-dir)) "index.html")]
      (when-not (and (.exists dst) (= html (slurp dst)))
        (io/make-parents dst)
        (spit dst html))
      dst)))

(defn hook
  {:shadow.build/stage :flush}
  [build-state & _]
  (write! (get-in build-state [:shadow.build/config :output-dir]))
  build-state)
