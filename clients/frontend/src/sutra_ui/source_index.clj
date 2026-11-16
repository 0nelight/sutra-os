(ns sutra-ui.source-index
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private wurzel "src")
(def ^:private repo-wurzel "clients/frontend/src")
(def ^:private je-klasse
  40)

(def ^:private stil-datei "src/sutra_ui/style.cljs")

(def ^:private hiccup-klassen #":[a-z][a-z0-9]*((?:\.[A-Za-z][\w-]*)+)")
(def ^:private text #"\"([^\"\n]*)\"")
(def ^:private eigene-klasse #"^(?:[a-z][a-z0-9]*-)+[a-z0-9_-]*$|^[a-z][a-z0-9-]*__[a-z0-9_-]+$")

(defn stil-klassen
  [text]
  (into {} (for [[_ n v] (re-seq #"\(def\s+([\w-]+)\s+\"([^\"]*)\"" text)]
             [n (vec (remove str/blank? (str/split v #"\s+")))])))

(defn klassen-der-zeile
  ([zeile] (klassen-der-zeile {} zeile))
  ([stil zeile]
  (concat
   (mapcat (fn [[_ n]] (get stil n)) (re-seq #"\bstyle/([\w-]+)" zeile))
   (mapcat (fn [[_ k]] (remove str/blank? (str/split k #"\."))) (re-seq hiccup-klassen zeile))
   (when (or (str/includes? zeile ":class") (str/includes? zeile "__"))
     (for [[_ t] (re-seq text zeile)
           wort (str/split t #"\s+")
           :when (re-matches eigene-klasse wort)]
       (str/replace wort #"--$" ""))))))

(defn- quellen []
  (->> (file-seq (io/file wurzel))
       (filter #(re-find #"\.clj[cs]$" (.getName %)))
       (sort-by #(.getPath %))))

(defn index
  ([files] (index (let [f (io/file stil-datei)] (if (.exists f) (stil-klassen (slurp f)) {}))
                  files))
  ([stil files]
  (into
   (sorted-map)
   (filter (fn [[_ stellen]] (<= (count stellen) je-klasse)))
   (reduce
    (fn [idx ^java.io.File f]
      (let [rel (str repo-wurzel "/" (subs (.getPath f) (inc (count wurzel))))]
        (reduce (fn [idx [i zeile]]
                  (reduce (fn [idx k]
                            (let [stelle (str rel ":" (inc i))
                                  alle (get idx k [])]
                              (if (some #{stelle} alle)
                                idx
                                (assoc idx k (conj alle stelle)))))
                          idx
                          (klassen-der-zeile stil zeile)))
                idx
                (map-indexed vector (str/split-lines (slurp f))))))
    {}
    files))))

(defn hook
  {:shadow.build/stage :flush}
  [build-state & _]
  (let [ziel (io/file (get-in build-state [:shadow.build/config :output-dir]) "quellen.edn")]
    (when (or (= :release (:shadow.build/mode build-state)) (not (.exists ziel)))
      (io/make-parents ziel)
      (spit ziel (pr-str (index (quellen))))))
  build-state)
