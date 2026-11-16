(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(def root
  (str (.getCanonicalPath (io/file (.getParent (io/file *file*)) "..")) "/clients/frontend/src/"))
(def locales ["de" "en"])
(def prune? (some #{"--prune"} *command-line-args*))

(defn- rel [f] (subs (.getPath ^java.io.File f) (count root)))

(def sources
  (->> (file-seq (io/file root "sutra_ui"))
       (filter #(re-find #"\.clj[sc]?$" (.getName ^java.io.File %)))
       (map (fn [f] [(rel f) (slurp f)]))))

(defn- app-of [path] (second (re-find #"^sutra_ui/apps/([^/]+)/" path)))

(defn- bundle-file [app l]
  (if app (str "sutra_ui/apps/" app "/i18n." l ".edn") (str "i18n/ui." l ".edn")))

(defn- bundles [l]
  (cons (bundle-file nil l)
        (for [d (.listFiles (io/file root "sutra_ui/apps"))
              :when (.isDirectory ^java.io.File d)
              :let [p (bundle-file (.getName ^java.io.File d) l)]
              :when (.exists (io/file root p))]
          p)))

(defn- entries
  [text]
  (let [body (-> text str/trim (subs 1) (as-> s (subs s 0 (dec (count s)))))]
    (->> (str/split-lines body)
         (reduce (fn [acc l]
                   (if (re-find #"^ ?:ui/" l)
                     (conj acc [l])
                     (update acc (dec (count acc)) conj l)))
                 [])
         (map (fn [ls] [(keyword (subs (re-find #":ui/[^\s]+" (first ls)) 1)) ls])))))

(defn- render [es]
  (str "{"
       (str/join "\n"
                 (map-indexed (fn [i [_ [l & more]]]
                                (str/join "\n" (cons (str (when (pos? i) " ") (str/triml l))
                                                     more)))
                              (sort-by (comp str first) es)))
       "}\n"))

(def users
  (reduce (fn [m [path text]]
            (let [lit  (map #(keyword "ui" (second %)) (re-seq #":ui/([\w.*+!?<>=-]+)" text))
                  pre  (map second (re-seq #"\(keyword \"ui\" \(str \"([\w-]+)\"" text))]
              (as-> m m
                (reduce #(update %1 [:key %2] (fnil conj #{}) path) m lit)
                (reduce #(update %1 [:prefix %2] (fnil conj #{}) path) m pre))))
          {} sources))

(defn- users-of [k]
  (into (get users [:key k] #{})
        (mapcat (fn [[[t p] paths]]
                  (when (and (= t :prefix) (str/starts-with? (name k) p)) paths)))
        users))

(defn- home
  [k]
  (let [us (users-of k)]
    (cond (empty? us) ::unused
          :else (let [apps (set (map app-of us))]
                  (when (and (= 1 (count apps)) (first apps)) (first apps))))))

(doseq [l locales]
  (let [all     (for [b (bundles l), e (entries (slurp (io/file root b)))] e)
        before  (into {} (map (fn [[k ls]] [k (str/join "\n" ls)])) all)
        placed  (group-by (fn [[k]] (home k)) all)
        unused  (sort (map first (placed ::unused)))
        placed  (cond-> (dissoc placed ::unused)
                  (not prune?) (update nil concat (placed ::unused)))]
    (assert (= (count before) (count all)) (str l ": a key stands in two bundles"))
    (doseq [b (bundles l)] (.delete (io/file root b)))
    (doseq [[app es] placed]
      (spit (io/file root (bundle-file app l)) (render es)))
    (when (= l (first locales))
      (println (count all) "keys," (count unused) (if prune? "unused, deleted:" "unused:"))
      (doseq [k unused] (println " " k))
      (doseq [[app es] (sort-by (comp str key) placed)]
        (println (format "  %-14s %4d" (or app "ui (shell)") (count es)))))))
