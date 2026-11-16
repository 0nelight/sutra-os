(ns ent-lsp.workspace
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [ent-lsp.analysis :as an]
            [ent-lsp.checks :as checks]
            [ent-lsp.facts :as facts]
            [ent-lsp.grammar :as grammar]
            [ent-lsp.online :as online]
            [ent-lsp.read :as read]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.project-export :as pe]))

(defn rel-path [root p] (str (fs/relativize (fs/absolutize root) (fs/absolutize p))))

(def marker
  ".sutra-projection.edn")

(defn in-scope?
  [ws rel]
  (authoring/file? (:formats ws) rel))

(defn scan
  [ws]
  (->> (file-seq (fs/file (:root ws)))
       (filter #(.isFile ^java.io.File %))
       (map #(rel-path (:root ws) %))
       (filter #(in-scope? ws %))
       sort
       vec))

(defn- projections-of
  [files]
  (apply merge (keep (fn [[_ e]]
                       (let [d (get-in e [:raw :data])]
                         (when (map? d) (or (get-in d [:schema :projections])
                                            (when-not (contains? d :schema) (:projections d))))))
                     files)))

(defn- formats-of
  [{:keys [filename files]}]
  (if filename
    (registry/formats filename)
    (pe/checkout-formats {:projections (or (not-empty (projections-of files)) standard/projections)}
                         (keys files))))

(defn- judge
  [fmts rel doc]
  (try (when-let [r (authoring/judge fmts rel (constantly doc))]
         (let [k (get fmts (:format r))]
           (assoc (select-keys r [:format :owner :rule]) :envelope (:envelope k) :kind (:kind k))))
       (catch clojure.lang.ExceptionInfo e {:error (ex-message e)})))

(defn- read-raw [fmts rel text]
  (let [lang (authoring/language-of fmts rel)]
    (if (and lang (not= :edn lang))
      (try (read/from-data (second (authoring/document fmts rel text)))
           (catch Exception e {:error {:message (ex-message e) :row 1 :col 1}}))
      (read/parse text))))

(defn- load-entry
  [spec fmts rel text prev]
  (let [raw     (read-raw fmts rel text)
        judged  (when-not (:error raw) (judge fmts rel (:data raw)))
        ok      (when (and judged (not (:error judged))) judged)
        parsed  (if (and ok (empty? (:envelope ok))) (read/under-schema raw) raw)]
    {:text     text
     :kind     (:kind ok)
     :raw      raw
     :parsed   parsed
     :authored judged
     :facts    (if (:error parsed)
                 (or (:facts prev) {:defs [] :refs []})
                 (binding [facts/*authored* (or ok {})]
                   (facts/extract spec rel parsed)))}))

(defn facts-of
  [ws rel text]
  (:facts (load-entry (:spec ws) (:formats ws) rel text nil)))

(defn- rejudge
  [ws]
  (let [fmts (formats-of ws)]
    (if (= fmts (:formats ws))
      ws
      (assoc ws :formats fmts
             :files (into {} (for [[rel e] (:files ws)]
                               [rel (assoc (load-entry (:spec ws) fmts rel (:text e) e)
                                           :indexed? (get e :indexed? true))]))))))

(defn open-workspace
  [spec root]
  (let [m        (fs/file root marker)
        filename (when (fs/exists? m) (:filename (edn/read-string (slurp m))))
        paths    (->> (file-seq (fs/file root))
                      (filter #(.isFile ^java.io.File %))
                      (map #(rel-path root %))
                      sort)
        load     (fn [fmts]
                   {:root     (str (fs/absolutize root))
                    :spec     spec
                    :filename filename
                    :formats  fmts
                    :files    (into {}
                                    (for [rel paths :when (authoring/file? fmts rel)]
                                      [rel
                                       (load-entry spec fmts rel (slurp (fs/file root rel))
                                                   nil)]))})
        ws       (load (formats-of {:filename filename :files (zipmap paths (repeat nil))}))
        fmts     (formats-of ws)]
    (if (= fmts (:formats ws)) ws (load fmts))))

(defn set-text
  ([ws rel text] (set-text ws rel text true))
  ([ws rel text indexed?]
   (rejudge (update-in ws [:files rel] #(assoc (load-entry (:spec ws) (:formats ws) rel text %)
                                               :indexed? indexed?)))))

(defn reload-from-disk [ws rel]
  (let [f (fs/file (:root ws) rel)]
    (if (fs/exists? f) (set-text ws rel (slurp f)) (update ws :files dissoc rel))))

(defn- merge-schemas [schemas]
  (apply merge-with (fn [a b] (cond (and (map? a) (map? b)) (merge a b)
                                    (and (sequential? a) (sequential? b)) (into (vec a) b)
                                    :else b))
         schemas))

(defn- live-table [files online]
  (let [ents (for [[_ {:keys [kind parsed]}] files
                   :when (and (nil? kind) (not (:error parsed)))]
               (get-in parsed [:data :schema]))]
    (grammar/merge-tables
     [(:table online)
      (grammar/symbol-table (cons (grammar/lower (merge-schemas ents))
                                  (keep :lean (vals files))))])))

(defn set-online
  [ws online]
  (-> ws
      (assoc :online online)
      (update :files update-vals #(dissoc % :checked-text))))

(defn- platform-findings [live rel {:keys [kind parsed facts]}]
  (concat (grammar/check rel kind parsed)
          (grammar/resolve-findings rel kind parsed facts live)))

(defn analyze
  [ws]
  (let [spec      (:spec ws)
        leaned    (into {}
                        (for [[rel e] (:files ws)]
                          [rel (if (or (= (:checked-text e) (:text e)) (nil? (:kind e)))
                                 e
                                 (assoc e
                                        :lean (grammar/lean-schema rel (:kind e) (:parsed e))))]))
        live      (delay (live-table leaned (:online ws)))
        files     (into {} (for [[rel e] leaned]
                             [rel (if (= (:checked-text e) (:text e))
                                    e
                                    (assoc e :checked-text (:text e)
                                           :platform (vec (platform-findings @live rel e))))]))
        all-facts (into {} (for [[rel e] files]
                             [rel (if (get e :indexed? true)
                                    (:facts e)
                                    (update (:facts e) :defs
                                            (partial mapv #(assoc % :draft true))))]))
        all-facts (cond-> all-facts
                    (:online ws) (assoc online/file (online/facts-beside (:online ws) all-facts)))
        idx       (an/build-index spec all-facts)
        read-errs (for [[rel {:keys [parsed]}] files :when (:error parsed)
                        :let [{:keys [row col message]} (:error parsed)]]
                    {:rule :lesefehler :file rel
                     :loc {:row row :col col :end-row row :end-col (inc col)}
                     :vars {:message message}})
        first-char {:row 1 :col 1 :end-row 1 :end-col 2}
        undeclared (for [[rel {:keys [authored parsed]}] files
                         :when (not (:error parsed))
                         :let [root? (not (str/includes? rel "/"))]
                         f (cond
                             (:error authored)
                             [{:rule :format-unpassend :file rel :loc first-char
                               :vars {:message (:error authored)}}]
                             (and root? (nil? authored))
                             [{:rule :nicht-deklariert :file rel :loc first-char
                               :vars {:file rel}}])]
                     f)
        findings  (->> (concat read-errs undeclared
                               (mapcat :platform (vals files))
                               (checks/run-rules spec idx))
                       (map #(an/complete spec %))
                       (group-by :file))]
    (assoc ws :files files :index idx
           :findings (into {} (for [rel (keys files)] [rel (get findings rel [])])))))
