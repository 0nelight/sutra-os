(ns ent-lsp.facts
  (:require [clojure.string :as str]
            [ent-lsp.spec :as spec]))

(defn- def-spec-for [spec-defs kind path]
  (some (fn [{:keys [at] :as d}]
          (when (and (= kind (:unit d)) (spec/match? at path))
            d))
        spec-defs))

(def ^:dynamic *authored*
  nil)

(defn- file-owner
  []
  (some-> (:owner *authored*) keyword))

(defn- definitions [spec rel {:keys [data nodes by-path]}]
  (let [kind    (:kind *authored*)
        owner   (:owner *authored*)
        fowner  (file-owner)]
    (for [{:keys [path key? coll loc]} nodes
          :when (and (not key?) coll)
          :let [d (def-spec-for (:definitions spec) kind path)]
          :when d
          :let [v (get-in data path)]
          :when (or (nil? (:defining d)) (and (map? v) (some #(contains? v %) (:defining d))))
          :let [[nm name-node] (if (= :key (:name d))
                                 [(peek path) (by-path [:key path])]
                                 (let [p (into path (:name d))] [(get-in data p) (by-path p)]))]
          :when (some? nm)]
      {:kind           (:kind d)
       :name           nm
       :file           rel
       :path           path
       :loc            loc
       :name-loc       (or (:loc name-node) loc)
       :declared-owner (when-let [o (:owner d)]
                         (let [p (get-in v o)] (if (keyword? p) p fowner)))
       :owned-by       owner
       :extension      (boolean (and owner (= :object (:kind d)) (map? v) (seq (:attributes v))
                                     (every? #{:attributes} (keys v))))
       :derive         (:derive d)
       :unique         (boolean (:unique d))
       :data           v})))

(defn- ref-spec-for [spec-refs kind path]
  (some (fn [{:keys [at key many] :as r}]
          (when (or (nil? (:unit r)) (= kind (:unit r)))
            (cond
              at   (when (spec/match? at path) r)
              many (when (and (>= (count path) 2) (int? (peek path)) (= key (peek (pop path)))) r)
              :else (when (= key (peek path)) r))))
        spec-refs))

(defn- references [spec rel {:keys [nodes]}]
  (let [kind (:kind *authored*)]
    (for [{:keys [path key? value loc]} nodes
          :when (and (not key?) (keyword? value))
          :let [r (ref-spec-for (:references spec) kind path)]
          :when r]
      {:kinds (spec/kinds (:kind r))
       :name  value
       :file  rel
       :path  path
       :loc   loc
       :soft  (boolean (or (:soft r)
                           (when-let [suffix (:soft-when r)]
                             (str/ends-with? (name value) suffix))))})))

(defn- properties [spec rel {:keys [by-path]} defs]
  (let [{:keys [of attributes]} (:properties spec)]
    (for [d defs
          :when (= of (:kind d))
          :let [attrs (get-in d [:data attributes])]
          :when (sequential? attrs)
          [i a] (map-indexed vector attrs)
          :let [p (conj (:path d) attributes i)
                [nm name-path typ] (cond (vector? a)  [(first a) (conj p 0) (second a)]
                                         (keyword? a) [a p nil])]
          :when (keyword? nm)]
      {:kind        :property
       :name        nm
       :object      (:name d)
       :parent-path (:path d)
       :extension   (:extension d)
       :file        rel
       :path        p
       :loc         (:loc (by-path p))
       :name-loc    (:loc (by-path name-path))
       :unique      false
       :data        {:type typ}})))

(defn- scoped-object
  [data path {:keys [up key from link-fk]}]
  (let [base (vec (drop-last up path))]
    (cond key     (peek base)
          from    (get-in data (into base from))
          link-fk (let [l (get-in data base)]
                    (when (vector? l) (get l (if (= :one-to-many (get l 3)) 2 1)))))))

(defn- property-references [spec rel {:keys [data nodes]}]
  (for [{:keys [path key? value loc]} nodes
        :when (and (not key?) (keyword? value))
        :let [r (some #(when (spec/match? (:at %) path) %) (:property-references spec))]
        :when r
        :let [o (scoped-object data path (:object r))]
        :when (keyword? o)]
    {:kinds #{:property} :name value :object o :file rel :path path :loc loc :soft false}))

(defn- writes [spec rel {:keys [data nodes by-path]}]
  (let [{:keys [at op ops object] set-key :set} (get-in spec [:actions :writes])]
    (for [{:keys [path coll]} nodes
          :when (and (= :map coll) (spec/match? at path))
          :let [m (get-in data path)]
          :when (contains? ops (get m op))
          :let [o (get m object)]
          :when (keyword? o)]
      {:file rel :path path :object o :op (get m op) :set (set (get m set-key))
       :loc (:loc (by-path (conj path object)))})))

(defn- extension-references
  [defs]
  (for [d defs :when (:extension d)]
    {:kinds #{:object} :name (:name d) :file (:file d) :path (:path d) :loc (:name-loc d)
     :soft true}))

(defn- synonyms
  [data]
  (when-let [p (file-owner)]
    (let [m (get-in data [:schema :projects p :synonyms])]
      (when (map? m) m))))

(defn- resolve-alias [syn x]
  (if-let [real (and syn (keyword? (:name x)) (get syn (:name x)))]
    (assoc x :name real :alias (:name x))
    x))

(defn extract [spec rel parsed]
  (if (:error parsed)
    {:defs [] :refs [] :writes []}
    (let [syn  (synonyms (:data parsed))
          defs (mapv #(cond->> % (:extension %) (resolve-alias syn)) (definitions spec rel parsed))]
      {:defs    (into defs (properties spec rel parsed defs))
       :refs    (-> (vec (references spec rel parsed))
                    (into (property-references spec rel parsed))
                    (into (extension-references defs))
                    (->> (mapv #(cond->> % (contains? (:kinds %) :object) (resolve-alias syn)))))
       :writes  (vec (writes spec rel parsed))})))
