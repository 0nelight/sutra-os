(ns sutra.parser.authoring
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- kw [x] (if (keyword? x) x (keyword (str x))))

(defn- normalize [d]
  (cond-> d
    (:language d) (update :language kw)
    (:owner d)    (update :owner #(mapv kw %))
    (:sections d) (update :sections #(mapv kw %))
    (:home-for d) (update :home-for #(mapv kw %))
    (:envelope d) (update :envelope #(mapv kw %))
    (:kind d)     (update :kind kw)))

(defn formats
  [registry texts]
  (into {}
        (for [[id d] texts
              :let [d (normalize d)]]
          [id (assoc d ::language (get-in registry [:languages (:language d)])
                     ::kind (some->> (:kind d) (get (:kinds registry))))])))

(defn- glob->re [g]
  (re-pattern (str/join (for [part (re-seq #"\*\*|\*|[^*]+" g)]
                          (case part
                            "**" ".*"
                            "*"  "[^/]*"
                            (java.util.regex.Pattern/quote part))))))

(defn matching-glob [decl path]
  (some #(when (re-matches (glob->re %) (str path)) %) (:name decl)))

(defn file?
  [formats path]
  (boolean (some #(matching-glob % path) (vals formats))))

(defn- names-file?
  [glob]
  (str/includes? (subs glob (inc (or (str/last-index-of glob "/") -1))) "*"))

(defn base-name
  [glob path]
  (let [p      (str path)
        file   (subs p (inc (or (str/last-index-of p "/") -1)))
        suffix (subs glob (inc (or (str/last-index-of glob "*") -1)))]
    (if (and (names-file? glob) (seq suffix) (str/ends-with? file suffix))
      (subs file 0 (- (count file) (count suffix)))
      file)))

(def ^:private identifier #"[a-z][a-z0-9-]*")

(defn- owner-by
  [rule decl base body]
  (let [ps (keys (:projects body))]
    (case rule
      :declared  (when (seq ps) {:project (when (= 1 (count ps)) (name (kw (first ps))))})
      :facet     (when (empty? ps)
                   (if (:facet decl)
                     (when (re-matches identifier base) {:project base})
                     (when-let [[_ p] (re-matches #"([a-z][a-z0-9-]*)\.([^./]+)" base)]
                       {:project p})))
      :file-name (when (and (empty? ps) (re-matches identifier base)) {:project base})
      :repository (let [r (str/replace base "_" "-")] {:project (when (re-matches identifier r) r)})
      nil)))

(defn owner-field
  [decl-or-formats]
  (let [repo? #(some #{:repository} (:owner %))]
    (if (if (contains? decl-or-formats :name) (repo? decl-or-formats)
            (some repo? (vals decl-or-formats)))
      :repository
      :project)))

(defn owner
  [decl path body]
  (cond
    (:kind decl) {:project nil :rule :unit}
    (empty? (:owner decl)) {:project nil :rule :free}
    :else
    (let [glob (matching-glob decl path)
          base (base-name glob path)]
      (some (fn [r] (some-> (owner-by r decl base body) (assoc :rule r))) (:owner decl)))))

(defn- specificity
  [glob]
  (count (str/replace glob "*" "")))

(defn- matching
  [formats path]
  (let [ms (keep (fn [[id d]] (some->> (matching-glob d path) (vector id d))) formats)
        top (when (seq ms) (apply max (map (comp specificity peek) ms)))]
    (filterv #(= top (specificity (peek %))) ms)))

(defn- naming [formats path]
  (keep (fn [[_ d g]] (when (names-file? g) [d g])) (matching formats path)))

(defn facet
  [formats path]
  (some (fn [[d g]]
          (or (:facet d) (second (re-matches #"[a-z][a-z0-9-]*\.([^./]+)" (base-name g path)))))
        (naming formats path)))

(defn implied-project
  [formats path]
  (some (fn [[_ g]] (let [b (base-name g path)]
                      (or (second (re-matches #"([a-z][a-z0-9-]*)\.[^./]+" b))
                          (re-matches identifier b))))
        (naming formats path)))

(defn- candidates [formats path]
  (->> (matching formats path)
       (map (fn [[id d]] (clojure.lang.MapEntry/create id d)))
       (sort-by (fn [[id d]] [(- (count (:envelope d))) (or (:order d) 100) (name id)]))))

(defn- body-of [decl doc path]
  (let [env (:envelope decl)]
    (cond
      (::kind decl) {:schema (:schema ((:parse (::kind decl)) doc path)) :more {}}
      (seq env)  (when (and (map? doc) (some? (get-in doc env)))
                   {:schema (get-in doc env)
                    :more (if (= 1 (count env)) (dissoc doc (first env)) {})})
      (nil? doc) {:schema {} :more {}}
      (map? doc) {:schema doc :more {}})))

(defn- parse-in [decl path text]
  (let [{:keys [parse]} (or (::language decl)
                            (refusal/refuse! :refusal.parser/file-language-unknown
                                             {:kind ::unknown-language :language (:language decl)
                                              :path path}))]
    (try (parse text)
         (catch Exception e
           (refusal/refuse! :refusal.parser/file-unreadable {:kind ::unreadable :path path}
                            {:reason (ex-message e)} e)))))

(defn judge
  [formats path doc-in]
  (when-let [cands (seq (candidates formats path))]
    (or (some (fn [[id d]]
                (when-let [{:keys [schema] :as b} (body-of d (doc-in d) path)]
                  (when-let [o (owner d path schema)]
                    (assoc b :format id :owner (:project o) :rule (:rule o)))))
              cands)
        (refusal/refuse! :refusal.parser/no-format {:kind ::no-format :path path}
                         {:tried (str/join ", " (map (comp name key) cands))}))))

(defn unit-kind
  [formats path]
  (some-> (first (candidates formats path)) val :kind))

(defn read-file
  [formats path text]
  (let [parse (memoize (fn [_lang decl] (parse-in decl path text)))]
    (judge formats path #(parse (:language %) %))))

(defn document
  [formats path text]
  (when-let [[_ d] (first (candidates formats path))]
    [(:language d) (parse-in d path text)]))

(defn allows?
  [formats id section]
  (let [ss (:sections (get formats id))]
    (or (empty? ss) (boolean (some #{section} ss)))))

(defn print-file
  [formats id body more]
  (let [decl (or (get formats id)
                 (refusal/refuse! :refusal.parser/unknown-format
                                  {:kind ::unknown-format :format id}))
        env  (:envelope decl)
        doc  (if (seq env) (assoc-in (or more {}) env body) body)]
    ((:print (or (::language decl)
                 (refusal/refuse! :refusal.parser/unknown-language
                                  {:kind ::unknown-language :language (:language decl)})))
     doc)))

(defn- by-order [decls]
  (sort-by (fn [[id d]] [(or (:order d) 100) (name id)]) decls))

(defn default-format
  [formats path]
  (ffirst (by-order (map (fn [[id d]] [id d]) (matching formats path)))))

(defn- homes-of
  [formats pred section]
  (let [pick (fn [p]
               (not-empty (by-order (filter (fn [[_ d]] (and (pred d) (p (set (:home-for d)))))
                                            formats))))]
    (or (pick #(contains? % section)) (pick #(contains? % :*)))))

(defn- project-kind? [d] (boolean (seq (:owner d))))

(defn- one-wildcard [d] (first (filter #(= 1 (count (re-seq #"\*" %))) (:name d))))

(defn home
  [formats section]
  (some (fn [[_ d]] (first (remove #(str/includes? % "*") (:name d))))
        (homes-of formats (complement project-kind?) section)))

(defn project-home
  [formats project section]
  (some (fn [[_ d]] (some-> (one-wildcard d) (str/replace "*" (name project))))
        (homes-of formats project-kind? section)))

(defn project-texts
  [formats project]
  (vec (sort (distinct (for [[_ d] formats
                             :when (project-kind? d)
                             :let [w (one-wildcard d)]
                             :when w]
                         (str/replace w "*" (name project)))))))

(defn problems
  [formats ownable]
  (let [named   (fn [id] (str "kind " (name id)))
        by-glob (group-by first (for [[id d] formats, g (:name d)] [g id d]))]
    (vec
     (concat
      (for [[id d] formats
            :let [hf (set (:home-for d)) ss (set (:sections d))]
            :when (and (seq hf) (seq ss) (or (hf :*) (seq (remove ss hf))))]
        (str (named id) ": :home-for " (vec (sort (if (hf :*) [:*] (remove ss hf))))
             " is not among its :sections " (vec (sort ss))))
      (for [[id d] formats
            :when (and (project-kind? d) (seq (:home-for d)))
            :let
            [bad
             (cond (nil? (one-wildcard d))
                   "has no pattern with exactly one wildcard to put the project in"
                   (not
                    (some #{:facet :file-name :repository}
                          (:owner
                           d))) "does not let the name say the project (:owner needs :facet, :file-name or :repository) — a new text could not be read back"
                   ((set (:home-for d)) :*)
                   "cannot be the home of every section ([:*]) — a project owns only some")]
            :when bad]
        (str (named id) ": a project home " bad))
      (for [[id d] formats
            :when (project-kind? d)
            :let [field (owner-field d)
                  bad   (seq (remove (conj (set (get ownable field)) :*)
                                     (concat (:sections d) (:home-for d))))]
            :when bad]
        (str (named id) ": a " (name field) "'s text cannot hold " (vec (sort (distinct bad)))
             " — no " (name field) " owns them"))
      (when (< 1 (count (distinct (map owner-field (filter project-kind? (vals formats))))))
        ["the kinds own by project and by repository at once — a node's text would depend on which it has"])
      (for [section (sort (set (mapcat :home-for (filter project-kind? (vals formats)))))
            :let [ks (filter (fn [[_ d]] (and (project-kind? d) (some #{section} (:home-for d))))
                             formats)]
            :when (< 1 (count ks))]
        (str "section " (name section) " has " (count ks) " project homes ("
             (str/join ", " (sort (map (comp name key) ks))) ") — a new one could go to either"))
      (for [[g entries] (sort-by key by-glob)
            :let [shape (fn [[_ _ d]] (select-keys d [:sections :home-for :facet :kind]))]
            :when (< 1 (count (distinct (map shape entries))))]
        (str "pattern " g " is read by " (str/join ", " (sort (map (comp name second) entries)))
             ", which disagree on :sections/:home-for/:facet/:kind — which one reads a text"
             " would depend on its content"))))))

(defn language-of
  [formats path]
  (some-> (first (candidates formats path)) val :language))

(defn unit-path
  [formats kind nm]
  (some (fn [[_ d]] (some #(when (= 1 (count (re-seq #"\*" %))) (str/replace % "*" nm)) (:name d)))
        (by-order (filter (fn [[_ d]] (= kind (:kind d))) formats))))

(defn sibling?
  [formats path]
  (boolean (some (fn [d] (some #(re-matches (glob->re %) (str path)) (:siblings d)))
                 (vals formats))))
