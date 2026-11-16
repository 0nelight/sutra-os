(ns ent-lsp.analysis
  (:require [clojure.string :as str]
            [ent-lsp.spec :as spec]
            [sutra.parser.ent-graph :as graph]
            [sutra.parser.names :as sutra-names]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.synonyms :as synonyms]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]))

(defn show [x] (cond (keyword? x) (str x) (nil? x) "?" :else (pr-str x)))

(defn- legacy? [spec rel] (contains? (set (get-in spec [:layout :legacy])) rel))

(defn- path-project
  [spec projects rel]
  (let [segs (str/split rel #"/")]
    (when (and (> (count segs) 1) (not (legacy? spec rel)))
      (let [p (keyword (first segs))] (when (contains? projects p) p)))))

(defn- derived-via
  [{:keys [derive data]}]
  (when-let [{:keys [in ops]} derive]
    (let [children #(if (map? %) (vals %) (seq %))
          cands    (if in (tree-seq coll? children (get-in data in)) [data])]
      (some (fn [m] (when (and (map? m)
                               (or (nil? ops) (contains? ops (:op m)))
                               (keyword? (:object-type m)))
                      (:object-type m)))
            cands))))

(defn- project-entries [defs]
  (for [d defs :when (= :project (:kind d))] [(:name d) (:data d)]))

(defn- project-imports
  [defs]
  (-> (graph/project-imports (project-entries defs))
      (update-keys keyword)
      (update-vals #(into #{} (map keyword) %))))

(defn- mark-extensions
  [defs]
  (let [proj    (fn [d] (or (:declared-owner d) (some-> (:owned-by d) keyword)))
        part    (fn [d] [d (cond-> (:data d) (map? (:data d)) (assoc :project (proj d)))])
        imports (graph/project-imports (project-entries defs))
        extends (into #{}
                      (mapcat (fn [[_ ds]] (map first (graph/extensions (map part ds) imports))))
                      (group-by (juxt :kind :name) (remove :object defs)))]
    (mapv #(cond-> % (contains? extends %) (assoc :extends true)) defs)))

(defn- workspace-body
  [spec defs]
  (let [tag-of (fn [d] (get-in spec [:ownership :tags (:kind d)]))
        owned  (fn [d]
                 (if (and (map? (:data d)) (:declared-owner d))
                   (assoc (:data d) :project (:declared-owner d)) (:data d)))]
    (sugar/derive-projects
     (reduce (fn [b d]
               (let [tag (tag-of d) section (forms/author-section-of tag)]
                 (cond
                   (nil? section) b
                   (= :link-type tag) (update b :links (fnil conj []) (vec (:data d)))
                   (keyword? (:name d)) (assoc-in b [section (:name d)] (owned d))
                   :else b)))
             {}
             (remove #(or (:draft %) (:object %) (:extends %)) defs)))))

(defn- sutra-owners
  [spec body]
  (fn [d]
    (let [tag (get-in spec [:ownership :tags (:kind d)])]
      (some-> (if (= :link-type tag)
                (some (fn [[n _ _ _ o]] (when (= n (:name d)) (:project o))) (:links body))
                (get-in body [(forms/author-section-of tag) (:name d) :project]))
              keyword))))

(defn build-index [spec facts-by-file]
  (let [defs         (mark-extensions (vec (mapcat :defs (vals facts-by-file))))
        refs         (vec (mapcat :refs (vals facts-by-file)))
        by-key       (group-by (juxt :kind :name) (remove #(or (:draft %) (:extends %)) defs))
        project-defs (filter #(= :project (:kind %)) (remove :draft defs))
        projects     (set (map :name project-defs))
        imports      (project-imports project-defs)
        exports      (reduce (fn [m d]
                               (if (contains? (:data d) :exports)
                                 (update m (:name d) (fnil into #{}) (get-in d [:data :exports]))
                                 m))
                             {} project-defs)
        declared     (fn [kind nm] (some :declared-owner (by-key [kind nm])))
        resolve-def  (let [sutra-owner (sutra-owners spec (workspace-body spec defs))]
                       (fn [d]
                         (let [via (derived-via d)
                               viaown (or (sutra-owner d)
                                          (when via (declared (get-in d [:derive :kind]) via)))
                               owner (or (:declared-owner d) viaown)]
                           (cond-> (assoc d :via via :owner owner
                                          :owner-source
                                            (cond (:declared-owner d) :declared viaown :derived))
                             (nil? owner)
                               (assoc :suggest (path-project spec projects (:file d)))))))
        defs         (mapv resolve-def defs)
        object-at    (into {} (for [d defs :when (= :object (:kind d))] [[(:file d) (:path d)] d]))
        defs         (mapv (fn [d]
                             (if-let [o (when (:parent-path d)
                                          (object-at [(:file d) (:parent-path d)]))]
                               (assoc d :owner (:owner o) :owner-source (:owner-source o))
                               d))
                           defs)
        props        (group-by (juxt :object :name) (filter :object (remove :draft defs)))
        by-key       (group-by (juxt :kind :name) (remove #(or (:draft %) (:extends %)) defs))
        by-name      (group-by :name (remove :draft defs))
        defs-by-file (group-by :file defs)
        owned-at     (into {} (for [d defs :when (:owner d)] [[(:file d) (:path d)] d]))
        site-def     (fn [r] (loop [p (:path r)]
                               (or (owned-at [(:file r) p])
                                   (when (seq p) (recur (pop p))))))
        refs         (mapv (fn [r]
                             (let [sd (site-def r)]
                               (assoc r
                                      :site (:owner sd)
                                      :targets (cond (:object r) (props [(:object r) (:name r)])
                                                     (contains? (:kinds r) :any) (by-name (:name r))
                                                     :else
                                                       (mapcat #(by-key [% (:name r)])
                                                               (:kinds r))))))
                           refs)]
    {:defs defs :refs refs :by-key by-key :by-name by-name :props props
     :defs-by-file defs-by-file :refs-by-file (group-by :file refs)
     :writes (vec (mapcat :writes (vals facts-by-file)))
     :projects projects :imports imports :exports exports
     :names (sutra-names/index (forms/author (workspace-body spec defs)))}))

(defn entity-map
  [d]
  (let [v (:data d)
        m (if (vector? v) (get v 4) v)]
    (when (map? m) m)))

(defn status-of [spec d]
  (or (:status (entity-map d)) (get-in spec [:status :default])))

(defn same-symbol? [a b]
  (= ((juxt :kind :name :object) a) ((juxt :kind :name :object) b)))

(defn primary-key
  [spec idx object]
  (let [k (get-in (first (get-in idx [:by-key [:object object]]))
                  (into [:data] (get-in spec [:properties :primary-key])))]
    (cond (keyword? k) [k]
          (and (sequential? k) (seq k)) (vec k))))

(defn reachable
  [spec {:keys [imports]} p]
  (if (get-in spec [:ownership :transitive-imports])
    (loop [seen #{p} todo [p]]
      (if-let [x (first todo)]
        (let [nx (remove seen (imports x))] (recur (into seen nx) (into (subvec todo 1) nx)))
        seen))
    (conj (set (imports p)) p)))

(defn import-cycle?
  [{:keys [imports]} p q]
  (loop [seen #{} todo [q]]
    (if-let [x (first todo)]
      (cond (= x p) true
            (seen x) (recur seen (subvec todo 1))
            :else (recur (conj seen x) (into (subvec todo 1) (imports x))))
      false)))

(defn- finding [rule file loc vars]
  {:rule rule :file file :loc loc :vars vars})

(defn- governed?
  [spec kind]
  (if-let [tag (get-in spec [:ownership :tags kind])]
    (contains? member/member-tags tag)
    true))

(defn- rule-unbekannt [_spec {:keys [refs defs-by-file]}]
  (for [r refs
        :when (and (not (:soft r)) (not (:object r)) (empty? (:targets r))
                   (not (contains? (:kinds r) :any)))]
    (finding :unbekannt (:file r) (:loc r)
             (cond-> {:kind (str/join "/" (map name (sort (:kinds r)))) :name (show (:name r))}
               (and (contains? (:kinds r) :object) (:site r) (keyword? (:name r)))
               (assoc :data (cond-> {:skeleton (:name r) :project (:site r)}
                              (some :owned-by
                                    (defs-by-file (:file r))) (assoc :into (:file r))))))))

(defn- rule-visibility [spec {:keys [refs exports] :as idx}]
  (let [global (get-in spec [:ownership :global-projects])]
    (for [r refs
          :let [t (first (:targets r)) q (:owner t) p (:site r)]
          :when (and t q p (not= p q) (not (:object t)) (governed? spec (:kind t))
                     (not (contains? global q)))
          :let [vars {:name (show (:name r)) :owner (show q) :site (show p)}
                f (cond
                    (not (contains? (reachable spec idx p) q))
                    (finding :import-fehlt (:file r) (:loc r) (assoc vars :data {:site p :owner q}))
                    (and (contains? exports q) (not (contains? (exports q) (:name r))))
                    (finding :export-fehlt (:file r) (:loc r) vars))]
          :when f]
      f)))

(defn- rule-orphan-link [_spec {:keys [defs by-key]}]
  (for [d defs
        :when (= :link (:kind d))
        :let [[_ from to] (:data d)
              a (some :owner (by-key [:object from]))
              b (some :owner (by-key [:object to]))
              p (:owner d)]
        :when (and p a b (not= p a) (not= p b))]
    (finding :verwaister-link (:file d) (:name-loc d)
             {:name (show (:name d)) :site (show p) :from (show from) :to (show to)
              :owners (str/join " oder " (map show (distinct [a b])))})))

(defn- where [d] (str (:file d) ":" (get-in d [:name-loc :row])))

(defn- attr-key [a]
  (some-> (if (map? a) (:attribute-name a) (when (sequential? a) (first a))) name))

(defn- field-clashes
  [a b]
  (if (and (map? a) (map? b))
    (concat (for [[k v] a
                  :when (and (not (#{:attributes :project} k)) (contains? b k) (not= v (get b k)))]
              (show k))
            (let [by #(into {} (map (juxt attr-key identity)) (:attributes %))
                  bm (by b)]
              (for [[n x] (by a) :when (and (contains? bm n) (not= x (bm n)))]
                (str ":attributes/" n))))
    (when (not= a b) ["…"])))

(defn- rule-duplicates
  [_spec {:keys [by-key]}]
  (for [[[kind nm] ds] by-key
        :when (and (> (count ds) 1) (:unique (first ds)))
        d ds
        :let [others (remove #{d} ds)
              rivals (remove #(= (:owner d) (:owner %)) others)
              elsewhere (remove :owned-by rivals)]
        f (cond
            (and (:owned-by d) (seq elsewhere))
            (let [owner (some :owner elsewhere)]
              [(finding :projekttext-gibt-es-schon (:file d) (:name-loc d)
                        {:kind (name kind) :name (show nm) :owner (show owner)
                         :site (show (:owner d))
                         :others (str/join ", " (map where elsewhere))
                         :data {:site (:owner d) :owner owner :file (:file d) :path (:path d)}})])

            (seq rivals)
            [(finding :doppelte-definition (:file d) (:name-loc d)
                      {:kind (name kind) :name (show nm)
                       :others (str/join ", " (map where rivals))})]

            :else
            (for [o others
                  :let [fs (field-clashes (:data d) (:data o))]
                  :when (seq fs)]
              (finding :feld-widerspruch (:file d) (:name-loc d)
                       {:kind (name kind) :name (show nm) :fields (str/join ", " fs)
                        :others (where o)})))]
    f))

(defn- rule-projekttext-author [_spec {:keys [defs by-key]}]
  (for [d defs
        :when (and (:owned-by d) (= :object (:kind d)) (not (:extends d))
                   (= 1 (count (by-key [:object (:name d)]))))]
    (finding :projekttext-autorenort (:file d) (:name-loc d)
             {:kind "Object-Type" :name (show (:name d)) :site (show (:owner d))})))

(defn- rule-owner [spec {:keys [defs]}]
  (for [d defs
        :when (and (not (:object d)) (governed? spec (:kind d)) (not= :declared (:owner-source d)))]
    (if (:owner d)
      (finding :besitzer-abgeleitet (:file d) (:name-loc d)
               {:kind (name (:kind d)) :name (show (:name d)) :owner (show (:owner d))
                :via (show (:via d))})
      (finding :ohne-besitzer (:file d) (:name-loc d)
               (cond-> {:kind (name (:kind d)) :name (show (:name d))
                        :suggest (if-let [p (:suggest d)]
                                   (str "; der Ordner legt " (show p) " nahe") "")}
                 (:suggest d) (assoc :data {:project (:suggest d)}))))))

(defn layout-vars [d]
  (let [v (:data d)
        src (get v :external-source)]
    {:project     (:owner d)
     :name        (if (string? (:name d)) (str/lower-case (:name d)) (:name d))
     :object-type (:via d)
     :from        (when (= :link (:kind d)) (second v))
     :source      (cond (= :writeback (:kind d)) (get-in d [:path 2])
                        (map? src) (:datasource src))
     :table       (cond (= :writeback (:kind d)) (str/lower-case (:name d))
                        (and (map? src) (string? (:table src))) (str/lower-case (:table src))
                        (map? src) (:name d))
     :repository  (when (map? v) (:repository v))}))

(defn expected-paths [spec d]
  (let [entry (some (fn [e] (when (and (= (:kind e) (:kind d))
                                       (or (not= :synced (:when e))
                                           (map? (get-in d [:data :external-source]))))
                              e))
                    (get-in spec [:layout :paths]))]
    (when entry
      (let [vars (layout-vars d)]
        (keep #(spec/fill % vars) (cons (:path entry) (:alt entry)))))))

(defn- rule-layout [spec {:keys [defs]}]
  (for [d defs
        :when (not (legacy? spec (:file d)))
        :when (not (and (:owned-by d) (:owner d)))
        :let [paths (expected-paths spec d)]
        :when (and (seq paths) (not (some #{(:file d)} paths)))]
    (finding :falscher-ort (:file d) (:name-loc d)
             {:kind (name (:kind d)) :name (show (:name d)) :expected (first paths)
              :data {:expected (first paths)}})))

(defn- rule-unknown-property [_spec {:keys [refs by-key]}]
  (for [r refs
        :when (and (:object r) (empty? (:targets r)) (seq (by-key [:object (:object r)])))]
    (finding :unbekannte-eigenschaft (:file r) (:loc r)
             {:name (show (:name r)) :object (show (:object r))})))

(defn- rule-duplicate-properties [_spec {:keys [defs]}]
  (for [[_ ds] (group-by (juxt :file :parent-path :name) (filter :object defs))
        :when (> (count ds) 1)
        d ds]
    (finding :doppelte-eigenschaft (:file d) (:name-loc d)
             {:name (show (:name d)) :object (show (:object d))})))

(defn- rule-projekttext-similar
  [spec {:keys [defs names]}]
  (for [d defs
        :let [tag (get-in spec [:ownership :tags (:kind d)])]
        :when (and tag (:owned-by d) (not (:object d)) (not (:extends d)) (not (:extension d))
                   (keyword? (:name d)))
        :let [others (update names :entries
                             (fn [es] (remove #(and (= tag (:kind %)) (= (:name d) (:name %))) es)))
              {:keys [verdict candidates]} (sutra-names/suggest others (:name d) tag (:owner d))
              hits (take 3 (filter #(>= (:score %) 0.5) candidates))]
        :when (and (= :close verdict) (seq hits))]
    (finding :projekttext-aehnlich (:file d) (:name-loc d)
             {:name (show (:name d)) :site (show (:owner d))
              :matches (str/join ", " (for [{n :name why :why o :project} hits]
                                        (str (show n)
                                             (when o (str " (" (show o) ", " (name why) ")")))))})))

(defn- rule-extension-attributes [_spec {:keys [props]}]
  (for [[[o nm] ps] props
        :when (and (> (count ps) 1) (some :extension ps))
        p ps :when (:extension p)]
    (finding :projekttext-attribut-doppelt (:file p) (:name-loc p)
             {:name (show nm) :object (show o)
              :others (str/join ", " (map where (remove #{p} ps)))})))

(defn- rule-create-sets-pk [spec {:keys [writes by-key] :as idx}]
  (for [c writes
        :when (and (= :create (:op c)) (seq (by-key [:object (:object c)])))
        :let [missing (remove (:set c) (primary-key spec idx (:object c)))]
        :when (seq missing)]
    (finding :pk-fehlt (:file c) (:loc c)
             {:name (show (get (:path c) 2)) :object (show (:object c))
              :pk (str/join ", " (map show missing))})))

(defn- rule-sync-at-consumer [_spec {:keys [defs by-key]}]
  (for [d defs
        :when (= :dataset (:kind d))
        :let [s (get-in d [:data :external-source :datasource])
              q (when (keyword? s) (some :owner (by-key [:source s])))
              p (:owner d)]
        :when (and p q (not= p q))]
    (finding :sync-beim-verbraucher (:file d) (:name-loc d)
             {:name (show (:name d)) :source (show s) :source-owner (show q)})))

(defn- rule-action-sprawl [spec {:keys [defs by-key]}]
  (let [{limit :max :keys [name-pattern]} (get-in spec [:actions :sprawl])
        actions (filter #(= :action (:kind %)) defs)]
    (concat
     (for [[o as] (group-by :via actions)
           :when (and o (> (count as) limit))
           :let [d (first (by-key [:object o]))]
           :when d]
       (finding :action-sprawl (:file d) (:name-loc d) {:name (show o) :count (count as)}))
     (for [a actions
           :when (and (keyword? (:name a)) (re-find (re-pattern name-pattern) (name (:name a))))]
       (finding :action-pro-eigenschaft (:file a) (:name-loc a) {:name (show (:name a))})))))

(def rules
  [rule-unbekannt rule-visibility rule-orphan-link rule-duplicates rule-projekttext-author
   rule-projekttext-similar rule-owner rule-layout
   rule-unknown-property rule-duplicate-properties rule-extension-attributes rule-create-sets-pk
   rule-sync-at-consumer rule-action-sprawl])

(defn render [template vars]
  (str/replace template #"\{([a-z-]+)\}" (fn [[_ k]] (str (get vars (keyword k) "?")))))

(defn complete
  [spec f]
  (let [r (get-in spec [:rules (:rule f)])]
    (assoc f
           :severity (or (:severity f) (:severity r :error))
           :message  (render (:message r "{message}") (:vars f)))))

(defn run-rules [spec idx]
  (->> rules
       (mapcat #(% spec idx))
       (map #(complete spec %))))
