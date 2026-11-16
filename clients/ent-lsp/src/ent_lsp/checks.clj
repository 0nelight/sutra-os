(ns ent-lsp.checks
  (:require [clojure.string :as str]
            [ent-lsp.analysis :as an]))

(def ^:private show an/show)

(defn- finding [rule file loc vars]
  {:rule rule :file file :loc loc :vars vars})

(defn- top-defs [defs] (remove :object defs))

(defn- of-kind [kind defs] (filter #(= kind (:kind %)) defs))

(defn- links [defs] (filter #(vector? (:data %)) (of-kind :link defs)))

(defn prop-type
  [idx obj prop]
  (or (get-in (first (get-in idx [:props [obj prop]])) [:data :type])
      (when (seq (get-in idx [:props [obj prop]]))
        (get-in (first (get-in idx [:by-key [:shared-property prop]])) [:data :type]))))

(defn- rule-link-sides [_spec {:keys [defs]}]
  (let [ls    (links defs)
        sides (for [d ls
                    :let [[_ from to] (:data d) o (an/entity-map d)]
                    [obj k] [[from :forward-api-name] [to :reverse-api-name]]
                    :let [nm (get o k)]
                    :when (and (keyword? obj) nm)]
                {:object obj :side nm :link d})]
    (concat
     (for [[[obj side] ss] (group-by (juxt :object :side) sides)
           :when (> (count ss) 1)
           :let [ds (distinct (map :link ss))]
           d ds]
       (finding :link-seite-doppelt (:file d) (:name-loc d)
                {:object (show obj) :side (show side)
                 :links (str/join ", " (map (comp show :name) ds))}))
     (for [d ls
           :let [[_ from to] (:data d) o (an/entity-map d)]
           :when (and (= from to) (or (nil? (:forward-api-name o)) (nil? (:reverse-api-name o))))]
       (finding :selbst-link-seiten (:file d) (:name-loc d) {:name (show (:name d))})))))

(def ^:private backings [:fk :join :fk-resolve :external-source])

(defn- rule-link-backing [_spec {:keys [defs]}]
  (for [d (links defs)
        :let [card (get (:data d) 3)
              o    (an/entity-map d)
              bs   (filter #(contains? o %) backings)
              problem (cond
                        (> (count bs) 1)
                        (str "höchstens eine Anbindung, hier " (str/join ", " (map show bs)))
                        (and (= :many-to-many card) (some #{:fk :fk-resolve} bs))
                        "m:n läuft nicht über einen Fremdschlüssel — dafür braucht es eine Join-Tabelle (:join)"
                        (and (= :many-to-many card) (empty? bs))
                        "m:n braucht eine Join-Tabelle (:join oder :external-source)")]
        :when problem]
    (finding :link-anbindung (:file d) (:name-loc d) {:name (show (:name d)) :problem problem})))

(defn- rule-fk-type [spec {:keys [defs] :as idx}]
  (for [d (links defs)
        :let [[_ from to card] (:data d)
              fk (get-in (an/entity-map d) [:fk :attribute])
              [near far] (if (= :one-to-many card) [to from] [from to])
              pk (an/primary-key spec idx far)]
        :when (and (keyword? fk) (= 1 (count pk)))
        :let [ft (prop-type idx near fk) pt (prop-type idx far (first pk))]
        :when (and ft pt (not= ft pt))]
    (finding :fk-typ (:file d) (:name-loc d)
             {:fk (show fk) :fk-type (show ft) :pk (show (first pk)) :object (show far)
              :pk-type (show pt)})))

(defn- rule-dataset-once [_spec {:keys [refs]}]
  (let [backs (for [r refs
                    :when (contains? (:kinds r) :dataset)
                    :let [[s0 s1 o & more] (:path r)]
                    :when (and (= [:schema :objects] [s0 s1])
                               (#{[:dataset] [:backing :dataset]} (vec more)))]
                (assoc r :obj o))]
    (for [[ds rs] (group-by :name backs)
          :let [objs (distinct (map :obj rs))]
          :when (> (count objs) 1)
          r rs]
      (finding :datenquelle-mehrfach (:file r) (:loc r)
               {:name (show ds) :objects (str/join ", " (map show objs))}))))

(defn- vocab [spec kind]
  (get-in spec [:status :vocab kind] (get-in spec [:status :vocab :default])))

(defn- rule-status-vocab [spec {:keys [defs]}]
  (for [d (top-defs defs)
        :when (not= :project (:kind d))
        :let [st (:status (an/entity-map d)) ok (vocab spec (:kind d))]
        :when (and (some? st) (not (contains? ok st)))]
    (finding :status-unbekannt (:file d) (:name-loc d)
             {:kind (name (:kind d)) :name (show (:name d)) :status (show st)
              :allowed (str/join ", " (map show (sort ok)))})))

(defn- rule-deprecated [spec {:keys [refs defs]}]
  (concat
   (for [r refs
         :let [t (first (:targets r))]
         :when (and t (not (:object t)) (= :deprecated (an/status-of spec t)))
         :let [rep (get-in (an/entity-map t) [:deprecation :replacement])]]
     (finding :veraltet (:file r) (:loc r)
              (cond-> {:name (show (:name r))
                       :replacement (if (keyword? rep) (str " — Ersatz: " (show rep)) "")}
                (keyword? rep) (assoc :data {:replacement rep}))))
   (for [d (top-defs defs)
         :when (and (= :deprecated (:status (an/entity-map d)))
                    (not (map? (:deprecation (an/entity-map d)))))]
     (finding :deprecation-fehlt (:file d) (:name-loc d)
              {:kind (name (:kind d)) :name (show (:name d))}))))

(def ^:private follow-order [:deprecated :example :experimental])

(defn- rule-link-status [spec {:keys [defs by-key]}]
  (let [follows (get-in spec [:status :follows-ends])]
    (for [d (links defs)
          :let [[_ from to] (:data d)
                st   (an/status-of spec d)
                ends (for [e (distinct [from to])
                           :let [od (first (by-key [:object e]))]
                           :when od]
                       [e (an/status-of spec od)])
                need (some (fn [s]
                             (some (fn [[e es]] (when (and (= s es) (follows s)) [e es])) ends))
                           follow-order)]
          :when (and need (not= st (second need)))]
      (finding :link-status (:file d) (:name-loc d)
               {:name (show (:name d)) :status (show st) :end (show (first need))
                :end-status (show (second need))}))))

(defn- iface-def [by-key i] (first (by-key [:interface i])))

(defn- ancestors
  [by-key i]
  (loop [todo (vec (get (an/entity-map (iface-def by-key i)) :extends)) seen []]
    (if-let [x (first todo)]
      (if (or (= x i) (some #{x} seen) (not (keyword? x)))
        (recur (subvec todo 1) seen)
        (recur (into (vec (get (an/entity-map (iface-def by-key x)) :extends)) (subvec todo 1))
               (conj seen x)))
      seen)))

(defn- extends-cycle? [by-key i]
  (loop [todo (vec (get (an/entity-map (iface-def by-key i)) :extends)) seen #{}]
    (when-let [x (first todo)]
      (cond (= x i) true
            (or (seen x) (not (keyword? x))) (recur (subvec todo 1) seen)
            :else (recur (into (subvec todo 1) (get (an/entity-map (iface-def by-key x)) :extends))
                         (conj seen x))))))

(defn- own-props [d]
  (let [m (an/entity-map d) ps (or (:property-list m) (:properties m))]
    (for [a (when (sequential? ps) ps)
          :let [[nm t] (cond (vector? a)  [(first a) (second a)]
                             (keyword? a) [a nil]
                             (map? a)     [(:attribute-name a) (:attribute-type a)])]
          :when (keyword? nm)]
      [nm t])))

(defn- own-constraints [d]
  (let [m (an/entity-map d) cs (or (:link-constraints m) (:links m))]
    (filter map? (when (sequential? cs) cs))))

(defn- effective [by-key i f]
  (mapcat #(when-let [d (iface-def by-key %)] (f d)) (cons i (ancestors by-key i))))

(defn- implements [o]
  (let [x (:implements (an/entity-map o))] (filter keyword? (if (sequential? x) x [x]))))

(defn- implements? [by-key obj iface]
  (some #(or (= % iface) (some #{iface} (ancestors by-key %)))
        (mapcat implements (by-key [:object obj]))))

(defn- one-or-many [c]
  (case c (:one :many-to-one :one-to-one) :one (:many :one-to-many :many-to-many) :many nil))

(defn- from-side [card dir]
  (if (= dir :fwd)
    (one-or-many card)
    (case card :one-to-many :one :many-to-one :many (one-or-many card))))

(defn- rule-implementations [spec {:keys [defs by-key] :as idx}]
  (let [ls (links defs)]
    (concat
     (for [i (of-kind :interface defs)
           :when (extends-cycle? by-key (:name i))]
       (finding :extends-zyklus (:file i) (:name-loc i)
                {:name (show (:name i))
                 :via (str/join ", " (map show (get (an/entity-map i) :extends)))}))
     (for [o (of-kind :object defs)
           i (implements o)
           :when (iface-def by-key i)
           :let [want (into {} (effective by-key i own-props))
                 have (fn [p] (seq (get-in idx [:props [(:name o) p]])))
                 missing (remove have (keys want))]
           f (concat
              (when (seq missing)
                [(finding :interface-eigenschaft-fehlt (:file o) (:name-loc o)
                          {:object (show (:name o)) :interface (show i)
                           :missing (str/join ", " (map show (sort missing)))})])
              (for [[p t] want
                    :when (and t (have p))
                    :let [ot (prop-type idx (:name o) p)]
                    :when (and ot (not= ot t))]
                (finding :interface-typ (:file o) (:name-loc o)
                         {:object (show (:name o)) :name (show p) :type (show ot)
                          :interface (show i) :wanted (show t)})))]
       f)
     (for [o (of-kind :object defs)
           i (implements o)
           :when (iface-def by-key i)
           c (effective by-key i own-constraints)
           :when (true? (:required c))
           :let [lname  (:link-name c)
                 card   (one-or-many (:cardinality c))
                 target (or (:to-object-type c) (:to-interface c))
                 fits?  (fn [other]
                          (cond (:to-object-type c) (= other (:to-object-type c))
                                (:to-interface c)   (implements? by-key other (:to-interface c))
                                :else true))
                 met?   (some (fn [l]
                                (let [[nm from to lc] (:data l)
                                      sat
                                        (some #(and (= i (:interface %)) (= lname (:constraint %)))
                                              (filter map? (let [s (:satisfies (an/entity-map l))]
                                                             (if (map? s) [s] s))))]
                                  (some (fn [[near other dir]]
                                          (and (= near (:name o)) (or (= nm lname) sat)
                                               (fits? other)
                                               (or (nil? card) (= card (from-side lc dir)))))
                                        [[from to :fwd] [to from :rev]])))
                              ls)]
           :when (not met?)]
       (finding :interface-link-fehlt (:file o) (:name-loc o)
                {:object (show (:name o)) :interface (show i) :link (show lname)
                 :cardinality (show card) :target (show target)}))
     (for [i (of-kind :interface defs)
           :let [n (count (filter #(some #{(:name i)} (implements %)) (of-kind :object defs)))
                 searchable (true? (:searchable (an/entity-map i)))
                 limit (if searchable 50 1000)]
           :when (> n limit)]
       (finding :interface-zu-viele (:file i) (:name-loc i)
                {:name (show (:name i)) :count n :limit limit
                 :kind (if searchable "durchsuchbar" "nicht durchsuchbar")})))))

(defn- action-of [{:keys [defs-by-file]} w]
  (let [p (subvec (:path w) 0 (min 3 (count (:path w))))]
    (some #(when (and (= :action (:kind %)) (= p (:path %))) %) (defs-by-file (:file w)))))

(defn- rule-actions [spec {:keys [defs writes by-key] :as idx}]
  (let [fops      (get-in spec [:actions :function-ops])
        forbidden (get-in spec [:actions :forbidden-types])
        global    (get-in spec [:ownership :global-projects])]
    (concat
     (for [a (of-kind :action defs)
           :let [rules (:rules (an/entity-map a))]
           :when (and (sequential? rules) (> (count rules) 1)
                      (some #(and (map? %) (contains? fops (:op %))) rules))]
       (finding :function-rule-allein (:file a) (:name-loc a) {:name (show (:name a))}))
     (for [a (of-kind :action defs)
           [p spec'] (:params (an/entity-map a))
           :let [t (when (map? spec') (:type spec'))]
           :when (contains? forbidden t)]
       (finding :action-typ (:file a) (:name-loc a)
                {:name (show (:name a)) :what (str "Parameter " (show p)) :type (show t)}))
     (for [w writes
           p (:set w)
           :let [t (prop-type idx (:object w) p)]
           :when (contains? forbidden t)
           :let [a (action-of idx w)]
           :when a]
       (finding :action-typ (:file w) (:loc w)
                {:name (show (:name a)) :what (str "Eigenschaft " (show p)) :type (show t)}))
     (for [w writes
           :let [a (action-of idx w)
                 p (:owner a)
                 q (some :owner (by-key [:object (:object w)]))]
           :when (and a p q (not= p q) (not (contains? global q)))]
       (finding :fremd-schreiben (:file w) (:loc w)
                {:name (show (:name a)) :owner (show p) :object (show (:object w))
                 :object-owner (show q)})))))

(defn- rule-names [spec {:keys [defs by-key exports]}]
  (let [{:keys [kinds pattern silo-words time-pattern]} (:identifiers spec)
        id-re   (re-pattern pattern)
        time-re (re-pattern time-pattern)
        sources (set (keep #(when (keyword? (:name %)) (name (:name %))) (of-kind :source defs)))
        silos   (into silo-words sources)
        protected (get-in spec [:status :protected])]
    (concat
     (for [d defs
           :when (and (kinds (:kind d)) (keyword? (:name d))
                      (not (re-find id-re (name (:name d)))))]
       (finding :id-syntax (:file d) (:name-loc d) {:kind (name (:kind d)) :name (show (:name d))}))
     (for [d (top-defs defs)
           :when (kinds (:kind d))
           :let [m (an/entity-map d)
                 why (cond (some #(contains? (get exports %) (:name d)) [(:owner d)]) "exportiert"
                           (contains? protected (:status m)) (name (:status m)))]
           :when (and why (map? m) (not (:description m)))]
       (finding :beschreibung-fehlt (:file d) (:name-loc d)
                {:kind (name (:kind d)) :name (show (:name d)) :why why}))
     (for [d (of-kind :object defs)
           :when (keyword? (:name d))
           :let [w (some silos (str/split (name (:name d)) #"[-_]"))]
           :when w]
       (finding :system-silo (:file d) (:name-loc d) {:name (show (:name d)) :word w}))
     (for [d (of-kind :object defs)
           :when (and (keyword? (:name d)) (re-find time-re (name (:name d))))]
       (finding :time-machine (:file d) (:name-loc d) {:name (show (:name d))})))))

(def rules
  (into an/rules [rule-link-sides rule-link-backing rule-fk-type rule-dataset-once
                  rule-status-vocab rule-deprecated rule-link-status
                  rule-implementations rule-actions rule-names]))

(defn run-rules [spec idx]
  (mapcat #(% spec idx) rules))
