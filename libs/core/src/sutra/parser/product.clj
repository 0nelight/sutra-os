(ns sutra.parser.product
  (:refer-clojure :exclude [satisfies?])
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.kit.lifecycle :as lc]
            [sutra.parser.checkout :as co]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-view :as ent-view]
            [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]
            [sutra.types.rid :as rid]
            [sutra.types.tree :as tree])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.time DayOfWeek Instant LocalTime ZoneId)
           (java.util HexFormat UUID)))

(set! *warn-on-reflection* true)

(def ^:private container-sections
  (into #{} (comp (filter #(= :container (get-in % [:export :scope]))) (map :section))
        ents/entities))

(def ^:private stripped
  {:projects [:grants :space :shared-with]})

(defn- project-rids
  [schema projects]
  (let [ps (:projects schema)]
    (set (for [p projects
               :let [r (if (contains? ps p)
                         p
                         (some (fn [[r s]] (when (= (name p) (some-> (:project-name s) name)) r))
                               ps))]
               :when r]
           r))))

(defn- refs-of
  [section spec]
  (for [[targets v] (rid/ref-values section spec) :when (rid/rid? v)] [v targets]))

(defn package
  [schema {:keys [product version projects]}]
  (let [sel   (project-rids schema projects)
        repos (into #{} (keep (fn [[r s]] (when (sel (:project s)) r))) (:repositories schema))
        owned (fn [s] (or (sel (:project s)) (repos (:repository s))))
        res   (into {}
                    (for [section ents/rid-sections
                          :when (not (container-sections section))
                          :let  [m (if (= :projects section)
                                     (select-keys (:projects schema) sel)
                                     (into {} (filter (comp owned val)) (get schema section)))
                                 m (update-vals m #(apply dissoc % (get stripped section)))]
                          :when (seq m)]
                      [section m]))
        held  (into #{} (mapcat (comp keys val)) res)]
    {:product   product
     :version   version
     :resources res
     :inputs    (reduce (fn [m [r targets]]
                          (cond-> m
                            (not (held r)) (update-in [r :targets] (fnil into #{}) targets)))
                        {}
                        (for [[section m] res [_ spec] m ref (refs-of section spec)] ref))}))

(defn install-rid
  [install-id r]
  (rid/new-rid (rid/rid-kind r)
               (UUID/nameUUIDFromBytes (.getBytes (str install-id "|" r) StandardCharsets/UTF_8))))

(defn install-plan
  [bundle target {:keys [install-id space bindings]}]
  (let [held     (into {} (for [[_ m] (:resources bundle) [r _] m] [r (install-rid install-id r)]))
        remap    (fn [_ v] (or (held v) (get bindings v) v))
        exists?  (fn [r] (some #(contains? (get target %) r) ents/rid-sections))
        fragment (into {}
                       (for [[section m] (:resources bundle)]
                         [section
                          (into {}
                                (for [[r spec] m]
                                  [(held r)
                                   (cond-> (rid/rewrite-refs section spec remap)
                                     (and space (= :projects section)) (assoc :space space))]))]))
        taken    (for [[section m] fragment
                       :let [nf (ents/name-field section)]
                       [r spec] m
                       :let [nm (get spec nf)]
                       :when nm
                       [r' spec'] (get target section)
                       :when (and (not= r r') (= (name nm) (some-> (get spec' nf) name)))]
                   {:problem :name-taken :section section :name nm :held-by r'})]
    {:schema   fragment
     :problems (vec (concat
                     (for [[r {:keys [targets]}] (sort-by key (:inputs bundle))
                           :when (not (contains? bindings r))]
                       {:problem :unbound-input :input r :targets targets})
                     (for [[r b] (sort-by key bindings)
                           :when (and (contains? (:inputs bundle) r) (not (exists? b)))]
                       {:problem :unknown-binding :input r :bound-to b})
                     taken))}))

(defn upgrade
  [from to target opts]
  (let [base   (:schema (install-plan from target opts))
        mine   (:schema (install-plan to target opts))
        theirs (into {}
                     (for [s (distinct (concat (keys base) (keys mine)))]
                       [s
                        (select-keys (get target s)
                                     (distinct
                                      (concat (keys (get base s)) (keys (get mine s)))))]))]
    (co/merge3 (co/base-of base) mine theirs (select-keys opts [:resolve]))))

(defn- nodes-named
  [current bodies]
  (tree/select current (into #{} (mapcat (comp keys tree/index)) bodies)))

(defn release-upgrade
  [installed release current resolve]
  (let [theirs (nodes-named current (keep identity [installed release]))
        base   (co/base-of (or installed theirs))]
    (co/merge3 base release theirs {:resolve resolve})))

(def channels
  [:dev :test :stable])

(defn next-channel
  [{:keys [channel]}]
  (second (drop-while #(not= % channel) channels)))

(def release-lifecycle
  (lc/table {:name   :product-release
             :states #{:draft :released :deprecated :withdrawn}
             :final  #{:withdrawn}
             :installable #{:released :deprecated}
             :edges  {:draft     {:from #{} :to :draft}
                      :release   {:from #{:draft} :to :released}
                      :promote   {:from #{:released} :to :released
                                  :guard (fn [record _]
                                           (when-not (next-channel record)
                                             (str "already on " (name (:channel record))
                                                  ", the last channel")))}
                      :deprecate {:from #{:released} :to :deprecated}
                      :withdraw  {:from #{:draft :released :deprecated} :to :withdrawn}}}))

(defn installable?
  [r]
  (contains? (:installable release-lifecycle) (:state r)))

(defn install-state
  [present? types]
  (let [present (count (filter present? types))]
    {:status  (cond (and (pos? present) (= present (count types))) :installed
                    (pos? present)                                 :partial
                    :else                                          :not-installed)
     :present present
     :total   (count types)}))

(defn listing
  ([offers types-of present?] (listing offers types-of present? (constantly nil)))
  ([offers types-of present? installed-version]
   (vec (for [[id
               {:keys [name description requires
                       version]}] (sort-by (fn [[id o]] [(:order o 100) (clojure.core/name id)])
                                           offers)
              :let [types (types-of id)
                    had   (installed-version id)]]
          (merge (cond-> {:id           (clojure.core/name id)
                          :name         name
                          :description  description
                          :requires     (mapv clojure.core/name requires)
                          :object-types types}
                   version (assoc :version version)
                   had     (assoc :installed-version had
                                  :upgrade? (boolean (and version (not= had version)))))
                 (install-state present? types))))))

(defn parse-version
  [s]
  (when-let [[_ a b c] (re-matches #"v?(\d+)(?:\.(\d+))?(?:\.(\d+))?" (str/trim (str s)))]
    (mapv #(if % (parse-long %) 0) [a b c])))

(defn compare-versions [a b] (compare (parse-version a) (parse-version b)))

(defn breaking?
  [from to]
  (let [[fa fb] (parse-version from) [ta tb] (parse-version to)]
    (or (not= fa ta) (and (zero? fa) (not= fb tb)))))

(defn- comparator-ok?
  [v c]
  (let [[_ op x] (re-matches #"(\^|~|>=|<=|>|<|=)?(.+)" c)
        wild     (re-matches #"(\d+)(?:\.(\d+|x|\*))?(?:\.(x|\*))?" x)
        cmp      #(compare-versions v x)
        [xa xb]  (parse-version x)
        [va vb]  (parse-version v)]
    (cond
      (#{"*" "x"} c) true
      (and (nil? op) wild (some #{"x" "*"} (drop 2 wild)))
      (let [[_ wa wb] wild]
        (and (= va (parse-long wa)) (or (#{"x" "*"} wb) (= vb (parse-long wb)))))
      (nil? (parse-version x)) false
      :else
      (case op
        "^"  (and (>= (cmp) 0) (not (breaking? x v)))
        "~"  (and (>= (cmp) 0) (= [va vb] [xa xb]))
        ">=" (>= (cmp) 0)
        "<=" (<= (cmp) 0)
        ">"  (pos? (cmp))
        "<"  (neg? (cmp))
        (zero? (cmp))))))

(defn satisfies?
  [v range]
  (and (some? (parse-version v))
       (every? #(comparator-ok? v %) (str/split (str/trim (str (or range "*"))) #"\s+"))))

(def bundle-format 1)

(defn units-body
  [units]
  (apply merge-with (fn [a b] (if (and (map? a) (map? b)) (merge a b) b))
         {}
         (keep (fn [{:keys [kind doc]}]
                 (case kind
                   :app      (ent/enta->schema doc)
                   :template (ent/entt->schema doc)
                   :pipeline (ent/entp->schema doc)
                   :view     (ent-view/entv->schema doc)
                   nil))
               units)))

(defn secret-paths
  [x]
  (letfn [(walk* [path v]
            (cond (map? v)        (mapcat (fn [[k w]]
                                            (if (#{:secrets :secret-cipher :password} k)
                                              [(conj path k)]
                                              (walk* (conj path k) w)))
                                          v)
                  (sequential? v) (mapcat #(walk* (conj path %1) %2) (range) v)
                  :else           nil))]
    (vec (walk* [] x))))

(defn- canonical
  [x]
  (walk/postwalk
   (fn [v]
     (cond (map? v) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2))) v)
           (set? v) (vec (sort-by pr-str v))
           :else    v))
   x))

(defn- hex [^bytes bs] (.formatHex (HexFormat/of) bs))

(defn digest
  [bundle]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (hex (.digest md (.getBytes ^String (pr-str (canonical (dissoc bundle :digest :signature)))
                                StandardCharsets/UTF_8)))))

(defn sealed
  [bundle {:keys [issuer sign]}]
  (let [d (digest bundle)]
    (cond-> (assoc (dissoc bundle :signature) :digest d)
      sign (assoc :signature (assoc (select-keys (sign d) [:iss :public-key :sig])
                                    :issuer issuer)))))

(defn bundle-problems
  [bundle {:keys [trusted-as valid? require-signature?]}]
  (let [{:keys [issuer public-key sig] :as signature} (:signature bundle)]
    (vec (concat
          (when-not (= bundle-format (:format bundle))
            [{:problem :format :format (:format bundle)}])
          (when-not (= (:digest bundle) (digest bundle))
            [{:problem :digest-mismatch}])
          (map (fn [p] {:problem :credential :path p}) (secret-paths (:bodies bundle)))
          (cond
            (and (nil? signature) require-signature?) [{:problem :unsigned}]
            (nil? signature)                          nil
            (nil? (and trusted-as (trusted-as public-key)))
            [{:problem :untrusted-issuer :issuer issuer}]
            (not (and valid? (valid? public-key (:digest bundle) sig)))
            [{:problem :bad-signature :issuer issuer}])))))

(defn bundle-of
  [{:keys [product version description exports inputs requires documents]}]
  {:format      bundle-format
   :product     (name product)
   :version     version
   :description description
   :bodies      (vec (for [{:keys [project ent units]} exports]
                       {:project (name project)
                        :body    (merge-with (fn [a b] (if (map? a) (merge a b) b))
                                             (:schema ent) (units-body units))}))
   :inputs      (vec (sort-by (juxt :section :name) inputs))
   :requires    (into (sorted-map) (map (fn [[p r]] [(name p) (str r)])) requires)
   :documents   (vec (sort-by (juxt :project :id)
                              (for [d documents]
                                (-> (select-keys d [:id :project :name :format :body
                                                    :parameters :page])
                                    (update :project name)))))})

(defn inputs-of
  [schema pkg]
  (vec (distinct
        (for [[r _] (:inputs pkg)
              :let [section (some #(when (contains? (get schema %) r) %) ents/rid-sections)]
              :when section
              :let [nm (get-in schema [section r (ents/name-field section)])]
              :when nm]
          {:section section :name (name nm) :rid r}))))

(defn- named [prefix k] (keyword (str prefix "-" (name k))))

(defn with-prefix
  [body prefix]
  (if (str/blank? prefix)
    body
    (let [ren   (fn [ks] (into {} (map (fn [k] [(name k) (named prefix k)])) ks))
          objs  (ren (keys (:objects body)))
          links (ren (keep #(when (sequential? %) (first %)) (:links body)))
          acts  (ren (keys (:actions body)))
          own   (fn [m] (fn [k] (get m (name k) k)))
          ren-s (fn [b section names] (refs/rename ents/author-body-refs b section names))]
      (cond-> (-> body
                  (ren-s :object-types objs)
                  (ren-s :link-types links)
                  (ren-s :actions acts))
        (:objects body) (update :objects #(update-keys % (own objs)))
        (:actions body) (update :actions #(update-keys % (own acts)))
        (:links body)   (update :links (fn [ls] (mapv (fn [[n & more]]
                                                        (into [((own links) n)] more))
                                                      ls)))))))

(defn with-suffix
  [body suffix]
  (if (str/blank? suffix)
    body
    (let [ps (into {} (map (fn [k] [(name k) (keyword (str (name k) "-" suffix))]))
                   (keys (:projects body)))]
      (-> (refs/rename ents/author-body-refs body :projects ps)
          (update :projects #(some-> % (update-keys (fn [k] (get ps (name k) k)))))))))

(defn bind
  [body bindings inputs]
  (let [section (into {} (map (fn [{s :section n :name}] [(name n) (keyword (name s))]))
                      inputs)
        by-sec  (group-by (fn [[k _]] (get section (name k) :object-types)) bindings)
        names   (fn [bs] (into {} (map (fn [[k v]] [(name k) (keyword (name v))])) bs))]
    (reduce-kv (fn [b sec bs] (refs/rename ents/author-body-refs b sec (names bs)))
               body
               by-sec)))

(defn input-problems
  [inputs bindings present?]
  (vec (for [{:keys [section name]} inputs
             :let [bound (get bindings (keyword name) (get bindings name))]
             :when (not (present? section (or bound name)))]
         {:problem :unbound-input :section section :name name :bound-to bound})))

(defn- channel-rank [c] (.indexOf ^java.util.List channels (keyword (name (or c :stable)))))

(defn visible-on?
  [{:keys [state channel]} tracking]
  (and (= :released (keyword (name state)))
       (>= (channel-rank channel) (channel-rank tracking))))

(defn upgrade-target
  [{:keys [version channel]} releases auto?]
  (->> releases
       (filter #(visible-on? % channel))
       (filter #(pos? (compare-versions (:version %) version)))
       (remove #(and auto? (breaking? version (:version %))))
       (sort-by :version compare-versions)
       last))

(def ^:private day-names
  {"mon" DayOfWeek/MONDAY "tue" DayOfWeek/TUESDAY "wed" DayOfWeek/WEDNESDAY
   "thu" DayOfWeek/THURSDAY "fri" DayOfWeek/FRIDAY "sat" DayOfWeek/SATURDAY
   "sun" DayOfWeek/SUNDAY})

(defn window-problems
  [{:keys [days from to zone] :as window}]
  (when window
    (vec (concat
          (remove nil? (for [d days :when (not (day-names (str/lower-case (name d))))]
                         {:problem :unknown-day :day d}))
          (for [[k t] [[:from from] [:to to]]
                :when (not (try (LocalTime/parse (str t)) (catch Exception _ nil)))]
            {:problem :bad-time k t})
          (when (and zone (not (try (ZoneId/of zone) (catch Exception _ nil))))
            [{:problem :unknown-zone :zone zone}])))))

(defn in-window?
  [window ^Instant instant]
  (or (nil? window)
      (let [{:keys [days from to zone]} window
            z     (ZoneId/of (or zone "UTC"))
            at    (.atZone instant z)
            t     (.toLocalTime at)
            f     (LocalTime/parse from)
            e     (LocalTime/parse to)
            day   (fn [d] (or (empty? days)
                              (contains? (set (keep #(day-names (str/lower-case (name %))) days))
                                         d)))
            today (.getDayOfWeek at)]
        (if (.isBefore f e)
          (and (day today) (not (.isBefore t f)) (.isBefore t e))
          (or (and (day today) (not (.isBefore t f)))
              (and (day (.minus today 1)) (.isBefore t e)))))))

(defn resolve-requires
  [requires installations releases]
  (vec (for [[p r] (sort-by key requires)
             :let [p    (name p)
                   have (first (filter #(and (= p (:product %)) (satisfies? (:version %) r))
                                       installations))
                   rel  (->> releases
                             (filter #(and (= p (:product %)) (visible-on? % :stable)
                                           (satisfies? (:version %) r)))
                             (sort-by :version compare-versions)
                             last)]]
         (cond have {:product p :range r :via :installation :install-id (:install-id have)
                     :version (:version have)}
               rel  {:product p :range r :via :install :version (:version rel)}
               :else {:product p :range r :via :unmet}))))

(defn body-of
  [bundle]
  (apply merge-with
         (fn [a b] (cond (and (map? a) (map? b))               (merge a b)
                         (and (sequential? a) (sequential? b)) (vec (distinct (concat a b)))
                         :else                                 b))
         {}
         (map :body (:bodies bundle))))

(defn installed-body
  [bundle {:keys [prefix suffix bindings]}]
  (-> (body-of bundle) (bind bindings (:inputs bundle)) (with-prefix prefix)
      (with-suffix suffix)))

(defn installed-projects
  [bundle {:keys [suffix]}]
  (vec (for [{:keys [project]} (:bodies bundle)]
         (if (str/blank? suffix) project (str project "-" suffix)))))

(defn installed-documents
  [bundle install-id {:keys [suffix]}]
  (vec (for [{:keys [id project] :as d} (:documents bundle)]
         (assoc d
                :id      (str (UUID/nameUUIDFromBytes
                               (.getBytes (str install-id "|document|" id)
                                          StandardCharsets/UTF_8)))
                :project (if (str/blank? suffix) project (str project "-" suffix))))))

(defn held-names
  [body]
  (into {}
        (for [[section m] body
              :when (and (contains? ents/author->section section) (coll? m) (seq m))]
          [section (into #{} (map #(rid/->name (if (map-entry? %) (key %) (first %)))) m)])))

(defn uninstall-sweep
  [i]
  (dissoc (held-names (:release i)) :projects))

(defn sweep-standing?
  [schema sweep]
  (boolean
   (some (fn [[author names]]
           (let [section (ents/author->section author)
                 nf      (ents/name-field section)]
             (some #(contains? names (some-> (get % nf) rid/->name)) (vals (get schema section)))))
         sweep)))
