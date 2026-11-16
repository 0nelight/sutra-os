(ns sutra.types.forms
  (:require [sutra.types.transform-io :as tio]
            [sutra.types.refs :as refs]
            [sutra.types.entities :as ents]
            [sutra.types.membership :as member]
            [sutra.types.rid :as rid]))

(defn- kw [x] (when (or (keyword? x) (string? x)) (keyword (name x))))

(defn log-type-of
  [action-name action]
  (or (get-in action [:action-log :object-type]) (keyword (str (name action-name) "-log"))))

(defn- outputs-of
  [t]
  (keep identity (cons (:dataset t) (map :dataset (tio/outputs-of t)))))

(defn- link-entries [links]
  (for [[n from to card opts] links]
    [n (assoc (or opts {}) ::from from ::to to ::cardinality card)]))

(def ^:private author-section
  (into {} (for [e ents/entities] [(:tag e) (or (:author e) (:section e))])))

(def ^:private installed-section
  (into {} (for [e ents/entities] [(:tag e) (:section e)])))

(def tags
  (vec (keys author-section)))

(defn author-section-of
  [tag]
  (author-section tag))

(defn author
  ([schema] (author schema {}))
  ([schema known]
   (let [section     (fn [tag] (get schema (author-section tag)))
         entries     (fn [tag]
                       (if (= :link-type tag) (link-entries (:links schema)) (seq (section tag))))
         type-project  (fn [t]
                         (or (some-> (get-in schema [:objects (kw t) :project]) kw)
                             (get known (kw t))))
         repo-project  (fn [r] (some-> (get-in schema [:repositories (kw r) :project]) kw))
         link-project  (fn [l]
                         (some (fn [[n from _ _ opts]]
                                 (when (= (kw l) (kw n))
                                   (or (some-> (:project opts) kw) (type-project from))))
                               (:links schema)))
         fn-project    (fn [f] (let [spec (get-in schema [:functions (kw f)])]
                                 (or (some-> (:project spec) kw)
                                     (some-> (:repository spec) repo-project)
                                     (get-in known [:functions (kw f)]))))
         ds-project  (fn [d] (or (some-> (get-in schema [:datasets (kw d) :project]) kw)
                                 (get-in known [:datasets (kw d)])))
         producers   (fn [d]
                       (filter (fn [[_ t]] (and (map? t) (some #(= (kw d) (kw %)) (outputs-of t))))
                               (:transforms schema)))
         logs        (delay (into {} (for [[an a] (:actions schema) :when (map? a)
                                           :let [p (some-> (:project a) kw)] :when p]
                                       [(log-type-of an a) p])))
         backers     (delay (into {} (for [[n o] (:objects schema) :when (map? o)
                                           :let [d (kw (or (:dataset o)
                                                           (get-in o [:backing :dataset])))]
                                           :when d]
                                       [d [(kw n) o]])))]
     {:entries          entries
      :name-of          (fn [_tag [k _]] (kw k))
      :rid-of           (fn [_tag _] nil)
      :rules            (fn [spec] (refs/steps (:rules spec)))
      :params           (fn [spec] (map val (sort-by (comp str key) (:params spec))))
      :link-from        ::from
      :viewed-type      :object-type
      :root-type        :root
      :type-project       type-project
      :link-project       link-project
      :function-project   fn-project
      :repository-project repo-project
      :dataset-project    ds-project
      :producers          producers
      :backed-type        (fn [d] (get @backers (kw d)))
      :log-project        (fn [nm] (get @logs (kw nm)))})))

(defn assoc-project
  [schema tag nm p]
  (if (= :link-type tag)
    (update schema :links
            (fn [ls] (mapv (fn [[n :as l]]
                             (if (= (kw nm) (kw n))
                               (let [l (if (< (count l) 5)
                                         (into (vec l) (repeat (- 5 (count l)) nil)) l)]
                                 (assoc l 4 (assoc (or (get l 4) {}) :project p)))
                               l))
                           ls)))
    (assoc-in schema [(member/author-sections tag) nm :project] p)))

(defn installed
  [current]
  (let [name-of    (fn [section x]
                     (when x
                       (or (some-> (get-in current
                                           [section (str x) (get rid/name-field-of section)]) kw)
                           (kw x))))
        project    (fn [p] (name-of :projects p))
        named      (atom {})
        names-in   (fn [section]
                     (or (get @named section)
                         (let [f (get rid/name-field-of section)
                               m (reduce (fn [m [_ v]]
                                           (let [n (kw (get v f))]
                                             (cond-> m (not (contains? m n)) (assoc n v))))
                                         {} (get current section))]
                           (swap! named assoc section m)
                           m)))
        by-name    (fn [section x]
                     (or (get-in current [section (str x)])
                         (get (names-in section) (kw x))))
        project-in   (fn [section x] (some-> (by-name section x) :project project))
        logs         (delay (into {} (for [[an a] (:actions current) :when (map? a)
                                           :let [p (some-> (:project a) project)] :when p]
                                       [(log-type-of (kw (or (:action-name a) an)) a) p])))
        backers      (delay (into {} (for [[k ot] (:object-types current) :when (map? ot)
                                           d [(:dataset ot) (get-in ot [:backing :dataset])]
                                           :when d]
                                       [(str d) [k ot]])))
        producers    (delay (reduce (fn [m [_ t :as e]]
                                      (if (map? t)
                                        (reduce #(update %1 %2 (fnil conj []) e) m
                                                (distinct (map str (outputs-of t))))
                                        m))
                                    {} (:transforms current)))
        ds-key       (fn [d]
                       (if (contains? (:datasets current) (str d))
                         (str d)
                         (some (fn [[k v]]
                                 (when (= (kw d) (kw (get v (get rid/name-field-of :datasets))))
                                   (str k)))
                               (:datasets current))))]
    {:entries          (fn [tag] (seq (get current (installed-section tag))))
     :name-of          (fn [tag [k spec]]
                         (or (some-> (get spec (get rid/name-field-of (installed-section tag))) kw)
                             (kw k)))
     :rid-of           (fn [_tag [k _]] (when (string? k) k))
     :rules            (fn [spec] (refs/steps (:rules spec)))
     :params           (fn [spec] (vals (:params spec)))
     :link-from        :from-object-type
     :viewed-type      :object-type
     :root-type        :root
     :type-project       #(project-in :object-types %)
     :link-project       #(project-in :link-types %)
     :function-project   #(project-in :functions %)
     :repository-project #(project-in :repositories %)
     :dataset-project    #(project-in :datasets %)
     :producers          (fn [d] (get @producers (str d)))
     :backed-type        (fn [d] (some->> (ds-key d) (get @backers)))
     :log-project        (fn [nm] (get @logs (name-of :object-types nm)))
     :project-name     project}))
