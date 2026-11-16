(ns sutra-ui.apps.enta-editor.names
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.prop-source :as ps]
            [sutra.capps.bind :as bind]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.enta-editor.fields :as f]))

(def ^:private slot-keys
  {:items :ui/app-builder-slot-items :body :ui/app-builder-slot-body
   :empty :ui/app-builder-slot-empty :tabs :ui/app-builder-slot-tabs
   :pages :ui/app-builder-slot-pages :variants :ui/app-builder-slot-variants})

(defn slot-label [slot]
  (if-let [k (get slot-keys (->kw slot))] (i18n/t k) (some-> slot name)))

(defn node-type-of
  [idx node]
  (when (= :widget (cat/node-type node))
    (ps/node-object-type idx node)))

(defn node-label
  ([tree node] (node-label tree (ps/vars-index tree) node))
  ([_tree idx node]
   (let [spec (cat/node-spec node)]
     (if (or (nil? node) (nil? spec))
       (cat/node-label node)
       (let [t (or (:title node) (:label node)
                   (some-> (node-type-of idx node) (f/type-label true)))]
         (cond-> (:label spec) (seq (str t)) (str " · " t)))))))

(defn app-label [file]
  (or (not-empty (str (:display-name file)))
      (some-> (:name file) f/var-label)
      "App"))

(defn crumb-entries
  [tree path]
  (let [idx (ps/vars-index tree)]
    (vec (keep (fn [n]
                 (let [p (subvec (vec path) 0 n)
                       x (cat/node-at tree p)]
                   (when (and (map? x) (not= :unknown (cat/node-type x)))
                     {:path p :label (node-label tree idx x)})))
               (range 0 (count path))))))

(defn labelled-entries
  [tree]
  (let [idx (ps/vars-index tree)]
    (mapv #(assoc % :label (node-label tree idx (:node %))) (cat/flatten-tree tree))))

(defn type-users
  [tree t]
  (let [idx (ps/vars-index tree)
        t   (->kw t)]
    (vec (for [{:keys [path node]} (cat/flatten-tree tree)
               :when (= t (node-type-of idx node))]
           {:path path :label (node-label tree idx node)}))))

(defn- refs-in
  [v under]
  (cond
    (and (map? v) (bind/variable-ref? v)) [[under v]]
    (map? v)        (mapcat (fn [[k x]] (refs-in x k)) v)
    (sequential? v) (mapcat #(refs-in % under) v)
    :else           nil))

(defn- ref-name [m] (->kw (or (:var m) (:val m) (:from m))))

(defn var-usage
  [tree]
  (let [idx      (ps/vars-index tree)
        declared (set (cat/declared-vars tree))
        add      (fn [acc nm role entry]
                   (if (contains? declared nm)
                     (update-in acc [nm role] (fnil conj []) entry)
                     acc))]
    (reduce
     (fn [acc {:keys [path node]}]
       (if-not (map? node)
         acc
         (let [label  (node-label tree idx node)
               entry  {:path path :label label}
               kinds  (into {} (map (juxt :key :kind)) (cat/node-params node))
               own    (apply dissoc node :vars cat/path-key (map :key (cat/child-slots node)))
               acc    (reduce (fn [acc [nm spec]]
                                (let [nm (->kw nm)
                                      acc (update-in acc [nm :declared] (fnil conj [])
                                                     {:path path :spec spec :label label})]
                                  (reduce (fn [acc other]
                                            (add acc other :reads
                                                 {:path path :label (f/var-label nm)
                                                  :var nm}))
                                          acc
                                          (distinct
                                           (concat (keep (comp ->kw spec) [:from :of :source])
                                                   (map (comp ref-name second)
                                                        (refs-in spec nil)))))))
                              acc (when (map? (:vars node)) (:vars node)))]
           (reduce (fn [acc [k v]]
                     (let [kind (get kinds k)]
                       (if (and (= :var-name kind) (some? v))
                         (add acc (->kw v) :writes entry)
                         (reduce (fn [acc [under m]]
                                   (let [write? (or (and (= :handle kind) (contains? m :var))
                                                    (and (= :event kind) (= :var under)))]
                                     (add acc (ref-name m) (if write? :writes :reads) entry)))
                                 acc (distinct (refs-in v k))))))
                   acc own))))
     {}
     (cat/flatten-tree tree))))
