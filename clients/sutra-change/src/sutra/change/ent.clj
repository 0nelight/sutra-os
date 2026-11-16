(ns sutra.change.ent
  (:require [sutra.change.git :as git]
            [sutra.i18n.core :refer [t]]
            [clojure.string :as str]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]))

(def sections
  (into [] (comp (drop 1) (map first)) (rest ent/EntSchema)))

(defn changeset-inhalt?
  [formats path]
  (= :entity (pe/plane-of formats path)))

(defn ent-paths
  [formats paths]
  (->> paths (filter #(authoring/file? formats %)) sort vec))

(defn- parse*
  [formats path text]
  (try
    (let [[_ m] (authoring/document formats path text)
          kind  (authoring/unit-kind formats path)]
      (if-not (map? m)
        {:kaputt (t :change/err-not-a-map)}
        {:roh    m
         :kind   kind
         :schema (or (try (:schema (authoring/read-file formats path text))
                          (catch Exception _ nil))
                     (when (and (= :app kind) (keyword? (:name m)))
                       {:applications {(:name m) m}})
                     {})}))
    (catch Exception e {:kaputt (or (ex-message e) "unlesbar")})))

(def parse (memoize parse*))

(defn resources
  [{:keys [schema]}]
  (persistent!
   (reduce (fn [acc sec]
             (let [v (get schema sec)]
               (cond
                 (map? v)    (reduce-kv (fn [a k e] (assoc! a [sec k] e)) acc v)
                 (vector? v) (reduce (fn [a e]
                                       (if (and (vector? e) (keyword? (first e)))
                                         (assoc! a [sec (first e)] e)
                                         a))
                                     acc v)
                 :else acc)))
           (transient {})
           sections)))

(defn- code-file->resource
  [resources path]
  (let [base (last (str/split path #"/"))]
    (some (fn [[[sec nm] _]]
            (when (or (and (#{:transforms :functions :applications} sec)
                           (#{(pemit/py-filename nm) (pemit/clj-filename nm)
                              (pemit/cljs-filename nm)} base))
                      (and (= :templates sec)
                           (contains? (set (pemit/template-filenames nm)) base)))
              [sec nm]))
          resources)))

(defn attribute-paths
  [formats resources paths]
  (into {} (for [p paths
                 :when (not (authoring/file? formats p))]
             [p (when (changeset-inhalt? formats p) (code-file->resource resources p))])))

(defn diff
  [base head]
  (let [ks (into (sorted-set) (concat (keys base) (keys head)))]
    (into {}
          (for [k ks
                :let [a (get base k) b (get head k)]
                :when (not= a b)]
            [k (cond
                 (nil? a) {:art :neu     :nachher b}
                 (nil? b) {:art :entfernt :vorher a}
                 :else    {:art :geändert :vorher a :nachher b
                           :felder (when (and (map? a) (map? b))
                                     (vec (sort (for [f (into (set (keys a)) (keys b))
                                                      :when (not= (get a f) (get b f))]
                                                  f))))})]))))

(defn- per-text
  [texts]
  (reduce (fn [acc [p m]]
            (let [rs (resources m)]
              (-> acc
                  (update :resources merge rs)
                  (update :quellen merge (zipmap (keys rs) (repeat p))))))
          {:resources {} :quellen {}}
          texts))

(defn tree-at
  [formats dir rev paths]
  (let [text-of (fn [p]
                  (if rev (git/show dir rev p)
                    (try (slurp (str dir "/" p)) (catch Exception _ nil))))
        parsed  (into (sorted-map)
                      (keep (fn [p] (when-let [t (text-of p)] [p [t (parse formats p t)]]))) paths)
        kaputt  (into {} (for [[p [_ m]] parsed :when (:kaputt m)] [p (:kaputt m)]))
        good    (into (sorted-map) (for [[p [t m]] parsed :when (not (:kaputt m))] [p [t m]]))
        graph   (try (pe/read-texts formats (keys good) #(first (get good %)))
                     (catch clojure.lang.ExceptionInfo _ nil))]
    (assoc (if graph
             (let [rs (resources graph)]
               {:resources rs
                :quellen   (into {} (for [[[sec k] _] rs
                                          :let [q (get-in graph [:placement [sec (name k)]])]
                                          :when q]
                                      [[sec k] q]))})
             (per-text (for [[p [_ m]] good] [p m])))
           :kaputt kaputt)))
