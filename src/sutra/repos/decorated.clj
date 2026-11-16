(ns sutra.repos.decorated
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.project-emit :as pemit]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(def ^:private output-kinds
  {"dataset" :dataset "object_type" :object-type "link_type" :link-type})

(defn- outputs-in
  [decorator]
  (vec (for [call (re-seq #"\bOutput\((?:[^()]|\([^()]*\))*\)" decorator)
             :let [[_ field nm] (re-find #"\b(dataset|object_type|link_type)\s*=\s*[\"']([^\"']+)"
                                         call)]
             :when nm]
         [(output-kinds field) nm])))

(defn entries
  [text]
  (vec (for [[_ decorator f] (re-seq #"(?ms)^@transform\((.*?)^def\s+(\w+)" (str text))]
         {:function f :outputs (outputs-in decorator)})))

(defn- file->name
  [path]
  (-> (subs path (inc (or (str/last-index-of path "/") -1)))
      (str/replace #"\.py$" "")
      (str/replace "_" "-")
      keyword))

(defn- new-name
  [path multi? {:keys [function outputs]}]
  (let [[kind nm] (first outputs)]
    (cond
      (= :dataset kind) (keyword (str/replace nm "_" "-"))
      multi?            (keyword (str/replace function "_" "-"))
      :else             (file->name path))))

(defn with-code-path
  [k node]
  (let [f (:code-file node)]
    (cond-> node
      (and f (str/ends-with? f ".py")
           (not= f (str "transforms/" (pemit/py-filename (rid/->name k)))))
      (assoc :code-path f))))

(defn- named-outputs
  [names e]
  (update e :outputs (fn [os] (mapv (fn [[kind nm]] [kind (get names nm nm)]) os))))

(defn- refuse-former-names!
  [former entries]
  (let [stale (distinct (for [e entries [kind nm] (:outputs e)
                              :when (and (= :dataset kind) (get former nm))]
                          nm))]
    (when (seq stale)
      (refusal/refuse! :refusal.repos/former-dataset-name
                       {:kind ::former-name :names (vec stale)}
                       {:renames (str/join ", " (map #(str % " is now " (get former %)) stale))}))))

(defn- refuse-shared-outputs!
  [members]
  (let [twice (for [[[kind nm] n] (frequencies (mapcat (comp :outputs second) members))
                    :when (and (= :dataset kind) (> n 1))]
                nm)]
    (when (seq twice)
      (refusal/refuse! :refusal.repos/shared-output
                       {:kind ::shared-output :outputs (vec (sort twice))}
                       {:names (str/join ", " (sort twice))}))))

(defn- member
  [{:keys [outputs gone repository project]} nodes claimed bound path multi? e]
  (let [owner (some (fn [out]
                      (when-let [t (get outputs out)]
                        (when (or (some #{t} bound) (not (claimed t))) t)))
                    (:outputs e))
        nm    (or (when (and (not multi?) (= 1 (count bound))) (first bound))
                  owner
                  (new-name path multi? e))
        node  (or (get nodes nm) (get gone nm))]
    (when-not (and (contains? nodes nm) (not (some #{nm} bound)) (not owner))
      [nm (if (and (= path (:code-file node)) (not multi?) (not (:entry-point node)))
            node
            (with-code-path
              nm
              (cond-> (assoc (dissoc node :entry-point :code-path)
                             :language :python :code-file path :repository repository)
                project (assoc :project project)
                multi?  (assoc :entry-point (:function e)))))])))

(defn- claimed-by-their-files
  [outputs bound files]
  (into #{}
        (mapcat (fn [[p es]]
                  (let [names (map key (bound p))]
                    (if (and (= 1 (count es)) (= 1 (count names)))
                      names
                      (filter (set (keep outputs (mapcat :outputs es))) names)))))
        files))

(defn with-decorated
  [formats paths text-at owner schema]
  (let [nodes   (:transforms schema)
        taken   (into #{} (keep :code-file) (vals (:functions schema)))
        bound   (group-by (comp :code-file val) nodes)
        files   (for [p (sort paths)
                      :when (and (authoring/sibling? formats p) (not (taken p)))
                      :let [es (mapv #(named-outputs (:names owner) %) (entries (text-at p)))]
                      :when (seq es)]
                  [p es])
        _       (refuse-former-names! (:former owner) (mapcat second files))
        _       (refuse-shared-outputs! (for [[_ es] files, e es] [nil e]))
        claimed (claimed-by-their-files (:outputs owner) bound files)
        members (for [[p es] files
                      :let [names (mapv key (bound p))
                            multi? (> (count es) 1)
                            made (keep #(member owner nodes claimed names p multi? %) es)
                            kept (into #{} (map first) made)]]
                  {:made made :dropped (remove kept names)})
        moved   (mapcat :made members)
        dropped (mapcat :dropped members)]
    (cond-> schema
      (or (seq moved) (seq dropped))
      (update :transforms #(into (apply dissoc (or % {}) dropped) moved)))))
