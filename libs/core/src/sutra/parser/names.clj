(ns sutra.parser.names
  (:require [sutra.parser.synonyms :as syn]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]))

(set! *warn-on-reflection* true)

(defn index
  [lens]
  {:entries  (vec (for [tag forms/tags
                        [_ _spec :as entry] ((:entries lens) tag)
                        :let [nm ((:name-of lens) tag entry)]
                        :when nm]
                    {:kind  tag
                     :name  nm
                     :rid   ((:rid-of lens) tag entry)
                     :project (when (member/member-tags tag)
                              (some-> (member/project-of tag entry lens)
                                      ((:project-name lens keyword))))}))
   :synonyms (vec (for [[p-key spec] ((:entries lens) :project)
                        :let [p ((:name-of lens) :project [p-key spec])]
                        [alias target] (:synonyms spec)]
                    {:alias (keyword (name alias)) :name (keyword (name target)) :kind :object-type
                     :project p}))})

(defn- by-folded [entries]
  (group-by (comp syn/fold :name) entries))

(defn duplicates
  [{:keys [entries]}]
  (vec (for [[[kind _] es] (group-by (juxt :kind (comp syn/fold :name)) entries)
             :when (< 1 (count es))]
         {:kind kind :name (:name (first es)) :entries (vec es)})))

(defn homonyms
  [{:keys [entries]}]
  (vec (for [[_ es] (by-folded entries)
             :let [kinds (into (sorted-set) (map :kind) es)]
             :when (< 1 (count kinds))]
         {:name (:name (first es)) :kinds kinds :entries (vec es)})))

(defn shadows
  [{:keys [entries synonyms]}]
  (let [names (by-folded entries)]
    (vec (for [{:keys [alias kind] :as s} synonyms
               :let [hit (filter #(= kind (:kind %)) (get names (syn/fold alias)))]
               :when (seq hit)]
           (assoc s :hides (vec hit))))))

(defn lookup
  [{:keys [entries synonyms]} nm kind]
  (let [f      (syn/fold nm)
        kind?  #(or (nil? kind) (= kind (:kind %)))
        direct (filter #(and (kind? %) (= f (syn/fold (:name %)))) entries)
        via    (for [{:keys [alias name project] k :kind} synonyms
                     :when (and (= f (syn/fold alias)) (or (nil? kind) (= kind k)))
                     e entries
                     :when (and (= k (:kind e)) (= (syn/fold name) (syn/fold (:name e))))]
                 (assoc e :via {:synonym alias :of project}))]
    (vec (distinct (concat direct via)))))

(defn suggest
  [{:keys [entries synonyms] :as idx} nm kind site]
  (let [exact (lookup idx nm kind)
        mine  (filter #(= (some-> site name keyword) (:project %)) exact)
        homes (distinct (keep :project exact))
        of-kind (filter #(= kind (:kind %)) entries)
        project-of (into {} (map (juxt :name :project)) of-kind)
        known (into {} (for [s synonyms :when (= kind (:kind s))] [(:alias s) (:name s)]))
        cands (when (empty? exact)
                (mapv #(assoc % :project (project-of (:name %)))
                      (syn/candidates nm (map :name of-kind) known)))]
    {:verdict    (cond (seq mine)           :exists
                       (= 1 (count homes))  :use
                       (< 1 (count homes))  :ambiguous
                       (seq exact)          :exists
                       (seq cands)          :close
                       :else                :create)
     :exact      exact
     :candidates (or cands [])}))
