(ns sutra.media.sets
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.kit.digest :as digest]
            [sutra.types.media :as mt]))

(set! *warn-on-reflection* true)

(defn- branch-of [store] (or (:branch store) ctl/trunk))

(defn declared
  [schema nm]
  (when-let [rid (lookup/rid-of schema :media-sets (keyword nm))]
    [rid (get-in schema [:media-sets rid])]))

(defn items
  ([store] (ctl/list store :media-set-items {:branch (branch-of store)}))
  ([store media-set]
   (let [own   (ctl/list store :media-set-items {:branch    (branch-of store)
                                                 :media-set (keyword media-set)})
         mine  (into #{} (map :path) own)
         below (when (not= ctl/trunk (branch-of store))
                 (ctl/list store :media-set-items {:branch    ctl/trunk
                                                   :media-set (keyword media-set)}))]
     (into (vec own) (remove #(mine (:path %))) below))))

(defn set-name
  [schema ref]
  (or (lookup/name-of schema :media-sets ref)
      (keyword ref)))

(defn virtual-address
  [media-set physical-path]
  (digest/sha256-hex (str "virtual\u0000" (name media-set) "\u0000" physical-path)))

(defn virtual-item
  [store address]
  (some #(when (:source %) %) (ctl/list store :media-set-items {:address (str address)})))

(defn synced-files
  [store media-set]
  (into {} (map (fn [{:keys [path source-modified size]}]
                  [path {:modified source-modified :size size}]))
        (items store media-set)))

(defn signature
  [store media-set]
  (digest/sha256-hex
   (pr-str (sort (map (juxt :path :address :source-modified) (items store media-set))))))

(defn holds?
  [store media-set address]
  (boolean (some #(= (str address) (:address %)) (items store media-set))))

(defn addresses
  [store]
  (into #{} (keep :address) (items store)))

(defn record!
  [store {:keys [media-set path address size mime filename landed-by source physical-path
                 source-modified]}]
  (ctl/upsert! store :media-set-items
               (cond-> {:branch    (branch-of store)
                        :media-set (keyword media-set)
                        :path      (str path)
                        :address   (str address)
                        :size      (long (or size 0))}
                 mime      (assoc :mime (str mime))
                 filename  (assoc :filename (str filename))
                 landed-by (assoc :landed-by (str landed-by))
                 source    (assoc :source (name source) :physical-path (str physical-path))
                 source-modified (assoc :source-modified (str source-modified)))))

(defn- output-item
  [schema nm ms {:keys [path physical address size mime]}]
  (let [src    (:source ms)
        fail   #(refusal/refuse! :refusal.media/item-invalid
                                 {:media-set nm :path path}
                                 {:name (name nm) :why %})
        source (some-> (get-in schema [:sources src]) :source-name)]
    (cond
      (and (:virtual ms) (nil? physical))
      (fail "a virtual media set takes register_media_item, not bytes")
      (and (not (:virtual ms)) physical)
      (fail "register_media_item needs a virtual media set")
      (:virtual ms)
      {:media-set nm :path path :address (virtual-address nm physical)
       :mime mime :filename path :source (or source src) :physical-path physical}
      :else
      {:media-set nm :path path :address address :size size :mime mime :filename path})))

(defn land-refusal!
  [p data]
  (refusal/refuse! (case (:reason p)
                     :undeclared-media-set :refusal.media/undeclared-media-set
                     :format-not-held      :refusal.media/format-not-held)
                   (merge (dissoc p :message) data)
                   {:formats-text (str/join ", " (:formats p))}))

(defn record-output-items!
  [store schema {:keys [media-outputs transform-name]} items]
  (doseq [{:keys [output mime] :as item} items
          :let [ref (:media-set (get media-outputs (keyword output)))
                nm  (when ref (set-name schema ref))
                [_ ms] (when nm (declared schema nm))]]
    (when-not ms
      (refusal/refuse! :refusal.media/undeclared-output
                       {:output output}
                       {:transform transform-name}))
    (when-let [p (mt/format-problem ms mime)]
      (land-refusal! p {:media-set nm :path (:path item)}))
    (record! store (output-item schema nm ms item))))

(defn readable?
  [store schema media-set]
  (let [[rid ms] (declared schema media-set)]
    (boolean
     (when ms
       (try
         (grants/authorize-read! (grants/read-rights store) ms rid)
         (grants/cleared? (grants/clearances (grants/read-rights store)) (:markings ms))
         (catch clojure.lang.ExceptionInfo _ false))))))

(defn land-problem
  [schema media-set mime]
  (if-let [[_ ms] (declared schema media-set)]
    (mt/format-problem ms mime)
    {:reason  :undeclared-media-set
     :set     (keyword media-set)
     :message (str "Kein Media-Set " (pr-str (keyword media-set)) " erklärt."
                   " Medien landen nur in einem erklärten Set — sonst gibt es keine"
                   " Ressource, deren Projekt über das Lesen entscheidet.")}))

(defn expire!
  [store schema now-ms apply?]
  (let [sets (into {} (for [[rid ms] (:media-sets schema)]
                        [(lookup/name-of schema :media-sets rid) ms]))]
    (into []
          (keep (fn [{:keys [media-set path landed-at] :as item}]
                  (let [ms (get sets (keyword media-set))
                        at (cond (number? landed-at) landed-at
                                 (inst? landed-at)   (inst-ms landed-at))]
                    (when (and ms (mt/expired? ms at now-ms))
                      (when apply?
                        (ctl/delete! store :media-set-items
                                     {:branch    (branch-of store)
                                      :media-set (keyword media-set)
                                      :path      path}))
                      (select-keys item [:media-set :path :address :landed-at])))))
          (items store))))

(defn items-signature
  [store schema ref]
  (signature store (set-name schema ref)))
