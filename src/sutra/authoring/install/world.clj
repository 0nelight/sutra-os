(ns sutra.authoring.install.world
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [clojure.walk :as walk]
            [sutra.store.access.scope :as scope]
            [sutra.catalog.grants :as grants]
            [clojure.string :as str]
            [sutra.store.lib :as lib]
            [sutra.types.entities :as ents]
            [sutra.types.versions :as vers]
            [sutra.kit.digest :as digest]
            [sutra.store.control :as ctl]
            [sutra.store.api :as api]
            [sutra.types.rid :as rid]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.world-gates :as world-gates]
            [sutra.world.db :as kdb]
            [sutra.store.kept :as kept]
            [sutra.catalog.signature :as signature]
            [sutra.catalog.backing-checks :as backing]
            [sutra.store.world :as world]
            [sutra.ref.install :as ri]))

(set! *warn-on-reflection* true)

(defn- normalize-schema
  ([user-schema] (normalize-schema user-schema nil))
  ([user-schema installed] (ri/normalise kernel/install-cx user-schema installed)))

(defn prepare-schema
  [user-schema {:keys [old-schema project verdict] :or {verdict kernel/verdict}}]
  (let [installed (some-> (force old-schema) not-empty)
        old       (some-> installed normalize-schema)
        N         (cond-> (normalize-schema user-schema installed)
                    project (as-> n (member/place-new forms/installed (or old {}) n project)))
        {:keys [ok reasons] :as v} (verdict N old)]
    (if ok v (-> v (dissoc :reasons) (assoc :invalid reasons)))))

(defn- snapshot-digest [snapshot]
  (digest/sha256-hex
   (pr-str (walk/postwalk #(if (map? %)
                             (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b)))) %) %)
                          (vers/snapshot snapshot)))))

(defn- releases
  [b world]
  (for [[section m] world
        :when (and (map? m) (rid/name-field-of section))
        [_ spec] m
        :let  [nm (some-> (get spec (rid/name-field-of section)) name)]
        :when nm
        [version snapshot] (:releases spec)]
    {:key     {:branch (str b) :section (name section) :name nm :version (str version)}
     :digest  (snapshot-digest snapshot)
     :section section :name nm :version version}))

(defn- release-conflict [{:keys [section version] nm :name}]
  (refusal/refusal :refusal.authoring/release-republished
                   {:reason :release-conflict :section section :name nm :version version}
                   {:section-name (name section)}))

(defn- release-entries
  [store b world]
  (vec
   (for [{:keys [key digest] :as r} (releases b world)
         :let  [old (ctl/get store :releases key)]
         :when (not= digest (:digest old))]
     (if old
       (throw (release-conflict r))
       (assoc (ctl/entry store :releases (assoc key :digest digest)) :expect api/absent)))))

(defn install-world!
  [store b world opts]
  (when (nil? (:authority store))
    (anomaly/raise! :fault (str "an install on " b " names its authority (the request's, or"
                                " sutra.store.access.scope/as-system)")
                    {:branch b}))
  (let [opts (merge {:actor              (grants/principal store)
                     :allow-schema-wipe? (scope/schema-wipe-allowed?)
                     :dataset-decls      signature/datasets-of
                     :dataset-view       {:init signature/decls-init
                                          :step signature/decls-step}}
                    (dissoc opts :basis :step)
                    {:records (into (release-entries store b world) (:records opts))})]
    (kept/commit! store b world (:step opts)
                  #(world/commit! (world/at store b) (merge opts %)))))

(defn- refuse-instance-change!
  [store old w]
  (when-let [ks (and (world/instance-home store) (world/instance-keys store old w))]
    (let [d     (lib/diff-paths (ents/instance-view ks (or old {})) (ents/instance-view ks w))
          named (into (sorted-map)
                      (for [[s ch] d]
                        [s (vec (sort (for [k (mapcat keys (vals ch))]
                                        (str (or (some-> (or (get-in w [s k]) (get-in old [s k]))
                                                         (get (rid/name-field-of s)) name)
                                                 k)))))]))]
      (when (seq d)
        (refusal/refuse! :refusal.authoring/instance-scoped-on-branch
                         {:reason :instance-scoped :branch (:branch store)
                          :changed named}
                         {:named-text (str/join "; " (for [[s ns] named]
                                                       (str (name s) " " (str/join ", " ns))))})))))

(defn- stepped-world
  [{:keys [before after]} project]
  (let [after                         (kept/placed before after project)
        {:keys [ok reasons findings]} (world-gates/verdict before after)]
    (when-not ok (throw (kernel/schema-refusal reasons findings)))
    (backing/validate-external-sources! ok)
    {:old (kdb/world before) :w ok :step {:before before :after after}}))

(defn- install-over!
  [store b user-schema {:keys [guard project]}]
  (let [st                   (or (kept/step-of user-schema)
                                 (kept/step-of (kept/whole (kept/db store b) user-schema)))
        {:keys [old w step]} (stepped-world st (force project))]
    (refuse-instance-change! store old w)
    (install-world! (cond-> store guard (guard (or old {}) w)) b w {:step step})
    w))

(defn install-on-branch!
  ([store user-schema] (install-on-branch! store user-schema nil))
  ([store user-schema opts]
   (install-over! store (:branch store) user-schema opts)))

(defn install-schema!
  ([store user-schema] (install-schema! store user-schema nil))
  ([store user-schema opts]
   (install-over! store (str (:branch store)) user-schema opts)))
