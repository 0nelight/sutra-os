(ns support.install
  (:require [sutra.catalog.kernel :as kernel]
            [sutra.authoring.install.world :as onto-install]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.types.entities :as ents]))

(defn- with-fixture-project
  [world]
  (if (some (fn [[k spec]] (or (= :fixture k) (= "fixture" (some-> (:project-name spec) name))))
            (:projects world))
    world
    (update world :projects #(merge {:fixture {}} %))))

(defn- instance-part
  [W]
  (into {} (for [[s ks] (ents/instance-keys W)
                 :let [entries (if (= :all ks) (get W s) (select-keys (get W s) ks))]
                 :when (seq entries)]
             [s entries])))

(defn seat-on-main!
  [installed]
  (let [m   (ctx/system-store "main")
        old (or (world/authored m) {})
        new (merge-with merge old (instance-part installed))]
    (when (not= old new)
      (onto-install/install-on-branch! m new))))

(defn run-installing!
  [branch world rules]
  (let [r (kernel/eval-steps rules (with-fixture-project world) {} {}
                             {:installer {} :project :fixture})
        b (or branch "main")]
    (when-let [installed (:installed r)]
      (when (not= "main" b) (seat-on-main! installed))
      (onto-install/install-on-branch! (ctx/system-store b) installed))
    (:world r)))
