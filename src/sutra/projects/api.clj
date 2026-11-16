(ns sutra.projects.api
  (:require [sutra.projects.entity-resolution :as entity-resolution]
            [sutra.projects.file-grants :as file-grants]
            [sutra.projects.kompass :as kompass]
            [sutra.projects.map :as map]))

(set! *warn-on-reflection* true)

(defn kompass-node [store rid] (kompass/kompass-node store rid))

(defn crosswalk-lookup
  [store object-type id-kind id-value]
  (kompass/crosswalk-lookup store object-type id-kind id-value))

(defn crosswalk-put!
  [store object-type id-kind id-value entity-id]
  (kompass/crosswalk-put! store object-type id-kind id-value entity-id))

(defn crosswalk-redirect!
  [store object-type loser-ids survivor-id]
  (kompass/crosswalk-redirect! store object-type loser-ids survivor-id))

(defn import-key-counts [store] (kompass/import-key-counts store))

(defn placement-deletions
  [store branch ref] (kompass/placement-deletions store branch ref))

(defn entity-state-by-type
  [store branch] (entity-resolution/entity-state-by-type store branch))

(defn entity-state-elsewhere
  [store branch] (entity-resolution/entity-state-elsewhere store branch))

(defn sweep-entity-resolution!
  [store branch resolved-rids opts]
  (entity-resolution/sweep-entity-resolution! store branch resolved-rids opts))

(defn index-times [store] (kompass/index-times store))

(defn latest-provenance-by-source
  [store entity-id] (kompass/latest-provenance-by-source store entity-id))

(defn read-kompass-annotations [store] (kompass/read-kompass-annotations store))

(defn read-kompass-nodes [store] (kompass/read-kompass-nodes store))

(defn read-import-plan [store] (kompass/read-import-plan store))

(defn retract-provenance!
  [store entity-id source run-id] (kompass/retract-provenance! store entity-id source run-id))

(defn write-import-keys! [store pk-index] (kompass/write-import-keys! store pk-index))

(defn write-import-plan! [store plan] (kompass/write-import-plan! store plan))

(defn write-index-times!
  [store object-types run-id] (kompass/write-index-times! store object-types run-id))

(defn write-provenance! [store rows] (kompass/write-provenance! store rows))

(defn project-map
  [schema applications counts project] (map/project-map schema applications counts project))

(defn file-roles-held [store auth] (file-grants/held store auth))

(defn file-role-allows? [shares ref op] (file-grants/allows? shares ref op))

(defn file-role-may? [store auth ref op] (file-grants/may? store auth ref op))

(defn favorites [store principal] (kompass/favorites store principal))

(defn unplace! [store auth ref] (kompass/unplace! store auth ref))
