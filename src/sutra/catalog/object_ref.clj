(ns sutra.catalog.object-ref
  (:require [sutra.catalog.lookup :as lookup]
            [sutra.types.world-rules :as rules]
            [sutra.ref.objects :as ro]))

(set! *warn-on-reflection* true)

(defn- worded
  [schema rid {:keys [reason key source value] :as problem}]
  (let [find  (or (get-in schema [:object-types rid :object-type-name])
                  (get-in schema [:interfaces rid :interface-name])
                  rid)
        label (case source :primary-key "its primary key" :match-keys "its :match-keys" nil)]
    (assoc problem :message
           (case reason
             :ambiguous (str (pr-str value) " is the " (name key) " of more than one "
                             (name find) " — " (name key) " is not unique in the data,"
                             " so pass the object id instead")
             (str "names no live " (name find) ": " (pr-str value)
                  (if key
                    (str " is neither a " (name key) " (" label ") nor an object id")
                    (str " is not an object id — " (name find)
                         " declares no single key property, so only the"
                         " object id identifies one")))))))

(defn resolve-ref
  [schema rid raw lookup]
  (let [cx {:sigma {:object-key rules/object-key
                    :attr-types (fn [W T] (:types (lookup/attrs-index W T)))
                    :lookup     (fn [_W _T q] (lookup q))}}]
    (update (ro/resolve-ref cx schema rid raw) :problem #(some->> % (worded schema rid)))))
