(ns sutra-ui.apps.notebook.ymodel)

(defn ymap [^js ydoc k] (.getMap ydoc (name k)))

(defn entries
  [ydoc colls]
  (into {} (for [k colls]
             [(name k) (let [acc (volatile! {})]
                         (.forEach (ymap ydoc k) (fn [v id] (vswap! acc assoc id v)))
                         @acc)])))

(defn seed!
  [^js ydoc es]
  (.transact ydoc #(doseq [[c xs] es [id v] xs] (.set (ymap ydoc c) id v)) "seed"))

(defn awareness-states [^js aw]
  (let [acc (volatile! {})]
    (.forEach (.getStates aw) (fn [v k] (vswap! acc assoc k (js->clj v :keywordize-keys true))))
    @acc))

(defn new-id [] (subs (str (random-uuid)) 0 8))

(defn now [] (.toISOString (js/Date.)))
