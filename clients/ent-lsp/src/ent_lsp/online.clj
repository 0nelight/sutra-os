(ns ent-lsp.online
  (:require [ent-lsp.facts :as facts]
            [ent-lsp.grammar :as grammar]
            [ent-lsp.read :as read]
            [sutra.parser.project-emit :as pemit]))

(def file
  "⟨instanz⟩")

(defn from-schema
  [spec schema]
  (let [sugar  (get-in (pemit/export schema {:all-projects? true}) [:ent :schema])
        parsed (read/parse (pr-str {:schema sugar}))]
    {:facts {:defs (:defs (facts/extract spec (str file ".ent") parsed)) :refs [] :writes []}
     :table (grammar/symbol-table [schema])}))

(defn- local-names [local-facts]
  (let [ds (remove #(or (:draft %) (:extension %)) (mapcat :defs (vals local-facts)))]
    {:taken   (into #{} (comp (remove :object) (map (juxt :kind :name))) ds)
     :objects (into #{} (comp (filter #(= :object (:kind %))) (map :name)) ds)}))

(defn facts-beside
  [online local-facts]
  (when online
    (let [{:keys [taken objects]} (local-names local-facts)]
      (update (:facts online) :defs
              (fn [ds] (into [] (remove #(if (:object %)
                                           (objects (:object %))
                                           (taken [(:kind %) (:name %)])))
                             ds))))))
