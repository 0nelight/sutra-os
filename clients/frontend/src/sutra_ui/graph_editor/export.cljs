(ns sutra-ui.graph-editor.export
  (:require [sutra.viz.graph-dot :as gdot]
            [sutra.viz.graph-svg :as gsvg]
            [sutra-ui.download :as dl]
            [sutra-ui.state.instances :as instances]))

(def ^:private type->kind
  {"objectType"     :object-type
   "objectInstance" :object-type
   "datasource"     :datasource
   "dataset"        :dataset
   "transform"      :transform
   "action"         :action
   "function"       :function
   "application"    :application})

(defn- rf->node [n]
  (let [kind (get type->kind (:type n))]
    {:id     (:id n)
     :kind   kind
     :label  (or (get-in n [:data :label]) (get-in n [:data :name]) (:id n))
     :x      (get-in n [:position :x] 0)
     :y      (get-in n [:position :y] 0)
     :w      (or (:width n) (get-in n [:measured :width]) (get-in n [:style :width]))
     :h      (or (:height n) (get-in n [:measured :height]) (get-in n [:style :height]))
     :accent (get-in n [:data :paint])
     :icon   (get-in n [:data :icon])}))

(defn- rf->edge [e]
  {:id     (:id e)
   :source (:source e)
   :target (:target e)
   :label  (or (:label e) (get-in e [:data :label]) (get-in e [:data :transform])
               (get-in e [:data :link]))})

(defn- drawing [rf-nodes rf-edges title]
  {:title title
   :nodes (->> (js->clj rf-nodes :keywordize-keys true) (remove :hidden) (mapv rf->node))
   :edges (->> (js->clj rf-edges :keywordize-keys true) (remove :hidden) (mapv rf->edge))})

(defn graph->svg
  [rf-nodes rf-edges title]
  (gsvg/graph->svg (drawing rf-nodes rf-edges title)))

(defn graph->dot
  [rf-nodes rf-edges title]
  (gdot/graph->dot (drawing rf-nodes rf-edges title)))

(def ^:private formats
  {:svg {:ext ".svg" :mime "image/svg+xml;charset=utf-8" :render graph->svg}
   :dot {:ext ".dot" :mime "text/vnd.graphviz;charset=utf-8" :render graph->dot}})

(defn export!
  [fmt file-name]
  (when-let [^js inst @instances/rf-instance]
    (let [{:keys [ext mime render]} (get formats fmt (:svg formats))]
      (dl/text! (render (.getNodes inst) (.getEdges inst) nil)
                 mime
                 (str (or file-name "graph") ext)))))

(defn export-svg! [file-name] (export! :svg file-name))

(defn export-dot! [file-name] (export! :dot file-name))
