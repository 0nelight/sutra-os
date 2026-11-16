(ns hooks.binding-macros
  (:require [clj-kondo.hooks-api :as api]))

(defn binds-first
  [{:keys [node]}]
  (let [[_ bv & body] (:children node)
        [sym & exprs] (:children bv)]
    (when-not (and bv (api/vector-node? bv)
                   (or (api/token-node? sym) (api/vector-node? sym) (api/map-node? sym)))
      (api/reg-finding! (assoc (meta (or bv node))
                               :message "expects a binding vector [sym …]"
                               :type :binding-macro)))
    {:node (api/list-node
            (list* (api/token-node 'let)
                   (api/vector-node
                    [sym (api/list-node (list* (api/token-node 'do)
                                               (or (seq exprs) [(api/token-node nil)])))])
                   body))}))

(defn defplugin
  [{:keys [node]}]
  (let [[_ _name & hooks] (:children node)]
    {:node (api/list-node
            (list* (api/token-node 'do)
                   (for [h hooks
                         :let [[_ params & body] (:children h)]]
                     (api/list-node (list* (api/token-node 'fn) params body)))))}))

(defn defop
  [{:keys [node]}]
  (let [[_ op params & body] (:children node)]
    {:node (api/list-node
            (list (api/token-node 'do) op
                  (api/list-node (list* (api/token-node 'fn) params body))))}))
