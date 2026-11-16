(ns sutra-ui.state.selection
  (:require [sutra-ui.state.db :as db]
            [sutra-ui.state.instances :as instances]))

(defonce canvas-selection (db/cursor [:selection :canvas-selection] #{}))

(defonce selection-push (db/cursor [:selection :selection-push] 0))

(defn selected-node-ids [] @canvas-selection)

(defn note-canvas-selection!
  [ids]
  (db/put! canvas-selection (set ids)))

(defn select-nodes!
  [ids]
  (db/put! canvas-selection (set ids))
  (db/edit! selection-push inc))

(defn clear-selection! [] (select-nodes! #{}))

(defn select-all-nodes! [] (select-nodes! (instances/canvas-node-ids)))

(defn selected-boxes
  []
  (when-let [^js inst @instances/rf-instance]
    (let [sel @canvas-selection]
      (vec (for [^js n (array-seq (.getNodes inst))
                 :when (contains? sel (.-id n))
                 :let [p (.-position n)]
                 :when p]
             {:id (.-id n) :x (.-x p) :y (.-y p)
              :w (or (.-width n) 180) :h (or (.-height n) 60)})))))
