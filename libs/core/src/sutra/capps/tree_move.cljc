(ns sutra.capps.tree-move
  (:require [sutra.capps.catalog :as cat]))

(def zones [:before :into :after])

(defn parent-slot [path]
  (let [path (vec path)]
    (when (seq path)
      (let [lst (peek path)]
        (if (integer? lst)
          [(vec (drop-last 2 path)) (nth path (- (count path) 2))]
          [(vec (butlast path)) lst])))))

(defn- slot-spec [node slot]
  (some #(when (= slot (:key %)) %) (cat/child-slots node)))

(defn- holds [spec]
  (cond (:many spec) :many (:panes spec) :panes :else :single))

(defn node? [n] (contains? #{:widget :container} (cat/node-type n)))

(defn owner
  [tree path]
  (let [p (vec path)]
    (first (for [i    (range (count p) -1 -1)
                 :let [base (subvec p 0 i)
                       n    (cat/node-at tree base)]
                 :when (node? n)
                 :let [rest* (subvec p i)
                       kid   (some #(when (= rest* (vec (:path %))) %) (cat/children-of n))]
                 :when kid]
             {:base base :kid kid}))))

(defn handle
  [tree path]
  (when-let [{:keys [base kid]} (owner tree path)]
    (into base (:remove-path kid))))

(defn node-path-up
  [tree path]
  (:base (owner tree path) []))

(defn pane? [n]
  (boolean (and (map? n) (contains? n :body) (nil? (cat/node-spec n)))))

(defn- into-slot
  [node]
  (let [ss (cat/child-slots node)]
    (or (some #(when (:many %) %) ss)
        (some #(when (:panes %) %) ss)
        (let [s (first ss)]
          (when (and s (not (:many s)) (not (:panes s)) (nil? (get node (:key s))))
            s)))))

(defn target
  [tree path zone]
  (let [path (vec path)]
    (if (= :into zone)
      (let [node (cat/node-at tree path)]
        (when-let [s (into-slot node)]
          (let [h (holds s)]
            {:parent path :slot (:key s) :holds h
             :index (when-not (= :single h) (count (get node (:key s))))})))
      (when-let [row (handle tree path)]
        (let [[parent slot] (parent-slot row)
              spec (slot-spec (cat/node-at tree parent) slot)
              h    (holds spec)
              i    (peek row)]
          (when (and spec (not= :single h))
            {:parent parent :slot slot :holds h
             :index (if (integer? i) (if (= :after zone) (inc i) i) 0)}))))))

(defn add-target
  [tree path]
  (or (target tree path :into)
      (target tree path :after)
      (target tree [] :into)))

(defn- fit
  [{:keys [holds]} existing node]
  (cond
    (and (= :panes holds) (not (pane? node))) (cat/pane existing node)
    (and (not= :panes holds) (pane? node))    (:body node)
    :else                                     node))

(defn put
  [tree {:keys [parent slot holds index] :as tgt} node]
  (when (and tgt (some? node))
    (let [at (conj (vec parent) slot)]
      (if (= :single holds)
        {:tree (cat/set-node tree at (fit tgt nil node))
         :path at}
        (let [xs (vec (get (cat/node-at tree parent) slot))
              i  (max 0 (min (or index (count xs)) (count xs)))
              n  (fit tgt xs node)]
          {:tree (cat/set-node tree at (into (into (subvec xs 0 i) [n]) (subvec xs i)))
           :path (cond-> (conj at i) (= :panes holds) (conj :body))})))))

(defn- prefix? [a b]
  (= (vec a) (vec (take (count a) b))))

(defn movable?
  [tree src]
  (boolean (seq (handle tree src))))

(defn insert
  [tree path zone node]
  (put tree (target tree path zone) node))

(defn plan
  [tree src-row tgt-path zone]
  (let [src (vec (handle tree src-row))
        tgt (vec tgt-path)]
    (when (and (seq src) (not (prefix? src tgt)))
      (when-let [{:keys [parent slot index] :as t} (target tree tgt zone)]
        (let [si (peek src)
              sp (vec (butlast src))
              n  (count sp)
              q  (vec parent)]
          (if-not (integer? si)
            t
            (cond
              (= (conj q slot) sp)
              (assoc t :index (if (> index si) (dec index) index))

              (and (> (count q) n) (= (subvec q 0 n) sp)
                   (integer? (nth q n)) (> (nth q n) si))
              (assoc t :parent (assoc q n (dec (nth q n))))

              :else t)))))))

(defn move
  [tree src-row tgt-path zone]
  (when-let [t (plan tree src-row tgt-path zone)]
    (let [src  (vec (handle tree src-row))
          node (cat/node-at tree src)]
      (put (cat/remove-at tree src) t node))))
