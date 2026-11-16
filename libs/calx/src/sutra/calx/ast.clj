(ns sutra.calx.ast
  (:refer-clojure :exclude [volatile?])
  (:require [sutra.calx.address :as addr]
            [sutra.calx.value :as v]))

(set! *warn-on-reflection* true)

(defn- map-children [f node]
  (case (:kind node)
    (:call :op) (update node :args #(mapv f %))
    :invoke (-> node (update :fn f) (update :args #(mapv f %)))
    :array (update node :rows (fn [rows] (mapv #(mapv f %) rows)))
    :paren (update node :expr f)
    :spill (update node :ref f)
    node))

(defn postwalk
  [f node]
  (f (map-children #(postwalk f %) node)))

(defn nodes
  [node]
  (tree-seq map?
            (fn [n] (case (:kind n)
                      (:call :op) (:args n)
                      :invoke (cons (:fn n) (:args n))
                      :array (apply concat (:rows n))
                      :paren [(:expr n)]
                      :spill [(:ref n)]
                      nil))
            node))

(defn refs [ast] (filter #(= :ref (:kind %)) (nodes ast)))
(defn names [ast] (keep #(when (= :name (:kind %)) %) (nodes ast)))
(defn calls [ast] (keep #(when (= :call (:kind %)) (:fn %)) (nodes ast)))

(def ^:private ref-error {:kind :lit :value v/ERR-REF})

(defn ref-error? [node] (and (= :lit (:kind node)) (= v/ERR-REF (:value node))))

(defn bind-sheets
  [ast lookup]
  (postwalk (fn [n]
              (if-let [sn (and (#{:ref :name} (:kind n)) (:sheet-name n))]
                (if-let [id (lookup sn)]
                  (-> n (dissoc :sheet-name) (assoc :sheet id))
                  n)
                n))
            ast))

(defn remove-sheet
  [ast sheet-id]
  (postwalk (fn [n]
              (if (and (#{:ref :name} (:kind n)) (= sheet-id (:sheet n)))
                (if (= :ref (:kind n)) ref-error (-> n (dissoc :sheet) (assoc :sheet-name "#REF")))
                n))
            ast))

(defn- normalize-ref [{:keys [r1 r2 c1 c2 abs] :as n}]
  (let [[ar1 ac1 ar2 ac2] abs
        [r1 ar1 r2 ar2] (if (and r1 r2 (> r1 r2)) [r2 ar2 r1 ar1] [r1 ar1 r2 ar2])
        [c1 ac1 c2 ac2] (if (and c1 c2 (> c1 c2)) [c2 ac2 c1 ac1] [c1 ac1 c2 ac2])]
    (assoc n :r1 r1 :r2 r2 :c1 c1 :c2 c2 :abs [ar1 ac1 ar2 ac2])))

(defn- in-bounds? [{:keys [r1 r2 c1 c2]}]
  (and (or (nil? r1) (and (<= 0 r1) (< r2 addr/max-rows)))
       (or (nil? c1) (and (<= 0 c1) (< c2 addr/max-cols)))))

(defn offset
  [ast dr dc]
  (postwalk (fn [n]
              (if (= :ref (:kind n))
                (let [[ar1 ac1 ar2 ac2] (:abs n)
                      mv (fn [x a d] (if (or (nil? x) a) x (+ x d)))
                      n' (normalize-ref (assoc n
                                               :r1 (mv (:r1 n) ar1 dr) :r2 (mv (:r2 n) ar2 dr)
                                               :c1 (mv (:c1 n) ac1 dc) :c2 (mv (:c2 n) ac2 dc)))]
                  (if (in-bounds? n') n' ref-error))
                n))
            ast))

(defn- axis-keys [axis] (if (= axis :row) [:r1 :r2 :cols] [:c1 :c2 :rows]))

(defn- targets? [n host sheet axis]
  (let [[_ _ immune-shape] (axis-keys axis)]
    (and (= :ref (:kind n))
         (= sheet (or (:sheet n) host))
         (not= immune-shape (:shape n)))))

(defn insert
  [ast host sheet axis at cnt]
  (let [[k1 k2] (axis-keys axis)
        shift (fn [x] (if (>= x at) (+ x cnt) x))]
    (postwalk (fn [n]
                (if (targets? n host sheet axis)
                  (let [n' (-> n (update k1 shift) (update k2 shift))]
                    (if (in-bounds? n') n' ref-error))
                  n))
              ast)))

(defn delete
  [ast host sheet axis at cnt]
  (let [[k1 k2] (axis-keys axis)
        end (+ at cnt)]
    (postwalk (fn [n]
                (if (targets? n host sheet axis)
                  (let [a (k1 n) b (k2 n)
                        a' (cond (< a at) a (>= a end) (- a cnt) :else at)
                        b' (cond (< b at) b (>= b end) (- b cnt) :else (dec at))]
                    (if (> a' b')
                      ref-error
                      (assoc n k1 a' k2 b')))
                  n))
              ast)))

(defn move
  [ast host sheet area to-sheet dr dc]
  (let [{ar1 :r1 ac1 :c1 ar2 :r2 ac2 :c2} area]
    (postwalk (fn [n]
                (if (and (= :ref (:kind n))
                         (= sheet (or (:sheet n) host))
                         (#{:cell :area} (:shape n))
                         (<= ar1 (:r1 n)) (<= (:r2 n) ar2)
                         (<= ac1 (:c1 n)) (<= (:c2 n) ac2))
                  (let [n' (-> n
                               (update :r1 + dr) (update :r2 + dr)
                               (update :c1 + dc) (update :c2 + dc)
                               (cond-> (not= to-sheet host) (assoc :sheet to-sheet)
                                       (and (= to-sheet host) (nil? (:sheet n))) (dissoc :sheet)))]
                    (if (in-bounds? n') n' ref-error))
                  n))
              ast)))

(defn requalify
  [ast from-sheet]
  (postwalk (fn [n]
              (if (and (= :ref (:kind n)) (nil? (:sheet n)) (nil? (:sheet-name n)))
                (assoc n :sheet from-sheet)
                n))
            ast))

(def volatile-fns
  #{"RAND" "RANDBETWEEN" "RANDARRAY" "NOW" "TODAY" "OFFSET" "INDIRECT" "INFO" "CELL"})

(defn volatile? [ast] (boolean (some volatile-fns (calls ast))))

(defn ref->area
  [n host]
  {:sheet (or (:sheet n) host)
   :r1 (or (:r1 n) 0) :r2 (or (:r2 n) (dec addr/max-rows))
   :c1 (or (:c1 n) 0) :c2 (or (:c2 n) (dec addr/max-cols))})
