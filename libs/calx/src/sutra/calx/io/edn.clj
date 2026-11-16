(ns sutra.calx.io.edn
  (:require [clojure.edn :as edn]
            [clojure.pprint]
            [clojure.walk]
            [clojure.string :as str]
            [sutra.calx.value :as v]
            [sutra.calx.address :as addr]
            [sutra.calx.workbook :as wb]
            [sutra.calx.parser :as parser]))

(set! *warn-on-reflection* true)

(def readers {'calx/error (fn [t] (v/err t))})

(defn- cell->doc [w [s r c] cl]
  (cond
    (:ast cl) {:formula (wb/formula w [s r c])}
    (:invalid cl) {:formula (:invalid cl)}
    :else (:value cl)))

(def ^:private by-position
  (comparator (fn [a b] (neg? (compare (addr/parse-cell a) (addr/parse-cell b))))))

(defn workbook->doc
  [w]
  {:calx/format 1
   :sheets (vec (for [id (:order w)
                      :let [{:keys [name cells rows cols]} (get-in w [:sheets id])
                            sorted (sort-by (fn [[[r c]]] [r c]) cells)]]
                  (cond-> {:name name
                           :cells (into (sorted-map-by by-position)
                                        (keep (fn [[[r c] cl]]
                                                (let [x (cell->doc w [id r c] cl)]
                                                  (when (some? x) [(addr/cell->a1 r c) x]))))
                                        sorted)}
                    (some (comp :style val) cells)
                    (assoc :styles
                           (into (sorted-map-by by-position)
                                 (keep (fn [[[r c] cl]]
                                         (when (:style cl) [(addr/cell->a1 r c) (:style cl)])))
                                 sorted))
                    (seq rows) (assoc :rows rows)
                    (seq cols) (assoc :cols cols))))
   :names (vec (for [[_ {:keys [name scope ast]}] (sort-by (comp str key) (:names w))]
                 {:name name
                  :scope (when scope (wb/sheet-name w scope))
                  :formula (str "=" (parser/unparse ast {:sheet-name #(wb/sheet-name w %)}))}))
   :config (select-keys (:config w) [:parse-dates? :history-limit])})

(defn doc->workbook
  ([doc] (doc->workbook doc {}))
  ([{:keys [sheets names config]} {:keys [functions] :as opts}]
   (let [[first-sheet & more] sheets
         w (wb/new-workbook {:sheet-name (:name first-sheet "Sheet1")
                             :config (merge config (:config opts))
                             :functions functions})
         ops (concat
              (map (fn [s] [:add-sheet (:name s)]) more)
              (for [{:keys [name cells styles rows cols]} sheets
                    op (concat
                        (for [[a x] cells
                              :let [[r c] (addr/parse-cell a)]]
                          (if (and (map? x) (contains? x :formula))
                            [:set-cell [name r c] (:formula x)]
                            [:set-value [name r c] x]))
                        (for [[a st] styles :let [[r c] (addr/parse-cell a)]]
                          [:set-style [name r c] st])
                        (for [[r m] rows] [:set-row-meta name r m])
                        (for [[c m] cols] [:set-col-meta name c m]))]
                op)
              (for [{:keys [name scope formula]} names]
                [:define-name name formula scope]))]
     (-> (wb/transact w ops)
         (assoc :history {:undo () :redo ()} :log [])))))

(defn write-string
  [w]
  (binding [*print-namespace-maps* false]
    (with-out-str
      (clojure.pprint/pprint
       (clojure.walk/prewalk #(if (v/error? %) (tagged-literal 'calx/error (:type %)) %)
                             (workbook->doc w))))))

(defn read-string*
  ([s] (read-string* s {}))
  ([s opts] (doc->workbook (edn/read-string {:readers readers} s) opts)))

(defn save [w path] (spit path (write-string w)))
(defn load-file* ([path] (load-file* path {})) ([path opts] (read-string* (slurp path) opts)))
