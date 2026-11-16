(ns sutra.calx.core
  (:require [clojure.string :as str]
            [sutra.calx.value :as v]
            [sutra.calx.address :as addr]
            [sutra.calx.workbook :as wb]
            [sutra.calx.format :as fmt]
            [sutra.calx.functions :as functions]
            [sutra.calx.graph]
            [sutra.calx.i18n :as i18n]
            [sutra.calx.parser]))

(set! *warn-on-reflection* true)

(defn workbook
  ([] (wb/new-workbook))
  ([sheets] (workbook sheets {}))
  ([sheets {:keys [config names functions]}]
   (let [pairs (seq sheets)
         [first-name] (first pairs)
         w (wb/new-workbook {:sheet-name (or first-name "Sheet1") :config config
                             :functions functions})
         ops (concat
              (map (fn [[n _]] [:add-sheet n]) (rest pairs))
              (for [[n rows] pairs
                    [r row] (map-indexed vector rows)
                    [c x] (map-indexed vector row)
                    :when (some? x)]
                [:set-cell [n r c] x])
              (map (fn [[n input]] [:define-name n input nil]) names))]
     (-> (wb/transact w ops)
         (assoc :history {:undo () :redo ()} :log [])))))

(defn key-of
  ([w a] (wb/resolve-key w a))
  ([w a sheet] (wb/resolve-key w a sheet)))

(defn value
  [w a]
  (wb/value w (wb/resolve-key w a)))

(defn locale [w] (i18n/locale (get-in w [:config :locale])))

(defn- canonical [w x]
  (i18n/canonical-input x (locale w) {:parse-dates? (get-in w [:config :parse-dates?] true)}))

(defn- canonical-at
  [w k x]
  (if (and (string? x) (wb/text-format? (:style (wb/cell w k)))) x (canonical w x)))

(defn formula
  [w a]
  (some-> (wb/formula w (wb/resolve-key w a)) (i18n/localize (locale w))))

(defn input [w a]
  (let [k (wb/resolve-key w a)]
    (or (formula w a) (i18n/cell-input (wb/input w k) (:style (wb/cell w k)) (locale w)))))

(defn style [w a]
  (let [s (:style (wb/cell w (wb/resolve-key w a)))]
    (cond-> s (:format s) (update :format i18n/localize-format (locale w)))))

(defn rendered [w a]
  (let [k (wb/resolve-key w a)
        x (wb/value w k)
        f (get-in (wb/cell w k) [:style :format])]
    (if (and f (not (v/error? x)))
      (fmt/render (locale w) x f)
      {:text (v/display (locale w) x)})))

(defn display
  [w a]
  (:text (rendered w a)))

(defn sheet-names [w] (wb/sheet-names w))

(defn extent
  [w sheet]
  (get-in w [:sheets (wb/sheet-id w sheet) :extent]))

(defn- grid [w sheet f]
  (let [id (or (wb/sheet-id w sheet)
               (throw (ex-info (str "No such sheet " sheet) {:calx/error true})))
        [nr nc] (get-in w [:sheets id :extent])]
    (mapv (fn [r] (mapv (fn [c] (f [id r c])) (range nc))) (range nr))))

(defn sheet-values
  [w sheet]
  (grid w sheet #(wb/value w %)))

(defn sheet-inputs
  [w sheet]
  (grid w sheet #(wb/input w %)))

(defn range-values
  [w a]
  (let [[s {:keys [r1 c1 r2 c2]}] (wb/resolve-area w a)
        [nr nc] (get-in w [:sheets s :extent])
        r2 (min r2 (max r1 (dec nr))) c2 (min c2 (max c1 (dec nc)))]
    (mapv (fn [r] (mapv (fn [c] (wb/value w [s r c])) (range c1 (inc c2)))) (range r1 (inc r2)))))

(defn calculate
  ([w formula] (calculate w formula (first (wb/sheet-names w))))
  ([w formula sheet] (wb/evaluate-formula w (canonical w formula) sheet)))

(defn spill-area
  [w a]
  (wb/spill-area w (wb/resolve-key w a)))

(defn precedents
  [w a]
  (sutra.calx.graph/precedents (:graph w) (wb/resolve-key w a)))

(defn dependents
  [w a]
  (set (map #(wb/key->address w %) (sutra.calx.graph/dependents (:graph w) (wb/resolve-key w a)))))

(defn transact
  [w ops]
  (wb/transact w ops))

(defn- area-arg [w a] (wb/resolve-area w a))

(defn set-cell
  [w a input]
  (let [k (wb/resolve-key w a)] (wb/transact w [[:set-cell k (canonical-at w k input)]])))

(defn set-cells
  [w m]
  (wb/transact w (map (fn [[a x]] (let [k (wb/resolve-key w a)]
                                      [:set-cell k (canonical-at w k x)]))
                      m)))

(defn set-range
  [w a rows]
  (let [[s r0 c0] (wb/resolve-key w a)]
    (wb/transact w (for [[i row] (map-indexed vector rows) [j x] (map-indexed vector row)]
                     [:set-cell [s (+ r0 i) (+ c0 j)]
                      (canonical-at w [s (+ r0 i) (+ c0 j)] x)]))))

(defn set-style [w a style]
  (wb/transact w [[:set-style (wb/resolve-key w a)
                   (cond-> style
                     (:format style) (update :format i18n/delocalize-format (locale w)))]]))

(defn clear [w a]
  (let [[s area] (area-arg w a)] (wb/transact w [[:clear s area]])))

(defn add-sheet [w name] (wb/transact w [[:add-sheet name]]))
(defn remove-sheet [w sheet] (wb/transact w [[:remove-sheet (wb/sheet-id w sheet)]]))
(defn rename-sheet [w sheet new-name]
  (wb/transact w [[:rename-sheet (wb/sheet-id w sheet) new-name]]))
(defn move-sheet [w sheet index] (wb/transact w [[:move-sheet (wb/sheet-id w sheet) index]]))

(defn- sheet-arg [w sheet]
  (or (wb/sheet-id w sheet) (throw (ex-info (str "No such sheet " sheet) {:calx/error true}))))

(defn insert-rows
  ([w sheet row] (insert-rows w sheet row 1))
  ([w sheet row n] (wb/transact w [[:insert-rows (sheet-arg w sheet) (dec row) n]])))

(defn delete-rows
  ([w sheet row] (delete-rows w sheet row 1))
  ([w sheet row n] (wb/transact w [[:delete-rows (sheet-arg w sheet) (dec row) n]])))

(defn- col-index [col] (if (string? col) (addr/letters->col col) (dec col)))

(defn insert-cols
  ([w sheet col] (insert-cols w sheet col 1))
  ([w sheet col n] (wb/transact w [[:insert-cols (sheet-arg w sheet) (col-index col) n]])))

(defn delete-cols
  ([w sheet col] (delete-cols w sheet col 1))
  ([w sheet col n] (wb/transact w [[:delete-cols (sheet-arg w sheet) (col-index col) n]])))

(defn- move-lines [w sheet axis start n target]
  (let [id (sheet-arg w sheet)
        end (+ start n)]
    (if (<= start target end)
      w
      (let [src (if (< target start) (+ start n) start)
            [ins mv del] (if (= axis :row)
                           [:insert-rows
                            [:move id {:r1 src :r2 (+ src n -1) :c1 0 :c2 (dec addr/max-cols)} id
                             target 0]
                            :delete-rows]
                           [:insert-cols
                            [:move id {:c1 src :c2 (+ src n -1) :r1 0 :r2 (dec addr/max-rows)} id 0
                             target]
                            :delete-cols])]
        (wb/transact w [[ins id target n] mv [del id src n]])))))

(defn move-rows
  [w sheet row n before]
  (move-lines w sheet :row (dec row) n (dec before)))

(defn move-cols
  [w sheet col n before]
  (move-lines w sheet :col (col-index col) n (col-index before)))

(defn move-range
  [w from to]
  (let [[s area] (area-arg w from) [ts tr tc] (wb/resolve-key w to s)]
    (wb/transact w [[:move s area ts tr tc]])))

(defn copy-range
  [w from to]
  (let [[s area] (area-arg w from) [ts tr tc] (wb/resolve-key w to s)]
    (wb/transact w [[:copy s area ts tr tc]])))

(defn fill
  [w source target]
  (let [[s src] (area-arg w source) [_ tgt] (area-arg w target)]
    (wb/transact w [[:fill s src tgt]])))

(defn sort-range
  ([w a] (sort-range w a nil))
  ([w a sort-keys]
   (let [[s area] (area-arg w a)]
     (wb/transact w [[:sort s area (mapv #(update % :col col-index) sort-keys)]]))))

(defn define-name
  ([w name input] (define-name w name input nil))
  ([w name input scope]
   (wb/transact w [[:define-name name (canonical w input) (when scope (sheet-arg w scope))]])))

(defn remove-name
  ([w name] (remove-name w name nil))
  ([w name scope] (wb/transact w [[:remove-name name (when scope (sheet-arg w scope))]])))

(defn names
  [w]
  (into {}
        (map (fn [[[scope _] {:keys [name ast]}]]
               [name
                {:scope (when scope (wb/sheet-name w scope))
                 :formula (i18n/localize (str "="
                                              (sutra.calx.parser/unparse
                                               ast {:sheet-name #(wb/sheet-name w %)}))
                                         (locale w))}]))
        (:names w)))

(defn register-function
  [w name spec]
  (wb/register-function w name spec))

(def undo wb/undo)
(def redo wb/redo)
(def can-undo? wb/can-undo?)
(def can-redo? wb/can-redo?)
(def recalculate wb/recalculate)
(def replay wb/replay)

(defn changes
  [w]
  (into (sorted-set) (map #(wb/key->address w %))
        (filter #(get-in w [:sheets (first %)]) (wb/changes w))))

(defn edit-log [w] (:log w))

(defn function-names
  [w]
  (sort (map #(i18n/function-name % (locale w)) (keys (merge functions/registry (:functions w))))))

(def error? v/error?)
(defn error-type [x] (when (v/error? x) (:type x)))
