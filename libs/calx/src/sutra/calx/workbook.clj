(ns sutra.calx.workbook
  (:require [clojure.string :as str]
            [sutra.calx.value :as v]
            [sutra.calx.address :as addr]
            [sutra.calx.parser :as parser]
            [sutra.calx.ast :as ast]
            [sutra.calx.graph :as graph]
            [sutra.calx.engine :as engine]
            [sutra.calx.eval :as ev]))

(set! *warn-on-reflection* true)

(def default-config {:parse-dates? true :now nil :seed nil :history-limit 100})

(defn- new-sheet [id name] {:id id :name name :cells {} :extent [0 0] :rows {} :cols {}})

(defn new-workbook
  ([] (new-workbook {}))
  ([{:keys [sheet-name config functions] :or {sheet-name "Sheet1"}}]
   {:sheets {1 (new-sheet 1 sheet-name)}
    :order [1]
    :next-id 2
    :names {}
    :functions (or functions {})
    :config (merge default-config config)
    :graph graph/empty-graph
    :spills {} :owner {} :blocked {}
    :history {:undo () :redo ()}
    :log []}))

(defn sheet-id
  [wb s]
  (cond
    (and (integer? s) (get-in wb [:sheets s])) s
    (string? s) (some (fn [[id sh]] (when (= (str/upper-case (:name sh)) (str/upper-case s)) id))
                      (:sheets wb))
    :else nil))

(defn sheet-name [wb id] (get-in wb [:sheets id :name]))

(defn sheet-names [wb] (mapv #(sheet-name wb %) (:order wb)))

(defn- err! [msg data] (throw (ex-info msg (assoc data :calx/error true))))

(defn- require-sheet [wb s]
  (or (sheet-id wb s) (err! (str "No such sheet: " s) {:sheet s})))

(defn resolve-key
  ([wb a] (resolve-key wb a nil))
  ([wb a default-sheet]
   (if (vector? a)
     (let [[s r c] a] [(require-sheet wb s) r c])
     (let [[sn local] (addr/split-sheet a)
           s (require-sheet wb (or sn default-sheet (first (:order wb))))
           [r c] (or (addr/parse-cell local) (err! (str "Invalid cell address: " a) {:address a}))]
       [s r c]))))

(defn resolve-area
  ([wb a] (resolve-area wb a nil))
  ([wb a default-sheet]
   (if (vector? a)
     [(require-sheet wb (first a)) (second a)]
     (let [[sn local] (addr/split-sheet a)
           s (require-sheet wb (or sn default-sheet (first (:order wb))))]
       [s (or (addr/parse-area local) (err! (str "Invalid area: " a) {:address a}))]))))

(defn key->address
  [wb [s r c]]
  (str (addr/quote-sheet-name (sheet-name wb s)) "!" (addr/cell->a1 r c)))

(defn cell [wb key] (engine/cell-at wb key))

(defn value
  [wb [s r c :as key]]
  (if-let [cl (engine/cell-at wb key)]
    (let [x (:value cl)] (if (v/array? x) (ffirst x) x))
    (when-let [[_ ar ac :as a] (get (:owner wb) key)]
      (get-in (:value (engine/cell-at wb a)) [(- r ar) (- c ac)]))))

(defn formula
  [wb key]
  (let [cl (engine/cell-at wb key)]
    (cond
      (:ast cl) (str "=" (parser/unparse (:ast cl) {:sheet-name #(sheet-name wb %)}))
      (:invalid cl) (:invalid cl)
      :else nil)))

(defn input
  [wb key]
  (or (formula wb key) (:value (engine/cell-at wb key))))

(defn spill-area
  [wb key]
  (get (:spills wb) key))

(defn- name-ast-fn [wb] (fn [n s] (engine/name-ast wb n s)))

(defn- mark [wb ks] (update wb ::changed (fnil into #{}) ks))
(defn- mark-full [wb] (assoc wb ::full true))

(defn- grow-extent [wb [s r c]]
  (update-in wb [:sheets s :extent]
             (fn [[er ec]] [(max (or er 0) (inc r)) (max (or ec 0) (inc c))])))

(defn- sheet-lookup [wb] (fn [n] (sheet-id wb n)))

(defn- free-spill
  [wb k]
  (let [size (or (get (:spills wb) k) (get (:blocked wb) k))
        [s r c] k
        ks (when-let [[nr nc] (get (:spills wb) k)]
             (for [i (range nr) j (range nc) :when (or (pos? i) (pos? j))] [s (+ r i) (+ c j)]))]
    (if size
      (-> wb
          (update :spills dissoc k)
          (update :blocked dissoc k)
          (update :owner #(apply dissoc % ks))
          (mark ks))
      wb)))

(defn- put-cell
  [wb [s r c :as key] cl]
  (let [old (engine/cell-at wb key)
        style (:style old)
        cl (cond-> cl (and style (not (contains? cl :style))) (assoc :style style))
        empty? (and (nil? (:ast cl)) (nil? (:invalid cl)) (nil? (:value cl)) (empty? (:style cl)))
        wb (free-spill wb key)
        wb (if empty?
             (update-in wb [:sheets s :cells] dissoc [r c])
             (-> wb (assoc-in [:sheets s :cells [r c]] cl) (grow-extent key)))
        wb (if (:ast cl)
             (update wb :graph graph/add-formula key (:ast cl) (name-ast-fn wb))
             (update wb :graph graph/remove-formula key))]
    (mark wb [key])))

(defn- parsed-cell [wb input]
  (let [{:keys [formula value date?]}
        (v/parse-input input {:parse-dates? (get-in wb [:config :parse-dates?] true)})]
    (if formula
      (let [{:keys [ast error]} (parser/try-parse formula)]
        [(if error
           {:invalid formula :value (v/err :error)}
           {:ast (ast/bind-sheets ast (sheet-lookup wb)) :value nil})
         false])
      [{:value value} (boolean date?)])))

(defn parse-cell-input
  [wb input]
  (first (parsed-cell wb input)))

(defn- typed-date-format [serial]
  (let [day (Math/floor serial)
        secs (Math/round (* (- serial day) 86400.0))]
    (cond (and (zero? day) (pos? (mod secs 60))) "h:mm:ss"
          (zero? day) "h:mm"
          (zero? secs) "m/d/yyyy"
          :else "m/d/yyyy h:mm")))

(defn- all-formulas
  [wb]
  (for [[s sheet] (:sheets wb) [[r c] cl] (:cells sheet) :when (:ast cl)] [[s r c] (:ast cl)]))

(defn- transform-asts
  [wb f]
  (let [wb (reduce (fn [wb [[s r c] a]] (assoc-in wb [:sheets s :cells [r c] :ast] (f a s))) wb
                   (all-formulas wb))]
    (update wb :names (fn [names]
                        (into {} (map (fn [[k n]] [k (update n :ast f (:scope n))])) names)))))

(defn rebuild-graph [wb]
  (let [naf (name-ast-fn wb)]
    (assoc wb
           :graph (reduce (fn [g [k a]] (graph/add-formula g k a naf)) graph/empty-graph
                          (all-formulas wb)))))

(defn- recompute-extent [sheet]
  (assoc sheet
         :extent (reduce (fn [[er ec] [r c]] [(max er (inc r)) (max ec (inc c))]) [0 0]
                         (keys (:cells sheet)))))

(defn- rebind-all [wb]
  (transform-asts wb (fn [a _] (ast/bind-sheets a (sheet-lookup wb)))))

(defn- unresolved-sheet-refs? [wb]
  (some (fn [[_ a]] (some :sheet-name (concat (ast/refs a) (ast/names a)))) (all-formulas wb)))

(defmulti apply-op
  (fn [_wb op] (first op)))

(defmethod apply-op :default [_ op] (err! (str "Unknown operation " (first op)) {:op op}))

(defn text-format?
  [style]
  (= "@" (:format style)))

(defmethod apply-op :set-cell [wb [_ key input]]
  (let [key (resolve-key wb key)
        style (:style (engine/cell-at wb key))
        [cl date?] (if (and (string? input) (not= "" input) (text-format? style))
                     [{:value input} false]
                     (parsed-cell wb input))
        cl (cond-> cl
             (and date? (nil? (:format style)))
             (assoc :style (assoc style :format (typed-date-format (:value cl)))))]
    (put-cell wb key cl)))

(defmethod apply-op :set-value [wb [_ key x]]
  (let [key (resolve-key wb key)]
    (put-cell wb key {:value (if (number? x) (v/normalize-number x) x)})))

(defmethod apply-op :set-style [wb [_ key style]]
  (let [[s r c :as key] (resolve-key wb key)
        old (engine/cell-at wb key)
        style' (when style (merge (:style old) style))]
    (if (and (nil? old) (empty? style'))
      wb
      (let [cl (if (empty? style') (dissoc old :style) (assoc (or old {:value nil}) :style style'))]
        (if (and (nil? (:ast cl)) (nil? (:invalid cl)) (nil? (:value cl)) (empty? (:style cl)))
          (update-in wb [:sheets s :cells] dissoc [r c])
          (assoc-in wb [:sheets s :cells [r c]] cl))))))

(defmethod apply-op :clear [wb [_ s area]]
  (let [[s {:keys [r1 c1 r2 c2]}] (resolve-area wb [s area])
        ks (for [[[r c] _] (get-in wb [:sheets s :cells]) :when (and (<= r1 r r2) (<= c1 c c2))]
             [s r c])]
    (reduce (fn [wb k] (put-cell wb k {:value nil :style nil})) wb ks)))

(defn- valid-sheet-name? [n]
  (and (string? n) (<= 1 (count n) 31) (not (re-find #"[\[\]:*?/\\]" n))))

(defmethod apply-op :add-sheet [wb [_ name]]
  (when-not (valid-sheet-name? name) (err! (str "Invalid sheet name: " name) {:name name}))
  (when (sheet-id wb name) (err! (str "Sheet already exists: " name) {:name name}))
  (let [id (:next-id wb)
        wb (-> wb
               (assoc-in [:sheets id] (new-sheet id name))
               (update :order conj id)
               (update :next-id inc))]
    (if (unresolved-sheet-refs? wb) (-> wb rebind-all mark-full) wb)))

(defmethod apply-op :remove-sheet [wb [_ s]]
  (let [id (require-sheet wb s)]
    (when (= 1 (count (:order wb))) (err! "Cannot remove the last sheet" {:sheet s}))
    (-> wb
        (update :sheets dissoc id)
        (update :order (fn [o] (vec (remove #{id} o))))
        (update :names (fn [ns] (into {} (remove (fn [[[scope _] _]] (= scope id))) ns)))
        (transform-asts (fn [a _] (ast/remove-sheet a id)))
        mark-full)))

(defmethod apply-op :rename-sheet [wb [_ s new-name]]
  (let [id (require-sheet wb s)]
    (when-not (valid-sheet-name? new-name)
      (err! (str "Invalid sheet name: " new-name) {:name new-name}))
    (when-let [other (sheet-id wb new-name)]
      (when (not= other id) (err! (str "Sheet already exists: " new-name) {:name new-name})))
    (let [wb (assoc-in wb [:sheets id :name] new-name)]
      (-> (if (unresolved-sheet-refs? wb) (rebind-all wb) wb) mark-full))))

(defmethod apply-op :move-sheet [wb [_ s index]]
  (let [id (require-sheet wb s)
        others (vec (remove #{id} (:order wb)))
        i (max 0 (min index (count others)))]
    (-> wb
        (assoc :order (vec (concat (subvec others 0 i) [id] (subvec others i))))
        mark-full)))

(defn- shift-map-keys
  [m get-i set-i f]
  (into {} (keep (fn [[k x]] (when-let [i' (f (get-i k))] [(set-i k i') x]))) m))

(defn- structural [wb s axis kind at n]
  (let [id (require-sheet wb s)
        _ (when (or (neg? at) (not (pos? n))) (err! "Invalid structural edit" {:at at :n n}))
        coord (if (= axis :row) first second)
        set-coord (if (= axis :row) (fn [[_ c] i] [i c]) (fn [[r _] i] [r i]))
        limit (if (= axis :row) addr/max-rows addr/max-cols)
        f (case kind
            :insert (fn [i] (let [i' (if (>= i at) (+ i n) i)] (when (< i' limit) i')))
            :delete (fn [i] (cond (< i at) i (>= i (+ at n)) (- i n) :else nil)))
        meta-key (if (= axis :row) :rows :cols)
        xform (case kind
                :insert (fn [a host] (ast/insert a host id axis at n))
                :delete (fn [a host] (ast/delete a host id axis at n)))]
    (-> wb
        (transform-asts xform)
        (update-in [:sheets id :cells] shift-map-keys coord set-coord f)
        (update-in [:sheets id meta-key] shift-map-keys identity (fn [_ i] i) f)
        (update-in [:sheets id] recompute-extent)
        mark-full)))

(defmethod apply-op :insert-rows [wb [_ s at n]] (structural wb s :row :insert at (or n 1)))
(defmethod apply-op :delete-rows [wb [_ s at n]] (structural wb s :row :delete at (or n 1)))
(defmethod apply-op :insert-cols [wb [_ s at n]] (structural wb s :col :insert at (or n 1)))
(defmethod apply-op :delete-cols [wb [_ s at n]] (structural wb s :col :delete at (or n 1)))

(defmethod apply-op :set-row-meta [wb [_ s row m]]
  (assoc-in wb [:sheets (require-sheet wb s) :rows row] m))

(defmethod apply-op :set-col-meta [wb [_ s col m]]
  (assoc-in wb [:sheets (require-sheet wb s) :cols col] m))

(defn- area-cells [wb s {:keys [r1 c1 r2 c2]}]
  (into {} (filter (fn [[[r c] _]] (and (<= r1 r r2) (<= c1 c c2))))
        (get-in wb [:sheets s :cells])))

(defmethod apply-op :move [wb [_ s area to-s to-r to-c]]
  (let [from (require-sheet wb s)
        to (require-sheet wb to-s)
        {:keys [r1 c1 r2 c2]} area
        dr (- to-r r1) dc (- to-c c1)
        _ (when (or (neg? to-r) (neg? to-c) (>= (+ r2 dr) addr/max-rows)
                    (>= (+ c2 dc) addr/max-cols))
            (err! "Move target out of bounds" {:to [to-r to-c]}))
        wb (transform-asts wb (fn [a host] (ast/move a host from area to dr dc)))
        moving (area-cells wb from area)
        wb (update-in wb [:sheets from :cells] #(apply dissoc % (keys moving)))
        target {:r1 to-r :c1 to-c :r2 (+ r2 dr) :c2 (+ c2 dc)}
        wb (update-in wb [:sheets to :cells] #(apply dissoc % (keys (area-cells wb to target))))
        wb (reduce (fn [wb [[r c] cl]]
                     (let [cl (if (and (:ast cl) (not= from to)) (update cl :ast ast/requalify from)
                                cl)]
                       (assoc-in wb [:sheets to :cells [(+ r dr) (+ c dc)]] cl)))
                   wb moving)]
    (-> wb
        (update-in [:sheets from] recompute-extent)
        (update-in [:sheets to] recompute-extent)
        mark-full)))

(defn- offset-cell [cl dr dc]
  (if (:ast cl) (update cl :ast ast/offset dr dc) cl))

(defmethod apply-op :copy [wb [_ s area to-s to-r to-c]]
  (let [from (require-sheet wb s)
        to (require-sheet wb to-s)
        {:keys [r1 c1 r2 c2]} area
        dr (- to-r r1) dc (- to-c c1)
        src (area-cells wb from area)]
    (reduce (fn [wb [r c]]
              (let [tr (+ r dr) tc (+ c dc)]
                (if (and (< -1 tr addr/max-rows) (< -1 tc addr/max-cols))
                  (put-cell wb [to tr tc]
                            (if-let [cl (get src [r c])]
                              (assoc (offset-cell cl dr dc) :value (when-not (:ast cl) (:value cl)))
                              {:value nil :style nil}))
                  wb)))
            wb
            (for [r (range r1 (inc r2)) c (range c1 (inc c2))] [r c]))))

(defn- trailing-number [s]
  (when (string? s) (when-let [[_ pre n] (re-matches #"(?s)(.*?)(\d+)" s)] [pre (parse-long n)])))

(defn- fill-series
  [cells i]
  (let [k (count cells)
        vals (map :value cells)
        literal? (every? #(and % (nil? (:ast %)) (nil? (:invalid %))) cells)]
    (cond
      (and literal? (every? number? vals) (>= k 2))
      (let [xs (vec vals)
            n (double k) mx (/ (dec n) 2.0) my (/ (reduce + xs) n)
            sxy (reduce + (map-indexed (fn [j y] (* (- j mx) (- y my))) xs))
            sxx (reduce + (map (fn [j] (let [d (- j mx)] (* d d))) (range k)))
            slope (if (zero? sxx) 0.0 (/ sxy sxx))]
        {:cell {:value (v/normalize-number (+ my (* slope (- (+ k i) mx))))} :src nil})

      (and literal? (= k 1) (trailing-number (first vals)))
      (let [[pre n] (trailing-number (first vals))]
        {:cell {:value (str pre (+ n (inc i)))} :src nil})

      :else {:cell nil :src (mod i k)})))

(defmethod apply-op :fill [wb [_ s source target]]
  (let [id (require-sheet wb s)
        {sr1 :r1 sc1 :c1 sr2 :r2 sc2 :c2} source
        {tr1 :r1 tc1 :c1 tr2 :r2 tc2 :c2} target
        down? (and (= sc1 tc1) (= sc2 tc2) (> tr2 sr2))
        right? (and (= sr1 tr1) (= sr2 tr2) (> tc2 sc2))
        cells (get-in wb [:sheets id :cells])]
    (when-not (or down? right?)
      (err! "Fill target must extend the source down or right" {:source source :target target}))
    (reduce
     (fn [wb [line positions src-positions]]
       (let [src (mapv #(get cells %) src-positions)]
         (reduce (fn [wb [i [r c]]]
                   (let [{:keys [cell src]} (fill-series src i)
                         cl (or cell
                                (let [[sr sc] (nth src-positions src)
                                      orig (get cells [sr sc])]
                                  (when orig
                                    (assoc (offset-cell orig (- r sr) (- c sc))
                                           :value (when-not (:ast orig) (:value orig))))))]
                     (put-cell wb [id r c] (or cl {:value nil}))))
                 wb (map-indexed vector positions))))
     wb
     (if down?
       (for [c (range sc1 (inc sc2))]
         [c (for [r (range (inc sr2) (inc tr2))] [r c])
          (vec (for [r (range sr1 (inc sr2))] [r c]))])
       (for [r (range sr1 (inc sr2))]
         [r (for [c (range (inc sc2) (inc tc2))] [r c])
          (vec (for [c (range sc1 (inc sc2))] [r c]))])))))

(defmethod apply-op :sort [wb [_ s area sort-keys]]
  (let [id (require-sheet wb s)
        {:keys [r1 c1 r2 c2]} area
        sort-keys (or (seq sort-keys) [{:col c1 :ascending? true}])
        row-val (fn [r col] (value wb [id r col]))
        cmp (fn [ra rb]
              (or (some (fn [{:keys [col ascending?] :or {ascending? true}}]
                          (let [a (row-val ra col) b (row-val rb col)
                                c (cond (and (nil? a) (nil? b)) 0
                                        (nil? a) 1
                                        (nil? b) -1
                                        :else (cond-> (v/compare-values a b) (not ascending?) -))]
                            (when-not (zero? c) c)))
                        sort-keys)
                  0))
        rows (vec (range r1 (inc r2)))
        sorted (vec (sort cmp rows))
        cells (get-in wb [:sheets id :cells])
        snapshot (into {} (for [r rows c (range c1 (inc c2))] [[r c] (get cells [r c])]))]
    (reduce (fn [wb [target-r src-r]]
              (reduce (fn [wb c]
                        (let [cl (get snapshot [src-r c])]
                          (put-cell wb [id target-r c]
                                    (if cl (offset-cell cl (- target-r src-r) 0) {:value nil}))))
                      wb (range c1 (inc c2))))
            wb (map vector rows sorted))))

(defn valid-name? [n]
  (and (string? n)
       (re-matches #"[A-Za-z_\\À-￿][\w.À-￿]*" n)
       (not (re-matches #"(?i)[A-Z]{1,3}\d+" n))
       (not (re-matches #"(?i)R\d*C\d*" n))
       (not (#{"TRUE" "FALSE"} (str/upper-case n)))))

(defmethod apply-op :define-name [wb [_ name input scope]]
  (when-not (valid-name? name) (err! (str "Invalid name: " name) {:name name}))
  (let [scope (when scope (require-sheet wb scope))
        qualify-sheet (or scope (first (:order wb)))
        cl (parse-cell-input wb input)
        a (cond (:ast cl) (ast/requalify (:ast cl) qualify-sheet)
                (:invalid cl) (err! (str "Invalid formula for name " name ": " input) {:name name})
                :else {:kind :lit :value (:value cl)})]
    (-> wb
        (assoc-in [:names [scope (str/upper-case name)]] {:name name :scope scope :ast a})
        mark-full)))

(defmethod apply-op :remove-name [wb [_ name scope]]
  (let [scope (when scope (require-sheet wb scope))]
    (-> wb (update :names dissoc [scope (str/upper-case name)]) mark-full)))

(defn- strip-history [wb] (dissoc wb :history :log))

(defn- finish
  [wb]
  (let [changed (::changed wb #{})
        full? (::full wb)
        wb (dissoc wb ::changed ::full)]
    (if full?
      (let [wb' (-> wb rebuild-graph engine/recalculate-all)]
        (vary-meta wb' update ::engine/changes into changed))
      (let [wb' (engine/recalculate wb changed)]
        (vary-meta wb' update ::engine/changes (fnil into #{}) changed)))))

(defn transact
  [wb ops]
  (let [ops (vec ops)
        before (strip-history wb)
        wb' (finish (reduce apply-op wb ops))
        limit (get-in wb [:config :history-limit] 100)]
    (-> wb'
        (assoc :history {:undo (take limit (cons before (get-in wb [:history :undo]))) :redo ()})
        (assoc :log (conj (:log wb []) ops)))))

(defn changes
  [wb]
  (let [c (::engine/changes (meta wb) #{})] (if (delay? c) @c c)))

(defn can-undo? [wb] (boolean (seq (get-in wb [:history :undo]))))
(defn can-redo? [wb] (boolean (seq (get-in wb [:history :redo]))))

(defn- restore [wb target from-stack to-stack entry]
  (let [{:keys [undo redo] :as h} (:history wb)
        h' (-> h
               (update from-stack rest)
               (update to-stack #(cons (strip-history wb) %)))]
    (-> target
        (assoc :history h' :log (conj (:log wb) entry))
        (with-meta {::engine/changes
                      (delay (set (concat (engine/formula-keys target)
                                          (engine/formula-keys wb))))}))))

(defn undo [wb]
  (if-let [prev (first (get-in wb [:history :undo]))]
    (restore wb prev :undo :redo :undo)
    wb))

(defn redo [wb]
  (if-let [nxt (first (get-in wb [:history :redo]))]
    (restore wb nxt :redo :undo :redo)
    wb))

(defn replay
  [wb log]
  (reduce (fn [wb entry]
            (case entry
              :undo (undo wb)
              :redo (redo wb)
              (transact wb entry)))
          wb log))

(defn register-function
  [wb name spec]
  (let [spec (if (fn? spec) {:arity [0 nil] :impl spec} spec)]
    (-> wb
        (assoc-in [:functions (str/upper-case name)] spec)
        rebuild-graph
        engine/recalculate-all)))

(defn recalculate
  [wb]
  (engine/recalculate-all wb))

(defn evaluate-formula
  [wb formula s]
  (let [id (require-sheet wb s)
        {:keys [ast error]} (parser/try-parse formula)]
    (if error
      (v/err :error)
      (let [value-at (fn [sh r c] (value wb [sh r c]))
            spill-at (fn [sh r c] (get (:spills wb) [sh r c]))
            ctx (assoc (engine/base-ctx wb value-at spill-at) :sheet id :row 0 :col 0)]
        (ev/evaluate-value ctx (ast/bind-sheets ast (sheet-lookup wb)))))))
