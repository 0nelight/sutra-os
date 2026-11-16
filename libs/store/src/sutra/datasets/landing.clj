(ns sutra.datasets.landing)

(set! *warn-on-reflection* true)

(def capabilities #{:merge :diff})

(defn offer
  [{:keys [landing]}]
  {:merge (contains? landing :merge) :diff (contains? landing :diff)})

(defn residual
  [offer]
  (filterv #(not (get offer %)) [:merge :diff]))

(defn reads-before?
  [offer {:keys [whole-row?]}]
  (or (not (:diff offer))
      (and (not (:merge offer)) (not whole-row?))))

(defn- by-key
  [pk rows]
  (into {} (map (fn [row] [(str (get row pk)) row])) rows))

(defn- named [row] (into {} (remove (comp nil? val)) row))

(defn pushed
  [{:keys [kind pk rows whole-row?] :as tx} offer before exists?]
  (let [pk     (keyword pk)
        old    (by-key pk before)
        given  (by-key pk (map #(update-keys % (comp keyword name)) rows))
        merged (if whole-row?
                 given
                 (into {} (map (fn [[id row]] [id (merge (get old id) row)])) given))
        after  (if (= :snapshot kind) merged (into old merged))]
    (if (and (not (:diff offer)) exists? (= (update-vals old named) (update-vals after named)))
      {:rows (count given)}
      {:tx (if (:merge offer) tx (assoc tx :rows (vec (vals merged))))})))

(defn sweeps-inline? [replace? groups] (and replace? (= 1 (count groups))))

(defn plan
  [{:keys [fork? replace? groups]}]
  (let [inline? (sweeps-inline? replace? groups)]
    (-> (cond-> []
          fork?    (conj [:fork])
          replace? (conj [:retract-inherited]))
        (conj [:ensure-columns])
        (into (map (fn [[cols rows]] [:merge cols rows {:full-refresh? inline?}])) groups)
        (cond-> (and replace? (not inline?)) (conj [:retain])))))
