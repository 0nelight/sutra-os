(ns sutra.authoring.placements
  (:require [clojure.edn :as edn]
            [sutra.foundation.ops :as op]
            [sutra.parser.checkout :as co]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def ^:private parts [:placement :fields :attributes])

(defn- where [projection]
  {:branch ctl/trunk :projection (keyword (name projection))})

(defn- node-of [part el] (if (= :placement part) el (first el)))

(defn- rows
  [projection memory]
  (for [part parts
        [el path] (get memory part)]
    (assoc (where projection)
           :element (pr-str [part el]) :node (pr-str (node-of part el)) :path path)))

(defn- elements
  [rs]
  (into {} (map (fn [{:keys [element path]}] [(edn/read-string element) path])) rs))

(defn- as-memory [m els]
  (reduce (fn [m [[part el] path]] (assoc-in m [part el] path)) m els))

(defn- files-row
  [store projection]
  (ctl/get store :projection-placements (where projection)))

(defn- migrate-row!
  [store projection]
  (let [old (select-keys (:memory (files-row store projection)) parts)
        rs  (vec (rows projection old))]
    (when (seq old)
      (ctl/commit! store [[:upsert :projection-elements rs]
                          [:swap :projection-placements (where projection)
                           (fn [r] (update r :memory #(apply dissoc % parts)))]]))
    (count rs)))

(defn migrate!
  [store]
  (into {}
        (for [{:keys [projection memory]} (ctl/list store :projection-placements
                                                    {:branch ctl/trunk})
              :when (some #(seq (get memory %)) parts)
              :let [n (migrate-row! store projection)]]
          [projection n])))

(defn memory
  [store projection]
  (as-memory (select-keys (:memory (files-row store projection)) [:files])
             (elements (ctl/list store :projection-elements (where projection)))))

(defn source
  [store projection]
  (let [w (where projection)]
    (co/source (get-in (files-row store projection) [:memory :files])
               (fn [node]
                 (elements (ctl/list store :projection-elements (assoc w :node (pr-str node)))))
               (fn [path]
                 (set (keys (elements (ctl/list store :projection-elements
                                                (assoc w :path path)))))))))

(op/defop :authoring/placements [{:keys [store]} {:keys [projection]}]
  (memory store projection))

(defn- write!
  [store projection m files-of]
  (let [w (where projection)]
    (ctl/commit! store
                 (cond-> [[:swap :projection-placements w
                           (fn [r] (assoc (or r w) :memory
                                          (files-of (select-keys (:memory r) [:files]))))]]
                   (seq (rows projection m))
                   (conj [:upsert :projection-elements (vec (rows projection m))])))))

(defn seed!
  [store projection found]
  (let [all #(elements (ctl/list store :projection-elements (where projection)))]
    (if (try (files-row store projection) false
             (catch InterruptedException e (throw e))
             (catch Exception e
               (println (str "[placements] overwriting a row that does not read: "
                             (.getMessage e)))
               true))
      (do (ctl/insert-many! store :projection-placements
                            [(assoc (where projection) :memory (select-keys found [:files]))])
          (write! store projection found identity)
          (as-memory (select-keys found [:files]) (all)))
      (let [have (all)
            new  (as-memory {} (remove (fn [[k]] (contains? have k))
                                       (for [part parts [el path] (get found part)]
                                         [[part el] path])))]
        (write! store projection new #(co/memory-merge (select-keys found [:files]) %))
        (memory store projection)))))

(defn remember!
  [store projection said]
  (write! store projection said #(co/memory-merge % (select-keys said [:files]))))
