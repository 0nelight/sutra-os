(ns sutra-ui.apps.doc-intel.model
  (:require [clojure.string :as str]))

(def strategy-kinds ["raw-text" "ocr" "layout-ocr" "vlm"])

(def modes ["AUTO" "ELECTRONIC" "SCAN"])

(def preprocessings ["none" "ocr" "layout" "layout-tables"])

(def criteria [:text :tables :headings :lists :reading_order :completeness])

(def plain-kinds
  #{"raw-text" "ocr"})

(defn show-raw?
  [st {:keys [id strategy]}]
  (not= (contains? plain-kinds strategy) (contains? (:raw st) id)))

(defn new-strategy [id kind model]
  (cond-> {:id id :strategy kind :mode "AUTO" :languages ""}
    (= "vlm" kind) (assoc :model model :preprocess "none" :prompt "")))

(defn initial [strategies]
  {:sources :loading :models [] :source nil :items {} :q "" :item nil :page 0
   :strategies (or (not-empty strategies) [])
   :results {} :evals {} :judge nil :raw #{} :focus nil :editing nil})

(defn- next-id [st] (inc (reduce max 0 (map :id (:strategies st)))))

(defn add-strategy [st kind]
  (let [id (next-id st)]
    (-> st
        (update :strategies conj (new-strategy id kind (first (:models st))))
        (assoc :editing id))))

(defn remove-strategy [st id]
  (-> st
      (update :strategies (fn [ss] (filterv #(not= id (:id %)) ss)))
      (update :focus #(when-not (= id %) %))))

(defn edit-strategy [st id k v]
  (update st :strategies
          (fn [ss] (mapv #(if (= id (:id %))
                            (cond-> (assoc % k v)
                              (and (= k :strategy) (= "vlm" v) (nil? (:model %)))
                              (merge (select-keys (new-strategy 0 v (first (:models st)))
                                                  [:model :preprocess :prompt])))
                            %)
                         ss))))

(defn with-default-strategies
  [st]
  (if (seq (:strategies st))
    st
    (reduce add-strategy (assoc st :editing nil) ["raw-text" "layout-ocr" "vlm"])))

(defn strategy-of [st id] (some #(when (= id (:id %)) %) (:strategies st)))

(defn wire-strategy
  [{:keys [strategy mode languages model preprocess prompt]}]
  (let [langs (vec (remove str/blank? (map str/trim (str/split (str languages) #"[,\s]+"))))]
    (cond-> {:strategy strategy}
      (not= "raw-text" strategy)    (assoc :mode mode)
      (seq langs)                   (assoc :languages langs)
      (= "vlm" strategy)            (assoc :model model :preprocess preprocess)
      (and (= "vlm" strategy)
           (not (str/blank? prompt))) (assoc :prompt prompt))))

(defn result-key [st s] [(:address (:item st)) (:page st) (wire-strategy s)])

(defn result [st s] (get-in st [:results (result-key st s)]))

(defn eval-key [st s judge] [(result-key st s) judge])

(defn evaluation [st s] (get-in st [:evals (eval-key st s (:judge st))]))

(def running {:status :running})

(defn finished [row] {:status :done :row row})

(defn failed [err] {:status :error :error err})

(defn page-count
  [st]
  (some (fn [[[addr] {:keys [row]}]] (when (= addr (:address (:item st))) (:page_count row)))
        (:results st)))

(defn turn-page [st n]
  (let [last-page (some-> (page-count st) dec)]
    (assoc st :page (cond-> (max 0 n) last-page (min last-page)))))

(defn open-item [st item] (assoc st :item item :page 0 :focus nil))

(defn mean-score
  [scores]
  (let [xs (filter number? (vals scores))]
    (when (seq xs) (/ (reduce + xs) (count xs)))))

(defn bests
  [runs]
  (let [lead (fn [f pick] (let [xs (filter (comp number? second)
                                           (map (fn [[id row ev]] [id (f row ev)]) runs))]
                            (when (> (count xs) 1) (first (apply pick second xs)))))]
    {:ms    (lead (fn [row _] (:ms row)) min-key)
     :cost  (lead (fn [row _] (:cost row)) min-key)
     :chars (lead (fn [row _] (some-> (:markdown row) count)) max-key)
     :score (lead (fn [_ ev] (mean-score (:scores ev))) max-key)}))

(defn source-key [{:keys [kind name]}] (str kind ":" name))

(defn items-of [st] (let [xs (get (:items st) (some-> (:source st) source-key))]
                      (when (sequential? xs) xs)))

(defn matching
  [items q]
  (let [ws (remove str/blank? (str/split (str/lower-case (str q)) #"\s+"))]
    (filterv (fn [{:keys [label]}] (let [l (str/lower-case (str label))]
                                     (every? #(str/includes? l %) ws)))
             items)))

(defn pdf? [{:keys [label mime]}]
  (or (= "application/pdf" mime) (str/ends-with? (str/lower-case (str label)) ".pdf")))

(defn image? [{:keys [label mime]}]
  (or (str/starts-with? (str mime) "image/")
      (re-find #"(?i)\.(png|jpe?g|gif|webp|tiff?|bmp)$" (str label))))

(defn blob-path
  [{:keys [kind name]} address]
  (str "/api/blob/" address
       (when (= "media-set" kind) (str "?media-set=" (js/encodeURIComponent name)))))

(defn block-marks
  [{:keys [blocks page page_width page_height]}]
  (vec (for [{:keys [block_id block_type bounding_box]} blocks
             :let [{:keys [left top width height]} bounding_box]]
         {:id block_id :kind :rect :page (inc page) :label block_type
          :color (case block_type "TABLE" "#2d72d2" "TITLE" "#9d3f9d"
                       ("PAGE_HEADER" "PAGE_FOOTER") "#8f99a8" "#d1980b")
          :geom {:x (* left page_width) :y (* top page_height)
                 :w (* width page_width) :h (* height page_height)}})))

(defn deployment [st id] (get-in st [:deployments id] {:name "" :repository ""}))
