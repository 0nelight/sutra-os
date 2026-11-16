(ns sutra.capps.markdown
  (:require [clojure.string :as str]
            [sutra.capps.table :as tbl]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.url :as url]))

(def ^:private inline-patterns
  [[:ref     #":objectreference\[([^\]]*)\]\{([^}]*)\}"]
   [:code    #"`([^`]+)`"]
   [:image   #"!\[([^\]]*)\]\(([^)\s]+)\)"]
   [:link    #"\[([^\]]+)\]\(([^)\s]+)\)"]
   [:bold    #"\*\*([^*]+(?:\*[^*]+\*[^*]*)*)\*\*"]
   [:bold    #"__([^_]+)__"]
   [:strike  #"~~([^~]+)~~"]
   [:strike  #"~([^~]+)~"]
   [:mark    #"==([^=]+)=="]
   [:italic  #"\*([^*]+)\*"]
   [:italic  #"_([^_]+)_"]])

(defn- first-match
  [s]
  (reduce (fn [best [kind re]]
            (let [m (re-find re s)]
              (if m
                (let [idx (str/index-of s (first m))]
                  (if (or (nil? best) (< idx (:idx best)))
                    {:kind kind :idx idx :m m}
                    best))
                best)))
          nil inline-patterns))

(defn- ref-attrs
  [s]
  (into {} (map (fn [[_ k v]] [k v])) (re-seq #"(\w+)=\"([^\"]*)\"" s)))

(declare parse-inline)

(defn- inline-node
  [kind m idx base]
  (let [[whole g1 g2] m
        inner-at (+ base idx (or (str/index-of whole (str g1)) 0))]
    (case kind
      :ref    (let [a (ref-attrs g2)]
                {:t :ref :object-type (get a "objectType") :pk (get a "primaryKey")
                 :label g1 :kids (parse-inline g1 inner-at)})
      :code   {:t :code :s g1 :at inner-at}
      :image  {:t :image :alt g1 :src (or (url/image-src g2) "")}
      :link   {:t :link :href (url/href g2) :kids (parse-inline g1 inner-at)}
      :bold   {:t :strong :kids (parse-inline g1 inner-at)}
      :strike {:t :strike :kids (parse-inline g1 inner-at)}
      :mark   {:t :mark :kids (parse-inline g1 inner-at)}
      :italic {:t :em :kids (parse-inline g1 inner-at)})))

(defn parse-inline
  ([s] (parse-inline s 0))
  ([s base]
   (loop [s (str s) at base acc []]
     (if (= "" s)
       acc
       (if-let [{:keys [kind idx m]} (first-match s)]
         (let [whole (first m)
               next  (+ idx (count whole))]
           (recur (subs s next)
                  (+ at next)
                  (cond-> acc
                    (pos? idx) (conj {:t :text :s (subs s 0 idx) :at at})
                    :always    (conj (inline-node kind m idx at)))))
         (conj acc {:t :text :s s :at at}))))))

(defn- lines-of
  [text]
  (loop [at 0 ls (str/split (str text) #"\n" -1) acc []]
    (if-let [l (first ls)]
      (recur (+ at (count l) 1) (rest ls) (conj acc {:s l :at at}))
      acc)))

(defn- heading? [l]  (re-find #"^(#{1,6})\s+(.*)$" l))
(defn- hr? [l]       (re-find #"^\s*(---|\*\*\*|___)\s*$" l))
(defn- task? [l]     (re-find #"^(\s*[-*]\s+\[([ xX])\]\s+)(.*)$" l))
(defn- bullet? [l]   (re-find #"^(\s*[-*]\s+)(.*)$" l))
(defn- ordered? [l]  (re-find #"^(\s*\d+\.\s+)(.*)$" l))
(defn- quote? [l]    (re-find #"^(\s*>\s?)(.*)$" l))
(defn- fence? [l]    (re-find #"^\s*```" l))
(defn- row? [l]      (re-find #"^\s*\|.*\|\s*$" l))
(defn- rule-row? [l] (re-find #"^\s*\|?\s*:?-{2,}:?\s*(\|\s*:?-{2,}:?\s*)*\|?\s*$" l))

(defn cell-spans
  [l]
  (let [n (count l)
        bars (filterv (fn [i] (and (= \| (nth l i)) (or (zero? i) (not= \\ (nth l (dec i))))))
                      (range n))
        ends (partition 2 1 bars)]
    (mapv (fn [[a b]]
            (let [raw   (subs l (inc a) b)
                  lead  (- (count raw) (count (str/triml raw)))]
              [(str/replace (str/trim raw) "\\|" "|") (+ a 1 lead)]))
          ends)))

(defn cells
  [l]
  (mapv first (cell-spans l)))

(defn- align-of
  [rule]
  (let [c (str/trim rule)
        l (str/starts-with? c ":")
        r (str/ends-with? c ":")]
    (cond (and l r) :center r :right l :left :else nil)))

(defn- table-node
  [ls]
  (let [[head rule & body] ls
        row (fn [{:keys [s at]}]
              (mapv (fn [[c off]] (parse-inline c (+ at off))) (cell-spans s)))]
    {:t      :table
     :aligns (mapv align-of (cells (:s rule)))
     :head   (row head)
     :rows   (mapv row body)}))

(defn- item-of
  [m at]
  (let [[_ lead txt] m]
    (parse-inline txt (+ at (count lead)))))

(defn- list-items
  [ls detect]
  (mapv (fn [{:keys [s at]}]
          (if-let [[_ lead mark txt] (task? s)]
            {:task (if (= " " mark) :open :done)
             :kids (parse-inline txt (+ at (count lead)))}
            {:kids (item-of (detect s) at)}))
        ls))

(defn- plain-line?
  [s]
  (not (or (str/blank? s) (heading? s) (hr? s) (bullet? s) (ordered? s) (quote? s) (fence? s)
           (row? s))))

(defn- paragraph
  [ls breaks?]
  {:t    :p
   :kids (into []
               (mapcat identity)
               (interpose [(if breaks? {:t :br} {:t :text :s " " :at nil})]
                          (map (fn [{:keys [s at]}] (parse-inline s at)) ls)))})

(defn parse
  ([text] (parse text nil))
  ([text {:keys [breaks?]}]
   (loop [ls (lines-of text) out []]
     (if-let [{:keys [s at]} (first ls)]
       (cond
         (str/blank? s)
         (recur (rest ls) out)

         (fence? s)
         (let [[code more] (split-with #(not (fence? (:s %))) (rest ls))]
           (recur (rest more) (conj out {:t :pre :s (str/join "\n" (map :s code))})))

         (and (row? s) (some-> (second ls) :s rule-row?))
         (let [rows (take-while #(row? (:s %)) ls)]
           (recur (drop (count rows) ls) (conj out (table-node rows))))

         (hr? s)
         (recur (rest ls) (conj out {:t :hr}))

         (heading? s)
         (let [[_ hashes txt] (heading? s)]
           (recur (rest ls)
                  (conj out {:t     :h
                             :level (count hashes)
                             :kids  (parse-inline txt (+ at (- (count s) (count txt))))})))

         (bullet? s)
         (let [items (take-while #(bullet? (:s %)) ls)]
           (recur (drop (count items) ls)
                  (conj out {:t :ul :items (list-items items bullet?)})))

         (ordered? s)
         (let [items (take-while #(ordered? (:s %)) ls)]
           (recur (drop (count items) ls)
                  (conj out {:t :ol :items (list-items items ordered?)})))

         (quote? s)
         (let [items (take-while #(quote? (:s %)) ls)]
           (recur (drop (count items) ls)
                  (conj out {:t     :quote
                             :lines (mapv (fn [{:keys [s at]}] (item-of (quote? s) at)) items)})))

         :else
         (let [items (take-while #(plain-line? (:s %)) ls)]
           (recur (drop (count items) ls) (conj out (paragraph items breaks?)))))
       out))))

(defn slices
  [s at ranges]
  (let [end  (+ at (count s))
        hits (filterv (fn [{a :start b :end}] (and (< a end) (> b at))) ranges)
        cuts (->> hits
                  (mapcat (fn [{a :start b :end}] [(max a at) (min b end)]))
                  (concat [at end])
                  distinct
                  sort)]
    (mapv (fn [[a b]]
            {:s     (subs s (- a at) (- b at))
             :at    a
             :range (some #(when (and (<= (:start %) a) (>= (:end %) b)) %) hits)})
          (partition 2 1 cuts))))

(defn- row-field
  [row k]
  (let [k (->kw k)]
    (if (contains? row k) (get row k) (get (:values row) k))))

(defn annotation-ranges
  [layers]
  (into []
        (mapcat
         (fn [{:keys [rows start end color conditional-format display active] :as layer}]
           (let [active-ids (into #{} (map :id) (if (map? active) [active] active))]
             (keep (fn [row]
                     (let [a (row-field row start)
                           b (row-field row end)
                           look (tbl/cond-format conditional-format
                                                 (or (:values row) row))]
                       (when (and (number? a) (number? b) (>= a 0) (< a b))
                         {:start   a
                          :end     b
                          :row     row
                          :layer   layer
                          :color   (or (:background look) (:color look) color)
                          :display (or (->kw display) :highlight)
                          :active? (contains? active-ids (:id row))})))
                   rows))))
        layers))

(defn selection
  [source [anchor-at anchor-off] [focus-at focus-off]]
  (when (and (number? anchor-at) (number? focus-at))
    (let [a (+ anchor-at anchor-off)
          b (+ focus-at focus-off)
          s (min a b)
          e (min (count source) (max a b))]
      (when (< s e)
        {:start s :end e :text (subs source s e)}))))

(defn ref-row
  [{:keys [object-type pk label]}]
  {:id pk :object-type-ref (->kw object-type) :object-type (->kw object-type) :title label})

(defn ref-type
  [ref-types {:keys [object-type]}]
  (some #(when (= (->kw (:object-type %)) (->kw object-type)) %) ref-types))

(defn ref-color
  [{:keys [color color-prop conditional-format rows]} {:keys [pk]}]
  (let [row (some #(when (= (str (:id %)) (str pk)) %) rows)]
    (or (when (and row color-prop) (not-empty (str (row-field row color-prop))))
        (when row
          (let [look (tbl/cond-format conditional-format (or (:values row) row))]
            (or (:background look) (:color look))))
        color)))

(defn ref-selected?
  [behavior {:keys [pk object-type]} {:keys [last-ref selected-rows]}]
  (case (->kw behavior)
    :last     (= last-ref [object-type pk])
    :selected (boolean (some #(= (str (:id %)) (str pk)) selected-rows))
    false))
