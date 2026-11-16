(ns sutra.capps.button-group
  (:require [clojure.string :as str]
            [sutra.capps.intent :as ci]
            [sutra.kit.coerce :refer [->kw]]))

(defn state
  [{:keys [when-false disabled?] :as b}]
  (cond
    (and (contains? b :visible) (not (:visible b)))
    (if (= :hidden (->kw when-false)) :hidden :disabled)
    disabled? :disabled
    :else     :shown))

(defn shown [buttons] (filterv #(not= :hidden (state %)) buttons))

(defn button-props
  [{:keys [intent minimal? tag? large? fill?]}]
  (cond-> (ci/btn-props (or (->kw intent) :none))
    minimal? (assoc :minimal true)
    tag?     (assoc :small true)
    large?   (assoc :large true)
    fill?    (assoc :fill true)))

(defn button-style
  [{:keys [color minimal?]}]
  (when color
    (if minimal?
      {:color color :background "transparent" :border-color "transparent"}
      {:background color :border-color color :color "#fff"})))

(defn plan
  [{:keys [action url export function-export media-export on-click] :as b}]
  (cond
    action          [:action b]
    url             [:url b]
    export          [:export b]
    function-export [:function-export b]
    media-export    [:media-export b]
    (seq on-click)  [:events b]))

(defn- safe-name
  [s]
  (str/replace (str s) #"[\\/:*?\"<>|]" "_"))

(defn export-format
  [{:keys [format props]}]
  (cond
    (= :clipboard (->kw format))                              :clipboard
    (some #(and (map? %) (or (:from %) (:function %))) props) :csv
    :else                                                     :xlsx))

(defn export-cols
  [props rows]
  (if (seq props)
    (mapv #(if (map? %) % {:prop %}) props)
    (mapv (fn [k] {:prop k}) (sort (keys (:values (first rows)))))))

(defn tsv-text
  [{:keys [header rows]}]
  (str/join "\n" (map #(str/join "\t" (map (fn [v] (str/replace (str v) #"[\t\n\r]" " ")) %))
                      (cons header rows))))

(defn export-filename
  [filename ext]
  (str (safe-name (or (not-empty (str filename)) "export")) "." ext))

(def file-types
  {:csv  {:ext "csv"  :mime "text/csv;charset=utf-8"}
   :txt  {:ext "txt"  :mime "text/plain;charset=utf-8"}
   :json {:ext "json" :mime "application/json"}
   :xml  {:ext "xml"  :mime "application/xml"}
   :pdf  {:ext "pdf"  :mime "application/pdf" :binary? true}
   :docx {:ext  "docx" :binary? true
          :mime "application/vnd.openxmlformats-officedocument.wordprocessingml.document"}
   :xlsx {:ext  "xlsx" :binary? true
          :mime "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"}})

(defn function-file
  [{:keys [file-type filename]} output]
  (let [{:keys [ext mime binary?] :as t} (get file-types (->kw file-type))
        text (when (some? output) (str output))]
    (cond
      (nil? t)    {:error (str "unbekannter Dateityp " (pr-str file-type))}
      (nil? text) {:error "die Funktion lieferte nichts"}
      (and binary? (str/starts-with? text "data:"))
      {:error "die Funktion darf keinen data:-Präfix liefern"}
      :else {:text text :binary? (boolean binary?) :mime mime
             :filename (export-filename filename ext)})))

(defn- values-of [row] (or (:values row) row))

(defn- original-name
  [v]
  (if (map? v)
    (or (:filename v) (:name v))
    (some-> v str (str/split #"/") last)))

(defn media-name
  [template row v timestamp]
  (let [original (original-name v)
        vs       (values-of row)]
    (safe-name
     (if (str/blank? (str template))
       original
       (str/replace (str template) #"\{\s*([^{}\s]+)\s*\}"
                    (fn [[_ k]]
                      (str (case k
                             "timestamp" timestamp
                             "filename"  original
                             (get vs (keyword k) "")))))))))

(defn media-downloads
  [{:keys [items filename]} timestamp]
  (into []
        (mapcat (fn [{:keys [rows prop max]}]
                  (let [k (->kw prop)]
                    (for [row  (cond->> rows max (take max))
                          :let [v (get (values-of row) k)]
                          v    (if (sequential? v) v [v])
                          :when (some? v)]
                      {:src v :name (media-name filename row v timestamp)}))))
        items))
