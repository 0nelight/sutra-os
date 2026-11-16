(ns sutra.documents.sheets
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.calx.address :as addr]
            [sutra.calx.core :as c]
            [sutra.calx.io.csv :as csv]
            [sutra.calx.io.edn :as cedn]
            [sutra.calx.io.xlsx :as xlsx]
            [sutra.calx.value :as v]
            [sutra.kit.html :refer [esc]]))

(set! *warn-on-reflection* true)

(def max-cells
  50000)

(def first-sheet "Tabelle1")

(defn sheet? [d] (= "sheet" (some-> (:format d) name)))

(defn- opts [locale] {:config {:locale (keyword (name (or locale :de)))}})

(defn- qualified [sheet a] (str (addr/quote-sheet-name sheet) "!" a))

(defn- checked
  [sheets]
  (let [names (mapv #(str/trim (str (:name %))) sheets)
        n     (reduce + (map (comp count :cells) sheets))]
    (cond
      (empty? sheets) (refusal/refuse! :refusal.sheet/no-sheet {})
      (some str/blank? names) (refusal/refuse! :refusal.sheet/unnamed {})
      (not (apply distinct? (map str/lower-case names)))
      (refusal/refuse! :refusal.sheet/duplicate-name {:sheets names})
      (> n max-cells)
      (refusal/refuse! :refusal.sheet/too-many-cells {:cells n} {:max max-cells})
      :else
      (doseq [{:keys [cells styles]} sheets
              a (concat (keys cells) (keys styles))]
        (when-not (addr/parse-cell (name a))
          (refusal/refuse! :refusal.sheet/not-a-cell {:cell (name a)}))))
    (mapv #(assoc %2 :name %1) names sheets)))

(defn from-sheets
  [sheets locale]
  (let [sheets (checked sheets)
        w      (reduce c/add-sheet
                       (c/workbook (array-map (:name (first sheets)) []) (opts locale))
                       (map :name (rest sheets)))
        w      (reduce (fn [w [a st]] (c/set-style w a st))
                       w
                       (for [{nm :name styles :styles} sheets
                             [a st] styles
                             :when (seq st)]
                         [(qualified nm (name a)) st]))]
    (c/set-cells w (for [{nm :name cells :cells} sheets
                         [a x] cells
                         :when (not (and (string? x) (str/blank? x)))]
                     [(qualified nm (name a)) x]))))

(defn from-body
  [body locale]
  (if (str/blank? body)
    (c/workbook (array-map first-sheet []) (opts locale))
    (try (cedn/read-string* body (opts locale))
         (catch Exception e
           (refusal/refuse! :refusal.sheet/unreadable {} {:reason (ex-message e)} e)))))

(defn body
  [w]
  (cedn/write-string w))

(defn- kind [x]
  (cond (nil? x) "empty" (number? x) "number" (string? x) "text" (boolean? x) "bool"
        (v/error? x) "error" :else "text"))

(defn- occupied
  [w id]
  (into (set (keys (get-in w [:sheets id :cells])))
        (for [[[s r c] [nr nc]] (:spills w)
              :when (= s id)
              i (range nr) j (range nc)]
          [(+ r i) (+ c j)])))

(defn- cell-view [w a]
  (let [in (c/input w a)
        x  (c/value w a)
        {:keys [text color]} (c/rendered w a)]
    (cond-> {:text (str text) :kind (kind x)}
      (some? in) (assoc :input (str in))
      color      (assoc :color (name color)))))

(defn view
  [w]
  (vec (for [id (:order w)
             :let [nm (get-in w [:sheets id :name])
                   as (for [[r c] (sort (occupied w id))]
                        [(addr/cell->a1 r c) (qualified nm (addr/cell->a1 r c))])]]
         {:name   nm
          :cells  (into {} (for [[a q] as
                                 :let [cv (cell-view w q)]
                                 :when (or (:input cv) (not= "empty" (:kind cv)))]
                             [a cv]))
          :styles (into {} (for [[a q] as
                                 :let [st (c/style w q)]
                                 :when (seq st)]
                             [a st]))})))

(def ^:private css-color #"#[0-9a-fA-F]{3,8}|[a-zA-Z]{3,20}")

(defn- cell-css [st cv]
  (str/join ";"
            (cond-> []
              (:bold st)      (conj "font-weight:bold")
              (:italic st)    (conj "font-style:italic")
              (:underline st) (conj "text-decoration:underline")
              (#{"left" "center" "right"} (:align st)) (conj (str "text-align:" (:align st)))
              (and (nil? (:align st)) (#{"number" "bool"} (:kind cv))) (conj "text-align:right")
              (:wrap st)      (conj "white-space:pre-wrap")
              (some->> (or (:color st) (:color cv)) str (re-matches css-color))
              (conj (str "color:" (or (:color st) (:color cv))))
              (some->> (:fill st) str (re-matches css-color))
              (conj (str "background:" (:fill st))))))

(defn html
  [w]
  (str/join
   "\n"
   (for [{:keys [name cells styles]} (view w)
         :let [rcs (keep (comp addr/parse-cell key) (merge styles cells))
               nr  (if (seq rcs) (inc (apply max (map first rcs))) 0)
               nc  (if (seq rcs) (inc (apply max (map second rcs))) 0)]]
     (str "<h2>" (esc name) "</h2>\n<table>"
          (str/join
           (for [r (range nr)]
             (str "<tr>"
                  (str/join
                   (for [c (range nc)
                         :let [a  (addr/cell->a1 r c)
                               cv (get cells a)
                               st (get styles a)
                               cs (cell-css st cv)]]
                     (str "<td" (when (seq cs) (str " style=\"" cs "\"")) ">"
                          (esc (:text cv)) "</td>")))
                  "</tr>")))
          "</table>"))))

(defn xlsx-b64 [w]
  (.encodeToString (java.util.Base64/getEncoder) (xlsx/xlsx-bytes w)))

(defn csv-text
  ([w sheet] (csv-text w sheet {}))
  ([w sheet opts]
   (csv/export-csv w (or (some #{sheet} (c/sheet-names w)) (first (c/sheet-names w))) opts)))
