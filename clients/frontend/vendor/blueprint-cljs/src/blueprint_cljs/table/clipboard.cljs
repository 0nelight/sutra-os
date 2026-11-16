(ns blueprint-cljs.table.clipboard
  "Blueprint's `Clipboard`: cells as tab-separated text onto the system clipboard."
  (:require [clojure.string :as str]))

(defn hiccup-text
  "The text a hiccup form shows: strings and numbers, depth first; component props maps
   are skipped. What `getCellClipboardData` defaults to (Blueprint uses `innerText`)."
  [x]
  (cond
    (nil? x)        ""
    (string? x)     x
    (number? x)     (str x)
    (boolean? x)    ""
    (map? x)        ""
    (vector? x)     (let [[head & more] x
                          out (when (fn? head) (try (apply head more) (catch :default _ nil)))]
                      ;; a form-1 component renders its children; a form-2 one returns a fn
                      (if (vector? out)
                        (hiccup-text out)
                        (str/join (map hiccup-text more))))
    (sequential? x) (str/join (map hiccup-text x))
    :else           (str x)))

(defn- tsv [rows]
  (->> rows
       (map (fn [cells] (str/join "\t" (map #(str/replace (str %) #"[\t\n]" " ") cells))))
       (str/join "\n")))

(defn- fallback-copy! [text]
  (let [ta (.createElement js/document "textarea")]
    (set! (.-value ta) text)
    (set! (.. ta -style -position) "fixed")
    (set! (.. ta -style -opacity) "0")
    (.appendChild (.-body js/document) ta)
    (.select ta)
    (let [ok (try (.execCommand js/document "copy") (catch :default _ false))]
      (.remove ta)
      ok)))

(defn copy-string!
  "Puts `text` on the clipboard. Returns a promise of true/false."
  [text]
  (if-let [cb (some-> js/navigator .-clipboard)]
    (-> (.writeText cb text)
        (.then (constantly true))
        (.catch (fn [_] (fallback-copy! text))))
    (js/Promise.resolve (fallback-copy! text))))

(defn copy-cells!
  "Blueprint `Clipboard.copyCells`: a seq of rows of cell values as TSV. Returns a promise."
  [rows]
  (copy-string! (tsv rows)))
