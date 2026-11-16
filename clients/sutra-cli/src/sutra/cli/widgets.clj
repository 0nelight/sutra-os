(ns sutra.cli.widgets
  (:require [clojure.string :as str]
            [sutra.capps.catalog :as cat]))

(defn- param-line
  [{:keys [key kind required options default unless]}]
  (str "  " key " " (some-> kind name)
       (when required " (required)")
       (when unless (str " (unless " unless ")"))
       (when (seq options) (str " " (str/join " | " options)))
       (when (some? default) (str " default " default))))

(defn- child-line
  [{:keys [key many panes]}]
  (str "  " key (cond many " [nodes]" panes " [panes]" :else " node")))

(defn- spec-lines
  [spec]
  (let [nm (or (:widget spec) (:kind spec))]
    (concat
     [(str (if (:widget spec) ":widget " ":kind ") nm " — " (:label spec))
      "params:"]
     (let [own (set (map :key (:params spec)))]
       (map param-line (concat (:params spec)
                               (remove (comp own :key) cat/common-params))))
     (when (seq (:children spec))
       (cons "children:" (map child-line (:children spec)))))))

(defn- listing
  [refs]
  (concat
   [(str ":widget  " (str/join " " (sort (map (comp name :widget) cat/widget-specs))))
    (str ":kind  " (str/join " " (sort (map (comp name :kind) cat/container-specs))))
    "explain widget <name> names its params."]
   [""]
   (str/split-lines refs)))

(defn answer
  [item refs]
  (let [spec (when item (or (cat/widget-spec item) (cat/container-spec item)))]
    (cond
      (nil? item) {:lines (vec (listing refs))}
      spec        {:lines (vec (concat (spec-lines spec) [""] (str/split-lines refs)))}
      :else       {:error "unknown widget"
                   :lines [(str "no widget or layout kind " (name item)
                                " — `explain widget` names them")]})))
