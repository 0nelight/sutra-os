(ns sutra-ui.apps.notebook.doc
  (:require [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.notebook.live :as live]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.notebook.state :refer [link-rows new-in send! session templates]]))

(defn target-project
  []
  (let [known (catalog/project-list)
        ok    #(when (contains? known %) %)]
    (when-not (= ::none @new-in)
      (or (ok @new-in)
          (ok (:selected-project @shell/ui-state))
          (some ok @ks/kompass-recents)))))

(defn template-in [tpls tname]
  (some #(when (= (some-> (:template-name %) name) (some-> tname name)) %) tpls))

(defn template-of [tname] (template-in @templates tname))

(defn template-title
  [tname]
  (or (not-empty (:title (template-of tname))) (tbl/humanize tname)))

(defn version-title
  [v]
  (or (not-empty (:label v)) (i18n/t :ui/notebook-version-autosave)))

(def page-mm
  {:a3 [297 420] :a4 [210 297] :a5 [148 210] :letter [215.9 279.4] :legal [215.9 355.6]
   :tabloid [279.4 431.8]})

(defn page-dims [page]
  (let [[w h] (get page-mm (keyword (or (:size page) :a4)) (:a4 page-mm))]
    (if (= :landscape (keyword (or (:orientation page) :portrait))) [h w] [w h])))

(defn sheet-doc?
  [d]
  (= :sheet (some-> (:format d) keyword)))

(defn markup-doc?
  [d]
  (= :markup (some-> (:format d) keyword)))

(defn diagram-doc?
  [d]
  (= :diagram (some-> (:format d) keyword)))

(defn own-format? [d] (or (sheet-doc? d) (markup-doc? d) (diagram-doc? d)))

(defn layout-doc?
  [d]
  (boolean (and (not (own-format? d))
                (re-find #"(?i)<style|<!doctype|<html|<body|\bclass=\"(?!page-break|np-)"
                         (str (:body d))))))

(defn page-css [page]
  (let [[w h] (page-dims page)
        box   (fn [pos text]
                (when (seq text)
                  (str "@" pos "{content:" (js/JSON.stringify (str text))
                       ";font-size:8pt;color:#5f6b7c}")))]
    (str "@page{size:" w "mm " h "mm;margin:" (or (not-empty (:margin page)) "18mm 16mm") ";"
         (box "top-center" (:header page)) (box "bottom-center" (:footer page)) "}")))

(defn wrap-html
  ([html] (wrap-html html nil))
  ([html page]
   (if (re-find #"(?i)^\s*<!doctype" (str html))
     html
     (str "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
          (when page (page-css page))
          "html{font-family:'DejaVu Sans','Liberation Sans',sans-serif;font-size:10pt}"
          "body{margin:0;color:#1c2127}table{border-collapse:collapse;width:100%}"
          "th,td{border:1px solid #c5cbd3;padding:3pt 5pt;text-align:left;vertical-align:top}"
          "th{background:#edeff2}img{max-width:100%}"
          "@media screen{.page-break{border-top:1px dashed #c5cbd3;margin:1.5em 0}}"
          "@media print{.page-break{break-after:page}}"
          "</style></head><body>" html "</body></html>"))))

(defn persistable [d]
  (select-keys d [:name :format :body :page]))

(defn live-state
  ([] (live-state @session))
  ([s] (some-> s :state deref)))

(defn saves-here?
  ([] (saves-here? @session))
  ([s] (or (nil? s) (live/leader? (live-state s)))))

(defn live-doc?
  [d]
  (and (:id d) (not (layout-doc? d))
       (contains? #{:html :sheet :markup :diagram} (keyword (:format d)))))

(defn html-body [html]
  (if-let [[_ b] (re-find #"(?is)<body[^>]*>(.*)</body>" (str html))] b (str html)))

(defn type-kw [t] (some-> t catalog/name-of name keyword))

(defn linked-row
  [t id]
  (get @link-rows [t (str id)]))

(defn create! [d] (send! [:notebook/create d (some-> (target-project) name)]))

(defn open! [id] (send! [:notebook/open id]))

(defn close! [] (send! [:notebook/close]))
