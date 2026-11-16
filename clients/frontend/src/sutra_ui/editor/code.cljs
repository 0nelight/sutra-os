(ns sutra-ui.editor.code
  (:require [reagent.core :as r]
            ["@codemirror/state" :refer [EditorState]]
            ["@codemirror/view" :refer [EditorView keymap]]
            ["@codemirror/commands" :refer [indentWithTab]]
            ["@codemirror/lang-python" :refer [python]]
            ["@codemirror/merge" :refer [unifiedMergeView]]
            ["codemirror" :refer [basicSetup]]))

(defn- editor-theme [min-height]
  (.theme EditorView
          #js {"&"            #js {:fontSize "12px"
                                   :border "1px solid var(--c-border)"
                                   :borderRadius "4px"
                                   :backgroundColor "var(--c-surface)"}
               ".cm-content"  #js {:fontFamily "monospace"}
               ".cm-gutters"  #js {:backgroundColor "var(--c-surface-soft)"
                                   :color "var(--c-muted-2)"
                                   :border "none"}
               ".cm-scroller" #js {:minHeight (or min-height "260px")
                                   :fontFamily "monospace"
                                   :lineHeight "1.5"}
               "&.cm-focused" #js {:outline "none"}}))

(defn- lang-ext
  [language]
  (case language
    "python" (python)
    nil))

(defn- build-extensions [{:keys [language read-only? min-height on-change]}]
  (->> [basicSetup
        (lang-ext language)
        (.of keymap #js [indentWithTab])
        (editor-theme min-height)
        (when read-only? (.of (.-readOnly EditorState) true))
        (when read-only? (.of (.-editable EditorView) false))
        (when on-change
          (.of (.-updateListener EditorView)
               (fn [^js vu]
                 (when (.-docChanged vu)
                   (on-change (.toString (.. vu -state -doc)))))))]
       (remove nil?)
       clj->js))

(defn code-editor
  [_props]
  (let [!view (atom nil)
        !el   (atom nil)]
    (r/create-class
     {:display-name "code-editor"

      :component-did-mount
      (fn [this]
        (let [props (second (r/argv this))
              state (.create EditorState
                             #js {:doc (or (:value props) "")
                                  :extensions (build-extensions props)})]
          (reset! !view (EditorView. #js {:state state :parent @!el}))))

      :component-did-update
      (fn [this _]
        (let [props (second (r/argv this))
              ^js view @!view
              v     (or (:value props) "")]
          (when (and view (not= v (.toString (.. view -state -doc))))
            (.dispatch view
                       #js {:changes #js {:from   0
                                          :to     (.. view -state -doc -length)
                                          :insert v}}))))

      :component-will-unmount
      (fn [_]
        (when-let [^js v @!view] (.destroy v) (reset! !view nil)))

      :reagent-render
      (fn [_props]
        [:div {:ref #(reset! !el %)}])})))

(defn- diff-extensions [{:keys [language min-height baseline]}]
  (->> [basicSetup
        (lang-ext language)
        (editor-theme min-height)
        (.of (.-readOnly EditorState) true)
        (.of (.-editable EditorView) false)
        (unifiedMergeView #js {:original         (or baseline "")
                               :mergeControls    false
                               :highlightChanges true})]
       (remove nil?)
       clj->js))

(defn code-diff
  [_props]
  (let [!view (atom nil)
        !el   (atom nil)]
    (r/create-class
     {:display-name "code-diff"
      :component-did-mount
      (fn [this]
        (let [props (second (r/argv this))
              state (.create EditorState
                             #js {:doc (or (:value props) "")
                                  :extensions (diff-extensions props)})]
          (reset! !view (EditorView. #js {:state state :parent @!el}))))
      :component-will-unmount
      (fn [_] (when-let [^js v @!view] (.destroy v) (reset! !view nil)))
      :reagent-render
      (fn [_props] [:div {:ref #(reset! !el %)}])})))
