(ns sutra-ui.apphost.markdown-editor
  (:require [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            ["@tiptap/core" :refer [Editor]]
            [sutra-ui.ui.core :as ui :refer [text-area]]
            ["@tiptap/starter-kit" :default StarterKit]
            ["@tiptap/extensions" :refer [Placeholder]]
            [reagent.core :as r]
            [sutra-ui.apphost.markdown-text :as mt]))

(defn- markdown-of
  [^js editor]
  (mt/doc->markdown (js->clj (.getJSON editor) :keywordize-keys true)))

(defn- command!
  [^js editor id]
  (let [c (-> (.chain editor) (.focus))]
    (.run
     (case id
       :bold    (.toggleBold c)
       :italic  (.toggleItalic c)
       :strike  (.toggleStrike c)
       :code    (.toggleCode c)
       :bullet  (.toggleBulletList c)
       :ordered (.toggleOrderedList c)
       :heading (.toggleHeading c #js {:level 2})
       c))))

(defn- rich-view
  [_opts]
  (let [editor (atom nil)
        shown  (atom nil)
        el     (atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (let [{:keys [sel placeholder]} (r/props this)
              text (str (or (rd sel) ""))
              ^js e (Editor. #js {:element    @el
                                  :extensions #js [StarterKit
                                                   (.configure Placeholder
                                                               #js {:placeholder
                                                                    (or placeholder "")})]
                                  :content    (mt/markdown->html text)
                                  :onUpdate   (fn [_]
                                                (let [md (markdown-of @editor)]
                                                  (reset! shown md)
                                                  (wr! sel md)))})]
          (reset! shown text)
          (reset! editor e)
          (when-let [on-ready (:on-ready (r/props this))] (on-ready e))))
      :component-did-update
      (fn [this]
        (let [text (str (or (rd (:sel (r/props this))) ""))]
          (when (and @editor (not= text @shown))
            (reset! shown text)
            (.setContent (.-commands ^js @editor) (mt/markdown->html text)))))
      :component-will-unmount
      (fn [_] (some-> ^js @editor .destroy))
      :reagent-render
      (fn [{:keys [sel]}]
        (rd sel)
        [:div.capp-mdedit__rich {:ref #(reset! el %)}])})))

(defn- raw-view
  [{:keys [sel placeholder on-area]}]
  [text-area
   {:input-ref on-area
    :value (str (or (rd sel) ""))
    :placeholder placeholder
    :on-change #(wr! sel (.. % -target -value))}])

(defn- raw-command!
  [sel ^js area spec]
  (let [[text at] (mt/format-raw (str (or (rd sel) ""))
                                 (.-selectionStart area) (.-selectionEnd area) spec)]
    (wr! sel text)
    (js/setTimeout #(do (.focus area) (.setSelectionRange area at at)) 0)))

(defn markdown-editor
  [_opts]
  (let [st     (r/atom {:raw? false})
        editor (atom nil)
        area   (atom nil)]
    (fn [{:keys [sel placeholder class auto-size? height]}]
      [:div {:class (cond-> "capp-mdedit"
                      auto-size? (str " capp-mdedit--auto")
                      class      (str " " class))
             :style (when (and height (not auto-size?))
                      {:height (if (number? height) (str height "px") height)})}
       [:div.capp-mdedit__bar
        (doall
         (for [{:keys [id label title] :as spec} mt/toolbar]
           ^{:key id}
           [ui/tooltip {:content title}
            [ui/button
             {:minimal       true :small true
              :on-mouse-down (fn [e]
                               (.preventDefault e)
                               (if (:raw? @st)
                                 (when-let [a @area] (raw-command! sel a spec))
                                 (some-> @editor (command! id))))}
             label]]))
        [ui/button
         {:minimal true :small true :class "capp-mdedit__btn--view"
          :on-click #(swap! st update :raw? not)}
         (if (:raw? @st) "Formatiert" "Markdown")]]
       (if (:raw? @st)
         [raw-view {:sel sel :placeholder placeholder :on-area #(reset! area %)}]
         [rich-view {:sel sel :placeholder placeholder :on-ready #(reset! editor %)}])])))
