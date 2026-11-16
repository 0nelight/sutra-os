(ns sutra-ui.shell.boundary
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.debug :as dbg]
            [sutra-ui.client-log :as client-log]
            [sutra-ui.apphost.registry :as capps]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui]))

(defn- top-component-frame
  [component-stack]
  (when component-stack
    (->> (str/split-lines component-stack)
         (map str/trim)
         (some (fn [l]
                 (when (str/starts-with? l "in ")
                   (first (str/split (subs l 3) #"\s+"))))))))

(def error-boundary
  (r/create-class
   {:display-name "ErrorBoundary"
    :get-initial-state (fn [_] {:error nil :component nil})
    :component-did-catch
    (fn [this err info]
      (let [[_ a _] (r/argv this)
            label   (or (:label a) "error-boundary")
            cstack  (some-> info .-componentStack)
            comp    (top-component-frame cstack)]
        (js/console.error "[error-boundary]" label err info)
        (let [sci-trace (capps/error-trace err)]
          (dbg/record! :render-error label
                       (str (when comp (str "in " comp " — ")) err
                            (when sci-trace (str "\n[app-source trace]\n" sci-trace)))
                       :stack (str (some-> err .-stack)
                                   (when sci-trace
                                     (str "\n\n[app-source trace]\n" sci-trace))
                                   (when cstack
                                     (str "\n\n[component stack]" cstack)))))
        (r/set-state this {:error err :component comp})
        (-> (client-log/map-stack! {:label   label
                                    :message (str (some-> err .-message))
                                    :stack   (str (some-> err .-stack))})
            (.then (fn [mapped]
                     (when (not-empty mapped)
                       (r/set-state this {:mapped mapped})))))))
    :reagent-render
    (fn [a b]
      (let [opts  (when (map? a) a)
            child (if opts b a)
            view-state    (some-> (r/state (r/current-component)))
            err   (:error view-state)
            comp  (:component view-state)
            mapped (:mapped view-state)]
        (if err
          [:div {:class "errboundary"}
           [:div {:class "boundary__header"}
            [:strong {:class "errboundary__title" :style {:font-family "sans-serif"}}
             (i18n/t :ui/render-error-friendly)]
            (let [this (r/current-component)]
              [ui/button {:on-click (fn [_] (r/set-state this {:error nil :component nil}))
                          :small    true
                          :icon     :refresh}
               (i18n/t :ui/render-error-retry)])]
           [ui/section
            {:title (i18n/t :ui/render-error-details) :collapsible true :compact true
             :collapse-props {:default-is-open false} :class "u-mt-8"}
            [ui/section-card
             [:div {:class "errboundary__message"}
              (if-let [l (:label opts)]
                (i18n/t :ui/render-error-in {:label l})
                (i18n/t :ui/render-error))
              "\n"
              (when comp [:span (i18n/t :ui/component-label) [:code comp] "\n"])
              (.-message err)]
             [:div {:class "errboundary__stack"}
              (if mapped
                (str "── " (i18n/t :ui/source-mapped) " ──\n" mapped
                     "\n\n── " (i18n/t :ui/raw) " ──\n" (.-stack err))
                (.-stack err))]]]]
          child)))}))
