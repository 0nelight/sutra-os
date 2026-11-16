(ns sutra-ui.apphost.widgets.freshness
  (:require [reagent.core :as r]
            [sutra-ui.ui.busy :as busy]
            [sutra.capps.format :as fmt]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.render-ctx :as rctx]))

(def ^:private reread-ms 60000)

(defn- shown
  [at]
  (or (fmt/temporal-text at {:style :relative :brief? true} (rctx/current))
      (i18n/t :ui/data-freshness-never)))

(defn- line
  [class label at]
  [:div {:class class}
   [:span.capp-freshness__name label]
   [:span.capp-freshness__updated
    [:span.capp-freshness__caption (i18n/t :ui/data-freshness-last-updated)]
    [:span.capp-freshness__time (shown at)]]])

(defn- by-node
  [updates]
  (into {} (map (juxt (juxt :kind :name) identity)) updates))

(defn- text [x] (if (keyword? x) (name x) (str x)))

(defn data-freshness
  [env {:keys [items]}]
  (r/with-let [f      (frame/frame-for env)
               slot   [::freshness (random-uuid)]
               again! #(when-not (.-hidden js/document) (frame/send! f [:capp/reread slot]))
               timer  (js/setInterval again! reread-ms)
               _      (when (exists? js/window)
                        (.addEventListener js/window "sutra:data-changed" again!))]
    (let [descriptor           (:descriptor env)
          {:keys [data error]} (frame/read! env slot items
                                            (if-let [g (:data-freshness-fn descriptor)]
                                              [g items]
                                              [client/data-freshness! descriptor]))
          updates              (when data (by-node (:updates data)))
          at                   (fn [kind nm] (get updates [kind (text nm)]))]
      (cond
        (and error (nil? updates))
        [:div.capp-freshness__error (str (i18n/t :ui/data-freshness-failed) ": " error)]

        (nil? updates)
        [busy/block {:class "capp-freshness__caption"} (i18n/t :ui/sv-history-loading)]

        :else
        (into [:div.capp-freshness]
              (for [{:keys [object-type sources]} items
                    :let [row (at "object-type" object-type)]
                    :when row]
                (into [:div.capp-freshness__item
                       [line "capp-freshness__type" (tbl/humanize (text object-type)) (:at row)]]
                      (for [{:keys [resource override-name]} sources
                            :let [src (at "source" resource)]
                            :when src]
                        [line "capp-freshness__source"
                         (or override-name (text resource)) (:at src)]))))))
    (finally
      (js/clearInterval timer)
      (frame/send! f [:capp/drop-read slot])
      (when (exists? js/window)
        (.removeEventListener js/window "sutra:data-changed" again!)))))
