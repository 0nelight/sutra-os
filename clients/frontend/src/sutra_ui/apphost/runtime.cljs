(ns sutra-ui.apphost.runtime
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]))

(defn layout-kind
  [{:keys [layout name]}]
  (or (some-> (:kind layout) keyword)
      (some-> name keyword)))

(defmulti render-app
  (fn [descriptor _data] (layout-kind descriptor)))

(defmethod render-app :default [descriptor data]
  [:div {:class "capp capp--unknown"}
   [ui/problem (i18n/t :ui/capp-app-no-view)
    (str "layout :kind = " (pr-str (layout-kind descriptor))
         " · gebundene Objekttypen: "
         (if (seq data) (str/join ", " (map name (keys data))) "—"))]])
