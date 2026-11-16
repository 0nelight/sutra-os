(ns sutra-ui.apphost.widgets.media
  (:require [sutra.capps.leaf-props :as lp]
            [sutra.capps.media-view :as mv]
            [sutra-ui.apphost.widgets.image-annotation :as image-annotation]
            [sutra-ui.apphost.widgets.media-viewer :as media]
            [sutra-ui.apphost.widgets.spreadsheet :as spreadsheet]))

(defn- media-cfg
  [cfg env]
  (-> cfg
      (assoc :src ((:media-src env) (mv/source cfg)) :env env)
      (dissoc :object :prop)))

(def widgets
  {:media            (fn [cfg env] [media/media (media-cfg cfg env)])
   :spreadsheet      (fn [cfg env]
                       [spreadsheet/spreadsheet
                        (-> (media-cfg cfg env)
                            (update :layers (fn [ls] (mapv #(update % :rows lp/as-rows) ls))))])
   :image-annotation (fn [cfg env]
                       [image-annotation/image-annotation
                        (update (media-cfg cfg env) :rows lp/as-rows)])})
