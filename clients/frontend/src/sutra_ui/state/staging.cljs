(ns sutra-ui.state.staging
  (:require [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.state.db :as db]))

(defonce
  staged (db/cursor [:staging :staged] rd/loading))

(defn staged-list [] (rd/value-or @staged []))

(defn staged-view
  []
  (rd/list-view @staged))

(defn- as-staged [resp] (vec (:staged resp)))

(def ^:private asking [[:staging :staged] [api/get! :staging] {:pick as-staged :report "staged"}])

(def refresh (apply db/load asking))

(defn refresh-staged! [] (apply db/load! asking))
