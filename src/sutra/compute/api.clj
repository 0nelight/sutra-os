(ns sutra.compute.api
  (:require [sutra.compute.clojure :as clojure]
            [sutra.compute.external :as ext]
            [sutra.compute.render :as render]
            [sutra.compute.runner :as runner]))

(set! *warn-on-reflection* true)

(defn run-edit-function! [opts host] (clojure/run-edit-function! opts host))

(defn ->wire [x] (ext/->wire x))

(defn <-wire [x] (ext/<-wire x))

(defn run-python! [job] (ext/run-python! job))

(defn spans [err] (ext/spans err))

(defn generate [template opts] (render/generate template opts))

(defn prepare-inputs
  [language resolved prev-rows] (runner/prepare-inputs language resolved prev-rows))

(defn run-code! [language compute opts] (runner/run-code! language compute opts))

(def runners runner/runners)
