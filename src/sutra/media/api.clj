(ns sutra.media.api
  (:require [sutra.media.input-entries :as input-entries]
            [sutra.media.sets :as sets]))

(set! *warn-on-reflection* true)

(defn language-model-entry [schema in] (input-entries/language-model-entry schema in))

(defn set-items-entry [store schema ref] (input-entries/set-items-entry store schema ref))

(defn items-signature [store schema ref] (sets/items-signature store schema ref))

(defn record-output-items! [store schema compute items]
  (sets/record-output-items! store schema compute items))

(defn media-item-entry
  [store schema in address] (input-entries/media-item-entry store schema in address))

(defn media-set-entry [schema ds-rid rows] (input-entries/media-set-entry schema ds-rid rows))

(defn set-item-entry
  [store schema media-set address]
  (input-entries/set-item-entry store schema media-set address))

(defn media-sources [store schema admin?] (input-entries/media-sources store schema admin?))

(defn source-items [store schema source] (input-entries/source-items store schema source))

(defn whole-media-set [store schema in] (input-entries/whole-media-set store schema in))

(defn land-problem [schema media-set mime] (sets/land-problem schema media-set mime))

(defn land-refusal! [p data] (sets/land-refusal! p data))

(defn record! [store opts] (sets/record! store opts))

(defn set-name [schema ref] (sets/set-name schema ref))

(defn declared [schema media-set] (sets/declared schema media-set))

(defn items [store media-set] (sets/items store media-set))

(defn synced-files [store media-set] (sets/synced-files store media-set))

(defn virtual-address [media-set physical-path] (sets/virtual-address media-set physical-path))
