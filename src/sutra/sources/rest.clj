(ns sutra.sources.rest
  (:require [sutra.kit.refusal :as refusal]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [sutra.sources.http-connector :as fhttp]))

(set! *warn-on-reflection* true)

(defn land-glpi-bundle!
  [{:keys [path] :as spec}]
  (when-not path (refusal/refuse! :refusal.foreign/glpi-path-missing {:spec (dissoc spec :rest)}))
  (let [src    (fhttp/source spec)
        bundle (into {} (for [it (get-in src [:decl :read-itemtypes])]
                          [(name it) (fhttp/records-of src (name it))]))]
    (io/make-parents path)
    (spit path (json/write-str bundle))
    path))
