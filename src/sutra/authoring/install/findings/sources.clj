(ns sutra.authoring.install.findings.sources
  (:require [sutra.kit.coerce :refer [->name-kw]]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]))

(set! *warn-on-reflection* true)

(defn- by-kw [m] (update-keys m ->name-kw))

(defn- head
  [{:keys [entity entity-class source]}]
  (str (if (= :functions entity-class) "Function " "Transform ") (name entity)
       " declares source " (name source) " in :external-systems"))

(def external-systems
  {:find (fn [{:keys [text] :as d}]
           (let [sources (live/merged (diff/by-name-kw d :sources ->name-kw)
                                      (by-kw (:sources text)))]
             (for [[t-name args] (diff/staged d :transform)
                   src           (map #(->name-kw (if (map? %) (:source-id %) %))
                                      (:external-systems args))
                   :let [spec (live/lookup sources src)
                         rule (cond (nil? spec) :external-system-unknown
                                    (not (:external-transform spec))
                                    :external-system-not-released)]
                   :when rule]
               {:rule rule :entity-class (if (:function-name args) :functions :transforms)
                :entity t-name :source src})))
   :says {:external-system-unknown
          (fn [f]
            (str (head f) ", and no source of that name exists. Declare the source first (in"
                 " :sources, or `sutra source …`), then the code that reaches it."))
          :external-system-not-released
          (fn [f]
            (str (head f) ", and that source is not released to code: the egress agent hands"
                 " its secrets only to a source with :external-transform true. Set it on the"
                 " source."))}})
