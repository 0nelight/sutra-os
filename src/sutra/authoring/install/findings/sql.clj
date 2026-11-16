(ns sutra.authoring.install.findings.sql
  (:require [clojure.string :as str]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.coerce :refer [->name-kw]]
            [sutra.types.languages :as langs]
            [sutra.types.rid :as rid]
            [sutra.types.sql-traps :as traps]))

(set! *warn-on-reflection* true)

(defn sql-transform?
  [spec]
  (langs/query-language? (->name-kw (:language spec))))

(defn- postgres-spec?
  [spec]
  (= :postgres (langs/dialect (->name-kw (:language spec)))))

(defn staged-sql-specs
  [d]
  (for [nm (keys (diff/staged d :transform))
        :let [spec (live/lookup (get-in d [:after :transforms]) nm)]
        :when (sql-transform? spec)]
    [nm spec]))

(defn live-sql-specs
  [current]
  (for [[nm spec] ((diff/live-by-name current) :transforms :transform-name)
        :when (sql-transform? spec)]
    [nm spec]))

(defn- ->rid-refs
  [current spec]
  (let [idx (rid/name->rid-index current)]
    (rid/rewrite-refs :transforms spec
                      (fn [targets v] (or (rid/resolve-name->rid idx targets v) v)))))

(defn sql-verdicts
  [store code current named-specs]
  (when-let [explain (:sql-explain code)]
    (vec (for [[nm spec] named-specs]
           (assoc (if (str/blank? (str (:query spec)))
                    {:state :unknown :detail "it carries no :query"}
                    (explain store current (->rid-refs current spec)))
                  :transform nm)))))

(defn staged-sql-verdicts
  [store code {:keys [current] :as d}]
  (sql-verdicts store code current (staged-sql-specs d)))

(defn sql-trap-findings
  [named-specs]
  (mapcat (fn [[nm spec]]
            (when (postgres-spec? spec) (traps/char-cast-findings nm (:query spec))))
          named-specs))

(defn sql-compile-findings
  [verdicts]
  (traps/verdict-findings verdicts))

(def sql-traps
  {:find (fn [d] (sql-trap-findings (staged-sql-specs d)))})

(def ^:private introspection-says
  {:introspection-refused
   (fn [{transform :entity {:keys [detail]} ::fnd/ctx}]
     (str "Python transform " (pr-str transform) " did not import, so its"
          " @transform decorator was never read. The :inputs its .ent"
          " declares are NOT a substitute: they say what the code is"
          " supposed to declare, and this code declares nothing — it"
          " raises. Installing on the declaration would defer the same"
          " failure to the next build, in a run nobody is watching."
          "\n" (enc/elide 4000 detail)))

   :introspection-unavailable
   (fn [{transform :entity {:keys [detail]} ::fnd/ctx}]
     (str "Python transform " (pr-str transform) " was NOT introspected: "
          (enc/elide 500 detail) ". That is an infrastructure failure, not a"
          " verdict on the code — which is precisely why it cannot pass:"
          " nothing about this transform was verified. Retry once the"
          " transform sandbox/worker answers."))})

(defn introspection-problems
  [verdicts]
  (for [{:keys [transform state detail]} verdicts]
    (fnd/render
     introspection-says
     {:rule (if (= :refused state) :introspection-refused :introspection-unavailable)
      :entity-class :transform :entity transform
      ::fnd/ctx {:detail detail}})))
