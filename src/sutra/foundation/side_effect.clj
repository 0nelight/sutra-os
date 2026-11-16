(ns sutra.foundation.side-effect
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.branch-kinds :as branch-kinds]))

(set! *warn-on-reflection* true)

(def default-branch-name
  "main")

(def side-effect-kinds
  {:webhooks       :suppress
   :notifications  :suppress
   :external-calls :fail
   :builds         :allow})

(defn side-effect-verdict
  [branch allowed kind]
  (cond
    (or (nil? branch) (= branch default-branch-name)) :allow
    (contains? (set allowed) kind)                    :allow
    (and (= :external-calls kind)
         (branch-kinds/scenario-branch? branch))                 :defer
    :else                                             (get side-effect-kinds kind :suppress)))

(defn side-effect
  [{:keys [branch allowed]} kind detail]
  {:kind kind :verdict (side-effect-verdict branch allowed kind) :branch branch :detail detail})

(defn admit!
  [{:keys [kind verdict branch detail]}]
  (case verdict
    :allow true
    :fail  (refusal/refuse! :refusal.foundation/external-call-on-branch
                            {:kind        kind
                             :branch      branch
                             :detail      detail
                             :remedy      (str "land the change on " default-branch-name
                                               ", or opt this action type in with"
                                               " :on-branch {:external-calls true}")})
    false))
