(ns sutra-ui.apps.evals.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.apps.evals.core :as evals]))

(feature/contribute!
 :operations-tab :evals
 {:label-key :ui/ops-tab-evals :view evals/app :order 10 :bleed? true})
