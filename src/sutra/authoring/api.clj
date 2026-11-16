(ns sutra.authoring.api
  (:require [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as plan]
            [sutra.authoring.install.world :as world]
            [sutra.authoring.instance-change :as instance-change]
            [sutra.authoring.marketplace :as marketplace]
            [sutra.authoring.mutation :as mutation]
            [sutra.authoring.projections :as projections]
            [sutra.authoring.stage :as stage]))

(set! *warn-on-reflection* true)

(defn check-install [store schema current opts] (install/check-install store schema current opts))

(defn on-index [store b] (install/on-index store b))

(defn planned-ontology [schema current opts] (install/planned-ontology schema current opts))

(defn publish-schema!
  [store schema current opts callbacks]
  (install/publish-schema! store schema current opts callbacks))

(defn stage! [ctx params scope] (stage/stage! ctx params scope))

(defn stage-schema! [store schema current opts] (install/stage-schema! store schema current opts))

(defn live-ref-symbols [cur] (plan/live-ref-symbols cur))

(defn plan-retirements [schema current opts] (plan/plan-retirements schema current opts))

(defn install-world! [store b world opts] (world/install-world! store b world opts))

(defn apply-instance-change!
  [store seen request]
  (instance-change/apply-request! store seen request))

(defn rename-entity
  [store schema kinds-key from to opts]
  (mutation/rename-entity store schema kinds-key from to opts))

(defn projection-memory [ctx nm] (projections/memory ctx nm))

(defn projection-push! [ctx nm body] (projections/push! ctx nm body))

(defn projection-push-plan [ctx nm files body] (projections/push-plan ctx nm files body))

(defn inputs-around [ctx base] (projections/inputs-around ctx base))

(defn inputs-changed [ctx around] (projections/inputs-changed ctx around))

(def python-field mutation/python-field)

(defn rename-in-python [code field old new] (mutation/rename-in-python code field old new))

(defn product-of-project [store p] (marketplace/product-of-project store p))
