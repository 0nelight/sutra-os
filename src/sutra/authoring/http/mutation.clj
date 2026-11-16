(ns sutra.authoring.http.mutation
  (:require [sutra.authoring.install.live :as live]
            [sutra.authoring.mutation :as mutation]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.publishing :as publishing]
            [sutra.store.access.scope :as scope]
            [sutra.store.kept :as kept]
            [sutra.types.rid :as rid]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(defn answer
  [r]
  (if-let [c (:conflict r)]
    (refusal/refuse! :refusal.authoring/mutation-conflict (dissoc c :error) {:error (:error c)})
    (ok r)))

(defn step-handler
  [id step-fn]
  (ep/handler id {:respond (fn [a]
                             (answer
                              (mutation/run-step (scope/at (:store a) (:branch a)) (:schema a)
                                                 (step-fn a)
                                                 {:project (:sutra/project a) :edge (:edge a)})))}))

(defn- live-on
  [{:keys [store branch] :as a}]
  (let [names (delay (:names (live/index-of (kept/db store branch))))
        s     (:schema a)]
    (fn [sec k]
      (let [nk (rid/name-field-of sec)]
        (live/lookup (live/section s @names sec nk #(live/whole-section s sec nk)) k)))))

(def ^:private step-of
  {:add     (fn [kind a]
              (kernel/step (scope/code) :create kind
                           (publishing/versioned-body (live-on a) kind (:body a))))
   :update  (fn [kind a]
              (kernel/step (scope/code) :update kind
                           (publishing/versioned-body (live-on a) kind
                                                      (assoc (:body a) :name (:name a)))))
   :delete  (fn [kind a] (kernel/step (scope/code) :delete kind {:name (:name a)}))
   :cascade (fn [kind a]
              (assoc (kernel/step (scope/code) :delete kind {:name (:name a)})
                     :cascade? (:cascade a)))})

(defn step
  [id kind op]
  (step-handler id #((step-of op) kind %)))

(defn rename-handler
  [id kinds-key]
  (ep/handler id {:respond (fn [a]
                             (answer
                              (mutation/rename-entity (scope/at (:store a) (:branch a))
                                                      (:schema a) kinds-key (:name a)
                                                      (:to (:body a)) {:edge (:edge a)})))}))
