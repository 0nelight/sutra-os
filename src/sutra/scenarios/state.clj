(ns sutra.scenarios.state
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.ops :as op]
            [sutra.kit.lifecycle :as lc]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lifecycle]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.api :as api]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(def table
  (lc/table {:name   :scenario
             :states #{:open :sealed :applied}
             :final  #{:applied}
             :edges  {:seal  {:from #{:open} :to :sealed}
                      :apply {:from #{:open :sealed} :to :applied}}}))

(def sealed-mark "sys/scenario-sealed")
(def applied-mark "sys/scenario-applied")

(defn- logged-state [store b]
  (let [names (into #{} (map :action-name) (run-log/action-log (world/at store b)))]
    (cond (names applied-mark) :applied
          (names sealed-mark)  :sealed
          :else                :open)))

(defn state
  [store b]
  (or (some-> (ctl/get store :scenarios {:branch (str b)}) :state keyword)
      (logged-state store b)))

(defn- transitioning [store b event]
  (lifecycle/transitioning :scenarios {:branch (str b)} table event
                           {:id (str b)
                            :state-of (fn [old]
                                        (if old (keyword (:state old)) (logged-state store b)))
                            :sets (fn [_ to] {:state (name to)})}))

(defn opened!
  [store b]
  (ctl/upsert! store :scenarios {:branch (str b) :state "open"}))

(defn seal! [store b] (ctl/commit! store [(transitioning store b :seal)]))

(defn applying
  [store b]
  (:records ((transitioning store b :apply) store (ctl/clock store))))

(defn append-fence
  [store b]
  (let [s (state store b)]
    (when-not (= :open s)
      (refusal/refuse! :refusal.scenarios/not-open
                       {:op/error :scenario/not-open :branch b :state s} {:state-name (name s)}))
    (if-let [e (ctl/expect store :scenarios {:branch (str b)})]
      [:record (:coll e) (:key e) (:expect e)]
      [:record :ctl/scenarios (str b) api/absent])))

(op/defop :scenarios/state [{:keys [store]} {:keys [branch]}]
  (state store branch))

(op/defop :scenarios/append-fence [{:keys [store]} {:keys [branch]}]
  (append-fence store branch))
