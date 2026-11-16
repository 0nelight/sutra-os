(ns sutra-ui.apps.assistant.evals-events
  (:require [clojure.string :as str]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(def poll-ms 3000)

(def initial {:data rd/loading :error nil :picked {} :open nil})

(defn- then-load [db c]
  (rq/request db {:ask c :then [[:evals/load]] :error [:error]}))

(machine/reg-event :evals/load
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask  [api/get! :agent-tests] :path [:data] :rd? true
                                     :then [[:evals/loaded]]})))

(machine/reg-event :evals/loaded
                   (fn [_ [_ r]]
                     {:fx (if (some (comp seq :running) (:suites r))
                            [[:after {:key :poll :ms poll-ms :event [:evals/load]}]]
                            [])}))

(machine/reg-event :evals/leave (fn [_ _] {:fx [[:cancel :poll]]}))

(machine/reg-event :evals/clear-error (fn [{:keys [db]} _] {:db (assoc db :error nil)}))

(machine/reg-event :evals/pick
                   (fn [{:keys [db]} [_ suite id agent-model]]
                     {:db (update-in db [:picked suite]
                                     (fn [p] (let [p (or p #{agent-model})]
                                               (if (p id) (disj p id) (conj p id)))))}))

(machine/reg-event :evals/version
                   (fn [{:keys [db]} [_ suite v]] {:db (assoc-in db [:version suite] v)}))

(machine/reg-event :evals/run
                   (fn [{:keys [db]} [_ suite models version]]
                     (then-load db [api/post! :agent-test-run {:suite suite}
                                    (cond-> {:models (vec models)}
                                      version (assoc :version version))])))

(machine/reg-event :evals/delete-case
                   (fn [{:keys [db]} [_ suite id]]
                     (then-load db [api/delete! :agent-test-case {:suite suite :case id}])))

(def case-initial {:open nil :reference "" :must "" :saved? false :error nil})

(defn- without-citations [answer]
  (-> (str answer) (str/replace #"\s*\[\[[A-Za-z0-9_\-]+:[^\]\s]+\]\]" "") str/trim))

(defn- case-body [{:keys [reference must]} {:keys [agent question history thread run]}]
  {:agent agent :input question :reference reference
   :must-contain (vec (remove str/blank? (map str/trim (str/split must #","))))
   :history (vec history) :thread thread :run run})

(machine/reg-event :evals.case/good
                   (fn [{:keys [db]} [_ answer]]
                     {:db (assoc db :open :good :reference (without-citations answer))}))

(machine/reg-event :evals.case/bad
                   (fn [{:keys [db]} _] {:db (assoc db :open :bad :reference "")}))

(machine/reg-event :evals.case/field (fn [{:keys [db]} [_ k v]] {:db (assoc db k v)}))

(machine/reg-event :evals.case/cancel (fn [{:keys [db]} _] {:db (assoc db :open nil)}))

(machine/reg-event :evals.case/save
                   (fn [{:keys [db]} [_ ctx on-saved]]
                     (rq/request db {:ask  [api/post! :agent-test-cases (case-body db ctx)]
                                     :path [:saved?] :pick (constantly true)
                                     :fx   [[:invoke [on-saved]]] :error [:error]})))
