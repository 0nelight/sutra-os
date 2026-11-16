(ns sutra-ui.state.governance
  (:require [sutra.kit.remote :as rd]
            [sutra.types.protection :as protection]
            [sutra-ui.api :as api]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.sync :as sync]))

(defonce
  proposals (db/cursor [:governance :proposals] rd/loading))

(defn proposal-list [] (rd/value-or @proposals []))
(defonce
  open-proposal (db/cursor [:governance :open-proposal] nil))

(defn- refresh-proposals [opts]
  (db/load [:governance :proposals] [api/list-proposals opts]
           {:pick (comp vec :proposals) :report "proposals"}))

(defn refresh-proposals!
  ([] (refresh-proposals! {:status "open"}))
  ([opts] (db/send! (second (refresh-proposals opts)))))

(db/reg :sutra.governance/proposal
        (fn [db [_ id]]
          {:db (update-in db [:governance :open-proposal]
                          (fn [o] (if (= id (:id o)) o {:id id :proposal rd/loading})))
           :fx [(db/load [:governance :open-proposal :proposal] [api/get! :proposal {:id id}]
                         {:pick rd/of-answer :current [[:governance :open-proposal :id] id]})]}))

(defn load-proposal! [id] (db/send! [:sutra.governance/proposal id]))

(defn- send!
  [what req]
  (db/promise! [:rf/request (assoc req :report what)]))

(defn- propose! [{:keys [source target title description]} branch?]
  (send! "propose"
         {:ask [api/post! :proposals (cond-> {:source source :title title}
                                       target      (assoc :target target)
                                       description (assoc :description description))]
          :fx  [(refresh-proposals {:status "all" :source source})
                (when branch? [:invoke [branches/refresh-branch-changes!]])]}))

(defn propose-change!
  [proposal]
  (propose! proposal false))

(defn propose-branch!
  [{:keys [source target title]}]
  (propose! {:source source :target target :title title} true))

(defn- build-after-merge
  [{:keys [build-scope]} build]
  (when-let [names (seq (get build-scope build))]
    (rq/ask {:ask    [api/post! :import-run {:only (mapv keyword names) :strategy :selected}]
             :report "build"})))

(db/reg :sutra.governance/merged
        (fn [_ [_ id build branch? r]]
          {:fx [[:dispatch [:sutra.governance/proposal id]]
                (refresh-proposals {:status "all"})
                [:invoke [sync/chain-refreshes! [sync/refresh-snapshot! branches/refresh-branches!
                                                 instances/refresh-instance-state!]]]
                (when build (build-after-merge r build))
                (when branch? [:invoke [branches/refresh-branch-changes!]])]}))

(defn- merge! [id build branch?]
  (send! "merge" {:ask  [api/post! :proposal-merge {:id id} nil]
                  :then [[:sutra.governance/merged id build branch?]]}))

(defn merge-proposal!
  ([id] (merge-proposal! id nil))
  ([id build] (merge! id build false)))

(defn merge-branch-proposal!
  ([id] (merge-branch-proposal! id nil))
  ([id build] (merge! id build true)))

(defn remove-from-branch!
  [branch tasks]
  (send! "remove from branch"
         {:ask [api/post! :branch-resources {:name branch} {:remove (vec tasks)}]
          :fx  [[:invoke [sync/chain-refreshes! [sync/refresh-snapshot!
                                                 branches/refresh-branch-changes!]]]]}))

(defonce
  protections (db/cursor [:governance :protections] rd/loading))

(defn refresh-protections! []
  (db/load! [:governance :protections] [api/get! :protections]
            {:pick rd/of-answer :report "protections"}))

(defn set-protection!
  [refs on?]
  (send! "protect" {:ask  [api/post! :protections {:resources (vec refs) :on (boolean on?)}]
                    :path [:governance :protections] :pick rd/of-answer}))

(defn protectable-ref?
  [ref]
  (contains? #{:protected :open} (protection/ref-state @protections ref)))

(defn protected-ref?
  [ref]
  (= :protected (protection/ref-state @protections ref)))
