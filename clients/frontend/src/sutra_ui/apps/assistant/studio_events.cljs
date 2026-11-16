(ns sutra-ui.apps.assistant.studio-events
  (:require [sutra.kit.slug :refer [slug]]
            [sutra-ui.api :as api]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.staging :as staging]))

(def initial
  {:name nil :text "" :saved "" :new? false :busy nil :error nil :result nil
   :projects nil :project nil :new-name ""})

(defn template [model project]
  (str "{:display-name \"Neuer Agent\"\n"
       " :description  \"Wofür er da ist.\"\n"
       " :project      :" project "\n"
       " :model        \"" (or model "modell-id") "\"\n"
       " :system-prompt \"Du …\"\n"
       " ;; :propose = jede Action als Vorschlag zum Annehmen, :apply = sofort, :dry = nur "
       "aufzeichnen\n"
       " :writes       :propose\n"
       " ;; Werkzeuge: {:kind :ontology} {:kind :object-query :object-type :x}\n"
       " ;;            {:kind :action :action :x} {:kind :function :function :x} {:kind "
       ":calculator}\n"
       " :tools        [{:kind :ontology}\n"
       "                {:kind :calculator}]}\n"))

(defn- busy [db c then]
  (rq/request (assoc db :error nil :result nil)
              {:ask c :busy [:busy] :error [:error] :then [then]}))

(defn- create [{:keys [new-name project] :as db} model]
  (let [nm (slug new-name)]
    (when (and (seq nm) project)
      {:db (assoc db :name nm :new? true :text (template model project) :saved "" :error nil
                  :result nil :new-name "")})))

(defn- stage [{:keys [name text] :as db} preview? on-change]
  (busy db [api/post! :resource-file {:section "agents" :name name}
            {:content text :preview preview?}]
        [:studio/staged preview? text on-change]))

(defn- staged [db preview? text on-change {:keys [staged]}]
  (if preview?
    {:db (assoc db :result {:preview? true :staged staged})}
    {:db (assoc db :result {:preview? false :staged staged} :saved text :new? false)
     :fx [[:invoke [staging/refresh-staged!]] [:invoke [on-change]]]}))

(machine/reg-event :studio.projects/load
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask  [api/get! :projects] :then [[:studio.projects/loaded]]
                                     :else [[:rf/set [:projects] []]]})))

(machine/reg-event :studio.projects/loaded
                   (fn [{:keys [db]} [_ {:keys [projects]}]]
                     (let [ps (sort (map (comp name :name) projects))]
                       {:db (assoc db :projects ps :project (or (:project db) (first ps)))})))

(machine/reg-event :studio/project (fn [{:keys [db]} [_ v]] {:db (assoc db :project v)}))

(machine/reg-event :studio/new-name (fn [{:keys [db]} [_ v]] {:db (assoc db :new-name v)}))

(machine/reg-event :studio/create (fn [{:keys [db]} [_ model]] (create db model)))

(machine/reg-event :studio/open
                   (fn [{:keys [db]} [_ nm]]
                     (busy (assoc db :name nm :new? false)
                           [api/get! :resource-file {:section "agents" :name nm}]
                           [:studio/opened])))

(machine/reg-event :studio/opened
                   (fn [{:keys [db]} [_ {:keys [content]}]]
                     {:db (assoc db :text content :saved content)}))

(machine/reg-event :studio/text (fn [{:keys [db]} [_ v]] {:db (assoc db :text v)}))

(machine/reg-event :studio/restore
                   (fn [{:keys [db]} [_ content]] {:db (assoc db :text content :result nil)}))

(machine/reg-event :studio/stage
                   (fn [{:keys [db]} [_ preview? on-change]] (stage db preview? on-change)))

(machine/reg-event :studio/staged
                   (fn [{:keys [db]} [_ preview? text on-change r]]
                     (staged db preview? text on-change r)))
