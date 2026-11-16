(ns sutra-ui.apphost.widgets.notebook
  (:require [sutra-ui.ui.busy :as busy]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.variables :refer [deref*]]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.editor.rich-text :as rich-text]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.widgets.media-viewer :as media]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(def ^:private save-ms 800)

(defn- document-kind
  [{:keys [format body]}]
  (cond
    (= :sheet (some-> format keyword))    :sheet
    (= :markdown (some-> format keyword)) :markdown
    (re-find #"(?i)<style|<!doctype|<html|<body" (str body)) :layout
    :else                                 :rich))

(defn- html-body
  [html]
  (if-let [[_ b] (re-find #"(?is)<body[^>]*>(.*)</body>" (str html))] b (str html)))

(defn- editor
  [_doc _editable? _on-change]
  (let [el (atom nil) ed (atom nil) gone? (atom false)]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (let [[_ doc editable? on-change] (r/argv this)]
          (.then (rich-text/ensure!)
                 (fn [{:keys [create!]}]
                   (when (and create! (not @gone?))
                     (reset! ed (create! {:el        @el
                                          :content   (:body doc)
                                          :editable? editable?
                                          :on-update on-change})))))))
      :component-will-unmount (fn [_] (reset! gone? true) (some-> @ed .destroy))
      :reagent-render
      (fn [_ editable? _]
        [:div {:class (cond-> "np-rich capp-notebook__body"
                        (not editable?) (str " np-rich--readonly"))
               :ref   #(reset! el %)}])})))

(defn- load-document!
  [id]
  (-> (api/get! :document-of {:id id})
      (.then (fn [d]
               (if (#{:markdown :layout} (document-kind d))
                 (.then (api/post! :document-export {:id id} {:targets ["html"]})
                        #(assoc d :body (html-body (:html %)) :rendered (:html %)))
                 d)))))

(machine/reg-event ::typed
                   (fn [{:keys [db]} [_ slot html]]
                     {:db (assoc-in db [::edit slot :latest] html)
                      :fx [[:after {:key [::save slot] :ms save-ms :event [::save slot]}]]}))

(machine/reg-event ::save
                   (fn [{:keys [db]} [_ [_ id :as slot]]]
                     (let [doc (get-in db [:reads slot :rd])]
                       (rq/request (assoc-in db [::edit slot :error] nil)
                                   {:ask    [api/put! :document-of {:id id}
                                             {:body           (get-in db [::edit slot :latest])
                                              :format         :html
                                              :name           (:name doc)
                                              :expect-version (:version doc)}]
                                    :busy   [[::edit slot :saving] :saving]
                                    :error  [::edit slot :error] :within [::edit slot]
                                    :then   [[::saved slot]]}))))

(machine/reg-event ::saved
                   (fn [{:keys [db]} [_ slot r]]
                     {:db (-> db
                              (assoc-in [::edit slot :saving] :saved)
                              (update-in [:reads slot :rd] merge
                                         (select-keys (:document r) [:version])))}))

(machine/reg-event ::left
                   (fn [{:keys [db]} [_ slot]]
                     {:db (update db ::edit dissoc slot)
                      :fx [[:cancel [::save slot]] [:dispatch [:capp/drop-read slot]]]}))

(defn- document-view
  [env id editable?]
  (r/with-let [f    (frame/frame-for env)
               slot [::document id]]
    (let [{doc :data load-error :error} (frame/read! env slot id [load-document! id])
          {:keys [saving error]} @(machine/subscribe [:capp/at [::edit slot]] {:frame f})
          error (or error load-error)
          kind  (some-> doc document-kind)]
      [:div.capp-notebook
       (when error [ui/problem error nil])
       (cond
         (nil? doc)      (when-not error [busy/block {:class "capp-objectview__loading"}
                                          (i18n/t :ui/capp-loading)])
         (= :sheet kind) [ui/empty-state {:title (i18n/t :ui/capp-notebook-sheet)}]
         (= :layout kind)
         [media/iframe {:html (:rendered doc) :title (:name doc)}]
         :else
         [:<>
          (when (and editable? saving)
            [:div.capp-notebook__state
             (i18n/t (if (= :saving saving)
                       :ui/capp-notebook-saving
                       :ui/capp-notebook-saved))])
          [editor doc (boolean editable?) #(frame/send! f [::typed slot %])]])])
    (finally (frame/send! f [::left slot]))))

(defn embedded-document
  [{:keys [document editable? class]} env]
  (let [id (str (deref* document))]
    [:div (when class {:class class})
     (if (str/blank? id)
       [ui/empty-state {:title (i18n/t :ui/capp-notebook-none)}]
       ^{:key id} [document-view env id editable?])]))

(def widgets
  {:notebook-document (fn [cfg env] [embedded-document cfg env])})
