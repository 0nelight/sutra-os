(ns sutra-ui.apphost.widgets.comments
  (:require [sutra-ui.apphost.uploader :as uploader]
            [sutra-ui.ui.display :as display]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.download :as dl]
            [clojure.string :as str]
            [sutra-ui.ui.busy :as busy]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [text-area]]
            [sutra.capps.comment-feed :as feed]
            [sutra.capps.variables :as vars]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.toast :refer [toast!]]
            [sutra-ui.apphost.markdown-text :as md]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]
            [sutra.kit.remote :as rd]))

(defn- parent-of
  [object]
  (let [v (vars/deref* object)]
    (if (map? v) v (first v))))

(defn- ref-candidates
  [references]
  (into []
        (mapcat (fn [{:keys [rows label] :as spec}]
                  (for [row (let [v (vars/deref* rows)] (if (coll? v) v []))]
                    {:label    (or (when label (get (:values row) (->kw label))) (:title row)
                                   (str (:id row)))
                     :ref-kind :object
                     :ref      (str (:id row))
                     :row      row
                     :spec     spec})))
        references))

(defn- ref-tooltip
  [{:keys [row]}]
  (into [:dl.capp-comment__tip]
        (mapcat (fn [[k v]] [[:dt (name k)] [:dd (str v)]]))
        (take 8 (:values row))))

(defn- text-html [t] (r/unsafe-html (md/markdown->html t)))

(defn- mention-view
  [_m _found _env]
  (let [hover (r/atom false)]
    (fn [{:keys [label ref-kind ref]} found env]
      (let [c  (when (= :object ref-kind)
                 (some #(when (= ref (:ref %)) %) found))
            on (get-in c [:spec :on-select])]
        [:span.capp-comment__mention
         {:on-mouse-enter #(reset! hover true)
          :on-mouse-leave #(reset! hover false)
          :on-click       (fn []
                            (when (seq on)
                              (when-let [f ((:event-fn env) on)] (f (:row c)))))}
         (str "@" label)
         (when (and @hover c (empty? on))
           [overlay/popover {:on-close #(reset! hover false)} (ref-tooltip c)])]))))

(defn- body-view
  [body found env]
  (into [:div.capp-comment__body]
        (map-indexed
         (fn [i {:keys [text mention]}]
           ^{:key i}
           (if mention
             [mention-view mention found env]
             [:span.capp-comment__text {:dangerouslySetInnerHTML (text-html text)}])))
        (feed/segments body)))

(defn- download-attachment!
  [{:keys [descriptor] :as env} spec comment-id {:keys [hash name mime]}]
  (frame/request!
   (frame/frame-for env)
   {:ask    [client/comment-attachment!
             descriptor (assoc spec :op :attachment :comment comment-id :hash hash)]
    :then   [[:rf/invoke (fn [^js blob]
                           (dl/blob! (if (seq mime) (.slice blob 0 (.-size blob) mime) blob)
                                     name))]]
    :report true}))

(defn- entry-view
  [{:keys [feed author at body attachments id edit-kind values] :as c} me found spec env on-delete]
  (if (= :action feed)
    [:div.capp-comment.capp-comment--action
     [:div.capp-comment__head
      [:span.capp-comment__author author]
      (when at [:span.capp-comment__at at])
      [display/badge :info (str (or edit-kind "Action"))]]
     (when (seq values)
       [:div.capp-comment__body
        (str/join ", " (map (fn [[k v]] (str (name k) ": " v)) values))])]
    [:div.capp-comment
     [:div.capp-comment__head
      [:span.capp-comment__author author]
      (when at [:span.capp-comment__at at])
      (when (feed/deletable? c me)
        [ui/button
         {:minimal true :small true :class "capp-comment__delete" :on-click #(on-delete id)}
         "Löschen"])]
     [body-view body found env]
     (when (seq attachments)
       (into [:div.capp-comment__files]
             (map (fn [f]
                    ^{:key (:hash f)}
                    [ui/button
                     {:minimal true :small true
                      :on-click #(download-attachment! env spec id f)}
                     (str "📎 " (:name f))]))
             attachments))]))

(defn- run-after-action!
  [{:keys [descriptor refresh] :as env} {:keys [action param users-param object-param]}
   text users parent]
  (when action
    (frame/request!
     (frame/frame-for env)
     {:ask    [client/submit-strict! descriptor
               (cond-> {(or (->kw param) :body) text}
                 users-param  (assoc (->kw users-param) users)
                 object-param (assoc (->kw object-param) (:id parent)))
               {:action action}]
      :then   [[:rf/invoke (fn [d] (when (and refresh d) (refresh d)))]]
      :report true})))

(defn- read-files!
  [files on-done]
  (let [files (vec files)]
    (if (empty? files)
      (on-done [])
      (let [out (atom [])]
        (doseq [^js f files]
          (uploader/file->b64
           f
           (fn [b64]
             (swap! out conj {:name (.-name f) :mime (.-type f) :bytes b64})
             (when (= (count @out) (count files)) (on-done @out)))
           (fn [m] (toast! (str m) {:error? true}))))))))

(machine/reg-event ::post
                   (fn [{:keys [db]} [_ slot key ask then]]
                     (rq/request db {:ask  ask
                                     :then [[::posted slot key then]]
                                     :else [[::refused slot key]]})))

(machine/reg-event ::posted
                   (fn [{:keys [db]} [_ slot key then answer]]
                     (when (= key (get-in db [:reads slot :key]))
                       {:db (-> db
                                (assoc-in [:reads slot :rd] (rd/of-answer answer))
                                (update ::refused dissoc slot))
                        :fx [(when then [:invoke [then answer]])]})))

(machine/reg-event ::refused
                   (fn [{:keys [db]} [_ slot key e]]
                     (when (= key (get-in db [:reads slot :key]))
                       {:db (assoc-in db [::refused slot] (str e))})))

(defn comments
  [cfg env]
  (r/with-let [f     (frame/frame-for env)
               held  (atom nil)
               drop! #(doseq [s [::thread ::log ::users]] (frame/send! f [:capp/drop-read [s %]]))
               draft (r/atom "")
               caret (r/atom 0)
               files (r/atom [])]
    (let [{:keys [object placeholder empty references action-log? mention-users? notify?]} cfg
          {:keys [descriptor]} env
          parent (parent-of object)
          t      (or (:object-type-ref parent) (:object-type parent))
          spec   {:object-type (some-> t name) :id (some-> (:id parent) str)}
          shows? (and t (:id parent))
          slot   [::thread spec]
          _      (when (and shows? (not= spec @held)) (some-> @held drop!) (reset! held spec))
          read   (fn [s ask] (if shows? (frame/read! env [s spec] spec ask) {}))
          thread (read ::thread [client/comments! descriptor (assoc spec :op :list)])
          log    (when action-log?
                   (read ::log [client/edit-history! descriptor {:type (some-> t name)
                                                                 :pk (:id spec)}]))
          users  (when mention-users?
                   (mapv (fn [{:keys [label]}] {:label label :ref-kind :user :ref label})
                         (:users (:data (read ::users [client/users! descriptor {}])))))
          post!  (fn [body then]
                   (frame/send! f [::post slot spec
                                   [client/comments! descriptor (merge spec body)] then]))
          err    (or @(machine/subscribe [:capp/at [::refused slot]] {:frame f})
                     (some-> (:error thread) str))
          found  (ref-candidates references)
          typed  (feed/typed-mention @draft @caret)
          hits   (when typed (feed/candidates users found (:query typed)))
          {me :me cs :comments} (:data thread)]
      [:div.capp-comments
       (cond
         (not shows?)         [:div.capp-comments__empty "Kein Objekt gewählt."]
         (nil? (:data thread)) [busy/block {:class "capp-comments__empty"} nil]
         :else
         (let [entries (feed/feed (vec cs) (when action-log? (vec (:history (:data log)))))]
           (if (seq entries)
             (into [:div.capp-comments__list]
                   (map-indexed
                    (fn [i e]
                      ^{:key (or (:id e) i)}
                      [entry-view e me found spec env
                       #(post! {:op :delete :comment %} nil)]))
                   entries)
             [:div.capp-comments__empty (or empty "Noch keine Kommentare.")])))
       (when shows?
         [:div.capp-comments__composer
          [text-area
           {:class       "capp-comments__input"
            :value       @draft
            :placeholder (or placeholder "Kommentar…")
            :on-change   (fn [e]
                           (reset! caret (.. e -target -selectionStart))
                           (reset! draft (.. e -target -value)))
            :on-key-up   #(reset! caret (.. % -target -selectionStart))
            :on-click    #(reset! caret (.. % -target -selectionStart))}]
          (when (seq hits)
            (into [:div.capp-comments__mentions]
                  (map (fn [c]
                         ^{:key (str (:ref-kind c) (:ref c))}
                         [ui/button
                          {:minimal true :small true :on-click #(reset! draft
                                              (feed/insert-mention @draft @caret typed c))}
                          (:label c)]))
                  hits))
          [:label.capp-comments__attach
           (if (seq @files) (str "📎 " (count @files)) "📎")
           [:input {:type      "file"
                    :multiple  true
                    :class     "capp-uploader__file"
                    :on-change #(reset! files (vec (array-seq (.. % -target -files))))}]]
          [ui/button
           {:intent   :primary
            :disabled (and (str/blank? @draft) (empty? @files))
            :on-click
            (fn []
              (let [text @draft]
                (read-files!
                 @files
                 (fn [attachments]
                   (post! {:op :add :text text :attachments attachments
                           :notify (not (false? notify?))}
                          (fn [{us :users}]
                            (reset! draft "")
                            (reset! files [])
                            (run-after-action! env cfg text (vec us) parent)))))))}
           "Senden"]
          (when err [:div.capp-action__error err])])])
    (finally (some-> @held drop!))))
