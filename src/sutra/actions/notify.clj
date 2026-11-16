(ns sutra.actions.notify
  (:require [sutra.kit.refusal :as refusal]
            [sutra.actions.effects :as action-effects]
            [sutra.actions.execution :as x]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.lookup :as lookup]
            [sutra.compute.api :as compute]
            [sutra.governance.api :as governance]
            [sutra.notifications.api :as notifications]
            [sutra.ref.value :as rv]
            [sutra.store.seat :as seat]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn- refuse!
  ([code data] (refuse! code data nil))
  ([code data params] (refusal/refuse! code (assoc data :rule :notification) params)))

(defn- object-rid [world ptype]
  (when (and ptype (or (keyword? ptype) (string? ptype)))
    (lookup/->type-rid world :object-types ptype)))

(defn- id-of [v] (str (if (map? v) (:id v) v)))

(defn- object-param-values
  [world read params args]
  (reduce-kv
   (fn [m p v]
     (if-let [rid (object-rid world (get-in params [p :type]))]
       (let [one (fn [x] (let [id (id-of x)]
                           (assoc (or (when (map? x) (:values x))
                                      (:values (x/object read rid id))
                                      {})
                                  :id id ::type rid)))]
         (assoc m p (if (sequential? v) (mapv one v) (one v))))
       (assoc m p v)))
   {} (or args {})))

(defn- objects-shown
  [values]
  (for [v (vals values)
        o (if (sequential? v) v [v])
        :when (and (map? o) (::type o))]
    [(::type o) (:id o)]))

(defn- principal-names
  [rows]
  (mapcat (fn [r] (if (map? r) (keep r [:user :username :group :principal :recipient]) [r]))
          rows))

(defn- recipient-names
  [world ctx io values args recipients]
  (notifications/names
   (mapcat (fn [r]
             (cond
               (not (map? r)) [r]
               (:function r) (principal-names
                              (:rows (action-effects/rule-function
                                      world ctx io (keyword (:function r)) args nil)))
               (:property r) (let [o (get values (keyword (:param r)))]
                               (map #(get % (keyword (:property r))) (if (map? o) [o] o)))
               (:param r)    [(get args (keyword (:param r)))]
               :else         (refuse! :refusal.notification/recipient-shape {:recipient r})))
           recipients)))

(defn- judged!
  [{:keys [allowed denied]} require cap]
  (let [n (+ (count allowed) (count denied))]
    (cond
      (> n cap)
      (refuse! :refusal.notification/too-many-recipients {:recipients n :max cap})

      (and (not= :any require) (seq denied))
      (refuse! :refusal.notification/recipient-may-not-read {:denied denied}
               {:denied-list (vec denied)})

      (and (= :any require) (empty? allowed) (seq denied))
      (refuse! :refusal.notification/no-recipient-may-read {:denied denied}
               {:denied-list (vec denied)})

      :else allowed)))

(defn- function-content
  [row]
  (when (map? row)
    {:heading       (or (:heading row) (:subject row) (:title row))
     :message       (or (:message row) (:body row) (:content row))
     :link-url      (or (:link-url row) (:url row))
     :link-label    (or (:link-label row) (:link-text row))
     :email-subject (:email-subject row)
     :email-html    (or (:email-html row) (:email-body row))}))

(defn- template-content [content values]
  (into {} (keep (fn [k] (when-let [t (get content k)]
                           [k (notifications/render t values)])))
        [:heading :message :email-subject :email-html]))

(defn- link-of
  [world {:keys [label object object-type id] :as link} values bindings]
  (when link
    (let [target (cond
                   object      (let [o (get values (keyword object))]
                                 (when (map? o)
                                   [(get-in world [:object-types (::type o) :object-type-name])
                                    (:id o)]))
                   object-type [object-type (rv/resolve-arg bindings id)])
          url    (notifications/link-url (cond-> (select-keys link [:app :url])
                                           target (assoc :object target))
                                         values)]
      (when url
        {:link-url url :link-label (some-> label (notifications/render values))}))))

(defn- deliveries
  [world ctx io {:keys [content link]} values args bindings recipients]
  (let [shared (link-of world link values bindings)
        actor  (some-> (grants/acting-user (:authority ctx)) name)]
    (vec
     (for [u recipients
           :let [vs (assoc values :recipient {:username u :name u}
                           :current-user {:username actor :name actor})
                 c  (if-let [f (:function content)]
                      (function-content
                       (first (:rows (action-effects/rule-function
                                      world ctx io (keyword f)
                                      (assoc args :recipient u :current-user actor) nil))))
                      (merge (template-content content vs) (link-of world link vs bindings)))]
           :when (and c (seq (:heading c)))]
       (merge shared c {:recipient u})))))

(defn- outcome-event
  [ctx action deliveries request]
  (let [fields {:action action :recipients (mapv :recipient deliveries)}]
    (if-let [{:keys [outcome side-effect]} (x/gate ctx [:notifications {:channel action}])]
      (kernel/event :notification (if (= :planned outcome)
                                    (assoc fields :planned? true)
                                    (assoc fields :side-effect side-effect)))
      (kernel/event :notification (assoc fields :outbox request)))))

(defn notify-verb
  [{:keys [recipients content require pdf] :as step} world bindings ctx io]
  (let [action  (some-> (or (:action-type ctx) (get-in ctx [:action :action-name])) name)
        params  (get-in ctx [:action :params])
        args    (merge (:arg0 bindings) (select-keys bindings (keys params)))
        values  (object-param-values world (x/reader ctx io) params args)
        branch  (str (or (:branch (:effect-gate ctx)) "main"))
        cap     (get notifications/max-recipients (if (:function content) :function :template))
        names   (recipient-names world ctx io values args recipients)
        allowed (judged! (:result (x/ask ctx io {:kind    :notification-audience
                                                 :branch  branch :names names
                                                 :objects (vec (objects-shown values))}))
                         (or require :all) cap)
        ds      (deliveries world ctx io step values args bindings allowed)
        request (cond-> {:kind :notification :branch branch :action action :deliveries ds}
                  pdf (assoc :pdf (update pdf :values #(rv/resolve-bindings bindings (or % {}))))
                  (= :branch-owner (:notification-recipients (:effect-gate ctx)))
                  (assoc :branch-owner? true))]
    {:world        world
     :bindings     (assoc bindings (or (:as step) :notified) (mapv :recipient ds))
     :side-effects (if (seq ds) [(outcome-event ctx action ds request)] [])}))

(defn- perform-audience
  [{:keys [branch names objects]} {:keys [store]}]
  (notifications/admitted store branch names {:objects objects}))

(defn- pdf-attachment
  [store {:keys [template values filename]}]
  (delay
    (let [schema (seat/branch-schema (world/at store "main"))
          spec   (some->> (lookup/rid-of schema :templates (keyword template))
                          (get (:templates schema)))
          out    (when spec (compute/generate spec {:store store :schema schema :values values
                                                    :targets [:pdf]}))]
      (when-let [b64 (:pdf_b64 out)]
        [{:filename     (or filename (str (name template) ".pdf"))
          :content-type "application/pdf"
          :bytes        (.decode (java.util.Base64/getDecoder) ^String b64)}]))))

(defn- redirected
  [store {:keys [branch deliveries branch-owner?]}]
  (if branch-owner?
    (let [owners (map str (governance/owners store branch))]
      (vec (distinct (for [d deliveries o owners] (assoc d :recipient o)))))
    deliveries))

(defn- perform-notification
  [{:keys [branch pdf] :as request} {:keys [store]}]
  (let [ds (redirected store request)]
    {:sent (count (notifications/deliver!
                   store
                   (notifications/plan-each {:source :action :branch branch} ds
                                            (notifications/settings-of
                                             store (map :recipient ds)))
                   (when pdf (pdf-attachment store pdf))))}))

(def module
  {:name        :action-notify
   :contributes {:action/verbs   {:notification notify-verb}
                 :run/performers {:notification          perform-notification
                                  :notification-audience perform-audience}}})
