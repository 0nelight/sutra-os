(ns sutra-ui.apphost.events
  (:require [clojure.string :as str]
            [sutra.capps.bind :as bind]
            [sutra.capps.variables :as vars]
            [sutra.capps.warn :as warn]
            [sutra-ui.apphost.toast :as toast]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.state :as state]
            [sutra-ui.apphost.view-state :as view-state]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.api :as api]))

(defn effect-value
  [x row]
  (vars/resolved x (bind/resolve x (vars/fill-scope :row row))))

(defn- row-type [e row] (or (:type e) (:object-type-ref row) (:object-type row)))

(defn- type-token
  [e row]
  (let [t (row-type e row)
        n (catalog/name-of t)]
    (cond (keyword? n) n
          (some? n)    (keyword (str n))
          :else        t)))

(defn- object-of
  [e row]
  (when (contains? e :object)
    (let [x (effect-value (:object e) row)]
      (cond (and (map? x) (some? (:id x))) x
            (sequential? x)                (first x)))))

(defn- open-row-object!
  [e row {:keys [open-object]}]
  (let [t   (type-token e row)
        raw (effect-value (if (contains? e :id) (:id e) {:row :id}) row)
        id  (when-not (coll? raw) (some-> raw str str/trim not-empty))]
    (cond
      (not (and t id))
      (warn/warn! (str "app/open-object: kein Objekt — Typ " (pr-str t) ", Id " (pr-str raw)
                       ". Ohne :type/:id nimmt der Effekt die geklickte Zeile, und die"
                       " nennt hier keines."))

      (object-view/object-view-surface?)
      (shell/open-object! t id {:new-tab? (boolean (:new-tab? e))})

      open-object
      (open-object (row-type e row) id)

      :else
      (warn/warn! (str "app/open-object: diese Sitzung hat keine Objekt-Seite und"
                       " diese Ansicht keine Objekt-Schublade — /object/" (name t) "/" id)))))

(defn- open-object!
  [e row ctx]
  (if (contains? e :object)
    (if-let [o (object-of e row)]
      (open-row-object! (dissoc e :object :id) o ctx)
      (warn/warn! (str "app/open-object: " (pr-str (:object e)) " hält kein Objekt")))
    (open-row-object! e row ctx)))

(defn- open-app!
  [e row]
  (let [app (:app e)
        q   (into {} (keep (fn [[ext x]]
                             (let [val (effect-value x row)]
                               (when-let [s (and (some? val) (view-state/encode-value val))]
                                 [(name ext) s]))))
                  (:with e))]
    (if-let [card (pstate/consumer-app-of-name app)]
      (do (view-state/handover! q)
          (pstate/open-consumer-app! card))
      (do (toast/toast! (str "„" app "“ ist für Sie nicht freigegeben") {:error? true})
          (warn/warn! (str "app/open-app: keine Anwendung namens " (pr-str app)
                           " — oder sie ist für diesen Betrachter nicht freigegeben"))))))

(defn- object-id [x] (str (if (map? x) (:id x) x)))

(defn explorer-query
  [e row]
  (let [ids (effect-value (:ids e) row)]
    (if (some? (:ids e))
      {:ids (mapv object-id (if (coll? ids) ids (some-> ids vector)))}
      (let [where (into {} (keep (fn [[k x]]
                                   (let [val (effect-value x row)]
                                     (when-not (or (nil? val) (and (string? val)
                                                                   (str/blank? val)))
                                       [k {:= val}]))))
                        (:where e))]
        (cond-> {} (seq where) (assoc :where where))))))

(defn- open-explorer!
  [e row]
  (let [t (type-token e row)]
    (cond
      (nil? t)
      (warn/warn! "app/open-explorer: kein Objekttyp — :type fehlt und die Zeile nennt keinen")
      (feature/command :open-object-set)
      (feature/command! :open-object-set
                        {:kind :object-set :object-type t :query (explorer-query e row)})
      :else
      (warn/warn! "app/open-explorer: diese Sitzung hat keinen Explorer"))))

(defn- open-document!
  [e row]
  (let [doc (some-> (effect-value (:document e) row) str str/trim not-empty)]
    (cond
      (nil? doc) (warn/warn! "app/open-document: kein Dokument")
      (feature/command :open-document) (feature/command! :open-document doc)
      :else (warn/warn! "app/open-document: diese Sitzung hat kein Notebook"))))

(defn blocked-app
  [effects]
  (some (fn [e] (when (and (map? e) (= :open-app (:do e))
                           (not (pstate/app-reachable? (:app e))))
                  (str (:app e))))
        (cond (map? effects) [effects] (sequential? effects) effects)))

(defn- put! [h v & [why]] (if (satisfies? IReset h) (reset! h v) (some-> why warn/warn!)))

(doseq [id [::set ::open ::close]]
  (machine/reg-event id (fn [_ [_ & call]] {:fx [[:invoke call]]})))

(defn- act!
  [f id g & args]
  (if f (frame/send! f (into [id g] args)) (apply g args)))

(defn- ask!
  [f ask on-ok on-error]
  (let [req (assoc (frame/kept on-ok on-error) :ask ask)]
    (if f (frame/request! f req) (db/send! [:rf/request req]))))

(defn- document-body [e row]
  (let [val #(effect-value % row)
        typ (row-type e row)
        id  (val (:link e))]
    (cond-> {:template (:template e)
             :values   (into {} (map (fn [[k x]] [k (val x)])) (or (:values e) {}))}
      (some? (:name e))    (assoc :name (str (val (:name e))))
      (and typ (some? id)) (assoc :link {:object-type typ :id id})
      (:save? e)           (assoc :save true)
      (some? (:media e))   (assoc :media true))))

(defn- document!
  [e row {:keys [descriptor frame]}]
  (if-not descriptor
    (warn/warn! "app/document: diese Ansicht kennt keine Anwendung — nichts zu rendern")
    (ask! frame [client/document! descriptor (document-body e row)]
          (fn [{:keys [url media]}]
            (when (and (:media e) media)
              (put! (:media e) media "app/document: :media ist keine setzbare Variable"))
            (put! (:var e) url (str "app/document: der Beleg ist erzeugt, aber :var ist"
                                    " keine setzbare Variable — niemand kann ihn zeigen")))
          #(warn/warn! (str "app/document: " %)))))

(defn- bytes-of [url] (.then (api/fetch-url url) #(.arrayBuffer ^js %)))

(defn- mail!
  [e row {:keys [descriptor frame]}]
  (let [val     #(effect-value % row)
        desktop (some-> js/window .-sutraDesktop)
        fail    #(toast/toast! (str "E-Mail-Entwurf: " %) {:error? true})
        text    #(str (or (val (% e)) ""))
        compose (fn [buf]
                  (.then (.compose ^js desktop
                                   #js {:to       (text :to)
                                        :subject  (text :subject)
                                        :text     (text :text)
                                        :from     (some-> (val (:from e)) str)
                                        :filename (str (or (val (:filename e)) "beleg.pdf"))
                                        :pdf      (js/Uint8Array. buf)})
                         #(toast/toast! (if (str/blank? (text :to))
                                          "Entwurf in Thunderbird geöffnet — Empfänger eintragen"
                                          "Entwurf in Thunderbird geöffnet"))
                         #(fail (or (.-message %) (str %)))))
        send    (fn [url] (ask! frame [bytes-of url] compose fail))
        url     (val (:attach e))]
    (cond
      (not desktop)
      (fail "nur im Desktop-Client — eine Webseite kann Thunderbird nicht öffnen")
      (not (str/blank? (str url))) (send url)
      (and (:template e) descriptor)
      (ask! frame [client/document! descriptor (document-body e row)]
            (fn [{:keys [url]}]
              (put! (:var e) url)
              (send url))
            fail)
      :else (fail "noch kein PDF"))))

(defn run-effects!
  ([effects] (run-effects! effects nil nil))
  ([effects row ctx]
   (let [ov (:overlays ctx)
         f  (:frame ctx)]
     (doseq [e (if (map? effects) [effects] effects)]
       (case (:do e)
         (:set :navigate) (act! f ::set put! (:var e) (effect-value (:to e) row))
         :open-object (open-object! e row ctx)
         :open-app    (open-app! e row)
         :close-app   (pstate/close-app-back!)
         :open-explorer (open-explorer! e row)
         :open-document (open-document! e row)
         :open  (when ov
                  (act! f ::open state/open! ov (:overlay e) (effect-value (:arg e) row)))
         :close (when ov (act! f ::close state/close! ov (:overlay e)))
         :document (document! e row ctx)
         :mail     (mail! e row ctx)
         :refresh-data (client/refresh-page!)
         :recompute (if-let [g (:recompute! (meta (:var e)))]
                      (g)
                      (warn/warn! "app/recompute: :var rechnet nicht auf dem Server"))
         :enable-auto-refresh  (client/auto-refresh! true)
         :disable-auto-refresh (client/auto-refresh! false)
         nil)))))

(defn on-click-fn
  [on-click ctx]
  (cond
    (nil? on-click) nil
    (fn? on-click)  on-click
    :else           (fn [row] (run-effects! on-click row ctx))))
