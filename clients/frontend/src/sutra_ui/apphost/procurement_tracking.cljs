(ns sutra-ui.apphost.procurement-tracking
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.display :as display]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [input-group]]
            [reagent.core :as r]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.render :as rcr]
            [sutra-ui.apphost.state :as app-state]
            [sutra-ui.apphost.runtime :as rt]
            [sutra-ui.object.card :as oc]))

(def ^:private v      app/v)
(def ^:private ->num  app/calc)
(def ^:private fmt-menge app/menge)
(def ^:private fmt-date  app/datum)

(defn- fmt-eur [x]
  (when (some? x)
    (when-let [n (app/num x)]
      (app/eur n))))

(defn- domain [addr]
  (when (string? addr)
    (some-> (str/split addr #"@") last str/lower-case str/trim)))

(defn- pfad-key [p]
  (mapv #(let [n (js/parseInt % 10)] (if (js/isNaN n) 0 n))
        (str/split (str (or (:positionspfad (:values p)) "")) #"\.")))

(def ^:private status-meta
  {"angefragt"     {:label "Angefragt"     :tone :info   :mod "info"}
   "bestellt"      {:label "Bestellt"      :tone :info   :mod "info"}
   "bestätigt"     {:label "Bestätigt"     :tone :info   :mod "info"}
   "zustellung"    {:label "In Zustellung" :tone :warn   :mod "warn"}
   "teilgeliefert" {:label "Teilgeliefert" :tone :warn   :mod "partial"}
   "abgeschlossen" {:label "Geliefert"     :tone :ok     :mod "ok"}
   "storniert"     {:label "Storniert"     :tone :danger :mod "danger"}})

(def ^:private pos-state-meta
  {:geliefert     {:label "Geliefert"     :tone :ok   :glyph "✅"}
   :teilgeliefert {:label "Teilgeliefert" :tone :info :glyph "🔄"}
   :offen         {:label "Offen"         :tone :warn :glyph "⏳"}})

(defn- pos-state [vv]
  (let [m   (->num (:menge vv))
        gel (->num (:gelieferte-menge vv))]
    (cond
      (or (= (:zeilen-art vv) "titel") (nil? m) (not (pos? m))) :titel
      (or (:vollständig-geliefert? vv) (and gel (>= gel m)))    :geliefert
      (and gel (pos? gel))                                      :teilgeliefert
      :else                                                     :offen)))

(defn- delivered-context? [o]
  (or (contains? #{"zustellung" "teilgeliefert" "abgeschlossen"} (:status o))
      (boolean (some #(when-let [g (->num (:gelieferte-menge (v %)))] (pos? g))
                     (:positionen o)))))

(def ^:private kind-glyph
  {"anfrage"             ["❓" "Anfrage"]
   "angebot"             ["🧮" "Angebot"]
   "bestellung"          ["🧾" "Bestellung"]
   "auftragsbestätigung" ["✅" "Auftragsbestätigung"]
   "versandavis"         ["🚚" "Versandavis"]
   "lieferschein"        ["📦" "Lieferschein"]
   "rechnung"            ["💶" "Rechnung"]
   "gutschrift"          ["↩️" "Gutschrift"]
   "mahnung"             ["⚠️" "Mahnung"]})

(def ^:private default-columns
  [{:key "placed"    :label "Bestellt"      :status #{"angefragt" "bestellt" "bestätigt"}}
   {:key "shipping"  :label "In Zustellung" :status #{"zustellung"}}
   {:key "delivered" :label "Geliefert"     :status #{"teilgeliefert" "abgeschlossen"}}])

(defn- columns-of [descriptor]
  (let [cols (get-in descriptor [:layout :columns])]
    (->> (if (seq cols) cols default-columns)
         (mapv (fn [c] (update c :status set))))))

(defn- index-orders
  [data]
  (let [partners   (get data :geschäftspartner)
        bestellung (get data :bestellung)
        positions  (get data :bestellzeile)
        events     (get data :auftrags-ereignis)
        eingänge   (get data :wareneingang)
        pname      (into {} (map (fn [p] [(:partner-id (v p)) (:name (v p))])) partners)
        pos-by     (group-by #(:bestellnummer (v %)) positions)
        ev-by      (group-by #(:bestellnummer (v %)) events)
        we-by      (group-by #(:bestellnummer (v %)) eingänge)
        order-row  (fn [b]
                     (let [vv    (v b)
                           nr    (:bestellnummer vv)
                           ps    (pos-by nr)
                           evs   (->> (ev-by nr) (sort-by #(:doc-date (v %))))
                           offen (->> ps (filter (fn [p]
                                                   (when-let [om (->num (:offene-menge (v p)))]
                                                     (pos? om)))))
                           track (->> evs
                                      (filter #(= "versandavis" (:kind (v %))))
                                      (keep (fn [e]
                                              (when-let [t (:tracking-nr (v e))]
                                                {:tracking t
                                                 :carrier
                                                   (get pname (domain (:von-adresse (v e))))}))))]
                       {:id          (:id b)
                        :row         b
                        :partner-row (some #(when (= (:partner-id (v %)) (:partner-id vv)) %)
                                           partners)
                        :nr          nr
                        :titel       (:titel vv)
                        :status      (:status vv)
                        :partner     (or (get pname (:partner-id vv)) (:partner-id vv))
                        :bestelldatum (:bestelldatum vv)
                        :summe-brutto (:summe-brutto vv)
                        :summe-netto  (:summe-netto vv)
                        :komm-codes  (->> ps (keep #(:komm-code (v %))) distinct vec)
                        :positionen  ps
                        :offene      offen
                        :ereignisse  evs
                        :lieferungen (we-by nr)
                        :tracking    (vec track)}))]
    (mapv order-row bestellung)))

(defn- kommission-rows [data orders]
  (let [orders-for (fn [code] (filterv #(some #{code} (:komm-codes %)) orders))]
    (->> (get data :kommission)
         (mapv (fn [k]
                 (let [vv   (v k)
                       code (:komm-code vv)]
                   {:id      (:id k)
                    :code    code
                    :name    (:name vv)
                    :kunde   (:kunde vv)
                    :summe   (:summe-netto vv)
                    :orders  (orders-for code)})))
         (sort-by :code)
         vec)))

(defn- status-badge [status]
  (let [{:keys [label tone]} (get status-meta status {:label (or status "—") :tone :info})]
    [display/badge tone label]))

(defn- event-timeline [evs]
  [:div {:class "pt-timeline"}
   (for [[i e] (map-indexed vector evs)
         :let [k (:kind (v e))
               [glyph lbl] (get kind-glyph k ["•" k])
               dt (fmt-date (:doc-date (v e)))]]
     ^{:key i}
     [:span {:class "pt-timeline__dot" :title (str lbl (when dt (str " · " dt)))}
      glyph])])

(defn- order-card [on-select o]
  [:div {:class "pt-order pt-order--click" :role "button" :tabIndex 0
         :title "Positionen anzeigen"
         :on-click #(on-select o)
         :on-key-down #(when (contains? #{"Enter" " "} (.-key %))
                         (.preventDefault %) (on-select o))}
   [:div {:class "pt-order__head"}
    [:span {:class "pt-order__nr"} (:nr o)]
    [status-badge (:status o)]]
   [:div {:class "pt-order__supplier"} "🏭 " (:partner o)]
   (when-let [trs (seq (:tracking o))]
     [:div {:class "pt-order__track"}
      (for [[i t] (map-indexed vector trs)]
        ^{:key i}
        [:div {:class "pt-track"}
         "🚚 " (when (:carrier t) [:span {:class "pt-track__carrier"} (:carrier t) " · "])
         [:span {:class "pt-track__nr"} (:tracking t)]])])
   (when (seq (:offene o))
     [:div {:class "pt-order__open"}
      (str "⏳ " (count (:offene o)) " offene Position"
           (when (> (count (:offene o)) 1) "en"))
      [:ul {:class "pt-open-list"}
       (for [[i p] (map-indexed vector (:offene o))]
         ^{:key i}
         [:li (str (fmt-menge (:offene-menge (v p))) " " (:einheit (v p))
                   " offen · " (:beschreibung (v p)))])]])
   [:div {:class "pt-order__foot"}
    (when-let [d (fmt-date (:bestelldatum o))] [:span {:class "pt-order__date"} d])
    (when-let [b (fmt-eur (:summe-brutto o))] [:span {:class "pt-order__sum"} b])]
   (when (seq (:ereignisse o)) [event-timeline (:ereignisse o)])])

(defn- kommission-card [columns on-select k]
  (let [orders (:orders k)
        bucket (fn [{:keys [status]}]
                 (fn [o] (contains? status (:status o))))
        matched (set (mapcat (fn [c] (filter (bucket c) orders)) columns))
        others  (remove matched orders)]
    [:div {:class "pt-komm"}
     [:div {:class "pt-komm__head"}
      [:div {:class "pt-komm__id"}
       [:span {:class "pt-komm__code"} "🏷️ " (:code k)]
       (when (and (:name k) (not= (:name k) (:code k)))
         [:span {:class "pt-komm__name"} (:name k)])]
      [:div {:class "pt-komm__meta"}
       (when (:kunde k) [:span {:class "pt-komm__kunde"} "👤 " (:kunde k)])
       [:span {:class "pt-komm__count"} (count orders) " Bestellung"
        (when (not= 1 (count orders)) "en")]
       (when-let [s (fmt-eur (:summe k))] [:span {:class "pt-komm__sum"} s])]]
     [:div {:class "pt-board"}
      (for [c columns
            :let [os (filter (bucket c) orders)]]
        ^{:key (:key c)}
        [:div {:class "pt-col"}
         [:div {:class "pt-col__head"} (:label c)
          [:span {:class "pt-col__n"} (count os)]]
         [:div {:class "pt-col__body"}
          (if (seq os)
            (for [o os] ^{:key (:id o)} [order-card on-select o])
            [:div {:class "pt-col__empty"} "—"])]])
      (when (seq others)
        [:div {:class "pt-col pt-col--other"}
         [:div {:class "pt-col__head"} "Sonstige"
          [:span {:class "pt-col__n"} (count others)]]
         [:div {:class "pt-col__body"}
          (for [o others] ^{:key (:id o)} [order-card on-select o])]])]]))

(defn- pos-status-badge [status]
  (let [{:keys [label tone glyph]} (pos-state-meta status)]
    [display/badge tone (str glyph " " label)]))

(defn- link-to
  [descriptor on-open row text]
  (if row
    [oc/object-link (:object-type-ref row) (:id row)
     {:row row :on-open on-open
      :meta (oc/descriptor-meta descriptor (:object-type-ref row))}]
    text))

(defn- order-modal [descriptor data on-open o on-close]
  (let [show-del? (delivered-context? o)
        ps        (sort-by pfad-key (:positionen o))
        komms     (filter #(some #{(:komm-code (v %))} (:komm-codes o)) (get data :kommission))]
    [overlay/modal
     {:title    [:span {:class "pt-modal__title"}
                 [link-to descriptor on-open (:row o) (:nr o)]
                 " " [status-badge (:status o)]]
      :on-close on-close
      :size     :lg
      :foot     (when show-del?
                  (str (count (filter #(= :geliefert (pos-state (v %))) ps)) " von "
                       (count (remove #(= :titel (pos-state (v %))) ps))
                       " Positionen vollständig geliefert"))}
     [:<>
      [:div {:class "pt-modal__sub"}
       [:span "🏭 " [link-to descriptor on-open (:partner-row o) (:partner o)]]
       (for [k komms]
         ^{:key (:id k)} [:span "🏷️ " [link-to descriptor on-open k (:komm-code (v k))]])
       (when-let [t (:titel o)] [:span t])
       (when-let [d (fmt-date (:bestelldatum o))] [:span "📅 " d])
       (when-let [b (fmt-eur (:summe-brutto o))] [:span {:class "pt-modal__sum"} b])]
      [:div {:class "pt-modal__body"}
       (if (seq ps)
         [:table {:class "pt-postable"}
          [:thead
           [:tr
            [:th "Pos"] [:th "Beschreibung"]
            [:th {:class "pt-num"} "Bestellt"] [:th {:class "pt-num"} "Einzelpreis"]
            (when show-del?
              [:<> [:th {:class "pt-num"} "Geliefert"]
               [:th {:class "pt-num"} "Offen"] [:th "Status"]])]]
          [:tbody
           (for [[i p] (map-indexed vector ps)
                 :let [vv     (v p)
                       status     (pos-state vv)
                       titel? (= status :titel)
                       m      (->num (:menge vv))
                       gel    (->num (:gelieferte-menge vv))
                       offen  (->num (:offene-menge vv))]]
             ^{:key i}
             [:tr {:class (str "pt-porow" (when titel? " pt-porow--titel"))}
              [:td {:class "pt-porow__pfad"} (:positionspfad vv)]
              [:td (:beschreibung vv)]
              [:td {:class "pt-num"} (when (and m (pos? m)) (str (fmt-menge m) " " (:einheit vv)))]
              [:td {:class "pt-num"} (fmt-eur (:einzelpreis vv))]
              (when show-del?
                (if titel?
                  [:<> [:td] [:td] [:td]]
                  [:<>
                   [:td {:class "pt-num"} (when gel (fmt-menge gel))]
                   [:td {:class (str "pt-num" (when (and offen (pos? offen)) " pt-num--open"))}
                    (when offen (fmt-menge offen))]
                   [:td [pos-status-badge status]]]))])]]
         [:div {:class "pt-modal__empty"} "Keine Positionsdaten vorhanden."])]]]))

(defn- dashboard [_descriptor _data]
  (let [flt      (r/atom "")
        selected (r/atom nil)
        ov       (app-state/overlays)]
    (fn [descriptor data]
      (let [columns (columns-of descriptor)
            orders  (index-orders data)
            komms   (kommission-rows data orders)
            q       (str/lower-case (str/trim @flt))
            shown   (if (str/blank? q)
                      komms
                      (filterv (fn [k]
                                 (some #(and % (str/includes? (str/lower-case (str %)) q))
                                       [(:code k) (:name k) (:kunde k)]))
                               komms))
            n-orders (count orders)
            by-status (frequencies (map :status orders))]
        [:div {:class "pt"}
         [:div {:class "pt__bar"}
          [:div {:class "pt__title"}
           (or (get-in descriptor [:layout :title])
               (some-> (:name descriptor) name)
               "Beschaffung")]
          [:div {:class "pt__stats"}
           [:span {:class "pt__stat"} (count komms) " Kommissionen"]
           [:span {:class "pt__stat"} n-orders " Bestellungen"]
           (for [[status lbl] [["zustellung" "unterwegs"] ["teilgeliefert" "teilgeliefert"]
                           ["abgeschlossen" "geliefert"]]
                 :let [c (get by-status status 0)]
                 :when (pos? c)]
             ^{:key status} [:span
                             {:class (str "pt__stat pt__stat--" (get-in status-meta [status :mod]))}
                         c " " lbl])]
          [input-group {:class "pt__filter" :placeholder "Kommission filtern…"
                        :value @flt :on-change #(reset! flt (.. % -target -value))}]]
         (if (seq shown)
           [:div {:class "pt__list"}
            (for [k shown]
              ^{:key (:id k)}
              [kommission-card columns #(reset! selected (:id %)) k])]
           [ui/empty-state {:icon :search
                            :title "Keine Kommissionen gefunden."
                            :class "pt__empty"}])
         (when-let [sid @selected]
           (when-let [o (first (filter #(= sid (:id %)) orders))]
             [order-modal descriptor data
              (fn [t id] (reset! selected nil) ((rcr/object-opener ov) t id))
              o #(reset! selected nil)]))
         [rcr/object-drawer ov
          {:descriptor descriptor :overlays ov :open-object (rcr/object-opener ov)}
          {:descriptor descriptor :handles {} :derived {} :data* (atom data)}]]))))

(defmethod rt/render-app :procurement-tracking [descriptor data]
  [dashboard descriptor data])
