(ns sutra-ui.apphost.standard-object-view-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.render :as render]
            [sutra-ui.apphost.state :as app-state]
            [sutra-ui.state.auth :as auth]
            [sutra.capps.standard-view]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.core :as ec]))

(def ^:private rid "ri.sutra.main.object-type.5a1d0000-0000-4000-8000-00000000beef")

(def ^:private display
  {:nr     {:prop :nr :type :string :name "Nummer" :order 0 :visibility :prominent}
   :betrag {:prop :betrag :type :decimal :name "Betrag" :order 1
            :format {:kind :currency :currency "EUR" :decimals 2}}
   :intern {:prop :intern :type :string :name "Intern" :order 2 :visibility :hidden}})

(def ^:private descriptor
  {:id "ri.sutra.main.application.standard-demo"
   :name :standard-demo
   :object-names {rid :bestellung}
   :object-titles {rid :nr :bestellung :nr}
   :property-display {rid display :bestellung display}
   :standard-views {rid {:type :bestellung :symbol "📦"
                         :links [{:api :positionen :label "Positionen" :cols [:sku]}]}}
   :layout {:kind :rows
            :items [{:widget :object-view :object {:from :bestellung}}]}})

(def ^:private data
  {:bestellung [{:id "b1" :object-type-ref rid
                 :values {:nr "B-7" :betrag 1180.5 :intern "GEHEIM-9"}}]})

(deftest the-widget-falls-back-to-the-standard-view
  (let [html (rds/render-to-string [app/render-data-app descriptor data])]
    (is (not (str/includes? html "keine Object View"))
        "a type without an .entv still HAS a view — its standard one")
    (testing "the header names the object, with its type's symbol"
      (is (str/includes? html "📦"))
      (is (str/includes? html "B-7")))
    (testing "the properties, with the ontology's names and formats"
      (is (str/includes? html "Nummer"))
      (is (str/includes? html "1.180,50"))
      (is (not (str/includes? html "GEHEIM-9")) "a hidden property is not shown"))
    (is (not (str/includes? html "Positionen"))
        "no payload for the side, so no heading for it — link sections are covered by
         a-full-relationship-shows-its-rows-an-empty-one-stays-away")))

(def ^:private ooux-descriptor
  (-> descriptor
      (assoc-in [:property-display rid :stand] {:prop :stand :type :string :name "Stand" :order 3})
      (assoc-in [:property-display :bestellung :stand]
                {:prop :stand :type :string :name "Stand" :order 3})
      (update-in [:standard-views rid] assoc
                 :subtitle :nr
                 :actions [{:action :bestellung-kommentieren :label "Kommentieren"}
                           {:action :bestellung-freigeben :label "Freigeben"
                            :property :stand :from ["offen"]}
                           {:action :bestellung-stornieren :label "Stornieren"
                            :property :stand :from ["freigegeben"]}])
      (assoc :object-titles {rid :intern :bestellung :intern})))

(def ^:private offen
  {:bestellung [{:id "b1" :object-type-ref rid
                 :values {:nr "B-7" :betrag 1180.5 :intern "Bürostühle" :stand "offen"}}]})

(deftest the-standard-view-reads-like-its-object
  (let [html (rds/render-to-string [app/render-data-app ooux-descriptor offen])]
    (testing "the head: title, then the number (`:subtitle`)"
      (is (str/includes? html "sutra-objhead__subtitle"))
      (is (< (str/index-of html "Bürostühle") (str/index-of html "sutra-objhead__subtitle"))))
    (testing "the Actions offered on the type, a transition only in its :from states"
      (is (str/includes? html "Aktionen"))
      (is (str/includes? html "Kommentieren"))
      (is (str/includes? html "Freigeben") "the object stands in offen — Freigeben can succeed")
      (is (not (str/includes? html "Stornieren")) "Stornieren starts only from freigegeben"))
    (is (not (str/includes? html "Verlauf"))
        "Edit History is a widget a configured view places")))

(deftest actions-the-module-places-are-not-offered-again
  (let [placed (rds/render-to-string
                [app/render-data-app
                 (assoc ooux-descriptor :layout
                        {:kind :rows
                         :items [{:widget :object-view :object {:from :bestellung}}
                                 {:widget :action-button :action :bestellung-kommentieren
                                  :label "HOST-KNOPF"}]})
                 offen])
        hidden (rds/render-to-string
                [app/render-data-app
                 (assoc ooux-descriptor :layout
                        {:kind :rows
                         :items [{:widget :object-view :object {:from :bestellung}
                                  :hide-actions? true}]})
                 offen])]
    (is (str/includes? placed "HOST-KNOPF"))
    (is (not (str/includes? placed "Kommentieren")) "the module shows it itself")
    (is (str/includes? placed "Freigeben") "the others still stand")
    (is (not (str/includes? hidden "Aktionen")) "`:hide-actions?` takes them all out")))

(deftest a-table-without-cols-reads-its-rows-type-row
  (let [html (rds/render-to-string
              [app/render-data-app
               (assoc descriptor
                      :object-rows {rid [:betrag :nr]}
                      :layout {:kind :rows
                               :items [{:widget :object-table :rows {:from :bestellung}}]})
               data])]
    (is (and (str/includes? html "Betrag") (str/includes? html "Nummer"))
        "the columns the type's `:row` names, with the ontology's headers")
    (is (< (str/index-of html "Betrag") (str/index-of html "Nummer")) "in that order")
    (is (not (str/includes? html "GEHEIM-9")) "and nothing the row does not name")))

(deftest a-configured-view-still-wins
  (let [html (rds/render-to-string
              [app/render-data-app
               (assoc descriptor :object-views
                      {rid {:full {:kind :rows
                                   :items [{:widget :markdown :text "HANDGESCHRIEBEN"}]}}})
               data])]
    (is (str/includes? html "HANDGESCHRIEBEN"))
    (is (not (str/includes? html "Positionen"))
        "the standard view is the fallback, never beside the configured one")))

(deftest the-property-list-widget
  (let [html (rds/render-to-string
              [app/render-data-app
               (assoc descriptor :layout
                      {:kind :rows
                       :items [{:widget :property-list :object {:from :bestellung}
                                :props [:betrag {:prop :nr :label "Beleg"}]
                                :label-position :below :columns 2}]})
               data])]
    (is (str/includes? html "capp-proplist--below"))
    (is (str/includes? html "Beleg") "an item's own label wins")
    (is (< (str/index-of html "1.180,50") (str/index-of html "B-7")) "in the order given")
    (is (not (str/includes? html "Positionen")))))

(deftest the-builder-says-where-the-preview-data-comes-from
  (let [tree {:kind :rows :title "Entwurf" :items []}
        at   (fn [src err]
               (swap! es/ui assoc :draft {:app nil :file {:name :entwurf :status :draft :layout tree}
                                 :tree tree :saved nil :data {} :cursor [] :version 1
                                 :past [] :future [] :preview-source src :preview-error err})
               (rds/render-to-static-markup [ec/app-builder]))]
    (is (str/includes? (at :draft nil) "data-source=\"draft\"")
        "a live preview of the EDITED draft says so")
    (let [html (at :published "no edit access to entwurf")]
      (is (str/includes? html "data-source=\"published\""))
      (is (str/includes? html "no edit access to entwurf")
          "a refused draft falls back to the published data and says why"))
    (swap! es/ui assoc :draft {:app nil :file nil :tree nil :cursor [] :version 0
                      :past [] :future [] :data {}})))

(defn- link-html [rows]
  (rds/render-to-string
   [app/render-data-app
    (assoc descriptor :layout
           (sutra.capps.standard-view/node :bestellung
                                           {:links [{:api :positionen :label "Positionen"
                                                     :cols [:sku]}]}
                                           display))
    (assoc offen :link/positionen rows)]))

(deftest a-full-relationship-shows-its-rows-an-empty-one-stays-away
  (let [voll (link-html [{:id "p1" :values {:sku "SKU-42"}}])
        leer (link-html [])]
    (is (str/includes? voll "SKU-42"))
    (is (str/includes? voll "Positionen") "the side that has rows names itself")
    (is (not (str/includes? leer "Positionen"))
        "a side without rows gets no heading — the standard view shows what an object
         hangs on, not the catalogue of edges it could have")))

(deftest when-reads-an-object-set-as-has-any
  (let [h (fn [layout]
            (rds/render-to-string [app/render-data-app (assoc descriptor :layout layout)
                                   (assoc offen
                                          :link/positionen [{:id "p1" :values {:sku "SKU-42"}}])]))]
    (is (str/includes? (h {:kind :rows
                           :items [{:widget :markdown :text "JA" :when {:from :bestellung}}]}) "JA")
        "a set of rows is a vector, not a predicate — before, every such :when was false")
    (is (not (str/includes? (h {:kind :rows
                                :items [{:widget :markdown :text "JA" :when {:from :link/leer}}]})
                            "JA")))))

(deftest a-table-leads-with-the-objects-title
  (let [html (rds/render-to-string
              [app/render-data-app
               (assoc descriptor :layout {:kind :rows
                                          :items [{:widget :object-table
                                                   :rows {:from :bestellung}}]})
               data])]
    (is (str/includes? html "sutra-objlink") "the title is a link to the object")
    (is (str/includes? html "B-7"))
    (is (< (str/index-of html "Bestellung") (str/index-of html "B-7")) "under the type's name")))

(deftest an-empty-table-says-so
  (let [html (rds/render-to-string
              [app/render-data-app
               (assoc descriptor :layout {:kind :rows
                                          :items [{:widget :object-table
                                                   :rows {:from :bestellung}}]})
               {:bestellung []}])]
    (is (str/includes? html "Keine Objekte gefunden"))))

(defn- frame-html
  [h]
  (loop [h h]
    (cond (nil? h) nil
          (#{ui/drawer ui/dialog} (first h))
          (let [{:keys [class title]} (second h)]
            (str class " " title " " (rds/render-to-string (into [:div] (drop 2 h)))))
          (fn? (first h)) (recur (apply (first h) (rest h)))
          :else (rds/render-to-string h))))

(deftest outside-the-builder-an-object-opens-in-a-drawer
  (let [before @auth/auth
        ov     (app-state/overlays)]
    (swap! auth/auth assoc :role "consumer")
    (try
      ((render/object-opener ov) rid "b1")
      (let [html (frame-html
                  (render/object-drawer ov {:descriptor descriptor :overlays ov
                                            :open-object (render/object-opener ov)}
                                        {:descriptor descriptor :handles {} :derived {}
                                         :data* (atom data)}))]
        (is (str/includes? html "capp-drawer"))
        (is (str/includes? html "B-7") "the object the payload holds, read at once")
        (is (not (str/includes? html "Nichts verknüpft"))
            "in its standard view — and a side the drawer has no rows for stays away"))
      (finally (reset! auth/auth before)))))
