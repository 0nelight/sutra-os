(ns sutra-ui.apps.ontology.panel.search
  (:require [clojure.string :as str]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.forms.interface :as iface-forms]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.forms.lists :as entity-lists]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.object.card :as oc]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.apps.ontology.panel.common :refer [section-shell]]
            [sutra-ui.apps.ontology.panel.detail :refer [function-label open-function-detail!]]))

(defn- hit-in
  [q fields]
  (some (fn [[k txt]] (when (and (seq txt) (str/includes? (str/lower-case txt) q)) k)) fields))

(defn search-hits
  [q]
  (let [q (str/lower-case (str/trim (str q)))]
    (when (seq q)
      (let [sch   @catalog/schema
            canon @catalog/canonical-schema
            hit   (fn [kind icon label api field open]
                    (when field
                      {:kind kind :icon icon :label label :api api :field field :open open}))]
        (->> (concat
              (for [[k ot] (sort-by (comp oc/type-label key) (:object-types sch))
                    :let [m (oc/type-meta k)]]
                (hit :ui/object-type :cube (oc/type-label m) (name k)
                     (hit-in q
                             [[:ui/ot-display-name (:display-name m)] [:ui/plural-name (:plural m)]
                              [:ui/api-name (name k)] [:ui/ot-description (str (:description ot))]])
                     #(object-forms/open-object-detail! k)))
              (for [[k ot] (sort-by (comp oc/type-label key) (:object-types sch))
                    fld (:fields ot)
                    :let [p (some-> (:name fld) name)]
                    :when p]
                (hit :ui/om-property :property (entity-lists/property-label k p) p
                     (hit-in q [[:ui/ot-display-name (get-in fld [:opts :name])] [:ui/api-name p]])
                     #(object-forms/open-object-detail! k)))
              (for [c (sort-by (comp name :name) (:links sch))]
                (hit :ui/link-type :link (or (not-empty (:forward-name c)) (name (:name c)))
                     (name (:name c))
                     (hit-in q
                             [[:ui/om-link-side (:forward-name c)]
                              [:ui/om-link-side (:reverse-name c)]
                              [:ui/api-name (name (:name c))]])
                     #(link-forms/open-link-detail! (catalog/link-id c))))
              (for [[k spec] (catalog/declared-actions)]
                (hit :ui/action-type :flash (or (not-empty (:display-name spec)) (name k)) (name k)
                     (hit-in q [[:ui/ot-display-name (:display-name spec)] [:ui/api-name (name k)]])
                     #(catalog/open-resource! :actions k)))
              (for [[k _] (:shared-properties canon)]
                (hit :ui/shared-properties :properties (name k) (name k)
                     (hit-in q [[:ui/api-name (name k)]])
                     #(ont-model/edit! [] assoc :section :shared-properties
                             :detail nil)))
              (for [[k iface] (:interfaces sch)]
                (hit :ui/kind-interface :layers (or (not-empty (:display-name iface)) (name k))
                     (name k)
                     (hit-in q
                             [[:ui/ot-display-name (:display-name iface)] [:ui/api-name (name k)]])
                     #(iface-forms/open-interface-detail! k)))
              (for [[k spec] (:functions canon)]
                (hit :ui/kind-function :function (function-label k spec) (name k)
                     (hit-in q
                             [[:ui/ot-display-name (function-label k spec)]
                              [:ui/api-name (name k)]])
                     #(open-function-detail! k spec))))
             (remove nil?)
             (take 100)
             vec)))))

(defn- go! [h] (ont-model/edit! [] assoc :search nil) ((:open h)))

(defn search-keys
  [^js e draft]
  (let [{q :search i :search-idx} @ont-model/ui
        hits (search-hits q)
        i    (or i 0)]
    (case (.-key e)
      "ArrowDown" (do (.preventDefault e)
                      (ont-model/edit! [] assoc :search-idx (min (dec (count hits)) (inc i))))
      "ArrowUp"   (do (.preventDefault e)
                      (ont-model/edit! [] assoc :search-idx (max 0 (dec i))))
      "Enter"     (when-let [h (and (= draft q) (nth hits i nil))]
                    (.preventDefault e)
                    (go! h))
      "Escape"    (ont-model/edit! [] assoc :search nil)
      nil)))

(defn search-results [q]
  (let [hits (search-hits q)
        lit  (or (:search-idx @ont-model/ui) 0)]
    [section-shell
     [:div
      [:div {:class "ontology-list__header"} (i18n/t :ui/om-search-results {:n (count hits) :q q})]
      (if (empty? hits)
        [:div {:class "u-text-empty"} (i18n/t :ui/om-search-none)]
        (into [ui/menu]
              (for [[i {:keys [kind icon label api field] :as h}] (map-indexed vector hits)
                    :let [where (i18n/t :ui/om-search-hit-in {:field (i18n/t field)})]]
                ^{:key (str i api)}
                [ui/menu-item
                 {:icon      icon
                  :active    (= i lit)
                  :multiline true
                  :on-click  #(go! h)
                  :label     [:span {:class "u-mono-12 u-muted"} api]
                  :text      [:<> [:div label]
                              [:div {:class "sutra-menu__sub"}
                               (str (i18n/t kind) " · " where)]]}])))]]))
