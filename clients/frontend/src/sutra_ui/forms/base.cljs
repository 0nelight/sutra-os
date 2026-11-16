(ns sutra-ui.forms.base
  (:require [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.ui.form :as form]
            [sutra.i18n.core :as i18n]))

(defn via
  [v write!]
  (reify
    IDeref (-deref [_] v)
    IReset (-reset! [_ x] (write! x) x)))

(defn named
  [xs]
  (mapv (fn [x] {:value x :label x}) xs))

(def new-object-form (fs/form :new-object))

(def new-link-form (fs/form :new-link))

(def connect-modal-form (fs/form :connect-modal))

(def edit-object-form (fs/form :edit-object))

(def edit-link-form (fs/form :edit-link))

(def new-interface-form (fs/form :new-interface))

(def edit-interface-form (fs/form :edit-interface))

(def new-object-type-group-form (fs/form :new-object-type-group))

(def edit-object-type-group-form (fs/form :edit-object-type-group))

(def entity-views
  {:object-type
   {:icon  {:glyph "▢" :bg "var(--c-primary-bg)" :fg "var(--c-primary)"}
    :label :ui/object-type
    :section-label :object-types}
   :link-type
   {:icon  {:glyph "⇄" :bg "var(--c-success-bg)" :fg "var(--c-success)"}
    :label :ui/link-type
    :section-label :link-types}
   :interface
   {:icon  {:glyph "◇"
            :bg    "color-mix(in srgb, var(--violet3) 12%, var(--c-surface))"
            :fg    "var(--violet3)"}
    :label :ui/kind-interface
    :section-label :interfaces}
   :object-type-group
   {:icon  {:glyph "⊞" :bg "var(--c-success-bg)" :fg "var(--c-success)"}
    :label :ui/object-type-group
    :section-label :object-type-groups}
   :function
   {:icon  {:glyph "▶" :bg "var(--c-warning-bg)" :fg "var(--c-warning)"}
    :label :ui/kind-function
    :section-label :functions}})

(defn implements-chips
  [{:keys [form-atom]}]
  (let [implements (or (:implements @form-atom) [])
        impl-set (set (map (comp keyword name) implements))
        ifaces (sort (keys (:interfaces @catalog/schema)))
        toggle! (fn [iface-kw]
                  (fs/edit! form-atom update :implements
                            (fn [cur]
                              (let [s (set (map (comp keyword name) cur))]
                                (vec (if (contains? s iface-kw)
                                       (disj s iface-kw)
                                       (conj s iface-kw)))))))]
    [form/field {:label (i18n/t :ui/implements)}
     (if (seq ifaces)
       [:div {:class "form-chips"}
        (for [iface ifaces]
          (let [active? (contains? impl-set iface)]
            ^{:key iface}
            [:button
             {:on-click (fn [e]
                          (.preventDefault e)
                          (toggle! iface))
              :class "form-chip"
              :style {:background (if active? "var(--c-primary)" "var(--c-surface)")
                      :color (if active? "var(--c-on-accent)" "var(--c-text-2)")
                      :border (str "1px solid "
                                   (if active? "var(--c-primary)" "var(--c-border)"))}}
             (clojure.core/name iface)]))]
       [:div {:class "form-chips__empty"}
        (i18n/t :ui/interfaces-none-yet)])]))

(defn extends-chips
  [{:keys [form-atom exclude]}]
  (let [extends  (or (:extends @form-atom) [])
        ext-set  (set (map (comp keyword name) extends))
        excl-kw  (when (seq (str exclude)) (keyword exclude))
        ifaces   (->> (keys (:interfaces @catalog/schema))
                      (remove #(= % excl-kw))
                      sort)
        toggle!  (fn [iface-kw]
                   (fs/edit! form-atom update :extends
                             (fn [cur]
                               (let [s (set (map (comp keyword name) cur))]
                                 (vec (if (contains? s iface-kw)
                                        (disj s iface-kw)
                                        (conj s iface-kw)))))))]
    [form/field {:label (i18n/t :ui/extends)}
     (if (seq ifaces)
       [:div {:class "form-chips"}
        (for [iface ifaces]
          (let [active? (contains? ext-set iface)]
            ^{:key iface}
            [:button
             {:on-click (fn [e] (.preventDefault e) (toggle! iface))
              :class "form-chip"
              :style {:background (if active? "var(--c-primary)" "var(--c-surface)")
                      :color (if active? "var(--c-on-accent)" "var(--c-text-2)")
                      :border (str "1px solid " (if active? "var(--c-primary)" "var(--c-border)"))}}
             (clojure.core/name iface)]))]
       [:div {:class "form-chips__empty"}
        (i18n/t :ui/interfaces-none-other)])]))
