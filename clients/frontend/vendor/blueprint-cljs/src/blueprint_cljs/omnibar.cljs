(ns blueprint-cljs.omnibar
  "Omnibar (Blueprint select package): a search box with its result list in an overlay
   over the page. Opening it is the caller's business (`:is-open`, e.g. from a hotkey).
   Props: everything `query-list` takes, plus `:is-open`, `:on-close (fn [event])`,
   `:input-props`, `:overlay-props`; `:initial-content` defaults to nil (nothing is
   listed while the query is empty)."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.input-group :as input-group]
            [blueprint-cljs.overlay :as overlay]
            [blueprint-cljs.query-list :as ql]))

(def ^:private own-keys [:is-open :on-close :input-props :overlay-props])

(defn omnibar
  [_]
  (let [cur   (atom nil)
        props (fn [] @cur)]
    (letfn [(handle-close [e]
              (let [p (props)]
                (when-let [f (get-in p [:overlay-props :on-close])] (f e))
                (when-let [f (:on-close p)] (f e))))
            (render-query-list [lp]
              (let [{:keys [input-props is-open overlay-props]} (props)
                    handlers (when is-open {:on-key-down (:handle-key-down lp)
                                            :on-key-up   (:handle-key-up lp)})]
                [overlay/overlay
                 (merge {:has-backdrop true}
                        overlay-props
                        {:is-open  (boolean is-open)
                         :class    (bp/cx (bp/cls "omnibar-overlay") (:class overlay-props))
                         :on-close handle-close})
                 [:div (merge {:class (bp/cx (bp/cls "omnibar") (:class lp))} handlers)
                  [input-group/input-group
                   (merge {:auto-focus  true
                           :left-icon   :search
                           :placeholder "Search..."
                           :size        :large}
                          input-props
                          {:on-change (:handle-query-change lp)
                           :value     (:query lp)})]
                  (:item-list lp)]]))]
      (fn [p]
        (reset! cur p)
        [ql/query-list (merge {:initial-content nil}
                              (apply dissoc p own-keys)
                              {::outer     p
                               :menu-props {:role "menu"}
                               :renderer   render-query-list})]))))
