(ns sutra-ui.apphost.widgets.custom
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.sel :as sel]
            [sutra-ui.storage :as storage]
            [cljs.reader :as reader]
            [sutra-ui.ui.busy :as busy]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.custom-widget :as cw]
            [sutra.capps.variables :refer [deref*]]
            [sutra.i18n.core :as i18n]
            [sutra.kit.size :as g]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.ui.theme :as theme]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as fr]))

(def ^:private dev-poll-ms 1500)

(defn- storage-key [set-name widget-id] (str "sutra.widget/" set-name "/" (name widget-id)))

(defn- stored-values
  [set-name widget-id]
  (or (storage/get-edn (storage-key set-name widget-id)) {}))

(defn- store!
  [set-name widget-id k v]
  (storage/set-edn! (storage-key set-name widget-id)
                    (assoc (stored-values set-name widget-id) k v)))

(defn- fill? [opts] (contains? #{:fill "fill"} (:height opts)))

(defn frame
  [_opts]
  (let [el     (atom nil)
        cur    (atom nil)
        height (r/atom nil)
        ready? (atom false)
        last*  (atom ::nothing)
        doc*   (atom nil)
        loads  (atom 0)
        klasse (atom nil)
        obs    (atom nil)
        own?   (fn [] (<= @loads 1))
        post!  (fn [msg]
                 (when (own?)
                   (some-> ^js @el .-contentWindow (.postMessage (pr-str msg) "*"))))
        push!  (fn []
                 (let [vs (cw/frame-values (:values @cur))]
                   (when (and @ready? (not= vs @last*))
                     (reset! last* vs)
                     (post! [:values vs]))))
        init!  (fn []
                 (let [{:keys [bundle widget-id values]} @cur
                       vs (cw/frame-values values)]
                   (reset! ready? true)
                   (reset! last* vs)
                   (post! [:init {:files   (:files bundle)
                                  :widget  (get-in bundle [:manifest :widgets widget-id])
                                  :values  vs
                                  :stored  (stored-values (:set-name bundle) widget-id)
                                  :groesse @klasse
                                  :thema   (if (theme/dark?) :dark :light)
                                  :tokens  (groesse/tokens)}])))
        measure! (fn [^js entries]
                   (let [w (.. (aget entries 0) -contentRect -width)
                         k (g/klasse w)]
                     (when (and (pos? w) (not= k @klasse))
                       (reset! klasse k)
                       (when @ready? (post! [:groesse k])))))
        rpc!   (fn [id op spec]
                 (-> ((:rpc @cur) op spec)
                     (.then #(post! [:reply id true %]))
                     (.catch #(post! [:reply id false (str %)]))))
        handle (fn [[op a b c]]
                 (let [{:keys [bundle widget-id on-emit]} @cur]
                   (case op
                     :ready  (init!)
                     :emit   (when on-emit (on-emit a b))
                     :rpc    (rpc! a b c)
                     :store  (store! (:set-name bundle) widget-id a b)
                     :height (reset! height a)
                     :error  (js/console.error "custom widget:" a)
                     nil)))
        listen (fn [^js e]
                 (when (and (own?)
                            (some-> ^js @el .-contentWindow (identical? (.-source e)))
                            (= "null" (.-origin e))
                            (string? (.-data e)))
                   (try
                     (handle (reader/read-string (.-data e)))
                     (catch :default err
                       (js/console.error "custom widget: unreadable message" err)))))]
    (r/create-class
     {:component-did-mount    (fn [_]
                                (.addEventListener js/window "message" listen)
                                (when-let [e @el]
                                  (reset! klasse (g/klasse (.-clientWidth e)))
                                  (reset! obs (doto (js/ResizeObserver. measure!) (.observe e)))))
      :component-did-update   (fn [_ _] (push!))
      :component-will-unmount (fn [_]
                                (some-> ^js @obs .disconnect)
                                (.removeEventListener js/window "message" listen))
      :reagent-render
      (fn [{:keys [bundle title allow] :as opts}]
        (reset! cur opts)
        (let [doc (cw/frame-document (.. js/window -location -origin) (:manifest bundle))]
          (when (not= doc @doc*)
            (reset! doc* doc)
            (reset! loads 0))
          [:iframe (cond-> {:ref     #(reset! el %)
                            :class   (str "capp-frame capp-custom-widget"
                                          (when (fill? opts) " capp-frame--fill"))
                            :on-load #(swap! loads inc)
                            :srcDoc  doc
                            :sandbox cw/sandbox
                            :title   (or title (:set-name bundle))
                            :style   {:width  "100%"
                                      :height (when-not (fill? opts)
                                                (str (or (:height opts) @height 120) "px"))
                                      :border "none"
                                      :display "block"}}
                     allow (assoc :allow allow))]))})))

(defn- app-rpc
  [{:keys [descriptor refresh] :as env}]
  (fn [op spec]
    (js/Promise.
     (fn [ok fail]
       (let [ask  (case op
                    :objects   [client/objects! descriptor spec]
                    :search    [client/search! descriptor spec]
                    :aggregate [client/aggregate! descriptor spec]
                    :function  [client/function! descriptor spec]
                    :submit    [client/submit-strict! descriptor (:parameters spec)
                                {:action (:action spec)}]
                    nil)
             done (if (= :submit op)
                    (fn [d] (when (and refresh d) (refresh d)) (ok nil))
                    ok)]
         (if ask
           (fr/request! (fr/frame-for env) (assoc (fr/kept done fail) :ask ask))
           (fail (str "unknown request " (pr-str op)))))))))

(defn- run-effect!
  [[op k v] {:keys [variables events]} {:keys [event-fn]}]
  (case op
    :set   (let [target (get variables k)]
             (if (satisfies? IReset target)
               (sel/wr! target v)
               (js/console.error "custom widget: parameter" (name k)
                                 "is bound by value and cannot be written — bind {:var …}")))
    :event (when-let [f (event-fn (get events k))] (f nil))
    nil))

(defn- chosen-widget
  [manifest widget-name]
  (let [widgets (:widgets manifest)
        k       (some-> widget-name name keyword)]
    (cond
      k                     (when (contains? widgets k) k)
      (= 1 (count widgets)) (key (first widgets)))))

(defn- shown
  [{:keys [variables widget-name height allow title] :as cfg} env bundle]
  (let [id (chosen-widget (:manifest bundle) widget-name)]
    (if-not id
      [ui/problem (i18n/t :ui/capp-custom-widget-choose)
       (str/join ", " (map name (keys (get-in bundle [:manifest :widgets]))))]
      (let [widget   (get-in bundle [:manifest :widgets id])
            problems (cw/binding-problems widget cfg)]
        (if (seq problems)
          [ui/problem (i18n/t :ui/capp-custom-widget-binding) (str/join "; " problems)]
          [frame {:bundle bundle :widget-id id :height height :allow allow :title title
                  :values (update-vals (or variables {}) deref*)
                  :rpc    (app-rpc env)
                  :on-emit
                  (fn [event updates]
                    (let [{:keys [refused effects]} (cw/emit-effects widget cfg event updates)]
                      (if refused
                        (js/console.error "custom widget:" refused)
                        (doseq [e effects] (run-effect! e cfg env)))))}])))))

(defn- loaded
  [{:keys [widget-set version]} {:keys [descriptor] :as env}]
  (let [state  (r/atom {:loading? true})
        timer  (atom nil)
        load!  (fn []
                 (fr/request!
                  (fr/frame-for env)
                  (assoc (fr/kept (fn [b]
                                       (when (not= (:stamp b) (get-in @state [:bundle :stamp]))
                                         (reset! state {:bundle b})))
                                     (fn [m] (reset! state {:error m})))
                         :ask [client/widget-set! descriptor (name widget-set) version]
                         :dedup [::widget-set widget-set version])))]
    (load!)
    (reset! timer (js/setInterval
                   #(when (get-in @state [:bundle :dev?]) (load!))
                   dev-poll-ms))
    (r/create-class
     {:component-will-unmount (fn [_] (js/clearInterval @timer))
      :reagent-render
      (fn [cfg env]
        (let [{:keys [bundle error loading?]} @state]
          (cond
            loading? [busy/block {:class "capp-embed capp-embed--loading"}
                      (i18n/t :ui/capp-loading)]
            error    [ui/problem (i18n/t :ui/capp-custom-widget-failed) (str error)]
            :else    ^{:key (:stamp bundle)} [shown cfg env bundle])))})))

(defn custom-widget
  [{:keys [widget-set version] :as cfg} env]
  (if (str/blank? (some-> widget-set name))
    [ui/problem (i18n/t :ui/capp-custom-widget-no-set) ":widget-set"]
    ^{:key [(name widget-set) (str version)]} [loaded cfg env]))

(def widgets
  {:custom-widget (fn [cfg env] [custom-widget cfg env])})
