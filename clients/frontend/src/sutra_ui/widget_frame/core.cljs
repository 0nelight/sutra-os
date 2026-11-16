(ns sutra-ui.widget-frame.core
  (:require [cljs.reader :as reader]
            [reagent.core :as r]
            [reagent.dom.client :as rdomc]
            [sci.core :as sci]
            [sutra.capps.custom-widget :as cw]
            [sutra.kit.size :as g]
            [clojure.string :as str]))

(defonce ^:private state (r/atom {:values {} :stored {}}))
(defonce ^:private replies (atom {}))
(defonce ^:private next-id (atom 0))
(defonce ^:private generation (r/atom 0))
(defonce ^:private fetches (atom {}))

(defn- host-origin
  []
  (some (fn [^js s]
          (when (str/ends-with? (.-src s) cw/runtime-url) (.-origin (js/URL. (.-src s)))))
        (array-seq (.-scripts js/document))))

(defonce ^:private host (delay (host-origin)))

(defn- post!
  [msg]
  (when-let [o @host] (.postMessage js/window.parent (pr-str msg) o)))

(defn- rpc!
  [op spec]
  (js/Promise.
   (fn [ok fail]
     (let [id (swap! next-id inc)]
       (swap! replies assoc id [ok fail])
       (post! [:rpc id op spec])))))

(defn param
  ([k] (get-in @state [:values k]))
  ([k default] (get-in @state [:values k] default)))

(defn params [] (:values @state))

(defn emit!
  ([event] (emit! event nil))
  ([event updates] (post! [:emit event (or updates {})])))

(defn objects [spec] (rpc! :objects spec))
(defn search [spec] (rpc! :search spec))
(defn aggregate [spec] (rpc! :aggregate spec))
(defn call-function [function args] (rpc! :function {:function function :args args}))

(defn apply-action!
  [action parameters]
  (.then (rpc! :submit {:action action :parameters parameters})
         (fn [v]
           (reset! fetches {})
           (swap! generation inc)
           v)))

(defn fetching
  [k f]
  @generation
  (let [a (or (get @fetches k)
              (let [a (r/atom {:loading? true})]
                (swap! fetches assoc k a)
                (-> (f)
                    (.then #(reset! a {:data %}))
                    (.catch #(reset! a {:error (str %)})))
                a))]
    @a))

(defn stored
  ([k] (get-in @state [:stored k]))
  ([k default] (get-in @state [:stored k] default)))

(defn store!
  [k v]
  (swap! state assoc-in [:stored k] v)
  (post! [:store k v]))

(defn dark?
  []
  (if-let [t (:thema @state)]
    (= :dark t)
    (.-matches (js/window.matchMedia "(prefers-color-scheme: dark)"))))

(defn groesse
  []
  (:groesse @state))

(defn- markieren!
  [k]
  (when k
    (doto js/document.documentElement
      (.setAttribute "data-groesse" (name k))
      (.setAttribute "data-ab" (str/join " " (map name (g/ab k)))))))

(defn- tokens!
  [tokens thema]
  (let [root js/document.documentElement]
    (doseq [[p v] tokens] (.setProperty (.-style root) p v))
    (when thema
      (.setAttribute root "data-thema" (name thema))
      (set! (.. root -style -colorScheme) (name thema)))))

(def ^:private widget-api
  {'param param 'params params 'emit! emit! 'objects objects 'search search
   'aggregate aggregate 'call-function call-function 'apply-action! apply-action!
   'fetching fetching 'stored stored 'store! store! 'dark? dark? 'groesse groesse})

(def ^:private reagent-api
  {'atom r/atom 'cursor r/cursor 'create-class r/create-class 'as-element r/as-element
   'track r/track 'track! r/track! 'dispose! r/dispose!
   'flush r/flush 'after-render r/after-render})

(defn- context
  [files]
  (sci/init
   {:namespaces {'sutra.widget widget-api 'reagent.core reagent-api}
    :classes    {'js goog/global :allow :all}
    :load-fn    (fn [{:keys [namespace]}]
                  (when-let [p (cw/source-of files namespace)]
                    {:file p :source (get files p)}))}))

(defn- report! [msg] (post! [:error (str msg)]))

(defn- problem-view
  [msg]
  [:pre {:style {:margin "var(--sp-3)" :color "var(--c-danger)" :white-space "pre-wrap"
                 :font-size "var(--fs-sm)"}} msg])

(defn- boundary
  [_view]
  (let [failed (r/atom nil)]
    (r/create-class
     {:get-derived-state-from-error (fn [e] (reset! failed (str e)) #js {})
      :component-did-catch          (fn [_ e _] (report! e))
      :reagent-render
      (fn [view]
        (if-let [m @failed]
          [problem-view m]
          [view]))})))

(defn- add-css!
  [text]
  (let [el (js/document.createElement "style")]
    (set! (.-textContent el) text)
    (.appendChild js/document.head el)))

(defn- watch-size!
  []
  (let [send (fn [] (post! [:height (.-scrollHeight js/document.body)]))]
    (.observe (js/ResizeObserver. send) js/document.body)
    (send)))

(defonce ^:private root (delay (rdomc/create-root (js/document.getElementById "root"))))

(defn- show!
  [view]
  (rdomc/render @root [boundary view]))

(defn- start!
  [{:keys [files widget values stored groesse thema tokens]}]
  (swap! state assoc :values values :stored (or stored {}) :groesse groesse :thema thema)
  (markieren! groesse)
  (tokens! tokens thema)
  (let [{:keys [main css]} widget]
    (add-css! "body{margin:0;font-family:system-ui,sans-serif;color-scheme:light dark}")
    (when css (add-css! (get files css)))
    (try
      (let [view (sci/eval-string* (context files)
                                   (str "(require '" (namespace main) ") " main))]
        (show! view))
      (catch :default e
        (let [m (or (some-> e ex-message) (str e))]
          (report! m)
          (show! (fn [] [problem-view m])))))
    (watch-size!)))

(defn- answer!
  [id ok? value]
  (when-let [[ok fail] (get @replies id)]
    (swap! replies dissoc id)
    (if ok? (ok value) (fail value))))

(defn- on-message
  [^js e]
  (when (and (identical? (.-source e) js/window.parent) (= @host (.-origin e))
             (string? (.-data e)))
    (let [[op a b c] (reader/read-string (.-data e))]
      (case op
        :init   (start! a)
        :values (swap! state assoc :values a)
        :groesse (do (swap! state assoc :groesse a) (markieren! a))
        :reply  (answer! a b c)
        nil))))

(defn init []
  (.addEventListener js/window "message" on-message)
  (post! [:ready]))
