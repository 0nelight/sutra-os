(ns sutra-ui.apphost.modules
  (:require [sutra-ui.debug :as dbg]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [shadow.loader :as loader]
            [sutra-ui.api :as api]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.interop :as interop]
            [sutra-ui.state.db :as db]))

(defrecord Module [interface view])

(defn module
  [interface view]
  (->Module interface view))

(defn module? [x] (instance? Module x))

(def ^:private type-pred
  {:any     (constantly true)
   :string  string?  :keyword keyword? :int int?     :number number?
   :bool    boolean? :map     map?     :vector vector? :set  set?  :fn fn?
   :boolean boolean? :date string? :timestamp string?
   :array   sequential? :object-set sequential?})

(defn- check-one!
  [kind field typ v]
  (let [pred (get type-pred typ)]
    (when (nil? pred)
      (throw (js/Error. (str "module interface: unknown type " (pr-str typ)
                             " declared for " kind " " (pr-str field)))))
    (when-not (pred v)
      (throw (js/Error. (str "module " kind " " (pr-str field) ": expected "
                             (name typ) ", got " (pr-str v)))))))

(defn- check!
  [kind spec m]
  (doseq [[k v] m]
    (when-not (contains? spec k)
      (throw (js/Error. (str "module: unknown " kind " " (pr-str k)
                             " — not in the interface " (pr-str (vec (keys spec)))))))
    (check-one! kind k (get spec k) v))
  m)

(defn validate-inputs [mod m] (check! "input" (get-in mod [:interface :inputs]) (or m {})))

(defn validate-output
  [mod k v]
  (let [spec (get-in mod [:interface :outputs])]
    (if (nil? v)
      (when-not (contains? spec k)
        (throw (js/Error. (str "module: unknown output " (pr-str k)))))
      (check! "output" spec {k v})))
  [k v])

(defn standalone
  [mod]
  (fn [descriptor data]
    (let [in (validate-inputs mod (get-in descriptor [:layout :module-inputs]))]
      [(:view mod) {:in in :emit (fn [& _] nil) :descriptor descriptor :data data}])))

(defn embed
  [{:keys [module inputs on-output data bound]}]
  (if-not (module? module)
    [ui/problem "app/embed: :module is not a module (nothing loaded yet?)" nil "capp-error"]
    (let [in   (merge (validate-inputs module (into {}
                                                     (remove (fn [[k v]]
                                                               (and (nil? v) (contains? bound k))))
                                                     inputs))
                      (select-keys inputs bound))
          emit (fn [k v]
                 (let [[k v] (validate-output module k v)]
                   (when on-output (on-output k v))))]
      [(:view module) {:in in :emit emit :descriptor (:descriptor module) :data data}])))

(defonce ^:private compiler (atom nil))
(defn set-compiler! [f] (reset! compiler f))

(defonce sci (r/atom nil))
(defn set-sci! [m] (reset! sci m))

(defn ensure-sci!
  []
  (if @compiler
    (js/Promise.resolve true)
    (try
      (loader/load "sci")
      (catch :default e
        (js/console.warn "[sci] no module loader in this build — not fetching" e)
        (js/Promise.resolve false)))))

(defonce ^:private data-builder (atom nil))
(defn set-data-builder! [f] (reset! data-builder f))

(defn- as-module [compiled descriptor]
  (if (module? compiled)
    (assoc compiled :descriptor descriptor)
    (map->Module {:interface {:inputs {} :outputs {}}
                  :descriptor descriptor
                  :view (fn [{:keys [descriptor data]}] [compiled descriptor data])})))

(defn- loaded
  [child-id ^js body]
  (let [desc  (dbg/timed :app/js->clj-descriptor
                         #(js->clj (.-application body) :keywordize-keys true))
        data  (dbg/timed :app/js->clj-data #(js->clj (.-data body) :keywordize-keys true)
                         interop/rows-note)
        build (and (str/blank? (str (:source desc))) @data-builder)]
    (if build
      (.then (api/get! :my-application {:rid (str child-id)})
             (fn [tr] {:module (build (or (:application tr) (get tr "application")))
                       :data   (or (:data tr) (get tr "data"))}))
      (.then (js/Promise.resolve (ensure-sci!))
             (fn [_]
               (if-let [compile-view @compiler]
                 {:module (as-module (compile-view (:source desc)) desc) :data data}
                 {:error "kein Compiler registriert (sci-runtime nicht geladen)"}))))))

(defn- load-child!
  [child-id id]
  (.then (client/door! {:id id} "" {:fail "Laden fehlgeschlagen"})
         #(loaded child-id %)))

(defn use-app
  [child-id]
  (let [h   (r/atom {:loading? true})
        id  (cond-> child-id (keyword? child-id) name)
        say #(reset! h (assoc % :loading? false))]
    (db/send! [:rf/request {:ask  [load-child! child-id id]
                            :then [[:rf/invoke say]]
                            :else [[:rf/invoke #(say {:error %})]]}])
    h))
