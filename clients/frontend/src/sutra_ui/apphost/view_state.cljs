(ns sutra-ui.apphost.view-state
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [sutra.capps.variables :as vars]
            [reagent.core :as r]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.portal :as pstate]))

(defonce ^:private params (r/atom {}))

(defonce ^:private behaviors (atom {}))

(defonce ^:private live (atom nil))

(defonce ^:private schnittstelle (atom {}))

(defn- parse-query
  []
  (let [p    (js/URLSearchParams. (.-search js/location))
        skip (set routing/sticky-params)
        m    (transient {})]
    (.forEach p (fn [v k] (when-not (skip k) (assoc! m k v))))
    (persistent! m)))

(defn- encode-query
  [m]
  (let [p (js/URLSearchParams. (or (some-> (routing/sticky-query) (subs 1)) ""))]
    (doseq [k (sort (keys m))] (.set p k (get m k)))
    (.toString p)))

(defn- dom? [] (exists? js/window))

(defn- shows?
  [b k]
  (if-let [{:keys [writes? only-while-live?]} (and b (vars/url-behavior b))]
    (and writes?
         (or (not only-while-live?)
             (let [l @live] (or (nil? l) (contains? l k)))))
    true))

(defn- shown
  [m]
  (let [b @behaviors]
    (into {} (filter (fn [[k _]] (shows? (get b k) k))) m)))

(defn- write-search!
  [m push?]
  (when (dom?)
    (let [qs   (encode-query (shown m))
          base (.-pathname js/location)
          url  (if (str/blank? qs) base (str base "?" qs))
          cur  (str base (.-search js/location))]
      (when (not= cur url)
        (if push?
          (.pushState js/history nil "" url)
          (.replaceState js/history nil "" url))))))

(def ^:private presets
  {:string  {:parse identity            :encode str}
   :keyword {:parse #(let [t (str/replace-first (str %) #"^:" "")]
                       (when-not (str/blank? t) (keyword t)))
             :encode #(when % (name %))}
   :int     {:parse #(let [n (js/parseInt % 10)] (when-not (js/isNaN n) n))
             :encode #(when % (str %))}
   :bool    {:parse #(= % "1")          :encode #(when % "1")}
   :edn     {:parse (fn [s]
                      (let [v (try (reader/read-string s) (catch :default _ ::unlesbar))]
                        (cond
                          (= ::unlesbar v) nil
                          (symbol? v)      s
                          :else            v)))
             :encode pr-str}})

(assert (= vars/url-coercions (set (keys presets)))
        "view-state/presets und sutra.capps.variables/url-coercions sind auseinandergelaufen")

(defn- coercion
  [opts]
  (merge (presets (:as opts :edn) (:edn presets))
         (select-keys opts [:parse :encode])))

(defn- read-raw
  [raw default parse]
  (let [v (when (some? raw) (parse raw))]
    (if (nil? v) default v)))

(deftype ViewCursor [store k default parse encode push? commit!]
  IDeref
  (-deref [_]
    (read-raw (get @store k) default parse))
  IWatchable
  (-add-watch [this key f]
    (add-watch store [::cursor this key]
               (fn [_ _ old-store new-store]
                 (let [o (read-raw (get old-store k) default parse)
                       n (read-raw (get new-store k) default parse)]
                   (when (not= o n) (f key this o n)))))
    this)
  (-remove-watch [this key]
    (remove-watch store [::cursor this key])
    this)
  (-notify-watches [this _ _] this)
  IReset
  (-reset! [_ v]
    (let [enc     (encode v)
          def-enc (encode default)
          s       (when (and (some? enc) (not= enc def-enc)) enc)
          m       (swap! store (fn [m] (if (nil? s) (dissoc m k) (assoc m k s))))]
      (commit! m push?)
      v))
  ISwap
  (-swap! [this f]          (-reset! this (f (-deref this))))
  (-swap! [this f a]        (-reset! this (f (-deref this) a)))
  (-swap! [this f a b]      (-reset! this (f (-deref this) a b)))
  (-swap! [this f a b more] (-reset! this (apply f (-deref this) a b more))))

(defn pushes?
  [h]
  (and (instance? ViewCursor h) (.-push? ^js h)))

(defn make
  [store k default opts commit!]
  (let [{:keys [parse encode]} (coercion opts)]
    (->ViewCursor store (name k) default parse encode (boolean (:push? opts)) commit!)))

(defn- sync-from-url! []
  (reset! params (parse-query)))

(defn- clear-url! []
  (reset! params {})
  (reset! behaviors {})
  (reset! live nil)
  (.replaceState js/history nil "" (.-pathname js/location)))

(defonce ^:private handover (atom nil))

(defn handover!
  [m]
  (reset! handover (not-empty m)))

(defn- adopt! [m]
  (reset! behaviors {})
  (reset! live nil)
  (reset! params m)
  (write-search! m false))

(defonce ^:private installed? (atom false))

(defonce ^:private current-module (atom ::none))

(declare install!)

(defn module!
  [k]
  (install!)
  (when (not= k @current-module)
    (let [first? (= ::none @current-module)]
      (reset! current-module k)
      (reset! schnittstelle {})
      (cond
        (some? @handover) (let [h @handover] (reset! handover nil) (adopt! h))
        first?            (sync-from-url!)
        :else             (clear-url!)))))

(defn install!
  []
  (when (and (dom?) (compare-and-set! installed? false true))
    (routing/on-history! ::view-state :after sync-from-url!)
    (add-watch pstate/current-consumer-app ::view-state
               (fn [_ _ _ new]
                 (when-let [id (some-> new :card :id)] (module! id))))
    (sync-from-url!)))

(defn live!
  [ks]
  (when (not= ks @live)
    (reset! live ks)
    (write-search! @params false)))

(defn view-state
  ([k] (view-state k nil nil))
  ([k default] (view-state k default nil))
  ([k default opts]
   (install!)
   (let [behavior (vars/in-url opts)]
     (swap! behaviors assoc (name k) behavior)
     (let [h (if (:writes? (vars/url-behavior behavior))
               (make params k default opts write-search!)
               (let [{:keys [parse]} (coercion opts)]
                 (r/atom (read-raw (get @params (name k)) default parse))))]
       (swap! schnittstelle assoc (name k) h)
       h))))

(defn stand
  []
  (into {} (map (fn [[k h]] [k @h])) @schnittstelle))

(defn encode-value
  [x]
  (let [{:keys [encode]} (coercion {:as (vars/coercion-for x)})]
    (encode x)))
