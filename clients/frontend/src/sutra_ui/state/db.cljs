(ns sutra-ui.state.db
  (:require [reagent.core :as r]
            [reagent.ratom :as ratom]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defonce db (r/atom {}))

(defonce sutra (machine/frame db {:name :sutra}))

(defn send!
  [event]
  (machine/dispatch event {:frame sutra}))

(deftype Cursor [path inner]
  ratom/IReactiveAtom
  IDeref
  (-deref [_] @inner)
  IReset
  (-reset! [_ v] (swap! db assoc-in path v) v)
  ISwap
  (-swap! [_ f] (get-in (swap! db update-in path f) path))
  (-swap! [_ f x] (get-in (swap! db update-in path f x) path))
  (-swap! [_ f x y] (get-in (swap! db update-in path f x y) path))
  (-swap! [_ f x y more] (get-in (swap! db #(apply update-in % path f x y more)) path))
  IWatchable
  (-add-watch [this key f]
    (add-watch db [::cursor path key]
               (fn [_ _ old new]
                 (let [o (get-in old path) n (get-in new path)]
                   (when-not (identical? o n) (f key this o n)))))
    this)
  (-remove-watch [this key]
    (remove-watch db [::cursor path key])
    this)
  (-notify-watches [this _ _] this))

(defn cursor
  ([path] (cursor path nil))
  ([path init]
   (when (= ::none (get-in @db path ::none)) (swap! db assoc-in path init))
   (Cursor. path (r/cursor db path))))

(defn reg
  [id f]
  (machine/reg-event id (fn [{s :db} event] (f s event))))

(defn promise!
  [event]
  (js/Promise. (fn [resolve reject] (send! (conj event {:resolve resolve :reject reject})))))

(defn load
  [path ask & [opts reply]]
  (rq/ask (assoc opts :path path :ask ask :rd? true) reply))

(defn load!
  [path ask & [opts]]
  (promise! [:rf/request (assoc opts :path path :ask ask :rd? true)]))

(reg :sutra/ignored (fn [_ _] nil))

(reg :sutra.db/put (fn [s [_ path v]] {:db (assoc-in s path v)}))

(reg :sutra.db/edit (fn [s [_ path f args]] {:db (apply update-in s path f args)}))

(defn put!
  [^Cursor c v]
  (send! [:sutra.db/put (.-path c) v]))

(defn edit!
  [^Cursor c f & args]
  (send! [:sutra.db/edit (.-path c) f args]))
