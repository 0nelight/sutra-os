(ns sutra-ui.apphost.state
  (:require [reagent.core :as r]
            [reagent.ratom :as ratom]
            [sutra.capps.leaf-props :as lp]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.view-state :as vs]))

(def view-state
  vs/view-state)

(defn vars
  ([specs] (vars specs true))
  ([specs routing?]
   (let [reg (atom {})
         ref (fn [nm]
               (or (get @reg nm)
                   (throw (js/Error. (str "app/vars: unknown variable " (pr-str nm))))))
         mk  (fn [_nm spec]
               (cond
                 (:derived spec)
                 (r/track (fn [] ((:derived spec) ref)))

                 (and routing? (:external-id spec))
                 (vs/view-state (:external-id spec) (:default spec)
                                {:as     (:as spec :edn)
                                 :push?  (:push? spec)
                                 :in-url (:in-url spec)})

                 :else (r/atom (:default spec))))]
     (reset! reg (reduce-kv (fn [m nm spec] (assoc m nm (mk nm spec))) {} specs))
     @reg)))

(defn- identity-of
  [row]
  (when (map? row)
    (let [t (or (:object-type-ref row) (:object-type row))
          id (:id row)]
      (when (and (some? t) (some? id)) [(str t) (str id)]))))

(defn row-index
  [data]
  (persistent!
   (reduce-kv (fn [m _ v]
                (if (sequential? v)
                  (reduce (fn [m row]
                            (if-let [k (identity-of row)] (assoc! m k (conj (get m k []) row)) m))
                          m v)
                  m))
              (transient {})
              (if (map? data) data {}))))

(defn- current-row
  [index known row]
  (let [k (identity-of row)]
    (if-let [cands (some->> k (get index))]
      (if (next cands)
        (let [ks (set (keys row))] (or (some #(when (= ks (set (keys %))) %) cands) (first cands)))
        (first cands))
      (get known k row))))

(defn- holds-rows?
  [v]
  (cond (map? v)  (some? (identity-of v))
        (coll? v) (some identity-of v)
        :else     false))

(defn freshen
  ([v index] (freshen v index {}))
  ([v index known]
   (let [cur #(current-row index known %)]
     (cond (map? v)    (cur v)
           (vector? v) (mapv cur v)
           (set? v)    (into #{} (map cur) v)
           :else       v))))

(defn- rows-by-identity
  [v]
  (into {} (keep (fn [row] (when-let [k (identity-of row)] [k row])))
        (lp/as-rows v)))

(deftype Selection [inner index ^:mutable known]
  IDeref
  (-deref [_]
    (let [v @inner]
      (if (holds-rows? v)
        (let [now (freshen v @index known)]
          (set! known (rows-by-identity now))
          now)
        v)))
  IReset
  (-reset! [_ v] (set! known {}) (reset! inner v))
  ISwap
  (-swap! [this f] (-reset! this (f @this)))
  (-swap! [this f a] (-reset! this (f @this a)))
  (-swap! [this f a b] (-reset! this (f @this a b)))
  (-swap! [this f a b xs] (-reset! this (apply f @this a b xs)))
  IWatchable
  (-notify-watches [_ old new] (-notify-watches inner old new))
  (-add-watch [this k f] (add-watch inner k f) this)
  (-remove-watch [this k] (remove-watch inner k) this))

(defn payload-index
  [payload*]
  (ratom/make-reaction #(row-index @payload*)))

(defn selection
  [h index]
  (if (satisfies? IReset h) (Selection. h index {}) h))

(defn inner
  [h]
  (if (instance? Selection h) (.-inner h) h))

(defn overlays
  ([] (with-meta (r/atom {}) {::bound (atom {})}))
  ([f] (let [bound (atom {})]
         (specify! (frame/cursor f [:overlays]) IMeta (-meta [_] {::bound bound})))))

(defn- bound-handle
  [ov k]
  (some-> (::bound (meta ov)) deref (get k)))

(defn bind-visibility!
  [ov k h]
  (when-let [reg (::bound (meta ov))]
    (if h (swap! reg assoc k h) (swap! reg dissoc k))))

(defn open?
  [ov k]
  (if-let [h (bound-handle ov k)]
    (boolean (deref h))
    (contains? @ov k)))

(defn overlay-arg
  [ov k]
  (if-let [h (bound-handle ov k)]
    (deref h)
    (get @ov k)))

(defn open!
  ([ov k] (open! ov k nil))
  ([ov k arg]
   (let [h (bound-handle ov k)]
     (cond
       (not h)                        (swap! ov assoc k (if (some? arg) arg true))
       (not (satisfies? IReset h))    nil
       (some? arg)                    (reset! h arg)
       (not (boolean (deref h)))      (reset! h true)
       :else                          nil))))

(defn- schliessen-melden!
  []
  (when (exists? js/window)
    (.dispatchEvent js/window (js/CustomEvent. "sutra:fenster-zu" #js {:detail "befehl"}))))

(defn close!
  [ov k]
  (schliessen-melden!)
  (let [h (bound-handle ov k)]
    (cond
      (and h (satisfies? IReset h)) (reset! h nil)
      h                             nil
      :else                         (swap! ov dissoc k))))

(defn toggle!
  ([ov k] (toggle! ov k true))
  ([ov k arg]
   (if (open? ov k) (close! ov k) (open! ov k arg))))

(defn close-all!
  [ov]
  (doseq [k (keys (some-> (::bound (meta ov)) deref))] (close! ov k))
  (reset! ov {}))
