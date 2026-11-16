(ns sutra-ui.apphost.scope-handles
  (:require [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.bind :as bind]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.table :as tbl]
            [sutra.capps.variables :as vars :refer [var-kind deref*]]
            [sutra.capps.warn :as warn]
            [sutra-ui.apphost.client :as client :refer [aggregate! page-generation]]
            [sutra-ui.apphost.widgets.filter :as fw]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.model :as model]
            [sutra-ui.apphost.state :as state :refer [vars]]))

(defn today
  []
  (let [d (js/Date.)
        p #(if (< % 10) (str "0" %) (str %))]
    (str (.getFullYear d) "-" (p (inc (.getMonth d))) "-" (p (.getDate d)))))

(defn resolve-ref
  [ref handles derived data]
  (vars/resolve-ref ref handles derived (vars/with-today data (today))))

(defn- resolve-refs-against-payload
  [x handles derived data*]
  (vars/resolve-call-args x handles derived (vars/with-today (some-> data* deref) (today))))

(defn- track! [{f :frame sid :sid} nm thunk] (model/track! f [sid :track nm] thunk))

(defn- recomputing-handle
  [{f :frame sid :sid :as at} nm descriptor mode args-fn ask!]
  (let [asked (frame/cursor f [:scopes sid :asked nm])
        fired (volatile! ::never)]
    (frame/send! f [:capp/put [:scopes sid :computed nm] {:loading? (not= :event mode)}])
    (track! at nm
            #(let [n    (or @asked 0)
                   args (args-fn)
                   k    (case mode
                          :event          (when (pos? n) [n])
                          :load-and-event [n]
                          [args (page-generation descriptor) n])]
               (when (and k (not= k @fired))
                 (vreset! fired k)
                 (frame/send! f [:capp/compute sid nm (ask! args)]))))
    (specify! (frame/cursor f [:scopes sid :computed nm])
      IMeta
      (-meta [_] {:recompute! #(swap! asked inc)}))))

(defn- make-agg-handle
  [at nm descriptor spec handles derived data* scenario-h]
  (recomputing-handle
   at nm descriptor (->kw (:recompute spec))
   (fn [] [(some-> scenario-h deref)
           (resolve-refs-against-payload (:aggregate spec) handles derived data*)])
   (fn [[sc wspec]] {:ask [aggregate! descriptor wspec {:scenario sc}]})))

(defn- make-function-handle
  [at nm descriptor spec handles derived data* scenario-h]
  (recomputing-handle
   at nm descriptor (->kw (:recompute spec))
   (fn [] [(some-> scenario-h deref)
           (when (:args spec)
             (resolve-refs-against-payload (:args spec) handles derived data*))])
   (fn [[sc args]]
     (let [call (cond-> {:function (:function spec)} args (assoc :args args))]
       {:ask   [client/function! descriptor call {:scenario sc}]
        :dedup [:capp/function (client/function-key descriptor call sc)]}))))

(defn- make-compare-handle
  [at nm descriptor spec handles derived data* _scenario-h]
  (let [{:keys [of key]} (:compare spec)
        state  (r/atom {:loading? true :rows [] :scenarios []})
        cache  (r/atom {})
        seen   (volatile! (page-generation descriptor))
        names  (r/atom {})
        key-fn (or (tbl/field-fn key) :id)]
    (if (contains? #{:aggregate :function} (some-> (get derived of) var-kind))
      (reset! state {:loading? false :rows [] :scenarios []
                     :error (str "compare :of " (pr-str of) " names an "
                                 (name (var-kind (get derived of)))
                                 " variable — :of must be a derived object set "
                                 "(:source/:from), which is what can be recomputed "
                                 "on another branch")})
      (do
        (frame/request! (:frame at)
                        (assoc (frame/kept #(reset! names (into {} (map (juxt :scenario :name))
                                                                %)))
                               :ask [client/scenarios-list descriptor]))
        (track!
         at nm
         (fn []
           (let [g    (page-generation descriptor)
                 _    (when (not= g @seen) (vreset! seen g) (reset! cache {}))
                 segs (lp/seg-list (some-> (resolve-ref (:scenarios spec) handles derived nil)
                                           deref*))
                 base (vec (resolve-ref {:val of} handles derived @data*))
                 have @cache]
             (doseq [s segs :when (not (contains? have s))]
               (frame/request! (:frame at)
                               (assoc (frame/kept #(when % (swap! cache assoc s %))
                                                  #(swap! cache assoc s ::failed))
                                      :ask [client/refresh! descriptor {:scenario s}]
                                      :dedup [:capp/compare s])))
             (let [ready (filterv #(and (contains? have %) (not= ::failed (get have %))) segs)
                   by    (into {}
                               (map (fn [s]
                                      [s
                                       (vec (resolve-ref {:val of} handles derived (get have s)))]))
                               ready)]
               (reset! state {:loading?  (not= (count ready) (count segs))
                              :key       key
                              :rows      (tbl/compare-rows base by ready key-fn)
                              :labels    @names
                              :scenarios
                              (mapv (fn [s] {:scenario s :name (get @names s (str s))})
                                    ready)})))))))
    state))

(defn url-keys-on-screen
  [tree handles]
  (let [specs (vars/all-var-specs tree)
        read  (fn [nm] (some-> (get handles nm) deref*))]
    (into #{}
          (keep (fn [nm] (some-> (:external-id (get specs nm)) name)))
          (vars/live-vars tree specs read))))

(defn build-scope
  [{f :frame sid :sid :as at} descriptor specs parent scenario-var data*]
  (let [by-kind   (group-by (comp var-kind val) specs)
        derived   (merge (:derived parent)
                         (into {} (concat (:aggregate by-kind) (:function by-kind)
                                          (:compare by-kind) (:derived by-kind))))
        herein    (get-in descriptor [:layout :module-inputs])
        url?      (if (false? (:routing? parent)) (constantly false) :external-id)
        data      (some-> data* deref)
        specs*    (into {} (map (fn [[nm sp]]
                                  [nm (assoc (select-keys sp [:external-id :in-url :push?])
                                             :as      (vars/coercion-of sp)
                                             :default (if (and (:input? sp)
                                                               (contains? herein nm))
                                                        (get herein nm)
                                                        (vars/state-default sp data nm)))]))
                        (:state by-kind))
        own       (into {} (remove (comp url? val)) specs*)
        _         (frame/send! f [:capp/put [:scopes sid :vars] (update-vals own :default)])
        index     (state/payload-index data*)
        state-h   (merge (vars (into {} (filter (comp url? val)) specs*))
                         (into {} (map (fn [[nm]] [nm (frame/cursor f [:scopes sid :vars nm])]))
                               own))
        state-h   (update-vals state-h #(state/selection % index))
        state-h   (fw/wire-filters (into {} (:state by-kind)) state-h)
        base-h    (merge (:handles parent) state-h)
        scenario-h (when scenario-var (get base-h scenario-var))
        var-scen  (fn [nm sp]
                    (if-let [v (bind/name-in :var (:scenario sp))]
                      (or (get base-h v)
                          (do (warn/warn! (str "app/vars: " (pr-str nm) " names :scenario "
                                               (pr-str v) ", which is not a variable in scope"
                                               " — reading the base instead"))
                              scenario-h))
                      scenario-h))
        computed  (fn [make kind]
                    (into {} (map (fn [[nm sp]]
                                    [nm (make at nm descriptor sp base-h derived data*
                                              (var-scen nm sp))]))
                          (get by-kind kind)))]
    {:handles (merge base-h
                     (computed make-agg-handle :aggregate)
                     (computed make-function-handle :function)
                     (computed make-compare-handle :compare))
     :derived derived}))
