(ns sutra.kit.decisions
  (:require #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn]))
  #?(:cljs (:require-macros [sutra.kit.decisions])))

(def kinds
  {:build/schedule   {:fn 'sutra.builds.run.schedule/step :process :worker
                      :idle? (fn [[state] [state' effects]]
                               (and (= state state') (empty? effects)))}
   :build/admission  {:fn 'sutra.builds.run.lock/wait-step :process :worker
                      :idle? (fn [[state] [state' effects]]
                               (and (= state state')
                                    (every? (comp #{:wait :sleep} first) effects)))}
   :build/plan       {:fn 'sutra.builds.plan/build-plan :process :worker :keep 3}
   :build/merge      {:fn 'sutra.store.pg.dataset/plan-merge :process :worker}
   :view/portal      {:fn 'sutra.capps.portal/view :process :browser}
   :view/object-page {:fn 'sutra.capps.standard-view/page-view :process :browser}})

(def kept-per-kind 50)

(defonce ^:private on (atom nil))

(defonce ^:private kept (atom {}))

(defn recording?
  [kind]
  (when-let [ks @on] (contains? ks kind)))

(defn- now [] #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))

(defn record!
  [kind inputs output]
  (when-not (when-let [idle? (get-in kinds [kind :idle?])] (idle? inputs output))
    (swap! kept update kind
           (fn [v]
             (let [v (conj (or v []) {:kind kind :inputs inputs :output output :at (now)})]
               (if (> (count v) (get-in kinds [kind :keep] kept-per-kind)) (subvec v 1) v)))))
  nil)

#?(:clj
   (defmacro decide
     [kind & args]
     (let [f  (or (get-in kinds [kind :fn])
                  (throw (ex-info (str "no decision kind " kind) {:kind kind})))
           as (vec (repeatedly (count args) #(gensym "in")))]
       `(let [~@(interleave as args)
              out# (~f ~@as)]
          (when (recording? ~kind) (record! ~kind ~as out#))
          out#))))

(defn start!
  [asked]
  (let [ks (into #{} (filter kinds) (map keyword asked))
        ks (if (seq ks) ks (set (keys kinds)))]
    (reset! kept {})
    (reset! on ks)
    {:recording (vec (sort ks))}))

(defn stop!
  []
  (reset! on nil)
  nil)

(defn clear!
  []
  (reset! kept {})
  nil)

(defn records
  []
  (vec (sort-by :at (mapcat val @kept))))

(defn status
  []
  (cond-> {:recording (some-> @on sort vec)}
    (seq @kept) (assoc :kept (into (sorted-map) (map (fn [[k v]] [k (count v)])) @kept))))

(defn text
  [records]
  (binding [*print-length* nil *print-level* nil *print-meta* false
            *print-namespace-maps* false]
    (pr-str records)))

(defn read-text
  [s]
  (edn/read-string s))

#?(:clj
   (defn replay
     [{:keys [kind inputs output at]}]
     (let [sym (or (get-in kinds [kind :fn])
                   (throw (ex-info (str "no decision kind " kind) {:kind kind})))
           got (apply (requiring-resolve sym) inputs)]
       {:kind kind :at at :same? (= got output) :output got :recorded output})))
