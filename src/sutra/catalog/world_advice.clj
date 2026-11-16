(ns sutra.catalog.world-advice
  (:require [clojure.string :as str]
            [sutra.capps.events :as ce]
            [sutra.capps.sidebar :as sb]
            [sutra.world.db :as db]
            [sutra.world.gate :as gate]
            [sutra.world.gate.base :as b]
            [sutra.world.gate.lint :as kl]
            [sutra.world.gate.near :as n]
            [sutra.world.index :as ix]
            [sutra.types.nested :as nested]
            [sutra.kit.tree :as tree]
            [sutra.parser.impact :as impact]
            [sutra.parser.lint :as lint]
            [sutra.parser.lint.support :as ls]
            [sutra.parser.spec :as spec]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn- ot-name-of [e] (some-> (:object-type-name e) ix/nm))

(def fact-rows
  [{:id :ds-out :sections #{:transforms}
    :facts (fn [_ t] (for [o (ls/outs-of t)
                           :when (and (map? o) (= :dataset (:kind o)) (some? (:dataset o)))]
                       [:ds-out (ix/nm (:dataset o))]))}
   {:id :ot-out :sections #{:transforms}
    :facts (fn [_ t] (for [o (ls/outs-of t) :when (and (map? o) (some? (:object-type o)))]
                       [:ot-out (ix/nm (:object-type o))]))}
   {:id :writeback :sections #{:sources}
    :facts (fn [_ s] (for [[_ allow] (:writeback s)
                           :let [ot (some-> (:object-type allow) ix/nm)]
                           :when ot]
                       [:writes ot]))}
   {:id :edits :sections #{:actions}
    :facts (fn [_ a] (for [t (impact/edit-targets a) :when (or (keyword? t) (string? t))]
                       [:edits (ix/nm t)]))}
   {:id :visible :sections #{:object-types}
    :facts (fn [_ e] (when-not (or (nil? (:object-type-name e))
                                   (= :hidden (some-> (:visibility e) ix/kw))
                                   (str/starts-with? (str (:plural e)) "[LOG]"))
                       [[:visible]]))}
   {:id :any-type :sections #{:object-types}
    :facts (fn [_ _] [[:any]])}
   {:id :labels :sections #{:object-types}
    :facts (fn [_ e] (when-let [nm (ot-name-of e)]
                       (when-not (or (= :hidden (some-> (:visibility e) ix/kw))
                                     (str/starts-with? (str (:plural e)) "[LOG]"))
                         (for [l (ls/type-labels nm e)] [:label l]))))}])

(defn- fact [row f] [[:gate-facts row] f])

(defn- named
  [r db sec x]
  (if (nil? x)
    r
    (-> (kl/by-name r db sec x)
        (n/see [[sec (keyword (ix/nm x))]]))))

(defn- entries-named
  [db sec nm]
  (when nm (n/found db [[:by-name-str [sec nm]]])))

(defn- type-ref
  [r db x]
  (if (nil? x)
    [r nil]
    (let [k2 (when (keyword? x) (subs (str x) 1))
          r  (n/see r [[:object-types x] [:interfaces x] [:object-types k2]])
          nm (or (some-> (or (db/entry db [:object-types x])
                             (when k2 (db/entry db [:object-types k2])))
                         ot-name-of)
                 (some-> (db/entry db [:interfaces x]) :interface-name ix/nm)
                 (ix/nm x))]
      [r nm])))

(defn- ref-forms-of-name
  [db nm]
  (into #{nm} (for [sec [:object-types :interfaces] k (entries-named db sec nm)]
                (ix/nm k))))

(defn- typed-by-name
  [r db nm]
  (n/ask r db (for [sec [:object-types :interfaces]] [:by-name-str [sec nm]])))

(defn- log-facts
  [db k]
  (let [nm (ot-name-of (db/entry db [:object-types k]))]
    (concat (for [f (kl/own-names db :object-types k)] [[:gate-facts :action-logs] [:log f]])
            (when nm [[:by-name-str [:interfaces nm]]])
            (for [i (entries-named db :interfaces nm)]
              [[:gate-facts :action-logs] [:log (ix/nm i)]]))))

(defn- user-status
  [r db k]
  (n/ask r db (log-facts db k)))

(defn- typed-users
  [r db nm]
  (reduce #(user-status (n/see %1 [[:object-types %2]]) db %2)
          (n/ask r db [[:by-name-str [:object-types nm]]])
          (entries-named db :object-types nm)))

(defn- managing
  [r db k]
  (let [nm (ot-name-of (db/entry db [:object-types k]))]
    (n/ask (typed-by-name r db nm) db
           (for [f (into #{(ix/nm k)} (when nm (ref-forms-of-name db nm)))]
             (fact :edits [:edits f])))))

(defn- ends-at
  [r db nm]
  (let [forms (ref-forms-of-name db nm)]
    (n/ask (typed-by-name r db nm) db
           (for [f (into forms (map keyword forms))] [:edges [:link-end f]]))))

(defn- witness
  [r db k]
  (let [f  (fact :visible [:visible])
        cs (remove #{[:object-types k]} (n/lookup db f))]
    (loop [r r [c & more] cs]
      (cond
        (nil? c) (n/ask r db [f])
        :else (let [r    (user-status (n/see r [c]) db (second c))
                    logs (remove #(= :interfaces (first %))
                                 (mapcat #(n/lookup db %) (log-facts db (second c))))]
                (if (empty? logs) r (recur r more)))))))

(defn- dataset-keys
  [db x]
  (let [nm (ix/nm x)]
    (into (set (entries-named db :datasets nm))
          (filter #(db/entry db [:datasets %]) [x nm (keyword nm)]))))

(defn- dataset-named
  [r db x]
  (named r db :datasets x))

(defn- dataset-written
  [r db x]
  (if (nil? x)
    r
    (let [fs (for [f (into #{(ix/nm x)} (map ix/nm (dataset-keys db x)))]
               (fact :ds-out [:ds-out f]))]
      (-> (named r db :datasets x)
          (n/ask db fs)
          (n/see (for [t (n/found db fs)
                       o (ls/outs-of (db/entry db [:transforms t]))
                       :when (and (map? o) (some? (:dataset o)))]
                   [:datasets (:dataset o)]))))))

(defn- project-of
  [r db p]
  (named r db :projects p))

(defn- self [sec] (kl/self sec))

(defn- users-near [db k] (user-status (n/see n/none [[:object-types k]]) db k))

(defn- isolated-near
  [db k]
  (let [e  (db/entry db [:object-types k])
        nm (ot-name-of e)]
    (as-> (users-near db k) r
      (if nm (ends-at r db nm) r)
      (reduce (fn [r i] (let [[r inm] (type-ref r db i)]
                          (if inm
                            (-> (ends-at r db inm)
                                (n/see (for [j (entries-named db :interfaces inm)]
                                         [:interfaces j])))
                            (n/ask r db [[:edges [:link-end nil]]]))))
              r (nested/seqable (:implements e)))
      (witness r db k))))

(defn- managed-near [db k] (managing (users-near db k) db k))

(defn- status-near
  [db k]
  (let [nm (ot-name-of (db/entry db [:object-types k]))]
    (cond-> (managed-near db k) nm (ends-at db nm))))

(defn- labels-near
  [db k]
  (let [e  (db/entry db [:object-types k])
        nm (ot-name-of e)
        fs (when nm (for [l (ls/type-labels nm e)] (fact :labels [:label l])))]
    (reduce #(user-status %1 db %2)
            (n/ask (users-near db k) db fs)
            (n/found db fs))))

(defn- link-sides-near
  [db k]
  (reduce (fn [r x] (let [[r nm] (type-ref r db x)] (if nm (typed-users r db nm) r)))
          (n/see n/none [[:link-types k]])
          (keep (db/entry db [:link-types k]) [:from-object-type :to-object-type])))

(defn- lifecycle-type-near
  [db k]
  (let [lc (:lifecycle (db/entry db [:object-types k]))]
    (reduce (fn [r an]
              (let [r (named r db :actions an)]
                (reduce (fn [r a] (reduce #(first (type-ref %1 db %2)) r
                                          (keep #(when (map? %) (:type %))
                                                (vals (:params a)))))
                        r (keep #(db/entry db [:actions %])
                                (into (set (entries-named db :actions (ix/nm an)))
                                      [an (ix/nm an) (keyword (ix/nm an))])))))
            (n/see n/none [[:object-types k]])
            (keep :action (nested/seqable (:transitions lc))))))

(defn- lifecycle-action-near
  [db k]
  (let [on (:on (db/entry db [:actions k]))
        r  (n/see n/none [[:actions k]])]
    (if (nil? on)
      r
      (let [[r nm] (type-ref r db on)
            r      (n/ask r db [[:by-name-str [:object-types nm]]])]
        (if (seq (entries-named db :object-types nm))
          (n/see r (for [t (entries-named db :object-types nm)] [:object-types t]))
          (-> r
              (n/ask db [(fact :any-type [:any])])
              (n/see (take 1 (n/lookup db (fact :any-type [:any]))))))))))

(defn- dataset-ios-near
  [db k]
  (let [t (db/entry db [:transforms k])]
    (reduce #(dataset-named %1 db %2)
            (n/see n/none [[:transforms k]])
            (keep #(when (map? %) (:dataset %)) (concat (ls/ins-of t) (ls/outs-of t))))))

(defn- typed-ios-near
  [db k]
  (let [t (db/entry db [:transforms k])]
    (n/see (n/see n/none [[:transforms k]])
           (for [io (concat (ls/ins-of t) (ls/outs-of t))
                 :when (and (map? io) (some? (:object-type io)))]
             [:object-types (:object-type io)]))))

(defn- upload-near
  [db k]
  (dataset-named (n/see n/none [[:applications k]]) db
                 (get-in (db/entry db [:applications k]) [:layout :upload :dataset])))

(defn- backing-near
  [db k]
  (let [ds (get-in (db/entry db [:object-types k]) [:backing :dataset])
        nm (or (some-> (db/entry db [:datasets ds]) :dataset-name ix/nm) (some-> ds ix/nm))]
    (cond-> (n/see n/none [[:object-types k]])
      ds (dataset-written db ds)
      nm (dataset-written db nm))))

(defn- writeback-near
  [db k]
  (let [ots (for [[_ allow] (:writeback (db/entry db [:sources k]))
                  :let [ot (some-> (:object-type allow) ix/nm)]
                  :when ot]
              ot)]
    (reduce (fn [r ot]
              (let [ks (entries-named db :object-types ot)
                    fs (into #{ot} (map ix/nm ks))]
                (-> r
                    (n/ask db [(fact :writeback [:writes ot])
                               [:by-name-str [:object-types ot]]])
                    (n/see (for [t ks] [:object-types t]))
                    (n/see [[:object-types ot] [:object-types (keyword ot)]])
                    (n/ask db (for [f fs] (fact :ot-out [:ot-out f]))))))
            (n/see n/none [[:sources k]])
            ots)))

(defn- visibility-near
  [db k]
  (reduce (fn [r l] (-> r (kl/by-name db :object-types l) (kl/by-name db :link-types l)))
          (n/see n/none [[:applications k]])
          (keep #(get-in % [:visibility :link])
                (tree/nodes (:layout (db/entry db [:applications k]))))))

(defn- opened-apps
  [a]
  (concat (sb/app-names (:sidebar a))
          (for [nd (tree/nodes (:layout a))
                x  (concat (when (= :embed (some-> (:widget nd) ix/kw)) [(:app nd)])
                           (for [kk [:on-click :on-row-click :on-change]
                                 e (ce/as-vector (get nd kk))
                                 :when (= :open-app (:do e))]
                             (:app e)))
                :when x]
            x)))

(defn- opens-near
  [db k]
  (reduce #(named %1 db :applications %2)
          (n/see n/none [[:applications k]])
          (opened-apps (db/entry db [:applications k]))))

(defn- functions-near
  [db k]
  (let [a (db/entry db [:custom-applications k])]
    (reduce #(named %1 db :functions %2)
            (n/see n/none [[:custom-applications k]])
            (concat (get-in a [:layout :functions]) (get-in a [:scope :functions])))))

(def ^:private name-sections (vec (sort (keys ix/name-field))))

(defn- deprecated-named
  [r db nm]
  (if (nil? nm)
    r
    (reduce (fn [r sec] (-> r
                            (n/ask db [[:by-name-str [sec nm]]])
                            (n/see [[sec nm] [sec (keyword nm)]])))
            r name-sections)))

(defn- deprecation-near
  [sec types-of]
  (fn [db k]
    (reduce (fn [r x] (let [[r nm] (type-ref r db x)]
                        (-> (deprecated-named r db nm) (deprecated-named db (ix/nm x)))))
            (n/see n/none [[sec k]])
            (types-of (db/entry db [sec k])))))

(defn- param-types [a] (keep #(when (map? %) (:type %)) (vals (:params a))))

(defn- unreferenced-near
  [db k]
  (let [t  (db/entry db [:transforms k])
        r  (project-of (n/see n/none [[:transforms k]]) db (:project t))
        ps (keep #(db/entry db [:projects %])
                 (into (set (entries-named db :projects (some-> (:project t) ix/nm)))
                       [(:project t)]))
        r  (reduce (fn [r p] (reduce #(project-of %1 db %2) r (nested/seqable (:imports p)))) r ps)
        section {:dataset :datasets :object-type :object-types :transform :transforms}]
    (reduce (fn [r in]
              (let [kk (some #(when (get in %) %) (keys section))
                    x  (get in kk)]
                (if-not kk
                  r
                  (let [sec (section kk)
                        r   (named r db sec x)
                        es  (keep #(db/entry db [sec %])
                                  (into (set (entries-named db sec (ix/nm x))) [x]))]
                    (reduce #(project-of %1 db (:project %2)) r es)))))
            r (filter map? (tio/inputs-of t)))))

(defn- every-name-section [near] (into {} (for [s name-sections] [s near])))

(def ^:private rules
  {:cryptic-name                       (every-name-section [nil :all])
   :entity-without-name                (every-name-section [nil :all])
   :deprecated-without-metadata        (every-name-section [nil :all])
   :missing-identity                   {:object-types [nil :all]}
   :source-config                      {:sources [nil :all]}
   :app-source                         {:applications [nil :all]}
   :var-shadows-type                   {:applications [nil :all]}
   :pane-blind-for-audience            {:applications [nil :all]}
   :pane-shows-nothing                 {:applications [nil :all]}
   :view-too-many-profiles             {:applications [nil :all]}
   :writeback-form-hand-built          {:applications [nil :all]}
   :criterion-without-message          {:actions [nil :all]}
   :action-override-redundant          {:actions [nil :all]}
   :ontology-as-pipeline-intermediate  {:transforms [typed-ios-near :all]}
   :transform-writes-own-input         {:transforms [typed-ios-near :all]}
   :transform-reads-object-table       {:transforms [dataset-ios-near :all]}
   :names-undeclared-dataset           {:transforms [dataset-ios-near :all]
                                        :applications [upload-near :all]}
   :backing-without-producer           {:object-types [backing-near :all]}
   :writeback-without-read-side        {:sources [writeback-near :writeback]}
   :visibility-link-unknown            {:applications [visibility-near :all]}
   :view-opens-app-its-audience-cannot {:applications [opens-near :named]}
   :app-function-unknown               {:custom-applications [functions-near :all]}
   :reference-to-deprecated            {:actions [(deprecation-near :actions param-types)
                                                  :part]
                                        :custom-applications
                                        [(deprecation-near :custom-applications
                                                           #(get-in % [:scope :object-types]))
                                         :part]
                                        :applications
                                        [(deprecation-near :applications
                                                           #(some-> (:object-type %) vector))
                                         :part]}
   :input-unreferenced                 {:transforms [unreferenced-near :named]}
   :title-is-identifier                {:object-types [users-near :all]}
   :object-type-unnamed                {:object-types [users-near :all]}
   :attributes-unprioritized           {:object-types [users-near :all]}
   :object-type-isolated               {:object-types [isolated-near :named]}
   :per-period-type                    {:object-types [managed-near :all]}
   :status-lookup-type                 {:object-types [status-near :all]}
   :type-name-collision                {:object-types [labels-near :named]}
   :link-side-names                    {:link-types [link-sides-near :all]}
   :lifecycle                          {:object-types [lifecycle-type-near :named]
                                        :actions [lifecycle-action-near :named]}})

(def rule-ids (set (keys rules)))

(def specs
  (vec (for [[rule by-sec] (sort-by key rules)
             [sec [near own]] (sort-by key by-sec)]
         {:id (keyword (str "advice." (name rule)) (name sec)) :gate :lint-advice
          :rule :lint/advice :lint true :section sec :near (or near (self sec))
          :check [:advice rule own]})))

(def ^:private vocab (delay (spec/vocabulary)))

(defn entry-of
  [U k]
  (some (fn [[s es]] (when (and (map? es) (contains? es k)) [s (get es k)])) U))

(defn named-own?
  [sec k e {x :entity cls :entity-class}]
  (let [names (into #{} (keep #(some-> % rid/->name))
                     [(get e (rid/name-field-of sec)) (:name e) k])]
    (and (contains? #{sec (ents/section->tag sec)} cls)
         (if (nil? x)
           (nil? (or (get e (rid/name-field-of sec)) (:name e)))
           (let [x (str (rid/->name x))]
             (some #(or (= x %) (str/starts-with? x (str % "."))) names))))))

(defn- part-own?
  [sec k e {x :entity cls :entity-class}]
  (let [nm (or (get e (rid/name-field-of sec)) (:name e) (when-not (rid/rid? k) k))]
    (and (contains? #{sec (ents/section->tag sec)} cls)
         (str/starts-with? (str x) (str (some-> nm rid/->name) ".")))))

(defn- writeback-own?
  [U k {x :entity}]
  (= k (first (sort-by str (for [[s src] (:sources U)
                                 [_ allow] (:writeback src)
                                 :when (= (str x) (some-> (:object-type allow) ix/nm))]
                             s)))))

(def ^:private owns
  {:all       (constantly true)
   :named     (fn [_ k sec e f] (named-own? sec k e f))
   :part      (fn [_ k sec e f] (part-own? sec k e f))
   :writeback (fn [U k _ _ f] (writeback-own? U k f))})

(defn- check [rule own]
  (let [own? (or (owns own) (throw (ex-info (str "world-advice: no own " own) {:own own})))]
    (fn [U k]
      (let [[sec e] (entry-of U k)]
        (filterv #(own? U k sec e %) (lint/rule-findings rule U @vocab))))))

(def checks
  (into {} (for [{[_ rule own :as c] :check} specs] [c (check rule own)])))

(def ^:private facts (concat kl/fact-rows fact-rows))

(def rows (n/rows checks specs facts))

(def views
  [(b/facts-view facts) (n/near-view specs facts) (gate/findings-of rows)])

(defn advisories
  [d]
  (into [] (comp (map #(dissoc % :gate)) (filter lint/advisory?))
        (gate/report-of rows d)))
