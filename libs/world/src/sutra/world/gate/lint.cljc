(ns sutra.world.gate.lint
  (:require [sutra.world.db :as db]
            [sutra.world.gate.base :as b]
            [sutra.world.gate.near :as n]
            [sutra.world.index :as ix]
            [sutra.types.entities :as ents]
            [sutra.types.kinds :as kinds]
            [sutra.types.refs :as refs]))

(defn- seqable [x] (cond (nil? x) [] (sequential? x) x :else [x]))

(defn- log-names
  [a]
  (let [lt (when (map? (:action-log a)) (get-in a [:action-log :object-type]))]
    (cond-> [(str (ix/nm (or (:action-name a) (:id a))) "-log")]
      (some? lt) (conj (ix/nm lt)))))

(def fact-rows
  [{:id :action-logs :sections #{:actions}
    :facts (fn [_ a] (for [l (log-names a)] [:log l]))}
   {:id :shelves :sections #{:workspaces}
    :facts (fn [_ w] (cons [:shelf]
                           (for [v (concat (seqable (:apps w)) (seqable (:anchored w))
                                           [(:landing w)])
                                 :when (some? v)]
                             [:carries (ix/nm v)])))}
   {:id :dataset-writes :sections #{:transforms}
    :facts (fn [_ t] (for [d (concat [(when (map? (:output t)) (:dataset (:output t)))
                                      (:dataset t)]
                                     (keep #(when (map? %) (:dataset %)) (seqable (:outputs t))))
                           :when (some? d)]
                       [:writes (str d)]))}
   {:id :backs :sections #{:object-types}
    :facts (fn [_ ot] (for [d [(:dataset ot) (when (map? (:backing ot)) (:dataset (:backing ot)))]
                            :when (some? d)]
                        [:backs (str d)]))}])

(defn by-name
  [r db sec v]
  (if (nil? v)
    r
    (-> r
        (n/see [[sec v] [sec (str v)] [sec (ix/nm v)]])
        (n/ask db [[:by-name-str [sec (ix/nm v)]]]))))

(defn names-in
  [r db e secs]
  (reduce (fn [r v] (reduce #(by-name %1 db %2 v) r secs)) r (n/leaves e)))

(defn own-names
  [db sec k]
  (let [e (db/entry db [sec k])]
    (into #{(ix/nm k)} (keep #(some-> (get e %) ix/nm) [(ix/name-field sec) :name]))))

(defn logs-of
  [r db sec k]
  (n/ask r db (for [nm (own-names db sec k)] [[:gate-facts :action-logs] [:log nm]])))

(defn self [sec] (fn [_ k] (n/see n/none [[sec k]])))

(defn- primary-key-near
  [db k]
  (-> n/none
      (n/see [[:object-types k]])
      (n/ask db (for [v (b/ref-forms db :object-types k)] [:edges [:output :object-type v]]))))

(defn- plural-near
  [db k]
  (-> n/none (n/see [[:object-types k]]) (logs-of db :object-types k)))

(defn- sync-near
  [db k]
  (n/see n/none (cons [:transforms k]
                      (for [o (ix/outputs-of (db/entry db [:transforms k])) :when (map? o)]
                        [:object-types (:object-type o)]))))

(defn- action-types-near
  [db k]
  (names-in (n/see n/none [[:actions k]]) db (db/entry db [:actions k]) [:object-types]))

(defn- fk-near
  [db k]
  (let [fs (for [v (b/ref-forms db :object-types k)] [:edges [:link-end v]])
        ls (n/found db fs)]
    (-> (n/see n/none [[:object-types k]])
        (names-in db (db/entry db [:object-types k]) [:object-types])
        (n/ask db fs)
        (n/see (for [l ls
                     end (keep (db/entry db [:link-types l]) [:from-object-type :to-object-type])]
                 [:object-types end])))))

(defn- shelf-near
  [db k]
  (-> (n/see n/none [[:applications k]])
      (n/ask-first db [[:gate-facts :shelves] [:shelf]])
      (n/ask db (for [nm (own-names db :applications k)]
                  [[:gate-facts :shelves] [:carries nm]]))))

(defn- app-names-near
  [db k]
  (names-in (n/see n/none [[:applications k]]) db (db/entry db [:applications k])
            [:applications :actions]))

(defn- outputs-of
  [e]
  (cond (seq (:outputs e)) (:outputs e)
        (map? (:output e)) [(:output e)]
        :else              []))

(defn- inputs-of
  [e]
  (let [i (:inputs e)]
    (cond (map? i)   (vals i)
          (seq i)    i
          (:input e) [(:input e)]
          :else      [])))

(defn- placed
  [r db e]
  (-> r
      (by-name db :repositories (:repository e))
      (as-> r (reduce (fn [r step]
                        (-> r
                            (by-name db :object-types (:object-type step))
                            (by-name db :link-types (or (:link step) (:link-type step)))
                            (by-name db :functions (:function step))))
                      r (filter map? (refs/steps (:rules e)))))
      (as-> r (reduce #(by-name %1 db :object-types (when (map? %2) (:type %2)))
                      r (vals (when (map? (:params e)) (:params e)))))
      (by-name db :object-types (:from-object-type e))
      (by-name db :object-types (:object-type e))
      (by-name db :object-types (:root e))
      (as-> r (reduce #(by-name %1 db :datasets (when (map? %2) (:dataset %2)))
                      r (inputs-of e)))
      (as-> r (reduce (fn [r o]
                        (let [[sec v] (when (map? o) (kinds/output-target o))]
                          (if sec (by-name r db sec v) r)))
                      r (outputs-of e)))))

(defn- dataset-placed
  [r db _sec k]
  (let [writes [[[:gate-facts :dataset-writes] [:writes (str k)]]]
        backs  [[[:gate-facts :backs] [:backs (str k)]]]]
    (as-> (n/ask r db (concat writes backs)) r
      (reduce #(placed %1 db (db/entry db [:transforms %2])) r (n/found db writes))
      (reduce #(logs-of %1 db :object-types %2) r (n/found db backs)))))

(def ^:private placed-beyond
  {:object-types logs-of
   :datasets     dataset-placed})

(defn- project-near
  [sec]
  (fn [db k]
    (let [r (placed (n/see n/none [[sec k]]) db (db/entry db [sec k]))]
      (if-let [f (placed-beyond sec)] (f r db sec k) r))))

(def ^:private member-sections
  (into [] (for [e ents/entities :when (contains? (:fields e) :project)] (:section e))))

(def ^:private rules
  {:missing-primary-key                {:object-types primary-key-near}
   :object-type-plural-missing         {:object-types plural-near}
   :source-syncs-into-ontology         {:transforms sync-near}
   :action-rule-op-unknown             {:actions (self :actions)}
   :action-override-misleading         {:actions (self :actions)}
   :action-param-without-name          {:actions (self :actions)}
   :prefill-type-mismatch              {:actions action-types-near}
   :action-writes-derived              {:actions action-types-near}
   :fk-without-link                    {:object-types fk-near}
   :app-outside-every-workspace        {:applications shelf-near}
   :app-pages-serve-disjoint-audiences {:applications (self :applications)}
   :open-app-misses-the-interface      {:applications app-names-near}
   :view-sidebar-names-nothing         {:applications app-names-near}
   :resource-without-project           (into {} (for [s member-sections] [s (project-near s)]))})

(def specs
  (vec (for [[rule by-sec] (sort-by key rules)
             [sec near] (sort-by key by-sec)]
         {:id (keyword (str "lint." (name rule)) (name sec)) :gate :lint-gate-clean?
          :rule :lint/gate :lint true :section sec :near near :check [:lint rule]})))

(def rule-ids
  (set (keys rules)))
