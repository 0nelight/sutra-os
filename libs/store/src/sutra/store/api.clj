(ns sutra.store.api
  (:refer-clojure :exclude [read])
  (:require [malli.core :as m]
            [sutra.kit.anomaly :as anomaly]))

(set! *warn-on-reflection* true)

(def op-descriptors
  {:create-object   {:object? true  :ledger :APPEND :edit-kind :create :authorized-as :create
                     :payload [:map [:id some?] [:values [:maybe :map]]
                               [:markings {:optional true} [:maybe [:sequential :keyword]]]
                               [:stamps {:optional true} [:maybe [:sequential :map]]]]}
   :update-object   {:object? true  :ledger :APPEND :edit-kind :update :version-checked? true
                     :authorized-as :update
                     :payload [:map [:id some?] [:values [:maybe :map]]
                               [:stamps {:optional true} [:maybe [:sequential :map]]]]}
   :update-markings {:object? true  :ledger :APPEND :edit-kind :update :version-checked? true
                     :payload [:map [:id some?]
                               [:add    {:optional true} [:maybe [:sequential :keyword]]]
                               [:remove {:optional true} [:maybe [:sequential :keyword]]]]}
   :delete-object   {:object? true  :ledger :DELETE :edit-kind :delete :version-checked? true
                     :authorized-as :delete-object
                     :payload [:map [:id some?]]}
   :create-link     {:link? true    :ledger :APPEND :edit-kind :connect :authorized-as :create-link
                     :payload [:map [:id some?] [:from-id some?] [:to-id some?]
                               [:markings {:optional true} [:maybe [:sequential :keyword]]]]}
   :delete-link     {:link? true    :ledger :DELETE :edit-kind :disconnect
                     :authorized-as :delete-link
                     :payload [:map [:id some?]]}})

(def authority-schema
  [:map
   [:denied-markings {:optional true} [:maybe [:set any?]]]
   [:hidden-object-types {:optional true} [:maybe [:set any?]]]
   [:system? {:optional true} :boolean]])

(def unrestricted
  {:system? true})

(def reserved-keys
  #{:id :markings :object-type-ref :link-type-ref})

(defn object-version
  [obj]
  (when obj (hash (into {} (remove (comp nil? val)) (apply dissoc (:values obj) reserved-keys)))))

(defprotocol Db
  (-snapshot [db basis])
  (-prepare [db tx])
  (-branch! [db op])
  (-read-at [db basis authority query]))

(defprotocol Snapshot
  (-basis [snap])
  (-answer [snap authority query])
  (-overlay [snap staged])
  (-close [snap]))

(defprotocol Prepared
  (-commit! [p])
  (-abort! [p]))

(def refusals
  {:malformed         {:category :incorrect     :retryable? false}
   :unsupported       {:category :incorrect     :retryable? false}
   :as-of-unsupported {:category :incorrect     :retryable? false}
   :no-authority      {:category :forbidden     :retryable? false}
   :no-branch         {:category :not-found     :retryable? false}
   :conflict          {:category :conflict      :retryable? true}
   :stale-object      {:category :conflict      :retryable? true}
   :object-key-taken  {:category :conflict      :retryable? false}
   :lifecycle-edge    {:category :unprocessable :retryable? false}
   :fenced            {:category :conflict      :retryable? false}
   :snapshot-expired  {:category :conflict      :retryable? true}
   :prepare-expired   {:category :conflict      :retryable? true}
   :unavailable       {:category :unavailable   :retryable? true}
   :unindexed         {:category :unavailable   :retryable? true}
   :read-only         {:category :forbidden     :retryable? false}})

(defn refused
  [kind msg data]
  (let [{:keys [category retryable?]} (refusals kind)]
    (anomaly/refused category msg (assoc data :store/refusal kind :retryable? retryable?))))

(defn refusal
  ([kind msg data] (refusal kind msg data nil))
  ([kind msg data cause] (anomaly/ex (refused kind msg data) cause)))

(defn refuse!
  ([kind msg] (refuse! kind msg {}))
  ([kind msg data] (throw (refusal kind msg data))))

(def auto
  ::auto)

(def absent
  ::absent)

(def snapshot-basis-schema
  [:map [:branch :string] [:rev {:optional true} some?] [:lane {:optional true} [:maybe :keyword]]])

(def tx-basis-schema
  [:map [:branch :string] [:rev some?]])

(def fence-schema
  [:multi {:dispatch #(when (vector? %) (first %))}
   [:head      [:tuple [:= :head]]]
   [:schema    [:tuple [:= :schema]]]
   [:type      [:tuple [:= :type] some?]]
   [:link-type [:tuple [:= :link-type] some?]]
   [:object    [:tuple [:= :object] some? some? any?]]
   [:record    [:tuple [:= :record] :qualified-keyword some? any?]]
   [:lease     [:tuple [:= :lease] some? :int]]
   [:branch-head [:tuple [:= :branch-head] :string :int]]])

(def origins
  #{:edit :index :system :direct})

(def migration-step-schema
  [:multi {:dispatch #(when (vector? %) (first %))}
   [:drop-type        [:tuple [:= :drop-type] some?]]
   [:drop-link-type   [:tuple [:= :drop-link-type] some?]]
   [:drop-attribute   [:tuple [:= :drop-attribute] some? :keyword]]
   [:rename-attribute [:tuple [:= :rename-attribute] some? :keyword :keyword]]
   [:retype-attribute [:tuple [:= :retype-attribute] some? :keyword any? any?]]
   [:clear-attribute  [:tuple [:= :clear-attribute] some? :keyword]]
   [:relayout         [:tuple [:= :relayout]]]
   [:drop-unclaimed   [:tuple [:= :drop-unclaimed]]]])

(defn reducible?
  [x]
  (or (nil? x) (sequential? x) (instance? clojure.lang.IReduceInit x) (instance? Iterable x)))

(def store-op-schema
  (into [:multi {:dispatch :op}]
        (for [[op {:keys [payload]}] op-descriptors]
          [op [:map [:op [:= op]] [:type some?] [:payload payload]
               [:expected-version {:optional true} any?]]])))

(def tx-map-schema
  [:map {:closed true}
   [:basis tx-basis-schema]
   [:lane {:optional true} [:maybe :keyword]]
   [:actor [:map [:principal any?] [:run {:optional true} any?]
            [:action {:optional true} :string] [:action-id {:optional true} :string]
            [:agent {:optional true} :string] [:agent-run {:optional true} :string]]]
   [:message {:optional true} :string]
   [:parents {:optional true} [:vector snapshot-basis-schema]]
   [:reparent {:optional true} snapshot-basis-schema]
   [:retires {:optional true} [:vector [:map {:closed true} [:branch :string] [:expect-rev some?]]]]
   [:schema {:optional true}
    [:or [:map [:authored map?] [:migration [:vector migration-step-schema]]]
     [:map {:closed true} [:changes vector?] [:migration [:vector migration-step-schema]]]]]
   [:ops {:optional true} [:fn reducible?]]
   [:log {:optional true} [:vector [:map [:kind :qualified-keyword] [:payload any?]
                                    [:outbox? {:optional true} :boolean]]]]
   [:records {:optional true} [:vector [:map [:coll :qualified-keyword] [:key some?]
                                        [:expect {:optional true} any?] [:value any?]]]]
   [:fences {:optional true} [:vector fence-schema]]
   [:origin {:optional true} (into [:enum] origins)]
   [:forget-edits {:optional true} [:vector some?]]
   [:adopt-edits {:optional true} [:vector [:map {:closed true} [:type some?] [:id some?]
                                            [:kind [:enum :create :update :delete]]
                                            [:values {:optional true} map?]
                                            [:at {:optional true} nat-int?]]]]
   [:restore {:optional true}
    [:map {:closed true} [:rev some?] [:types {:optional true} [:set some?]]]]
   [:touched {:optional true}
    [:vector [:or [:tuple [:= :type] some?] [:tuple [:= :link-type] some?]]]]])

(def branch-op-schema
  [:multi {:dispatch :op}
   [:fork     [:map {:closed true} [:op [:= :fork]] [:name :string] [:from snapshot-basis-schema]
               [:meta {:optional true} map?]]]
   [:retire   [:map {:closed true} [:op [:= :retire]] [:name :string] [:expect-rev some?]]]
   [:adopt    [:map {:closed true} [:op [:= :adopt]] [:name :string] [:from snapshot-basis-schema]
               [:expect-rev {:optional true} some?] [:meta {:optional true} map?]]]
   [:set-meta [:map {:closed true} [:op [:= :set-meta]] [:name :string] [:meta map?]
               [:expect-meta {:optional true} any?]]]
   [:repair-schema-log [:map {:closed true} [:op [:= :repair-schema-log]] [:name :string]]]])

(def collection-schema
  [:map [:coll :qualified-keyword] [:owner {:optional true} symbol?]
   [:scope [:enum :branch :global]] [:key some?] [:value some?]
   [:auto {:optional true} :keyword]
   [:unique {:optional true} [:vector [:vector :keyword]]]
   [:index {:optional true} [:vector [:or :keyword [:tuple :keyword [:= :range]]]]]])

(def records-list-opts-schema
  [:map {:closed true}
   [:prefix {:optional true} :string]
   [:index {:optional true} [:tuple :keyword any?]]
   [:where {:optional true} [:map-of :keyword any?]]
   [:range {:optional true} [:tuple :keyword any? any?]]
   [:order {:optional true} [:tuple :keyword [:enum :asc :desc]]]
   [:after {:optional true} any?]
   [:limit {:optional true} nat-int?]
   [:count? {:optional true} :boolean]
   [:project {:optional true} [:set :keyword]]
   [:group-by {:optional true} :keyword]
   [:pick {:optional true} [:or [:map {:closed true} [:by [:vector :keyword]]
                                 [:max [:vector :keyword]]]
                            [:map {:closed true} [:by [:vector :keyword]]
                             [:min [:vector :keyword]]]]]])

(def changes-opts-schema
  [:map {:closed true}
   [:ids {:optional true} [:sequential some?]]
   [:after {:optional true} some?]
   [:upto {:optional true} some?]
   [:origin {:optional true} (into [:enum] origins)]
   [:limit {:optional true} nat-int?]])

(def overlay-schema
  [:or [:sequential map?]
   [:map {:closed true} [:world {:optional true} map?]
    [:migration {:optional true} [:vector migration-step-schema]]
    [:ops {:optional true} [:sequential map?]]]])

(def answer-query-schema
  [:multi {:dispatch #(when (vector? %) (first %))}
   [:store/capabilities [:tuple [:= :store/capabilities]]]
   [:branch/list        [:tuple [:= :branch/list]]]
   [:branch/get         [:tuple [:= :branch/get] :string]]
   [:branch/tips        [:tuple [:= :branch/tips] [:sequential :string]]]
   [:schema/world       [:or [:tuple [:= :schema/world]]
                         [:tuple [:= :schema/world]
                          [:map {:closed true} [:derived? {:optional true} :boolean]]]]]
   [:schema/history     [:or [:tuple [:= :schema/history]]
                         [:tuple [:= :schema/history]
                          [:map {:closed true} [:schema? {:optional true} :boolean]
                           [:after {:optional true} some?] [:before {:optional true} some?]
                           [:limit {:optional true} nat-int?]]]]]
   [:schema/commits     [:tuple [:= :schema/commits]
                         [:map {:closed true} [:after {:optional true} nat-int?]
                          [:limit {:optional true} nat-int?]
                          [:newest? {:optional true} :boolean]]]]
   [:schema/removed     [:tuple [:= :schema/removed]
                         [:map {:closed true} [:limit {:optional true} pos-int?]]]]
   [:schema/last-changed [:tuple [:= :schema/last-changed]
                          [:map {:closed true} [:limit {:optional true} pos-int?]
                           [:after {:optional true} nat-int?]]]]
   [:schema/resource-history [:tuple [:= :schema/resource-history] [:tuple :keyword some?]]]
   [:schema/changed     [:tuple [:= :schema/changed]
                         [:map {:closed true} [:after {:optional true} nat-int?]]]]
   [:store/layout       [:tuple [:= :store/layout]]]
   [:objects/changes    [:tuple [:= :objects/changes] some? changes-opts-schema]]
   [:objects/clock      [:tuple [:= :objects/clock] some?]]
   [:objects/count      [:tuple [:= :objects/count] some?]]
   [:objects/edited     [:or [:tuple [:= :objects/edited] some?]
                         [:tuple [:= :objects/edited] some? [:maybe [:sequential some?]]]]]
   [:objects            [:or [:tuple [:= :objects] some?]
                         [:tuple [:= :objects] some? [:maybe [:sequential any?]]]]]
   [:links              [:or [:tuple [:= :links] some?] [:tuple [:= :links] some? map?]]]
   [:select             [:tuple [:= :select] map?]]
   [:select/explain     [:tuple [:= :select/explain] map?]]
   [:log/range          [:or [:tuple [:= :log/range]] [:tuple [:= :log/range] map?]]]
   [:records/get        [:tuple [:= :records/get] :qualified-keyword some?]]
   [:records/clock      [:tuple [:= :records/clock] :qualified-keyword]]
   [:records/list       [:or [:tuple [:= :records/list] :qualified-keyword]
                         [:tuple [:= :records/list] :qualified-keyword records-list-opts-schema]]]
   [::m/default         [:and vector? [:cat :keyword [:* any?]]]]])

(def object-queries #{:objects :links :select :select/explain :objects/changes :objects/edited
                      :objects/clock :objects/count})

(def ^:private validator (memoize m/validator))

(defn- check! [schema x what]
  (when-let [e (and (not ((validator schema) x)) (m/explain schema x))]
    (refuse! :malformed (str "store: malformed " (name what)) {:explain e what x}))
  x)

(defn check-op!
  [op]
  (check! store-op-schema op :op))

(defn snapshot
  ^java.io.Closeable [db basis]
  (-snapshot db (check! snapshot-basis-schema basis :basis)))

(defn basis [snap] (-basis snap))

(defn close [snap] (-close snap))

(defn overlay
  [snap staged]
  (check! overlay-schema staged :overlay)
  (-overlay snap (if (map? staged)
                   (update staged :ops #(mapv check-op! %))
                   {:ops (mapv check-op! staged)})))

(defn prepare!
  [db tx]
  (-prepare db (check! tx-map-schema tx :tx)))

(defn commit! [p] (-commit! p))

(defn abort! [p] (-abort! p))

(defn branch!
  [db op]
  (-branch! db (check! branch-op-schema op :branch-op)))

(defn- check-query!
  [authority query]
  (check! answer-query-schema query :query)
  (when (object-queries (first query))
    (when (nil? authority)
      (refuse! :no-authority "an object read needs an authority (fail-closed)" {:query query}))
    (check! authority-schema authority :authority)))

(defn read
  ([snap authority query]
   (check-query! authority query)
   (-answer snap authority query)))

(defn read-at
  [db basis authority query]
  (check! snapshot-basis-schema basis :basis)
  (when-not (:rev basis)
    (refuse! :malformed "read-at names a revision; a snapshot resolves the head" {:basis basis}))
  (check-query! authority query)
  (-read-at db basis authority query))

(defn try-transact!
  [db tx]
  (anomaly/then (prepare! db tx)
                (fn [p]
                  (let [r (try (anomaly/catching #(commit! p))
                               (catch Throwable e (abort! p) (throw e)))]
                    (when (anomaly/refusal? r) (abort! p))
                    r))))

(defn transact!
  [db tx]
  (anomaly/unwrap! (try-transact! db tx)))

(defn capabilities
  [db branch]
  (with-open [s ^java.io.Closeable (snapshot db {:branch branch})]
    (read s nil [:store/capabilities])))

(defn check-collection!
  [decl]
  (check! collection-schema decl :collection))

(def ^:dynamic *tally*
  nil)

(defn tally!
  [op x]
  (when-let [t *tally*]
    (swap! t (fn [m]
               (let [m (update-in m [:calls op] (fnil inc 0))]
                 (cond
                   (counted? x) (update-in m [:rows op] (fnil + 0) (count x))
                   (instance? clojure.lang.ISeq x) (update-in m [:uncounted op] (fnil inc 0))
                   :else m)))))
  x)

(defn tallying
  [f]
  (let [t (atom {})]
    (binding [*tally* t] [(f) @t])))

(defn totals
  [{:keys [calls rows uncounted]}]
  (cond-> {:calls (reduce + 0 (vals calls)) :rows (reduce + 0 (vals rows))}
    (seq uncounted) (assoc :uncounted (reduce + 0 (vals uncounted)))))
