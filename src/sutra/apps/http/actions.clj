(ns sutra.apps.http.actions
  (:require [sutra.kit.noted :as noted]
            [sutra.capps.intent :as ci]
            [sutra.builds.api :as builds]
            [sutra.foundation.encoding :refer [coerce-uuids]]
            [sutra.objects.api :as objects]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [sutra.apps.http.delivery :refer [app-data-projection request-vars]]
            [sutra.apps.http.standard-view :refer [app-row on-rid]]
            [sutra.http.provenance :as provenance]
            [sutra.http.responses :refer [json-response]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.store.api :as api]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.tx-vector :as tv]
            [sutra.types.submission :as csub]
            [sutra.governance.api :as governance]
            [sutra.actions.api :as actions-api]
            [sutra.store.seat :as seat]
            [sutra.capps.catalog :as cat]
            [sutra.apps.http.session :refer [scenario-at scenario-schema with-app]]
            [sutra.apps.http.view :refer [app-read app-read-want app-view-rids
                                          app-with-properties view-layouts]]))

(set! *warn-on-reflection* true)

(defn- tree-action-entries
  [tree]
  (into []
        (comp (mapcat (fn [n] (if (:boxes n) [n] (cat/widget-action-entries n))))
              (filter (fn [n] (let [a (:action n)]
                                (or (keyword? a) (and (string? a) (seq a))))))
              (map (fn [n] {:action (:action n)})))
        (tree/nodes tree)))

(defn- app-view-actions
  [auth schema a]
  (let [rids (app-view-rids auth schema a)]
    (-> []
        (into (mapcat tree-action-entries) (view-layouts schema rids))
        (into (keep (fn [[an act]]
                      (when (some-> (on-rid schema act) rids)
                        {:action an})))
              (lookup/actions-by-name schema)))))

(defn app-action-set
  ([a] (vec (or (get-in a [:layout :actions])
                (some-> (get-in a [:layout :action]) vector))))
  ([auth schema a]
   (let [own  (app-action-set a)
         seen (into #{} (map #(->kw (:action %))) own)]
     (into own
           (comp (remove #(seen (->kw (:action %))))
                 (distinct))
           (app-view-actions auth schema a)))))

(defn app-action-params
  [auth schema a]
  (into {}
        (keep (fn [entry]
                (let [nm       (->kw (:action entry))
                      resolved (actions/resolve-action schema nm)
                      declared (:params resolved)
                      allowed  (or (some->> (:parameters entry) (mapv ->kw) not-empty)
                                   (vec (keys declared)))]
                  (when (and nm (seq declared))
                    [nm (csub/with-param-formats
                          (csub/action-param-descriptors declared allowed)
                          schema resolved)]))))
        (app-action-set auth schema a)))

(defn app-action-forms
  [auth schema a]
  (into {}
        (keep (fn [entry]
                (let [nm       (->kw (:action entry))
                      resolved (actions/resolve-action schema nm)
                      declared (:params resolved)
                      allowed  (or (some->> (:parameters entry) (mapv ->kw) not-empty)
                                   (vec (keys declared)))]
                  (when (and nm (seq declared))
                    [nm (csub/form-sections
                         (:form resolved)
                         (csub/action-param-descriptors declared allowed))]))))
        (app-action-set auth schema a)))

(defn app-action-intents
  [auth schema a]
  (into {}
        (keep (fn [entry]
                (let [nm (->kw (:action entry))]
                  (when-let [i (some->> nm (actions/resolve-action schema) ci/action-intent)]
                    [nm i]))))
        (app-action-set auth schema a)))

(defn app-action-evaluated
  [auth schema a]
  (into []
        (keep (fn [entry]
                (let [nm (->kw (:action entry))
                      r  (when nm (actions/resolve-action schema nm))]
                  (when (and r (csub/evaluated?
                                r (csub/action-param-descriptors (:params r))))
                    nm))))
        (app-action-set auth schema a)))

(defn- option-rows-reader
  [store sch]
  (fn [query label]
    (:rows (objects/run-object-query store sch (cond-> query label (assoc :want #{label}))))))

(defn resolve-param-options
  [store sch declared allowed]
  (let [rows-of (option-rows-reader store sch)
        sets (into {}
                   (keep (fn [p]
                           (when-let [src (actions-api/param-option-query sch (get declared p))]
                             [p src])))
                   allowed)]
    (when (seq sets)
      (into {}
            (keep (fn [[p {:keys [query label]}]]
                    (try
                      [p (actions-api/options-page label (rows-of query label))]
                      (catch clojure.lang.ExceptionInfo e
                        (noted/note (str "options of parameter " p) e)))))
            sets))))

(defn app-optioned-actions
  [auth schema a]
  (into []
        (keep (fn [entry]
                (let [nm (->kw (:action entry))]
                  (when (some #(actions-api/param-option-query schema %)
                              (vals (:params (actions/resolve-action schema nm))))
                    nm))))
        (app-action-set auth schema a)))

(defn- object-param-type
  [schema ptype]
  (and ptype (lookup/->type-rid schema :object-types ptype)))

(defn- created-objects
  [store branch sb schema bsch ops]
  (let [by-type (reduce (fn [m op]
                          (if (and (= :create-object (:op op)) (tv/store-write? op))
                            (let [t   (:type op)
                                  rid (if (get-in schema [:object-types t])
                                        t
                                        (lookup/->type-rid bsch :object-types t))
                                  id  (some-> (get-in op [:payload :id]) str)]
                              (if (and rid id) (update m rid (fnil conj []) id) m))
                            m))
                        {} ops)]
    (when (seq by-type)
      (app-read store branch sb
                (fn [store sch]
                  (vec (for [[rid ids] by-type
                             :let [nm (get-in sch [:object-types rid :object-type-name])]
                             :when nm
                             r (:rows (objects/run-object-query store sch {:find nm :ids ids}))]
                         (app-row rid r))))))))

(defn- chosen-action
  [act-set req-act]
  (if req-act
    (first (filter #(= req-act (->kw (:action %))) act-set))
    (first act-set)))

(defn- supplied-args
  [act aspec supplied]
  (into {} (keep (fn [k] (when-some [v (get supplied k)] [k v])))
        (or (some->> (:parameters act) (mapv ->kw) not-empty)
            (vec (keys (:params aspec))))))

(defn- version-claims
  [bsch aspec args body]
  (let [expected (some->> (:expectedVersions body) (into {} (map (fn [[k v]] [(name k) v]))))]
    (into []
          (keep (fn [[k v]]
                  (let [otype (object-param-type bsch (get-in aspec [:params k :type]))
                        exp   (when (and otype expected) (get expected (str v)))]
                    (when (some? exp) {:id v :type otype :expected exp}))))
          args)))

(defn- stale-claims
  [claims versions]
  (into []
        (keep (fn [{:keys [id expected]}]
                (let [cur (get versions (str id))]
                  (when (not= expected cur) {:id id :expected expected :actual cur}))))
        claims))

(defn- versions-of
  [branch-store claims]
  (into {} (map (fn [{:keys [id type]}]
                  [(str id) (api/object-version
                             (first (sub/read-objects branch-store type [(str id)])))]))
        claims))

(defn submit-action!
  [branch-store edge auth run-branch run-sch action-name args]
  (let [{:keys [result]}
        (anomaly/unwrap!
         (actions-api/apply-action
          {:action action-name :args args :authority auth :mode :dispatch
           :branch run-branch :schema run-sch :edge edge
           :store  (governance/write-guard branch-store edge run-branch nil {:data? true})}))]
    (builds/announce-action! branch-store run-branch run-sch result)
    result))

(defn submission-refusal
  [branch-store edge auth run-branch run-sch action-name args]
  (try
    (let [r (actions-api/apply-action
             {:action action-name :args args :authority auth :mode :dry-run
              :branch run-branch :schema run-sch :edge edge :store branch-store})]
      (when (anomaly/refusal? r) r))
    (catch clojure.lang.ExceptionInfo e
      (or (anomaly/of e) (throw e)))))

(defn- refused-run
  [req e aspec args]
  (if-let [r (anomaly/of e)]
    (provenance/with-origin
      req
      (json-response (anomaly/status r)
                     {:error    (anomaly/message r)
                      :category (anomaly/category r)
                      :rejected (select-keys (anomaly/data r)
                                             [:action :failed-criterion :missing
                                              :sutra/acl-denied])})
      (provenance/with-sent req r #(:params aspec) args))
    (throw e)))

(defn- held-kinds
  [result]
  (into [] (comp (map :kind) (distinct)) (:suppressed-side-effects result)))

(defn my-application-submit
  [req]
  (with-app req {:json? true :branch? true :seg :query :preview? true}
    (fn [{:keys [schema a sb body scope store auth edge] req-branch :branch}]
      (let [act-set (app-action-set auth schema a)
            req-act (some-> (:action body) ->kw)
            act     (chosen-action act-set req-act)]
        (cond
          (empty? act-set)
          (json-response 400
                         {:error "diese App erlaubt keine Action-Submit (:layout :action fehlt)"})

          (nil? act)
          (json-response 400 {:error (str "Action nicht in dieser App freigegeben: " req-act)})

          :else
          (let [run-branch   (or sb req-branch)
                branch-store (ctx/at store run-branch)
                bsch         (scenario-schema store schema sb)
                action-name  (->kw (:action act))
                aspec        (actions/resolve-action schema action-name)
                args         (supplied-args act aspec (:parameters body))
                claims       (version-claims bsch aspec args body)
                stale        (stale-claims claims (versions-of branch-store claims))]
            (cond
              (nil? aspec)
              (json-response 400 {:error (str "unbekannte Action: " action-name)})

              (seq stale)
              (json-response 409
                             {:error (str "StaleObject: Objekt wurde zwischenzeitlich geändert"
                                          " — bitte neu laden")
                              :sutra/stale-object true :conflicts stale})

              :else
              (try
                (let [run-sch (if sb bsch (seat/branch-schema branch-store))
                      result  (submit-action! branch-store edge auth run-branch run-sch
                                              action-name (coerce-uuids args))]
                  (json-response
                   200
                   {:ok        true :action action-name
                    :action-id (:action-id result)
                    :data      (let [vals (request-vars req)]
                                 (app-data-projection branch-store bsch a
                                                      (app-with-properties a vals)
                                                      (app-read-want auth schema scope a)
                                                      vals))
                    :created   (or (created-objects store req-branch sb schema bsch
                                                    (:tx-vector result))
                                   [])
                    :held-back (held-kinds result)}))
                (catch clojure.lang.ExceptionInfo e (refused-run req e aspec args))))))))))

(defn my-application-action-params
  [req]
  (with-app req {:json? true :branch? true :seg :query :preview? true}
    (fn [{:keys [schema branch a sb body store auth]}]
      (let [act-set (app-action-set auth schema a)
            req-act (some-> (:action body) ->kw)
            act     (chosen-action act-set req-act)]
        (cond
          (empty? act-set)
          (json-response 400 {:error "diese App erlaubt keine Action (:layout :action fehlt)"})

          (nil? act)
          (json-response 400 {:error (str "Action nicht in dieser App freigegeben: " req-act)})

          :else
          (let [action-name (->kw (:action act))
                aspec       (actions/resolve-action schema action-name)
                search      (update-vals (supplied-args act aspec (:search body)) str)
                args        (coerce-uuids (supplied-args act aspec (:parameters body)))]
            (if (nil? aspec)
              (json-response 400 {:error (str "unbekannte Action: " action-name)})
              (try
                (json-response
                 200
                 (assoc
                  (actions-api/evaluate-action-params!
                   (scenario-at store branch sb) (scenario-schema store schema sb)
                   action-name args search)
                  :action action-name))
                (catch clojure.lang.ExceptionInfo e
                  (if-let [r (anomaly/of e)]
                    (json-response (anomaly/status r)
                                   {:error (anomaly/message r) :category (anomaly/category r)})
                    (throw e)))))))))))
