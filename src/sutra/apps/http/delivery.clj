(ns sutra.apps.http.delivery
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.settings :as settings]
            [sutra.foundation.encoding :as enc]
            [clojure.string :as str]
            [sutra.capps.bind :as bind]
            [sutra.capps.walk :as walk]
            [sutra.capps.catalog :as cat]
            [sutra.capps.layout :as lay]
            [sutra.capps.object-filter :as ofilter]
            [sutra.ref.tokens :as rt]
            [sutra.types.set-query :as sq]
            [sutra.capps.variables :as vars]
            [sutra.capps.visibility :as vis]
            [sutra.foundation.profile :as prof]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [sutra.apps.http.standard-view :refer [app-type-rid]]
            [sutra.store.api :as api]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.objects.api :as objects]
            [sutra.types.link-side :as ls]))

(set! *warn-on-reflection* true)

(defn- resolve-ref
  [ref handles derived data]
  (vars/resolve-ref ref handles derived
                    (vars/with-today data (str (java.time.LocalDate/now)))))

(defn- with-title [schema rid want]
  (let [title (some-> (get-in schema [:object-types rid :title-key]) ->kw)]
    (cond-> want (and title (seq want)) (-> set (conj title)))))

(defn app-bound-objects
  ([store schema t] (app-bound-objects store schema t nil nil))
  ([store schema t extra] (app-bound-objects store schema t extra nil))
  ([store schema t extra want]
   (if-let [r (app-type-rid schema t)]
     (prof/with-mark-result :Rows (fn [rows] {:type-name (str t) :rows (count rows)})
       (mapv #(assoc % :_version (api/object-version %))
             (objects/objects-of-type store schema r
                                      {:derived? true :derived-extra extra
                                       :want (with-title schema r want)
                                       :max-objects (settings/value :app-bind-max)})))
     [])))

(defn- json-param [req k]
  (some-> (get-in req [:query-params k]) not-empty (enc/json-or-nil :key-fn keyword)))

(defn request-vars
  [req]
  (json-param req "vars"))

(defn request-url [req] (json-param req "url"))

(defn request-held [req] (json-param req "held"))

(defn request-more
  [req]
  (not-empty (into {} (keep (fn [[k v]] (when (nat-int? v) [(keyword (name k)) v])))
                   (json-param req "more"))))

(defn layout-vars
  [a]
  (sq/layout-specs (:layout a)))

(def ^:private reads-rows?
  #{:from :agg :object-property :source :aggregate :function :compare :join :default-agg})

(defn- decidable-var?
  [specs evals nm]
  (letfn [(go [seen nm]
            (cond
              (seen nm)            true
              (contains? evals nm) true
              :else
              (let [spec (get specs nm)
                    seen (conj seen nm)]
                (cond
                  (nil? spec)                             false
                  (or (:external-id spec) (:input? spec)) false
                  (some #(contains? spec %) reads-rows?)  false
                  (contains? spec :default)               false
                  :else (every? #(go seen %) (vars/var-deps spec))))))]
    (go #{} nm)))

(defn- gate-verdict
  [specs evals node]
  (cond
    (:hidden? node) false
    (contains? node :when)
    (let [w (:when node)]
      (when (every? #(decidable-var? specs evals %) (vars/var-refs w))
        (vis/truthy? (walk/postwalk #(if (bind/variable-ref? %)
                                       (vars/deref* (resolve-ref % evals specs {}))
                                       %)
                                    w)
                     true)))
    :else true))

(defn- selected-panes
  [specs evals node k]
  (let [panes (get node k)
        sel   (:select node)
        nm    (when (and (map? sel) (or (contains? sel :var) (contains? sel :val)))
                (->kw (or (:var sel) (:val sel))))]
    (when (and (vector? panes) (seq panes) nm
               (every? :id panes)
               (not-any? :visibility panes)
               (decidable-var? specs evals nm))
      (let [v (vars/deref* (resolve-ref {:val nm} evals specs {}))]
        [(or (some #(when (= (:id %) v) %) panes)
             (some #(when (= (->kw (:id %)) (->kw v)) %) panes)
             (first panes))]))))

(defn- prune-invisible
  [specs evals form]
  (cond
    (map? form)
    (when-not (false? (gate-verdict specs evals form))
      (let [form (reduce (fn [f k]
                           (if-let [one (selected-panes specs evals f k)]
                             (assoc f k one)
                             f))
                         form
                         [:pages :tabs])]
        (reduce-kv (fn [m k v] (assoc m k (prune-invisible specs evals v))) {} form)))
    (vector? form) (mapv #(prune-invisible specs evals %) form)
    :else          form))

(defn- if-form? [e]
  (and (vector? e) (= 4 (count e)) (= :if (->kw (first e)))))

(defn- if-tests [e]
  (when (if-form? e)
    (let [[_ c a b] e] (into [c] (mapcat if-tests) [a b]))))

(defn- taken-branch
  [specs evals e]
  (if (and (if-form? e) (every? #(decidable-var? specs evals %) (vars/var-refs (second e))))
    (let [rv (fn [r] (vars/deref* (resolve-ref r evals specs {})))
          [_ c a b] e]
      (recur specs evals (if (= 1 (vars/eval-expr [:if c 1 0] rv)) a b)))
    e))

(defn- read-deps
  [specs evals spec]
  (if (if-form? (:expr spec))
    (vars/var-refs (taken-branch specs evals (:expr spec)))
    (vars/var-deps spec)))

(defn- gate-inputs
  [specs layout]
  (let [gates (concat
               (walk/visit (fn [x _]
                             (for [form [(:when x) (when (or (:pages x) (:tabs x)) (:select x))]
                                   nm   (vars/var-refs form)]
                               nm))
                           layout)
               (for [[_ spec] specs
                     c        (if-tests (:expr spec))
                     nm       (vars/var-refs c)]
                 nm))]
    (loop [todo (seq gates) seen #{}]
      (if-let [nm (first todo)]
        (if (seen nm)
          (recur (next todo) seen)
          (recur (concat (next todo) (vars/var-deps (get specs nm))) (conj seen nm)))
        seen))))

(def ^:private never {:or []})

(defn- base-type [t] (if (vector? t) (first t) t))

(defn- zero-of [t]
  (case (base-type t)
    :boolean                                    false
    (:integer :long :short :byte :double :float
              :decimal :number)                          0
    (:string :text)                             "0"
    nil))

(defn- open-needle? [t]
  (or (nil? t) (and (string? t) (str/blank? t)) (and (coll? t) (empty? t))))

(defn- lower-one
  [types p op t]
  (case op
    :contains (if (open-needle? t) ::open {p {:contains-ci (str t)}})
    :=        (if (nil? t) {p {:nil? true}} {p {:= t}})
    :not=     (if (nil? t) {p {:nil? false}} {:or [{p {:not= t}} {p {:nil? true}}]})
    (:< :> :<= :>=) (if (nil? t) never {p {op t}})
    :in       (cond
                (open-needle? t) ::open
                (coll? t)        {:or (into [] (comp (distinct)
                                                     (map #(if (nil? %) {p {:nil? true}}
                                                               {p {:= %}})))
                                            t)}
                :else            {p {:= t}})
    :truthy   (if-some [z (zero-of (get types p))]
                {:or [{p {:nil? true}} {p {:not= z}}]}
                ::open)
    :falsy    (if-some [z (zero-of (get types p))] {p {:= z}} never)
    (:within-distance :within-bounding-box) (if (map? t) {p {op t}} ::open)
    (refusal/refuse! :refusal.apps/no-store-meaning-for-the {:op op} {:op op})))

(defn- lower-filter
  [types strings vals t]
  (let [x (ofilter/resolved t #(get vals %))
        {:keys [own hops]} (ofilter/->where x {:attr? #(contains? types %) :strings strings})]
    (if (or own hops) {::hops hops ::own own} ::open)))

(defn- lower-clause
  [types strings vals [prop op _] t]
  (let [op    (->kw op)
        props (cond (= :* (->kw prop))                     strings
                    (and (vector? prop)
                         (not (#{:within-distance :within-bounding-box} op))) (map ->kw prop))]
    (cond
      (= :filter op) (lower-filter types strings vals t)
      (= :tokens op) {:matches-tokens-in [(or props [(->kw prop)]) t]}
      (not props)    (lower-one types (if (vector? prop) (mapv ->kw prop) (->kw prop)) op t)
      :else
      (let [parts (map #(lower-one types % op t) props)]
        (cond
          (some #{::open} parts) ::open
          :else (let [ps (into [] (remove #{never}) parts)]
                  (case (count ps) 0 never 1 (first ps) {:or ps})))))))

(defn- target
  [vals [_ _ ref]]
  (if (and (map? ref) (or (contains? ref :val) (contains? ref :var)))
    (let [k (->kw (or (:val ref) (:var ref)))]
      (if (contains? vals k) (get vals k) ::unresolved))
    ref))

(defn- lower
  [schema rid clauses vals]
  (let [ts (map #(target vals %) clauses)]
    (if (some #{::unresolved} ts)
      ::unresolved
      (let [types   (lookup/attr-type-map schema rid)
            strings (into [] (keep (fn [{:keys [attribute-name attribute-type]}]
                                     (when (#{:string :text} (base-type attribute-type))
                                       attribute-name)))
                          (lookup/attrs schema rid))
            parts   (into [] (remove #{::open})
                          (map #(lower-clause types strings vals %1 %2) clauses ts))
            hops    (apply merge-with merge (keep ::hops parts))
            own     (into [] (keep #(if (contains? % ::hops) (::own %) %)) parts)]
        (cond-> (if (seq own) {:and own} {}) hops (merge hops))))))

(def ^:private page-max 5000)

(defn page-query [w {:keys [limit order-by sort-dir offset]}]
  (cond-> {:where w :limit (min page-max (if (pos-int? limit) limit (settings/value :app-page)))}
    order-by          (assoc :order-by (->kw order-by))
    sort-dir          (assoc :sort-dir (->kw sort-dir))
    (pos-int? offset) (assoc :offset offset)))

(defn- shown [schema rid want]
  (when (seq want)
    (not-empty (into [] (filter (set want))
                     (map :attribute-name (lookup/attrs schema rid))))))

(defn- page-order
  [schema rid {:keys [items-order] :as q}]
  (cond-> q
    (and items-order
         (some #(= (:order-by items-order) (->kw (:attribute-name %)))
               (lookup/attrs schema rid)))
    (merge items-order)))

(defn- read-page
  [store schema with want rid root q w]
  (let [want (with-title schema rid want)
        show (shown schema rid want)
        query (cond-> (page-query w (page-order schema rid q)) show (assoc :show show))
        {:keys [rows total]} (objects/page-matching store schema rid query
                                                    {:derived-extra (get with root)
                                                     :want want})]
    {:rows  (mapv #(assoc % :_version (api/object-version %)) rows)
     :total total}))

(defn- read-agg
  [store schema rid w [f prop]]
  (if (= :count f)
    (or (objects/count-matching store schema rid {:where w}) 0)
    (let [v (objects/aggregate-matching store schema rid {:where w}
                                        {:fn (if (= :distinct-count f) :cardinality f)
                                         :attr prop})]
      (if (and (nil? v) (#{:sum :avg} f)) 0 v))))

(defn restricted?
  [e]
  (true? (:sutra/acl-denied (ex-data e))))

(defn restricted-answer
  [q]
  (assoc (if-let [[f] (:agg q)]
           {:value (when (#{:count :distinct-count :sum :avg} f) 0)}
           {:rows [] :total 0})
         :restricted true))

(defmacro ^:private unless-restricted
  [else-expr & body]
  `(try ~@body
        (catch clojure.lang.ExceptionInfo e#
          (if (restricted? e#) ~else-expr (throw e#)))))

(defn- authorize-hops!
  [store schema rid w]
  (doseq [side (remove #{:and :or} (keys w))]
    (let [far (:far-type (ls/resolve-side schema rid side))]
      (grants/authorize-read! (grants/read-rights store)
                              (get-in schema [:object-types far]) far))))

(defn- read-once
  [reads k read]
  (if reads
    @(get (swap! reads #(cond-> % (not (contains? % k)) (assoc k (delay (read))))) k)
    (read)))

(declare frames)

(defn- answer-query
  [store schema with want vals nm rid q reads]
  (let [w (lower schema rid (:clauses q) vals)]
    (when-not (= ::unresolved w)
      (prof/with-mark-result :Rows (fn [a] {:type-name (str nm) :rows (count (:rows a))})
        (try
          (authorize-hops! store schema rid w)
          (read-once reads
                     [rid (:root q) w
                      (select-keys q (conj sq/page-keys :agg :items-order :offset))]
                     #(if-let [agg (:agg q)]
                        {:value (read-agg store schema rid w agg)}
                        (read-page store schema with want rid (:root q) q w)))
          (catch InterruptedException e (throw e))
          (catch Exception e
            (if (restricted? e)
              (restricted-answer q)
              {:refused (str (name nm) ": " (or (ex-message e) (str e))
                             " @ " (frames e))})))))))

(defn- answer
  [store schema bound with want vals nm q reads]
  (let [rid (get bound (:root q))]
    (cond
      (sq/problem q) {:refused (sq/problem q)}
      (nil? rid)     {:refused (str (name nm) " reads " (name (:root q))
                                    ", which this application does not bind")}
      :else          (answer-query store schema with want vals nm rid q reads))))

(defn- bound-types
  [schema a]
  (into {} (keep (fn [t] (let [t (->kw t)] (some->> (app-type-rid schema t) (vector t)))))
        (get-in a [:layout :object-types])))

(defn- query-of [specs bound nm]
  (if (contains? specs nm)
    (or (sq/agg-of specs nm) (sq/set-of specs nm))
    (when (contains? bound nm) {:root nm :clauses []})))

(defn- needed
  [specs bound evals visible]
  (loop [todo (seq (vars/var-refs visible)) seen #{}]
    (if-let [nm (first todo)]
      (if (seen nm)
        (recur (next todo) seen)
        (let [spec (get specs nm)]
          (recur (concat (next todo)
                         (when-not (query-of specs bound nm)
                           (cond-> (vec (read-deps specs evals spec))
                             (:join spec) (conj (->kw (:index (:join spec)))))))
                 (conj seen nm))))
      seen)))

(defn- frames [^Throwable e]
  (->> (.getStackTrace e)
       (keep (fn [^StackTraceElement f]
               (when (str/starts-with? (.getClassName f) "sutra.")
                 (str (.getClassName f) ":" (.getLineNumber f)))))
       (take 4)
       (str/join " < ")))

(declare closure with-answers)

(defn- reads-itself?
  [waits nm]
  (loop [todo (seq (waits nm)) seen #{}]
    (when-let [x (first todo)]
      (cond (= x nm) true
            (seen x) (recur (next todo) seen)
            :else    (recur (concat (next todo) (waits x)) (conj seen x))))))

(defn- answer-rounds
  [store schema specs bound with want vals asked]
  (let [once   (atom {})
        reads  (fn [h] (filter #(contains? asked %) (closure specs h)))
        holes  (fn [q] (filter #(and (not (contains? vals %)) (contains? specs %)
                                     (not= :state (vars/var-kind (get specs %))))
                               (sq/holes q)))
        waits  (into {} (for [[nm q] asked] [nm (into #{} (mapcat reads) (holes q))]))
        cyclic (into #{} (filter #(reads-itself? waits %)) (keys asked))
        cells  (into {} (map (fn [nm] [nm (promise)])) (keys asked))
        ask    (fn [nm q]
                 (let [got   (when-not (cyclic nm)
                               (into {} (map (fn [d] [d @(cells d)])) (waits nm)))
                       data  (with-answers {} (remove (comp nil? val) got))
                       ready (fn [h] (and (not (cyclic nm))
                                          (every? #(some? (get got %)) (reads h))))
                       vals* (into vals
                                   (for [h     (holes q)
                                         :when (ready h)
                                         :let  [v (vars/deref* (resolve-ref
                                                                {:val h} vals specs data))]
                                         :when (not (vars/fn-error? v))]
                                     [h v]))]
                   (answer store schema bound with want vals* nm q once)))
        run    (fn [[nm q]]
                 (prof/span-future
                  (let [a (try (ask nm q)
                               (catch Throwable e
                                 {:refused (str (name nm) ": " (or (ex-message e) (str e))
                                                " @ " (frames e))}))]
                    (deliver (cells nm) a)
                    [nm a])))]
    (into {} (comp (map deref) (remove (comp nil? second))) (mapv run asked))))

(defn- delivered-ids
  [bound asked answers]
  (reduce-kv (fn [acc nm {:keys [rows]}]
               (let [rid (get bound (:root (get asked nm)))]
                 (cond-> acc
                   (and rid (seq rows)) (update rid (fnil into #{}) (map :id rows)))))
             {} answers))

(defn- app-bound-links
  [links-of schema l ids-by-rid]
  (when-let [r (lookup/->type-rid schema :link-types l)]
    (let [lt (get-in schema [:link-types r])
          f  (get ids-by-rid (:from-object-type lt))
          t  (get ids-by-rid (:to-object-type lt))]
      (if-not (or f t)
        []
        (mapv (fn [row]
                {:id   (:id row)
                 :from (or (:from-id row) (:from_id row))
                 :to   (or (:to-id row)   (:to_id row))})
              (links-of r (cond-> {}
                            f (assoc :from-ids (vec f))
                            t (assoc :to-ids   (vec t)))))))))

(defn- visible-of [a specs vals]
  (prune-invisible specs vals (dissoc (:layout a) :vars :object-types :link-types)))

(defn- stale-names
  [a specs bound held vals names]
  (let [changed (into #{} (filter #(not= (get held %) (get vals %)))
                      (into (set (keys held)) (keys vals)))
        before  (needed specs bound held (visible-of a specs held))
        dirty   (into #{} (filter #(or (not (contains? before %))
                                       (some changed (closure specs %))))
                      names)]
    (into dirty (filter (fn [nm] (some #(contains? (closure specs %) nm) dirty))) names)))

(defn app-data-projection
  ([store schema a with want vals] (app-data-projection store schema a with want vals nil))
  ([store schema a with want vals held]
   (app-data-projection store schema a with want vals held nil))
  ([store schema a with want vals held more]
   (let [specs    (layout-vars a)
         bound    (bound-types schema a)
         vals     (or vals {})
         data?    (lay/data-layout? (:layout a))
         names    (cond
                    (seq more) (filter #(query-of specs bound %) (keys more))
                    data?      (needed specs bound vals (visible-of a specs vals))
                    :else      (keys bound))
         partial? (and (empty? more) data? (map? held)
                       (empty? (get-in a [:layout :link-types])))
         names    (if partial? (stale-names a specs bound held vals names) names)
         orders   (sq/items-orders specs (:layout a))
         asked    (into {} (keep (fn [nm] (some->> (query-of specs bound nm)
                                                   (sq/with-items-order (orders nm))
                                                   (#(cond-> % (get more nm)
                                                       (assoc :offset (get more nm))))
                                                   (vector nm))))
                        names)
         answers  (answer-rounds store schema specs bound with want vals asked)
         ids      (delivered-ids bound asked answers)
         links    (->> (get-in a [:layout :link-types]) (keep ->kw) distinct)]
     (doseq [[nm {:keys [refused]}] answers :when refused]
       (println "app" (name (or (:application-name a) "?")) "variable" (name nm)
                "refused:" refused))
     (cond-> (into {} (keep (fn [[nm {:keys [rows value refused]}]]
                              (when-not refused [nm (if rows rows value)])))
                   answers)
       :always (into (map (fn [l] [l (or (unless-restricted []
                                          (app-bound-links
                                           #(objects/links-of-type store schema %1 %2)
                                           schema l ids))
                                         [])]))
                     links)
       (some (fn [[nm {:keys [rows total]}]]
               (and rows (> total (+ (get more nm 0) (count rows)))))
             answers)
       (assoc :_truncated
              (into {} (keep (fn [[nm {:keys [rows total]}]]
                               (when (and rows (> total (+ (get more nm 0) (count rows))))
                                 [nm total])))
                    answers))
       (seq more) (assoc :_more (select-keys more (keys asked)))
       (some (comp :refused val) answers)
       (assoc :_refused (into {} (keep (fn [[nm {:keys [refused]}]]
                                         (when refused [nm refused])))
                              answers))
       partial? (assoc :_partial (vec (keys asked)))))))

(defn type-page
  [store schema t want]
  (if-let [rid (app-type-rid schema t)]
    (unless-restricted [] (:rows (read-page store schema nil want rid t {} {})))
    []))

(defn shown-object-queries
  [schema a vals]
  (let [specs   (layout-vars a)
        bound   (bound-types schema a)
        vals    (or vals {})
        visible (prune-invisible specs vals
                                 (dissoc (:layout a) :vars :object-types :link-types))]
    (for [x     (walk/visit (fn [x _] [x]) visible)
          :when (= :object-view (->kw (:widget x)))
          :let  [ref (:object x)
                 nm  (or (bind/name-in :val ref) (bind/name-in :var ref))
                 q   (when nm (query-of specs bound nm))
                 rid (get bound (:root q))
                 w   (when (and rid (not (:agg q)) (not (sq/problem q)))
                       (lower schema rid (:clauses q) vals))]
          :when (map? w)]
      {:rid rid :query {:where w} :form-factor (or (some-> (:form-factor x) ->kw) :full)})))

(defn- closure
  [specs nm]
  (loop [todo [nm] seen #{}]
    (if-let [x (first todo)]
      (recur (if (seen x) (rest todo) (concat (rest todo) (vars/var-deps (get specs x))))
             (conj seen x))
      seen)))

(defn- state-defaults [specs]
  (into {} (keep (fn [[nm sp]] (when (and (contains? sp :default) (not (map? (:default sp))))
                                 [nm (:default sp)])))
        specs))

(defn- with-answers [payload answered]
  (into payload (keep (fn [[nm {:keys [rows value refused]}]]
                        (when-not refused [nm (if rows rows value)])))
        answered))

(defn- same-answers
  [answered]
  (first (reduce (fn [[same seen] [nm {:keys [rows]}]]
                   (if-let [[_ holder] (some #(when (identical? rows (first %)) %) seen)]
                     [(assoc same nm holder) seen]
                     [same (conj seen [rows nm])]))
                 [{} []]
                 (sort-by key (filter (comp seq :rows val) answered)))))

(defn view-answers
  [store schema hidden layouts payload state]
  (let [specs   (reduce (fn [acc n] (merge acc (:vars n))) {} (mapcat tree/nodes layouts))
        state   (merge (state-defaults specs) (select-keys state (keys specs)))
        want    (into #{} (mapcat cat/referenced-properties) layouts)
        orders  (sq/items-orders specs layouts)
        once    (atom {})
        queries (into {} (keep (fn [nm] (let [q (or (sq/agg-of specs nm) (sq/set-of specs nm))]
                                          (when (and q (or (sq/problem q) (:source? q)))
                                            [nm (sq/with-items-order (orders nm) q)]))))
                      (keys specs))
        open?   (fn [answered nm] (and (contains? queries nm) (not (contains? answered nm))))
        ready?  (fn [answered h] (not-any? #(open? answered %) (closure specs (->kw h))))
        value   (fn [data h] (if (contains? state h)
                               (get state h)
                               (vars/deref* (resolve-ref {:val h} state specs data))))
        rid-of  (fn [root] (or (lookup/->type-rid schema :object-types root)
                               (lookup/->type-rid schema :interfaces root)))
        row?    (fn [q] (some #(#{:item :item-not} (bind/form-id %))
                              (tree-seq coll? seq (:clauses q))))
        run     (fn [data [nm q]]
                  (let [q   (cond-> q
                              (row? q) (update :clauses lay/resolve-item
                                               (first (get data vars/view-object))))
                        vs  (into {} (map (fn [h] [h (value data h)])) (sq/holes q))
                        rid (rid-of (:root q))]
                    (cond
                      (sq/problem q)              {:refused (sq/problem q)}
                      (contains? hidden rid)      (restricted-answer q)
                      (nil? rid)                  {:refused (str (name nm) " reads "
                                                                 (name (:root q))
                                                                 ", which is not readable here")}
                      (some vars/fn-error? (vals vs)) {:refused (str (name nm) ": "
                                                                     (some vars/fn-error-msg
                                                                           (vals vs)))}
                      :else (or (answer-query store schema nil want vs nm rid q once)
                                {:refused (str (name nm) ": a value it reads is not known")}))))
        early    (mapv (fn [e] (prof/span-future [(key e) (run {} e)]))
                       (filter #(and (empty? (sq/holes (val %))) (not (row? (val %))))
                               queries))
        payload  (force payload)
        answered (loop [answered (into {} (map deref) early)]
                   (let [data  (with-answers payload answered)
                         ready (filter (fn [[nm q]] (and (open? answered nm)
                                                         (every? #(ready? answered %)
                                                                 (sq/holes q))))
                                       queries)]
                     (if (empty? ready)
                       answered
                       (recur (into answered
                                    (map deref (mapv (fn [e]
                                                       (prof/span-future [(key e) (run data e)]))
                                                     ready)))))))
        same     (same-answers answered)]
    {:answers (with-answers {} (apply dissoc answered (keys same)))
     :answers-same same
     :inputs  (into (sorted-set)
                    (comp (mapcat (comp sq/holes val))
                          (mapcat #(closure specs %))
                          (filter #(contains? state %)))
                    queries)
     :truncated (into {} (keep (fn [[nm {:keys [rows total]}]]
                                 (when (and rows (> total (count rows))) [nm total])))
                      answered)
     :refused (into {} (keep (fn [[nm {:keys [refused]}]] (when refused [nm refused])))
                    answered)}))

(def ^:private search-limit 50)

(defn search-set
  [store schema rid-of hidden specs nm term fields vals want]
  (let [q   (sq/set-of specs (->kw nm))
        rid (some-> q :root rid-of)]
    (cond
      (nil? q)        {:refused (str (name nm) " is no object set")}
      (sq/problem q)  {:refused (sq/problem q)}
      (contains? hidden rid) (restricted-answer q)
      (nil? rid)      {:refused (str (name nm) " reads " (name (:root q))
                                     ", which is not readable here")}
      (empty? (rt/tokens term)) {:rows [] :total 0}
      :else
      (or (answer-query store schema nil (not-empty want) vals nm rid
                        (-> q
                            (update :clauses into
                                    (map (fn [word]
                                           [(if (seq fields) (mapv ->kw fields) :*)
                                            :tokens word]))
                                    (rt/tokens term))
                            (assoc :limit search-limit))
                        nil)
          {:refused (str (name nm) ": a value its clauses read was not sent")}))))

(def ^:private facet-limit 1000)

(defn- type-name-of [schema rid]
  (or (get-in schema [:object-types rid :object-type-name])
      (get-in schema [:interfaces rid :interface-name])))

(defn- property-of
  [{:keys [attribute-name attribute-type opts]}]
  {:name       attribute-name
   :label      (not-empty (str (:name opts)))
   :type       (base-type attribute-type)
   :visibility (some-> (:visibility opts) ->kw)})

(defn- link-of
  [schema {:keys [api-name far-type]}]
  {:side       api-name
   :far-type   (type-name-of schema far-type)
   :label      (not-empty (str (get-in schema [:object-types far-type :display-name])))
   :properties (mapv property-of (lookup/attrs schema far-type))})

(defn- buckets
  [store schema rid where prop]
  (let [{:keys [aggregations]}
        (objects/run-object-query store schema
                                  {:find (type-name-of schema rid) :where where
                                   :group-by [prop] :aggs [{:fn :count}]
                                   :limit facet-limit})]
    {:buckets (mapv (fn [{:keys [group metrics]}]
                      {:value (get group prop) :count (first (vals metrics))})
                    aggregations)
     :cut?    (<= facet-limit (count aggregations))}))

(defn- facet-of
  [store schema rid where {:keys [prop link]}]
  (let [prop (->kw prop)]
    (if link
      (let [far (:far-type (ls/resolve-side schema rid (->kw link)))]
        (unless-restricted {:buckets [] :cut? false :restricted true}
          (grants/authorize-read! (grants/read-rights store)
                                  (get-in schema [:object-types far]) far)
          (buckets store schema far {} prop)))
      (buckets store schema rid where prop))))

(defn- restricted-part
  [type-name want]
  {:type       type-name
   :total      0
   :facets     (mapv (fn [{:keys [prop link]}]
                       {:prop (->kw prop) :link (some-> link ->kw) :buckets [] :cut? false})
                     want)
   :restricted true})

(defn- facet-part
  [store schema rid-of hidden specs vals want describe? nm]
  (let [q   (sq/set-of specs nm)
        rid (when (and q (not (sq/problem q))) (rid-of (:root q)))
        w   (when rid (lower schema rid (:clauses q) vals))
        out (when rid (restricted-part (type-name-of schema rid) want))]
    (cond
      (or (nil? rid) (= ::unresolved w)) nil
      (contains? hidden rid)             out
      :else
      (unless-restricted out
        (authorize-hops! store schema rid w)
        (cond-> {:type   (type-name-of schema rid)
                 :total  (objects/count-matching store schema rid {:where w})
                 :facets (mapv #(assoc (facet-of store schema rid w %)
                                       :prop (->kw (:prop %))
                                       :link (some-> (:link %) ->kw))
                               want)}
          describe? (assoc :properties (mapv property-of (lookup/attrs schema rid))
                           :links (mapv #(link-of schema %) (ls/outgoing schema rid))))))))

(defn facet-set
  [store schema rid-of hidden specs nm want vals describe?]
  (let [nm (->kw nm)]
    (try
      {:parts (into []
                    (keep #(facet-part store schema rid-of hidden specs vals want describe? %))
                    (sq/parts specs nm))}
      (catch InterruptedException e (throw e))
      (catch Exception e {:refused (or (ex-message e) (str e))}))))

(defn bound-rid [schema a t] (get (bound-types schema a) (->kw t)))

(defn- app-data-inputs
  [schema a]
  (let [specs  (layout-vars a)
        bound  (bound-types schema a)
        state? (fn [h] (or (not (contains? specs h)) (= :state (vars/var-kind (get specs h)))))
        sends  (fn [h] (if (state? h) [h] (filter state? (closure specs h))))]
    (into (sorted-set)
          (comp (keep #(query-of specs bound %))
                (remove sq/problem)
                (mapcat sq/holes)
                (mapcat sends))
          (keys specs))))

(defn app-inputs
  [schema a]
  (into (app-data-inputs schema a)
        (gate-inputs (layout-vars a) (dissoc (:layout a) :vars))))

(defn- url-value [{:keys [type] :as spec} url k]
  (let [v (get url k ::absent)]
    (cond
      (= ::absent v)                 (let [d (:default spec)]
                                       (when-not (map? d) [d]))
      (#{nil :string :keyword} (->kw type)) [v])))

(defn url-input-vars
  [schema a url]
  (when (map? url)
    (let [specs (layout-vars a)]
      (into {}
            (keep (fn [nm]
                    (let [{:keys [external-id in-url input?] :as spec} (get specs nm)]
                      (cond
                        input? nil
                        (and external-id in-url)
                        (when-let [[v] (url-value spec url (keyword (name external-id)))]
                          [nm v])
                        (and (not external-id) (contains? spec :default)
                             (not (map? (:default spec))) (not (:default-agg spec)))
                        [nm (:default spec)]))))
            (app-inputs schema a)))))
