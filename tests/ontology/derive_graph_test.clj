(ns ^:unit ontology.derive-graph-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [sutra.types.derive-graph :as dg]
            [sutra.objects.views :as views]
            [sutra.objects.planner :as planner]
            [sutra.types.link-side :as ls]
            [sutra.ref.objectset :as ros]
            [clojure.walk]
            [sutra.store.derived :as sd]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [support.in-project :refer [in-project]]
            [sutra.parser.project-export :as pe]))

(def ^:private far-of
  (fn [T handle] (get-in {:kopf {:zeilen :zeile} :zeile {:teile :teil}} [T handle])))

(defn- reads [T e] (set (map (juxt :attr :slot) (dg/far-reads far-of T e))))

(deftest every-slot-that-reads-the-far-type-is-a-dependency
  (testing "the fold's own :of, which the positional walker did see"
    (is (= #{[:wert :of]} (reads :kopf [:connected-sum :zeilen :wert]))))

  (testing "a fold's :where — it filters ON the far object, so its attributes are read
            there, and a derived one that nobody resolved read as nil: the fold then
            counted the row in or out by accident"
    (is (= #{[:wert :of] [:marge :where] [:code :where]}
           (reads :kopf [:connected-sum :zeilen :wert [:and [:> :marge 1] [:= :code "A"]]]))))

  (testing ":connected-count has no :of at all, so the positional walker skipped it whole —
            and its :where reads the far type just the same"
    (is (= #{[:marge :where]} (reads :kopf [:connected-count :zeilen [:> :marge 1]]))))

  (testing "an arg-max reads TWO far attributes: the one it sorts by and the one it picks.
            The walker took the second element of the node, which is :by — :pick was never
            seen, and :pick is the value that is returned"
    (is (= #{[:rang :by] [:spanne :pick]}
           (reads :zeile [:connected-arg-max :teile :rang :spanne]))))

  (testing ":connected-one picks one far attribute"
    (is (= #{[:name :pick]} (reads :zeile [:connected-one :teile :name]))))

  (testing ":connected?'s predicate is evaluated over the far object, and the positional
            walker saw a vector where it wanted a keyword and dropped the node"
    (is (= #{[:preis :pred]} (reads :zeile [:connected? :teile [:> :preis 0]]))))

  (testing "a link op nested in that predicate traverses from the FAR object — the graph
            says so with :near, which is what lets the read refuse it by name instead of
            resolving a side it holds no near rows for"
    (is (= [{:near :zeile :handle :teile :far :teil :attr :spanne :slot :of}]
           (dg/far-reads far-of :kopf
                         [:connected? :zeilen [:> [:connected-sum :teile :spanne] 3]])))))

(defn- world
  [derived links]
  [(fn [T] (get derived T)) (fn [T h] (get-in links [T h]))])

(deftest a-chain-is-nodes-and-edges-whatever-its-length
  (let [[d f] (world {:a {:x [:connected-sum :l :y]}
                      :b {:y [:* [:get :z] 2] :z [:connected-sum :m :w]}
                      :c {:w [:get :raw]}}
                     {:a {:l :b} :b {:m :c}})
        g     (dg/graph [:a :b :c] d f)]
    (testing "the edge of a chain, and the edge inside a type"
      (is (= #{[:b :y]} (get g [:a :x])))
      (is (= #{[:b :z]} (get g [:b :y])) "y reads z of its own type")
      (is (= #{[:c :w]} (get g [:b :z]))))
    (testing "a fold over a STORED far property is no edge at all: the store answers it as
              a column, in the statement"
      (is (= #{} (get g [:c :w]))))
    (testing "and the whole chain is planned before anything is read: three rounds of link
              reads, the sides they go over, the far properties they read"
      (is (= {:levels 2
              :sides #{[:a :l] [:b :m]}
              :nodes #{[:b :y] [:b :z] [:c :w]}}
             (dg/chain-plan d f :a (seq (d :a))))))))

(deftest a-cycle-is-named-with-the-chain-it-closes
  (let [[d f] (world {:a {:x [:connected-sum :l :y]} :b {:y [:connected-sum :back :x]}}
                     {:a {:l :b} :b {:back :a}})
        g     (dg/graph [:a :b] d f)]
    (is (= [[[:a :x] [:b :y]]] (dg/cycles g)))
    (is (= {[:a :x] [[:a :x] [:b :y]] [:b :y] [[:a :x] [:b :y]]} (dg/cycle-index g)))
    (testing "the sentence says why it cannot be answered, not merely that it is a cycle"
      (is (re-find #"a.x → b.y → a.x"
                   (dg/cycle-message (fn [[T a]] (str (name T) "." (name a)))
                                     (first (dg/cycles g))))))
    (testing "and the plan still ends — the walk expands no node twice, so a cycle costs
              the planner nothing to notice"
      (is (= 2 (:levels (dg/chain-plan d f :a (seq (d :a)))))
          "two rounds, then nothing new to expand — the walk ends on its own"))))

(deftest a-hierarchy-closes-the-chain-on-its-first-hop
  (testing "a rollup over a link that reaches the type itself — a parent, a tree — reads
            its own property again, and no member of it can be computed first"
    (let [[d f] (world {:knoten {:summe [:connected-sum :kinder :summe]}}
                       {:knoten {:kinder :knoten}})]
      (is (= [[[:knoten :summe]]] (dg/cycles (dg/graph [:knoten] d f)))))))

(def ^:private live
  (in-project :t
              {:object-types {"ri.o.1" {:object-type-name :kopf
                                        :attribute-list
                                          [{:attribute-name :nr :attribute-type :string}]
                                        :primary-key [:nr]
                                        :plural "Köpfe"}}}))

(def ^:private opts {:strict? true :strict-sections (pe/carried-sections true)})

(defn- rules
  [tree]
  (set (map :rule (install-plan/plan-problems tree live (install/plan-schema tree live opts)))))

(defn- tree
  [kopf-attrs zeile-attrs]
  (in-project :t
              {:objects {:kopf  {:attributes kopf-attrs :match-keys [:nr] :plural "Köpfe"}
                         :zeile {:attributes zeile-attrs :match-keys [:nr] :plural "Zeilen"}}
               :links   [[:zeilen :kopf :zeile :one-to-many]
                         [:kopf-von :zeile :kopf :many-to-one]]}))

(deftest the-install-refuses-a-cycle-a-push-adds
  (testing "it cannot be left to the read: a cycle lands, and from then on every read of
            the type refuses — the push is the last moment anyone is there to fix it"
    (is (contains?
         (rules (tree [[:nr :string]
                       [:summe :decimal {:derived {:expression [:connected-sum :zeilen :wert]}}]]
                      [[:nr :string]
                       [:wert :decimal
                        {:derived {:expression [:connected-sum :kopf-von :summe]}}]]))
         :derived-cycle)))

  (testing "a chain that does not come back is ordinary work and is not refused"
    (is (not (contains?
              (rules (tree [[:nr :string]
                            [:summe :decimal
                             {:derived {:expression [:connected-sum :zeilen :wert]}}]]
                           [[:nr :string] [:menge :decimal]
                            [:wert :decimal {:derived {:expression [:* [:get :menge] 2]}}]]))
              :derived-cycle))))

  (testing "and a cycle through a slot the positional walker never read — here an arg-max's
            :pick — is the same cycle"
    (is (contains?
         (rules (tree [[:nr :string]
                       [:summe :decimal
                        {:derived {:expression [:connected-arg-max :zeilen :nr :wert]}}]]
                      [[:nr :string]
                       [:wert :decimal
                        {:derived {:expression [:connected-sum :kopf-von :summe]}}]]))
         :derived-cycle))))

(defn- chain-schema
  [n]
  (let [T  (fn [i] (keyword (str "ri.test.object-type.t" i)))
        L  (fn [i] (keyword (str "ri.test.link-type.l" i)))
        lh (fn [i] (keyword (str "l" i)))]
    {:object-types
     (into {} (for [i (range (inc n))]
                [(T i) {:object-type-name (keyword (str "t" i))
                        :attribute-list
                        (if (= i n)
                          [{:attribute-name :menge :attribute-type :decimal}
                           {:attribute-name :s :attribute-type :decimal
                            :opts {:derived {:expression [:round [:get :menge]]}}}]
                          [{:attribute-name :s :attribute-type :decimal
                            :opts {:derived {:expression [:connected-sum (lh i) :s]}}}])}]))
     :link-types
     (into {} (for [i (range n)]
                [(L i) {:link-type-name (lh i) :from-object-type (T i)
                        :to-object-type (T (inc i)) :cardinality :one-to-many}]))}))

(defn- member
  [ir]
  (loop [n ir steps ()]
    (if (#{:with :compute :filter :order :project} (:op n))
      (recur (:in n) (cond->> steps (= :compute (:op n)) (cons n)))
      [n steps])))

(defn- reader
  [objs edges]
  (fn [[op a b]]
     (case op
       :links  (let [es (get edges a)]
                 (cond (:from b) (filter (comp (set (:from b)) :from-object-id) es)
                       (:to b)   (filter (comp (set (:to b)) :to-object-id) es)
                       :else     es))
       :select (let [[{:keys [root where]} steps] (member a)
                     ids (when (= :id-in (first where)) (set (second where)))]
                 (into []
                       (comp (filter #(or (nil? ids) (ids (:id %))))
                             (map (fn [row]
                                    (-> row
                                        (assoc :object-type-ref root)
                                        (update :values
                                                (fn [V]
                                                  (reduce (fn [V {:keys [name expr cast]}]
                                                            (assoc V name
                                                                   (ros/cast-value
                                                                    cast
                                                                    (ros/compute-value
                                                                     {} expr (some? cast)))))
                                                          V
                                                          (map (fn [step]
                                                                 (update step :expr
                                                                         (fn [e]
                                                                           (clojure.walk/postwalk
                                                                            #(if (keyword? %)
                                                                               (get V % %) %) e))))
                                                               steps))))))))
                       (get objs root))))))

(defn- line-store
  [n]
  (let [T (fn [i] (keyword (str "ri.test.object-type.t" i)))
        L (fn [i] (keyword (str "ri.test.link-type.l" i)))
        id (fn [i] (str "o" i))]
    (reader (into {(T n) [{:id (str "o" n "a") :values {:menge 2M}}
                         {:id (str "o" n "b") :values {:menge 3M}}]}
                 (for [i (range n)] [(T i) [{:id (id i) :values {}}]]))
           (into {(L (dec n)) [{:from-object-id (id (dec n)) :to-object-id (str "o" n "a")}
                               {:from-object-id (id (dec n)) :to-object-id (str "o" n "b")}]}
                 (for [i (range (dec n))]
                   [(L i) [{:from-object-id (id i) :to-object-id (id (inc i))}]])))))

(def ^:private t0 :ri.test.object-type.t0)

(deftest a-chain-longer-than-any-constant-is-answered
  (testing "eight levels — two more than the depth this evaluator used to resolve, where it
            refused the read outright. Nothing counts levels now: the graph planned the
            chain, and each level is one batched pair of reads"
    (let [schema (chain-schema 8)
          rows   (views/attach-derived (line-store 8) schema t0 [{:id "o0" :values {}}])]
      (is (= 5M (get-in (first rows) [:values :s])))))

  (testing "and the plan says what it will cost before it is spent, which is what a size
            limit can be stated against — a depth could not"
    (let [schema (chain-schema 8)
          chain  (views/chain-of schema t0 (vec (sd/derived-of schema t0)))]
      (is (= 8 (:levels chain)))
      (is (= 8 (count (:sides chain)))))))

(def ^:private kopf :ri.test.object-type.kopf)
(def ^:private posi :ri.test.object-type.position)

(def ^:private where-schema
  {:object-types
   {kopf {:object-type-name :bestellung
          :attribute-list [{:attribute-name :offene-pos-count :attribute-type :integer
                            :opts
                              {:derived
                                 {:expression
                                    [:connected-count :positionen [:> :offene-menge 0]]}}}]
          :plural "Bestellungen"}
    posi {:object-type-name :position
          :attribute-list [{:attribute-name :menge :attribute-type [:decimal 10 3]}
                           {:attribute-name :geliefert :attribute-type [:decimal 10 3]}
                           {:attribute-name :offene-menge :attribute-type [:decimal 10 3]
                            :opts {:derived {:expression [:- [:get :menge] [:get :geliefert]]}}}]
          :plural "Positionen"}}
   :link-types
   {:ri.test.link-type.positionen {:link-type-name :positionen :from-object-type kopf
                                   :to-object-type posi :cardinality :one-to-many}}})

(deftest a-fold-filtered-by-a-derived-far-property-is-stated-over-a-relation-that-carries-it
  (testing "the plan CAN state this fold — a count over a link with a comparison — and it
            did, filtering on the column that wears the derived property's name. That column
            is deliberately NULL (the indexer nulls derived columns so no raw reader finds a
            build-old number), so no row matched and the count came back 0: an answer shaped
            like one. In prod every order read 0 open positions while its positions had open
            quantities of 1 and 2. 5257b127 refused the fold, which made the answer right and
            left it to the evaluator; the far side now CARRIES the property as a column, so
            the predicate has a real value to compare and the whole fold is one statement."
    (let [lowered (sd/lower where-schema kopf :offene-pos-count)
          fold    (first (filter #(= :with (:op %)) (:steps lowered)))]
      (is (some? lowered) "the plan states it")
      (is (= [:> :sutra.store.derived/offene-menge 0] (:where fold))
          "the predicate compares a column of the fold's own relation, never the NULL one
           that wears the property's name")
      (is (= [:- :menge :geliefert] (:expr (:far-in fold)))
          "and that relation computes the property from the far row's own stored columns")
      (is (= [] (sd/leftovers where-schema {:op :static :root kopf :ids []}
                              (vec (sd/derived-of where-schema kopf))))
          "so nothing of it is left for the evaluator")))

  (testing "the graph still names the :where attribute as the dependency it is. What ACTS
            on that is now only a property the plan cannot state — the read plans its chain
            from the leftovers (views/chain-of) — but the edge is there to be seen, and it
            is what the authoring and install doors judge a cycle by"
    (let [[d-of f-of] [(fn [T] (sd/derived-of where-schema T))
                       (fn [T h] (:far-type (ls/resolve-side where-schema T h)))]]
      (is (= [{:near kopf :handle :positionen :far posi :attrs #{:offene-menge}}]
             (dg/far-needs d-of f-of kopf (vec (sd/derived-of where-schema kopf)))))))

  (testing "the VALUE this shape answers is a store's to get right, so it is pinned where a
            real one answers: sutra.store.conformance folds over a far relation and filters
            by one of its columns, and both engines answer it or the suite fails. The store
            here is a seam that answers :compute columns and no fold, so it could only ever
            check this while the fold ran above it — which is the thing that changed."
    (is (some? (sd/lower where-schema kopf :offene-pos-count)))))

(deftest a-cycle-is-refused-at-the-read-before-a-single-link-is-read
  (testing "the read's own door: an app's module-scoped derived property passes neither the
            authoring door nor the install gate, so the read cannot assume acyclicity"
    (let [knoten :ri.test.object-type.knoten
          schema {:object-types
                  {knoten {:object-type-name :knoten
                           :attribute-list [{:attribute-name :summe :attribute-type :decimal
                                             :opts
                                               {:derived
                                                  {:expression [:connected-sum :kinder :summe]}}}]
                           :plural "Knoten"}}
                  :link-types
                  {:ri.test.link-type.kinder {:link-type-name :kinder :from-object-type knoten
                                              :to-object-type knoten :cardinality :one-to-many
                                              :forward-api-name :kinder
                                              :reverse-api-name :eltern}}}
          ex     (try (views/attach-derived nil schema knoten [{:id "k1" :values {}}])
                      (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo ex) "nil as the store proves nothing was read")
      (is (= 422 (anomaly/status (ex-data ex))))
      (is (re-find #"knoten.summe → knoten.summe" (ex-message ex)))
      (is (= [{:object-type "knoten" :attribute :summe}] (:cycle (ex-data ex)))))))

(deftest a-step-that-would-load-more-than-it-may-is-refused-with-the-number
  (testing "the limit is a SIZE: the far objects of one step. It refuses
            rather than deriving as many as fitted — which is the failure the depth cap had,
            in another dress"
    (let [schema (chain-schema 1)
          reads  (line-store 1)]
      (with-redefs-fn {#'planner/default-caps (assoc planner/default-caps :max-derive-objects 1)}
        (fn []
          (let [ex (try (views/attach-derived reads schema t0 [{:id "o0" :values {}}])
                        (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo ex))
            (is (= 422 (anomaly/status (ex-data ex))))
            (is (= {:cap :max-derive-objects :limit 1 :actual 2}
                   (select-keys (ex-data ex) [:cap :limit :actual]))
                "the same shape a plan cap violation has: which bound, and by how much")
            (is (re-find #"must be loaded and derived above the store" (ex-message ex))))))
      (testing "under the limit the same read answers"
        (is (= 5M (get-in (first (views/attach-derived reads schema t0 [{:id "o0" :values {}}]))
                          [:values :s])))))))

(deftest a-link-op-nested-in-a-predicate-is-refused-and-the-way-out-is-named
  (testing "it traverses from the far object, so this read never held its near rows: it
            used to come back without derived values, read nil, and fold to 0"
    (let [kopf :ri.test.object-type.kopf
          zeile :ri.test.object-type.zeile
          teil :ri.test.object-type.teil
          schema {:object-types
                  {kopf  {:object-type-name :kopf
                          :attribute-list [{:attribute-name :ok? :attribute-type :boolean
                                            :opts
                                              {:derived
                                                 {:expression
                                                    [:connected? :zeilen
                                                     [:> [:connected-sum :teile :spanne] 3]]}}}]
                          :plural "Köpfe"}
                   zeile {:object-type-name :zeile :attribute-list [] :plural "Zeilen"}
                   teil  {:object-type-name :teil
                          :attribute-list [{:attribute-name :a :attribute-type :decimal}
                                           {:attribute-name :spanne :attribute-type :decimal
                                            :opts {:derived {:expression [:* [:get :a] 2]}}}]
                          :plural "Teile"}}
                  :link-types
                  {:ri.test.link-type.zeilen {:link-type-name :zeilen :from-object-type kopf
                                              :to-object-type zeile :cardinality :one-to-many}
                   :ri.test.link-type.teile {:link-type-name :teile :from-object-type zeile
                                             :to-object-type teil :cardinality :one-to-many}}}
          ex     (try (views/attach-derived nil schema kopf [{:id "k1" :values {}}])
                      (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo ex))
      (is (= 422 (anomaly/status (ex-data ex))))
      (is (re-find #"derived property of zeile" (ex-message ex))
          "the way out is a property of the near type, which the chain then plans like any other")
      (is (= {:near-type "zeile" :link :teile :far-type "teil"}
             (select-keys (ex-data ex) [:near-type :link :far-type]))))))
