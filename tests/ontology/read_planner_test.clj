(ns ^:unit ontology.read-planner-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.grants :as grants]
            [sutra.types.world-rules :as rules]
            [sutra.objects.planner :as planner]
            [sutra.store.mem :as mem]
            [sutra.store.pg :as store-pg]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.sql :as sql]
            [support.select-sql :as sel]
            [sutra.types.object-set :as os]
            [sutra.types.sql-ident :as sqli]
            [sutra.objects.plan-run :as plan-run]))

(defn- plan
  ([s ir] (plan s ir nil))
  ([s ir opts] (planner/plan s ir (merge {:authority grants/system-authority} opts))))

(defn- plan-qt
  ([s qt] (plan-qt s qt nil))
  ([s qt opts] (planner/plan-qt s qt (merge {:authority grants/system-authority} opts))))

(def ^:private widget :ri.test.object-type.widget)
(def ^:private gadget :ri.test.object-type.gadget)
(def ^:private uses   :ri.test.link-type.uses)

(def ^:private schema
  {:object-types
   {widget {:object-type-name :widget
            :attribute-list [{:attribute-name :name :attribute-type :string}]
            :plural "Widgets"}
    gadget {:object-type-name :gadget
            :attribute-list [{:attribute-name :label :attribute-type :string}]
            :plural "Gadgets"}}
   :link-types
   {uses {:link-type-name :uses :from-object-type widget
          :to-object-type gadget :cardinality :many-to-many}}})

(defn- explain
  [plan]
  (plan-run/explain {:db (mem/store {:world schema}) :branch "main"
                     :authority grants/system-authority}
                   plan))

(def ^:private hop
  {:op :search-around :link uses
   :in {:op :filter :where [:= :name "a"] :in {:op :scan :root widget}}})

(deftest a-flat-read-plans-as-flat-and-runs-the-old-sql
  (let [qt {:root widget :where [:= :name "a"] :limit 10}
        p  (plan-qt schema qt)]
    (is (= :flat (:strategy p)))
    (is (= :store (:engine p)))
    (is (= :exact (:accuracy p)))
    (is (= widget (:root p)))
    (testing "and it runs the flat read itself — the store gets the IR the qt lifts to"
      (is (= (os/qt->ir qt) (:ir p))))))

(deftest a-composed-read-says-so
  (let [p (plan schema hop)]
    (is (= :composed (:strategy p)))
    (is (= gadget (:root p)) "the plan names the type the read RETURNS")
    (is (= 1 (get-in p [:estimate :hops])))
    (is (= [widget] (get-in p [:estimate :scans])))))

(deftest the-estimate-reports-what-is-knowable-and-nothing-else
  (let [unbounded (plan-qt schema {:root widget})
        bounded   (plan-qt schema {:root widget :where [:= :name "a"] :limit 10})]
    (is (false? (get-in unbounded [:estimate :bounded?])))
    (is (false? (get-in unbounded [:estimate :filtered?])))
    (is (true?  (get-in bounded [:estimate :bounded?])))
    (is (true?  (get-in bounded [:estimate :filtered?])))
    (is (= 1 (get-in bounded [:estimate :statements]))
        "one statement — the composing lowering does not fan out into several")))

(deftest a-plan-carries-its-own-ir-so-execution-cannot-diverge
  (let [p (plan schema hop)]
    (is (= hop (:ir p)))
    (is (true? (:authority-bound? p))
        "the marking filter is baked into :sql — a plan belongs to one principal")))

(def ^:private two-hops
  {:op :search-around :link uses
   :in {:op :search-around :link uses :in {:op :scan :root widget}}})

(deftest every-shape-routes-to-the-one-engine-that-holds-the-tables
  (is (= :store (:engine (plan schema two-hops))))
  (is (= :store (:engine (plan schema hop))))
  (is (= :store (:engine (plan schema {:op :scan :root widget}))))
  (is (= :store (:engine (plan schema
                                      {:op :aggregate :aggs :count
                                       :in {:op :scan :root widget}}))))
  (testing "and it says WHY, so a rule that never matched stays distinguishable
            from a rule that matched and chose this"
    (is (string? (:engine-reason (plan schema two-hops))))
    (is (nil? (:engine-declined (plan schema two-hops)))
        "nothing is declined when the only engine is the available one")))

(defn- engine-explain
  [ir]
  (with-redefs [layout/relation      (fn [_c _ns t _as-of] (sql/frag (sqli/quote-ident t)))
                layout/link-relation (fn [_c _ns _sig L _as-of]
                                       (sql/frag (sqli/quote-ident (layout/link-table L))))]
    (#'store-pg/explain* {:ns "main" :sig (rules/signature schema) :as-of nil :view nil}
                         (sel/plan schema ir) #{})))

(deftest the-engine-reports-the-tables-a-read-reads
  (let [e (engine-explain hop)]
    (is (= 3 (count (:tables e)))
        "the far side, the edge table, and the near side the hop starts from")
    (is (every? string? (:tables e)))
    (is (every? #(re-find (re-pattern (java.util.regex.Pattern/quote %)) (:sql e)) (:tables e))
        "every table reported is a table the SQL actually names")
    (is (= 1 (:param-count e)))
    (is (not (contains? e :params))
        "an explain is something you paste into a ticket; the binds are the caller's data")))

(deftest caps-are-enforced-not-merely-declared
  (testing "too many hops"
    (let [deep (reduce (fn [in _] {:op :search-around :link uses :in in})
                       {:op :scan :root widget} (range 6))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exceeds plan caps"
                            (plan schema deep)))))
  (testing "a static set bigger than the cap"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"exceeds plan caps"
         (plan schema {:op :static :root widget
                               :ids (vec (range 20000))}))))
  (testing "a page bigger than the cap"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"exceeds plan caps"
         (plan-qt schema {:root widget :limit 100000}))))
  (testing "too many composed nodes"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"exceeds plan caps"
         (plan schema (reduce (fn [in _] {:op :filter :where [:= :a 1] :in in})
                              {:op :scan :root widget} (range 70))))))
  (testing "the columns of a type's derived properties are not what the node cap bounds"
    (is (some? (plan schema (reduce (fn [in i]
                                      {:op :compute :name (keyword (str "c" i)) :expr 1 :in in})
                                    {:op :scan :root widget} (range 70))))))
  (testing "caps are overridable — they are policy, and policy is an argument"
    (is (some? (plan-qt schema {:root widget :limit 100000}
                                {:caps {:max-page 1000000}})))))

(def ^:private big-stats   (fn [root] (when (= root widget) 200000)))
(def ^:private small-stats (fn [root] (when (= root widget) 50)))

(def ^:private unfiltered-hop
  {:op :search-around :link uses :in {:op :scan :root widget}})

(deftest the-cardinality-cap-refuses-only-what-statistics-prove
  (testing "an unfiltered scan over the cap, under statistics — provable, refused"
    (try
      (plan schema unfiltered-hop {:stats-fn big-stats})
      (is false "should have refused a 200k-object traversal")
      (catch clojure.lang.ExceptionInfo e
        (is (re-find #"exceeds plan caps" (ex-message e)))
        (is (= [:search-around-cardinality]
               (mapv :cap (:violations (ex-data e)))))
        (is (= 200000 (-> (ex-data e) :violations first :actual))))))
  (testing "a FILTERED input is only an upper bound — an upper bound can prove a
            read small, never prove it too big, so it passes rather than
            over-refusing a selective read"
    (is (some? (plan schema hop {:stats-fn big-stats}))))
  (testing "no statistics — undecidable, passes: exactly the pre-statistics
            behaviour, degraded to rather than guessed around"
    (is (some? (plan schema unfiltered-hop))))
  (testing "a static start over the cap is provable WITHOUT statistics"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"exceeds plan caps"
         (plan schema
                       {:op :search-around :link uses
                        :in {:op :static :root widget
                             :ids (mapv str (range 150000))}}
                       {:caps {:max-static-members 1000000}})))))

(deftest the-estimate-reports-the-start-bound-and-its-kind
  (let [exact (plan schema two-hops {:stats-fn small-stats})
        maxed (plan schema
                            {:op :search-around :link uses :in (:in hop)}
                            {:stats-fn big-stats})
        blind (plan schema two-hops)]
    (is (= {:kind :exact :n 50} (get-in exact [:estimate :start-bound]))
        "an unfiltered scan under an unrestricted principal IS its table count")
    (is (= {:kind :max :n 200000} (get-in maxed [:estimate :start-bound]))
        "a filter demotes the claim to a ceiling — the set is AT MOST that big")
    (is (nil? (get-in blind [:estimate :start-bound]))
        "no statistics, no claim — absence, not zero")))

(deftest accuracy-is-a-negotiated-preference-not-a-silent-flag
  (let [p (plan schema hop {:accuracy :approximate})]
    (is (= :exact (:accuracy p)) "what was SERVED — no approximate lowering yet")
    (is (= :approximate (:accuracy-requested p)) "what was ASKED — on the record")
    (is (= :approximate (:accuracy-requested (explain p)))))
  (is (not (contains? (plan schema hop) :accuracy-requested))
      "no request, no negotiation to report")
  (testing "an accuracy outside the vocabulary is a caller error"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown accuracy"
                          (plan schema hop {:accuracy :fast})))))

(deftest a-requested-engine-is-honoured-and-an-unknown-one-is-refused
  (let [flat {:op :scan :root widget}]
    (let [p (plan schema flat {:engine :store})]
      (is (= :store (:engine p)) "the caller's preference beats the rules table")
      (is (= :store (:engine-requested p))))
    (testing "an engine outside the table is a caller error, not a fallback case —
              which is what keeps a retired engine's name from silently working"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown engine"
                            (plan schema flat {:engine :spark}))))))

(deftest a-template-is-not-plannable
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"a template is not a read"
       (plan-qt schema {:root widget :param-slots [:p/x]}))))

(deftest a-malformed-set-fails-at-plan-time-not-at-execution-time
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"malformed object set"
                        (plan schema {:op :filter}))))

(deftest explain-is-printable-and-leaks-no-values
  (let [e (explain (plan schema hop))]
    (is (= :composed (:strategy e)))
    (is (= :store (:engine e)))
    (is (string? (:reason e)) "the engine choice states WHY, not just what")
    (is (boolean? (get-in e [:store :pushdown?])) "and the store says how it answers the read")
    (is (not (contains? e :params)))
    (is (= :within (:cap-status e)))))

(deftest planning-a-flat-qt-needs-no-knowledge-of-the-algebra
  (is (= (plan schema (os/qt->ir {:root widget}))
         (plan-qt schema {:root widget}))))
