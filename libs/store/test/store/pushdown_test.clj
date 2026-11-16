(ns ^:unit store.pushdown-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.generator :as mg]
            [sutra.store.plan :as plan]
            [sutra.store.scan :as scan]
            [sutra.store.semantics :as sem]
            [sutra.store.shapes :as shapes]))

(def ^:private astral (String. (Character/toChars 0x1F600)))

(deftest semantics-equality
  (testing "numbers by value across representations"
    (is (sem/equal? 1 1.0M))
    (is (sem/equal? 1 1N))
    (is (sem/equal? 1.5 1.50M)))
  (testing "a uuid or keyword equals its text"
    (is (sem/equal? #uuid "00000000-0000-0000-0000-000000000001"
                    "00000000-0000-0000-0000-000000000001"))
    (is (sem/equal? :a/b "a/b")))
  (testing "nil and NaN equal nothing, themselves included"
    (is (not (sem/equal? nil nil)))
    (is (not (sem/equal? ##NaN ##NaN)))
    (is (not (sem/equal? ##NaN 1.0))))
  (testing "an instant is its millisecond"
    (let [i (java.time.Instant/parse "2026-01-01T00:00:00.000500Z")
          d (java.util.Date. (inst-ms i))]
      (is (sem/equal? i d) "a Date equals an Instant of the same millisecond")
      (is (sem/equal? i (java.time.Instant/parse "2026-01-01T00:00:00Z")))
      (is (not (sem/equal? i (java.time.Instant/parse "2026-01-01T00:00:00.001Z"))))
      (is (= (sem/group-key i) (sem/group-key d)))
      (is (= (java.time.Instant/ofEpochMilli (inst-ms i)) (sem/millisecond i))
          "cut, of the class it had")))
  (testing "collections element-wise"
    (is (sem/equal? [1 "a"] [1.0M "a"]))
    (is (not (sem/equal? [1] [1 2])))))

(deftest semantics-order
  (testing "text by code point, not by UTF-16 unit"
    (is (neg? (sem/compare-values "￿" astral)))
    (is (pos? (compare "￿" astral)) "Java's compare orders them the other way"))
  (testing "nil last ascending, first descending"
    (is (= [1 2 nil] (sort #(sem/compare-in :asc %1 %2) [nil 2 1])))
    (is (= [nil 2 1] (sort #(sem/compare-in :desc %1 %2) [nil 2 1]))))
  (testing "NaN above every other number, a total order"
    (is (= [-1.0 1 2.5M ##Inf] (butlast (sort sem/compare-values [##NaN 2.5M ##Inf -1.0 1]))))
    (is (Double/isNaN (last (sort sem/compare-values [##NaN 2.5M 1]))))
    (is (zero? (sem/compare-values ##NaN ##NaN))))
  (testing "a group holds values the semantics call equal"
    (is (= 2 (sem/group-key 2.0M) (sem/group-key 2N) (sem/group-key 2.0)))
    (is (= 2.5M (sem/group-key 2.50M)))
    (is (= [1 "a"] (sem/group-key [1.0M "a"]))))
  (testing "different classes by :classes"
    (is (neg? (sem/compare-values true 1)))
    (is (neg? (sem/compare-values 1 "a"))))
  (testing "an engine may not override the semantics"
    (is (= {} (sem/check-overrides! {})))
    (is (thrown? clojure.lang.ExceptionInfo (sem/check-overrides! {:order {:nil :first-asc}})))))

(def ^:private entries
  [["k1" {:a 1    :s "apple"  :tags ["x" "y"]}]
   ["k2" {:a 2    :s "banana" :tags []}]
   ["k3" {:a nil  :s "avocado"}]
   ["k4" {:a 2.0M :s nil      :tags ["y"]}]])

(defn- ks [answer] (mapv first answer))

(deftest predicates
  (let [hits (fn [p] (ks (filter #(scan/holds? % p) entries)))]
    (is (= ["k2" "k4"] (hits [:= :a 2])) "numbers by value")
    (is (= [] (hits [:= :a nil])) "[:= a nil] holds for none")
    (is (= ["k3"] (hits [:nil? :a true])))
    (is (= ["k1" "k2" "k4"] (hits [:nil? :tags false]))
        "only k3 has no tags; an empty array is a value")
    (is (= ["k1"] (hits [:not= :a 2])) "not= holds for neither nil")
    (is (= ["k1" "k3"] (hits [:not [:= :a 2]]))
        "not is two-valued: a nil a is not 2 (pg: NOT coalesce(…, FALSE))")
    (is (= [] (hits [:in :a #{}])) "an empty set holds for none")
    (is (= ["k1" "k3"] (hits [:starts-with :s "a"])))
    (is (= ["k1" "k4"] (hits [:has :tags "y"])))
    (is (= ["k3"] (hits [:= :db/key "k3"])))
    (is (= ["k1" "k2" "k4"] (hits [:or [:> :a 1] [:< :a 2]])))
    (is (= ["k2" "k4"] (hits [:and [:>= :a 2] [:<= :a 2]])))
    (is (thrown? clojure.lang.ExceptionInfo (scan/holds? (first entries) [:like :s "a%"])))))

(def ^:private pg-profile
  {:sources #{:rows :links :records} :preds {:= :exact :< :exact :in :exact :starts-with :exact}
   :order? true :page? true :project? true :agg #{:count :group-by}})

(def ^:private pruning-profile
  {:sources #{:rows} :preds {:= :inexact :< :inexact}})

(def ^:private projecting-pruner
  {:sources #{:rows} :preds {:= :inexact :< :inexact} :project? true})

(def ^:private single-comparisons
  {:sources #{:rows} :preds {:= :exact :< :exact} :compound? false :project? true :page? true})

(deftest offers-and-residuals
  (let [s {:source [:rows :t] :where [[:= :a 2] [:starts-with :s "b"]] :order [[:s :asc]] :limit 1
           :project #{:s}}]
    (testing "an exact engine takes everything"
      (let [o (scan/offer pg-profile s)]
        (is (= {:where [:exact :exact] :order true :page true :project true :agg :none} o))
        (is (= {:where []} (scan/residual s o)))
        (is (= s (scan/pushed s o)))))
    (testing "an inexact conjunct keeps page and projection in the core"
      (let [o (scan/offer pruning-profile s)]
        (is (= {:where [:inexact :none] :order false :page false :project false :agg :none} o))
        (is (= [[:= :a 2]] (:where (scan/pushed s o)))
            "a conjunct the engine cannot judge is not sent")
        (is (= {:where [[:= :a 2] [:starts-with :s "b"]] :order [[:s :asc]] :page {:limit 1}
                :project #{:s}}
               (scan/residual s o)))))
    (testing "a projecting engine is sent the projection widened by what the core still reads"
      (let [o (scan/offer projecting-pruner s)]
        (is (true? (:project o)))
        (is (= #{:s :a} (:project (scan/pushed s o)))
            ":a for the conjunct filtered again, :s for the order")
        (is (= #{:s} (:project (scan/residual s o))) "and narrowed back by the core")
        (is (nil? (:project (scan/residual (dissoc s :where :order)
                                           (scan/offer projecting-pruner
                                                       (dissoc s :where :order)))))
            "nothing to widen, nothing to narrow")))
    (testing "an inexact negation is never sent; an engine without :compound? takes no structure"
      (is (= [:none :inexact :none]
             (:where (scan/offer pruning-profile
                                 {:source [:rows :t]
                                  :where [[:not [:= :a 1]] [:or [:= :a 1] [:< :a 0]]
                                          [:or [:not [:< :a 1]]]]}))))
      (is (= [:exact :none :none]
             (:where (scan/offer single-comparisons
                                 {:source [:rows :t]
                                  :where [[:= :a 1] [:or [:= :a 1]] [:not [:= :a 1]]]}))))
      (is (= [:exact :exact]
             (:where (scan/offer pg-profile
                                 {:source [:rows :t]
                                  :where [[:or [:= :a 1] [:< :a 0]] [:not [:= :a 1]]]})))
          "absent :compound? takes structure; an exact negation is sent"))
    (testing "an aggregate goes to the engine only when every conjunct is exact"
      (is (= :complete
             (:agg (scan/offer pg-profile
                               {:source [:rows :t] :where [[:= :a 2]] :agg {:count? true}}))))
      (is (= :partial
             (:agg (scan/offer (assoc pg-profile :agg-mode :partial)
                               {:source [:rows :t] :where [] :agg {:count? true}}))))
      (is (= :none
             (:agg (scan/offer pruning-profile
                               {:source [:rows :t] :where [[:= :a 2]] :agg {:count? true}})))))))

(defn- engine-answer
  [entries s o]
  (scan/reference entries (cond-> (scan/pushed s o)
                            (some #{:inexact} (:where o)) (assoc :where []))))

(deftest a-pushed-answer-finished-by-the-core-equals-the-reference
  (doseq [p [pg-profile pruning-profile projecting-pruner single-comparisons
             {:sources #{:rows} :preds {}}]
          s [{:source [:rows :t] :where [[:= :a 2]]}
             {:source [:rows :t] :where [[:starts-with :s "a"]] :order [[:a :desc]]}
             {:source [:rows :t] :where [] :order [[:a :asc]] :after "k1" :limit 2 :project #{:a}}
             {:source [:rows :t] :where [[:< :a 3]] :agg {:count? true}}
             {:source [:rows :t] :where [] :agg {:group-by :a}}]]
    (let [o (scan/offer p s)
          a (scan/answer s o (engine-answer entries s o))
          r (scan/reference entries s)]
      (is (scan/same-answer? s a r) (pr-str {:profile p :scan s :answer a :reference r})))))

(deftest reference-shapes
  (is (= ["k1" "k2" "k4" "k3"]
         (ks (scan/reference entries {:source [:rows :t] :where [] :order [[:a :asc]]})))
      "nil last; 2 and 2.0M tie and fall back to the key")
  (is (= {1 1 2 2 nil 1}
         (scan/reference entries {:source [:rows :t] :where [] :agg {:group-by :a}}))
      "2 and 2.0M are one group by value")
  (is (= ["k2"] (ks (scan/reference entries {:source [:rows :t] :where [] :after "k1" :limit 1})))))

(deftest partial-aggregates
  (testing "partition states merge: counts and sums add, least min, greatest max"
    (is (= {[] {:count 5 :sum 0.3M :min 1 :max "z"}}
           (scan/merge-agg-states [{[] {:count 2 :sum 0.1M :min 1 :max "a"}}
                                   {[] {:count 3 :sum 0.2M :min 4 :max "z"}}]))))
  (testing "a sum that overflows its integer type is refused, not widened"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"overflows"
                          (scan/merge-agg-states [{[] {:sum Long/MAX_VALUE}} {[] {:sum 1}}]))))
  (testing "a partial count answer equals the reference"
    (let [s {:source [:rows :t] :where [] :agg {:group-by :a}}
          o (scan/offer (assoc pg-profile :agg-mode :partial) s)
          parts (map #(scan/agg-state % (:agg s)) [(take 2 entries) (drop 2 entries)])]
      (is (= :partial (:agg o)))
      (is (scan/same-answer? s (scan/answer s o parts) (scan/reference entries s))))))

(deftest malformed-scans
  (is (thrown? clojure.lang.ExceptionInfo (scan/check! {:source [:kv :x "p"] :where []}))
      "kv stays the -kv-scan primitive")
  (is (thrown? clojure.lang.ExceptionInfo
               (scan/check! {:source [:rows :t] :where [] :order [[:a :up]]}))))

(def ^:private sig
  {:types {:t {:attrs [{:name :amount :type :decimal} {:name :n :type :long}]}
           :u {:attrs []}}
   :links {:fk-l {:from :t :to :u :fk {:holder :t :target :u :side :from :attr :u-id}}
           :plain {:from :t :to :u}}})

(def ^:private tree
  {:op :union :type :t
   :in [{:op :filter :type :t :where [:starts-with :name "a"] :in {:op :scan :type :t}}
        {:op :search-around :type :t :link :fk-l :in {:op :scan :type :u}}
        {:op :filter :type :t :where [:= :n 1] :in {:op :scan :type :t}}]})

(deftest claims-and-fragments
  (let [all   {:ops #{:scan :filter :search-around :union :fk-join}
               :preds {:= :exact :starts-with :exact}}
        some* {:ops #{:scan :filter :search-around :union} :preds {:= :exact}}
        fk    (plan/fk-joins sig tree)]
    (testing "fk-derived hops carry their :fk-join node"
      (is (= {:op :fk-join :link :fk-l :holder :t :target :u :fk (get-in sig [:links :fk-l :fk])}
             (get-in fk [:in 1 :edges])))
      (is (nil? (plan/fk-join sig :plain))))
    (testing "an engine that takes every node claims the whole plan"
      (is (= [[]] (plan/claims all sig fk))))
    (testing "an engine without :fk-join or :starts-with claims the subtrees it can"
      (is (= [[0 0] [1 0] [2]] (plan/claims some* sig fk))))
    (testing "an empty profile claims nothing"
      (is (= [] (plan/claims {:ops #{}} sig fk))))
    (testing "split and assemble give back a plan the core can finish"
      (let [cl (plan/claims some* sig fk)
            {:keys [fragments residual]} (plan/split fk cl #{:secret})]
        (is (= 3 (count fragments)))
        (is (every? #(= #{:secret} (:denied %)) fragments))
        (is (= {:op :claimed :ref 2 :type :t} (get-in residual [:in 2])))
        (is (= {:op :static :ids ["9"] :root :t}
               (get-in (plan/assemble residual {0 [{:id "1"}] 1 [{:id 7}] 2 [{:id 9}]})
                       [:in 2])))))))

(deftest folds-over-an-expression-are-claimed-only-where-they-are-compiled
  (let [fold (fn [more] (merge {:op :with :type :t :name :c :fn :sum :link :plain :far :t
                                :in {:op :scan :type :t}}
                               more))
        p    (fn [folds] (cond-> {:ops #{:scan :with} :aggs {:sum :all :count :all}}
                           folds (assoc :folds folds)))]
    (testing "a profile that does not speak of :folds folds an attribute, as every engine
              always has — nothing an engine declared before this means less now"
      (is (= [[]] (plan/claims (p nil) sig (fold {:attr :amount})))))
    (testing "and an :expr fold is the core's until the engine says it compiles one: the
              value is the same either way, so the only thing at stake is where it is paid"
      (is (= [[0]] (plan/claims (p nil) sig (fold {:expr [:+ :amount :n]})))
          "the scan below is still the engine's; the fold is not")
      (is (= [[]] (plan/claims (p #{:attribute :expr}) sig (fold {:expr [:+ :amount :n]})))))
    (testing "a fold over a far RELATION is claimed only by an engine that says it nests,
              and then only if every node of that relation is claimable too — a nested
              fold is a fold, judged as one"
      (let [nested (fold {:attr :c2 :far-in {:op :with :type :t :name :c2 :fn :count
                                             :link :plain :far :t
                                             :in {:op :scan :type :t}}})]
        (is (= [[0]] (plan/claims (p #{:attribute :expr}) sig nested))
            "an engine that folds attributes and expressions still need not nest")
        (is (= [[]] (plan/claims (p #{:attribute :nested}) sig nested)))
        (is (= [[0]] (plan/claims (assoc (p #{:attribute :nested}) :ops #{:scan}) sig nested))
            "and it must be able to execute the relation's own nodes")
        (testing "a column of that relation has no attribute type, so a :fn narrowed by
                  type does not judge it here — the nested node judges itself"
          (is (= [[]]
                 (plan/claims (assoc (p #{:attribute :nested}) :aggs {:sum #{:long} :count :all})
                              sig nested))))))
    (testing "an :expr folds what its attributes are, so a :fn narrowed by type judges
              every one of them and not the absence of an :attr"
      (let [typed (assoc (p #{:attribute :expr}) :aggs {:sum #{:long}})]
        (is (= [[]] (plan/claims typed sig (fold {:expr [:+ :n 1]}))))
        (is (= [[0]] (plan/claims typed sig (fold {:expr [:+ :n :amount]})))
            "one decimal among them is enough to leave the fold with the core")))))

(deftest typed-aggregates
  (let [agg-of (fn [a]
                 {:op :aggregate :type :t :aggs [{:fn :sum :attr a}] :in {:op :scan :type :t}})
        p      {:ops #{:scan :aggregate} :aggs {:sum #{:long :double} :count :all}}]
    (is (= [[]] (plan/claims p sig (agg-of :n))) "a sum over a long is the engine's")
    (is (= [[0]] (plan/claims p sig (agg-of :amount)))
        "a sum over a decimal stays the core's (Spark, SQLite); the engine still scans")))

(deftest executors
  (let [storage [:pg {:consistency :transactional :latency {:startup-ms 1}}]
        spark   [:spark {:consistency :snapshot :latency {:startup-ms 4000}}]
        duck    [:duck {:consistency :snapshot :latency {:startup-ms 0}}]
        any?    (constantly true)]
    (testing "a question inside a commit goes only to a transactional executor"
      (is (= :pg (first (plan/executor-for [storage duck] {:commit? true :binding? true} any?)))))
    (testing "a snapshot executor reads only bound to the revision's storage snapshot"
      (is (= :pg (first (plan/executor-for [storage duck] {:binding? false} any?))))
      (is (= :duck (first (plan/executor-for [storage spark duck] {:binding? true} any?)))
          "the quickest eligible executor"))
    (testing "none that can: the core answers"
      (is (nil? (plan/executor-for [spark] {:commit? true} any?))))
    (is (= {:rev 7 :storage-snapshot "s-12" :root-rev 3}
           (plan/snapshot-binding {:rev 7 :root-rev 3} "s-12")))))

(deftest divergence-report
  (let [d (scan/divergence :scan {:source [:rows :t]} {:where []} [["a" {}] ["b" {}]]
                           [["b" {}] ["a" {}]] 10)]
    (is (= :pushdown (:store/engine-contract d)))
    (is (true? (get-in d [:diff :order-differs?])))
    (is (= [] (get-in d [:diff :only-pushed])))))

(deftest divergence-by-the-semantics
  (testing "numbers by value: 2 and 2.0M are no difference"
    (let [d (scan/divergence :scan {:source [:rows :t]} nil [["a" {:n 2}]] [["a" {:n 2.0M}]] 10)]
      (is (= [] (get-in d [:diff :only-pushed]) (get-in d [:diff :only-reference])))))
  (testing "a group-by answer is diffed by its groups"
    (let [d (scan/divergence :scan {:source [:rows :t]} nil {2 1 3 1} {2.0M 1} 10)]
      (is (= [[3 1]] (get-in d [:diff :only-pushed])))
      (is (shapes/valid? :driver/divergence d) (pr-str (shapes/explain :driver/divergence d))))))

(def ^:private generated-scan
  [:map {:closed true}
   [:source [:= [:rows :t]]]
   [:where [:vector {:max 3}
            [:or
             [:tuple [:enum := :not= :< :<= :> :>=] [:enum :a :s :db/id]
              [:or :int [:enum "a" "b" "k2"] [:= 2.0M]]]
             [:tuple [:= :in] [:enum :a :s] [:set {:max 3} [:or :int [:enum "a" "b"]]]]
             [:tuple [:= :nil?] [:enum :a :s] :boolean]
             [:tuple [:= :starts-with] [:enum :s :db/id] [:enum "a" "b" "k"]]
             [:tuple [:= :not] [:tuple [:= :=] [:enum :a :s] [:or :int [:enum "a"]]]]]]]
   [:order {:optional true} [:vector {:max 2} [:tuple [:enum :a :s :db/id] [:enum :asc :desc]]]]
   [:after {:optional true} [:enum "k1" "k2" "k3"]]
   [:limit {:optional true} [:int {:min 0 :max 4}]]
   [:project {:optional true} [:set [:enum :a :s]]]
   [:agg {:optional true} [:or [:map [:count? [:= true]]] [:map [:group-by [:enum :a :s]]]]]])

(deftest the-pure-functions-keep-the-driver-shapes
  (doseq [s (mg/sample generated-scan {:size 6 :seed 147 :num-samples 300})]
    (is (shapes/valid? :driver/scan s) (pr-str s))
    (doseq [p [pg-profile pruning-profile projecting-pruner single-comparisons
               {:sources #{:rows} :preds {}}]]
      (let [o (scan/offer p s)
            r (scan/residual s o)
            a (scan/answer s o (engine-answer entries s o))
            ref (scan/reference entries s)]
        (is (shapes/valid? :driver/offer o) (pr-str (shapes/explain :driver/offer o)))
        (is (shapes/valid? :driver/residual r) (pr-str (shapes/explain :driver/residual r)))
        (is (shapes/valid? :driver/scan (scan/pushed s o)))
        (is (shapes/valid? :driver/answer ref))
        (is (scan/same-answer? s a ref) (pr-str {:profile p :scan s :answer a :reference ref}))))))
