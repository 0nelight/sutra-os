(ns ^:unit ontology.build-dag-test
  (:require [sutra.ref.install :as ri]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.outputs :as outputs]
            [sutra.builds.plan :as plan]
            [sutra.catalog.transforms :as transforms]
            [sutra.types.world-rules :as rules]))

(def ^:private schema
  {:object-types {:email {} :be {} :pos-a {} :pos-b {}}
   :link-types   {:l {:from-object-type :be :to-object-type :pos-a}}
   :transforms
   {:pull-email {:transform-name :pull-email
                 :input  {:kind :datasource :datasource :imap}
                 :output {:kind :object-type :object-type :email}}
    :classify   {:transform-name :classify
                 :input  {:kind :datasource :datasource :src}
                 :output {:kind :object-type :object-type :be}}
    :parse-a    {:transform-name :parse-a :language :python
                 :inputs {:mail {:kind :object-type :object-type :email}}
                 :output {:kind :object-type :object-type :pos-a}}
    :parse-b    {:transform-name :parse-b :language :python
                 :inputs {:mail {:kind :object-type :object-type :email}}
                 :output {:kind :object-type :object-type :pos-b}}
    :link-x     {:transform-name :link-x :language :pg-sql
                 :output {:kind :link-type :link-type :l}}}})

(defn- ts [k] (get-in schema [:transforms k]))

(deftest full-deps-test
  (testing "a datasource root has no in-schema deps"
    (is (= #{} (transforms/full-deps schema (ts :pull-email))))
    (is (= #{} (transforms/full-deps schema (ts :classify)))))
  (testing "an :object-type input depends on that type's PRODUCER transform"
    (is (= #{:pull-email} (transforms/full-deps schema (ts :parse-a))))
    (is (= #{:pull-email} (transforms/full-deps schema (ts :parse-b)))))
  (testing "a link backing transform depends on BOTH its endpoint object-types' producers"
    (is (= #{:classify :parse-a} (transforms/full-deps schema (ts :link-x))))))

(deftest topo-levels-test
  (let [levels (mapv set (transforms/topo-levels schema))]
    (testing "roots share level 0; parsers level 1; the link waits for both endpoints"
      (is (= [#{:pull-email :classify}
              #{:parse-a :parse-b}
              #{:link-x}]
             levels)))
    (testing "every transform appears exactly once"
      (is (= (set (keys (:transforms schema)))
             (into #{} cat levels))))
    (testing "a node never precedes a node it depends on"
      (let [idx (into {} (for [[i lvl] (map-indexed vector levels), k lvl] [k i]))]
        (doseq [[k t] (:transforms schema)
                d      (transforms/full-deps schema t)]
          (is (< (idx d) (idx k)) (str k " must build after " d)))))))

(deftest upstream-closure-test
  (testing "closure includes the target + its transitive upstream, excludes unrelated siblings"
    (is (= #{:link-x :classify :parse-a :pull-email}
           (transforms/upstream-closure schema [:link-x])))
    (is (not (contains? (transforms/upstream-closure schema [:link-x]) :parse-b))))
  (testing "a leaf's closure is just itself"
    (is (= #{:pull-email} (transforms/upstream-closure schema [:pull-email]))))
  (testing "unknown targets are dropped"
    (is (= #{} (transforms/upstream-closure schema [:nope])))))

(def ^:private siblings-schema
  {:object-types {:buchungssatz {} :rechnung {} :partner {}}
   :transforms
   {:shape-buchungssatz
    {:transform-name :shape-buchungssatz :language :pg-sql
     :input  {:kind :datasource :datasource :mro}
     :output {:kind :object-type :object-type :buchungssatz}}
    :resolve-buchung-rechnung
    {:transform-name :resolve-buchung-rechnung :language :pg-sql
     :inputs {:buchungssatz {:kind :object-type :object-type :buchungssatz}
              :rechnung     {:kind :object-type :object-type :rechnung}}
     :output {:kind :object-type :object-type :buchungssatz
              :primary-key :buchung-id :role :supplement}}
    :resolve-buchungssatz-partner
    {:transform-name :resolve-buchungssatz-partner :language :pg-sql
     :inputs {:buchungssatz {:kind :object-type :object-type :buchungssatz}
              :partner      {:kind :object-type :object-type :partner}}
     :output {:kind :object-type :object-type :buchungssatz
              :primary-key :buchung-id :role :supplement}}}})

(deftest sibling-supplements-do-not-deadlock-the-planner
  (testing "two supplements on the SAME type schedule instead of throwing"
    (is (some? (transforms/topo-levels siblings-schema))
        "each sibling is a producer of the type the other reads, so without
         dropping the pair Kahn stalls and EVERY build on the instance dies —
         not only the branch below that type"))
  (testing "neither sibling depends on the other"
    (let [g (transforms/dep-graph siblings-schema)]
      (is (not (contains? (g :resolve-buchung-rechnung) :resolve-buchungssatz-partner)))
      (is (not (contains? (g :resolve-buchungssatz-partner) :resolve-buchung-rechnung)))))
  (testing "the §7b edge SURVIVES: a supplement still builds after the primary"
    (let [g (transforms/dep-graph siblings-schema)]
      (is (contains? (g :resolve-buchung-rechnung) :shape-buchungssatz))
      (is (contains? (g :resolve-buchungssatz-partner) :shape-buchungssatz))))
  (testing "the primary lands strictly before both supplements"
    (let [lvls (transforms/topo-levels siblings-schema)
          at   (fn [k] (first (keep-indexed #(when (some #{k} %2) %1) lvls)))]
      (is (< (at :shape-buchungssatz) (at :resolve-buchung-rechnung)))
      (is (< (at :shape-buchungssatz) (at :resolve-buchungssatz-partner))))))

(defn- reordered-transforms
  [sc]
  (let [ts (:transforms sc), ks (vec (keys ts))]
    (for [i (range (count ks))
          :let [order (concat (drop i ks) (take i ks))]]
      (assoc sc :transforms (into (array-map) (map (fn [k] [k (ts k)])) order)))))

(deftest the-backing-of-a-type-is-never-a-supplement
  (doseq [sc (reordered-transforms siblings-schema)
          :let [order (vec (keys (:transforms sc)))]]
    (testing (str "transform order " order)
      (is (= :shape-buchungssatz
             (first (outputs/primary-output-for sc :object-type :buchungssatz)))
          "the primary is picked regardless of where it sits in the map")
      (is (= [:shape-buchungssatz]
             (mapv first (outputs/primary-outputs-for sc :object-type :buchungssatz)))
          "and a supplement is never counted as one")))
  (testing "the PK comes off the primary, not off whichever output sorts first"
    (let [sc (assoc-in siblings-schema
                       [:transforms :shape-buchungssatz :output :primary-key]
                       :satz-id)]
      (doseq [sc (reordered-transforms sc)]
        (is (= :satz-id (rules/object-primary-key sc :buchungssatz))
            "the supplements' :buchung-id must not answer this"))))
  (testing "a type whose ONLY output is a supplement has no primary to count"
    (let [sc (update siblings-schema :transforms dissoc :shape-buchungssatz)]
      (is (empty? (outputs/primary-outputs-for sc :object-type :buchungssatz)))
      (is (some? (outputs/primary-output-for sc :object-type :buchungssatz))
          "the single-arg read still answers, so the gates report a MALFORMED type
           rather than an absent one"))))

(def ^:private backed-schema
  (assoc siblings-schema
         :transforms
         (array-map
          :resolve-buchung-rechnung
          {:transform-name :resolve-buchung-rechnung :language :pg-sql
           :incremental {:write-mode :modify}
           :inputs {:buchungssatz {:kind :object-type :object-type :buchungssatz}}
           :output {:kind :object-type :object-type :buchungssatz :dataset :supp-ds
                    :primary-key :buchung-id :role :supplement}}
          :shape-buchungssatz
          {:transform-name :shape-buchungssatz :language :pg-sql
           :inputs {:mro {:kind :datasource :datasource :mro}}
           :output {:kind :object-type :object-type :buchungssatz :dataset :satz-ds
                    :primary-key :satz-id :column-map {:satz-id :SATZ_ID}}})))

(deftest the-backing-value-answers-without-the-transform-spec
  (let [b (backing/object-backing backed-schema :buchungssatz)]
    (testing "it names the PRIMARY's producer, dataset and key"
      (is (= :shape-buchungssatz (:producer b)))
      (is (= :satz-ds (:dataset b)))
      (is (= :satz-id (:primary-key b)))
      (is (= {:satz-id :SATZ_ID} (:column-map b)))
      (is (= :primary (:role b))))
    (testing "and the WRITE facts come from that same producer"
      (is (true? (:snapshot? b))
          "the primary is a full refresh, so the deletion reconcile RUNS — read off the
           supplement this is false and rows the primary retired stay live")
      (is (false? (:modify? b)))
      (is (false? (:incremental? b)))
      (is (true? (:external? b)) "its rows come from a datasource, so an empty read must
                                  not propagate as a deletion")))
  (testing "backing-of answers the same for a target with no supplements"
    (is (= {:producer :shape-buchungssatz :dataset :satz-ds :incremental? false
            :snapshot? true :modify? false :external? true}
           (backing/backing-of backed-schema :object-type :buchungssatz))))
  (testing "an unbacked target has no backing at all"
    (is (nil? (backing/object-backing backed-schema :rechnung)))
    (is (nil? (backing/backing-of backed-schema :object-type :rechnung)))))

(def ^:private authored-schema
  {:object-types {:buchungssatz {:backing {:dataset :satz-ds :primary-key :satz-id
                                           :column-map {:satz-id :SATZ_ID}}
                                 :plural "Buchungssätze"}
                  :rechnung     {:backing {:dataset :shared-ds} :plural "Rechnungen"}
                  :partner      {:backing {:dataset :orphan-ds} :plural "Partner"}}
   :transforms
   {:fill-satz  {:transform-name :fill-satz :language :pg-sql
                 :incremental {:write-mode :modify}
                 :inputs {:mro {:kind :datasource :datasource :mro}}
                 :output {:kind :dataset :dataset :satz-ds}}
    :fill-a     {:transform-name :fill-a :language :pg-sql
                 :inputs {:x {:kind :dataset :dataset :raw}}
                 :output {:kind :dataset :dataset :shared-ds}}
    :fill-b     {:transform-name :fill-b :language :pg-sql
                 :inputs {:y {:kind :dataset :dataset :raw}}
                 :output {:kind :dataset :dataset :shared-ds}}}})

(deftest an-authored-backing-answers-instead-of-a-producer-search
  (testing "the type's own declaration is the backing"
    (let [b (backing/object-backing authored-schema :buchungssatz)]
      (is (= :satz-ds (:dataset b)))
      (is (= :satz-id (:primary-key b)))
      (is (= {:satz-id :SATZ_ID} (:column-map b)))
      (is (= :fill-satz (:producer b)) "the single transform filling that dataset")
      (is (= :satz-id (rules/object-primary-key authored-schema :buchungssatz)))))
  (testing "the write facts describe the DATASET, not the producer's write-mode"
    (let [b (backing/object-backing authored-schema :buchungssatz)]
      (is (true? (:snapshot? b))
          "`fill-satz` writes :modify, yet the index still reconciles against the dataset's
           current rows — under the old reading that :modify silenced the retraction")
      (is (false? (:modify? b)))
      (is (true? (:external? b)) "the dataset is filled from a datasource")))
  (testing "two producers name no single job — the index is nobody's transform"
    (let [b (backing/object-backing authored-schema :rechnung)]
      (is (= :shared-ds (:dataset b)))
      (is (nil? (:producer b))
          "a nil producer schedules the node last and opens no Job, which is honest;
           picking one of the two would be the very guess this work removes")))
  (testing "a backing nothing fills is still the backing"
    (is (= :orphan-ds (:dataset (backing/object-backing authored-schema :partner))))
    (is (nil? (:producer (backing/object-backing authored-schema :partner)))))
  (testing "backing-of carries only the write facts"
    (is (= #{:producer :dataset :incremental? :snapshot? :modify? :external?}
           (set (keys (backing/backing-of authored-schema :object-type :buchungssatz)))))))

(def ^:private beside-a-cycle
  {:object-types {:ta {:object-type-name :ta} :tb {:object-type-name :tb}
                  :tc {:object-type-name :tc} :td {:object-type-name :td}}
   :transforms
   {:a {:transform-name :a :language :python
        :inputs {:x {:kind :object-type :object-type :tb}}
        :output {:kind :object-type :object-type :ta}}
    :b {:transform-name :b :language :python
        :inputs {:x {:kind :object-type :object-type :ta}}
        :output {:kind :object-type :object-type :tb}}
    :after {:transform-name :after :language :python
            :inputs {:x {:kind :object-type :object-type :ta}}
            :output {:kind :object-type :object-type :tc}}
    :last {:transform-name :last :language :python
           :inputs {:x {:kind :object-type :object-type :tc}}
           :output {:kind :object-type :object-type :td}}}})

(deftest a-cycle-keeps-its-core-out-of-the-levels-and-only-that
  (testing "dropping sibling edges must not blunt the cycle detector"
    (is (= #{:a :b :after :last} (:cycle (transforms/build-graph beside-a-cycle))))
    (is (= [:a :b] (:cycle (first (transforms/loop-problems beside-a-cycle))))))
  (testing "what builds after the cycle keeps its order, the core has no level"
    (is (= [[:after] [:last]] (transforms/topo-levels beside-a-cycle)))))

(deftest a-cycle-refuses-only-the-build-that-names-it
  (is (nil? (plan/resolution-refusal beside-a-cycle [:after :last])))
  (is (re-find #"build resolution failed"
               (:error (plan/resolution-refusal beside-a-cycle [:a :after])))))

(deftest a-diamond-is-a-valid-compute-graph-and-a-cycle-is-not
  (let [valid? #(empty? ((ri/stated-gates :compute-graph-valid?) %))
        t      (fn [& ins]
                 {:inputs (into {} (map (fn [i] [i {:kind :transform :transform i}])) ins)})]
    (is (true? (valid? {:transforms {:a (t) :b (t :a) :c (t :a) :d (t :b :c)}})))
    (is (false? (valid? {:transforms {:a (t :c) :b (t :a) :c (t :b)}})))))
