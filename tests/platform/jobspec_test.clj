(ns ^:unit platform.jobspec-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.plan.jobspec :as jobspec]))

(def ^:private a "ri.d.a")
(def ^:private b "ri.d.b")
(def ^:private c "ri.d.c")

(defn- t [nm q in out]
  {:transform-name nm :language :pg-sql :query q
   :inputs {:i {:kind :dataset :dataset in}}
   :output {:kind :dataset :dataset out}})

(def ^:private datasets {a {:dataset-name :a} b {:dataset-name :b} c {:dataset-name :c}})

(defn- link [branch ts base main?]
  {:branch branch :transforms ts
   :published (jobspec/published {:transforms ts} (when base {:transforms base}) main?)})

(defn- build [branch-ts base main-ts]
  (jobspec/build-transforms [(link "feature" branch-ts base false)
                             (link "main" main-ts nil true)]
                            {:datasets datasets :transforms branch-ts}))

(deftest the-id-is-the-logic-not-the-description
  (let [x (t :ab "SELECT * FROM a" a b)]
    (is (= (jobspec/id {} x) (jobspec/id {} (assoc x :description "neu"))))
    (is (not= (jobspec/id {} x) (jobspec/id {} (assoc x :query "SELECT 1 FROM a"))))))

(deftest a-spec-names-inputs-logic-outputs-and-worker
  (let [s (jobspec/spec-of {} "ri.t.1" (t :ab "SELECT * FROM a" a b))]
    (is (= [[:datasets a]] (:inputs s)))
    (is (= [[:datasets b]] (:outputs s)))
    (is (= :transforms (:worker-type s)))
    (is (= "pg-sql" (get-in s [:logic :language])))
    (is (= :sync (:worker-type (jobspec/spec-of {} "ri.t.2"
                                                {:transform-name :raw
                                                 :input {:kind :datasource :datasource :sap}
                                                 :output {:kind :dataset :dataset a}}))))))

(deftest main-publishes-all-a-branch-only-what-it-wrote
  (let [old {"ri.t.1" (t :ab "SELECT * FROM a" a b) "ri.t.2" (t :bc "SELECT * FROM b" b c)}
        now (assoc-in old ["ri.t.2" :query] "SELECT x FROM b")]
    (is (= #{"ri.t.1" "ri.t.2"} (jobspec/published {:transforms old} nil true)))
    (is (= #{"ri.t.2"} (jobspec/published {:transforms now} {:transforms old} false)))
    (is (= #{} (jobspec/published {:transforms (assoc-in old ["ri.t.1" :description] "d")}
                                    {:transforms old} false)))))

(deftest an-unpublished-output-falls-back-to-mains-current-jobspec
  (let [forked {"ri.t.1" (t :ab "SELECT * FROM a" a b)}
        main   {"ri.t.1" (t :ab "SELECT y FROM a" a b)}
        r      (build forked forked main)]
    (is (empty? (:problems r)))
    (is (= "SELECT y FROM a" (get-in r [:transforms "ri.t.1" :query]))
        "logic main changed after the fork is what the branch builds")
    (is (= "main" (get-in r [:jobspecs [:datasets b] :branch])))))

(deftest a-branchs-own-jobspec-wins
  (let [forked {"ri.t.1" (t :ab "SELECT * FROM a" a b)}
        mine   {"ri.t.1" (t :ab "SELECT z FROM a" a b)}
        main   {"ri.t.1" (t :ab "SELECT y FROM a" a b)}
        r      (build mine forked main)]
    (is (= "SELECT z FROM a" (get-in r [:transforms "ri.t.1" :query])))
    (is (= "feature" (get-in r [:jobspecs [:datasets b] :branch])))))

(deftest an-output-resolves-to-whatever-main-builds-it-with-now
  (let [forked {"ri.t.1" (t :ab "SELECT * FROM a" a b)}
        main   {"ri.t.9" (t :ab2 "SELECT * FROM a" a b)}
        r      (build forked forked main)]
    (is (= ["ri.t.9"] (keys (:transforms r)))
        "main replaced the producer of b: the branch builds b with the new one")
    (testing "main deleted it outright: no JobSpec anywhere, b is not built"
      (is (empty? (:transforms (build forked forked {})))))))

(deftest a-fallback-the-branch-cannot-run-is-a-problem
  (let [forked {"ri.t.2" (t :bc "SELECT * FROM b" b c)}
        main   {"ri.t.2" (t :bc "SELECT * FROM n" "ri.d.new" c)}
        r      (build forked forked main)]
    (is (= [{:problem :missing :transform-rid "ri.t.2" :branch "main"
             :ref [:datasets "ri.d.new"]}]
           (:problems r)))))

(deftest a-fallback-reads-a-dataset-only-its-branch-declares
  (let [forked {"ri.t.2" (t :bc "SELECT * FROM b" b c)}
        main   {"ri.t.2" (t :bc "SELECT * FROM n" "ri.d.new" c)}
        r      (jobspec/build-transforms
                [(link "feature" forked forked false)
                 (assoc (link "main" main nil true)
                        :datasets {"ri.d.new" {:dataset-name :new}})]
                {:datasets datasets :transforms forked})]
    (is (empty? (:problems r)))
    (is (= {"ri.d.new" {:dataset-name :new}} (:datasets r)))))

(deftest a-reference-by-name-finds-its-dataset
  (let [forked {"ri.t.2" (t :bc "SELECT * FROM b" "b" c)}
        r      (build forked forked forked)]
    (is (empty? (:problems r))
        "an input kept as a name resolves at run time, so it is no missing dataset")))

(defn- writes-type [nm q ot]
  {:transform-name nm :language :pg-sql :query q :inputs {}
   :output {:kind :object-type :object-type ot}})

(deftest every-writer-of-a-type-stays-in-a-branch-build
  (let [ts      {"ri.t.shape" (writes-type :shape "SELECT 1" "ri.o.x")
                 "ri.t.supp"  (writes-type :supp "SELECT 2" "ri.o.x")}
        now     (assoc-in ts ["ri.t.supp" :query] "SELECT 3")
        resolve (fn [mine]
                  (jobspec/build-transforms
                   [(link "feature" mine ts false) (link "main" now nil true)]
                   {:object-types {"ri.o.x" {}} :transforms mine}))]
    (testing "untouched: both writers, main's current logic"
      (let [r (resolve ts)]
        (is (= #{"ri.t.shape" "ri.t.supp"} (set (keys (:transforms r)))))
        (is (= "SELECT 3" (get-in r [:transforms "ri.t.supp" :query])))
        (is (empty? (:problems r)))))
    (testing "one published on the branch: that one from the branch, the other from main"
      (let [r (resolve (assoc-in ts ["ri.t.supp" :query] "SELECT 9"))]
        (is (= #{"ri.t.shape" "ri.t.supp"} (set (keys (:transforms r)))))
        (is (= "SELECT 9" (get-in r [:transforms "ri.t.supp" :query])))
        (is (empty? (:problems r)))))
    (testing "a writer main retired is not built"
      (let [r (jobspec/build-transforms
               [(link "feature" ts ts false)
                (link "main" (dissoc ts "ri.t.supp") nil true)]
               {:object-types {"ri.o.x" {}} :transforms ts})]
        (is (= ["ri.t.shape"] (keys (:transforms r))))))))

(deftest a-second-writer-of-a-dataset-stays-a-problem
  (let [ts {"ri.t.1" (t :one "SELECT 1" a c)
            "ri.t.2" (assoc (t :two "SELECT 2" a b)
                            :outputs [{:kind :dataset :dataset b}
                                      {:kind :dataset :dataset c}])}
        r  (build ts ts ts)]
    (is (some #(= :claimed (:problem %)) (:problems r)))))

(deftest rollback-follows-counterpart
  (let [spec {:spec {:id "j2"}}]
    (is (nil? (jobspec/rollback-verdict {:kind :standalone} nil "j1"))
        "it takes a transactional dataset, not for a JobSpec: an upload rolls back")
    (doseq [ds [{:stream {}} {:kind :restricted-view} {:kind :virtual}]]
      (is (:refuse (jobspec/rollback-verdict ds spec "j2")) (pr-str ds)))
    (is (re-find #"logic was changed after the selected transaction"
                 (:warning (jobspec/rollback-verdict {} spec "j1"))))
    (is (nil? (jobspec/rollback-verdict {} spec "j2")))
    (is (nil? (jobspec/rollback-verdict {} spec nil))
        "a version built before JobSpecs were recorded cannot tell")))

(deftest on-main-nothing-changes
  (let [ts {"ri.t.1" (t :ab "SELECT * FROM a" a b) "ri.t.2" (t :bc "SELECT * FROM b" b c)}
        r  (jobspec/build-transforms [(link "main" ts nil true)]
                                     {:datasets datasets :transforms ts})]
    (is (= ts (:transforms r)))
    (is (empty? (:problems r)))))

(def ^:private reads
  {:transform-name :parse :language :python :code "def f(): ..."
   :inputs {:llm {:kind :language-model :model "lesemodell"}
            :i   {:kind :dataset :dataset a}}
   :output {:kind :dataset :dataset b}})

(defn- registry [pm] {:llm-models {:lesemodell {:in 1 :out 2 :source :xai :provider-model pm}}})

(deftest the-model-a-name-resolves-to-is-part-of-the-logic
  (let [v1 (registry "grok-4.20") v2 (registry "grok-5")]
    (is (not= (jobspec/id v1 reads) (jobspec/id v2 reads))
        "a new model version is a new function version")
    (is (= (jobspec/id v1 reads)
           (jobspec/id (assoc-in v1 [:llm-models :lesemodell :in] 9) reads))
        "a price is not logic")
    (is (= [{:alias "llm" :model "lesemodell" :source "xai" :provider-model "grok-5"}]
           (get-in (jobspec/spec-of v2 "ri.t.1" reads) [:logic :models])))
    (testing "a branch that switches the registry publishes the transform"
      (let [ts {"ri.t.1" reads}]
        (is (= #{"ri.t.1"} (jobspec/published (assoc v2 :transforms ts)
                                              (assoc v1 :transforms ts) false)))
        (is (= #{} (jobspec/published (assoc v1 :transforms ts)
                                      (assoc v1 :transforms ts) false)))))))
