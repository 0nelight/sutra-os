(ns ^:unit ontology.build-loop-test
  (:require [sutra.builds.plan.dag :as dag]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.transforms :as transforms]))

(def ^:private classify     #'dag/classify-node-failure)
(def ^:private doomed-by    #'dag/doomed-by)
(def ^:private levels-of    #'dag/levels-of)
(def ^:private level-groups #'dag/level-groups)
(def ^:private runnable     #'dag/runnable)

(def ^:private schema
  {:object-types {:email {} :be {} :pos {}}
   :link-types   {:l {:from-object-type :be :to-object-type :pos}}
   :transforms
   {:pull-email {:transform-name :pull-email
                 :input  {:kind :datasource :datasource :imap}
                 :output {:kind :object-type :object-type :email}}
    :classify   {:transform-name :classify
                 :input  {:kind :datasource :datasource :src}
                 :output {:kind :object-type :object-type :be}}
    :parse      {:transform-name :parse :language :python
                 :inputs {:mail {:kind :object-type :object-type :email}}
                 :output {:kind :object-type :object-type :pos}}
    :supp-pos   {:transform-name :supp-pos :language :pg-sql
                 :inputs {:p {:kind :object-type :object-type :pos}}
                 :output {:kind :object-type :object-type :pos
                          :primary-key :pk :role :supplement}}
    :link-x     {:transform-name :link-x :language :pg-sql
                 :output {:kind :link-type :link-type :l}}}})

(def ^:private graph (transforms/build-graph schema))

(def ^:private nodes
  [{:kind :link       :key :l     :t-kw :link-x}
   {:kind :supplement :key :pos   :t-kw :supp-pos}
   {:kind :object     :key :pos   :t-kw :parse}
   {:kind :object     :key :be    :t-kw :classify}
   {:kind :object     :key :email :t-kw :pull-email}])

(deftest a-throw-is-classified-before-anything-reacts-to-it
  (is (= :rethrow (:disposition (classify (StackOverflowError.)))))
  (is (= :rethrow (:disposition (classify (ex-info "stop" {:cancelled true})))))
  (let [v (classify (ex-info "dup" {:violations [:a :b] :violation-count 2}))]
    (is (= :restriction (:disposition v)))
    (is (= 2 (:violation-count v)))
    (is (= [:a :b] (:violations v))))
  (is (= :failure (:disposition (classify (RuntimeException. "boom"))))))

(deftest a-failure-dooms-what-reads-it-transitively-and-nothing-else
  (testing "object producer → parser → its supplement → the link on that type"
    (is (= #{:parse :supp-pos :link-x}
           (doomed-by schema graph {:kind :producer :t-kw :pull-email}))))
  (testing "the link's other endpoint producer dooms only the link"
    (is (= #{:link-x} (doomed-by schema graph {:kind :producer :t-kw :classify})))
    (is (not (contains? (doomed-by schema graph {:kind :producer :t-kw :classify}) :parse))))
  (testing "a leaf dooms nobody, and never itself"
    (is (= #{} (doomed-by schema graph {:kind :producer :t-kw :link-x})))
    (is (not (contains? (doomed-by schema graph {:kind :producer :t-kw :pull-email})
                        :pull-email)))))

(deftest nodes-are-levelled-by-producer-and-ordered-by-kind-within-a-level
  (is (= [#{:pull-email :classify} #{:parse} #{:supp-pos} #{:link-x}]
         (mapv #(set (map :t-kw %)) (levels-of graph nodes))))
  (is (= [:object :supplement :link]
         (map key (level-groups [{:kind :link} {:kind :supplement} {:kind :object}]))))
  (testing "a node with no producer in the DAG sorts last, alone"
    (is (= [nil]
           (map :t-kw (last (levels-of graph (conj nodes {:kind :object :key :x :t-kw nil}))))))))

(deftest a-doomed-node-is-skipped-where-its-level-starts
  (let [doomed (doomed-by schema graph {:kind :producer :t-kw :pull-email})
        levels (levels-of graph nodes)]
    (is (= [#{:pull-email :classify} #{} #{} #{}]
           (mapv #(set (map :t-kw (runnable doomed %))) levels)))
    (is (= [#{:pull-email :classify} #{:parse} #{:supp-pos} #{}]
           (mapv #(set (map :t-kw
                            (runnable (doomed-by schema graph {:kind :producer :t-kw :classify})
                                      %))) levels)))))
