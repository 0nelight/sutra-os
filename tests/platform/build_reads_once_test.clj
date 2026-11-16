(ns ^:unit platform.build-reads-once-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.plan.dag :as dag]
            [sutra.builds.plan.freshness :as fresh]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.substrate :as sub]))

(def ^:private best "ri.sutra.main.object-type.bestellung")
(def ^:private raw "ri.sutra.main.dataset.raw")
(def ^:private a "ri.sutra.main.dataset.a")
(def ^:private b "ri.sutra.main.dataset.b")

(def ^:private schema
  {:object-types {best {:object-type-name :bestellung
                        :attribute-list [{:attribute-name :pos-nr :attribute-type :string}]}}
   :sources      {"src-imap" {:source-name :imap :type :imap}}
   :datasets     {raw {:dataset-name :raw_messages :kind :transform-output}
                  a   {:dataset-name :a :kind :transform-output}
                  b   {:dataset-name :b :kind :transform-output}}
   :transforms
   {:pull {:transform-name :pull :language :python
           :input   {:kind :datasource :datasource :imap}
           :outputs [{:kind :dataset :dataset raw :name :raw_messages}]}
    :ta   {:transform-name :ta :language :python
           :inputs  {:in {:kind :dataset :dataset raw}
                     :ob {:kind :object-type :object-type best}}
           :outputs [{:kind :dataset :dataset a :name :a}]}
    :tb   {:transform-name :tb :language :python
           :inputs  {:in {:kind :dataset :dataset raw}
                     :ob {:kind :object-type :object-type best}
                     :up {:kind :dataset :dataset a}}
           :outputs [{:kind :dataset :dataset b :name :b}]}}})

(defn- counting-reads
  [f]
  (let [reads (atom {})
        note! (fn [k] (swap! reads update k (fnil inc 0)))]
    (with-redefs [dsets/content-signature (fn [_ t] (note! [:content t]) (str "sig-" t))
                  sub/type-clock          (fn [_ ot] (note! [:clock ot]) 7)]
      (f))
    @reads))

(deftest a-freshness-decision-reads-each-source-once
  (let [standing #'fresh/standing
        result   (atom nil)
        reads    (counting-reads #(reset! result (standing ::store schema [:pull :ta :tb])))]
    (testing "every source the three fingerprints stand on is read, each once"
      (is (= 4 (count reads)) (pr-str reads))
      (is (every? #{1} (vals reads)) (pr-str reads)))
    (testing "and the decision is made from what was read"
      (is (:volatile? (:pull @result)))
      (is (string? (:fingerprint (:ta @result))))
      (is (string? (:fingerprint (:tb @result)))))))

(deftest the-world-a-decision-reads-is-a-value
  (testing "the questions are known before anything is read"
    (is (= 4 (count (fresh/fingerprint-questions schema [:pull :ta :tb])))))
  (testing "a question not read before deciding is no answer: the fingerprint is unknown"
    (let [ask (fresh/world-asker {[:content "x"] "s"})]
      (is (= "s" (ask [:content "x"])))
      (is (nil? (ask nil)))
      (is (thrown? clojure.lang.ExceptionInfo (ask [:content "y"])))))
  (testing "asking that throws is read as unknown, once"
    (is (= {[:content "x"] ::fresh/unknown}
           (fresh/read-world (fn [_] (throw (ex-info "down" {}))) #{[:content "x"]})))))

(deftest a-link-endpoint-is-read-again-only-when-its-clock-moved
  (let [seed   #'dag/seed-level-writeback!
        nodes  [{:kind :link :key :ereignis-le
                 :spec {:from-type :ereignis :to-type :lieferant :fk-side :from}}]
        clock  (atom 3)
        reads  (atom 0)
        level  (fn [state]
                 (with-redefs [sub/type-clock (fn [_ _] @clock)
                               sub/read-type  (fn [_ t]
                                                (swap! reads inc)
                                                [{:id (str "id-" (name t))
                                                  :values {:adresse (str "a@" @clock)}}])]
                   (seed {:store ::store} nodes state)))
        s1     (level {:acc {}})
        s2     (level s1)]
    (is (= 2 @reads) "both endpoints read in the first level, neither in the second")
    (is (= (get-in s1 [:acc :writeback-index]) (get-in s2 [:acc :writeback-index])))
    (is (= {"a@3" "id-lieferant"} (get-in s2 [:acc :writeback-index :lieferant])))
    (reset! clock 4)
    (let [s3 (level s2)]
      (is (= 4 @reads) "a moved clock reads the rows again")
      (is (= {"a@4" "id-lieferant"} (get-in s3 [:acc :writeback-index :lieferant]))))))
