(ns ^:unit grammar.resource-regimes-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.types.entities :as ents]
            [sutra.types.purposes :as purposes]))

(def ^:private allowed
  {
   :no-text-for-a-contract    #{:llm-models}
   :may-outlive-what-it-shows #{:graphs}})

(defn- check-regimes
  [allowed]
  (let [matrix (ents/regime-matrix)
        gaps   (remove (fn [{:keys [verdict section]}]
                         (let [a (get allowed verdict #{})]
                           (or (= :all a) (contains? a section))))
                       (ents/regime-gaps matrix))]
    (doseq [[verdict why] (sort-by key ents/regime-verdicts)
            :let [rows (filter #(= verdict (:verdict %)) gaps)]]
      (is (empty? rows)
          (str (name verdict) " — " why ":\n"
               (str/join "\n" (for [r (sort-by (comp str :section) rows)]
                                (str "  " (name (:section r))
                                     ": regime " (pr-str (:regime r))
                                     ", tree " (pr-str (:tree r))
                                     (when (seq (:points-at r))
                                       (str ", points at " (pr-str (:points-at r))))))))))
    matrix))

(deftest every-resource-says-who-may-say-no
  (testing "the ledger is the work list; what is not on it fails"
    (is (= (count ents/entities) (count (check-regimes allowed))))))

(deftest a-regime-is-judged-without-the-table
  (testing "the verdict follows from regime, tree and what it points at — nothing else"
    (let [v #(map :verdict (ents/regime-gaps [%]))]
      (is (= [:no-regime]      (v {:section :x :regime nil :tree nil :points-at []})))
      (is (= [:unknown-regime] (v {:section :x :regime :erfunden :tree nil :points-at []})))
      (is (= [:no-text-for-a-contract]
             (v {:section :x :regime :declared :tree nil :points-at []})))
      (is (= [:text-without-review]
             (v {:section :x :regime :view :tree :ontology :points-at []})))
      (is (= [:may-outlive-what-it-shows]
             (v {:section :x :regime :view :tree nil :points-at [:object-types]})))
      (is (empty? (v {:section :x :regime :declared :tree :ontology :points-at [:object-types]}))
          "declared and held by a text: nothing to say"))))

(deftest the-reason-is-data-so-a-reader-is-told-the-decision-itself
  (testing "every regime carries its label, its why and what an undo is"
    (doseq [[k r] ents/regimes]
      (is (every? (comp seq str) [(:label r) (:why r) (:undo r)])
          (str k " explains itself"))))
  (testing "the reader's table is DERIVED — one row per resource, each with its reason"
    (is (= (count ents/entities) (count (ents/regime-report))))
    (is (every? :why (ents/regime-report)))))

(deftest coverage-is-judged-in-both-directions
  (testing "a resource nobody can explain, and a sentence about nothing"
    (is (= [{:section :b :verdict :no-purpose}]
           (remove nil? (purposes/coverage-gaps [:a :b] {:a "warum a"}))))
    (is (= [{:section :weg :verdict :no-such-resource}]
           (remove nil?
                   (purposes/coverage-gaps [:a] {:a "warum a" :weg "ein Satz ohne Ressource"}))))
    (is (= [{:section :a :verdict :empty-purpose}]
           (remove nil? (purposes/coverage-gaps [:a] {:a "   "})))
        "present and silent reads as covered and is not")
    (is (empty? (remove nil? (purposes/coverage-gaps [:a] {:a "warum a"})))
        "one sentence per resource, no more: nothing to say")))

(deftest the-readers-table-is-derived-and-complete
  (testing "one row per resource, each saying why it exists AND why it is versioned so"
    (let [t (purposes/reader-table)]
      (is (= (count ents/entities) (count t)))
      (is (every? #(and (seq (str (:purpose %))) (seq (str (:why %))) (seq (str (:undo %)))) t)
          "a row that cannot explain itself would be a document with a hole in it"))))

(deftest a-regime-says-where-a-change-is-carried
  (testing "a regime that promises a branch, against a section no branch changes"
    (let [v #(map :verdict (ents/regime-gaps
                            [(merge {:section :x :tree :ontology :points-at []} %)]))]
      (is (= [:branch-cannot-carry-it] (v {:regime :declared :branching :instance})))
      (is (= [:branched-after-all]     (v {:regime :declared-on-main})))
      (is (empty? (v {:regime :declared}))
          "branched and :declared — the two statements agree")
      (is (empty? (v {:regime :declared-on-main :branching :instance}))
          "instance-wide and declared at the trunk — they agree too")))
  (testing ":declared-on-main is reviewed, so the earlier laws keep seeing it"
    (is (contains? ents/reviewed :declared-on-main))
    (is (= [:no-text-for-a-contract]
           (map :verdict (ents/regime-gaps [{:section :x :regime :declared-on-main
                                             :tree nil :branching :instance :points-at []}])))
        "a contract with no text is one whether a branch or only the trunk carries it")))

(deftest the-two-tables-say-the-same-thing-about-every-instance-wide-section
  (testing "`instance-sections` and the regimes are two spellings of one fact, so they are
            held against each other instead of being kept in step by hand"
    (is (seq ents/instance-sections))
    (is (= #{:declared-on-main}
           (set (map ents/section->regime ents/instance-sections)))
        "every section a branch never changes is declared at the trunk")
    (is (empty? (remove ents/instance-sections
                        (for [e ents/entities :when (= :declared-on-main (:regime e))]
                          (:section e))))
        "and nothing else claims that regime")))

