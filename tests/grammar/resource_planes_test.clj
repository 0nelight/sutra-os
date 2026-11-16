(ns ^:unit grammar.resource-planes-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.types.entities :as ents]
            [sutra.types.planes :as planes]))

(def ^:private allowed
  {
   :content-without-a-folder-door #{:documents}})

(defn- check-planes [allowed]
  (let [gaps (remove (fn [{:keys [verdict plane]}]
                       (let [a (get allowed verdict #{})]
                         (or (= :all a) (contains? a plane))))
                     (planes/plane-gaps planes/planes ents/regimes))]
    (doseq [[verdict why] (sort-by key planes/plane-verdicts)
            :let [rows (filter #(= verdict (:verdict %)) gaps)]]
      (is (empty? rows)
          (str (name verdict) " — " why ":\n"
               (str/join "\n" (for [r (sort-by (comp str :plane) rows)]
                                (str "  " (name (:plane r))
                                     ": regime " (pr-str (:regime r))
                                     ", doors " (pr-str (:doors r))))))))
    planes/planes))

(deftest every-plane-is-decided-and-keeps-content-out-of-the-review-path
  (testing "the ledger is the work list; what is not on it fails"
    (is (= (count planes/planes) (count (check-planes allowed))))))

(deftest the-contract-door-is-refused-to-content
  (testing "a plane that is not :declared may name no door of the review path"
    (let [row (fn [m] (planes/plane-gaps {:x (merge {:purpose "p" :regime :own-history
                                                     :doors ["document-create"] :folder-sync "x"
                                                     :counterpart "f"}
                                                    m)}
                                         ents/regimes))]
      (is (= [:content-through-the-contract-door]
             (map :verdict (row {:doors ["propose"]})))
          "this is the detour, as a verdict")
      (is (= [:content-through-the-contract-door]
             (map :verdict (row {:doors ["document-create" "merge-proposal"]})))
          "one review door among its own is enough")
      (is (empty? (row {:regime :declared :doors ["propose" "approve" "merge-proposal"]}))
          "a declaration belongs in the review path — that is what it is for")
      (is (empty? (row {})) "content through its own door: nothing to say"))))

(deftest a-plane-row-is-judged-without-the-table
  (testing "the verdict follows from the row alone"
    (let [v #(map :verdict (planes/plane-gaps {:x %} ents/regimes))]
      (is (= [:no-regime]      (v {:purpose "p" :doors ["d"] :counterpart "f"})))
      (is (= [:unknown-regime] (v {:purpose "p" :regime :erfunden :doors ["d"] :counterpart "f"})))
      (is (= [:no-door]        (v {:purpose "p" :regime :setting :doors [] :counterpart "f"})))
      (is (= [:no-purpose]     (v {:purpose "  " :regime :setting :doors ["d"] :counterpart "f"})))
      (is (= [:no-counterpart]
             (v {:purpose "p" :regime :setting :doors ["d"]}))
          "conformance has to be readable, not asserted")
      (is (= [:content-without-a-folder-door]
             (v {:purpose "p" :regime :own-history :doors ["document-create"] :counterpart "f"}))))))

(deftest one-table-covers-every-resource
  (testing "the reader gets entities AND planes, each row explaining itself"
    (let [all (planes/all-resources)]
      (is (= (+ (count ents/entities) (count planes/planes)) (count all)))
      (is (every? #(and (seq (str (:purpose %))) (seq (str (:why %))) (seq (str (:undo %)))) all)
          "a row that cannot explain itself would be a document with a hole in it"))))
