(ns ^:integration ontology.agent-proposals-store
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.actions.proposals :as proposals]
            [sutra.store.pg.control-tables :as ct]
            [sutra.store.control.tables :as cc]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(defn- reading [auto n]
  {:automation-id auto :object-id (str "o" n) :action :rechnung-erfassen
   :args {:nr (str "R-" n)} :source-key (str "grok_email_attached:" n)
   :reason (str "gelesen " n) :provenance {:model "grok-4" :source-pk (str n)}})

(deftest a-reading-is-staged-once-and-pages
  (let [branch   "main"
        auto (str (random-uuid))]
    (doseq [n (range 5)] (proposals/propose! (ctx/store) branch (reading auto n)))
    (testing "re-running the whole migration adds nothing"
      (is (every? nil? (map #(proposals/propose! (ctx/store) branch (reading auto %)) (range 5))))
      (is (= 5 (count (proposals/list-for (ctx/store) branch auto nil {:limit 100})))))
    (testing "the table refuses a second row for a key even past the check"
      (is (thrown? Exception
                   (ct/insert! (tsys/ds) cc/tables :agent-proposals
                                {:id (str (random-uuid)) :branch "main" :automation-id auto
                                 :action :rechnung-erfassen :args-digest "x" :status "open"
                                 :source-key "grok_email_attached:0"}))))
    (testing "provenance and key round-trip"
      (let [p (first (filter #(= "o1" (:object-id %))
                             (proposals/list-for (ctx/store) branch auto nil)))]
        (is (= {:model "grok-4" :source-pk "1"} (:provenance p)))
        (is (= "grok_email_attached:1" (:source-key p)))))
    (testing "pages do not overlap and cover everything"
      (let [a (proposals/list-for (ctx/store) branch auto :open {:limit 2})
            b (proposals/list-for (ctx/store) branch auto :open {:limit 2 :offset 2})
            c (proposals/list-for (ctx/store) branch auto :open {:limit 2 :offset 4})]
        (is (= 5 (count (into #{} (map :id) (concat a b c)))))))
    (testing "counts by status"
      (proposals/reject! (ctx/store) branch auto
                         (:id
                          (first (proposals/list-for (ctx/store) branch auto :open {:limit 1}))))
      (is (= {"open" 4 "rejected" 1 "applied" 0 "failed" 0}
             (proposals/counts-for (ctx/store) branch auto)))
      (testing "and a rejected reading stays rejected on the next run"
        (is (every? nil? (map #(proposals/propose! (ctx/store) branch (reading auto %)) (range 5))))
        (is (= 4 (get (proposals/counts-for (ctx/store) branch auto) "open")))))))
