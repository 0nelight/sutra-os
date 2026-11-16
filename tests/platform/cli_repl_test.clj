(ns ^:api platform.cli-repl-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.cli.parse :as parse]
            [sutra.cli.repl :as repl]
            [sutra.cli.session :as session]
            [support.pod :as pod]
            [support.seed :as seed]))

(use-fixtures :once pod/with-mem-pod)

(defn- with-repl
  [f]
  (repl/with-session {:yes? true} f))

(deftest register-carries-id-from-read-to-next-verb
  (testing "setup — a :note type + two matching objects on main"
    (api/mutate :create-project {:name :notizen} {:stage? true})
    (api/mutate :create-object-type
                {:name :note :project :notizen :primary-key :text
                 :attributes [{:attribute-name :text
               :attribute-type :string}]
                 :plural "Notes"}
                {:stage? true})
    (is (ok? (api/post! :staging)) "commit :note type")
    (seed/objects! :note [{:values {:text "versandavis alpha"}}
                          {:values {:text "versandavis beta"}}]))

  (with-repl
    (fn [ctx sess]
      (testing "explore populates the register (#1..#n / $)"
        (repl/dispatch-line! ctx sess "explore versandavis")
        (is (= 2 (count (session/rows sess))) "both notes captured")
        (is (every? :id (session/rows sess)) "each row carries a live object id"))

      (testing "a pronoun resolves against the register into the NEXT command"
        (let [id1 (:id (first (session/rows sess)))]
          (let [{:keys [steps]} (parse/line->cmds sess "around #1")]
            (is (= [:around {:ids [id1]}] (first steps))
                "#1 resolved to the seeded object id"))
          (is (nil? (repl/dispatch-line! ctx sess "around #1")))))

      (testing "a saved set survives a later read"
        (repl/dispatch-line! ctx sess "explore versandavis")
        (session/save-set! sess "hits")
        (repl/dispatch-line! ctx sess "explore alpha")
        (is (= 2 (count (session/named-set sess "hits"))) "saved set persisted")))))

(deftest save-gate-commits-a-clean-batch
  (with-repl
    (fn [ctx sess]
      (testing "stage a create, then `save` (gate passes on a valid schema)"
        (binding [tx/*auth-token* @(:token ctx)]
          (is (ok? (api/mutate :create-project {:name :memos} {:stage? true})))
          (is (ok? (api/mutate :create-object-type
                               {:name :memo :project :memos :primary-key :text
                                :attributes [{:attribute-name :text
                              :attribute-type :string}]
                                :plural "Memos"}
                               {:stage? true}))))
        (repl/dispatch-line! ctx sess "check")
        (repl/dispatch-line! ctx sess "save")
        (is (zero? (:staged @sess)) "staged count cleared after save")
        (binding [tx/*auth-token* @(:token ctx)]
          (is (zero? (count (:staged (:body (api/get! :staging))))) "batch actually committed")))
      (testing "`save` with nothing staged is a no-op, not a commit"
        (repl/dispatch-line! ctx sess "save")
        (is (zero? (:staged @sess)))))))

(deftest checkout-moves-the-session-branch
  (with-repl
    (fn [ctx sess]
      (let [child (str "repl-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
        (is (ok? (binding [tx/*auth-token* @(:token ctx)]
                   (api/post! :branches {:parent "main" :name child}))))
        (repl/dispatch-line! ctx sess (str "checkout " child))
        (is (= child (:branch @sess)) "checkout captured the branch into the prompt state")
        (repl/dispatch-line! ctx sess "checkout main")
        (is (= "main" (:branch @sess)))))))
