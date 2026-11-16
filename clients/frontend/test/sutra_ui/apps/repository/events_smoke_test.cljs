(ns sutra-ui.apps.repository.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.repository.events :as ev]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]))

(defn- calls [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(deftest entering-loads-what-the-app-shows
  (let [[_ fx] (step {} [:repository/enter])]
    (is (= 5 (count fx)))
    (is (every? #(= :invoke (first %)) fx))
    (is (some #{[:invoke [gov/refresh-proposals! {:status "open"}]]} fx))))

(deftest saving-a-file-sends-its-draft-and-rechecks
  (let [spec {:transform-name :t :language :sql :inputs [:a] :output {:dataset :o}}
        [{:keys [call ok]}] (calls (step {} [:repository.file/save spec :query "select 1"]))]
    (is (= [ontology/mutate! :update-transform] (take 2 call)))
    (is (= "select 1" (:query (nth call 2))))
    (is (= :t (:name (nth call 2))))
    (is (not (contains? (nth call 2) :outputs)))
    (is (= [[:repository/recheck]] (:then (second ok)))))
  (is (= [[:invoke [data/refresh-schema-problems!]]]
         (second (step {} [:repository/recheck {}])))))

(deftest a-draft-is-kept-per-file
  (let [[s] (step {} [:repository.draft/edit :t "x"])]
    (is (= "x" (get-in s [:drafts :t])))))

(deftest a-failure-is-shown-an-abort-is-not
  (let [[{:keys [err]}] (calls (step {} [:repository.review/approve "p" nil nil]))]
    (is (= [[:invoke [shell/fail! "approve" "nein"]]]
           (remove nil? (second (step {} (conj err "nein"))))))
    (is (empty? (remove nil? (second (step {} (conj err nil))))))))

(deftest a-verdict-reloads-the-proposal
  (let [[{:keys [call ok]}] (calls (step {} [:repository.review/approve "p" "t/1" nil]))]
    (is (= [api/post! :proposal-approve {:id "p"} {:task "t/1"}] call))
    (is (= [[:invoke [gov/load-proposal! "p"]]]
           (remove nil? (second (step {} (conj ok {:ok true}))))))))

(deftest asking-for-review-reads-the-reviewer-draft
  (let [db {:review {"p" {:reviewers " anna, ,bert "}}}
        [{:keys [call]}] (calls (step db [:repository.review/ask "p"]))]
    (is (= [api/ask-review "p" ["anna" "bert"]] call))))

(deftest a-comment-is-sent-and-its-draft-emptied
  (let [db {:review {"p" {:drafts {"t/1" "warum?"}}}}
        [s :as r] (step db [:repository.comment/add "p" "t/1"])
        [{:keys [call]}] (calls r)]
    (is (= "" (get-in s [:review "p" :drafts "t/1"])))
    (is (= {:body "warum?" :task "t/1"} (nth call 3)))))

(deftest resetting-staged-changes-asks-first
  (let [[_ [[k {:keys [ok]}]]] (step {} [:repository.staging/reset 3])]
    (is (= :confirm k))
    (is (= [:repository.staging/clear] ok))
    (is (= [[sync/clear-staged!]] (map :call (calls (step {} ok)))))))

(deftest a-rebase-sends-the-choices-and-clears-them-once-done
  (let [db {:review {"p" {:choices {"x" "mine"}}}}
        [{:keys [call ok]}] (calls (step db [:repository.conflict/rebase "p" "feat"]))]
    (is (= ["feat" {"x" "mine"}] (rest call)))
    (is (= [[:repository.conflict/rebased "p"]] (:then (second ok))))
    (is (nil? (get-in (run db (conj ok {:ok true})) [:review "p" :choices])))
    (is (= [:invoke [gov/load-proposal! "p"]]
           (first (second (step db [:repository.conflict/rebased "p" {:ok true}]))))))
  (testing "a failed rebase keeps the choices"
    (let [db {:review {"p" {:choices {"x" "mine"}}}}]
      (is (= db (first (step db [:repository.conflict/rebased "p" nil])))))))

(deftest conflicts-are-chosen-and-opened-per-proposal
  (let [[s] (step {} [:repository.conflict/choose "p" "x" "theirs"])
        [s] (step s [:repository.conflict/toggle "p" "x"])]
    (is (= {"x" "theirs"} (get-in s [:review "p" :choices])))
    (is (= #{"x"} (get-in s [:review "p" :open-conflicts])))
    (is (= #{} (get-in (first (step s [:repository.conflict/toggle "p" "x"]))
                       [:review "p" :open-conflicts])))))

(deftest a-new-branch-is-forked-only-when-named
  (is (= [] (second (step {} [:repository.branch/named "main" "  "]))))
  (is (= 1 (count (second (step {} [:repository.branch/named "main" "neu"]))))))

(deftest opening-a-proposal-forgets-the-last-ones-review
  (let [[s fx] (step {:review {"p" {:choices {}}}} [:repository.proposal/open "q"])]
    (is (nil? (:review s)))
    (is (= [:invoke [gov/load-proposal! "q"]] (second fx)))))

(deftest reviewers-are-split-on-commas
  (is (= ["a" "b"] (ev/reviewers "a, b,,")))
  (is (= [] (ev/reviewers nil))))
