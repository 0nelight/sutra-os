(ns sutra-ui.apps.repository.proposals-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.apps.repository.proposals :as proposals]))

(def ^:private me-id "f0c3a1d2-0000-4000-8000-000000000001")
(def ^:private other-id "a1b2c3d4-0000-4000-8000-000000000002")

(def ^:private type-task "object-types/ri.sutra.main.object-type.7f3a")
(def ^:private ds-task   "datasets/ri.sutra.main.dataset.7f3a")

(defn- proposal
  [over]
  (merge
   {:id "p-1" :title "Lieferant um Ort erweitern"
    :description "Der Ort hing bisher am Beleg."
    :source-branch "feature/lieferant-ort" :target-branch "main"
    :author other-id :status "open" :stage "in-review"
    :reviewers [me-id] :approvals [] :rejections []
    :approval-count 0 :open-tasks [type-task]
    :tasks [{:task type-task :name "lieferant" :status "modified" :kind "breaking"
             :requires [{:when [:breaking :foreign] :by [:owner] :count 1}]
             :fingerprint "1" :approvals [] :stale [] :rejections []
             :comments [{:id "c-1" :task type-task :author other-id
                         :body "warum wandert der Ort?" :created-at "2026-08-07T09:00:00"}]}
            {:task ds-task :name "lieferant" :status "added" :kind "accretion" :requires []
             :fingerprint "2" :approvals [other-id] :stale [] :rejections []
             :comments []}]
    :comments [{:id "c-1" :task type-task :author other-id
                :body "warum wandert der Ort?" :created-at "2026-08-07T09:00:00"}
               {:id "c-2" :task nil :author me-id
                :body "weil der Beleg ihn nicht besitzt" :created-at "2026-08-07T09:05:00"}]
    :checks {:commit "4f2a9c1b77de0011" :status "passed"
             :checks [{:check "scope" :status "passed"}
                      {:check "ci/sutra-publish" :status "passed"}]}
    :preview {:status "clean" :conflicts []}}
   over))

(defn- render
  [p]
  (reset! gov/open-proposal {:id (:id p) :proposal p})
  (rds/render-to-string [proposals/proposals-view]))

(defn- render-list [ps]
  (reset! gov/open-proposal nil)
  (reset! gov/proposals ps)
  (rds/render-to-string [proposals/proposals-view]))

(deftest page-renders-in-each-stage
  (doseq [stage ["preparing" "in-review" "merge"]]
    (testing (str "stage " stage)
      (let [html (render (proposal {:stage stage}))]
        (is (string? html))
        (is (str/includes? html "Lieferant um Ort erweitern"))
        (is (str/includes? html "feature/lieferant-ort"))))))

(deftest stage-is-named-not-numbered
  (testing ""
    (is (str/includes? (render (proposal {:stage "preparing"})) "Preparing"))
    (is (str/includes? (render (proposal {:stage "merge"})) "Merge proposal"))))

(deftest ask-for-review-is-offered-only-while-preparing
  (testing "committing SAVES; asking for attention is a separate act with a
            moment — so the button exists exactly in the stage before it"
    (is (str/includes? (render (proposal {:stage "preparing"})) "Ask for review"))
    (is (not (str/includes? (render (proposal {:stage "merge"})) "Ask for review")))))

(deftest a-task-shows-name-and-rid
  (testing "an approval is recorded against the ID, so a rename cannot carry it
            onto a task nobody reviewed — while a reviewer still reads the name"
    (let [html (render (proposal {}))]
      (is (str/includes? html "lieferant"))
      (is (str/includes? html type-task)))))

(deftest a-stale-approval-is-named-not-dropped
  (testing "'nobody approved this' and 'this was approved and then changed' are
            different sentences, and only the second means somebody has read it"
    (let [html (render (proposal {:tasks [{:task type-task :name "lieferant"
                                           :status "modified" :fingerprint "9" :kind "breaking"
                                           :requires
                                             [{:when [:breaking :foreign] :by [:owner] :count 1}]
                                           :approvals [] :stale [other-id]
                                           :rejections [] :comments []}]}))]
      (is (str/includes? html "changed since"))
      (is (str/includes? html "awaiting an owner of this resource")
          "and it still counts as unreviewed, not as approved"))))

(deftest a-task-says-why-it-waits-or-why-it-does-not
  (testing "an addition waits for no one, a foreign break waits for its owner and names who it reaches"
    (let [html (render (proposal {:open-tasks [type-task]
                                  :tasks [{:task type-task :name "lieferant" :status "modified"
                                           :kind "breaking"
                                           :requires
                                             [{:when [:breaking :foreign] :by [:owner] :count 1}]
                                           :consumers ["einkauf"]
                                           :fingerprint "1" :approvals [] :stale [] :rejections []
                                           :comments []}
                                          {:task ds-task :name "lieferant" :status "added"
                                           :kind "accretion"
                                           :requires [] :fingerprint "2" :approvals [] :stale []
                                           :rejections [] :comments []}]}))]
      (is (str/includes? html "awaiting an owner of this resource"))
      (is (str/includes? html "breaks a resource used by einkauf"))
      (is (str/includes? html "an addition — waits for no one")))))

(deftest a-rejection-names-who-objected
  (let [html (render (proposal {:tasks [{:task type-task :name "lieferant"
                                         :status "removed" :fingerprint "1"
                                         :approvals [] :stale []
                                         :rejections [other-id] :comments []}]}))]
    (is (str/includes? html "rejected by"))))

(deftest unknown-checks-never-render-as-passed
  (testing "no check having run is not a check that succeeded — it is the state
            of exactly the commit whose fate is in doubt"
    (let [html (render (proposal {:checks {:commit "deadbeefcafe" :status "unknown"
                                           :checks []}}))]
      (is (str/includes? html "prop-pill--unknown"))
      (is (not (str/includes? html "prop-pill--passed"))))))

(deftest no-repository-commits-is-said-rather-than-implied
  (let [html (render (proposal {:checks nil}))]
    (is (str/includes? html "No repository commits"))
    (is (not (str/includes? html "prop-pill--passed")))))

(deftest conflicts-are-named-per-resource
  (let [html (render (proposal {:preview {:status "conflicts"
                                          :conflicts [{:table type-task}]}}))]
    (is (str/includes? html (str "<span class=\"prop-conflict__name\">" type-task "</span>")))))

(deftest the-general-thread-renders-and-the-per-task-one-is-behind-its-row
  (testing "a proposal-level remark is on the page; a per-task one waits until
            its resource is expanded (the row shows the count)"
    (let [html (render (proposal {}))]
      (is (str/includes? html "weil der Beleg ihn nicht besitzt"))
      (is (not (str/includes? html "warum wandert der Ort?"))
          "the task thread is collapsed until its row is opened"))))

(deftest own-comments-are-deletable-and-others-are-not
  (testing "governance records an author as its USER-OBJECT ID. Comparing that
            to a username renders a perfect page with every delete button
            missing — which reads as the feature being absent, not mis-keyed."
    (reset! auth/auth {:status :authed :user me-id :username "conrad"})
    (let [mine (render (proposal {:comments [{:id "c-2" :task nil :author me-id
                                              :body "meins" :created-at "t"}]}))
          theirs (render (proposal {:comments [{:id "c-3" :task nil :author other-id
                                                :body "nicht meins" :created-at "t"}]}))]
      (is (str/includes? mine "Delete this comment"))
      (is (not (str/includes? theirs "Delete this comment"))))))

(deftest the-list-renders-and-under-states-the-stage
  (testing "a row knows only the two stages a database row decides; the third
            costs a diff of two ontologies. It under-states rather than guesses."
    (let [html (render-list [{:id "p-1" :title "Lieferant um Ort erweitern"
                              :source-branch "feature/lieferant-ort"
                              :target-branch "main" :status "open"
                              :stage "merge" :approval-count 2 :open-tasks []
                              :checks {:commit "4f2a9c1b77de" :status "passed"
                                       :checks []}}])]
      (is (str/includes? html "Lieferant um Ort erweitern"))
      (is (str/includes? html "In Review"))
      (is (not (str/includes? html "Merge proposal"))
          "the list never claims the derived stage")
      (is (str/includes? html "prop-pill--passed")
          "but it DOES carry the checks — those cost a ledger read, not a diff"))))

(deftest an-empty-list-says-how-to-start-one
  (let [html (render-list [])]
    (is (str/includes? html "Keine Pull Requests"))))

(deftest the-checks-tab-renders
  (reset! gov/proposals [{:id "p-1" :title "Lieferant um Ort erweitern"
                            :source-branch "feature/lieferant-ort" :status "open"
                            :checks {:commit "4f2a9c1b77de" :status "failed"
                                     :checks [{:check "plan" :status "failed"
                                               :detail "producer removed"}]}}])
  (let [html (rds/render-to-string [proposals/checks-view])]
    (is (str/includes? html "plan"))
    (is (str/includes? html "producer removed"))
    (is (str/includes? html "prop-pill--failed"))))
