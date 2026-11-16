(ns ^:api platform.authoring-lifecycles-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.cli.projection :as projection]
            [sutra.kit.lifecycle :as lc]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.checkout :as ck]
            [sutra.catalog.branch-policy :as policy]
            [sutra.actions.staging :as staging]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth)
(use-fixtures :each pod/as-admin)

(defn- tmp []
  (str (java.nio.file.Files/createTempDirectory "authoring-lifecycles"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))))

(def ^:private text "handel.objects.ent")

(defn- edit! [dir f]
  (let [x (io/file dir text)]
    (spit x (pr-str (update (edn/read-string (slurp x)) :schema f)))))

(defn- add-attribute! [dir attr]
  (edit! dir #(update-in % [:objects :kunde :attributes] conj [attr :string])))

(defn- moved!
  [table event from to & [input]]
  (is (lc/allowed? table event from) (str (:name table) " " event " leaves " from))
  (is (= to (lc/decide table "id" event from {} input))
      (str (:name table) " " event ": " from " → " to))
  to)

(defn- checkout [b dir] (projection/checkout-state {:branch b} (str dir "/" text)))

(defn- batch [b]
  (tx/with-branch b (if (seq (get-in (api/get! :staging) [:body :staged])) :staged :empty)))

(defn- branch [b]
  (some #(when (= b (:name %)) (policy/branch-state (update-vals % (fn [v] (when-not (= "" v) v)))))
        (get-in (api/list-branches {:closed? true}) [:body :branches])))

(defn- proposal [id] (keyword (get-in (api/get! :proposal {:id id}) [:body :status])))

(defn- installed! []
  (is (ok? (api/post! :install
                      {:schema {:projections standard/projections
                                :projects {:handel {}}
                                :objects
                                  {:kunde {:primary-key :nr :attributes [[:nr :string]]
                                           :match-keys [:nr]
                                           :project :handel
                                           :plural "Kunden"}}}})))
  (is (ok? (api/post! :staging))))

(defn- forked! [b]
  (is (ok? (api/post! :branches {:parent "main" :name b})))
  (moved! policy/branch-lifecycle :fork nil (branch b)))

(deftest a-change-goes-from-checkout-through-staging-to-a-landed-proposal
  (installed!)
  (let [b "lc-land" dir (tmp)]
    (forked! b)
    (testing "pull → clean; edit → edited"
      (is (= 0 (projection/pull! {:branch b} "ontologie" {:texts [text] :to dir})))
      (moved! ck/lifecycle :pull :absent (checkout b dir))
      (add-attribute! dir :ort)
      (moved! ck/lifecycle :edit :clean (checkout b dir)))
    (testing "push → the checkout is staged, and so is the branch's batch"
      (is (= :empty (batch b)))
      (is (= 0 (projection/push! {:branch b} [(str dir "/" text)] {})))
      (moved! ck/lifecycle :push :edited (checkout b dir))
      (moved! staging/lifecycle :stage :empty (batch b)))
    (testing "commit → the batch ends committed, the checkout settles clean, the branch took a write"
      (is (ok? (tx/with-branch b (api/post! :staging))))
      (is (= :empty (batch b)) "a committed batch leaves nothing; the next stage opens a new one")
      (moved! staging/lifecycle :commit :staged :committed)
      (moved! ck/lifecycle :commit :staged (checkout b dir))
      (moved! policy/branch-lifecycle :write :active (branch b)))
    (testing "propose → open; land → merged, and the source with it"
      (let [id (get-in (api/post! :proposals {:source b :target "main" :title "Ort"}) [:body :id])]
        (moved! policy/proposal-lifecycle :open nil (proposal id))
        (let [m (api/post! :proposal-merge {:id id} nil)]
          (is (ok? m) (pr-str (:body m))))
        (moved! policy/proposal-lifecycle :land :open (proposal id))
        (moved! policy/branch-lifecycle :merge :active (branch b))))
    (testing "a merged branch takes no write"
      (is (not (lc/allowed? policy/branch-lifecycle :write :merged)))
      (add-attribute! dir :land)
      (is (= 0 (projection/push! {:branch b} [(str dir "/" text)] {})))
      (is (not (ok? (tx/with-branch b (api/post! :staging)))) "its commit is refused"))))

(deftest a-discarded-batch-orphans-its-checkout
  (installed!)
  (let [b "lc-discard" dir (tmp)]
    (forked! b)
    (is (= 0 (projection/pull! {:branch b} "ontologie" {:texts [text] :to dir})))
    (add-attribute! dir :ort)
    (is (= 0 (projection/push! {:branch b} [(str dir "/" text)] {})))
    (is (= :staged (checkout b dir)))
    (testing "discard → the batch ends, the checkout is orphaned"
      (is (ok? (tx/with-branch b (api/delete! :staging))))
      (moved! staging/lifecycle :discard :staged :discarded)
      (moved! ck/lifecycle :discard :staged (checkout b dir)))
    (testing "only pull --force makes it usable again"
      (is (= 1 (projection/pull! {:branch b} "ontologie" {:texts [text] :to dir})))
      (is (= 1 (projection/push! {:branch b} [(str dir "/" text)] {})))
      (is (= 0 (projection/pull! {:branch b} "ontologie" {:texts [text] :to dir :force? true})))
      (moved! ck/lifecycle :pull-force :orphaned (checkout b dir)))))

(deftest closing-a-branch-closes-its-proposal-and-a-new-fork-inherits-nothing
  (installed!)
  (let [b "lc-close" dir (tmp)]
    (forked! b)
    (is (= 0 (projection/pull! {:branch b} "ontologie" {:texts [text] :to dir})))
    (add-attribute! dir :ort)
    (is (= 0 (projection/push! {:branch b} [(str dir "/" text)] {})))
    (is (ok? (tx/with-branch b (api/post! :staging))))
    (let [id (get-in (api/post! :proposals {:source b :target "main" :title "Ort"}) [:body :id])]
      (is (ok? (api/delete! :branch {:name b})))
      (moved! policy/proposal-lifecycle :close :open (proposal id))
      (is (#{:closed :purged} (branch b)))
      (is (not (lc/allowed? policy/proposal-lifecycle :land :closed))))
    (testing "the name forked again is a new branch, not the closed one"
      (forked! b)
      (is (= 0 (projection/pull! {:branch b} "ontologie" {:texts [text] :to (tmp)}))))))
