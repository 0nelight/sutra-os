(ns ^:integration platform.flaw-git-merge-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.automation.runs :as autos]
            [sutra.branching.proposals :as governance]
            [sutra.repos.riders :as riders]
            [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]
            [sutra.repos.projection :as gp]
            [sutra.repos.checks :as repo-checks]
            [sutra.repos.objects :as g]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.store.seat :as seat]
            [support.pod :as pod]
            [support.wire :refer [ok?]]))

(def ^:private orid rid/ontology-repo-rid)

(def ^:private schema
  {:schema {:projects {:post {}}
            :objects {:eingangsdokument {:project :post
                                         :attributes [[:dok-id :string] [:einordnung :string]]
                                         :match-keys [:dok-id] :primary-key :dok-id
                                         :plural "Eingangsdokumente"}}
            :actions {:einordnen {:name :einordnen
                                  :params {:dokument {:name "Dokument" :type :eingangsdokument
                                                      :required true}}
                                  :rules [{:op :update :object-type :eingangsdokument
                                           :target :dokument
                                           :values {:einordnung "erfasst"}}]
                                  :edits [{:object-type :eingangsdokument}]}}}})

(def ^:private automation
  {:name "einordnen-nachziehen" :on :eingangsdokument
   :when [:= [:get :einordnung] "offen"]
   :effect {:action :einordnen :mode :propose :args {:dokument {:self :id}}}})

(defn- welt [f]
  (let [r (api/post! :install schema)] (assert (ok? r) (pr-str (:body r))))
  (assert (ok? (api/post! :staging)))
  (gp/project-branch! (ctx/store "main") (ctx/repos) (ctx/blobs) orid "main")
  (f))

(use-fixtures :once pod/with-mem-pod-auth)
(use-fixtures :each pod/as-admin)

(defn- project! [b] (gp/project-branch! (ctx/store b) (ctx/repos) (ctx/blobs) orid b))
(defn- tip [b] (g/ref-sha (ctx/repos) orid (str "refs/heads/" b)))
(defn- files [sha] (set (keys (g/read-tree (ctx/repos) orid sha))))
(defn- ent
  ([sha] (ent sha "ontology.ent"))
  ([sha path] (some-> (get (g/read-tree (ctx/repos) orid sha) path) :blob
                      (->> (g/blob-bytes (ctx/repos) orid)) (String. "UTF-8") edn/read-string)))
(defn- pending [] (ctl/list (ctx/store) :git-pending {}))

(defn- branch! [b]
  (is (ok? (api/post! :branches {:parent "main" :name b})))
  (project! b))

(defn- land!
  ([b] (land! b nil))
  ([b mode]
   (let [p (api/post! :proposals
                      (cond-> {:source b :target "main" :title (str "land " b)}
                        mode (assoc :merge-mode mode)))]
     (is (ok? p) (pr-str (:body p)))
     (let [m (api/post! :proposal-merge {:id (get-in p [:body :id])} nil)]
       (is (ok? m) (pr-str (:body m)))
       (:body m)))))

(defn- push-rider! [b]
  (let [parent (tip b)
        text   (pr-str (assoc (ent parent) :automations [automation]))
        sha    (g/with-inserter [ins (ctx/repos) orid]
                 (g/commit! ins
                            {:tree
                               (g/write-tree! ins
                                              (assoc
                                               (into {}
                                                     (for [[p e]
                                                           (g/read-tree (ctx/repos) orid parent)]
                                                            [p
                                                             (g/entry
                                                              (g/blob-bytes (ctx/repos)
                                                                            orid (:blob e))
                                                              (:mode e))]))
                                                    "ontology.ent"
                                                      (g/entry (.getBytes ^String text "UTF-8")
                                                               nil)))
                                  :parents [parent] :author {:name "dev"} :message "rider"}))]
    (g/update-ref! (ctx/repos) orid (str "refs/heads/" b) sha {:expected parent})
    (gp/adopt-push! (ctx/store b)
                    (ctx/repos) (ctx/blobs) orid [{:ref (str "refs/heads/" b) :new sha}])
    (riders/apply! (ctx/store nil {:user "u-admin" :username grants/admin-username}) b
                   (repo-checks/commit-ent (ctx/store) (ctx/repos) orid sha)
                   (repo-checks/commit-ent (ctx/store) (ctx/repos) orid parent))
    (gp/bind-published! (ctx/store b) (ctx/repos) orid b sha nil)
    (gp/normalize-followup! (ctx/store b) (ctx/repos) (ctx/blobs) orid b sha
                            (seat/branch-schema (ctx/at (ctx/store) b)))))

(deftest merge-half
  (welt
   (fn []
     (branch! "mh")
     (gp/commit-files! (ctx/store "mh")
                       (ctx/repos) (ctx/blobs) orid "mh" {"notes/mh.md" "hallo\n"}
                       "notiz")
     (testing "the git half fails: the proposal is merged, the git half is pending"
       (with-redefs [gp/merge-git-half! (fn [& _] (throw (ex-info "git down" {})))]
         (land! "mh"))
       (is (not (contains? (files (tip "main")) "notes/mh.md")))
       (is (= ["mh"] (map :source (pending)))))
     (testing "the next round writes it"
       (governance/git-pending-tick! (ctx/store))
       (is (contains? (files (tip "main")) "notes/mh.md"))
       (is (empty? (pending)))))))

(deftest ein-konflikt-wird-nicht-wiederholt
  (testing "a conflict is a verdict, not a transient failure. It used to DROP the pending
            row — not retried, not recorded, nobody told — so the ontology moved on and
            its git projection silently stayed behind."
    (welt
     (fn []
       (branch! "kf")
       (gp/commit-files! (ctx/store "kf")
                         (ctx/repos) (ctx/blobs) orid "kf" {"notes/kf.md" "hallo\n"}
                         "notiz")
       (let [id (:id (with-redefs [gp/merge-git-half! (fn [& _] {:conflicts ["notes/kf.md"]})]
                       (let [r (land! "kf")]
                         (governance/git-pending-tick! (ctx/store))
                         r)))]
         (testing "it stays, as unresolved, with the paths it could not carry"
           (let [[r] (pending)]
             (is (= "unresolved" (str (:state r))))
             (is (= 1 (:attempts r)))
             (is (re-find #"notes/kf\.md" (str (:error r))))
             (is (nil? (:next-at r)) "and nothing schedules it again")))
         (testing "a round leaves it alone — trying again cannot resolve a conflict"
           (governance/git-pending-tick! (ctx/store))
           (is (= ["unresolved"] (mapv (comp str :state) (pending))))
           (is (= [1] (mapv :attempts (pending))) "no further try was spent on it"))
         (testing "and the proposal answers for it afterwards"
           (let [g (:git-half
                    (governance/proposal (ctx/store) (seat/branch-schema (ctx/store)) id))]
             (is (= :unresolved (:state g)))
             (is (re-find #"notes/kf\.md" (str (:error g))))))
         (ctl/delete! (ctx/store) :git-pending {:proposal-id id}))))))

(deftest rider-split
  (welt
   (fn []
     (branch! "rs")
     (push-rider! "rs")
     (testing "the rider rows land, the rider file does not: pending"
       (with-redefs [gp/commit-files! (fn [& _] (throw (ex-info "git down" {})))]
         (land! "rs")
         (governance/git-pending-tick! (ctx/store)))
       (is (contains? (set (map :name (autos/list-all (ctx/store) "main"))) "einordnen-nachziehen"))
       (is (nil? (:automations (ent (tip "main")))))
       (is (seq (pending))))
     (testing "the next round brings git after the rows"
       (governance/git-pending-tick! (ctx/store))
       (is (= ["einordnen-nachziehen"] (mapv :name (:automations (ent (tip "main"))))))
       (is (empty? (pending)))))))

(deftest stale-merge-parent
  (welt
   (fn []
     (branch! "smp")
     (tx/with-branch "smp"
       (is (ok? (api/post! :install
                           (assoc-in schema [:schema :objects :geschaeftspartner]
                                     {:project :post :attributes [[:partner-id :string]]
                                      :primary-key :partner-id :plural "Geschaeftspartner"}))))
       (is (ok? (api/post! :staging))))
     (land! "smp" "merge")
     (governance/git-pending-tick! (ctx/store))
     (let [{:keys [parents]} (g/commit-info (ctx/repos) orid (tip "main"))]
       (is (= 2 (count parents)) "a merge commit")
       (is (contains? (get-in (ent (second parents) "post.objects.ent") [:schema :objects])
                      :geschaeftspartner)
           "its source parent holds what the source landed")))))
