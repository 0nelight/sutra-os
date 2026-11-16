(ns ^:api platform.riders-branch-merge-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.automation.runs :as autos]
            [sutra.repos.projection :as gp]
            [sutra.repos.checks :as repo-checks]
            [sutra.store.seat :as seat]
            [sutra.branching.proposals :as governance]
            [sutra.repos.riders :as riders]
            [sutra.repos.objects :as g]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]
            [support.in-project :refer [install-in-project]]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private orid rid/ontology-repo-rid)

(def ^:private automation
  {:name "rechnungen-nachspielen" :on :eingangsdokument
   :when [:and [:= [:get :einordnung] "offen"] [:not= [:get :lesung-nr] nil]]
   :effect {:action :rechnung-erfassen :mode :propose
            :args {:dokument {:self :id} :nr {:self :lesung-nr}}
            :key {:template "grok_email_attached:{{dok-id}}"}}})

(def ^:private schema
  {:schema {:objects {:eingangsdokument {:primary-key :dok-id
                                         :attributes [[:dok-id :string] [:einordnung :string]
                                                      [:lesung-nr :string]]
                                         :match-keys [:dok-id]
                                         :plural "Eingangsdokumente"}}
            :actions {:rechnung-erfassen
                      {:name :rechnung-erfassen
                       :params {:dokument {:name "Dokument" :type :eingangsdokument :required true}
                                :nr {:name "Nr" :type :string}}
                       :rules [{:op :update :object-type :eingangsdokument :target :dokument
                                :values {:einordnung "erfasst"}}]
                       :edits [{:object-type :eingangsdokument}]}}}})

(defn- project! [branch]
  (let [store (ctx/store branch)] (gp/project-branch! store (ctx/repos) (ctx/blobs) orid branch)))
(defn- tip [branch] (g/ref-sha (ctx/repos) orid (str "refs/heads/" branch)))
(defn- ent-text
  ([sha] (ent-text sha "ontology.ent"))
  ([sha path]
   (String. ^bytes
            (g/blob-bytes (ctx/repos) orid (:blob (get (g/read-tree (ctx/repos) orid sha) path)))
            "UTF-8")))

(def ^:private objects-text "rechnung.objects.ent")

(defn- push!
  [branch f]
  (let [parent (tip branch)
        text   (f (ent-text parent))
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
                                  :parents [parent] :author {:name "dev"} :message "riders"}))]
    (g/update-ref! (ctx/repos) orid (str "refs/heads/" branch) sha {:expected parent})
    (gp/adopt-push! (ctx/store branch) (ctx/repos) (ctx/blobs) orid
                    [{:ref (str "refs/heads/" branch) :new sha}])
    (riders/apply! (ctx/store nil {:user "u-admin" :username grants/admin-username}) branch
                   (repo-checks/commit-ent (ctx/store) (ctx/repos) orid sha)
                   (repo-checks/commit-ent (ctx/store) (ctx/repos) orid parent))
    (gp/bind-published! (ctx/store branch) (ctx/repos) orid branch sha nil)
    (gp/normalize-followup! (ctx/store branch) (ctx/repos) (ctx/blobs) orid branch sha
                                         (seat/branch-schema (ctx/at (ctx/store) branch)))
    sha))

(defn- with-rider [text]
  (let [form (edn/read-string text)]
    (str ";; Warum.\n" (pr-str (assoc form :automations [automation])) "\n")))

(defn- without-rider [text]
  (pr-str (dissoc (edn/read-string text) :automations)))

(defn- names-on [branch] (set (map :name (autos/list-all (ctx/store) branch))))

(defn- land!
  [branch]
  (let [p (api/post! :proposals {:source branch :target "main" :title (str "land " branch)})]
    (is (ok? p) (pr-str (:body p)))
    (let [m (api/post! :proposal-merge {:id (get-in p [:body :id])} nil)]
      (is (ok? m) (pr-str (:body m)))
      (governance/git-pending-tick! (ctx/store))
      (:body m))))

(deftest an-automation-branches-merges-and-is-removed-by-omission
  (testing "setup: the type and its action on main, projected"
    (is (ok? (install-in-project :rechnung schema)))
    (is (ok? (api/post! :staging)))
    (project! "main"))

  (testing "declared on a branch, it installs there and nowhere else"
    (is (ok? (api/post! :branches {:parent "main" :name "riders-a"})))
    (project! "riders-a")
    (push! "riders-a" with-rider)
    (is (contains? (names-on "riders-a") "rechnungen-nachspielen"))
    (is (not (contains? (names-on "main") "rechnungen-nachspielen"))))

  (testing "landing the proposal installs it on main and writes it into main's ontology.ent"
    (let [r (land! "riders-a")]
      (is (= "pending" (some-> (:file-plane-status r) name))
          "the merge no longer waits for the repositories; it says one is owed")
      (is (nil? (:git-half (:body (api/get! :proposal {:id (:id r)}))))
          "and once the tick carried it the pending row is gone — that IS the done state"))
    (is (contains? (names-on "main") "rechnungen-nachspielen"))
    (let [a (first (filter #(= "rechnungen-nachspielen" (:name %))
                           (autos/list-all (ctx/store) "main")))]
      (is (= (:effect automation) (edn/read-string (:effect-spec a))))
      (is (= (:when automation) (edn/read-string (:when-spec a)))))
    (let [form (edn/read-string (ent-text (tip "main")))]
      (is (= ["rechnungen-nachspielen"] (mapv :name (:automations form))))
      (is (contains? (get-in (edn/read-string (ent-text (tip "main") objects-text))
                             [:schema :objects])
                     :eingangsdokument)
          "the schema is still there, in its project's text")))

  (testing "a later schema change on main keeps the block — the projection restates only :schema"
    (is (ok? (install-in-project :rechnung
                                 (assoc-in schema [:schema :objects :geschaeftspartner]
                                          {:primary-key :partner-id
                                           :attributes [[:partner-id :string]]
                                           :plural "Geschaeftspartner"}))))
    (is (ok? (api/post! :staging)))
    (project! "main")
    (let [form (edn/read-string (ent-text (tip "main")))]
      (is (contains? (get-in (edn/read-string (ent-text (tip "main") objects-text))
                             [:schema :objects])
                     :geschaeftspartner))
      (is (= ["rechnungen-nachspielen"] (mapv :name (:automations form))))))

  (testing "a branch that leaves it out removes it there, and on main when it lands"
    (is (ok? (api/post! :branches {:parent "main" :name "riders-b"})))
    (project! "riders-b")
    (is (= (:automations (edn/read-string (ent-text (tip "riders-b"))))
           (:automations (edn/read-string (ent-text (tip "main")))))
        "the fork starts from main's text, rider included")
    (push! "riders-b" without-rider)
    (land! "riders-b")
    (is (not (contains? (names-on "main") "rechnungen-nachspielen")))
    (is (nil? (:automations (edn/read-string (ent-text (tip "main"))))))))

(deftest three-way-per-rider
  (let [a {:name "a" :on :x} a2 {:name "a" :on :y} a3 {:name "a" :on :z}
        d  #(riders/declared {:automations %})]
    (testing "unchanged on the source keeps the target's"
      (is (= a2 (get-in (riders/three-way (d [a]) (d [a]) (d [a2])) [:merged :automations "a"]))))
    (testing "changed on the source only wins"
      (is (= a2 (get-in (riders/three-way (d [a]) (d [a2]) (d [a])) [:merged :automations "a"]))))
    (testing "removed on the source only goes"
      (is (nil? (get-in (riders/three-way (d [a]) (d []) (d [a])) [:merged :automations "a"]))))
    (testing "added on the target only stays"
      (is (= a (get-in (riders/three-way (d []) (d []) (d [a])) [:merged :automations "a"]))))
    (testing "changed differently: the target's stays, named as a conflict"
      (let [r (riders/three-way (d [a]) (d [a2]) (d [a3]))]
        (is (= a3 (get-in r [:merged :automations "a"])))
        (is (= [[:automations "a"]] (:conflicts r)))))
    (testing "a conflict resolved by :resolve is no conflict"
      (let [r (riders/three-way (d [a]) (d [a2]) (d [a3]) {[:automations "a"] :source})]
        (is (= a2 (get-in r [:merged :automations "a"])))
        (is (empty? (:conflicts r)))))))

(deftest an-automation-is-saved-by-a-user-who-becomes-its-owner
  (testing "the system and anonymous mode own nothing"
    (let [e (try (autos/upsert! (ctx/system-store) "main"
                                      {:name "ohne-besitzer" :on :rechnung :effect {:notify "x"}})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :automation/no-owner (:rule (ex-data e))))
      (is (not (contains? (names-on "main") "ohne-besitzer"))))))
