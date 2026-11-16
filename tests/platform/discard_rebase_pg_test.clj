(ns ^:api platform.discard-rebase-pg-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [sutra.catalog.grants :as grants]
            [sutra.actions.staged :as staged]
            [support.in-project :refer [install-in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  {:objects {:user {:attributes [[:user-id :string] [:rolle :string]]
                    :primary-key :user-id :plural "Users"}}
   :actions {:benutzer-anlegen
             {:params {:id {:name "Id" :type :string :required true}
                       :rolle {:name "Rolle" :type :string :required true}}
              :rules  [{:op :create :object-type :user :id :id
                        :values {:user-id [:get :id] :rolle [:get :rolle]}}]
              :edits  [{:object-type :user}]}}})

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- als-admin [f]
  (binding [tx/*auth-token* (anmelden grants/admin-username pr/default-admin-password)] (f)))

(defn- mit-welt [f]
  (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
  (assert (ok? (api/post! :staging)))
  (f))

(use-fixtures :once pod/with-pod-auth als-admin mit-welt)

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- benutzer! [name rolle]
  (let [id (str (random-uuid))]
    (assert (ok? (tun! :benutzer-anlegen {:id id :rolle rolle})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    {:token (anmelden name name)}))

(defn- typ [n] {:objects {n {:primary-key :nr :attributes [[:nr :string]] :plural (name n)}}})

(defn- stage! [n] (assert (ok? (install-in-project {:schema (typ n)}))))

(defn- entwurf [] (get-in (api/get! :staging) [:body :staged]))

(defn- typen [] (set (map :object-type-name (vals (:object-types (:body (api/get! :schema)))))))

(defn- verwerfen!
  [n]
  (let [row   (some #(when (= n (some-> % :args :name keyword)) %) (entwurf))
        forms (atom [])
        land  @#'staged/land-discard!]
    (assert row (pr-str (entwurf)))
    (with-redefs [staged/land-discard! (fn [store staging keep p]
                                         (swap! forms conj (:form p))
                                         (land store staging keep p))]
      (is (ok? (api/delete! :staged-by-seq {:seq (:seq row)}))))
    @forms))

(deftest a-discard-after-main-moved-lays-the-draft-onto-main
  (api/delete! :staging)
  (testing "main moved by data only"
    (stage! :rb-a)
    (stage! :rb-b)
    (benutzer! (str "kollege-" (random-uuid)) "viewer")
    (is (= [:rebase] (verwerfen! :rb-a)) "laid onto main, not run anew")
    (is (= [:rb-b] (keep #(some-> % :args :name keyword) (entwurf))))
    (is (ok? (api/post! :staging)))
    (is (contains? (typen) :rb-b))
    (is (not (contains? (typen) :rb-a))))
  (testing "main moved by another resource"
    (stage! :rb-c)
    (stage! :rb-d)
    (let [kollege (benutzer! (str "kollege-" (random-uuid)) "ontology-editor")]
      (tx/with-token (:token kollege)
        (is (ok? (install-in-project {:schema (typ :rb-e)})))
        (is (ok? (api/post! :staging)))))
    (is (= [:rebase] (verwerfen! :rb-c)) "laid onto main")
    (is (= [:rb-d] (keep #(some-> % :args :name keyword) (entwurf))))
    (is (ok? (api/post! :staging)))
    (is (every? (typen) [:rb-d :rb-e]))
    (is (not (contains? (typen) :rb-c)))))
