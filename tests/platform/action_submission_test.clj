(ns ^:integration platform.action-submission-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- as-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- with-ontology [f]
  (assert (ok? (api/post! :install {:schema ontologie})))
  (assert (ok? (api/post! :staging)))
  (f))

(use-fixtures :once pod/with-mem-pod-auth as-admin with-ontology)

(defn- runs-of-kind [kind]
  (let [b (:body (api/get! :runs {:kind kind}))]
    (filter #(= (name kind) (name (or (:kind %) ""))) (if (map? b) (:runs b) b))))

(deftest a-submission-is-one-commit-and-its-log-entry
  (let [b  (str "d108-" (subs (str (random-uuid)) 0 8))
        store #(ctx/store b)]
    (api/post! :branches {:parent "main" :name b})
    (try
      (tx/with-branch b
        (let [r  (api/mutate :luftfahrzeug-registrieren {:kennzeichen "D-GAAA" :muster "A320"}
                             {:publish? true})
              id (get-in r [:body :action-id])]
          (testing "a dispatch answers its submission's id and writes no run"
            (is (ok? r))
            (is (some? id))
            (is (empty? (runs-of-kind :action))))
          (testing "the action log entry carries the id"
            (is (= (str id) (:action-id (last (run-log/action-log (store)))))))
          (let [logged (count (run-log/action-log (store)))
                r      (api/mutate :mangel-melden {:mangel-nr "G-1" :luftfahrzeug "D-GAAA"
                                                   :beschreibung "x" :kategorie "Z"}
                                   {:publish? true})
                failed (first (run-log/action-failures (store)))]
            (testing "a refused submission is counted by category and commits nothing else"
              (is (not (ok? r)))
              (is (= "mangel-melden" (:action-name failed)))
              (is (= :invalid-parameter (:category failed)))
              (is (some? (:action-id failed)))
              (is (= logged (count (run-log/action-log (store)))))
              (is (empty? (runs-of-kind :action)))))))
      (finally
        (api/delete! :branch {:name b})))))
