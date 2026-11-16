(ns ^:api platform.flaw-push-guard-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.branching.push]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.governance.protection :as gov-protection]
            [sutra.types.rid :as rid]
            [sutra.store.seat :as seat]
            [sutra.actions.staging :as stg]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth pod/as-admin)

(def ^:private admin
  {:user "u-admin" :username grants/admin-username
   :operations grants/all :project-operations grants/all :clearances grants/all})

(def ^:private orid rid/ontology-repo-rid)

(defn- branch! [prefix]
  (let [b (str prefix "-" (subs (str (random-uuid)) 0 8))]
    (assert (ok? (api/post! :branches {:parent "main" :name b})))
    b))

(defn- types-on [b]
  (set (map (comp name :object-type-name)
            (vals (:object-types (seat/branch-schema (ctx/at (ctx/store) b)))))))

(defn- publish! [b start-schema t]
  (let [branch-store  (ctx/at (ctx/store nil admin) b)
        live (seat/branch-schema branch-store)]
    (#'sutra.branching.push/publish-gate branch-store start-schema live orid
     {:origin :git-push :origin-ref (str "test " b)}
     {:schema {:projects {:pg {}}
               :objects {t {:project :pg :primary-key :nr :attributes [[:nr :string]]
                            :plural "Objekte"}}}})))

(deftest protected-push
  (let [b     (branch! "push-prot")
        start (seat/branch-schema (ctx/at (ctx/store) b))]
    (gov-protection/set-policy! (ctx/at (ctx/store) b) b {:protected true})
    (let [r (publish! b start :pp-typ)]
      (is (= :failed (:status r)) (pr-str r))
      (is (re-find #"protected" (str (:detail r))) (pr-str r))
      (is (not (contains? (types-on b) "pp-typ")) "nothing published onto the now protected branch")
      (is (empty? (stg/list-staged-enriched (ctx/at (ctx/store) b))) "nothing left staged"))))

(deftest stale-publish-mode
  (let [b     (branch! "push-mode")
        start (seat/branch-schema (ctx/at (ctx/store) b))
        orig  seat/branch-schema]
    (is (not= :stage-only (get-in start [:repositories orid :publish-mode])))
    (with-redefs [seat/branch-schema (fn [store]
                                       (assoc-in (orig store) [:repositories orid :publish-mode]
                                                 :stage-only))]
      (let [r (publish! b start :sm-typ)]
        (is (= :passed (:status r)) (pr-str r))
        (is (re-find #"^staged" (str (:detail r))) (pr-str r))))
    (is (not (contains? (types-on b) "sm-typ"))
        "the repository is :stage-only at the commit: staged, not published")))
