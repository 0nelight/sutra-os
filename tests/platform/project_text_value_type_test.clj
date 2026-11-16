(ns ^:api platform.project-text-value-type-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.authoring.projections :as projections]
            [sutra.store.seat :as seat]
            [sutra.actions.staging :as stg]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private schema
  {:schema {:projects    {:kasse {:description "Kasse"}}
            :value-types {:betrag {:type :decimal
                                   :format {:kind :currency :currency "EUR" :decimals 2}}}
            :objects     {:beleg {:primary-key :beleg-nr
                                  :attributes [[:beleg-nr :string] [:netto :decimal]]
                                  :match-keys [:beleg-nr] :project :kasse
                                  :plural "Belege"}}}})

(defn- c [store b]
  {:store (world/at store b) :schema (seat/branch-schema (world/at store b)) :branch b})

(defn- with-attrs
  [store b attrs]
  (let [p  (projections/pull (c store b) "ontologie" nil false)
        fs (update-keys (:files p) name)]
    (assoc p :files
           (into {} (for [[k t] fs
                          :let [s (edn/read-string t)]]
                      [k (pr-str (cond-> s
                                   (get-in s [:schema :objects :beleg])
                                   (update-in [:schema :objects :beleg :attributes] into
                                              attrs)))])))))

(defn- staged-attr [store b nm]
  (some #(when (= nm (first %)) %)
        (some #(when (= :beleg (:type-name %)) (:attributes %))
              (stg/list-staged-enriched (world/at store b)))))

(deftest a-project-text-reads-the-value-types-of-ontology-ent
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [admin (ctx/system-store "main")
        b     (str "vt-" (random-uuid))]
    (world/fork! admin "main" b {:kind :feature})
    (let [r (projections/push! (c admin b) "ontologie"
                               (with-attrs admin b
                                 [[:brutto :betrag {:name "Brutto"}]
                                  [:doppelt :betrag {:name "Doppelt"
                                                     :derived {:language :clojure
                                                               :expression [:* [:get :netto] 2]
                                                               :inputs [:netto]}}]]))]
      (testing "read back, the branch holds every element the push answered for"
        (is (pos? (:updated r 0)) (pr-str (dissoc r :base)))
        (is (empty? (:not-stored r)) (pr-str (:not-stored r)))))
    (testing "a stored attribute takes the value type's base and keeps the reference"
      (let [[_ t opts :as a] (staged-attr admin b :brutto)]
        (is (= :decimal t) (pr-str a))
        (is (= :betrag (:value-type opts)) (pr-str a))))
    (testing "so does a derived one"
      (let [[_ t opts :as a] (staged-attr admin b :doppelt)]
        (is (= :decimal t) (pr-str a))
        (is (= :betrag (:value-type opts)) (pr-str a))))))
