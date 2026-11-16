(ns ^:unit apps.app-with-properties-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.apps.http.view :as view]))

(def ^:private with-props #'view/app-with-properties)
(def ^:private resolve-refs #'view/resolve-var-refs)

(def ^:private app
  {:layout
   {:kind :rows
    :object-types [:luftfahrzeug]
    :vars {:jahr {:type :number :default-agg [:max :jahr :flug]
                 :external-id :jahr :in-url :always}
           :rows {:source :luftfahrzeug
                  :with {:blockstunden {:over :flug-luftfahrzeug :agg :sum :of :block-min
                                   :where [:= :jahr {:val :jahr}]}
                         :fluege {:over :flug-luftfahrzeug :agg :count}}}}}})

(deftest lowers-to-the-same-ast-as-a-declared-rollup
  (testing "one rollup lowering, not a second one for apps"
    (let [pairs (get (with-props app {:jahr 2026}) :luftfahrzeug)
          by    (into {} pairs)]
      (is (= [:connected-sum :flug-luftfahrzeug :block-min [:= :jahr 2026]]
             (:blockstunden by))
          "pivotTo(link).where(jahr).aggregate(sum) — the filter is IN the pivot")
      (is (= [:connected-count :flug-luftfahrzeug] (:fluege by)))
      (is (= (sugar/rollup->expr {:over :flug-luftfahrzeug :agg :sum :of :block-min
                                :where [:= :jahr 2026]})
             (:blockstunden by))
          "identical to what the .ent door produces for the same spec"))))

(deftest an-unresolved-ref-skips-the-property
  (testing "a filter with a hole would compute a confidently wrong number, so it is not computed"
    (let [by (into {} (get (with-props app {}) :luftfahrzeug))]
      (is (nil? (:blockstunden by)))
      (is (some? (:fluege by)) "the unfiltered sibling still computes"))))

(deftest only-source-names-a-type
  (testing ":from names another VAR, never an object type — it must not become a read scope"
    (is (empty? (with-props {:layout {:vars {:x {:from :rows :with {:n {:over :l :agg :count}}}}}}
                  {})))))

(deftest two-vars-over-one-type-merge
  (let [two (assoc-in app [:layout :vars :rows2]
                      {:source :luftfahrzeug :with {:mx {:over :flug-luftfahrzeug
                                                         :agg :max :of :block-min}}})]
    (is (= #{:blockstunden :fluege :mx}
           (set (map first (get (with-props two {:jahr 2026}) :luftfahrzeug)))))))

(deftest refs-resolve-anywhere-in-the-predicate
  (is (= [:and [:= :jahr 2026] [:> :block-min 0]]
         (resolve-refs [:and [:= :jahr {:val :jahr}] [:> :block-min 0]] {:jahr 2026})))
  (is (= [:= :jahr 2026] (resolve-refs [:= :jahr {:var :jahr}] {:jahr 2026})))
  (is (= :apps.app-with-properties-test/never
         (let [r (resolve-refs [:= :jahr {:val :fehlt}] {:jahr 2026})]
           (if (keyword? r) :apps.app-with-properties-test/never r)))
      "a missing value yields the sentinel, not a nil-filled predicate"))

(deftest the-grammar-accepts-with-and-rejects-a-typo
  (let [ok  {:name :flotten-blockstunden
             :layout {:kind :rows
                      :vars {:jahr {:type :number :default 2026}
                             :rows {:source :luftfahrzeug
                                    :with {:s {:over :flug-luftfahrzeug
                                               :agg :sum :of :block-min
                                               :where [:= :jahr {:val :jahr}]}}}}
                      :items [{:widget :object-table :rows {:val :rows}}]}}
        bad (assoc-in ok [:layout :vars :rows :with :s :aggg] :sum)]
    (is (= :accepted (try (ent/parse-enta ok "t") :accepted
                          (catch Exception e (.getMessage e)))))
    (let [msg (try (ent/parse-enta bad "t") nil (catch Exception e (.getMessage e)))]
      (is (string? msg) "a misspelled key inside :with is refused")
      (is (re-find #"aggg" msg) "and the message names it"))))
