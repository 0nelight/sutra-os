(ns ^:unit apps.schema-reach-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.schema-reach :as reach]))

(def ^:private schema
  {:applications {:view {:id "r1" :title "A" :layout {:kind :rows}}
                  :leer {:id "r2" :title "B"}}})

(defn- catalog [layout]
  (reach/catalog (assoc-in schema [:applications :view :layout] layout)))

(defn- hydrated [layout]
  (let [c (catalog layout)]
    (update-in c [:applications :view]
               #(reach/with-bodies :applications % {:layout layout :title "ignored"}))))

(deftest a-catalog-entry-names-the-version-of-what-it-left-out
  (let [e (get-in (catalog {:kind :rows}) [:applications :view])]
    (is (not (contains? e :layout)))
    (is (reach/elsewhere? e :layout))
    (is (some? (reach/body-version-key e))))
  (is (not (contains? (get-in (catalog {:kind :rows}) [:applications :leer])
                      reach/body-version-key))
      "nothing left out, nothing to version")
  (is (not= (get-in (catalog {:kind :rows}) [:applications :view reach/body-version-key])
            (get-in (catalog {:kind :tabs}) [:applications :view reach/body-version-key]))
      "a change only to the body still moves the catalog"))

(deftest a-whole-entry-holds-only-its-body
  (let [e (get-in (hydrated {:kind :rows}) [:applications :view])]
    (is (= {:kind :rows} (:layout e)))
    (is (= "A" (:title e)) "only bodies come from the fetched spec")
    (is (not (contains? e reach/elsewhere-key)))
    (is (not (contains? e reach/body-version-key)) "a form that saves the entry saves no mark")))

(deftest a-catalog-arriving-again-keeps-the-bodies-it-did-not-change
  (testing "same version: the entry stays whole and can be kept again"
    (let [once  (reach/keep-bodies (hydrated {:kind :rows}) (catalog {:kind :rows}))
          twice (reach/keep-bodies once (catalog {:kind :rows}))]
      (is (= {:kind :rows} (get-in once [:applications :view :layout])))
      (is (= {:kind :rows} (get-in twice [:applications :view :layout])))
      (is (not (reach/elsewhere? (get-in twice [:applications :view]) :layout)))))
  (testing "another version: the stale body goes and is fetched anew"
    (let [e (get-in (reach/keep-bodies (hydrated {:kind :rows}) (catalog {:kind :tabs}))
                    [:applications :view])]
      (is (not (contains? e :layout)))
      (is (reach/elsewhere? e :layout))))
  (testing "a change beside the body comes through, the body stays"
    (let [e (get-in (reach/keep-bodies (hydrated {:kind :rows})
                                       (assoc-in (catalog {:kind :rows})
                                                 [:applications :view :title] "Neu"))
                    [:applications :view])]
      (is (= "Neu" (:title e)))
      (is (= {:kind :rows} (:layout e)))))
  (testing "an entry never fetched stays a catalog entry"
    (is (= (catalog {:kind :rows})
           (reach/keep-bodies (catalog {:kind :rows}) (catalog {:kind :rows}))))))
