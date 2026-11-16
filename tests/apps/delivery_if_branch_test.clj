(ns ^:unit apps.delivery-if-branch-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.apps.http.delivery]))

(def ^:private needed @#'sutra.apps.http.delivery/needed)
(def ^:private gate-inputs @#'sutra.apps.http.delivery/gate-inputs)

(def ^:private specs
  {:reiter {:default "neu" :external-id :reiter :in-url :always :type :string}
   :offen  {:source :t :where [[:x := 1]]}
   :neu    {:source :t :where [[:x := 2]]}
   :erste-offen {:object-property :x :of :offen}
   :erste-neu   {:object-property :x :of :neu}
   :erste {:expr [:if [:= {:val :reiter} "offen"] {:val :erste-offen} {:val :erste-neu}]}})

(def ^:private visible {:widget :text :text {:val :erste}})

(deftest a-decided-if-reads-only-the-branch-it-takes
  (let [got (needed specs {:t "rid"} {:reiter "neu"} visible)]
    (is (contains? got :neu))
    (is (not (contains? got :offen)) "the closed tab's set is not delivered"))
  (is (contains? (needed specs {:t "rid"} {:reiter "offen"} visible) :offen)))

(deftest an-undecided-if-reads-both
  (let [got (needed specs {:t "rid"} {} visible)]
    (is (every? got [:neu :offen]))))

(deftest the-test-brings-the-page-back
  (is (contains? (gate-inputs specs {}) :reiter)))
