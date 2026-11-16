(ns ^:unit grammar.versions-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.entities :as ents]
            [sutra.types.versions :as v]))

(def ^:private agent-v0
  {:agent-name :clerk :project "ri.p" :status :active :model "m1" :tools [{:kind :ontology}]})

(deftest each-section-declares-how-it-is-versioned
  (is (= :release (v/mode :functions)))
  (is (= :release (v/mode :templates)))
  (is (= :publish (v/mode :applications)))
  (is (= :publish (v/mode :agents)))
  (is (nil? (v/mode :object-types)))
  (testing "a publishing section's grammar carries its history"
    (doseq [s v/publish-sections]
      (is (every? (set (keys (get ents/fields s))) [:releases :published :release-notes])
          (str s)))))

(deftest publishing-cuts-an-immutable-release-of-the-head
  (let [p1 (v/publish agent-v0 {:note "first" :by "ana" :at "t1"})]
    (is (= "1" (:published p1)))
    (is (= {:model "m1" :tools [{:kind :ontology}]} (get-in p1 [:releases "1"]))
        "a release holds content, never identity, place or lifecycle")
    (is (= {:note "first" :by "ana" :at "t1"} (get-in p1 [:release-notes "1"])))
    (testing "publishing an unchanged head names the newest release again"
      (is (= ["1"] (v/versions (v/publish p1 {})))))
    (testing "a changed head becomes the next release"
      (let [p2 (v/publish (assoc p1 :model "m2") {})]
        (is (= ["1" "2"] (v/versions p2)))
        (is (= "2" (:published p2)))
        (testing "and an older one can be published again"
          (is (= "m1" (:model (v/effective (v/publish p2 {:version "1"}))))))))
    (testing "an unknown version is refused, not invented"
      (is (thrown? clojure.lang.ExceptionInfo (v/publish p1 {:version "7"}))))))

(deftest consumers-see-the-published-release-builders-the-head
  (let [draft (-> agent-v0 (v/publish {}) (assoc :model "m2"))]
    (is (= "m1" (:model (v/effective draft))))
    (is (= "m2" (:model (v/as-version draft "draft"))))
    (is (= "m1" (:model (v/as-version draft "1"))))
    (is (= "m2" (:model (v/effective (assoc agent-v0 :model "m2")))) "nothing published: the head")
    (is (= "ri.p" (:project (v/effective draft))) "identity stays the head's")
    (is (= "m1" (get-in (v/published-view {:agents {"r" draft} :object-types {"o" {:x 1}}})
                        [:agents "r" :model])))
    (is (= {:published "1" :draft? true}
           (select-keys (v/listing draft) [:published :draft?])))))

(deftest a-save-keeps-the-history-and-the-text-never-shows-it
  (let [live (v/publish agent-v0 {:note "n"})
        want (v/carry {:agent-name :clerk :model "m3"} live)]
    (is (= (select-keys live [:releases :release-notes :published])
           (select-keys want [:releases :release-notes :published])))
    (is (= {:agent-name :clerk :model "m3"} (v/authored :agents want)))
    (is (= live (v/authored :functions live)) "a :release section's text names its releases")))

(deftest on-a-branch-consumers-see-what-the-branch-changed
  (let [released (v/publish {:application-name :brett :title "alt"} {})
        draft    (assoc released :title "main-entwurf")
        rid      "ri.app"
        schema   (fn [spec] {:applications {rid spec}})
        title    (fn [view] (get-in view [:applications rid :title]))]
    (testing "unchanged on the branch: the published release, as on main"
      (is (= "alt" (title (v/branch-view (schema draft) (schema draft))))
          "main's unpublished draft stays off the branch"))
    (testing "saved on the branch: its head, though :published still names the old release"
      (let [saved (assoc released :title "zweig")]
        (is (= "zweig" (title (v/branch-view (schema saved) (schema released)))))))
    (testing "new on the branch: its head"
      (is (= "kopf" (title (v/branch-view (schema (assoc released :title "kopf"))
                                          {:applications {}})))))
    (testing "a re-pointed release changes no content, so the pointer decides"
      (let [two   (-> released (assoc :title "neu") (v/publish {}))
            back  (assoc two :published "1")]
        (is (= "alt" (title (v/branch-view (schema back) (schema two)))))))
    (testing "a section that publishes nothing is left alone"
      (let [s {:object-types {"t" {:object-type-name :x}}}]
        (is (= s (v/branch-view s s)))))))
