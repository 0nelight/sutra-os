(ns ^:unit platform.package-release-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.product :as product]))

(def ^:private v1
  {:objects {:vertrag {:description "ein Vertrag" :attributes [[:nr :string]] :plural "Verträge"}
             :frist   {:description "eine Frist"}}
   :links   [[:frist-vertrag :frist :vertrag :many-to-one {:description "alt"}]]})

(def ^:private v2
  {:objects {:vertrag {:description "ein Vertrag, neu beschrieben" :attributes [[:nr :string]]
                       :plural "Verträge"}
             :frist   {:description "eine Frist"}
             :kuendigung {:description "neu im Release"}}
   :links   [[:frist-vertrag :frist :vertrag :many-to-one {:description "neu"}]]})

(deftest a-release-merges-onto-what-the-instance-made-of-the-last
  (testing "only the release changed: the release wins, a new resource comes in"
    (let [{:keys [body conflicts]} (product/release-upgrade v1 v2 v1 nil)]
      (is (empty? conflicts))
      (is (= "ein Vertrag, neu beschrieben" (get-in body [:objects :vertrag :description])))
      (is (contains? (:objects body) :kuendigung))
      (is (= "neu" (-> (:links body) first last :description))
          "a link tuple moves with the release")))
  (testing "only the instance changed a field: it keeps it"
    (let [mine (assoc-in v1 [:objects :frist :description] "meine Frist")
          {:keys [body conflicts]} (product/release-upgrade v1 v2 mine nil)]
      (is (empty? conflicts))
      (is (= "meine Frist" (get-in body [:objects :frist :description])))))
  (testing "both changed one field: a conflict, until resolved"
    (let [mine (assoc-in v1 [:objects :vertrag :description] "mein Vertrag")]
      (is (= [{:node [:objects "vertrag"] :field :description}]
             (:conflicts (product/release-upgrade v1 v2 mine nil))))
      (is (= "ein Vertrag, neu beschrieben"
             (get-in (:body (product/release-upgrade v1 v2 mine :mine))
                     [:objects :vertrag :description])))
      (is (= "mein Vertrag"
             (get-in (:body (product/release-upgrade v1 v2 mine :theirs))
                     [:objects :vertrag :description])))))
  (testing "what the instance holds beyond both releases is not touched"
    (let [mine (assoc-in v1 [:objects :eigenes] {:description "nur hier"})]
      (is (not (contains? (:objects (:body (product/release-upgrade v1 v2 mine nil))) :eigenes)))))
  (testing "no recorded install: a reinstall, the release wins everywhere"
    (let [mine (assoc-in v1 [:objects :vertrag :description] "mein Vertrag")
          {:keys [body conflicts]} (product/release-upgrade nil v2 mine nil)]
      (is (empty? conflicts))
      (is (= "ein Vertrag, neu beschrieben" (get-in body [:objects :vertrag :description]))))))

(deftest a-listing-names-versions
  (let [offers {:demo {:name "Demo" :version "2.0.0" :schema (constantly {})}}
        [row] (product/listing offers (constantly []) (constantly false) {:demo "1.0.0"})]
    (is (= "2.0.0" (:version row)))
    (is (= "1.0.0" (:installed-version row)))
    (is (:upgrade? row))))
