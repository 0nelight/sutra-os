(ns sutra-ui.apps.widget-sets.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.widget-sets.events :as ev]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.machine-testing :refer [calls requests run step]]))

(deftest selecting-a-set-clears-the-last-and-loads-it
  (let [[s :as r] (step {:detail {:name "a"} :problem "x"} [:widget-sets/select "b"])
        [{:keys [ok err]}] (requests r)]
    (is (= "b" (:selected s)))
    (is (nil? (:detail s)))
    (is (nil? (:problem s)))
    (is (= [[api/get! :widget-set {:name "b"}]] (calls r)))
    (is (= {:name "b"} (:detail (run s (conj ok {:widget-set {:name "b"}})))))
    (is (= "weg" (:problem (run s (conj err "weg"))))))
  (is (= [] (calls (step {} [:widget-sets/select nil])))))

(deftest creating-trims-the-name-and-selects-the-new-set
  (let [r (step {:draft {:name " neu " :project "p"}} [:widget-sets/create])
        [{:keys [ok]}] (requests r)]
    (is (= [[api/post! :widget-sets {:name "neu" :project "p" :description nil}]] (calls r)))
    (is (= [[:dispatch [:widget-sets/created "neu" {}]]]
           (remove nil? (second (step {} (conj ok {})))))))
  (let [[s fx] (step {:adding? true :draft {:name "neu"}} [:widget-sets/created "neu"])]
    (is (false? (:adding? s)))
    (is (= [[:invoke [ks/refresh! :widget-sets]] [:dispatch [:widget-sets/select "neu"]]] fx))))

(deftest an-upload-reads-the-file-then-posts-the-typed-version
  (is (= [[ev/read-b64 :file]] (calls (step {} [:widget-sets.release/upload "a" :file]))))
  (let [r (step {:upload-version "1.2.0"} [:widget-sets.release/read "a" "QQ=="])
        [{:keys [ok]}] (requests r)]
    (is (= [[api/post! :widget-set-releases {:name "a"} {:version "1.2.0" :zip-b64 "QQ=="}]]
           (calls r)))
    (testing "an upload clears the problem and reloads the set"
      (is (nil? (:problem (run {:problem "x"} (conj ok {})))))
      (is (= [[api/get! :widget-set {:name "a"}]] (calls (step {} (conj ok {}))))))))

(deftest the-playground-takes-only-the-bundle-it-asked-for
  (let [[s :as r] (step {:playground {:values {:x 1}}} [:widget-sets.playground/load "a" "1"])
        [{:keys [ok]}] (requests r)
        b {:bundle-edn (pr-str {:manifest {:widgets {:w {}}}})}]
    (is (= {:x 1} (get-in s [:playground :values])))
    (is (= :w (get-in (run s (conj ok b)) [:playground :widget])))
    (testing "an answer for another release is dropped"
      (let [s' (assoc-in s [:playground :for] ["a" "0"])]
        (is (= s' (run s' (conj ok b))))))))

(deftest an-emit-merges-its-updates-and-is-logged
  (let [[s] (step {:playground {:values {:a 1}}}
                  [:widget-sets.playground/emitted :changed {:b 2}])]
    (is (= {:a 1 :b 2} (get-in s [:playground :values])))
    (is (= [[:changed {:b 2}]] (get-in s [:playground :log])))))
