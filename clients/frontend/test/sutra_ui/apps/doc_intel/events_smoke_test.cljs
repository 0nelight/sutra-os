(ns sutra-ui.apps.doc-intel.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.doc-intel.events :as ev]
            [sutra-ui.apps.doc-intel.model :as m]
            [sutra-ui.storage :as storage]
            [sutra-ui.state.machine-testing :refer [calls requests run step]]))

(deftest the-first-arrival-builds-the-state-and-asks-for-sources
  (let [[s :as r] (step nil [:doc-intel/enter [{:id 1 :strategy "ocr"}] {:kind "k" :name "n"}])]
    (is (= :loading (:sources s)))
    (is (= [{:id 1 :strategy "ocr"}] (:strategies s)))
    (is (= [[api/get! :doc-intel-sources]] (calls r))))
  (is (= [] (second (step {:sources []} [:doc-intel/enter nil nil])))))

(deftest sources-pick-the-stored-one-and-store-the-defaults
  (let [a {:kind "media-set" :name "a"} b {:kind "media-set" :name "b"}
        [s fx] (step (assoc (m/initial nil) :stored-source b)
                     [:doc-intel/sources {:sources [a b] :models ["m1"]}])]
    (is (= "m1" (:judge s)))
    (is (= 3 (count (:strategies s))))
    (is (= [:invoke [storage/set-json! (ev/stored-keys :strategies) (:strategies s)]]
           (first fx)))
    (is (= [:dispatch [:doc-intel/source b]] (last fx)))))

(deftest a-source-loads-its-items-once
  (let [src {:kind "media-set" :name "a"}
        [s :as r] (step (m/initial nil) [:doc-intel/source src])]
    (is (= :loading (get-in s [:items "media-set:a"])))
    (is (= [[api/get! :doc-intel-items src]] (calls r)))
    (is (= [] (calls (step (assoc-in s [:items "media-set:a"] []) [:doc-intel/source src]))))
    (let [[{:keys [ok err]}] (requests r)]
      (is (= [{:address "x"}] (get-in (run s (conj ok {:items [{:address "x"}]}))
                                      [:items "media-set:a"])))
      (is (= {:error "weg"} (get-in (run s (conj err "weg")) [:items "media-set:a"]))))))

(deftest failed-sources-show-why
  (let [[s :as r] (step nil [:doc-intel/enter nil nil])
        [{:keys [err]}] (requests r)]
    (is (= {:error "weg"} (:sources (run s (conj err "weg")))))
    (is (= :loading (:sources (run s (conj err nil)))))))

(deftest a-run-is-asked-once-and-lands-where-it-was-asked
  (let [st (-> (m/initial [(m/new-strategy 1 "raw-text" nil)])
               (assoc :item {:address "x"} :source {:kind "media-set" :name "a"}))
        [s :as r] (step st [:doc-intel/extract 1])
        k [:results (m/result-key st (m/strategy-of st 1))]]
    (is (= :running (:status (get-in s k))))
    (is (= [[api/post! :doc-intel-extract
             {:source {:kind "media-set" :name "a"} :address "x" :page 0
              :strategy {:strategy "raw-text"}}]]
           (calls r)))
    (is (= [] (calls (step s [:doc-intel/extract 1]))))
    (testing "an answer finishes it, a failure says why, an abort leaves it not run"
      (let [[{:keys [ok err]}] (requests r)]
        (is (= {:status :done :row {}} (get-in (run s (conj ok {})) k)))
        (is (= {:status :error :error "weg"} (get-in (run s (conj err "weg")) k)))
        (is (nil? (get-in (run s (conj err nil)) k)))))))

(deftest an-edited-deployment-drops-its-last-result
  (let [[s] (step {:deployments {1 {:name "a" :repository "r" :result {:status :done}}}}
                  [:doc-intel/deployment 1 :name "b"])]
    (is (= {:name "b" :repository "r" :result nil} (get-in s [:deployments 1])))))
