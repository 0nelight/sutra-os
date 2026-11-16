(ns sutra-ui.apps.atlas.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.atlas.events :as ev]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.machine-testing :refer [calls requests run step]]
            [sutra-ui.state.shell :as shell]))

(deftest an-annotation-changes-at-once-and-is-sent
  (let [[s :as r] (step {} [:kompass.annotation/tag "dataset/1" "kunde"])
        [[f k body]] (calls r)]
    (is (= #{"kunde"} (get-in s [:annotations "dataset/1" :tags])))
    (is (= [api/post! :kompass-annotations] [f k]))
    (is (= ["kunde"] (:tags body)))
    (is (= "dataset/1" (:ref body))))
  (testing "a failure is shown"
    (let [[{:keys [err]}] (requests (step {} [:kompass.annotation/tag "r" "a"]))]
      (is (= [[:invoke [shell/fail! "annotation" "kaputt"]]]
             (remove nil? (second (step {} (conj err "kaputt"))))))))
  (testing "a second toggle takes the tag off"
    (let [[s] (step {:annotations {"r" (assoc ev/blank-annotation :tags #{"a"})}}
                    [:kompass.annotation/tag "r" "a"])]
      (is (= #{} (get-in s [:annotations "r" :tags]))))))

(deftest annotations-arrive-with-string-refs-and-tag-sets
  (let [[{:keys [ok]}] (requests (step {} [:kompass.annotations/refresh]))
        s (run {} (conj ok {:annotations {:einkauf     {:tags ["a"]}
                                          "dataset/7" {:tags ["a" "b"]}}}))]
    (is (= #{"a"} (get-in s [:annotations "einkauf" :tags])))
    (is (= #{"a" "b"} (get-in s [:annotations "dataset/7" :tags])))))

(deftest a-folder-change-refreshes-the-tree
  (let [r (step {} [:kompass.folder/create "Neu" "p1" :einkauf])
        [{:keys [ok]}] (requests r)]
    (is (= [[api/post! :kompass-folders {:name "Neu" :parent-rid "p1" :project "einkauf"}]]
           (calls r)))
    (is (= [[:invoke [ks/refresh-kompass-nodes!]]]
           (remove nil? (second (step {} (conj ok {}))))))))

(deftest entering-a-project-selects-it
  (let [[s fx] (step {:folder "x"} [:kompass.project/enter :einkauf nil])]
    (is (= "einkauf" (:project s)))
    (is (nil? (:folder s)))
    (is (= [[:invoke [ks/select-project! :einkauf]]] fx))))

(deftest a-confirmed-delete-runs-each-event
  (let [[_ [[k {:keys [ok]}]]] (step {} [:kompass/confirm {:title "?"} [[:a] [:b]]])]
    (is (= :confirm k))
    (is (= [[:dispatch [:a]] [:dispatch [:b]]] (second (step {} ok))))))

(deftest sharing-shows-the-answer-or-why-not
  (let [[s :as r] (step {:share {"f" {:view {}}}} [:kompass.share/load "f"])
        [{:keys [ok err]}] (requests r)]
    (is (nil? (get-in s [:share "f"])))
    (is (= [[api/get! :kompass-file-grants {:ref "f"}]] (calls r)))
    (is (= {:view {:grants []}} (get-in (run s (conj ok {:grants []})) [:share "f"])))
    (is (= {:refused "nein"} (get-in (run s (conj err "nein")) [:share "f"]))))
  (is (= [[api/post! :kompass-share-link {:ref "f" :on true :role :viewer}]]
         (calls (step {} [:kompass.share/link "f" true]))))
  (is (= [[api/post! :kompass-share-link {:ref "f" :on false}]]
         (calls (step {} [:kompass.share/link "f" false])))))

(deftest a-failed-upload-keeps-the-dialog-open-with-the-reason
  (let [db             {:dialog {:kind :upload}}
        [s :as r]      (step db [:kompass.upload/start :p nil []])
        [{:keys [err]}] (requests r)]
    (is (= {:busy? true :problem nil} (:upload s)))
    (let [s (run s (conj err "zu gross"))]
      (is (= {:busy? false :problem "zu gross"} (:upload s)))
      (is (= {:kind :upload} (:dialog s)))))
  (is (nil? (:dialog (first (step {:dialog {:kind :upload}} [:kompass.dialog/close nil]))))))
