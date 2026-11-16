(ns sutra-ui.apps.notebook.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.notebook.events]
            [sutra-ui.apps.notebook.state :as st]
            [sutra-ui.state.machine-testing :refer [calls requests run step]]))

(defn- dispatched [[_ fx]] (keep (fn [[k a]] (when (= :dispatch k) a)) fx))

(def ^:private d {:id 7 :name "Bericht" :format :html :body "<p>a</p>" :version 3})

(def ^:private open {:open-doc d :saved-doc d})

(deftest loading-the-list-asks-the-server-and-keeps-the-answer
  (is (= [[api/documents]] (calls (step {} [:notebook.docs/refresh]))))
  (let [[s :as r] (step {} [:notebook.docs/loaded
                            {:documents [{:id 1 :links [{:object-type "kunde" :id 4}]}]}])]
    (is (= [1] (map :id (:docs s))))
    (testing "a link is looked up once, as its object's row"
      (is (= 1 (count (calls r)))))))

(deftest an-edit-is-dirty-and-saves-after-a-pause
  (let [[s fx] (step open [:notebook/edit {:name "Neu"}])]
    (is (= "Neu" (:name (:open-doc s))))
    (is (= :dirty (:save-state s)))
    (is (= [:after {:key :notebook/autosave :ms st/autosave-ms :event [:notebook/save]}]
           (last fx)))))

(deftest a-save-sends-what-changed-with-the-loaded-version
  (let [db       (assoc-in open [:open-doc :name] "Neu")
        [s1 fx1] (step db [:notebook/save nil [[:then]]])
        [s2 :as r] (step s1 [:notebook.save/next])
        [[_ id body]] (calls r)]
    (is (= [[:cancel :notebook/autosave] [:dispatch [:notebook.save/next]]] fx1))
    (is (true? (:saving? s2)))
    (is (= :saving (:save-state s2)))
    (is (= "7" id))
    (is (= {:name "Neu" :format :html :body "<p>a</p>" :expect-version 3} body))
    (testing "a second save waits for the first"
      (let [[s3 fx3] (step s2 [:notebook/save nil [[:later]]])]
        (is (= 1 (count (:save-queue s3))))
        (is (= [[:cancel :notebook/autosave] nil] fx3))))
    (testing "the answer is the saved state; what waited runs, the next save starts"
      (let [[{:keys [ok err]}] (requests r)
            [s4 :as r4] (step s2 (conj ok {:document {:id 7 :version 4 :name "Neu"}}))
            s4'         (first (step s4 (first (dispatched r4))))]
        (is (= :saved (:save-state s4')))
        (is (= 4 (:version (:open-doc s4'))))
        (is (false? (:saving? s4)))
        (is (= [:notebook.save/done :notebook.save/ended :then :notebook.save/next]
               (map first (dispatched r4))))
        (testing "and when the save was broken off"
          (is (= [[:notebook.save/ended] [:then] [:notebook.save/next]]
                 (dispatched (step s2 (conj err nil)))))
          (is (= :error (:save-state (first (step s2 [:notebook.save/ended]))))))))))

(deftest nothing-to-save-runs-what-waits-at-once
  (let [[s :as r] (step (assoc open :save-queue [{:then [[:open]]}]) [:notebook.save/next])]
    (is (empty? (calls r)))
    (is (= [[:open] [:notebook.save/next]] (dispatched r)))
    (is (= [] (:save-queue s)))))

(deftest a-failed-save-is-shown-a-live-conflict-is-not
  (let [db              (assoc (assoc-in open [:open-doc :name] "Neu") :save-queue [{}])
        [s :as r]       (step db [:notebook.save/next])
        [{:keys [err]}] (requests r)
        s               (run s (conj err "kaputt"))]
    (is (= :error (:save-state s)))
    (is (= "kaputt" (:err s)))
    (is (false? (:saving? s))))
  (let [[s] (step {:session {:id 7}} [:notebook.save/done {} {:conflict "409"}])]
    (is (= :dirty (:save-state s)))
    (is (nil? (:err s)))))

(deftest opening-saves-first-then-shows-the-document
  (is (= [[:notebook/save nil [[:notebook.doc/fetch 9]]]]
         (dispatched (step open [:notebook/open 9]))))
  (let [[s :as r] (step {:doc 9} [:notebook.doc/show (assoc d :id 9 :format :markdown)])]
    (is (= 9 (:id (:open-doc s))))
    (is (= :saved (:save-state s)))
    (testing "markdown opens as rich text, rendered once"
      (is (= [[api/post! :document-export {:id "9"} {:targets ["html"]}]] (calls r))))
    (is (some #{[:notebook.live/start]} (dispatched r)))))

(deftest a-live-document-opens-a-session-once
  (let [[s fx] (step open [:notebook.live/start])]
    (is (= {:id 7} (:session s)))
    (is (= [[:notebook.live/open 7]] (remove nil? fx))))
  (is (= [] (second (step (assoc open :session {:id 7}) [:notebook.live/start]))))
  (testing "a read-only page holds none"
    (is (nil? (:session (first (step (assoc open :read-only? true :session {:id 7})
                                     [:notebook.live/start])))))))

(deftest a-confirmed-delete-deletes-and-goes-home
  (let [[_ [[k {:keys [ok]}]]] (step open [:notebook/delete])]
    (is (= :confirm k))
    (is (= [:notebook.doc/delete "7"] ok))
    (is (= [[api/delete! :document-of {:id "7"}]] (calls (step open ok)))))
  (let [[s :as r] (step (assoc open :doc 7) [:notebook.doc/deleted nil])]
    (is (nil? (:open-doc s)))
    (is (nil? (:doc s)))
    (is (= [[:notebook.docs/refresh]] (dispatched r)))))

(deftest export-is-busy-until-the-file-is-there
  (let [[s :as r] (step open [:notebook/export :pdf])]
    (is (true? (:busy s)))
    (is (= [[:notebook/save nil [[:notebook.export/fetch :pdf d]]]] (dispatched r))))
  (let [[s :as r] (step {:busy true} [:notebook.export/fetch :pdf d])
        [{:keys [call ok err]}] (requests r)
        [s' :as r'] (step s (conj ok {:pdf_b64 ""}))]
    (is (= [api/post! :document-export {:id "7"} {:targets ["pdf"]}] call))
    (is (false? (:busy s')))
    (is (= [:rf/invoke] (map first (dispatched r'))))
    (is (= {:busy false :err "nein"} (run s (conj err "nein"))))))

(deftest widget-rows-load-once-per-type
  (let [[s :as r] (step {} [:notebook.widget/rows "kunde"])
        [{:keys [ok err]}] (requests r)]
    (is (= :loading (get-in s [:pick-rows "kunde"])))
    (is (= 1 (count (calls r))))
    (is (= [] (second (step s [:notebook.widget/rows "kunde"]))))
    (is (= [{:id 1}] (get-in (run s (conj ok {:rows [{:id 1}]})) [:pick-rows "kunde"])))
    (is (= [] (get-in (run s (conj err "x")) [:pick-rows "kunde"])))))
