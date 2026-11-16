(ns sutra-ui.builder.bottom-panel.events-smoke-test
  (:require [cljs.test :refer [deftest is]]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.builder.bottom-panel.events]
            [sutra-ui.state.machine-testing :refer [step]]
            [sutra-ui.state.sync :as sync]))

(defn- fx-of [kind [_ fx]] (keep (fn [[k a]] (when (= kind k) a)) fx))

(defn- call-of [r] (first (fx-of :call r)))

(deftest a-health-tab-shows-loading-then-only-its-own-checks
  (let [[s :as r] (step {} [:shared.panel/health :dataset :orders true])
        [[_ {:keys [path ask pick]}]] (fx-of :dispatch r)]
    (is (= rd/loading (get-in s [:shared.panel :health [:dataset :orders]])))
    (is (= [:shared.panel :health [:dataset :orders]] path))
    (is (= [api/get! :health-checks] ask))
    (is (= [{:id 1 :node-kind "dataset" :node-key "orders"}]
           (pick {:checks [{:id 1 :node-kind "dataset" :node-key "orders"}
                           {:id 2 :node-kind "dataset" :node-key "lines"}]}))))
  (is (nil? (get-in (first (step {} [:shared.panel/health :dataset :orders false]))
                    [:shared.panel :health]))))

(deftest a-new-check-runs-and-the-list-is-read-after-the-run
  (let [{:keys [ok]} (call-of (step {} [:shared.panel/add-check :dataset :o {:name "n"}]))
        [added] (fx-of :dispatch (step {} (conj ok {:id 7})))]
    (is (= [[:shared.panel/run-check :dataset :o 7]] (fx-of :dispatch (step {} added)))))
  (let [[{:keys [event ms]}] (fx-of :after (step {} [:shared.panel/checks-changed :dataset :o
                                                     150 nil]))]
    (is (= 150 ms))
    (is (= [:shared.panel/health :dataset :o] event))))

(deftest deleting-a-check-is-asked-first
  (let [[{[ev {:keys [ask then]}] :ok}]
        (fx-of :confirm (step {} [:shared.panel/delete-check :dataset :o {:id 3 :name "n"}]))]
    (is (= :rf/request ev))
    (is (= [api/delete! :health-check-of {:id 3}] ask))
    (is (= [[:shared.panel/checks-changed :dataset :o 0]] then))))

(deftest a-writeback-answer-for-another-object-is-dropped
  (let [[s :as r] (step {} [:shared.panel/writeback :order "a"])
        {:keys [ok err]} (call-of r)
        preview #(get-in (first %) [:shared.panel :writeback :preview])]
    (is (rd/loading? (preview [s])))
    (is (= {:x 1} (preview (step s (conj ok {:x 1})))))
    (is (rd/loading? (preview (step (assoc-in s [:shared.panel :writeback :for] "b")
                                    (conj ok {:x 1})))))
    (is (= "weg" (rd/reason (preview (step s (conj err "weg"))))))))

(deftest a-confirmed-writeback-is-done-and-the-snapshot-read-again
  (let [[s :as r] (step {:shared.panel {:writeback {:for "a" :preview {:object-id "a"}}}}
                        [:shared.panel/writeback-confirm])
        [s' fx] (step s (conj (:ok (call-of r)) nil))]
    (is (get-in s [:shared.panel :writeback :sending?]))
    (is (false? (get-in s' [:shared.panel :writeback :sending?])))
    (is (= {:pushed true} (get-in s' [:shared.panel :writeback :done])))
    (is (= [[:invoke [sync/refresh-snapshot!]]] (remove nil? fx)))))

(deftest a-rollback-is-asked-first-then-refreshes
  (let [[{:keys [ok]}] (fx-of :confirm (step {} [:shared.panel/rollback
                                                 {:kind :dataset :node-kw :o
                                                  :commit "abcdef12345"}]))
        {call-ok :ok :keys [call]} (call-of (step {} ok))
        [changed] (fx-of :dispatch (step {} (conj call-ok {})))]
    (is (= [api/post! :lineage-rollback {:node-kind :dataset :node-key :o
                                         :commit "abcdef12345"}]
           call))
    (is (= [[:invoke [sync/refresh-snapshot!]]]
           (remove nil? (second (step {} changed)))))))
