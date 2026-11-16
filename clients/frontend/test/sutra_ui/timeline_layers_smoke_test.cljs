(ns sutra-ui.timeline-layers-smoke-test
  (:require [cljs.test :refer-macros [deftest is]]
            [sutra.capps.timeline-layers :as timeline-layers]))

(def ^:private rechnung
  {:id "r1" :doc-number "38-3615787" :eingegangen-am "2026-06-24T09:12:00Z"
   :extrahiert-am "2026-06-24T09:30:05Z" :modell "grok-4-fast-reasoning"})

(def ^:private vorschlag
  {:id "v1" :gefragt-am "2026-09-30T12:05:00+00:00" :auftrag-bezeichnung "Outdoorkitchen"
   :auf-nr "BA25.0151"})

(deftest layers-merge-into-one-chronological-rail
  (let [ev (timeline-layers/events [{:rows [vorschlag] :at :gefragt-am
                                     :title-prop :auftrag-bezeichnung
                        :body [:auf-nr] :label "Vorschlag" :tone :info}
                       {:rows [rechnung] :at :eingegangen-am :title "Mail eingegangen"
                        :label "Mail"}
                       {:rows [rechnung] :at :extrahiert-am :title "Vom Modell gelesen"
                        :body :modell}])]
    (is (= ["Mail eingegangen" "Vom Modell gelesen" "Outdoorkitchen"] (map :title ev))
        "oldest first, across layers and across two date properties of one object")
    (is (= "2026-06-24 09:12" (:at (first ev))) "date and minute, no zone")
    (is (= "grok-4-fast-reasoning" (:body (second ev))))
    (is (= "BA25.0151" (:body (last ev))))
    (is (= :info (:tone (last ev))))
    (is (= "Vorschlag" (:layer (last ev))))
    (is (= vorschlag (:row (last ev))) "the event carries its row")))

(deftest a-row-without-its-date-is-no-event
  (is (empty? (timeline-layers/events [{:rows [(dissoc rechnung :extrahiert-am)] :at :extrahiert-am
                                        :title "x"}])))
  (is (= "2026-06-24"
         (:at (first (timeline-layers/events [{:rows [{:d "2026-06-24"}] :at :d :title "x"}]))))
      "a plain date stays a date"))
