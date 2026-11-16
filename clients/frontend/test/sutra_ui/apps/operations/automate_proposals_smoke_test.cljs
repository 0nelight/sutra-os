(ns sutra-ui.apps.operations.automate-proposals-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [sutra-ui.apps.operations.core :as ops]))

(def ^:private proposal
  {:id "7f847e0c-ce0b-4024-b470-7e1bafe98300" :automation-id "a1" :status "open"
   :action :rechnung-erfassen
   :args {:dokument "2b683be2" :lieferant "899487f9" :doc-number "9116599996" :brutto "101.78"}
   :refs {:dokument  {:id "2b683be2" :type "eingangsdokument"
                      :title "9116599996_0099273073_1000.pdf"}
          :lieferant {:id "899487f9" :type "geschaeftspartner" :title "Aero Teile GmbH"}}
   :reason "Vor der Umstellung vom Modell gelesen: Aero Teile GmbH, Rechnung 9116599996"
   :provenance {:source-dataset "grok_email_attached" :source-pk "2357"}
   :source-key "grok_email_attached:2357"
   :object-id "2b683be2" :created-at "2026-09-30T18:00:00"})

(defn- card [p]
  (rds/render-to-string [@#'ops/proposal-card "a1" p]))

(deftest an-open-proposal-renders-its-content
  (let [html (card proposal)]
    (testing "the reason"
      (is (str/includes? html "Aero Teile GmbH, Rechnung 9116599996")))
    (testing "object parameters by title, scalars by value"
      (is (str/includes? html "Aero Teile GmbH</span>"))
      (is (str/includes? html "9116599996_0099273073_1000.pdf"))
      (is (str/includes? html "101.78"))
      (is (str/includes? html "doc-number")))
    (testing "an open proposal can be decided"
      (is (str/includes? html "Annehmen"))
      (is (str/includes? html "Bearbeiten"))
      (is (str/includes? html "Ablehnen")))))

(deftest a-decided-proposal-shows-no-buttons
  (let [html (card (assoc proposal :status "applied" :decided-by "edda"
                          :accepted-args {:brutto "110.00"}))]
    (is (str/includes? html "edda"))
    (is (str/includes? html "110.00"))
    (is (not (re-find #"<button" html)))))
