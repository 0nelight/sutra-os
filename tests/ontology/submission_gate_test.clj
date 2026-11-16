(ns ^:unit ontology.submission-gate-test
  (:require [sutra.parser.spec :as spec]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.types.submission :as csub]
            [sutra.parser.ent :as ent]
            [sutra.parser.lint :as lint]))

(def ^:private specs
  {:bestaetigt-von {:required true :name "Bestätigt von"}
   :menge          {:required true}
   :notiz          {}
   :wer            {:required true :prefill :current-user}
   :am             {:required true :default "heute"}
   :sonderfall     {:required true :overrides [{:when [:= 1 1] :required false}]}})

(deftest what-a-client-may-gate-on
  (testing "only a plainly :required param blocks — a prefilled, defaulted or"
    (is (= [:bestaetigt-von :menge] (csub/missing-required specs {})))
    (is (= [] (csub/missing-required specs {:bestaetigt-von "Alma" :menge 0}))))
  (testing "a server-filled or conditionally-required param is NOT the client's to block"
    (is (not (csub/client-gated? (:wer specs))))
    (is (not (csub/client-gated? (:am specs))))
    (is (not (csub/client-gated? (:sonderfall specs)))))
  (testing "0 and false are VALUES; blank string and empty collection are not"
    (is (csub/supplied? 0))
    (is (csub/supplied? false))
    (is (not (csub/supplied? "")))
    (is (not (csub/supplied? "   ")))
    (is (not (csub/supplied? [])))
    (is (not (csub/supplied? nil))))
  (testing "the block NAMES the field, by its authored :name when there is one"
    (let [g (csub/submit-gate specs {})]
      (is (true? (:blocked? g)))
      (is (str/includes? (:reason g) "Bestätigt von"))
      (is (str/includes? (:reason g) "menge")))
    (is (nil? (:reason (csub/submit-gate specs {:bestaetigt-von "Alma" :menge 1}))))))

(deftest a-criterion-carries-its-own-failure-message
  (testing "the authored sentence IS the refusal"
    (is (= "Nur ein Prüfer darf freigeben."
           (csub/criterion-refusal :freigeben {:test {} :pred [:= 1 2]
                                              :message "Nur ein Prüfer darf freigeben."}))))
  (testing "without one the platform can only be generic"
    (is (str/includes? (csub/criterion-refusal :freigeben {:test {} :pred [:= 1 2]})
                       "Vorbedingung"))))

(deftest the-ent-grammar-accepts-the-message
  (let [file {:schema {:actions {:freigeben
                                 {:params {:rolle {:type :string :required true}}
                                  :submission-criteria
                                  [{:test {:r [:param :rolle]} :pred [:= :r "pruefer"]
                                    :message "Nur ein Prüfer darf freigeben."}]}}}}]
    (testing "an authored :message survives the door"
      (is (= "Nur ein Prüfer darf freigeben."
             (-> (ent/read-text nil "t.ent" (pr-str file))
                 (get-in [:schema :actions :freigeben :submission-criteria 0 :message])))))
    (testing "a criterion without one is reported — the refusal would be generic"
      (let [mute (update-in file [:schema :actions :freigeben :submission-criteria 0]
                            dissoc :message)
            f    (->> (lint/run-lint (:schema (ent/read-text nil "t.ent" (pr-str mute)))
                                 (spec/vocabulary))
                      (filter #(= :criterion-without-message (:rule %))))]
        (is (= 1 (count f)))
        (is (str/includes? (:message (first f)) "freigeben"))))))

(def ^:private button-schema
  {:actions {:freigeben {:params {:ziel  {:type :string :required true}
                                 :grund {:type :string}}}}
   :applications
   {"ri.a" {:application-name :freigabe-app
            :layout {:action {:action :freigeben :parameters [:ziel :grund]}
                     :kind :rows
                     :items [{:widget :action-button :label "Freigeben" :action :freigeben
                              :params {:ziel {:val :z} :grund {:val :g}}}]}}}})

(defn- button-findings [schema]
  (filter #(= :action-button-cannot-submit (:rule %)) (lint/run-lint schema (spec/vocabulary))))

(deftest a-button-that-can-never-submit-is-named-before-it-ships
  (testing "a fully bound button is silent"
    (is (empty? (button-findings button-schema))))
  (testing "a button that binds no value for a :required param is refused every time"
    (let [f (button-findings
             (update-in button-schema
                        [:applications "ri.a" :layout :items 0 :params] dissoc :ziel))]
      (is (= 1 (count f)))
      (is (str/includes? (:message (first f)) "ziel"))))
  (testing "a button naming an action the app never whitelisted is dead on the page"
    (let [f (button-findings
             (assoc-in button-schema [:applications "ri.a" :layout :items 0 :action] :storno))]
      (is (= 1 (count f)))
      (is (str/includes? (:message (first f)) "whitelist")))))

(deftest a-whitelist-that-starves-a-required-param-is-named-too
  (testing "the app may only pass what it whitelists — omitting a required param kills the action"
    (let [f (button-findings
             (assoc-in button-schema
                       [:applications "ri.a" :layout :action :parameters] [:grund]))]
      (is (= 1 (count f)))
      (is (str/includes? (:message (first f)) "ziel"))
      (is (str/includes? (:message (first f)) ":parameters")))))

(deftest an-empty-list-is-a-value-when-the-param-takes-many
  (testing "a :multiple param fans over nothing — that is a no-op, not a missing field"
    (let [multi {:targets {:required true :multiple true}}]
      (is (= [] (csub/missing-required multi {:targets []})))
      (is (= [:targets] (csub/missing-required multi {})))))
  (testing "a scalar param is still missing when it is empty"
    (is (= [:ziel] (csub/missing-required {:ziel {:required true}} {:ziel []})))))
