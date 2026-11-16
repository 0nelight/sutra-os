(ns ^:unit platform.cli-action-plan-id-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest is testing]]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.out :as out]
            [sutra.cli.verbs.actions :as actions]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.lint :as lint]
            [sutra.parser.project-emit :as pemit]
            [sutra.wire.api :as api]))

(defn- run-action! [action args evaluation minted]
  (let [calls (atom [])]
    (with-redefs [out/prompt-yes? (constantly true)
                  api/get!  (fn [id & _] (case id :schema {:status 200 :body {}}))
                  api/post! (fn [id & _] (case id :action-params {:status 200 :body evaluation}))
                  api/mutate (fn [a args opts]
                               (swap! calls conj [args opts])
                               (let [args (cond-> args
                                            (and minted (not (:id args))) (assoc :id (minted)))]
                                 {:status 200
                                  :body
                                    (cond-> {:action a
                                             :plan [{:op :create
                                                     :target {:type "aufgabe" :id (:id args)}}]}
                                      (:dry-run? opts) (assoc :id (:id args)))}))]
      (with-out-str
        (dispatch/step! {:refs (atom {})} [:action {:name action :args args}])))
    @calls))

(deftest the-apply-carries-the-id-the-plan-showed
  (testing "create-<type>: the server mints on the preview, the apply names that id"
    (let [n (atom 0)
          [[plan-args plan-opts] [apply-args apply-opts]]
          (run-action! "create-aufgabe" {:titel "x"} nil #(str "id-" (swap! n inc)))]
      (is (:dry-run? plan-opts))
      (is (:publish? apply-opts))
      (is (nil? (:id plan-args)))
      (is (= "id-1" (:id apply-args)))))
  (testing "a :uuid prefill is filled once, before the preview, and both calls carry it"
    (let [[[plan-args] [apply-args]]
          (run-action! "anlegen" {:titel "x"}
                       {:params {:nr {:prefill :uuid} :titel {}}
                        :values {:nr "u-1" :titel "x"}}
                       nil)]
      (is (= {:titel "x" :nr "u-1"} plan-args apply-args))))
  (testing "a value the caller gave is never replaced"
    (is (= {:nr "mine"}
           (actions/pin-generated-params {:nr "mine"}
                                      {:params {:nr {:prefill "uuid"}} :values {:nr "u-1"}}))))
  (testing "a .entr step nests its args under :arg0"
    (is (= {:arg0 {:nr "u-1"}}
           (actions/pin-generated-params {:arg0 {}}
                                      {:params {:nr {:prefill :uuid}} :values {:nr "u-1"}})))))

(def ^:private ohne-schluessel
  {:actions {:anlegen {:params {:titel {:type :string}}
                       :rules  [{:op :create :object-type :aufgabe
                                 :values {:titel [:get :titel]}}]}}})

(deftest a-create-rule-without-a-key-gets-a-key-parameter
  (let [a (get-in (sugar/lower-schema ohne-schluessel) [:actions :anlegen])]
    (testing "the rule's :id reads a hidden :uuid parameter, so it is minted once per submission"
      (is (= [:get :regel-0-id] (:id (first (:rules a)))))
      (is (= {:type :string :prefill :uuid :exposure :hidden} (:regel-0-id (:params a)))))
    (testing "and the CLI pins it like any :uuid prefill, so the plan and the apply agree"
      (is (= {:titel "x" :regel-0-id "u-1"}
             (actions/pin-generated-params {:titel "x"}
                                        {:params (:params a) :values {:regel-0-id "u-1"}}))))
    (testing "lowering twice changes nothing"
      (is (= a
             (get-in (sugar/lower-schema (sugar/lower-schema ohne-schluessel))
                     [:actions :anlegen]))))))

(deftest an-authored-key-is-left-alone-and-a-multiple-param-still-gets-one
  (let [mit (assoc-in ohne-schluessel [:actions :anlegen :rules 0 :id] [:get :nr])
        mul (assoc-in ohne-schluessel [:actions :anlegen :params :titel :multiple] true)]
    (is (= (get-in mit [:actions :anlegen]) (get-in (sugar/lower-schema mit) [:actions :anlegen])))
    (is (= [:get :regel-0-id] (get-in (sugar/lower-schema mul) [:actions :anlegen :rules 0 :id]))
        "a bulk action is applied once: its :create makes one object, so one key")))

(deftest lint-warns-on-a-create-whose-key-no-parameter-names
  (let [hits (fn [schema] (filter #(= :create-without-key-param (:rule %))
                                  (lint/run-lint schema (spec/vocabulary))))]
    (is (= 1 (count (hits ohne-schluessel))) "the live form, before its next push")
    (is (= 1 (count (hits (sugar/lower-schema ohne-schluessel))))
        "the desugared form still asks for a named one")
    (is (= :info (:severity (first (hits ohne-schluessel)))))
    (is (empty? (hits (assoc-in ohne-schluessel [:actions :anlegen :rules 0 :id] [:get :nr]))))))

(deftest the-implicit-key-parameter-never-reaches-the-authors-file
  (let [authored (-> ohne-schluessel
                     (assoc-in [:actions :anlegen :rules 0]
                               {:op :create :object-type :aufgabe :values {:titel "fest"}})
                     (update-in [:actions :anlegen :rules] conj
                                {:op :create :object-type :notiz :id [:get :nr] :values {}}))
        back     (fn [schema]
                   (get-in (:ent (pemit/export schema {:all-projects? true :inline? true}))
                           [:schema :actions]))]
    (is (= (:actions authored) (back (sugar/lower-schema authored)))
        "read .ent, export: the file the author wrote, not the desugared one")
    (is (= {:anlegen {:rules [{:op :create :object-type :aufgabe :values {}}]}}
           (back (sugar/lower-schema {:actions
                                        {:anlegen
                                           {:rules [{:op :create :object-type :aufgabe
                                                     :values {}}]}}})))
        "an action whose only parameter was the implicit one exports without :params")))
