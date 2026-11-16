(ns sutra-ui.forms.events-smoke-test
  (:require [cljs.test :refer [deftest is]]
            [sutra-ui.credentials :as creds]
            [sutra-ui.forms.events]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.machine-testing :refer [step]]
            [sutra-ui.state.ontology :as ontology]))

(defn- fx-of [kind [_ fx]] (keep (fn [[k a]] (when (= kind k) a)) fx))

(deftest a-link-type-is-added-or-updated-then-the-form-closes
  (let [done #(do)]
    (is (= [ontology/add-link-type! {:n 1}]
           (:call (first (fx-of :call (step {} [:shared.forms/stage-link {:n 1} true done]))))))
    (let [[{:keys [call ok]}] (fx-of :call (step {} [:shared.forms/stage-link {:n 1} false done]))]
      (is (= [ontology/update-link-type! {:n 1}] call))
      (is (= [[done]] (fx-of :invoke (step {} (conj ok {}))))))))

(deftest an-incomplete-object-form-stays-open
  (let [done #(do)]
    (is (= [] (second (step {} [:shared.forms/staged-object done
                                :sutra-ui.forms.events/invalid]))))
    (is (= [[:invoke [done]]] (second (step {} [:shared.forms/staged-object done {}]))))))

(deftest credentials-are-sealed-before-the-datasource-is-saved
  (let [done #(do)
        base {:name :ops :type :postgres}
        [{:keys [call ok]}] (fx-of :call (step {} [:shared.forms/save-datasource base
                                                   {:user "u"} "ops" done]))
        [sealed] (fx-of :dispatch (step {} (conj ok "c")))]
    (is (= [creds/seal {:user "u"} {:source "ops"}] call))
    (is (= [data/upsert-datasource! (assoc base :secret-cipher "c")]
           (:call (first (fx-of :call (step {} sealed)))))))
  (is (= [[:shared.forms/sealed {:name :a} nil nil]]
         (fx-of :dispatch (step {} [:shared.forms/save-datasource {:name :a} {} "a" nil])))))

(deftest a-failed-table-says-why-on-the-form
  (let [err (atom nil)]
    (is (= [[:invoke [reset! err "kaputt"]]]
           (second (step {} [:shared.forms/table-failed err "kaputt"]))))))
