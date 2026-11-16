(ns ^:unit grammar.layout-refs-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.findings.apps :as apps]
            [sutra.capps.layout-refs :as lr]))

(def ^:private ctx
  {:actions {:zuordnen #{:abschnitt :auftrag} :ohne-params #{}}
   :types   {:block #{:id :netto} :beleg #{:id :pdf}}})

(defn- rules [layout & [extra]]
  (mapv (juxt :rule (some-fn :action :field) :param)
        (lr/problems layout (merge ctx extra))))

(deftest an-action-the-world-does-not-hold
  (is (= [[:action-unknown :weg nil]]
         (rules {:widget :action-button :action :weg :params {:p 1}})))
  (is (= [[:action-unknown :auch-weg nil]]
         (rules {:actions [{:action :auch-weg}]})) "the app's own list of actions too")
  (is (= [] (rules {:widget :action-button :action "ri.sutra.main.action.x"}))
      "a RID is not judged by name")
  (is (= [] (lr/problems {:action :weg} (dissoc ctx :actions))) "no world, no judgement"))

(deftest a-parameter-the-action-does-not-have
  (is (= [[:action-param :zuordnen :falsch]]
         (rules {:widget :action-button :action :zuordnen :params {:falsch 1 :abschnitt 2}})))
  (is (= [[:action-param :zuordnen :falsch]]
         (rules {:widget :combo-input :action :zuordnen :value-param :falsch})))
  (is (= [] (rules {:boxes :b :action :zuordnen :object-param :abschnitt
                    :boxes-param :auftrag})))
  (is (= [] (rules {:action :ohne-params :params {:x 1}}))
      "an action that declares none is judged by its name alone"))

(deftest a-field-the-row-does-not-have
  (testing "at the root of an object view"
    (is (= [[:item-field :weg nil]]
           (rules {:items [{:text {:item :weg}} {:text {:item :pdf}}]} {:item-type :beleg})))
    (is (= [[:item-field :weg nil]] (rules {:when {:item-not :weg}} {:item-type :beleg}))))
  (testing "in a loop over a type a variable names"
    (let [loop* {:vars {:bloecke {:linked :block}}
                 :items [{:kind :loop :items {:val :bloecke}
                          :body {:text {:item :weg}}
                          :empty {:text {:item :pdf}}
                          :group-head {:text {:item :gruppe}}}]}]
      (is (= [[:item-field :weg nil] [:item-field :gruppe nil]] (rules loop* {:item-type :beleg}))
          ":empty reads the row around the loop, :group-head the group's first row")))
  (testing "in a loop over {:source t} — the spelling every set has since refs/lift-body"
    (is (= [[:item-field :weg nil]]
           (rules {:kind :loop :items {:source :block} :body {:text {:item :weg}}})))
    (is (= [[:item-field :weg nil]]
           (rules {:vars  {:alle {:source :block} :offen {:from :alle :where [[:netto :> 0]]}}
                   :items [{:kind :loop :items {:val :offen} :body {:text {:item :weg}}}]}))
        "followed through {:from v} to the type")
    (is (= []
           (rules {:derived-properties {:block {:summe {:kind :aggregation}}}
                   :items [{:kind :loop :items {:source :block} :body {:text {:item :summe}}}]}))
        "a property the app derives for the type is a field of its row"))
  (testing "what is not statically known is not judged"
    (is (= [] (rules {:text {:item :weg}})) "an app has no row at its root")
    (is (= [] (rules {:kind :loop :items {:val :offen} :body {:text {:item :weg}}})))
    (is (= [] (rules {:vars {:s {:from :block :aggregate {:n :count}}}
                      :items [{:kind :loop :items {:val :s} :body {:text {:item :n}}}]})))
    (is (= [] (rules {:widget :chart :layers [{:text {:item :weg}}]} {:item-type :beleg}))
        "a param whose row the widget spec leaves open (:row :unknown)")))

(defn- findings [before-layout after-layout]
  (apps/layout-reference-findings
   {:before {:applications {:app {:layout before-layout}}}
    :after  {:actions {} :object-types {}}
    :plan   [{:kind :application :entity :app :outcome :updated
              :args {:layout after-layout}}]
    :staged {:application {:app {:layout after-layout}}}}))

(deftest what-prod-holds-warns-what-is-new-refuses
  (let [old {:widget :action-button :action :alt}
        new {:widget :action-button :action :neu}]
    (is (= [:warn] (map :severity (findings old old))))
    (is (= [:warn :error] (map :severity (findings old {:items [old new]}))))
    (is (re-find #"Aktion :neu gibt es nicht" (:message (first (findings old new)))))))

(defn- param-bindings [layout]
  ((:find apps/app-param-bindings)
   {:plan   [{:kind :application :entity :app :outcome :updated :args {:layout layout}}]
    :staged {:application {:app {:layout layout}}}}))

(deftest a-binding-the-param-does-not-take-only-warns-at-the-push
  (let [fs (param-bindings {:kind :tabs :select {:val :v} :tabs []})]
    (is (= [[:app-param-binding :warn]] (map (juxt :rule :severity) fs)))
    (is (re-find #"select braucht eine Variable" (:message (first fs)))))
  (testing ":compare reads the compare variable's value — the spec says so per param"
    (is (= [] (param-bindings {:widget :chart :compare {:val :v}}))
        "what schichtplanung writes, and what the renderer reads (a map, not a handle)")
    (is (re-find #"compare braucht den Wert"
                 (:message (first (param-bindings {:widget :chart :compare {:var :v}})))))))

(deftest the-world-knows-derived-and-interface-properties
  (let [loop* {:kind :loop :items {:source :werkzeug}
               :body {:items [{:text {:item :stumpf?}} {:text {:item :zustand}}
                              {:text {:item :weg}}]}}
        fs    (apps/layout-reference-findings
               {:before {:applications {}}
                :after  {:actions      {}
                         :object-types {:werkzeug
                                        {:attribute-list
                                         [{:attribute-name :stumpf? :attribute-type :boolean
                                           :opts {:derived {:expression [:= :zustand "s"]}}}]
                                         :implements [:schaerfbar]}}
                         :interfaces   {:schaerfbar
                                        {:property-list [{:attribute-name :zustand}]}}}
                :plan   [{:kind :application :entity :app :outcome :created
                          :args {:layout loop*}}]
                :staged {:application {:app {:layout loop*}}}})]
    (is (= 1 (count fs)))
    (is (re-find #":weg" (:message (first fs))) "only the field the type does not hold")))
