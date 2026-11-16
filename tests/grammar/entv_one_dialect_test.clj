(ns ^:unit grammar.entv-one-dialect-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.layout :as lay]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-emit :as pemit]))

(def ^:private cards
  {:cards [{:widget :attributes}
           {:widget :relations :title "Luftfahrzeug & Halter"}
           {:widget :matrix :config {:object-type :arbeitsauftrag :owner-type :arbeitsauftrag
                                     :children-conn :hat-arbeitskarte :other-type :sku}}
           {:widget :documents :hidden? true}]})

(deftest cards-lower-to-a-rows-tree
  (is (= {:kind  :rows
          :items [{:widget :attributes}
                  {:widget :relations :title "Luftfahrzeug & Halter"}
                  {:widget :matrix :owner-type :arbeitsauftrag :children-conn :hat-arbeitskarte
                   :other-type :sku}
                  {:widget :documents :hidden? true}]}
         (lay/lower-view-body cards))
      "config inline, the injected :object-type gone, title and hidden as riders")
  (let [tree {:kind :rows :items [{:widget :markdown :text "x"}]}]
    (is (identical? tree (lay/lower-view-body tree)) "a tree passes untouched")))

(defn- layout-of [parsed factor]
  (some (fn [[_ a]] (when (= factor (:form-factor a)) (:layout a)))
        (get-in parsed [:schema :applications])))

(deftest the-entv-door-stores-the-tree
  (let [parsed (ent/parse-entv {:object-type :arbeitsauftrag :full cards
                                :panel {:cards [{:widget :attributes}]}}
                               "object-views/arbeitsauftrag.entv")]
    (is (= :rows (:kind (layout-of parsed :full))))
    (is (= [{:widget :attributes}] (:items (layout-of parsed :panel))))
    (testing "the projection writes the tree back, and it reads back the same"
      (let [back (#'pemit/emit-view-file :arbeitsauftrag
                                      (get-in parsed [:schema :applications]))]
        (is (not (contains? (:full back) :cards)))
        (is (= (layout-of parsed :full)
               (layout-of (ent/parse-entv back "object-views/arbeitsauftrag.entv") :full)))))))

(deftest a-stored-card-view-is-written-back-as-a-tree
  (let [back (#'pemit/emit-view-file :arbeitsauftrag
                                  {:arbeitsauftrag-view-full {:object-type :arbeitsauftrag
                                                :form-factor :full
                                                :layout cards}})]
    (is (= (lay/lower-view-body cards) (:full back)))))

(deftest a-monolith-object-view-lowers-the-same-way
  (is (= (lay/lower-view-body cards)
         (get-in (sugar/lower-schema {:applications
                                        {:v {:object-type :arbeitsauftrag :form-factor :full
                                           :layout cards}}})
                 [:applications :v :layout]))))

(def ^:private neu-view
  {:kind  :rows
   :vars  {:alle-mechaniker     {:source :mechaniker}
           :neuer-mechaniker {:default nil}}
   :items [{:widget :action-form :action :mangel-beheben
            :defaults {:mechaniker {:val :neuer-mechaniker}}}
           {:widget :button :label "＋ neu"
            :on-click {:do :open :overlay :mechaniker-neu}}
           {:kind :overlay :overlay :mechaniker-neu :title "Neuer Mechaniker"
            :body {:widget :action-form :action :mechaniker-anlegen
                   :output {:var :neuer-mechaniker}}}
           {:widget :action-button :action :mechaniker-anlegen :label "schnell"
            :params {:name "?"} :output {:var :neuer-mechaniker}}]})

(deftest an-action-form-names-its-output-variable-and-sits-in-an-overlay
  (let [parsed (ent/parse-entv {:object-type :mangel :full neu-view}
                               "object-views/mangel.entv")
        tree   (layout-of parsed :full)]
    (is (= {:var :neuer-mechaniker}
           (get-in tree [:items 2 :body :output]))
        "the overlay and the form's `:output` pass the `.entv` door unchanged")
    (is (= {:var :neuer-mechaniker} (get-in tree [:items 3 :output]))
        "an `:action-button` names one the same way"))
  (testing "the output is a variable to WRITE — a value has nowhere to put the object"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid \.entv"
                          (ent/parse-entv {:object-type :mangel
                                           :full (assoc-in neu-view [:items 2 :body :output]
                                                           {:val :neuer-mechaniker})}
                                          "object-views/mangel.entv")))))
