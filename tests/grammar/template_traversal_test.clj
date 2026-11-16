(ns ^:unit grammar.template-traversal-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.ent :as ent]
            [sutra.types.template :as tmpl]))

(defn- entt [parameters]
  (pr-str {:name :bericht :title "Bericht" :source "{{ x }}" :parameters parameters}))

(def ^:private anchored
  {:auftrag {:type :object :object-type :arbeitsauftrag}
   :zeiten  {:type :object-set :object-type :zeitbuchung
             :search-around :zeitbuchung-arbeitsauftrag :of :auftrag}})

(def ^:private chained
  (assoc anchored
         :projekte {:type :object-set :object-type :projekt}
         :angebote {:type :object-set :object-type :angebot
                    :search-around :angebot-projekt :of :projekte}))

(deftest one-hop-from-one-object-passes
  (is (empty? (tmpl/parameter-problems anchored)))
  (is (map? (ent/read-text :doc-template "bericht.entt" (entt anchored)))))

(deftest a-traversal-anchored-on-a-set-is-refused-before-install
  (testing "the shared rule names the parameter, its :of, and the one-object requirement"
    (let [[p & more] (tmpl/parameter-problems chained)]
      (is (nil? more))
      (is (re-find #":angebote follows :angebot-projekt from :projekte" p))
      (is (re-find #"one object to start from" p))))
  (testing "the .entt grammar — what changeset diff and the push gates run — refuses it"
    (is (thrown-with-msg? Exception #"one object to start from"
                          (ent/read-text :doc-template "bericht.entt" (entt chained)))))
  (testing "a traversal anchored on another traversal is the same second hop"
    (is (seq (tmpl/parameter-problems
              (assoc anchored :weiter {:type :object-set :object-type :projekt
                                       :search-around :x :of :zeiten}))))))

(def ^:private kette
  {:ka     {:type :object :object-type :kundenauftrag}
   :zeiten {:type :object-set :object-type :zeitbuchung :of :ka
            :search-around [:projekt-kundenauftrag :arbeitsauftrag-projekt
                            :zeitbuchung-arbeitsauftrag]}})

(defn- near
  [links]
  (constantly links))

(def ^:private links
  (near {"projekt-kundenauftrag"       {:from :projekt :to :kundenauftrag}
         "arbeitsauftrag-projekt"     {:from :arbeitsauftrag :to :projekt}
         "zeitbuchung-arbeitsauftrag" {:from :zeitbuchung :to :arbeitsauftrag}
         "zahlung-ausgangsrechnung"    {:from :zahlung :to :ausgangsrechnung}}))

(deftest a-chain-of-links-is-grammar-and-walks
  (is (map? (ent/read-text :doc-template "bericht.entt" (entt kette))))
  (is (nil? (tmpl/search-around-problem kette :zeiten links)))
  (is (nil? (tmpl/search-around-problem anchored :zeiten
                                         (near {"zeitbuchung-arbeitsauftrag"
                                                {:from :zeitbuchung :to :arbeitsauftrag}})))
      "the one-link form is a chain of one"))

(deftest a-chain-that-does-not-walk-is-named
  (is
   (re-find #"projekt has no link side named zahlung-ausgangsrechnung \(the way \[:ka :projekt-kundenauftrag\] stands at projekt"
            (tmpl/search-around-problem
             (assoc-in kette [:zeiten :search-around]
                       [:projekt-kundenauftrag :zahlung-ausgangsrechnung])
             :zeiten links)))
  (is (re-find #"ends at arbeitsauftrag, not at its :object-type zeitbuchung"
               (tmpl/search-around-problem
                (assoc-in kette [:zeiten :search-around]
                          [:projekt-kundenauftrag :arbeitsauftrag-projekt])
                :zeiten links)))
  (is (re-find #"has no link side named gibt-es-nicht"
               (tmpl/search-around-problem
                (assoc-in kette [:zeiten :search-around] [:gibt-es-nicht])
            :zeiten links))))

(def ^:private self-link
  (near {"hauptprojekt-projekte" {:from :projekt :to :projekt
                                  :forward-api-name :hauptprojekt :reverse-api-name :projekte}}))

(deftest a-self-link-is-walked-by-the-side-it-names
  (let [p (fn [side] {:p {:type :object :object-type :projekt}
                      :x {:type :object-set :object-type :projekt :of :p :search-around side}})]
    (is (nil? (tmpl/search-around-problem (p :projekte) :x self-link)))
    (is (nil? (tmpl/search-around-problem (p :hauptprojekt) :x self-link)))
    (is (re-find #"has no link side named hauptprojekt-projekte"
                 (tmpl/search-around-problem (p :hauptprojekt-projekte) :x self-link))
        "a self-link's type name is no side once both sides are named")))
