(ns ent-lsp.online-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ent-lsp.daemon :as daemon]
            [ent-lsp.facts :as facts]
            [ent-lsp.grammar :as grammar]
            [ent-lsp.online :as online]
            [ent-lsp.read :as read]
            [ent-lsp.spec :as spec]
            [ent-lsp.workspace :as ws]))

(def ^:private live-schema
  {:projects {:controlling {} :partner {}}
   :objects  {:geschaeftspartner {:project :partner :title :gp-nr
                                  :attributes [[:gp-nr :string] [:name :string]]}
              :auftrag           {:project :controlling :title :auf-nr
                                  :attributes [[:auf-nr :string] [:alt :string]]}}})

(defn- live
  []
  (let [s (spec/load-spec)]
    {:facts {:defs (:defs (facts/extract s (str online/file ".ent")
                                         (read/parse (pr-str {:schema live-schema}))))
             :refs [] :writes []}
     :table (grammar/symbol-table [(grammar/lower live-schema)])}))

(def ^:private checkout
  {"ontology.ent"
   (pr-str {:schema {:projects {:controlling {:imports [:partner]}}
                     :objects  {:auftrag {:project :controlling :title :auf-nr
                                          :attributes [[:auf-nr :string]]}}
                     :links    [[:auftrag-partner :auftrag :geschaeftspartner :many-to-one
                                 {:project :controlling :fk {:attribute :auf-nr}}]]}})})

(defn- analyzed [online]
  (ws/analyze (cond-> (reduce-kv ws/set-text {:root "/ws" :spec (spec/load-spec) :files {}}
                                 checkout)
                online (ws/set-online online))))

(defn- found [w rule]
  (set (for [[file fs] (:findings w) f fs :when (= rule (:rule f))]
         [file (get-in f [:vars :name])])))

(deftest a-type-that-lives-only-on-the-instance-resolves
  (is (contains? (found (analyzed nil) :unbekannt) ["ontology.ent" ":geschaeftspartner"]))
  (is (not (contains? (found (analyzed (live)) :unbekannt) ["ontology.ent" ":geschaeftspartner"]))))

(deftest the-checkout-wins-over-the-instance
  (let [w (analyzed (live))]
    (testing "a type both declare is not a duplicate"
      (is (some #(= [:object :auftrag] ((juxt :kind :name) %)) (get-in (live) [:facts :defs])))
      (is (empty? (found w :doppelte-definition))))
    (testing "the live properties of a type the checkout declares are not merged in"
      (is (empty? (filter #(and (= online/file (:file %)) (= :auftrag (:object %)))
                          (get-in w [:index :defs])))))
    (testing "nothing is ever reported on the instance itself"
      (is (not (contains? (:findings w) online/file))))))

(deftest the-hook-text
  (let [base  {:rel "a.ent" :defs [{:kind :action :name :x}]
               :online-status {:rev "91c7a35b" :checked-at 0}}
        f     (fn [sev rule row]
                {:file "a.ent" :loc {:row row :col 1} :severity sev :rule rule :message "m"})
        text  (daemon/render (assoc base
                                    :before {}
                                    :after
                                      {"a.ent" [(f :error :unbekannt 2)
                                                (f :hint :beschreibung-fehlt 3)]
                                       "b.ent" [(assoc (f :warning :import-fehlt 4) :file "b.ent")]}
                                    :lint {:findings [{:entity "x" :entity-class :actions :rule :r
                                                       :severity :error
                                                       :message (apply str (repeat 300 "y"))}
                                                      {:entity "other" :entity-class :actions
                                                       :rule :r :message "z"}]})
                             5000)]
    (testing "own findings in full, hints only counted"
      (is (str/includes? text "a.ent:2:1: error [unbekannt] m"))
      (is (not (str/includes? text "a.ent:3:1")))
      (is (str/includes? text "+ 1 Hinweise (beschreibung-fehlt 1)")))
    (testing "what is new elsewhere"
      (is (str/includes? text "Neu in anderen Dateien")))
    (testing "the instance's lint: only what this file defines, clipped"
      (is (str/includes? text "meldet 1 zu Ressourcen dieser Datei (r 1)"))
      (is (not (str/includes? text "other")))
      (is (str/includes? text "…")))
    (is (str/includes? text "Instanz: Stand 91c7a35b, geprüft vor 5 s"))))
