(ns ent-lsp.checks-test
  (:require [clojure.test :refer [deftest is testing]]
            [ent-lsp.spec :as spec]
            [ent-lsp.workspace :as ws]))

(defn- analyzed [files]
  (ws/analyze (reduce-kv ws/set-text {:root "/ws" :spec (spec/load-spec) :files {}} files)))

(def ^:private project
  {"p/project.ent" "{:schema {:projects {:p {}}}}"})

(defn- found [files rule]
  (set (for [[file fs] (:findings (analyzed (merge project files)))
             f fs :when (= rule (:rule f))]
         [file (get-in f [:vars :name] (get-in f [:vars :object]))])))

(defn- obj [nm & {:as extra}]
  (pr-str {:schema {:objects {nm (merge {:project :p :title :name
                                         :attributes
                                           [[(keyword (str (name nm) "-id")) :string]
                                            [:name :string]]}
                                        extra)}}}))

(deftest link-sides-are-unique-per-object
  (is
   (= #{["p/a/object.ent" ":a"]}
      (found
       {"p/a/object.ent"
          (str "{:schema {:objects {:a {:project :p :title :name :attributes [[:a-id :string] "
               "[:name :string]]}}"
               " :links [[:l1 :a :b :many-to-one {:project :p :forward-api-name :ziel}]"
               " [:l2 :a :b :many-to-one {:project :p :forward-api-name :ziel}]]}}")
        "p/b/object.ent" (obj :b)}
       :link-seite-doppelt)))
  (is
   (= #{["p/a/object.ent" ":selbst"]}
      (found
       {"p/a/object.ent" "{:schema {:objects {:a {:project :p :title :name :attributes [[:a-id :string] [:name :string]]}} :links [[:selbst :a :a :many-to-one {:project :p :forward-api-name :chef}]]}}"}
       :selbst-link-seiten))))

(deftest link-backing-follows-cardinality
  (let [links (fn [opts]
                {"p/a/object.ent"
                   (str "{:schema {:objects {:a {:project :p :title :name :attributes [[:a-id "
                        ":string] [:name :string] [:b-id :string]]}} :links [[:ab :a :b " opts
                        "]]}}")
                 "p/b/object.ent" (obj :b)})]
    (is (= #{["p/a/object.ent" ":ab"]}
           (found (links ":many-to-many {:project :p}") :link-anbindung)))
    (is (= #{["p/a/object.ent" ":ab"]}
           (found (links ":many-to-many {:project :p :fk {:attribute :b-id}}") :link-anbindung)))
    (is (= #{["p/a/object.ent" ":ab"]}
           (found (links ":many-to-one {:project :p :fk {:attribute :b-id} :join {}}")
                  :link-anbindung)))
    (is (empty? (found (links ":many-to-one {:project :p :fk {:attribute :b-id}}")
                       :link-anbindung)))
    (is (empty? (found (links ":many-to-many {:project :p :join {}}") :link-anbindung)))))

(deftest foreign-key-type-matches-the-primary-key
  (is
   (= #{["p/a/object.ent" ":b"]}
      (found
       {"p/a/object.ent" "{:schema {:objects {:a {:project :p :title :name :attributes [[:a-id :string] [:name :string] [:b-nr :long]]}} :links [[:ab :a :b :many-to-one {:project :p :fk {:attribute :b-nr}}]]}}"
        "p/b/object.ent" (obj :b :backing {:dataset :b_raw :primary-key :b-id})}
       :fk-typ))))

(deftest a-dataset-backs-one-object-type
  (is (= #{["p/a/object.ent" ":roh"] ["p/b/object.ent" ":roh"]}
         (found {"p/a/object.ent" (obj :a :dataset :roh)
                 "p/b/object.ent" (obj :b :backing {:dataset :roh})}
                :datenquelle-mehrfach))))

(deftest status
  (testing "vocabulary per kind"
    (is (= #{["p/a/object.ent" ":a"]}
           (found {"p/a/object.ent" (obj :a :status :live)} :status-unbekannt)))
    (is (empty? (found {"p/a/object.ent" (obj :a :status :endorsed)} :status-unbekannt))))
  (testing "a reference to a deprecated resource names the replacement"
    (let [files {"p/a/object.ent" (obj :a :status :deprecated :deprecation
                                       {:reason "alt" :replacement :b})
                 "p/b/object.ent" (obj :b)
                 "p/b/actions.ent" "{:schema {:actions {:mach {:rules [{:op :update :object-type :a :set [:name]}]}}}}"}
          fs (get-in (analyzed (merge project files)) [:findings "p/b/actions.ent"])]
      (is (some #(and (= :veraltet (:rule %)) (= {:replacement :b} (get-in % [:vars :data]))) fs))
      (is (empty? (found files :deprecation-fehlt)))))
  (testing "deprecated needs its metadata"
    (is (= #{["p/a/object.ent" ":a"]}
           (found {"p/a/object.ent" (obj :a :status :deprecated)} :deprecation-fehlt))))
  (testing "a link is at most as far as its ends"
    (let [files (fn [st]
                  {"p/a/object.ent"
                     (str "{:schema {:objects {:a {:project :p :status :active :title :name "
                          ":attributes [[:a-id :string] [:name :string]]}} :links [[:ab :a :b "
                          ":many-to-one {:project :p :status :active}]]}}")
                   "p/b/object.ent" (obj :b :status st)})]
      (is (= #{["p/a/object.ent" ":ab"]} (found (files :deprecated) :link-status)))
      (is (= #{["p/a/object.ent" ":ab"]} (found (files :experimental) :link-status)))
      (is (empty? (found (files :active) :link-status))))))

(def ^:private iface
  "{:schema {:interfaces {:ort {:project :p :properties [[:name :string] [:lage :geopoint]]
                                 :links [{:link-name :betreiber :cardinality :one :to-object-type :firma :required true}]}
                          :bau {:project :p :extends [:ort]}}}}")

(deftest interface-implementations
  (let [halle (fn [attrs links]
                {"p/ort/interface.ent" iface
                 "p/firma/object.ent" (obj :firma)
                 "p/halle/object.ent"
                   (str "{:schema {:objects {:halle {:project :p :title :name :implements [:bau] "
                        ":attributes " attrs "}} :links " links "}}")})
        ok-attrs "[[:halle-id :string] [:name :string] [:lage :geopoint] [:firma-id :string]]"
        ok-links
          "[[:betreiber :halle :firma :many-to-one {:project :p :fk {:attribute :firma-id}}]]"]
    (is (empty? (mapcat #(found (halle ok-attrs ok-links) %)
                        [:interface-eigenschaft-fehlt :interface-typ :interface-link-fehlt])))
    (is (= #{["p/halle/object.ent" ":halle"]}
           (found (halle "[[:halle-id :string] [:name :string] [:firma-id :string]]" ok-links)
                  :interface-eigenschaft-fehlt)))
    (is (= #{["p/halle/object.ent" ":lage"]}
           (found (halle "[[:halle-id :string] [:name :string] [:lage :string] [:firma-id :string]]"
                         ok-links) :interface-typ)))
    (is (= #{["p/halle/object.ent" ":halle"]}
           (found (halle ok-attrs "[[:betreiber :halle :firma :one-to-many {:project :p}]]")
                  :interface-link-fehlt))))
  (is
   (= #{["p/i/interface.ent" ":i"] ["p/i/interface.ent" ":j"]}
      (found
       {"p/i/interface.ent"
          "{:schema {:interfaces {:i {:project :p :extends [:j]} :j {:project :p :extends [:i]}}}}"}
       :extends-zyklus))))

(deftest actions
  (let [base {"p/a/object.ent" (obj :a :attributes
                                    [[:a-id :string] [:name :string] [:preis :decimal]])}]
    (is
     (= #{["p/a/actions.ent" ":f"]}
        (found
         (assoc base
                "p/a/actions.ent" "{:schema {:actions {:f {:rules [{:op :apply-edit-function :fn :x} {:op :update :object-type :a :set [:name]}]}}}}")
         :function-rule-allein)))
    (is
     (= #{["p/a/actions.ent" ":preis-setzen"]}
        (found
         (assoc base
                "p/a/actions.ent" "{:schema {:actions {:preis-setzen {:rules [{:op :update :object-type :a :set [:preis]}]}}}}")
         :action-typ)))
    (is
     (= #{["p/a/actions.ent" ":mach"]}
        (found
         (assoc base
                "p/a/actions.ent" "{:schema {:actions {:mach {:params {:menge {:type :short}} :rules [{:op :update :object-type :a :set [:name]}]}}}}")
         :action-typ)))
    (is
     (= #{["p/a/actions.ent" ":mach"]}
        (found
         (assoc base
                "q/project.ent" "{:schema {:projects {:q {}}}}"
                "q/b/object.ent" (obj :b :project :q)
                "p/a/actions.ent" "{:schema {:actions {:mach {:rules [{:op :update :object-type :a :set [:name]} {:op :update :object-type :b :set [:name]}]}}}}")
         :fremd-schreiben)))))

(deftest names
  (is (= #{["p/a/object.ent" ":Kunde"] ["p/a/object.ent" ":Kunde-id"]}
         (found {"p/a/object.ent" (obj :Kunde)} :id-syntax)))
  (is (= #{["p/a/object.ent" ":sap-kunde"]}
         (found {"p/a/object.ent" (obj :sap-kunde)} :system-silo)))
  (is (= #{["p/a/object.ent" ":kunde-v2"]}
         (found {"p/a/object.ent" (obj :kunde-v2)} :time-machine)))
  (is (= #{["p/a/object.ent" ":a"]}
         (found {"p/a/object.ent" (obj :a :status :active)} :beschreibung-fehlt)))
  (is (empty? (found {"p/a/object.ent" (obj :a :status :active :description "Ein A.")}
                     :beschreibung-fehlt))))
