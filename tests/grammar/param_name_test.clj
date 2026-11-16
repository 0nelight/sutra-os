(ns ^:unit grammar.param-name-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.parser.lint :as lint]
            [sutra.parser.sugar :as sugar]))

(def ^:private schema
  {:objects {:adresse {:attributes [[:plz :string {:name "PLZ"}]
                                    [:ort :string]]}}})

(deftest ein-vorbelegter-parameter-heisst-wie-die-eigenschaft-die-er-liest
  (let [act {:params {:adresse {:type :adresse :prefill :current-object}
                      :plz     {:default {:from :adresse :property :plz}}
                      :ort     {:default {:property :ort}}}
             :rules  [{:op :invoke-webhook}]}]
    (is (= {:type :string :name "PLZ"} (get (sugar/param-derivations schema act) :plz))
        "Name und Typ der gelesenen Eigenschaft")
    (is (= {:type :string} (get (sugar/param-derivations schema act) :ort))
        "ohne :from liest er das aktuelle Objekt — die Eigenschaft hat keinen Namen")))

(defn- findings [schema]
  ((:scan (some #(when (= :action-param-without-name (:id %)) %) lint/rules)) schema))

(deftest ein-gezeigter-parameter-ohne-namen-ist-ein-befund
  (let [fs (findings {:actions {:a {:action-name :a
                                    :params {:ort     {:type :string}
                                             :plz     {:type :string :name "PLZ"}
                                             :vorgang {:type :string :exposure :hidden}}}}})]
    (is (= ["ort"] (map #(re-find #"(?<=parameter \")[^\"]+" (:message %)) fs))
        "benannt und verborgen sind keine Befunde")))
