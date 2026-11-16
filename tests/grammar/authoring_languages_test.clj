(ns grammar.authoring-languages-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.authoring.languages :as languages]
            [sutra.parser.sugar :as sugar]))

(def ^:private known-lossy
  #{:yaml})

(def ^:private example-names
  ["data-rights" "farm" "household" "kostenstelle-logic" "teaser-rates"])

(defn- examples []
  (let [parse (get-in languages/languages [:edn :parse])]
    (for [n example-names]
      [n (parse (slurp (io/resource (str "presets/beispiele/" n ".ent"))))])))

(defn- lowered [doc] (sugar/lower-schema (:schema doc)))

(defn- round-trips? [{:keys [parse print]} doc]
  (= (lowered doc) (lowered (parse (print doc)))))

(deftest every-language-round-trips-the-examples
  (let [docs (examples)]
    (doseq [[id lang] languages/languages]
      (testing id
        (if (known-lossy id)
          (is (not-every? #(round-trips? lang (second %)) docs)
              (str id " round-trips now: drop it from known-lossy"))
          (doseq [[n doc] docs]
            (is (round-trips? lang doc) n)))))))
