(ns enta.flows-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.capps.derived :as cd]
            [enta.file :as f]
            [enta.main :as main]))

(def ^:private base
  {:name :quoten
   :description "Produktivquote je Auftrag"
   :object-types [:auftrag]
   :layout {:kind :rows :title "Quoten"
            :items [{:widget :object-table :rows {:from :auftrag}
                     :cols [{:prop :nummer}]}]}})

(defn- tmp! []
  (let [f (java.io.File/createTempFile "flows" ".enta")]
    (.deleteOnExit f)
    (spit f (pr-str base))
    (.getPath f)))

(defn- verb [path v & args]
  (let [out  (java.io.StringWriter.)
        opts (@#'main/parse-args args)
        code (binding [*out* out] (@#'main/run v (f/read-enta path) path opts))]
    {:code code :out (str out) :app (f/read-enta path)}))

(deftest quote-from-two-aggregations
  (testing "Overview → Capabilities → Derived properties → new Column math"
    (let [p (tmp!)]
      (verb p "derived-add" "--type" "auftrag" "--name" "produktiv"
            "--over" "zeiten" "--agg" "sum" "--of" "produktiv-std")
      (verb p "derived-add" "--type" "auftrag" "--name" "gesamt"
            "--over" "zeiten" "--agg" "sum" "--of" "std")
      (let [{:keys [code app]} (verb p "derived-add" "--type" "auftrag" "--name" "quote"
                                     "--label" "Produktivquote"
                                     "--op" "div" "--values" "produktiv,gesamt")]
        (is (= 0 code))
        (is (= {:kind :column-math :label "Produktivquote" :expression [:div :produktiv :gesamt]}
               (get-in app [:derived-properties :auftrag :quote]))
            "Produktiv ÷ Gesamt, as the module's own property")
        (testing "and it is MODULE scope — the file carries it, no object type does"
          (is (contains? app :derived-properties))
          (is (nil? (get-in app [:layout :derived-properties]))
              "the file form keeps it at the top level; the door folds it into :layout"))))))

(deftest the-file-stays-installable
  (let [p (tmp!)]
    (verb p "derived-add" "--type" "auftrag" "--name" "n" "--over" "zeiten" "--agg" "count")
    (is (nil? (f/validate (f/read-enta p) p))
        "every write goes through the same door a push does")
    (is (= 0 (:code (verb p "check"))))))

(deftest column-math-may-not-read-column-math
  (let [p (tmp!)]
    (verb p "derived-add" "--type" "auftrag" "--name" "a" "--over" "zeiten" "--agg" "count")
    (verb p "derived-add" "--type" "auftrag" "--name" "q" "--op" "div" "--values" "a,2")
    (let [{:keys [code out app]} (verb p "derived-add" "--type" "auftrag" "--name" "z"
                                       "--op" "*" "--values" "q,100")]
      (is (= 1 code))
      (is (re-find #"keine andere Spaltenrechnung" out))
      (is (nil? (get-in app [:derived-properties :auftrag :z]))
          "a refused flow writes nothing"))))

(deftest column-math-may-not-traverse-a-link
  (let [p (tmp!)
        {:keys [code out]} (verb p "derived-add" "--type" "auftrag" "--name" "x"
                                 "--expr" "[:connected-sum :zeiten :std]")]
    (is (= 1 code))
    (is (re-find #"Aggregation" out) "the message names the kind that CAN traverse")))

(deftest an-unknown-flag-is-refused-not-ignored
  (is (thrown? clojure.lang.ExceptionInfo (@#'main/parse-args ["--typo" "x"]))))

(deftest properties-lists-what-may-be-referenced
  (let [p (tmp!)]
    (verb p "derived-add" "--type" "auftrag" "--name" "a" "--over" "zeiten" "--agg" "count")
    (verb p "derived-add" "--type" "auftrag" "--name" "q" "--op" "div" "--values" "a,2")
    (let [{:keys [out]} (verb p "properties" "--type" "auftrag")]
      (is (re-find #"Aggregationen dieses Moduls" out))
      (is (re-find #"NICHT verwendbar" out)
          "a column math is SHOWN as unavailable rather than silently omitted"))))

(deftest overview-shows-the-capability
  (let [p (tmp!)]
    (verb p "derived-add" "--type" "auftrag" "--name" "a" "--over" "zeiten" "--agg" "count")
    (let [{:keys [out]} (verb p "overview")]
      (is (re-find #"Capabilities" out))
      (is (re-find #"Abgeleitete Eigenschaften" out))
      (is (re-find #"modulweit, nicht in der Ontologie" out)))))

(deftest rename-drags-the-expressions-that-read-it
  (let [p (tmp!)]
    (verb p "derived-add" "--type" "auftrag" "--name" "a" "--over" "zeiten" "--agg" "count")
    (verb p "derived-add" "--type" "auftrag" "--name" "q" "--op" "div" "--values" "a,2")
    (let [{:keys [app]} (verb p "derived-rename" "--type" "auftrag" "--name" "a" "--to" "anzahl")]
      (is (= [:div :anzahl 2] (get-in app [:derived-properties :auftrag :q :expression]))))))

(deftest the-client-and-the-panel-share-one-rule-set
  (testing "every refusal above is sutra.capps.derived's, not this client's"
    (is (= (cd/explain {:auftrag {:a {:kind :aggregation :over :l :agg :count}
                                  :q {:kind :column-math :expression [:div :a 2]}
                                  :z {:kind :column-math :expression [:* :q 2]}}}
                       {:object-types [:auftrag]})
           (cd/explain (f/derived-module
                        (let [p (tmp!)]
                          (verb p "derived-add" "--type" "auftrag" "--name" "a"
                                "--over" "l" "--agg" "count")
                          (verb p "derived-add" "--type" "auftrag" "--name" "q"
                                "--op" "div" "--values" "a,2")
                          (update (f/read-enta p) :derived-properties
                                  cd/put :auftrag :z {:kind :column-math
                                                      :expression [:* :q 2]})))
                       {:object-types [:auftrag]})))))
