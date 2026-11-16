(ns ^:unit apps.app-events-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.events :as ce]
            [malli.core :as m]
            [malli.error :as me]
            [sutra.parser.ent-view :as ent-view]))

(def ^:private Event @#'ent-view/Event)

(deftest explain-names-what-it-refuses
  (testing "a mistyped key"
    (let [ps (ce/explain {:do :set :vr {:var :x}})]
      (is (some #(= :vr (:key %)) ps))
      (is (re-find #":vr" (ce/message {:do :set :vr {:var :x}})))))
  (testing "an unknown :do — the message names the verb and the legal ones"
    (let [msg (ce/message {:do :navigate-to :var {:var :s} :to :mangel-detail})]
      (is (re-find #":navigate-to" msg))
      (is (re-find #":navigate" msg))))
  (testing "a required key missing"
    (is (not (ce/sound? {:do :close}))))
  (testing "a bare variable NAME — the silent-no-op shape"
    (is (not (ce/sound? {:do :set :var :gewaehlter-mangel :to 1}))
        "{:var n} is what resolve-scoped turns into a handle; a keyword is not")
    (is (ce/sound? {:do :set :var {:var :gewaehlter-mangel} :to 1})))
  (testing "an unknown value-ref form"
    (is (not (ce/sound? {:do :set :var {:var :a} :to {:roe :mangel-nr}})))
    (is (ce/sound? {:do :set :var {:var :a} :to {:row :mangel-nr}})))
  (testing "an overlay key that is not a key"
    (is (not (ce/sound? {:do :close :overlay "befund-detail"}))))
  (testing "the index names WHICH effect of a list broke"
    (let [msg (ce/message [{:do :open :overlay :befund} {:do :set :vr {:var :x}}])]
      (is (re-find #"#2" msg) (str "expected the second effect named: " msg))))
  (testing "an event that is not an effect at all"
    (is (not (ce/sound? "bei-klick")))))

(deftest the-refresh-events-are-sound
  (is (ce/sound? [{:do :refresh-data} {:do :enable-auto-refresh}
                  {:do :disable-auto-refresh} {:do :recompute :var {:var :summe}}]))
  (is (not (ce/sound? {:do :recompute}))
      "a recompute without a variable would silently do nothing"))

(deftest the-message-names-the-offending-effect
  (let [say (fn [v] (pr-str (me/humanize (m/explain Event v))))]
    (testing "a mistyped key is reported by name"
      (is (re-find #":vr" (say {:do :set :vr {:var :x}}))))
    (testing "an unknown :do names the verb nobody knows"
      (is (re-find #":navigate-to" (say {:do :navigate-to :var {:var :s} :to :mangel-detail}))))
    (testing "a semantic refusal in a LIST says which effect broke the rule"
      (is (re-find #"#2" (say [{:do :open :overlay :befund}
                               {:do :set :var {:var :x} :to {:roe :mangel-nr}}]))))
    (testing "a STRUCTURAL refusal localises by position instead — malli aligns
              those errors with the value's own shape"
      (let [h (me/humanize (m/explain Event [{:do :open :overlay :befund}
                                             {:do :set :vr {:var :x}}]))]
        (is (nil? (first h)) "the sound effect carries no error")
        (is (contains? (second h) :vr) "the offending effect is the second")))))

(deftest the-other-modules-open-beside-the-app
  (testing "Open Explorer: a type with filters, or ids"
    (is (ce/sound? {:do :open-explorer :type :rechnung :where {:status {:val :filter}}}))
    (is (ce/sound? {:do :open-explorer :type :rechnung :ids {:val :auswahl}}))
    (is (not (ce/sound? {:do :open-explorer :type "rechnung"})) "the type is a name")
    (is (not (ce/sound? {:do :open-explorer :where [:status]})) ":where is a map"))
  (testing "Open Notebook document needs the document"
    (is (ce/sound? {:do :open-document :document {:row :beleg}}))
    (is (not (ce/sound? {:do :open-document}))))
  (testing "an object page may open in a new tab"
    (is (ce/sound? {:do :open-object :new-tab? true}))
    (is (nil? (me/humanize (m/explain Event {:do :open-explorer :type :rechnung
                                             :ids {:val :auswahl}}))))))
