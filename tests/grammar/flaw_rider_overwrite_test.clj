(ns ^:unit grammar.flaw-rider-overwrite-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.repos.riders :as riders]
            [sutra.repos.records :as git-rows]
            [sutra.repos.projection :as gp]
            [sutra.repos.checks :as repo-checks]
            [sutra.repos.objects :as g]
            [support.system :as tsys]))

(use-fixtures :each
  #(tsys/with-provided* {:git/dir (constantly (System/getProperty "java.io.tmpdir"))} %))

(def ^:private basis  {:name "a" :on :x})
(def ^:private quelle {:name "a" :on :y})
(def ^:private ziel   {:name "a" :on :z})

(defn- merge-mit-konflikt
  []
  (let [geschrieben (atom [])
        commits     (atom 0)
        ents        {"base" {:automations [basis]}
                     "src"  {:automations [quelle]}
                     "tgt"  {:automations [ziel]}}]
    (with-redefs [git-rows/repo-anchor        (fn [_ _ b] {:tip-sha b})
                  g/merge-base            (fn [_ _ _ _] "base")
                  repo-checks/commit-ent  (fn [_ _ _ sha] (get ents sha))
                  riders/upsert!          (fn [_ k _ r]
                                            (swap! geschrieben conj [:upsert k (:name r) r]))
                  riders/delete!          (fn [_ k _ nm] (swap! geschrieben conj [:delete k nm]))
                  gp/root-texts           (fn [_ _ _] {"ontology.ent" "{}"})
                  gp/commit-files!        (fn [& _] (swap! commits inc) "sha")]
      {:result      (riders/merge! {} nil "repo" "src" "tgt")
       :geschrieben @geschrieben
       :commits     @commits})))

(deftest ein-rider-konflikt-bricht-ab-und-laesst-das-ziel-stehen
  (let [{:keys [result geschrieben commits]} (merge-mit-konflikt)]
    (testing "der Konflikt wird genannt"
      (is (= [[:automations "a"]] (:conflicts result))))
    (testing "keine Zeile des Ziels wird überschrieben"
      (is (empty? geschrieben) (pr-str geschrieben)))
    (testing "kein Git-Commit auf dem Ziel"
      (is (zero? commits))))
  (testing "der reine Dreiwege-Schritt behält im Konflikt den Wert des Ziels"
    (let [d #(riders/declared {:automations %})]
      (is (= ziel (get-in (riders/three-way (d [basis]) (d [quelle]) (d [ziel]))
                          [:merged :automations "a"]))))))
