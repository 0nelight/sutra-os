(ns ^:unit platform.edn-patch-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.repos.edn-patch :as ep]))

(def ^:private app
  ";; Mahnungen -- warum die App so gebaut ist.
{:name  :mahnungen ;; der Griff
 ;; der Titel steht auf der Kachel
 :title \"Alt\"
 :layout {:kind  :rows
          ;; oben die offenen Rechnungen
          :items [{:table :offen} {:form :mahnen}]}
 :weg   1}
")

(defn- means [target] #(= target (edn/read-string %)))

(deftest only-the-differing-nodes-change
  (let [target {:name :mahnungen :title "Neu"
                :layout {:kind :rows :items [{:table :offen} {:form :senden}]}
                :neu {:a [1 2]}}
        out    (ep/patch app target nil (means target))]
    (is (= target (edn/read-string out)))
    (testing "every comment the author wrote beside an unchanged node survives"
      (doseq [c [";; Mahnungen -- warum" ";; der Griff" ";; der Titel steht"
                 ";; oben die offenen Rechnungen"]]
        (is (str/includes? out c) c)))
    (testing "and the author's key order and alignment, too"
      (is (str/includes? out ":name  :mahnungen"))
      (is (< (str/index-of out ":title") (str/index-of out ":layout"))))))

(deftest a-key-the-author-owns-is-not-touched
  (let [text
          "{:schema {:a 1}\n ;; Automationen gehoeren dem Autor\n :automations [{:name \"x\"}]}\n"
        target {:schema {:a 2}}
        out    (ep/patch text target [:schema] #(= {:a 2} (:schema (edn/read-string %))))]
    (is (= {:a 2} (:schema (edn/read-string out))))
    (is (= [{:name "x"}] (:automations (edn/read-string out))))
    (is (str/includes? out ";; Automationen gehoeren dem Autor"))))

(deftest an-edit-the-meaning-does-not-need-is-left-out
  (testing "the author's spelling of the same meaning stays; only the real change lands"
    (let [text   "{:actions [{:action :a}] :title \"x\"}"
          target {:actions [:a] :title "y"}
          same?  (fn [t] (let [m (edn/read-string t)]
                           (and (= "y" (:title m))
                                (= [:a] (mapv #(if (map? %) (:action %) %) (:actions m))))))
          out    (ep/patch text target nil same?)]
      (is (= {:actions [{:action :a}] :title "y"} (edn/read-string out))))))

(deftest no-patch-that-cannot-reach-the-meaning
  (is (nil? (ep/patch "{:a 1}" {:a 2} nil (constantly false)))))

(deftest a-removed-first-key-leaves-a-well-formed-map
  (let [out (ep/patch "{:a 1\n :b 2}\n" {:b 2} nil (means {:b 2}))]
    (is (= {:b 2} (edn/read-string out)))
    (is (str/starts-with? out "{:b 2}"))))
