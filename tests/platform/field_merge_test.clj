(ns ^:unit platform.field-merge-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.lib :as lib]
            [sutra.governance.merge :as bm]
            [support.merge-orakel :as orakel]))

(def ^:private base
  {:object-types {"o1" {:object-type-name :zeichnung
                        :description      "Zeichnung"
                        :attribute-list   [{:attribute-name "datei" :attribute-type "string"}]}}
   :link-types   {}})

(defn- with [f] (update-in base [:object-types "o1"] f))
(defn- attr [n t] {:attribute-name n :attribute-type t})
(defn- o1 [w] (get-in w [:object-types "o1"]))

(deftest a-clean-merge-replays-mine-over-theirs
  (let [mine   (with #(assoc % :description "Eine CAD-Zeichnung"))
        theirs (with #(assoc % :backing {:dataset "zeichnungen"}))]
    (is (= :clean (:status (orakel/check base mine theirs))))
    (is (= {:object-type-name :zeichnung :description "Eine CAD-Zeichnung"
            :backing {:dataset "zeichnungen"} :attribute-list [(attr "datei" "string")]}
           (o1 (orakel/rebase base mine theirs {}))))))

(deftest attributes-merge-by-name-in-the-targets-order
  (let [mine   (with #(update % :attribute-list conj (attr "stunden" "double")))
        theirs (with #(update % :attribute-list conj (attr "bauteile" "integer")))]
    (is (= :clean (:status (orakel/check base mine theirs))))
    (is (= ["datei" "bauteile" "stunden"]
           (map :attribute-name (:attribute-list (o1 (orakel/rebase base mine theirs {}))))))))

(deftest a-list-whose-names-repeat-is-never-merged-into-one
  (let [mine   (with #(update % :attribute-list conj (attr "datei" "long")))
        theirs (with #(update % :attribute-list conj (attr "x" "string")))]
    (is (= :conflicts (:status (orakel/check base mine theirs))))
    (is (= ["attribute-list"] (:fields (first (:conflicts (orakel/check base mine theirs))))))))

(deftest a-clash-names-its-resource-sides-and-fields
  (let [mine   (with #(assoc % :description "A"))
        theirs (with #(assoc % :description "B"))
        [c :as cs] (:conflicts (orakel/check base mine theirs))]
    (is (= 1 (count cs)))
    (is (= {:class :object-types :key "o1" :fields ["description"]}
           (select-keys c [:class :key :fields])))
    (is (= "A" (:description (:mine c))))
    (is (= "B" (:description (:theirs c)))))
  (testing "one attribute changed differently names that attribute"
    (is (= ["attribute-list/datei"]
           (:fields (first (:conflicts
                            (orakel/check base
                                            (with #(assoc-in % [:attribute-list 0 :attribute-type]
                                                             "integer"))
                                            (with #(assoc-in % [:attribute-list 0 :attribute-type]
                                                             "long")))))))))
  (testing "a resource removed on one side and changed on the other: the side says :removed"
    (let [[c] (:conflicts (orakel/check base (update base :object-types dissoc "o1")
                                          (with #(assoc % :description "B"))))]
      (is (= :removed (:mine c)))
      (is (not (contains? c :fields))))))

(deftest a-number-changed-alike-is-clean-and-differently-a-clash
  (let [b  (assoc-in base [:object-types "o1" :semantic-version] 6)
        at (fn [v d] (-> b (assoc-in [:object-types "o1" :semantic-version] v)
                         (assoc-in [:object-types "o1" :description] d)))]
    (is (= :clean (:status (orakel/check b (at 7 "A") (at 7 "Zeichnung")))))
    (is (= ["semantic-version"]
           (:fields (first (:conflicts (orakel/check b (at 7 "Zeichnung")
                                                       (at 8 "Zeichnung")))))))))

(deftest no-base-is-said-not-guessed
  (is (= :no-base (:status (orakel/check nil base base)))))

(deftest a-resolution-is-read-in-every-form-a-caller-sends
  (let [mine   (with #(assoc % :description "A" :icon "m"))
        theirs (with #(assoc % :description "B" :backing {:dataset "z"}))]
    (doseq [r [{[:object-types "o1"] :mine}
               {[:object-types :o1] :mine}
               {"object-types/o1" "mine"}]]
      (let [w (o1 (orakel/rebase base mine theirs r))]
        (is (= "A" (:description w)) (pr-str r))
        (is (= {:dataset "z"} (:backing w)) "theirs' other change stays")
        (is (= "m" (:icon w)))))
    (is (= "B" (:description (o1 (orakel/rebase base mine theirs {}))))
        "undecided, the target's side stands")))

(deftest field-changes-are-rows-per-element
  (let [before (o1 base)
        after  (-> before (assoc :description "neu")
                   (update :attribute-list conj (attr "stunden" "double")))]
    (is (= [{:field "attribute-list/stunden" :before nil
             :after "{:attribute-name \"stunden\", :attribute-type \"double\"}"}
            {:field "description" :before "\"Zeichnung\"" :after "\"neu\""}]
           (bm/field-changes before after)))
    (is (= ["attribute-list/datei" "description" "object-type-name"]
           (map :field (bm/field-changes nil before)))
        "an added resource: every element is new")))

(deftest a-diff-is-per-section-added-removed-changed
  (let [after (-> base (assoc-in [:object-types "o1" :description] "neu")
                  (assoc-in [:link-types "l1"] {:link-type-name :l}))]
    (is (= {:object-types {:added {} :removed {} :changed {"o1" (o1 after)}}
            :link-types   {:added {"l1" {:link-type-name :l}} :removed {} :changed {}}}
           (lib/diff-paths base after)))
    (is (= {} (lib/diff-paths base base)))))
