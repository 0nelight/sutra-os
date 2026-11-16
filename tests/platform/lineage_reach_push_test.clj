(ns ^:unit platform.lineage-reach-push-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lineage.reach :as reach]))

(def ^:private schema
  {:object-types {"ri.o.1" {:object-type-name :rechnung
                            :attribute-list [{:attribute-name :nr :attribute-type :string
                                              :description "alt"}]}}
   :link-types   {"ri.l.1" {:link-type-name :rechnung-partner :cardinality :many-to-one
                            :from-object-type "ri.o.1"}}
   :actions      {"ri.a.1" {:action-name :erfassen :display-name "Erfassen"
                            :params {:r {:type :rechnung}}
                            :rules [{:op :update :object-type "ri.o.1"}]}}
   :applications {"ri.p.1" {:application-name :kasse
                            :layout {:actions [{:action :erfassen}]}}}})

(deftest a-texts-nodes-are-found-under-their-text-section
  (is (= #{"object-type/rechnung" "link-type/rechnung-partner"}
         (reach/node-ids schema [[:objects "rechnung"] [:links "rechnung-partner"]
                                 [:value-types "eur"]]))))

(deftest an-input-outside-the-checkout-is-remembered
  (let [a (reach/around schema #{"action/erfassen"})]
    (is (= ["object-type/rechnung"] (map first a)))
    (is (= [] (reach/drifted schema a)))))

(deftest only-a-contract-change-is-drift
  (let [pulled (reach/around schema #{"action/erfassen"})
        with   #(assoc-in schema [:object-types "ri.o.1" :attribute-list] %)]
    (is (= [] (reach/drifted (with [{:attribute-name :nr :attribute-type :string
                                     :description "neu"}])
                             pulled))
        "a new description breaks no reader")
    (is (= ["object-type/rechnung"]
           (reach/drifted (with [{:attribute-name :nummer :attribute-type :string}]) pulled)))
    (is (= ["object-type/rechnung"]
           (reach/drifted (update schema :object-types dissoc "ri.o.1") pulled))
        "an input that is gone has drifted too")))

(deftest a-push-reaches-what-its-nodes-feed
  (is (= {:direct ["action/erfassen" "link-type/rechnung-partner"] :further 1}
         (reach/downstream schema #{"object-type/rechnung"}))
      "the app reads the action: one step on"))

(deftest a-contract-change-is-told-from-any-other
  (let [sent {"a.ent" {[:objects "text"]  {:description "1" [:attributes] "1"}
                       [:objects "spalte"] {:description "1" [:attributes :nr] "1"}
                       [:objects "weg"]   {:description "1"}
                       [:links "l"]       {:sutra.parser.checkout/whole "1"}}}
        next {"a.ent" {[:objects "text"]  {:description "2" [:attributes] "1"}
                       [:objects "spalte"] {:description "1" [:attributes :nr] "2"}
                       [:objects "neu"]   {:description "1"}
                       [:links "l"]       {:sutra.parser.checkout/whole "2"}}}]
    (is (some #{[:objects "text"]} (reach/changed-nodes sent next))))
  (let [body  (fn [attrs desc]
                {:objects {:a {:attributes attrs :description desc}
                           :b {:attributes [[:x :string]]}}})
        nodes [[:objects "a"]]]
    (testing "an attribute's description is no contract, its name and type are"
      (is (= [] (reach/contract-nodes (body [[:nr :string {:description "alt"}]] "alt")
                                      (body [[:nr :string {:description "neu"}]] "neu")
                                      nodes)))
      (is (= nodes (reach/contract-nodes (body [[:nr :string]] "") (body [[:nr :long]] "") nodes)))
      (is (= nodes (reach/contract-nodes (body [[:nr :string]] "") (body [[:nummer :string]] "")
                                         nodes))))
    (testing "new and gone nodes count"
      (is (= [] (reach/contract-nodes (body [] "") (body [] "") [[:objects "c"]]))
          "absent on both sides: nothing to tell")
      (is (= nodes (reach/contract-nodes {:objects {}} (body [] "") nodes))))))

(deftest an-edit-functions-signature-is-its-contract
  (let [with #(assoc-in {:functions {"ri.f" {:function-name :f :source %}}}
                        [:functions "ri.f" :function-name] :f)
        fp   #(get (reach/contracts (with %)) "function/f")
        src  "(defn ^{:sutra/function {:edits [:a]}} f [x y] (inc x))"]
    (is (= (fp src) (fp "(defn ^{:sutra/function {:edits [:a]}} f [x y] (dec y))"))
        "a body may change")
    (is (not= (fp src) (fp "(defn ^{:sutra/function {:edits [:a]}} f [x] x)")) "params may not")
    (is (not= (fp src) (fp "(defn ^{:sutra/function {:edits [:b]}} f [x y] x)"))
        "what it edits may not")))

(deftest how-an-input-is-written-moves-apart-from-its-contract
  (let [base {:datasets   {"ri.d.1" {:dataset-name :roh} "ri.d.2" {:dataset-name :ziel}}
              :transforms {"ri.t.1" {:transform-name :schreib :query "SELECT 1" :language :pg-sql
                                     :output {:kind :dataset :dataset "ri.d.1"}}
                           "ri.t.2" {:transform-name :lies :query "SELECT 2" :language :pg-sql
                                     :inputs {:r {:kind :dataset :dataset "ri.d.1"}}
                                     :output {:kind :dataset :dataset "ri.d.2"}}}}
        ids  #{"transform/lies"}
        p    (reach/logic-around base ids)
        now  (assoc-in base [:transforms "ri.t.1" :query] "SELECT 3")]
    (is (= ["dataset/roh"] (map first p)))
    (is (= [] (reach/logic-drifted base p)))
    (is (= ["dataset/roh"] (reach/logic-drifted now p)))
    (is (= [] (reach/drifted now (reach/around base ids))) "its columns have not moved")))

(deftest only-the-nodes-a-push-changed-count
  (let [sent {"a.ent" {[:objects "x"] {:d "1"} [:objects "y"] {:d "1"} [:objects "weg"] {:d "1"}}
              "b.ent" {[:objects "z"] {:d "1"}}}
        next {"a.ent" {[:objects "x"] {:d "2"} [:objects "y"] {:d "1"}}}]
    (is (= #{[:objects "x"] [:objects "weg"]} (set (reach/changed-nodes sent next))))))
