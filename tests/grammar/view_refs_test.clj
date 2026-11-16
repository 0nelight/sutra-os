(ns ^:unit grammar.view-refs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.refs :as refs]
            [sutra.parser.ent :as ent]))

(defn- app [vars & items]
  {:name :probe :layout {:kind :rows :vars vars :items (vec items)}})

(defn- refused [doc parse path]
  (try (parse doc path) nil (catch Exception e (.getMessage e))))

(deftest ein-alter-text-wird-beim-lesen-ausdruecklich
  (testing "App: ein Typname in :from ist der ganze Typ"
    (is (= {:source :rechnung :where [[:offen := true]]}
           (get-in (refs/lift-app-doc (app {:offene {:from :rechnung :where [[:offen := true]]}}))
                   [:layout :vars :offene]))))
  (testing "eine Variable eines ANDEREN Knotens bleibt eine Variable"
    (let [doc {:name :probe
               :layout {:kind :rows :vars {:alle {:source :rechnung}}
                        :items [{:kind :rows :vars {:offene {:from :alle}} :items []}]}}]
      (is (= {:from :alle}
             (get-in (refs/lift-app-doc doc) [:layout :items 0 :vars :offene])))))
  (testing "inline: Variable als {:val}, Typ als {:source}"
    (let [l (refs/lift-app-doc (app {:alle {:source :kunde}}
                                    {:widget :object-table :rows {:from :alle}}
                                    {:widget :combo-input :items {:from :projekt}}))]
      (is (= {:val :alle} (get-in l [:layout :items 0 :rows])))
      (is (= {:source :projekt} (get-in l [:layout :items 1 :items])))))
  (testing "View: eigener Typ ist das Objekt, ein anderer die verlinkten Objekte"
    (let [l (refs/lift-view-doc
             {:object-type :rechnung
              :full {:kind :rows
                     :vars {:pos {:from :rechnungsposition}}
                     :items [{:widget :property-list :object {:from :rechnung}}]}
              :panel-set {:widget :object-table :rows {:from :rechnung}}})]
      (is (= {:linked :rechnungsposition} (get-in l [:full :vars :pos])))
      (is (= {:val :object} (get-in l [:full :items 0 :object])))
      (is (= {:val :objects} (get-in l [:panel-set :rows])))))
  (testing "ein Link-Verweis ist schon ausdruecklich"
    (is (= {:from :link/positionen}
           (get-in (refs/lift-view-doc {:object-type :rechnung
                                        :full {:widget :object-table
                                               :rows {:from :link/positionen}}})
                   [:full :rows])))))

(deftest die-grammatik-nimmt-nur-die-ausdrueckliche-form
  (testing "App"
    (is (nil? (refs/problems (:layout (app {:a {:source :rechnung} :b {:from :a}})) nil)))
    (is (str/includes? (first (refs/problems (:layout (app {:b {:from :rechnung}})) nil))
                       "{:source :rechnung}"))
    (is (str/includes? (first (refs/problems (:layout (app {:b {:linked :rechnung}})) nil))
                       "an app has none"))
    (is (nil? (refs/problems (:layout (app {} {:widget :object-table :rows {:val :weg}})) nil))
        "a name is judged in ONE scope, the lexical one of reference-problems")
    (is (= ["Variable :weg ist nicht deklariert"]
           (map :error (cat/reference-problems
                        (:layout (app {} {:widget :object-table :rows {:val :weg}})))))))
  (testing "View: das eine Objekt zu filtern ergibt nie ein anderes"
    (let [p (refs/problems {:kind :rows
                            :vars {:kinder {:from :object
                                            :where [[:eltern := {:item :id}]]}}
                            :items []}
                           {:object-type :position :form-factor :full})]
      (is (str/includes? (first p) "the ONE object"))))
  (testing ":object ist reserviert"
    (is (str/includes? (first (refs/problems {:kind :rows :vars {:object {:default 1}}
                                              :items []}
                                             {:object-type :t :form-factor :full}))
                       "reserved"))))

(deftest der-parser-hebt-an-und-prueft
  (testing "ein .enta in alter Schreibweise wird gelesen und gespeichert, wie es heute heisst"
    (let [s (ent/parse-enta (app {:offene {:from :rechnung}}
                                 {:widget :object-table :rows {:val :offene}})
                            "p.enta")]
      (is (= {:source :rechnung}
             (get-in s [:schema :applications :probe :layout :vars :offene])))))
  (testing "ein .entv, das sein eigenes Objekt filtert, wird abgewiesen"
    (is (str/includes?
         (str (refused {:object-type :position
                        :full {:kind :rows
                               :vars {:kinder {:from :position
                                               :where [[:eltern := {:item :id}]]}}
                               :items [{:widget :object-table :rows {:val :kinder}}]}}
                       ent/parse-entv "position.entv"))
         "the ONE object"))))

(deftest ein-name-gilt-im-lexikalischen-scope
  (let [layout {:kind  :rows
                :vars  {:a {:source :t}}
                :items [{:kind :rows :vars {:b {:from :a :where [[:x := {:val :c}]]}} :items []}
                        {:kind :tabs :select {:var :b} :tabs []}
                        {:kind :rows :vars {:c {:default 1}} :items []}]}]
    (is (= [[:items 0 :vars :b :where 0 2] [:items 1 :select]]
           (mapv :path (cat/reference-problems layout)))
        "in a declaration and in a :select, as in a widget: a sibling's :vars is not in scope")
    (is (nil? (refs/problems layout nil)) "the tree-wide check of names is gone")))
