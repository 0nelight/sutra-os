(ns ^:unit apps.object-view-embedding-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.layout :as lay]
            [sutra.capps.variables :as vars]))

(def ^:private row
  {:id "lf1" :object-type-ref :luftfahrzeug :values {:kennzeichen "D-AINA"}})

(def ^:private around
  {:object-type :luftfahrzeug
   :id "lf1"
   :objects {:mangel  [{:id "p1" :values {:mangel-nr "M-1"}}
                         {:id "p2" :values {:mangel-nr "M-2"}}]
             :mechaniker [{:id "l1" :values {:name "Jannik"}}]}})

(deftest the-payload-is-the-objects-neighbourhood
  (let [data (vars/object-view-payload around row)]
    (testing "the linked objects arrive under their own type names"
      (is (= 2 (count (:mangel data))))
      (is (= ["M-1" "M-2"] (mapv (comp :mangel-nr :values) (:mangel data))))
      (is (= 1 (count (:mechaniker data)))))
    (testing "the object itself stands under its own type — `{:from <own type>}` is IT"
      (is (= [row] (:luftfahrzeug data))))))

(deftest the-bound-row-wins-over-what-the-traversal-returned
  (testing "a self-link cannot replace the object with its own neighbours"
    (let [data (vars/object-view-payload
                (assoc-in around [:objects :luftfahrzeug]
                          [{:id "lf9" :values {:kennzeichen "D-AINB"}}])
                row)]
      (is (= [row] (:luftfahrzeug data))
          "the view's own object is the row it was opened for, never a sibling"))))

(deftest an-empty-neighbourhood-is-still-a-payload
  (is (= {:luftfahrzeug [row] :object [row]}
         (vars/object-view-payload {:object-type :luftfahrzeug :objects {}} row))
      "an object with no links reads as itself, not as nothing — also as the view's :object"))

(def ^:private view-vars
  {:jahr      {:default 2026 :external-id :year}
   :status    {:default :all}
   :offene    {:from :mangel :where [[:status := {:var :status}]]}})

(deftest only-an-external-id-is-mappable
  (let [host (atom 2025)
        {:keys [bind problems]} (vars/interface-bindings view-vars {:year host})]
    (is (= {:jahr host} bind)
        "the external id names the view's variable — the internal name is the view's business")
    (is (empty? problems))))

(deftest the-internal-name-is-not-a-door
  (let [host (atom 2025)
        {:keys [bind problems]} (vars/interface-bindings view-vars {:jahr host})]
    (is (empty? bind) "mapping by the INTERNAL name would make every rename a break")
    (is (= 1 (count problems)))
    (is (re-find #":year" (first problems))
        "the refusal names the interface the view actually offers")))

(deftest a-variable-without-an-external-id-is-not-in-the-interface
  (let [{:keys [bind problems]} (vars/interface-bindings view-vars {:status (atom :all)})]
    (is (empty? bind) "`:status` declares no :external-id — it is the view's own state")
    (is (= 1 (count problems)))))

(deftest a-mapped-slot-must-be-a-host-VARIABLE
  (let [{:keys [bind problems]} (vars/interface-bindings view-vars {:year 2026})]
    (is (empty? bind))
    (is (re-find #"\{:var" (first problems))
        "a literal cannot back a mapped variable")))

(deftest nothing-of-the-host-is-in-scope-without-a-mapping
  (is (= {:bind {} :problems []} (vars/interface-bindings view-vars nil))
      "no mapping, no host variables — that is what makes the view portable"))

(deftest the-mapped-name-is-what-the-view-must-drop
  (testing "the view's own definition of a mapped variable is not used"
    (let [host (atom 2025)
          {:keys [bind]} (vars/interface-bindings view-vars {:year host})
          own   (apply dissoc view-vars (keys bind))]
      (is (nil? (:jahr own)) "no second definition of the same name")
      (is (= #{:status :offene} (set (keys own))) "everything unmapped stays the view's"))))

(deftest the-slot-table-is-the-views-own-api
  (is (= {:year :jahr} (vars/interface-slots view-vars))))

(def ^:private tabbed-view
  {:kind   :pages
   :scenario {:var :fork}
   :select {:var :tab}
   :vars   {:tab {:default :overview} :jahr {:default 2026 :external-id :year}}
   :pages  [{:id :overview :label "Übersicht"
             :body {:widget :markdown :text {:item :kennzeichen}}}
            {:id :maengel :label "Mängel" :body {:widget :object-table :rows {:from :mangel}}}]})

(deftest the-embedding-fills-the-item-and-drops-what-it-owns
  (let [node (lay/object-view-node tabbed-view row {:bound [:jahr]})]
    (is (= "D-AINA" (get-in node [:pages 0 :body :text]))
        "`{:item …}` names THIS object — filled before the view's own scope opens")
    (is (nil? (:scenario node))
        "an embedded view reads the fork its host subtree is on; the rider would
         re-read the MODULE's payload instead of this object's neighbourhood")
    (is (nil? (get-in node [:vars :jahr]))
        "a mapped variable is backed by the host's handle — a second definition would shadow it")
    (is (some? (get-in node [:vars :tab])) "everything unmapped stays the view's")))

(deftest the-item-reaches-the-variable-definitions-too
  (let [view {:kind  :pages
              :vars  {:plaene      {:source :hangarplan
                                    :where  [[:stellplatz := {:item :stellplatz}]]}
                      :plan-breite {:agg [:max :spannweite-m] :of :plaene}}
              :pages [{:body {:widget :markdown :text {:item :kennzeichen}}}]}
        node (lay/object-view-node view (assoc-in row [:values :stellplatz] "hangar-3") {})]
    (is (= [[:stellplatz := "hangar-3"]]
           (get-in node [:vars :plaene :where]))
        "the filter names a value, not a reference the variable layer cannot resolve
         — `resolve-ref` knows only {:from}/{:var}/{:val}")
    (is (= {:agg [:max :spannweite-m] :of :plaene} (get-in node [:vars :plan-breite]))
        "a definition with no `{:item …}` in it is untouched")))

(deftest the-initial-tab-is-the-strip-variables-default
  (let [node (lay/object-view-node tabbed-view row {:tab-var :tab :initial-tab :maengel})]
    (is (= :maengel (get-in node [:vars :tab :default]))
        "a starting value belongs in the variable, so an addressed tab still wins over it"))
  (testing "…and is not written onto a definition that was dropped"
    (let [node (lay/object-view-node tabbed-view row
                                     {:bound [:tab] :tab-var :tab :initial-tab :maengel})]
      (is (nil? (get-in node [:vars :tab]))
          "the widget owns that handle already — it was built WITH the default"))))

(deftest hiding-the-tabs-is-a-runtime-rider-not-an-authored-key
  (is (true? (:hide-strip? (lay/object-view-node tabbed-view row {:hide-tabs? true}))))
  (is (nil? (:hide-strip? (lay/object-view-node tabbed-view row {})))
      "absent unless the embedding asked for it — the file never carries it"))

(def ^:private binding-view
  {:kind  :rows
   :vars  {:offene {:from :mangel :where [[:status := :offen]]}
           :alle   {:source :mechaniker}
           :teil   {:from :offene}}
   :items [{:widget :object-table :rows {:from :arbeitszeit} :cols [{:prop :stunden}]}
           {:widget :markdown :text {:item :kennzeichen}}
           {:widget :kpi :label "Mangel" :value {:val :offene}}]})

(deftest only-a-bound-set-names-a-type
  (let [ts (cat/referenced-object-types binding-view)]
    (is (contains? ts :arbeitszeit) "a leaf's `{:from <type>}`")
    (is (contains? ts :mangel)    "a derived variable's `:from`")
    (is (contains? ts :mechaniker)   "a derived variable's `:source`")
    (is (contains? ts :offene)
        "a `:from` that names another VARIABLE rides along — a name that is no
         object type resolves to none, and guessing which is which here would be
         the one place this could silently drop a real type")
    (is (not (contains? ts :stunden)) "a column's property is not a type")
    (is (not (contains? ts :kennzeichen))      "nor an `{:item …}` binding")
    (is (not (contains? ts :Mangel)) "nor a label")))

(deftest a-view-that-binds-nothing-asks-for-no-neighbourhood
  (is (empty? (cat/referenced-object-types
               {:kind :rows :items [{:widget :markdown :text {:item :kennzeichen}}]}))
      "it renders the object it was opened for — there is nothing to fetch around it"))

(deftest only-a-default-agg-needs-the-payload-at-build-time
  (is (true? (vars/folds-payload-at-build?
              {:vars {:jahr {:default-agg [:max :jahr :mangel]}} :kind :rows}))
      "its STARTING value is a fold over the payload")
  (is (true? (vars/folds-payload-at-build?
              {:kind :rows
               :items [{:kind :section
                        :vars {:jahr {:default-agg [:max :jahr :mangel]}}
                        :items []}]}))
      "a nested sandbox builds its scope when it renders — which is too early if
       the surface painted before the payload landed")
  (is (false? (vars/folds-payload-at-build? binding-view))
      "a literal default and a derived set both survive a payload that arrives late")
  (is (false? (vars/folds-payload-at-build? {:kind :rows :items []}))))

(def ^:private auftraege
  [{:id "a1" :values {:auftrag-nr "WA-1" :art 1}}
   {:id "a2" :values {:auftrag-nr "WA-2" :art 8}}
   {:id "a3" :values {:auftrag-nr "WA-3" :art 1}}])

(defn- resolve-var [specs data nm]
  (vars/resolve-ref {:val nm} {} specs data))

(deftest a-source-set-in-a-view-is-what-the-door-answered
  (let [data (vars/object-view-payload (assoc around :answers {:kand auftraege}) row)]
    (testing "around answers a :source variable under its own name, beside the neighbourhood"
      (is (= 3 (count (:kand data))))
      (is (nil? (:arbeitsauftrag data))
          "no neighbour of that type — `{:linked :arbeitsauftrag}` stays empty"))
    (testing "`{:source}` reads that answer"
      (is (= ["WA-1" "WA-2" "WA-3"]
             (mapv (comp :auftrag-nr :values)
                   (resolve-var {:kand {:source :arbeitsauftrag}} data :kand)))))
    (testing "a derived set `{:from <var> :where …}` over it filters the answer"
      (is (= ["WA-1" "WA-3"]
             (mapv (comp :auftrag-nr :values)
                   (resolve-var {:kand  {:source :arbeitsauftrag}
                                 :offen {:from :kand :where [[:art := 1]]}}
                                data :offen)))))))

(deftest a-source-in-an-app-reads-its-answer
  (testing "an app's door answers the variable under its name too — same answer"
    (let [data {:kand [{:id "a1" :values {:auftrag-nr "WA-1"}}]}]
      (is (= ["WA-1"] (mapv (comp :auftrag-nr :values)
                           (resolve-var {:kand {:source :arbeitsauftrag}} data :kand)))))))

(deftest a-source-the-server-left-out-is-no-neighbourhood
  (testing "without an answer (a type the reader may not read) `{:source}` reads nothing —
            never the same-named neighbours, and it never throws"
    (let [data (vars/object-view-payload around row)]
      (is (= 2 (count (:mangel data))) "the neighbours are there")
      (is (empty? (resolve-var {:pos {:source :mangel}} data :pos))))))

(deftest item-not-of-a-missing-value-is-true
  (testing "a property the object does not carry is absent, so its negation holds —
            `:when {:item-not :pdf}` shows the draft pane until a PDF is filed"
    (is (true?  (:when (lay/resolve-item {:when {:item-not :pdf}} row))))
    (is (false? (:when (lay/resolve-item {:when {:item-not :kennzeichen}} row))))
    (is (true?  (:when (lay/resolve-item {:when {:item-not :x}}
                                         {:id "lf2" :values {:x false}}))))))
