(ns ^:unit apps.standard-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.ctx :as cctx]
            [clojure.string :as str]
            [sutra.capps.standard-view :as sv]
            [sutra.capps.table :as tbl]
            [sutra.types.decimal :as d]
            [sutra.capps.variables :as vars]
            [sutra.capps.visibility :as vis]
            [malli.core :as m]
            [sutra.parser.ent-view :as ent-view]))

(def ^:private de (cctx/of {:locale :de}))

(def ^:private display
  (let [e {:nr      {:prop :nr :type :string :name "Nummer" :order "0" :visibility :prominent}
           :kosten  {:prop :kosten :type :decimal :name "Kosten" :order "9"
                     :format {:kind :currency :currency "EUR" :decimals 2}}
           :notiz   {:prop :notiz :type :string :order "10"}
           :intern  {:prop :intern :type :string :order "1" :visibility :hidden}
           :stand   {:prop :stand :type :string :order "2" :visibility :prominent}}]
    (merge e (update-keys e name))))

(def ^:private row
  {:id "wa1" :object-type-ref :arbeitsauftrag
   :values {:nr "WA-1" :kosten 1180.5 :notiz nil :intern "x-9" :stand "offen"}})

(deftest all-properties-in-the-types-order-without-the-hidden-ones
  (let [es (sv/property-entries row display {} de)]
    (is (= [:nr :stand :kosten :notiz] (mapv :prop es))
        "declared order (numerically, though it arrived as strings), each property
         ONCE although the table keys it twice, and the hidden one not at all")
    (is (= "Nummer" (:label (first es))) "the ontology's display name")
    (is (re-find #"1\.180,50" (:text (nth es 2))) "the property's own value format")
    (is (nil? (:text (nth es 3))) "an empty value stays empty — the widget draws the dash")
    (is (= [true true false false] (mapv :prominent? es)))))

(deftest the-data-options
  (testing "Prominent Properties"
    (is (= [:nr :stand] (mapv :prop (sv/property-entries row display {:only-prominent? true} de)))))
  (testing "Properties to Exclude"
    (is (= [:kosten :notiz]
           (mapv :prop (sv/property-entries row display {:exclude [:nr :stand]} de)))))
  (testing "Hide null properties"
    (is (= [:nr :stand :kosten]
           (mapv :prop (sv/property-entries row display {:hide-null? true} de)))))
  (testing "Specific properties: this order, own label and format win"
    (let [es (sv/property-entries row display
                                  {:props [:notiz {:prop :kosten :label "Summe" :fmt :int}]} de)]
      (is (= [:notiz :kosten] (mapv :prop es)))
      (is (= "Summe" (:label (second es))))
      (is (= "1.181" (:text (second es))))))
  (testing "no object, nothing to list"
    (is (nil? (sv/property-entries nil display {} de)))))

(deftest a-page-is-decided-by-the-doors-answer-alone
  (let [own {:kind :rows :items []}
        std {:links [] :actions []}]
    (testing "the type's own view wins, whatever else the answer holds"
      (is (= {:source :own :view own}
             (sv/page-view :t {:object-page {:view own} :standard-view std} {} nil))))
    (testing "settings without a layout are not an own view"
      (is (= :standard
             (:source (sv/page-view :t {:object-page {:app {}} :standard-view std} {} nil)))))
    (testing "no own view: the standard one"
      (is (= :standard (:source (sv/page-view :t {:standard-view std} {} nil)))))
    (testing "no answer yet: nothing to show, and no guess"
      (is (= {:source :none} (sv/page-view :t nil {} nil))))))

(deftest the-standard-view-is-an-ordinary-node-tree
  (let [n (sv/node :arbeitsauftrag
                   {:links [{:api :arbeitskarten :label "Arbeitskarten" :cols [:karten-nr]}
                            {:api :nacharbeit :label "Nacharbeit" :cols [:karten-nr]}]}
                   display)
        [cards rest* pos ers] (:items n)]
    (is (= {:widget :property-cards :object {:val :object} :cards [:nr :stand]}
           cards)
        "prominent properties on top, as Property Cards")
    (is (= :property-list (get-in rest* [:body :widget]))
        "the rest below, in its own section (47215f0d1)")
    (is (= [:nr :stand] (get-in rest* [:body :exclude])) "without the cards' ones")
    (testing "one section per link SIDE, never per far type"
      (is (= "Arbeitskarten" (:title pos)))
      (is (= {:from :link/arbeitskarten} (get-in pos [:body :rows])))
      (is (= {:from :link/nacharbeit} (get-in ers [:body :rows]))))
    (testing "a side without rows gets no heading at all, instead of an empty box"
      (is (= {:from :link/arbeitskarten} (:when pos))
          "the SECTION carries the gate, so a side with nothing on it is not named")
      (is (nil? (get-in pos [:body :items]))
          "and the table is the body itself — no inner :rows wrapper, and no
           '_Nichts verknuepft._' line to stand in for the missing rows"))
    (is (m/validate ((deref #'ent-view/data-node)) n)
        "the generated view passes the same node grammar a hand-written .entv does")))

(deftest a-property-carries-its-conditional-format
  (let [rules [{:when [:= [:get :stand] "storniert"] :color "#999" :icon "x"}
               {:when [:< [:get :kosten] 0] :background "#c62828"}
               {:color "#2e7d32"}]
        e     (assoc (:stand display) :conditional-format rules)]
    (testing "the first rule that holds wins, top to bottom"
      (is (= {:color "#999" :icon "x"}
             (tbl/cond-format rules {:stand "storniert" :kosten -5}))))
    (testing "a rule without :when is the always-true fallback"
      (is (= {:color "#2e7d32"} (tbl/cond-format rules {:stand "offen" :kosten 3}))))
    (testing "a :when the row cannot answer does not hold — it does not throw"
      (is (= {:color "#2e7d32"} (tbl/cond-format rules {:stand "offen"}))))
    (testing "exact decimals compare by value"
      (is (= {:background "#c62828"}
             (tbl/cond-format rules {:stand "offen" :kosten (d/carry "-1.5")}))))
    (testing "prop-look: label and format of the property, and the colour for this row"
      (let [l (tbl/prop-look e :stand {:stand "storniert"})]
        (is (= "stand" (str/lower-case (:label l))))
        (is (= {:color "#999" :icon "x"} (:look l)))))
    (testing "the property list carries the look"
      (let [display' (assoc display :stand e "stand" e)
            es       (sv/property-entries {:values {:stand "storniert"}} display'
                                          {:props [:stand]} de)]
        (is (= {:color "#999" :icon "x"} (:look (first es))))))))

(deftest without-prominent-properties-there-are-no-cards
  (let [n (sv/node :arbeitsauftrag {:links []} (dissoc display :nr "nr" :stand "stand"))]
    (is (not-any? #(= :property-cards (:widget %)) (:items n)))
    (is (= :property-list (get-in (first (:items n)) [:body :widget]))
        "the properties' section comes first")))

(def ^:private info
  {:links   [{:api :arbeitskarten :label "Arbeitskarten" :cols [:karten-nr :menge]}]
   :subtitle :nr
   :actions [{:action :arbeitsauftrag-kommentieren :label "Kommentieren"}
             {:action :arbeitsauftrag-abzeichnen :label "Abzeichnen"
              :property :stand :from ["offen" "in-pruefung"]}
             {:action :arbeitsauftrag-stornieren :label "Stornieren"
              :property :stand :from ["abgezeichnet"]}]})

(defn- section [n title]
  (some #(when (= title (:title %)) %) (:items n)))

(defn- shown?
  [node data]
  (vis/visible? (vars/resolve-scoped node {} {} data)))

(deftest the-head-carries-the-subtitle
  (is (= "WA-1" (sv/subtitle-text row display (:subtitle info) de)))
  (is (re-find #"1\.180,50" (sv/subtitle-text row display :kosten de))
      "formatted the way the property is everywhere else")
  (is (nil? (sv/subtitle-text row display nil de)) "a type without :subtitle has none")
  (is (nil? (sv/subtitle-text row display :notiz de)) "an empty value is no subtitle"))

(deftest the-actions-offered-on-the-type-and-the-lifecycle-gate
  (let [n    (sv/node :arbeitsauftrag info display)
        acts (:items (:body (section n "Aktionen")))
        by   (into {} (map (juxt :title identity)) acts)]
    (is (= ["Kommentieren" "Abzeichnen" "Stornieren"] (mapv :title acts)))
    (is (= {:widget :action-form :action :arbeitsauftrag-abzeichnen :label "Abzeichnen"}
           (:body (by "Abzeichnen")))
        "the object parameter is not bound here — `:on` made it `:prefill :current-object`")
    (is (nil? (:when (by "Kommentieren"))) "an Action that is no transition stands always")
    (testing "a transition stands only in its :from states"
      (let [in (fn [stand] {:arbeitsauftrag [(assoc-in row [:values :stand] stand)]})]
        (is (shown? (by "Abzeichnen") (in "offen")))
        (is (shown? (by "Abzeichnen") (in "in-pruefung")))
        (is (not (shown? (by "Abzeichnen") (in "abgezeichnet"))))
        (is (shown? (by "Stornieren") (in "abgezeichnet")))
        (is (not (shown? (by "Stornieren") (in "offen"))))))
    (testing "the gate survives the JSON trip a stored layout takes"
      (is (not-any? set? (tree-seq coll? seq (:when (by "Abzeichnen"))))
          "no set literal — a vector in its place would read as an operator"))
    (is (m/validate ((deref #'ent-view/data-node)) n))))

(deftest actions-the-host-places-are-not-offered-twice
  (is (nil? (section (sv/node :arbeitsauftrag info display {:actions? false}) "Aktionen"))
      "the standalone page has its Object Actions menu")
  (let [host {:kind :rows :items [{:widget :object-view :object {:from :arbeitsauftrag}}
                                  {:widget :action-button :action :arbeitsauftrag-abzeichnen}]}
        n    (sv/node :arbeitsauftrag info display {:skip (sv/placed-actions host)})]
    (is (= #{:arbeitsauftrag-abzeichnen} (sv/placed-actions host)))
    (is (= ["Kommentieren" "Stornieren"]
           (mapv :title (:items (:body (section n "Aktionen"))))))))

(deftest the-standard-view-carries-no-history
  (let [n (sv/node :arbeitsauftrag info display)]
    (is (nil? (section n "Verlauf")))
    (is (nil? (section n "Zeitachse")))))

(deftest the-neighbourhood-read-lands-under-link-keys
  (is (= {:arbeitsauftrag [{:id "frisch"}]
          :object         [{:id "frisch"}]
          :arbeitskarte   [{:id "p1"}]
          :link/arbeitskarten [{:id "p1"}]}
         (vars/object-view-payload {:object-type "arbeitsauftrag"
                                    :object      {:id "frisch"}
                                    :objects     {:arbeitskarte [{:id "p1"}]}
                                    :links       {:arbeitskarten [{:id "p1"}]}}
                                   {:id "host-row"}))
      "the object the read returned wins over the host's row — the standard view
       reads it whole — and each side's objects sit under their own key"))

(deftest a-final-state-says-no-action-is-possible
  (let [n    (sv/node :arbeitsauftrag
                      {:links []
                       :actions [{:action :abzeichnen :label "Abzeichnen" :property :stand
                                  :from ["offen"]}]}
                      display)
        acts (some #(when (= "Aktionen" (:title %)) %) (:items n))
        hint (peek (get-in acts [:body :items]))]
    (is (= :markdown (:widget hint)))
    (is (= :not (first (:when hint))) "shown exactly when no transition's :from holds"))
  (testing "an Action without a lifecycle is always possible — no hint"
    (let [n    (sv/node :arbeitsauftrag {:links [] :actions [{:action :kommentieren :label "K"}]}
                        display)
          acts (some #(when (= "Aktionen" (:title %)) %) (:items n))]
      (is (not-any? #(= :markdown (:widget %)) (get-in acts [:body :items]))))))
