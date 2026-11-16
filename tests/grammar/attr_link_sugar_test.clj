(ns ^:unit grammar.attr-link-sugar-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.sugar :as sugar]))

(def ^:private schema
  {:interfaces
   {:freigabedokument {:link-constraints [{:link-name :nachweise :cardinality :one-to-many
                                :to-object-type :nachweis}
                               {:link-name :aussteller :cardinality :many-to-one
                                :to-interface :organisation}]}
    :organisation {}}
   :objects
   {:wartungsbetrieb     {:implements [:organisation] :attributes [[:betrieb-id :string]]
                          :plural "Wartungsbetriebe"}
    :halterschaft  {:attributes [[:halterschaft-id :string]] :plural "Halterschaften"}
    :freigabeschein {:implements [:freigabedokument]
                       :attributes [[:schein-id :string]
                                    [:halterschaft-id :string
                                     {:description "Für wen die Freigabe gilt."
                                      :link {:to :halterschaft :as :halter
                                             :reverse :freigabescheine}}]
                                    [:aussteller-id :string
                                     {:link {:to :wartungsbetrieb :as :aussteller
                                             :reverse :freigabescheine}}]]
                     :plural "Freigabescheine"}
    :nachweis        {:attributes [[:nachweis-id :string]
                                    [:dokument-id :string
                                     {:link {:to :freigabeschein :as :freigabeschein
                                             :reverse :nachweise}}]]
                      :plural "Nachweise"}}})

(defn- link [lowered nm]
  (some #(when (= nm (first %)) %) (:links lowered)))

(deftest vorwaerts-ohne-constraint
  (let [l (link (sugar/lower-schema schema) :freigabeschein-halter)]
    (is (= [:freigabeschein-halter :freigabeschein :halterschaft :many-to-one]
           (subvec l 0 4)))
    (is (= {:fk {:attribute :halterschaft-id :on-null :skip :on-unresolved :skip}
            :forward-api-name :halter :reverse-api-name :freigabescheine}
           (-> (sugar/lower-attr-links schema) (link :freigabeschein-halter) (nth 4))))))

(deftest vorwaerts-erfuellt-constraint-ueber-interface-ziel
  (let [opts (nth (link (sugar/lower-attr-links schema) :freigabeschein-aussteller) 4)]
    (is (= [{:interface :freigabedokument :constraint :aussteller}] (:satisfies opts)))))

(deftest rueckwaerts-wenn-das-constraint-am-ziel-haengt
  (testing "freigabedokument.nachweise ist one-to-many VOM Schein — der Link dreht sich um"
    (let [l (link (sugar/lower-attr-links schema) :freigabeschein-nachweise)]
      (is (= [:freigabeschein-nachweise :freigabeschein :nachweis :one-to-many]
             (subvec l 0 4)))
      (is (= {:fk {:attribute :dokument-id :on-null :skip :on-unresolved :skip}
              :forward-api-name :nachweise :reverse-api-name :freigabeschein
              :satisfies [{:interface :freigabedokument :constraint :nachweise}]}
             (nth l 4))))))

(deftest das-attribut-verliert-nur-den-link
  (let [attrs (get-in (sugar/lower-attr-links schema) [:objects :freigabeschein :attributes])]
    (is (= [:halterschaft-id :string {:description "Für wen die Freigabe gilt."}] (nth attrs 1)))
    (is (= [:aussteller-id :string] (nth attrs 2)))))

(deftest gelowert-wie-von-hand-geschrieben
  (testing "der Zucker endet im selben :fk-Transform wie ein handgeschriebener Link"
    (let [von-hand (-> schema
                       (update-in [:objects :freigabeschein :attributes] assoc 1
                                  [:halterschaft-id :string
                                   {:description "Für wen die Freigabe gilt."}])
                       (update :links (fnil conj [])
                               [:freigabeschein-halter :freigabeschein :halterschaft :many-to-one
                                {:fk {:attribute :halterschaft-id :on-null :skip
                                      :on-unresolved :skip}
                                 :forward-api-name :halter :reverse-api-name :freigabescheine}]))]
      (is (= (get-in (sugar/lower-schema von-hand) [:transforms :link-freigabeschein-halter])
             (get-in (sugar/lower-schema schema) [:transforms :link-freigabeschein-halter]))))))

(deftest strict-heisst-fail
  (let [s (assoc-in schema [:objects :freigabeschein :attributes 1 2 :link :strict] true)
        fk (:fk (nth (link (sugar/lower-attr-links s) :freigabeschein-halter) 4))]
    (is (= {:attribute :halterschaft-id} fk))))

(deftest eine-beziehung-eine-stelle
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"an einer Stelle"
                        (sugar/lower-attr-links
                         (assoc schema :links [[:freigabeschein-halter :freigabeschein
                                                :halterschaft :many-to-one]])))))
