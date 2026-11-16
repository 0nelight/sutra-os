(ns ^:unit ontology.interface-link-rules-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]))

(defn- iface [links]
  {:interface-name :geraet
   :property-list [{:attribute-name :inventar-nr :attribute-type :string}]
   :link-constraints links})

(def ^:private constraint
  [{:link-name :stromkreis :to-object-type :kreis :cardinality :one :required true}])

(defn- ot [nm]
  {:object-type-name nm :implements [:geraet]
   :attribute-list [{:attribute-name :inventar-nr :attribute-type :string}]})

(def ^:private base
  {:interfaces   {:geraet (iface constraint)}
   :object-types {:betriebsmittel (ot :betriebsmittel)
                  :computer       (ot :computer)
                  :kreis          {:object-type-name :kreis
                                   :attribute-list [{:attribute-name :nr :attribute-type :string}]
                                   :plural "Kreise"}
                  :schrank        {:object-type-name :schrank :attribute-list []
                                   :plural "Schränke"}}
   :transforms   {}
   :link-types   {}})

(defn- satisfies-stromkreis [& [c]]
  {:satisfies [{:interface :geraet :constraint (or c :stromkreis)}]})

(def ^:private world
  (assoc base :link-types
         {:bm-kreis (merge {:link-type-name :bm-kreis :from-object-type :betriebsmittel
                            :to-object-type :kreis :cardinality :many-to-one}
                           (satisfies-stromkreis))
          :kreis-pc (merge {:link-type-name :kreis-pc :from-object-type :kreis
                            :to-object-type :computer :cardinality :one-to-many}
                           (satisfies-stromkreis))}))

(defn- logged [res]
  (mapv (juxt :op :type #(select-keys (:payload %) [:from-id :to-id])) (:ops res)))

(defn- run
  ([rule bindings] (run rule bindings world))
  ([rule bindings w] (logged (kernel/eval-step rule w bindings {}))))

(defn- fails
  ([rule bindings] (fails rule bindings world))
  ([rule bindings w]
   (try (run rule bindings w) nil
        (catch Exception e (or (ex-message e) (str e))))))

(def ^:private a-bm   {:id "bm1" :object-type-ref :betriebsmittel :values {}})
(def ^:private a-pc   {:id "pc1" :object-type-ref :computer :values {}})
(def ^:private kreis  {:id "k1"  :object-type-ref :kreis :values {}})

(deftest the-concrete-type-of-the-source-chooses-the-link
  (testing "one action, two implementers, two different concrete link types"
    (is (= [[:create-link :bm-kreis {:from-id "bm1" :to-id "k1"}]]
           (run {:op :create-link-of-interface :interface :geraet :constraint :stromkreis
                 :from-id [:get :g] :to-id [:get :k]}
                {:g a-bm :k kreis})))
    (is (= [[:create-link :kreis-pc {:from-id "k1" :to-id "pc1"}]]
           (run {:op :create-link-of-interface :interface :geraet :constraint :stromkreis
                 :from-id [:get :g] :to-id [:get :k]}
                {:g a-pc :k kreis}))
        "the ends follow the realising side's direction, not the rule's wording")))

(deftest a-raw-id-without-a-type-is-refused-with-the-way-out-named
  (let [msg (fails {:op :create-link-of-interface :interface :geraet :constraint :stromkreis
                    :from-id "bm1" :to-id "k1"} {})]
    (is (some? msg))
    (is (re-find #"object or interface" msg)
        "the source end is an interface reference parameter, and the refusal says so"))
  (testing ":object-type names it where the source arrives as a bare id"
    (is (= [[:create-link :bm-kreis {:from-id "bm1" :to-id "k1"}]]
           (run {:op :create-link-of-interface :interface :geraet :constraint :stromkreis
                 :object-type :betriebsmittel :from-id "bm1" :to-id "k1"} {})))))

(deftest two-realisations-refuse-a-create-and-delete-every-one
  (let [w (assoc-in world [:link-types :bm-kreis-alt]
                    (merge {:link-type-name :bm-kreis-alt :from-object-type :betriebsmittel
                            :to-object-type :kreis :cardinality :many-to-one}
                           (satisfies-stromkreis)))
        step (fn [op] {:op op :interface :geraet :constraint :stromkreis
                       :from-id [:get :g] :to-id [:get :k]})]
    (testing "create cannot choose, so it writes nothing"
      (let [msg (fails (step :create-link-of-interface) {:g a-bm :k kreis} w)]
        (is (some? msg))
        (is (re-find #"bm-kreis" msg))
        (is (re-find #"bm-kreis-alt" msg) "both candidates are named")))
    (testing "delete removes all of them"
      (is (= #{:bm-kreis :bm-kreis-alt}
             (set (map second (run (step :delete-link-of-interface) {:g a-bm :k kreis} w))))))))

(deftest an-unrealised-constraint-names-what-to-declare
  (let [msg (fails {:op :create-link-of-interface :interface :geraet :constraint :stromkreis
                    :from-id [:get :g] :to-id [:get :k]}
                   {:g a-bm :k kreis}
                   (assoc base :link-types {}))]
    (is (some? msg))
    (is (re-find #"betriebsmittel" msg) "the type that realises nothing is named")
    (is (re-find #"satisfies" msg) "and so is the declaration that would fix it")))

(deftest a-link-type-named-like-the-constraint-realises-it-without-satisfies
  (is (= [[:create-link :stromkreis {:from-id "bm1" :to-id "k1"}]]
         (run {:op :create-link-of-interface :interface :geraet :constraint :stromkreis
               :from-id [:get :g] :to-id [:get :k]}
              {:g a-bm :k kreis}
              (assoc base :link-types
                     {:stromkreis {:link-type-name :stromkreis
                                   :from-object-type :betriebsmittel
                                   :to-object-type :kreis :cardinality :many-to-one}})))))

(deftest the-membership-guard-still-runs
  (let [msg (fails {:op :create-link-of-interface :interface :geraet :constraint :stromkreis
                    :from-id [:get :s] :to-id [:get :k]}
                   {:s {:id "s1" :object-type-ref :schrank :values {}} :k kreis})]
    (is (some? msg))
    (is (re-find #"schrank" msg))
    (is (and (re-find #"betriebsmittel" msg) (re-find #"computer" msg))
        "the implementers are listed, as with the object rules")))

(deftest an-unknown-constraint-is-refused
  (is (re-find #"realises no link|no :constraint"
               (fails {:op :create-link-of-interface :interface :geraet :constraint :erdung
                       :from-id [:get :g] :to-id [:get :k]}
                      {:g a-bm :k kreis}))))

(deftest a-delete-may-name-the-concrete-link-by-handle
  (is (= [[:delete-link :bm-kreis {:from-id "bm1" :to-id "k1"}]]
         (run {:op :delete-link-of-interface :interface :geraet :constraint :stromkreis
               :target [:get :l]}
              {:l {:link-type-ref :bm-kreis :id "l1" :values {}
                   :from-object-id "bm1" :to-object-id "k1"}}))))
