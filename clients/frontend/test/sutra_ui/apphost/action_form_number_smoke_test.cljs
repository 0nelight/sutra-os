(ns sutra-ui.apphost.action-form-number-smoke-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing async]]
            [reagent.core :as r]
            [reagent.dom.server :as rds]
            [sutra-ui.ui.core :as ui :refer [input-group]]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.format :as fmt]
            [sutra.types.submission :as csub]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.apphost.action-form :as af]))

(defn- expand
  [[_ form-state rt descriptor opts]]
  (@#'af/form-sync! form-state rt descriptor opts)
  (@#'af/action-form* form-state rt descriptor opts))

(def ^:private de (cctx/of {:locale :de}))

(def ^:private eur {:kind :currency :currency "EUR" :decimals 2 :grouping true})

(deftest a-number-reads-in-the-locale
  (testing "currency: two places, grouped"
    (is (= "500,28" (fmt/input-text 500.28 eur de)))
    (is (= "0,00" (fmt/input-text 0 eur de)))
    (is (= "0,00" (fmt/input-text 0.0 eur de)))
    (is (= "1.234,50" (fmt/input-text 1234.5 eur de)))
    (is (= "-1.234,56" (fmt/input-text -1234.56 eur de))))
  (testing "a field never rounds away what it holds"
    (is (= "499,999" (fmt/input-text 499.999 eur de))))
  (testing "no format: the locale's comma, no grouping (a year stays 2026)"
    (is (= "1234,5" (fmt/input-text 1234.5 nil de)))
    (is (= "2026" (fmt/input-text 2026 nil de)))
    (is (= "499,99" (fmt/input-text 499.99 {} de)))))

(deftest typed-text-becomes-a-number
  (is (= 499.99 (fmt/input-number "499,99" {:locale :de})))
  (is (= 1234.56 (fmt/input-number "1.234,56" {:locale :de})))
  (is (= 1234.56 (fmt/input-number "1234.56" {:locale :de})))
  (is (= 1234.56 (fmt/input-number "1234,56" {:locale :de})))
  (is (= 3000000 (fmt/input-number "3.000.000" {:locale :de})))
  (is (nil? (fmt/input-number "zwölf" {:locale :de})))
  (testing "English reads its own display back"
    (is (= 1234.56 (fmt/input-number "1,234.56" {:locale :en})))
    (is (= 1234.56 (fmt/input-number "1234.56" {:locale :en})))
    (is (= 499.99 (fmt/input-number "499,99" {:locale :en})))))

(deftest the-affix-follows-the-kind
  (is (= {:suffix "€"} (fmt/affixes eur)))
  (is (= {:suffix "kg"} (fmt/affixes {:kind :unit :unit "kg"})))
  (is (= {:suffix "%"} (fmt/affixes {:kind :percentage})))
  (is (nil? (fmt/affixes nil))))

(defn- attrs-of
  [hiccup]
  (some #(when (and (vector? %) (#{:input input-group} (first %))) (second %))
        (tree-seq vector? seq hiccup)))

(defn- field
  [opts]
  (let [h     (r/atom nil)
        inner (inputs/number-input h opts)
        paint #(inner h opts)]
    {:h h :paint paint
     :type! (fn [s] ((:on-change (attrs-of (paint))) #js {:target #js {:value s}}))
     :blur! (fn [] ((:on-blur (attrs-of (paint)))))}))

(deftest the-field-shows-formatted-and-stores-a-number
  (let [{:keys [h paint type! blur!]} (field {:format eur})]
    (reset! h 500.28)
    (is (= "500,28" (:value (attrs-of (paint)))))
    (is (str/includes? (rds/render-to-string (paint)) "capp-numfield__affix\">€<"))
    (type! "1.234,56")
    (is (= 1234.56 @h) "the grouped German text is stored as the number")
    (is (= "1.234,56" (:value (attrs-of (paint)))) "while typing, the text stays")
    (type! "499,99")
    (is (= 499.99 @h))
    (type! "1234.56")
    (is (= 1234.56 @h))
    (blur!)
    (is (= "1.234,56" (:value (attrs-of (paint)))) "left → formatted")
    (type! "")
    (is (nil? @h) "empty → absent, never \"\"")
    (reset! h 0)
    (is (= "0,00" (:value (attrs-of (paint))))))
  (testing "no format, no affix"
    (let [{:keys [h paint]} (field {})]
      (reset! h 1234.5)
      (is (= "1234,5" (:value (attrs-of (paint)))))
      (is (not (str/includes? (rds/render-to-string (paint)) "capp-numfield"))))))

(def ^:private schema
  {:object-types
   {:beleglesung {:object-type-name :beleglesung
                     :attribute-list [{:attribute-name :netto :attribute-type :double
                                       :opts {:format eur}}
                                      {:attribute-name :menge :attribute-type :double}]}
    :rechnung       {:object-type-name :rechnung
                     :attribute-list [{:attribute-name :brutto :attribute-type :decimal
                                       :opts {:value-type :eur-amount}}
                                      {:attribute-name :netto :attribute-type :decimal}
                                      {:attribute-name :anzahl :attribute-type :integer}]}}
   :value-types {:eur-amount {:value-type-name :eur-amount :type :decimal
                              :format {:kind :currency :currency "EUR" :decimals 2}}}})

(def ^:private action
  {:params {:lesung {:type :beleglesung :exposure :hidden}
            :netto  {:type :number :default {:from :lesung :property :netto}}
            :brutto {:type :number :default {:from :lesung :property :brutto}}
            :anzahl {:type :integer}
            :nr     {:type :string :default {:from :lesung :property :netto}}}
   :rules  [{:object-type :rechnung :op :create :set [:netto :brutto]
             :values {:anzahl [:get :anzahl]}}]})

(deftest a-parameter-takes-the-format-of-its-property
  (let [fs (csub/param-formats schema action)]
    (is (= eur (:netto fs)) "prefill source :beleglesung.netto (rule target has none)")
    (is (= "EUR" (:currency (:brutto fs))) "rule target :rechnung.brutto via its value type")
    (is (nil? (:anzahl fs)) "an unformatted target stays plain")
    (is (nil? (:nr fs)) "only numbers are formatted"))
  (testing "the rule target wins over the prefill source"
    (let [a (assoc-in action [:params :netto :default] {:from :lesung :property :menge})
          s (assoc-in schema [:object-types :rechnung :attribute-list 1 :opts]
                      {:format {:kind :unit :unit "kg"}})]
      (is (= "kg" (:unit (:netto (csub/param-formats s a)))))))
  (testing "rule values nested under :then, RID-keyed types"
    (let [s {:object-types {"ri.x.ot.1" {:object-type-name :rechnung
                                         :attribute-list [{:attribute-name :summe
                                                           :opts {:format eur}}]}}}
          a {:params {:betrag {:type :double}}
             :rules [{:op :if :then [{:object-type "ri.x.ot.1" :op :update
                                      :values {:summe [:get :betrag]}}]}]}]
      (is (= eur (:betrag (csub/param-formats s a))))))
  (is (= eur (get-in (csub/with-param-formats {:netto {:type :number}} schema action)
                     [:netto :format]))))

(deftest the-form-shows-the-currency-and-still-marks-edits
  (let [specs {:netto {:type :number :name "Netto" :format eur
                       :default {:property :lesung-netto}}}
        obj   {:id "d1" :values {:lesung-netto 500.28}}
        opts  {:action :x :specs specs :object obj}
        inner (af/action-form {} opts)
        now   (atom nil)
        paint #(reset! now (expand (inner {} opts)))
        cell  (fn [] (some #(when (instance? af/StagedCell %) %) (tree-seq coll? seq @now)))
        html  #(rds/render-to-string (paint))
        marks #(count (re-seq #"capp-actionform__edited" %))]
    (let [h (html)]
      (is (str/includes? h "value=\"500,28\"") h)
      (is (str/includes? h ">€<"))
      (is (zero? (marks h))))
    (paint)
    (reset! (cell) 499.99)
    (let [h (html)]
      (is (= 1 (marks h)))
      (is (str/includes? h "Vorbelegt: 500,28\u00a0€")
          "the original reads as the property shows it"))
    (paint)
    (reset! (cell) 500.28)
    (is (zero? (marks (html))) "the same NUMBER is no edit, however it is written")))

(def ^:private prod-schema
  {:object-types
   {:beleglesung {:object-type-name :beleglesung
                     :id "ri.x.ot.lesung"
                     :attribute-list (vec (for [p [:netto :steuer]]
                                            {:attribute-name p :attribute-type :double
                                             :opts {:format {:currency "EUR" :decimals 2
                                                             :grouping true :kind :currency}}}))}
    :rechnung       {:object-type-name :rechnung
                     :id "ri.x.ot.rechnung"
                     :attribute-list [{:attribute-name :netto :attribute-type :decimal
                                       :opts {:value-type :eur-amount}}
                                      {:attribute-name :steuer :attribute-type :decimal}]}}
   :value-types {:eur-amount {:value-type-name :eur-amount :type :decimal
                              :format {:kind :currency :currency "EUR" :decimals 2
                                       :grouping true}}}})

(def ^:private prod-action
  {:params {:dokument {:type :eingangsdokument :prefill :current-object :required true}
            :lesung   {:type :beleglesung :exposure :hidden :required true}
            :netto    {:type :number :name "Netto" :required true
                       :default {:from :lesung :property :netto}}
            :steuer   {:type :number :name "USt." :default {:from :lesung :property :steuer}}}
   :rules [{:object-type "ri.x.ot.rechnung" :op :create :set [:netto :steuer]}]})

(defn- server-evaluate
  [_action values _search]
  (js/Promise.resolve
   {:params (update-vals (csub/action-param-descriptors (:params prod-action))
                         #(dissoc % :format))
    :values (merge (when (:lesung values) {:netto 500.28 :steuer 0.0})
                   (into {} (filter (comp some? val)) values))}))

(deftest the-object-view-form-keeps-its-currency-after-evaluation
  (async done
         (let [specs (csub/with-param-formats (csub/action-param-descriptors (:params prod-action))
                       prod-schema prod-action)
               wahl  (atom nil)
               opts  (fn [] {:action :rechnung-aus-lesung-erfassen :specs specs
                             :object {:id "dok-1" :values {}} :evaluate server-evaluate
                             :defaults {:dokument "dok-1" :lesung @wahl}})
               inner (af/action-form {} (opts))
               now   (atom nil)
               paint #(reset! now (expand (inner {} (opts))))
               html  #(rds/render-to-string (paint))
               cell  (fn [p] (some #(when (and (vector? %) (= p (second %))
                                               (instance? af/StagedCell (nth % 3 nil)))
                                      (nth % 3))
                                   (tree-seq coll? seq @now)))]
           (is (= "EUR" (get-in specs [:netto :format :currency])) "rule target, named by RID")
           (is (= "EUR" (get-in specs [:steuer :format :currency])) "prefill source")
           (html)
           (reset! wahl "l1")
           (paint)
           (js/setTimeout
            (fn []
              (let [h (html)]
                (is (str/includes? h "value=\"500,28\"") h)
                (is (str/includes? h "value=\"0,00\"") "USt. 0 reads 0,00")
                (is (= 2 (count (re-seq #"capp-numfield__affix\">€<" h)))
                    "both amounts keep their € after the server answered"))
              (paint)
              (reset! (cell :netto) 499.99)
              (js/setTimeout
               (fn []
                 (let [h (html)]
                   (is (= 1 (count (re-seq #"capp-actionform__edited" h))))
                   (is (str/includes? h "Vorbelegt: 500,28 €") h))
                 (done))
               300))
            300))))
