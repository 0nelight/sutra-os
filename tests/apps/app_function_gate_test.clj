(ns ^:unit apps.app-function-gate-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.cli.projection :as projection]
            [sutra.types.transform-io :as tio]))

(def ^:private live {:object-types {} :transforms {}})

(def ^:private rules
  #{:app-function-not-listed :app-function-unknown
    :app-function-type-outside :app-function-scalar-unbound
    :app-function-arg-unresolved})

(defn- refusals [desired]
  (->> (install-plan/plan-problems desired live (install/plan-schema desired live {}) {})
       (filter #(rules (:rule %)))))

(defn- schema [app]
  {:objects   {:flug    {:attributes [[:nr :string] [:sitze :integer]] :plural "Flüge"}
               :hafen   {:attributes [[:code :string]] :plural "Häfen"}}
   :functions {:grosse-fluege
               {:language :clojure
                :code     "(fn [{:keys [flug minimum]}] (filter #(> (:sitze %) minimum) flug))"
                :inputs   {:flug    {:kind :object-type :object-type :flug}
                           :minimum {:kind :scalar}}}}
   :applications {:tafel app}})

(defn- app [& {:keys [functions object-types args]
               :or   {functions [:grosse-fluege] object-types [:flug]}}]
  {:status :published
   :layout {:kind :rows
            :functions functions
            :object-types object-types
            :vars {:mindest {:default 300}
                   :gross   (cond-> {:function :grosse-fluege}
                              args (assoc :args args))}
            :items []}})

(deftest a-well-wired-function-variable-passes
  (is (empty? (refusals (schema (app :args {:minimum {:val :mindest}}))))))

(deftest a-function-reading-a-type-the-app-does-not-name-is-refused
  (let [[f :as fs] (refusals (schema (app :object-types [:hafen]
                                          :args {:minimum {:val :mindest}})))]
    (is (= [:app-function-type-outside] (map :rule fs)))
    (is (= [:flug] (:types f)))
    (is (= "tafel > layout > vars > gross" (:path f)))
    (is (re-find #"reads a type not in this application" (:message f))))
  (testing ":aggregate-types puts the type in the app's scope, as the door reads it"
    (is (empty? (refusals (schema (-> (app :object-types [:hafen]
                                           :args {:minimum {:val :mindest}})
                                      (assoc-in [:layout :aggregate-types] [:flug]))))))))

(deftest a-scalar-input-the-variable-does-not-bind-is-refused
  (let [[f :as fs] (refusals (schema (app)))]
    (is (= [:app-function-scalar-unbound] (map :rule fs)))
    (is (= [:minimum] (:unbound f)))
    (is (re-find #":args" (:message f)))))

(deftest a-function-the-app-does-not-list-or-that-does-not-exist-is-refused
  (is (= [:app-function-not-listed]
         (map :rule (refusals (schema (app :functions [] :args {:minimum 1}))))))
  (is (= [:app-function-unknown]
         (map :rule (refusals (schema (-> (app :functions [:gibts-nicht])
                                          (assoc-in [:layout :vars :gross :function]
                                                    :gibts-nicht))))))))

(defn- unresolved [desired]
  (->> (refusals desired)
       (filter #(= :app-function-arg-unresolved (:rule %)))
       (mapcat :unresolved)
       (map :form)))

(deftest an-arg-nothing-resolves-is-refused
  (testing "at an app's root, {:item} and an inline spec reach the Function as maps"
    (doseq [form [{:item :sitze} {:object-property :sitze :of :flug} {:foo 1 :bar 2}]]
      (is (= [form] (unresolved (schema (app :args {:minimum form}))))))
    (is (= [{:item :sitze}] (unresolved (schema (app :args {:minimum [1 {:item :sitze}]}))))))
  (testing "the message names the input and the way that works"
    (let [[f] (refusals (schema (app :args {:minimum {:item :sitze}})))]
      (is (re-find #":minimum to \{:item :sitze\}" (:message f)))
      (is (re-find #"object-property" (:message f))))))

(deftest an-arg-the-client-resolves-passes
  (doseq [v [300 "x" true nil [1 2] {:val :mindest} {:var :mindest} [{:val :mindest}]]]
    (is (empty? (unresolved (schema (app :args {:minimum v})))) (pr-str v))))

(defn- in-tree [node]
  (let [a (app :args {:minimum 1})]
    (-> a
        (update-in [:layout :vars] dissoc :gross)
        (assoc-in [:layout :items] [node]))))

(def ^:private gross-item
  {:function :grosse-fluege :args {:minimum {:item :sitze}}})

(deftest item-passes-where-the-renderer-fills-it
  (testing "a loop's body and group head"
    (doseq [k [:body :group-head]]
      (is (empty? (unresolved
                   (schema (in-tree {:kind :loop :over {:val :x}
                                     k {:kind :rows :vars {:gross gross-item} :items []}})))))))
  (testing "an overlay fills {:arg}, not {:item}"
    (let [ov (fn [v] {:kind :overlay :id :o
                      :body {:kind :rows :vars {:gross {:function :grosse-fluege
                                                        :args {:minimum v}}}
                             :items []}})]
      (is (empty? (unresolved (schema (in-tree (ov {:arg :sitze}))))))
      (is (= [{:item :sitze}] (unresolved (schema (in-tree (ov {:item :sitze})))))))))

(deftest an-object-view-fills-item-only-below-its-root
  (let [view (fn [path]
               (-> {:form-factor :full :object-type :flug :status :published
                    :layout {:kind :rows :functions [:grosse-fluege] :object-types [:flug]
                             :items [{:kind :rows :items []}]}}
                   (assoc-in path gross-item)))]
    (is (= [{:item :sitze}] (unresolved (schema (view [:layout :vars :gross])))))
    (is (empty? (unresolved (schema (view [:layout :items 0 :vars :gross])))))))

(deftest a-headline-says-what-the-refusal-is-about
  (is (re-find #"Function call"
               (install-plan/refusal-headline [{:rule :app-function-arg-unresolved}])))
  (is (re-find #"no producer" (install-plan/refusal-headline [{:rule :producer-removed}])))
  (testing "a rule without a headline of its own claims nothing"
    (is (not (re-find #"no producer"
                      (install-plan/refusal-headline [{:rule :gibts-nicht}]))))))

(deftest a-preview-the-server-refuses-says-why-and-fails
  (let [pushed   #'projection/pushed!
        answer   {:preview? true :updated 1 :refused? true
                  :problems {:plan [{:name "v" :error "binds :x to {:item :x}"}]}}
        code     (atom nil)
        printed  (with-out-str (reset! code (pushed {} {:preview? true} "b" answer)))]
    (is (= 1 @code))
    (is (re-find #"✗ preview — the push would be refused" printed))
    (is (re-find #"binds :x to \{:item :x\}" printed)))
  (testing "a preview the server accepts stays a success"
    (let [code (atom nil)
          out  (with-out-str
                 (reset! code (#'projection/pushed! {} {:preview? true} "b"
                                                    {:preview? true :updated 1})))]
      (is (= 0 @code))
      (is (re-find #"◦ preview — nothing staged" out)))))

(deftest the-door-refuses-a-map-for-a-scalar
  (let [f {:inputs {:minimum {:kind :scalar} :filter {:kind :scalar :type :struct}
                    :flug    {:kind :object-type :object-type :flug}}}]
    (is (= [:minimum] (tio/unfit-scalars f {:minimum {:item :sitze}})))
    (is (= [:minimum] (tio/unfit-scalars f {:minimum [{:id 1}]})))
    (is (= [] (tio/unfit-scalars f {:minimum "a" :filter {:a 1} :flug {:b 2}})))
    (is (= [] (tio/unfit-scalars f {})))))

(deftest the-door-and-the-gate-read-one-predicate
  (let [f {:inputs {:flug {:kind :object-type :object-type :flug}
                    :minimum {:kind :scalar} :maximum {:kind "scalar"}}}]
    (is (= [:flug] (tio/object-type-inputs f)))
    (is (= [:maximum :minimum] (tio/unbound-scalars f {})))
    (is (= [:maximum] (tio/unbound-scalars f {:minimum nil}))
        "a bound nil is an argument; only a missing key is unbound")))
