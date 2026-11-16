(ns ^:unit ontology.function-contract-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.catalog.function-contract :as contract]
            [sutra.catalog.program :as prog]
            [sutra.parser.project-emit :as pemit]))

(deftest the-code-states-types-and-optionality
  (is (= {:ticket {:type :ticket} :note {:type :string :optional true}}
         (prog/declared-params
          {:language :clojure
           :source "(defn f [^Ticket ticket ^:optional ^String note] nil)"})))
  (is (= {:rechnung {:type :string}
          :tage     {:type :integer :optional true}
          :stichtag {:type :date :optional true}
          :pdf      {:type :media}
          :roh      {:type nil}}
         (prog/declared-params
          {:language :python
           :code (str "@function(sources=['x'])\n"
                      "def f(rechnung: String, tage: int = 3,\n"
                      "      stichtag: Optional[Date] = None, pdf: Media, roh):\n"
                      "    return []\n")}))))

(defn- edit [source] {:language :clojure :target-type :t :source source})

(deftest a-new-edit-input-breaks-unless-it-is-optional
  (let [old (contract/signature :edit (edit "(defn f [ticket] nil)"))]
    (is (= ["required input :note was added"]
           (contract/breaks old (contract/signature :edit (edit "(defn f [ticket note] nil)")))))
    (is (empty? (contract/breaks old (contract/signature
                                      :edit (edit "(defn f [ticket ^:optional note] nil)")))))
    (is (= ["optional input :note became required"]
           (contract/breaks
            (contract/signature :edit (edit "(defn f [ticket ^:optional note] nil)"))
            (contract/signature :edit (edit "(defn f [ticket note] nil)")))))))

(deftest a-type-the-source-states-is-compared-without-param-types
  (let [old (contract/signature :edit (edit "(defn f [^String note] nil)"))]
    (is (re-find #"changed type from :string to :integer"
                 (first (contract/breaks
                         old (contract/signature :edit (edit "(defn f [^Integer note] nil)"))))))))

(deftest a-python-query-input-takes-its-type-from-the-annotation
  (let [sig (fn [ann] (contract/signature
                       :query {:language :python
                               :code (str "@function\ndef f(jahr: " ann "):\n    return []\n")
                               :inputs {:jahr {:kind :scalar}}}))]
    (is (= {:jahr :integer} (:params (sig "int"))))
    (is (re-find #"changed type" (first (contract/breaks (sig "int") (sig "str")))))))

(deftest a-wider-number-warns-and-does-not-break
  (let [sig (fn [t] {:params {:n t} :required #{:n}})]
    (is (empty? (contract/breaks (sig :integer) (sig :double))))
    (is (re-find #":integer to :double" (first (contract/widenings (sig :integer) (sig :double)))))
    (is (seq (contract/breaks (sig :double) (sig :integer))) "narrowing is a break")))

(deftest the-declared-output-is-part-of-the-promise
  (let [sig (fn [r] {:params {} :required #{} :returns r})]
    (is (= ["output column :betrag was dropped"]
           (contract/breaks (sig {:betrag :double :name :string}) (sig {:name :string}))))
    (is (re-find #"changed type"
                 (first (contract/breaks (sig {:betrag :double}) (sig {:betrag :string})))))
    (is (= ["the declared output was dropped"]
           (contract/breaks (sig {:betrag :double}) {:params {} :required #{}})))
    (is (empty? (contract/breaks (sig {:betrag :double}) (sig {:betrag :double :neu :string})))
        "a new column breaks nobody")))

(deftest rows-are-held-to-what-returns-declares
  (is (nil? (contract/returns-problem nil [{:a 1}])))
  (is (nil? (contract/returns-problem {:a :integer} [{:a 1} {"a" 2}])))
  (is (nil? (contract/returns-problem {:a :integer} [])))
  (is (re-find #":b" (contract/returns-problem {:a :integer :b :string} [{:a 1}]))))

(defn- edit-world [fn-source action]
  {:functions {:f (assoc (edit fn-source) :function-name :f)}
   :actions   {:a action}})

(def ^:private action-binding-ticket
  {:params {:ticket {:type :t}} :rules [{:op :apply-edit-function :function :f}]})

(deftest a-caller-is-judged-for-what-the-change-newly-asks
  (let [before (edit-world "(defn f [ticket] nil)" action-binding-ticket)
        after  (edit-world "(defn f [ticket note] nil)" action-binding-ticket)
        [c]    (contract/function-changes before after)]
    (is (= [{:consumer [:action :a] :inputs [:note]}] (:unbound c)))
    (is (= [[:action :a]] (:callers c))))
  (testing "an input the caller never bound is no news"
    (let [before (edit-world "(defn f [ticket other] nil)" action-binding-ticket)
          after  (edit-world "(defn f [^String ticket other] nil)" action-binding-ticket)]
      (is (empty? (:unbound (first (contract/function-changes before after)))))))
  (testing "an optional input asks nothing"
    (let [before (edit-world "(defn f [ticket] nil)" action-binding-ticket)
          after  (edit-world "(defn f [ticket ^:optional note] nil)" action-binding-ticket)]
      (is (empty? (:unbound (first (contract/function-changes before after)))))))
  (testing "a pin keeps its release"
    (let [pinned (update-in action-binding-ticket [:rules 0] assoc :version "1.0.0")
          before (edit-world "(defn f [ticket] nil)" pinned)
          after  (edit-world "(defn f [ticket note] nil)" pinned)]
      (is (empty? (:callers (first (contract/function-changes before after)))))))
  (testing "a batched rule hands over one list, not its inputs"
    (let [batched (update-in action-binding-ticket [:rules 0] assoc :batched true)
          before  (edit-world "(defn f [ticket] nil)" batched)
          after   (edit-world "(defn f [ticket note] nil)" batched)]
      (is (empty? (:callers (first (contract/function-changes before after))))))))

(deftest an-app-is-a-caller
  (let [app     {:layout {:vars {:v {:function :q :args {:jahr {:val 1}}}}}}
        query   (fn [inputs] {:function-name :q :language :clojure :code "(fn [_] [])"
                              :inputs inputs})
        world   (fn [inputs] {:transforms {:q (query inputs)} :applications {:tafel app}})
        [c]     (contract/function-changes
                 (world {:jahr {:kind :scalar}})
                 (world {:jahr {:kind :scalar} :monat {:kind :scalar}}))]
    (is (= [{:consumer [:application :tafel] :inputs [:monat]}] (:unbound c)))))

(defn- findings [desired live]
  (->> (install-plan/plan-findings desired live (install/plan-schema desired live {}) {})
       (filter #(#{:function-caller-unbound :function-change-affects-callers
                   :function-input-widened :function-returns-on-edit
                   :action-call-unbound :function-output-undeclared} (:rule %)))))

(defn- live-edit [source]
  {:functions {"ri.w.1" {:function-name :f :target-type :t :language :clojure :source source}}
   :actions   {"ri.a.1" {:action-name :a :params {:ticket {:type :t}}
                         :rules [{:op :apply-edit-function :function :f}]}}})

(defn- desired-edit [source]
  {:functions {:f {:target-type :t :language :clojure :source source}}})

(deftest an-install-refuses-a-function-its-unversioned-caller-cannot-call
  (let [fs (findings (desired-edit "(defn ^{:sutra/function {:edits [:t]}} f [ticket note] nil)")
                     (live-edit "(defn ^{:sutra/function {:edits [:t]}} f [ticket] nil)"))
        f  (first (filter #(= :function-caller-unbound (:rule %)) fs))]
    (is (= :error (:severity f)))
    (is (re-find #"action a binds nothing" (:message f)))))

(deftest an-install-warns-of-a-widening
  (let [fs (findings (desired-edit "(defn ^{:sutra/function {:edits [:t]}} f [^Double ticket] nil)")
                     (live-edit "(defn ^{:sutra/function {:edits [:t]}} f [^Integer ticket] nil)"))]
    (is (= [[:function-input-widened :warn]] (map (juxt :rule :severity) fs)))))

(deftest returns-belongs-on-a-query-function
  (let [src "(defn ^{:sutra/function {:edits [:t]}} f [ticket] nil)"
        fs  (findings (assoc-in (desired-edit src) [:functions :f :returns] {:a :string})
                      (live-edit src))]
    (is (= [:function-returns-on-edit] (map :rule fs)))))

(deftest a-release-keeps-what-its-function-returns
  (let [step (->> (install/plan-schema
                   {:functions {:tally {:language :python :code "def x(): pass"
                                        :version "1.0.0" :returns {:a :integer}}}}
                   {} {})
                  (filter #(= :functions (:phase %)))
                  first)]
    (is (= {:a :integer} (get-in step [:args :releases "1.0.0" :returns])))))

(deftest an-export-carries-what-a-function-returns
  (let [desired {:functions {:tally {:language :clojure :code "(fn [_] [])"
                                     :returns {:a :integer}}}}
        step    (->> (install/plan-schema desired {} {}) (filter #(= :functions (:phase %))) first)
        world   {:transforms {"ri.t.1" (assoc (:args step) :function-name :tally
                                              :transform-name :tally)}}
        spec    (get-in (pemit/export world {:inline? true}) [:ent :schema :functions :tally])]
    (is (= {:a :integer} (:returns spec)))))

(defn- two-input-world [action]
  (edit-world "(defn f [ticket note] nil)" action))

(def ^:private binding-both
  {:params {:ticket {:type :t} :note {:type :string}}
   :rules  [{:op :apply-edit-function :function :f}]})

(deftest an-action-is-judged-for-what-its-change-stops-binding
  (testing "it dropped a parameter the function requires"
    (is (= [{:action :a :function :f :inputs [:note]}]
           (contract/action-regressions (two-input-world binding-both)
                                        (two-input-world action-binding-ticket)))))
  (testing "an input it never bound is no news"
    (is (empty? (contract/action-regressions
                 (two-input-world action-binding-ticket)
                 (two-input-world (assoc action-binding-ticket :description "neu"))))))
  (testing "an unchanged action is not judged"
    (is (empty? (contract/action-regressions (two-input-world action-binding-ticket)
                                             (two-input-world action-binding-ticket)))))
  (testing "a new action answers for all it leaves out"
    (is (= [{:action :a :function :f :inputs [:note]}]
           (contract/action-regressions {:functions (:functions (two-input-world binding-both))}
                                        (two-input-world action-binding-ticket))))))

(deftest a-pinned-call-answers-to-its-release
  (let [release {:language :clojure :source "(defn f [ticket] nil)" :target-type :t}
        world   (fn [action]
                  (assoc-in (two-input-world action) [:functions :f :releases] {"1.0.0" release}))
        pinned  (update-in action-binding-ticket [:rules 0] assoc :version "1.0.0")]
    (is (empty? (contract/action-regressions
                 {:functions (:functions (world pinned))} (world pinned)))
        "the release asks only for :ticket, though the head asks for :note too")))

(defn- query-world [query app?]
  (cond-> {:transforms {:q (merge {:function-name :q :language :clojure
                                    :code "(fn [_] [])"}
                                   query)}}
    app? (assoc :applications {:tafel {:layout {:vars {:v {:function :q}}}}})))

(deftest a-touched-query-function-with-callers-should-declare-its-output
  (let [new-code {:code "(fn [_] [{:a 1}])"}]
    (is (= [{:function :q :callers [[:application :tafel]]}]
           (contract/undeclared-outputs (query-world {} true) (query-world new-code true))))
    (is (empty? (contract/undeclared-outputs (query-world {} true) (query-world {} true)))
        "an untouched function is not nagged on every install")
    (is (empty? (contract/undeclared-outputs (query-world {} false)
                                             (query-world new-code false)))
        "nobody depends on its output yet")
    (is (empty? (contract/undeclared-outputs
                 (query-world {} true)
                 (query-world (assoc new-code :returns {:a :integer}) true))))))

(deftest an-install-refuses-an-action-that-stops-binding
  (let [src  "(defn ^{:sutra/function {:edits [:t]}} f [ticket note] nil)"
        live {:functions {"ri.w.1" {:function-name :f :target-type :t :language :clojure
                                    :source src}}
              :actions   {"ri.a.1" {:action-name :a
                                    :params {:ticket {:type :t} :note {:type :string}}
                                    :rules [{:op :apply-edit-function :function :f}]}}}
        want {:functions {:f {:target-type :t :language :clojure :source src}}
              :actions   {:a {:params {:ticket {:type :t}}
                              :rules [{:op :apply-edit-function :function :f}]}}}
        [f]  (findings want live)]
    (is (= [:action-call-unbound :error] ((juxt :rule :severity) f)))
    (is (re-find #"action a calls function f" (:message f)))))

(deftest an-install-warns-of-an-undeclared-output
  (let [q    {:language :clojure :code "(fn [_] [{:a 1}])"}
        live {:transforms {"ri.t.1" {:transform-name :q :function-name :q :language :clojure
                                     :code "(fn [_] [])"}}}
        app  {:status :published :layout {:kind :rows :functions [:q]
                                          :vars {:v {:function :q}}}}
        fs   (findings {:functions {:q q} :applications {:tafel app}} live)]
    (is (= [[:function-output-undeclared :warn]] (map (juxt :rule :severity) fs)))))
