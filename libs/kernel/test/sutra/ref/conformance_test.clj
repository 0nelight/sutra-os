(ns sutra.ref.conformance-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.ref.expr :as expr]
            [sutra.ref.fixture :as f]
            [sutra.ref.install :as install]
            [sutra.ref.objects :as obj]
            [sutra.ref.run :as run]
            [sutra.ref.sides :as sides]
            [sutra.ref.value :as v]))

(defn- rules [rs & {:keys [B sigma kappa world] :or {B {} kappa {}}}]
  (run/run-rules (or sigma (f/sigma)) kappa (or world f/world) B rs))

(defn- kind [r] (get-in r [:error :kind]))

(deftest s4-resolvers
  (let [B {:k {:id "1" :object-type-ref :t :values {:a 7}} :xs [{:values {:a 1} :id "x"}]}]
    (is (= 7 (v/resolve-binding B [:get :k :a])))
    (is (= "1" (v/resolve-binding B [:get :k :id])) "attr-of falls back to the handle key")
    (is (= [1] (v/resolve-binding B [:pluck :xs :a])))
    (is (= 1 (v/resolve-binding B [:count :xs])))
    (is (= {:a 7 :b {:c [:get :k :a]}} (v/resolve-arg B {:a [:get :k :a] :b {:c [:get :k :a]}}))
        "resolve-arg is one level deep (the nested map stays literal)")
    (is (= {:a 7 :b {:c 7}} (v/resolve-deep (assoc B :$x 7) {:a :$x :b {:c [:get :k :a]}})))))

(deftest d11-unknown-keyword
  (is (= :offen (v/resolve-binding {} :offen)) "value resolution: literal")
  (is (= :unknown-field (try (expr/eval {} :offen) (catch Exception e (:ref/kind (ex-data e)))))
      "expression: ⊥")
  (is (true? (expr/pred {} :anything)) "bare predicate keyword: truthy, not looked up")
  (is (= :unbound (try (v/resolve-binding {} :_b1) (catch Exception e (:ref/kind (ex-data e)))))))

(deftest s5-value-ops
  (is (= 0.5 (expr/eval {} [:div 1 2])) "ratio → double")
  (is (= 2 (expr/eval {} [:div 4 2])))
  (is (= 3 (expr/eval {} [:round 2.5])))
  (is (= 1.23 (expr/eval {} [:round 1.234 2])))
  (is (= :division-by-zero
         (try (expr/eval {:x nil} [:div :x 0]) (catch Exception e (:ref/kind (ex-data e)))))
      "zero guard runs before the nil check")
  (is (= :nil-in-computation
         (try (expr/eval {:x nil} [:+ :x 1]) (catch Exception e (:ref/kind (ex-data e))))))
  (is (true? (expr/eval {:h "  Hallo   Welt "} [:contains? :h "hallo welt"])))
  (is (false? (expr/eval {:h "x"} [:contains? :h ""])) "an empty needle never matches")
  (is (true? (expr/eval {:xs []} [:each= :xs 1])) "∀ over the empty list")
  (is (= :unknown-op (try (expr/eval {} [:each= [] 1]) (catch Exception e (:ref/kind (ex-data e)))))
      "E has no list literals: every vector is an application"))

(deftest d10-three-equalities
  (is (true? (expr/eval {} [:= 1 1.0])) "E := uses numeric ==")
  (is (false? (expr/eval {} [:in 1 1.0])) "E :in uses Clojure =")
  (is (true? (expr/eval {:a nil} [:= :a nil])) "E: nil = nil")
  (is (false? (expr/match? {:values {:a nil}} [:= :a nil])) "P: nil never matches")
  (is (true? (expr/match? {:values {:a 1}} [:= :a 1.0])) "P := numeric =="))

(deftest s5-link-ops
  (let [self {:id "k" :values {}}
        ns   [{:id "a" :values {:v 3 :ok true}} {:id "b" :values {:v 9 :ok false}}
              {:id "c" :values {}}]
        ctx  {:self self :traverse (fn [_ _] ns)}]
    (is (= 3 (expr/eval {} [:connected-count :x] ctx)))
    (is (= 12 (expr/eval {} [:connected-sum :x :v] ctx)) "nil values are skipped")
    (is (= 1 (expr/eval {} [:connected-count :x [:= :ok true]] ctx)) ":where is predicate IR")
    (is (true? (expr/eval {} [:connected? :x [:> :v 5]] ctx)))
    (is (nil? (expr/eval {} [:connected-avg :x :v [:= :ok :nie]] ctx)) "avg of ∅ = nil")
    (is (= :not-one
           (try (expr/eval {} [:connected-one :x :v] ctx)
                (catch Exception e (:ref/kind (ex-data e))))))))

(deftest d12-rule-set-is-literal-and-world-local
  (let [r (rules [{:op :set :path [:actions :neu] :value {:x [:get :p]}}] :B {:p 1})]
    (is (= {:x [:get :p]} (get-in r [:world :actions :neu])) ":value is not resolved")
    (is (empty? (:ops r)) "nothing reaches R — the world edit is not persisted on its own"))
  (is (= :bad-path (kind (rules [{:op :set :path [:nope] :value 1}])))))

(deftest d13-binding-scope
  (testing ":if and :let leak, :branch and :action do not"
    (let [B (:bindings (rules [{:op :if :pred true
                                :then {:op :let :bind :a :value 1
                                       :step {:op :compute :from :xs :expr [:count] :as :n}}}]
                              :B {:xs [1 2]}))]
      (is (= 1 (:a B)))
      (is (= 2 (:n B))))
    (let [B (:bindings (rules [{:op :branch :test {} :pred true
                                :then [{:op :compute :from :xs :expr [:count] :as :n}]}
                               {:op :action
                                :chain [{:op :let :bind :m :value 1 :step {:op :notify}}]}]
                              :B {:xs [1 2]}))]
      (is (not (contains? B :n)))
      (is (not (contains? B :m)))))
  (testing ":for-each leaves the loop variable bound to the last item"
    (let [r (rules [{:op :for-each :collection :xs :as :x :steps [] :collect-ids :ids}]
                   :B {:xs [{:id "a"} {:id "b"} {:values {}}]})]
      (is (= {:values {}} (get-in r [:bindings :x])) "the last iterated item")
      (is (= ["a" "b"] (get-in r [:bindings :ids])) "ids of the ITEMS, not of created objects"))))

(deftest s3-branch-predicate-sees-only-test
  (let [r (rules [{:op :branch :test {:v [:get :p]} :pred [:= :v 1]
                   :then [{:op :notify :channel "c" :payload :p}]}]
                 :B {:p 1})]
    (is (= [:notify] (map :op (:events r)))))
  (is (= :unknown-field
         (kind (rules [{:op :branch :test {} :pred [:= :p 1] :then []}] :B {:p 1})))
      "B is not visible to the :branch predicate"))

(deftest d30-action-chain-substitutes-coll-ops
  (is (= :host-error
         (kind (rules [{:op :action :chain [{:op :compute :from :xs :expr [:count] :as :m}]}]
                      :B {:xs [1 2]})))
      "resolve-deep rewrites :expr [:count] to 0 before the step runs"))

(deftest s3-if-branch-must-be-one-step
  (is (= :unknown-step-op (kind (rules [{:op :if :pred true :then [{:op :notify}]}])))))

(deftest s3-let-has-no-link-context
  (is (= :no-link-context (kind (rules [{:op :let :bind :n :value [:connected-count :x]
                                         :step {:op :notify}}])))))

(deftest s7-create-ids
  (let [r  (rules [{:op :create :object-type f/kunde :values {:nr "42"} :as :k}
                   {:op :create :object-type f/kunde :values {:nr 42}}
                   {:op :create :object-type f/auftrag :values {:titel "x"}}])
        ids (map (comp :id :payload) (:ops r))]
    (is (string? (first ids)) "key-minted ids are strings")
    (is (not= (first ids) (second ids)) "pr-str makes \"42\" and 42 different ids")
    (is (= "u-1" (nth ids 2)) "no single primary key → Σ fresh id")
    (is (= (first ids) (get-in r [:bindings :k :id])))
    (is (= (first ids)
           (-> (rules [{:op :create :object-type f/kunde :values {:nr "42"}}]) :ops first :payload
               :id))
        "deterministic across runs")))

(deftest s7-row-validation
  (is (= :constraint-violation
         (kind (rules [{:op :create :object-type f/kunde :values {:nr "1" :name "zu lang"}}]))))
  (is (= :bad-attributes
         (kind (rules [{:op :create :object-type f/kunde :values {:nr "1" :x 1}}])))))

(deftest s7-update-existence
  (is (= :rule-target-unresolved
         (kind (rules [{:op :update :object-type f/kunde :target "nope" :values {}}]))))
  (is (nil? (kind (rules [{:op :create :object-type f/auftrag :values {:titel "x"} :as :a}
                          {:op :update :object-type f/auftrag :target [:get :a :id]
                           :values {:titel "y"}}])))
      "an id created earlier in the same run counts as existing")
  (let [o {:id "7" :object-type-ref f/kunde :values {:nr "7"}}]
    (is (nil? (kind (rules [{:op :update :target [:get :o] :values {:name "neu"}}] :B {:o o})))
        "a bound handle is trusted without a read")))

(deftest s7-link-ids-enforce-cardinality
  (let [r (rules [{:op :create-link :link-type :kunde-auftrag :from-id "a1" :to-id "k1"}
                  {:op :create-link :link-type :kunde-auftrag :from-id "a1" :to-id "k2"}
                  {:op :create-link :link-type :paar :from-id "x" :to-id "y"}
                  {:op :create-link :link-type :paar :from-id "y" :to-id "x"}])
        [a b c d] (map (comp :id :payload) (:ops r))]
    (is (= a b) "many-to-one: second link from the same source collides")
    (is (= c d) "many-to-many: endpoints sorted")))

(deftest s10-create-or-update
  (is (= [:create-object] (map :op (:ops (rules [{:op :create-or-update :object-type f/auftrag
                                                  :values {:titel "x"}}])))))
  (is (= :no-object-type (kind (rules [{:op :create-or-update :values {}}])))))

(defn- act [action args & {:keys [sigma world]}]
  (run/run-action (or sigma (f/sigma)) {} (f/with-action (or world f/world) :a action) :a args))

(deftest s9-required-and-types
  (is (= :missing-required (kind (act {:params {:nr {:type :string :required true}}} {}))))
  (is (= :missing-required (kind (act {:params {:nr {:type :string :required true}}} {:nr "  "})))
      "blank counts as absent")
  (is (nil? (kind (act {:params {:nr {:type :string :required true :default "x"}}} {})))
      ":default fills it before the check")
  (is (= :missing-required
         (kind (act {:params {:wer {:type :string :required true :prefill :current-user}}} {}
                    :sigma (assoc (f/sigma) :username (constantly nil)))))
      "a :prefill that yields nil does not exempt a required param (oracle finding)")
  (is (nil? (kind (act {:params {:m {:type :integer}
                                 :s {:type :string :required true
                                     :overrides [{:when [:= :m 1] :required false}]}}}
                       {:m 1})))
      "the matching override lifts :required")
  (is (nil? (kind (act {:params {:m {:type :integer :default 1}
                                 :s {:type :string :required true
                                     :overrides [{:when [:= :m 1] :required false}]}}}
                       {})))
      "an override sees the value a default filled, not only what was supplied")
  (is (= :invalid-type (kind (act {:params {:m {:type :integer}}} {:m 2.5}))))
  (is (= 2 (get-in (act {:params {:m {:type :integer}}
                         :rules [{:op :notify :channel "c" :payload :m}]}
                        {:m 2.0})
                   [:events 0 :payload]))
      "integral 2.0 is coerced to the long 2"))

(deftest s9-overrides
  (let [hide {:params {:m {:type :integer}
                       :s {:type :string
                           :overrides [{:when [:= :m 1] :exposure :hidden}]}}
              :rules [{:op :notify :channel "c" :payload {:s :s}}]}
        paid #(get-in (act hide %) [:events 0 :payload :s])]
    (is (= "typed" (paid {:m 2 :s "typed"}))
        "the condition does not hold, so the answer stands")
    (is (nil? (paid {:m 1 :s "typed"}))
        "an override that HIDES a parameter drops the submitted value — a form stops
         drawing it but still sends what was typed, and the rules would write it"))
  (is (= "fed" (get-in (act {:params {:s {:type :string :exposure :hidden}}
                             :rules [{:op :notify :channel "c" :payload {:s :s}}]}
                            {:s "fed"})
                       [:events 0 :payload :s]))
      "a parameter the AUTHOR hid stays passable — those are fed programmatically")
  (is (= :missing-required
         (kind (act {:params {:s {:type :string :required true
                                  :overrides [{:when [:= :unanswered 1] :required false}]}}}
                    {})))
      "an unanswered field makes the condition not hold — fail-closed, not fail-open")
  (is (= :unknown-op
         (kind (act {:params {:m {:type :integer}
                              :s {:type :string :required true
                                  :overrides [{:when [:frobnicate :m] :required false}]}}}
                    {:m 1})))
      "a malformed condition refuses the submission instead of becoming an override that
       silently never fires"))

(deftest s5-field-refs
  (is (= #{:zustand} (expr/field-refs [:= :zustand "behoben"])))
  (is (= #{} (expr/field-refs :zustand))
      "pred: a non-vector predicate is its own truthiness, so the top is no read")
  (is (= #{:a :b} (expr/field-refs [:and [:get :a] [:= [:get :b :attr] 1]]))
      ":get/:prop read their FIRST argument; the second is an attribute name")
  (is (= #{} (expr/field-refs [:connected? :lnk [:= :inner 1]]))
      "a :connected? predicate is judged against a NEIGHBOUR's values, not this V")
  (is (= #{:x :y} (expr/field-refs [:connected-all? :lnk :attr [:x :y]]))
      ":connected-all?'s `needed` is the one connected-* argument this V evaluates, and
       it is a LIST of expressions")
  (is (= #{:bogus} (expr/unknown-ops [:and [:= :a 1] [:bogus :a]]))
      "an op eval would refuse, found before it installs"))

(deftest s9-prefill
  (let [r (act {:params {:t {:type :date :prefill :now} :w {:type :string :prefill :current-user}
                         :regel-jetzt {:type :timestamp :prefill :now}}
                :rules [{:op :notify :channel "c" :payload {:t :t :w :w :j :regel-jetzt}}]}
               {:w "mallory" :regel-jetzt "1999"})]
    (is (= {:t "2026-10-06" :w "alma" :j "2026-10-05T22:30:00Z"} (get-in r [:events 0 :payload]))
        ":date in the zone (Vienna: already the 6th); :current-user and regel-* always overwrite")))

(deftest d21-a-date-list-keeps-its-dates
  (let [r (act {:params {:ds {:type :date :multiple true}}
                :rules [{:op :notify :channel "c" :payload :ds}]}
               {:ds ["2026-01-01" "2026-01-02"]})]
    (is (nil? (kind r)))
    (is (= ["2026-01-01" "2026-01-02"] (get-in r [:events 0 :payload]))
        "normalise-dates reads each date of a :multiple list (it once turned the list to nil)")))

(deftest s9-multiple-is-one-list
  (let [k1  {:id "1" :object-type-ref f/kunde :values {:nr "1"}}
        k2  {:id "2" :object-type-ref f/kunde :values {:nr "2"}}
        sig #(f/sigma {[f/kunde "1"] k1 [f/kunde "2"] k2})
        ops (fn [r op] (filter #(= op (:op %)) (:ops r)))]
    (testing "every rule runs once; a write rule over a list target applies to each element, reading it"
      (let [r (act {:params {:ks {:type :string :multiple true}}
                    :rules [{:op :update :target [:get :ks] :values {:name [:get :ks :nr]}}
                            {:op :notify :channel "c" :payload [:count :ks]}]}
                   {:ks [k1 k2]} :sigma (sig))]
        (is (nil? (kind r)) (pr-str (:error r)))
        (is (= [["1" "1"] ["2" "2"]]
               (map (juxt (comp :id :payload) (comp :name :values :payload))
                    (ops r :update-object))))
        (is (= [2] (map :payload (filter #(= :notify (:op %)) (:events r))))
            "a rule that edits nothing runs once and sees the whole list")))
    (testing "a create creates one object; one per element is an explicit :for-each"
      (let [r (act {:params {:xs {:type :string :multiple true}}
                    :rules [{:op :for-each :collection :xs :as :x
                             :steps [{:op :create :object-type f/auftrag :values {:titel :x}}]}]}
                   {:xs ["a" "b"]})]
        (is (= ["a" "b"] (map (comp :titel :values :payload) (ops r :create-object))))))
    (testing "a link from one object to a list links each"
      (let [r (act {:params {:k {:type :string} :ks {:type :string :multiple true}}
                    :rules [{:op :create-link :link-type :paar :from-id [:get :k :id]
                             :to-id [:get :ks :id]
                             :as :l}
                            {:op :notify :channel "c" :payload [:count :l]}]}
                   {:k k1 :ks [k1 k2]} :sigma (sig))]
        (is (= [["1" "1"] ["1" "2"]]
               (map (juxt (comp :from-id :payload) (comp :to-id :payload)) (ops r :create-link))))
        (is (= [2] (map :payload (filter #(= :notify (:op %)) (:events r))))
            ":as binds the list of links")))
    (testing "an empty list edits nothing, and create-or-update creates"
      (let [r (act {:params {:ks {:type :string :multiple true}}
                    :rules [{:op :delete-object :target [:get :ks]}
                            {:op :create-or-update :target [:get :ks] :object-type f/auftrag
                             :values {:titel "neu"}}]}
                   {:ks []} :sigma (sig))]
        (is (empty? (ops r :delete-object)))
        (is (= ["neu"] (map (comp :titel :values :payload) (ops r :create-object))))))))

(deftest s9-every-multiple-param-is-a-list
  (let [r (act {:params {:as {:type :string :multiple true} :bs {:type :string :multiple true}
                         :cs {:type :string :multiple true}}
                :rules [{:op :notify :channel "c" :payload [:get :as]}
                        {:op :notify :channel "c" :payload [:get :bs]}
                        {:op :notify :channel "c" :payload [:get :cs]}]}
               {:as ["a"] :bs "b"})]
    (is (nil? (kind r)) (pr-str (:error r)))
    (is (= [["a"] ["b"] []] (map :payload (:events r)))
        "one value and none are lists for the second and third :multiple param too")))

(deftest s9-criteria
  (is (= :criterion-failed
         (kind (act {:params {:m {:type :integer}}
                     :submission-criteria [{:test {:m :m} :pred [:> :m 3] :message "zu klein"}]}
                    {:m 1})))))

(deftest d26-side-effect-phase-refuses-edits-and-records-failures
  (testing "an edit in the side-effect phase refuses the whole run: R never gets a write without its event"
    (let [r (act {:rules [{:op :create :object-type f/auftrag :values {:titel "x"}}
                          {:op :action :phase :side-effect
                           :chain [{:op :create :object-type f/auftrag :values {:titel "y"}}]}]}
                 {})]
      (is (= :side-effect-edit (kind r)))
      (is (empty? (:ops r)) "a refused run has no R")))
  (testing "also through an action the side effect calls"
    (let [w (f/with-action f/world :inner
              {:rules [{:op :create :object-type f/auftrag :values {:titel "y"}}]})]
      (is (= :side-effect-edit
             (kind (act {:rules [{:op :action :phase :side-effect
                                  :chain [{:op :call :fn :inner :args {:arg0 {}}}]}]}
                        {} :world w))))))
  (testing "any other failure of a side effect is recorded and the action stands"
    (let [r (act {:rules [{:op :create :object-type f/auftrag :values {:titel "x"}}
                          {:op :action :phase :side-effect
                           :chain [{:op :notify :channel "c" :payload "p"}
                                   {:op :call :fn :nirgends :args {:arg0 {}}}]}]}
                 {})]
      (is (nil? (kind r)))
      (is (= [:create :side-effect-failed] (map :op (:events r)))
          "the failed rule's events are dropped")
      (is (= 1 (count (:ops r)))))))

(deftest d13-callee-sees-caller-bindings
  (let [w (-> f/world
              (f/with-action :inner {:rules [{:op :notify :channel "c" :payload :geheim}]})
              (f/with-action :outer {:params {:geheim {:type :integer}}
                                     :rules [{:op :call :fn :inner :args {:arg0 {}} :as :res}]}))
        r (run/run-action (f/sigma) {} w :outer {:geheim 3})]
    (is (= 3 (get-in r [:events 0 :payload])) "dynamic scope")
    (is (map? (get-in r [:bindings :res])) ":call :as binds the callee's whole binding map")))

(deftest s9-provenance
  (is (= :provenance
         (kind (act {:edits [{:object-type f/kunde}]
                     :rules [{:op :apply-edit-function :function :f}]}
                    {}
                    :sigma
                    (assoc (f/sigma)
                           :expand (fn [_ _ _ _] {:steps [{:op :create :object-type f/auftrag
                                                           :values {:titel "x"}}]})))))))

(deftest s9-propose-mode
  (let [w (f/with-action f/world :a {:rules [{:op :effect :fn :b :args {:arg0 {:x 1}}}
                                             {:op :create :object-type f/auftrag
                                              :values {:titel "x"}}]})]
    (is (= :propose-refused (kind (run/run-action (f/sigma) {:mode :propose} w :a {}))))
    (let [w (f/with-action f/world :a {:rules [{:op :effect :fn :b :args {:arg0 {:x 1}}}]})]
      (is (= [{:action :b :args {:x 1}}]
             (:proposals (run/run-action (f/sigma) {:mode :propose} w :a {})))))))

(deftest s13-tx-vector-is-R-then-E
  (let [r (rules [{:op :notify :channel "c" :payload 1}
                  {:op :create :object-type f/auftrag :values {:titel "x"}}])]
    (is (= [:create-object :notify :create] (map :op (:tx-vector r))))))

(deftest d17-rids-minted-once-carried-by-name
  (let [alt "ri.sutra.main.object-type.alt"
        cx  {:sigma (f/sigma)}
        installed {:object-types {alt {:id alt :object-type-name :rechnung}}}
        N   (install/ensure-rids cx
                                 {:object-types
                                  {:rechnung {:attribute-list []} :neu {:attribute-list []}}}
                                 installed)]
    (is (= :rechnung (get-in N [:object-types alt :object-type-name]))
        "a name the installed world knows keeps its RID")
    (is (= :neu (get-in N [:object-types "ri.sutra.main.object-type.u-1" :object-type-name]))
        "an unknown name gets a fresh RID through Σ")
    (is (= N (install/ensure-rids cx N installed)) "a RID-keyed world mints nothing"))
  (let [r   (rules [{:op :create-object-type :name :rechnung :attributes [[:nr :string]]}
                    {:op :update-object-type :name :rechnung
                     :attributes [[:nr :string] [:betrag :integer]]}
                    {:op :delete-object-type :name :rechnung}
                    {:op :create-object-type :name :rechnung :attributes [[:nr :string]]}]
                   :world {})
        ids (map (comp :id :target) (:events r))]
    (is (nil? (:error r)) (pr-str (:error r)))
    (is (= (first ids) (second ids) (nth ids 2))
        "update and delete address the entity by name and keep its RID")
    (is (not= (first ids) (nth ids 3)) "a re-created name is a new entity with a new RID")))

(deftest s3-events-thread-through-control-flow
  (is (= [{:n 1} {:n 2} {:n 3}]
         (map :payload (:events (rules [{:op :for-each :coll :items :as :ch
                                         :steps [{:op :notify :channel :ch :payload {:n :ch}}]}]
                                       :B {:items [1 2 3]}))))
      ":notify inside :for-each emits one event per item")
  (is (= 1
         (count (:events (rules [{:op :if :pred [:= 1 1]
                                  :then {:op :notify :channel :x :payload {}}}]))))
      "a taken :if branch emits")
  (is (empty? (:events (rules [{:op :if :pred [:= 1 2]
                                :then {:op :notify :channel :x :payload {}}}])))
      "an untaken :if branch emits nothing")
  (let [e (first (:events (rules [{:op :notify :channel :ops :payload {:e 1}}] :kappa
                                 {:actor "dispatcher"})))]
    (is (= ["dispatcher" :ops {:e 1}] [(:actor e) (:channel e) (:payload e)])
        ":notify carries κ's actor, its channel and payload")))

(deftest s7-authorize-before-write
  (let [deny (assoc (f/sigma)
                    :authorize (fn [op _]
                                 (when (= op :create) (throw (ex-info "denied" {:op op})))))
        r    (rules [{:op :create :object-type f/auftrag :values {:titel "X"}}] :sigma deny)]
    (is (= "denied" (get-in r [:error :message])))
    (is (empty? (:ops r)) "Σ :authorize refuses before anything reaches R")))

(def ^:private doc "ri.sutra.main.object-type.doc")

(deftest s7-row-required-and-one-of
  (let [w (assoc-in f/world [:object-types doc]
                    {:object-type-name :doc
                     :attribute-list [{:attribute-name :status :attribute-type :string
                                       :opts {:required true
                                              :constraints {:one-of ["draft" "final"]}}}
                                      {:attribute-name :title :attribute-type :string}]})
        create #(kind (rules [{:op :create :object-type doc :values %}] :world w))
        update #(kind (rules [{:op :create :object-type doc :values {:status "draft"} :as :d}
                              {:op :update :object-type doc :target [:get :d :id]
                               :values {:status %}}]
                             :world w))]
    (is (nil? (create {:status "draft"})))
    (is (= :constraint-violation (create {:status "zzz"})) "a present value off :one-of")
    (is (= :constraint-violation (create {:title "x"})) "a :required value absent on create")
    (is (nil? (update "final")))
    (is (= :constraint-violation (update "zzz")) "an update is held to :one-of too")))

(defn- prepared [W verb tag body]
  (let [cx {:sigma (f/sigma)}]
    (install/prepare cx W (install/build cx verb tag body))))

(def ^:private iface "ri.sutra.main.interface.locatable")
(def ^:private host  "ri.sutra.main.object-type.host")

(deftest s8-prepare-contributes-inherited-properties
  (let [W {:interfaces {iface {:id iface :interface-name :locatable :extends []
                               :property-list
                               [{:attribute-name :location :attribute-type :string}]}}
           :object-types {}}
        attrs (get-in (prepared W :create :object-type {:name :host :attributes [[:name :string]]
                                                        :implements [:locatable]})
                      [:value :attribute-list])]
    (is (= [:name :location] (map :attribute-name attrs))
        "a created type keeps its own columns and gains its interface's")))

(deftest s8-update-keeps-what-it-omits
  (let [W {:interfaces {iface {:id iface :interface-name :locatable :property-list [] :extends []}}
           :object-types {host {:id host :object-type-name :host
                                :attribute-list [{:attribute-name :name :attribute-type :string}]
                                :implements [iface] :symbol "🖥"
                                :survivorship {:strategy :last-write}}}}
        s (prepared W :update :object-type
                    {:name :host :attributes [[:name :string] [:os :string]]})]
    (is (= [host] [(second (:path s))]) "the update targets the existing RID")
    (is (= [:name :os] (map :attribute-name (get-in s [:value :attribute-list])))
        "a stated list replaces")
    (is (= [[iface] "🖥" {:strategy :last-write}]
           ((juxt :implements :symbol :survivorship) (:value s)))
        "omitted fields keep their stored value")
    (is (= "neu"
           (get-in (prepared W :update :object-type {:name :host :attributes [] :symbol "neu"})
                   [:value :symbol]))
        "a supplied field wins")))

(def ^:private sync-rid "ri.sutra.main.transform.sync")

(deftest s8-clear-table
  (let [sync {:transform-name :thing_raw-sync :id sync-rid :primary-key :name
              :input {:kind :datasource :datasource :mro}
              :source {:query "SELECT * FROM T"}
              :output {:kind :dataset :dataset :thing_raw}}
        W    {:transforms {sync-rid sync} :object-types {} :interfaces {}}
        upd  #(:value (prepared W :update :transform (assoc % :name :thing_raw-sync)))
        bind {:input {:kind :datasource :datasource :mro}
              :source {:query "SELECT * FROM T2"}
              :output {:kind :dataset :dataset :thing_raw}}]
    (is (nil? (:primary-key (upd bind))) "a dataset sync re-declared without a key retires it")
    (is (= "SELECT * FROM T2" (get-in (upd bind) [:source :query])))
    (is (= :satz_id (:primary-key (upd (assoc bind :primary-key :satz_id)))) "a named key wins")
    (is (= :name (:primary-key (upd {}))) "a metadata-only update leaves the key alone"))
  (let [rows {:transform-name :thing-rows :id sync-rid :primary-key :pk
              :input {:kind :datasource :datasource :mro}
              :source {:query "SELECT * FROM T"}
              :output {:kind :object-type :object-type :thing}}
        v    (:value (prepared {:transforms {sync-rid rows} :object-types {} :interfaces {}}
                               :update :transform
                               {:name :thing-rows :input {:kind :datasource :datasource :mro}
                                :source {:query "SELECT * FROM T2"}
                                :output {:kind :object-type :object-type :thing}}))]
    (is (= :pk (:primary-key v)) "an object-type backing keeps its key when the update omits it"))
  (let [app "ri.sutra.main.application.kv"
        W   {:applications {app {:application-name :kv :id app :layout {:kind :pages :pages []}
                                 :source "(ns v)" :object-type host}}
             :object-types {host {:object-type-name :host :id host :attribute-list []}}
             :interfaces {}}
        upd #(:value (prepared W :update :application (assoc % :name :kv)))
        lay {:layout {:kind :rows :items []}}]
    (is (= [nil nil {:kind :rows :items []}] ((juxt :source :object-type :layout) (upd lay)))
        "a declared :layout retracts the :source and :object-type it omits")
    (is (= ["(ns w)" host]
           ((juxt :source :object-type) (upd (assoc lay :source "(ns w)" :object-type :host)))))
    (is (= ["(ns v)" host] ((juxt :source :object-type) (upd {:description "x"})))
        "a metadata-only update leaves both alone")))

(deftest s8-with-project-sets-clears-or-keeps
  (let [step {:op :set :path [:applications :v] :value {:application-name :v}}
        v    #(:value (install/with-project step %))]
    (is (= "ri.x" (:repository (v {:repository "ri.x"}))))
    (is (= {:repository nil} (select-keys (v {:repository :none}) [:repository]))
        ":none writes an explicit nil")
    (is (not (contains? (v {:repository nil}) :repository)) "nil is silence")
    (is (= ["ri.p" nil false] [(:project (v {:project "ri.p"})) (:project (v {:project :none}))
                               (contains? (v {:project nil}) :project)]))))

(deftest s7-6-resolve-ref
  (let [W    {:object-types {:jahr {:object-key {:properties [:jahr] :source :primary-key}
                                    :attribute-list
                                    [{:attribute-name :jahr :attribute-type :integer}]}}}
        rows [{:id "2022" :values {:jahr 1999}} {:id "a" :values {:jahr 2022}}
              {:id "b" :values {:jahr 2023}}
              {:id "c" :values {:jahr 2023}}]
        cx   {:sigma {:lookup (fn [_W _T {[k v] :key id :id}]
                                (if id
                                  (filterv #(= id (:id %)) rows)
                                  (vec (take 2 (filter #(= v (get-in % [:values k])) rows)))))}}
        ref  #(obj/resolve-ref cx W :jahr %)]
    (testing "the key, decoded by its property type, wins over an equal id"
      (is (= "a" (:id (:object (ref "2022"))))))
    (testing "two hits by key are ambiguous"
      (is (= :ambiguous (:reason (:problem (ref "2023"))))))
    (testing "text that does not read as the key's type falls back to the id"
      (is (= :unresolved (:reason (:problem (ref "zweitausend")))))
      (is (= "c" (:id (:object (#(obj/resolve-ref cx W :jahr %) "c"))))))))

(deftest s5-4-link-sides
  (let [W {:object-types {:a {:implements [:i]} :b {} :c {}}
           :interfaces   {:i {}}
           :link-types   {:ab {:link-type-name :ab :from-object-type :a :to-object-type :b
                               :forward-api-name :bs :reverse-api-name :as}
                          :cb {:link-type-name :cb :from-object-type :c :to-object-type :b
                               :forward-api-name :bs :reverse-api-name :cs}
                          :ib {:link-type-name :ib :from-object-type :i :to-object-type :b
                               :forward-api-name :viaI}}}]
    (testing "a side api name is resolved on the near type, not anywhere in W"
      (is (= [:ab :to] ((juxt :link :dir) (sides/resolve-side W :a :bs))))
      (is (= [:cb :to] ((juxt :link :dir) (sides/resolve-side W :c :bs)))))
    (testing "sides an implemented interface declares count"
      (is (= [:ib :to] ((juxt :link :dir) (sides/resolve-side W :a :viaI)))))
    (testing "the far end names its own sides"
      (is (= [:ab :from] ((juxt :link :dir) (sides/resolve-side W :b :as)))))
    (testing "no side of that name is ⊥"
      (is (= :unknown-side
             (try (sides/resolve-side W :c :viaI)
                  (catch clojure.lang.ExceptionInfo e (:ref/kind (ex-data e)))))))))

(deftest s7-4-link-ends-exist
  (let [W     (assoc-in f/world [:link-types :auftrag-kunde]
                        {:cardinality :many-to-one :from-object-type f/auftrag
                         :to-object-type f/kunde})
        k1    {:id "k1" :object-type-ref f/kunde :values {:nr "1"}}
        sigma (f/sigma {[f/kunde "k1"] k1})
        link  (fn [from to] {:op :create-link :link-type :auftrag-kunde :from-id from :to-id to})]
    (testing "an end that is no object of its declared type is refused"
      (is (= :link-end-missing (kind (rules [(link "a-nirgends" "k1")] :world W :sigma sigma)))))
    (testing "an end created earlier in the run counts"
      (is (nil? (:error (rules [{:op :create :object-type f/auftrag :id "a1" :values {:titel "x"}}
                                (link "a1" "k1")]
                               :world W :sigma sigma)))))
    (testing "without read access the ends are trusted"
      (is (nil? (:error (rules [(link "a-nirgends" "k-nirgends")]
                               :world W :sigma (dissoc sigma :object))))))
    (testing "a link type W does not declare ends for is not checked"
      (is (nil? (:error (rules [{:op :create-link :link-type :paar :from-id "x" :to-id "y"}] :sigma
                               sigma)))))))

(deftest s2-reads-see-the-runs-own-r-through-under
  (let [seen  (atom [])
        sigma (assoc (f/sigma)
                     :under (fn [R]
                              {:link-ends (fn [_W _L id]
                                            (swap! seen conj (count R))
                                            (some
                                             #(when (and (= :create-link (:op %))
                                                         (= id (get-in % [:payload :id])))
                                                (select-keys (:payload %) [:from-id :to-id]))
                                             R))}))
        r     (rules [{:op :create-link :link-type :paar :id "l1" :from-id "a" :to-id "b"}
                      {:op :delete-link :link-type :paar :target "l1"}]
                     :sigma sigma)]
    (is (nil? (kind r)))
    (is (= [1] @seen) "the read ran under R as it stood: the create before it")
    (is (= {:id "l1" :from-id "a" :to-id "b"} (:payload (second (:ops r))))
        "so the delete carries the ends of a link this run created")))
