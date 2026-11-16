(ns sutra.store.conformance
  (:require [clojure.walk]
            [sutra.kit.anomaly :as anomaly]
            [sutra.store.semantics :as sem]
            [sutra.store.core]
            [sutra.store.core.commit :as commit]
            [sutra.store.core.engine :as engine]
            [sutra.store.core.inherited-edits :as inherited-edits]
            [sutra.store.core.kv :as kv]
            [sutra.store.core.log-kind :as log-kind]
            [clojure.test :refer [is testing]]
            [sutra.store.api :as api :refer [object-version]]))

(set! *warn-on-reflection* true)

(defn- stamp-version [op obj] (assoc op :expected-version (object-version obj)))

(def widget "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000c0f1")
(def other  "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000c0f2")
(def keyed  "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000c0f3")
(def owns   "ri.sutra.main.link-type.00000000-0000-0000-0000-00000000c0f4")

(def world
  {:object-types
   {widget {:object-type-name :widget
            :attribute-list [{:attribute-name :name  :attribute-type :string}
                             {:attribute-name :size  :attribute-type :integer}
                             {:attribute-name :owner :attribute-type :string}]}
    other  {:object-type-name :other
            :attribute-list [{:attribute-name :name  :attribute-type :string}
                             {:attribute-name :gross :attribute-type :integer}
                             {:attribute-name :tare  :attribute-type :integer}]}
    keyed  {:object-type-name :keyed
            :object-key {:properties [:code] :source :primary-key}
            :attribute-list [{:attribute-name :code  :attribute-type :string}
                             {:attribute-name :label :attribute-type :string}]}}
   :link-types
   {owns {:link-type-name :owns :cardinality :many-to-many
          :from-object-type widget :to-object-type other
          :forward-api-name :holdings :reverse-api-name :holder}}})

(defn id [n] (format "00000000-0000-0000-0000-%012d" n))

(defn create [T n values]
  {:op :create-object :type T :payload {:id (id n) :values values}})

(def seed
  [(create widget 1 {:name "alpha"    :size 10 :owner "ava"})
   (create widget 2 {:name "alphabet" :size 20 :owner "bob"})
   (create widget 3 {:name "beta"     :size 30 :owner "ava"})
   (create widget 4 {:name "gamma"    :size nil :owner nil})
   (create other  5 {:name "alpha" :gross 6 :tare 2})])

(def ^:private authority {:system? true})

(defn- head-of [db]
  (with-open [s (api/snapshot db {:branch "main"})] (api/basis s)))

(defn- submit
  [db {w :world :keys [tx-vector]}]
  (api/transact! db (cond-> {:basis (head-of db) :actor {:principal "conformance"}}
                      (some? tx-vector) (assoc :ops tx-vector)
                      w (assoc :schema {:authored w :migration []}))))

(defn- rd
  ([db query] (rd db query authority))
  ([db query auth] (with-open [s (api/snapshot db {:branch "main"})] (api/read s auth query))))

(defn- seeded [make]
  (doto (make) (submit {:world world :tx-vector seed})))

(defn- names [rows] (set (map (comp :name :values) rows)))

(defn- by-id [s T n]
  (first (rd s [:objects T [(id n)]])))

(defn- q
  ([s ir] (q s ir authority))
  ([s ir auth] (rd s [:select ir] auth)))

(defn- vals-of [k rows] (mapv (comp k :values) rows))

(defn link [n from to & [markings]]
  {:op :create-link :type owns
   :payload (cond-> {:id (id n) :from-id (id from) :to-id (id to)}
              markings (assoc :markings markings))})

(defn- pairs [links] (set (map (juxt :from-object-id :to-object-id) links)))

(defn- thrown-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn check-world [make]
  (testing "world"
    (let [s (make)
          r (submit s {:world world})]
      (is (= world (rd s [:schema/world] nil)) "the world reads back as it was committed")
      (is (= (set (keys (:object-types world)))
             (set (keys (get-in r [:schema-diff :object-types :added]))))
          ":schema-diff names the classes that moved")
      (testing "a commit without a schema leaves the world where it was"
        (submit s {:tx-vector [(create widget 9 {:name "x"})]})
        (is (= world (rd s [:schema/world] nil)))))))

(defn check-reads [make]
  (testing "reads"
    (let [s (seeded make)]
      (is (= #{"alpha" "alphabet" "beta" "gamma"} (names (rd s [:objects widget] authority))))
      (is (= #{"alpha"} (names (rd s [:objects other] authority)))
          "a type's rows never leak into another type")
      (is (= #{"alpha" "beta"}
             (names (rd s [:objects widget [(id 1) (id 3)]] authority))))
      (is (= [] (rd s [:objects widget []] authority))
          "an empty id coll is the empty set, never \"no filter\"")
      (is (= [] (rd s [:objects widget [(id 5)]] authority))
          "ids of another type stay behind the type bound"))))

(defn check-writes [make]
  (testing "writes"
    (let [s (seeded make)]
      (testing "update merges the given values over the stored ones"
        (submit s {:tx-vector [{:op :update-object :type widget
                                :payload {:id (id 1) :values {:size 11}}}]})
        (is (= {:name "alpha" :size 11 :owner "ava"}
               (select-keys (:values (by-id s widget 1)) [:name :size :owner]))))
      (testing "delete removes the object"
        (submit s {:tx-vector [{:op :delete-object :type widget :payload {:id (id 2)}}]})
        (is (nil? (by-id s widget 2)))))))

(defn check-versions [make]
  (testing "optimistic versions"
    (let [s       (seeded make)
          current (by-id s widget 3)
          update  (fn [values] {:op :update-object :type widget
                                :payload {:id (id 3) :values values}})]
      (testing "a write stamped with the version it read lands"
        (submit s {:tx-vector [(stamp-version (update {:size 31}) current)]})
        (is (= 31 (get-in (by-id s widget 3) [:values :size]))))
      (testing "a stale stamp refuses the whole submit — nothing before it lands"
        (let [d (thrown-data #(submit s
                                      {:tx-vector [(create widget 6 {:name "delta"})
                                                   (stamp-version (update {:size 99}) current)]}))]
          (is (:sutra/stale-object d))
          (is (nil? (by-id s widget 6)) "the create before the stale op did not land")
          (is (= 31 (get-in (by-id s widget 3) [:values :size])))))
      (testing "an attribute left out and one without a value are the same version"
        (submit s {:tx-vector [(create widget 7 {:name "sparse"})]})
        (let [o (by-id s widget 7)
              stamped {:op :update-object :type widget :payload {:id (id 7) :values {:size 7}}}]
          (submit s {:tx-vector [(stamp-version stamped o)]})
          (is (= 7 (get-in (by-id s widget 7) [:values :size])))
          (is (= (api/object-version {:values {:name "x"}})
                 (api/object-version {:values {:name "x" :size nil}}))))))))

(defn check-query [make]
  (testing "query"
    (let [s (seeded make)]
      (is (= #{"alpha" "alphabet"}
             (names (q s {:root widget :where [:starts-with :name "alpha"]}))))
      (is (= #{"alphabet" "beta"} (names (q s {:root widget :where [:>= :size 20]}))))
      (is (= #{"alpha"} (names (q s {:root other}))) ":root scopes to one type")
      (testing ":select narrows :values, not membership"
        (let [rows (q s {:root widget :select [:name]})]
          (is (= 4 (count rows)))
          (is (every? #(= #{:name} (set (keys (:values %)))) rows))))
      (testing "order → offset → limit compose"
        (is (= ["alphabet"]
               (map (comp :name :values)
                    (q s {:root widget :where [:not= :name "gamma"]
                          :order-by :size :sort-dir :asc :offset 1 :limit 1})))))
      (testing ":offset without :limit is refused as an unbounded read"
        (is (anomaly/client-error? (thrown-data #(q s {:root widget :offset 1})))))
      (testing "without :order-by, by id ascending"
        (is (= ["alpha" "alphabet" "beta" "gamma"] (vals-of :name (q s {:root widget})))))
      (testing "nil sorts last under :asc and first under :desc"
        (is (= [10 20 30 nil] (vals-of :size (q s {:root widget :order-by :size}))))
        (is (= [nil 30 20 10]
               (vals-of :size (q s {:root widget :order-by :size :sort-dir :desc})))))
      (testing "an attribute the type does not declare is refused"
        (is (anomaly/client-error? (thrown-data #(q s {:root widget :where [:= :colour "red"]})))))
      (testing "an empty :id-in is the empty set"
        (is (= [] (q s {:root widget :where [:id-in []]}))))
      (testing "a count answers about the whole filtered set"
        (is (= [{:group {} :metrics {:count 2}}]
               (q s
                  {:root widget :where [:starts-with :name "alpha"] :aggregate :count
                   :limit 1})))))))

(defn check-authority [make]
  (testing "authority"
    (let [s      (seeded make)
          secret (assoc-in (create widget 11 {:name "secret" :size 1 :owner "eve"})
                           [:payload :markings] [:secret])
          _      (submit s {:tx-vector [secret]})
          deny   {:denied-markings #{:secret}}]
      (testing "a denied marking hides the row from every read"
        (is (contains? (names (rd s [:objects widget] authority)) "secret"))
        (is (not (contains? (names (rd s [:objects widget] deny)) "secret")))
        (is (= [] (rd s [:objects widget [(id 11)]] deny)))
        (is (= #{"alpha" "alphabet" "beta" "gamma"} (names (q s {:root widget} deny))))
        (is (= [] (q s {:op :static :root widget :ids [(id 11)]} deny))))
      (testing "a marking nobody denies hides nothing, and unmarked rows always show"
        (is (= 5 (count (rd s [:objects widget] {:denied-markings #{:other}})))))
      (testing "adding a marking the row already carries changes nothing"
        (submit s
                {:tx-vector [{:op :update-markings :type widget
                              :payload {:id (id 11) :add [:secret]}}]})
        (is (= #{:secret} (:markings (by-id s widget 11)))))
      (testing "re-classifying the row makes it visible"
        (submit s
                {:tx-vector [{:op :update-markings :type widget
                              :payload {:id (id 11) :remove [:secret]}}]})
        (is (= #{"secret"} (names (rd s [:objects widget [(id 11)]] deny))))
        (is (nil? (:markings (by-id s widget 11))) "the last marking removed leaves none, not #{}"))
      (testing "a hidden object type reads empty, and only that type"
        (let [hide {:hidden-object-types #{widget}}]
          (is (= [] (rd s [:objects widget] hide)))
          (is (= [] (rd s [:objects widget [(id 1)]] hide)))
          (is (= #{"alpha"} (names (rd s [:objects other] hide))))))
      (testing "no authority is refused — reads fail closed"
        (is (thrown? clojure.lang.ExceptionInfo (rd s [:objects widget] nil)))
        (is (thrown? clojure.lang.ExceptionInfo (rd s [:objects widget [(id 1)]] nil)))
        (is (thrown? clojure.lang.ExceptionInfo (q s {:root widget} nil)))
        (is (thrown? clojure.lang.ExceptionInfo (rd s [:links owns] nil))))
      (testing "the world is schema, not rows: it needs no authority"
        (is (= world (rd s [:schema/world] nil)))))))

(defn check-object-keys [make]
  (testing "object keys"
    (let [s   (doto (make) (submit {:world world}))
          k   (fn [n code] (create keyed n {:code code :label (str n)}))
          get (fn [n] (first (rd s [:objects keyed [(id n)]] authority)))]
      (submit s {:tx-vector [(k 20 "A")]})
      (testing "a key another object holds is refused, and nothing of the submit lands"
        (let [d (thrown-data #(submit s {:tx-vector [(k 21 "B") (k 22 "A")]}))]
          (is (= :object-key-taken (:rule d)))
          (is (= 409 (anomaly/status d)))
          (is (anomaly/client-error? d))
          (is (= (id 20) (:taken-by d)))
          (is (= {:code "A"} (:key d)))
          (is (nil? (get 21)))))
      (testing "one submission creating a key twice is refused"
        (is (= :same-submission
               (:taken-by (thrown-data #(submit s {:tx-vector [(k 23 "C") (k 24 "C")]}))))))
      (testing "a key freed in the same submission can be taken"
        (submit s {:tx-vector [{:op :delete-object :type keyed :payload {:id (id 20)}} (k 25 "A")]})
        (is (some? (get 25))))
      (testing "an unset key property is no key"
        (submit s {:tx-vector [(k 26 nil) (k 27 nil)]})
        (is (some? (get 27))))
      (testing "a type without an object key takes duplicates"
        (submit s
                {:tx-vector [(create other 28 {:name "alpha"}) (create other 29 {:name "alpha"})]})
        (is (= 2 (count (rd s [:objects other] authority))))))))

(defn check-links [make]
  (testing "links"
    (let [s    (seeded make)
          deny {:denied-markings #{:secret}}
          hop  (fn [in] {:op :search-around :link owns :dir :to :in in})]
      (submit s {:tx-vector [(create other 6 {:name "omega" :gross 10 :tare 4})
                             (create other 7 {:name "psi" :gross 100 :tare 50})
                             (link 30 1 5) (link 31 1 6) (link 32 3 7 [:secret])]})
      (testing "links read back by link type, as authority sees them"
        (is (= #{[(id 1) (id 5)] [(id 1) (id 6)] [(id 3) (id 7)]}
               (pairs (rd s [:links owns] authority))))
        (is (every? #(= owns (:link-type-ref %)) (rd s [:links owns] authority)))
        (is (= #{[(id 1) (id 5)] [(id 1) (id 6)]} (pairs (rd s [:links owns] deny)))))
      (testing "a count is the number the authority may SEE — an engine that counts links"
        (testing "without reading them may only be asked where no marking is denied"
          (is (= 3 (rd s [:links owns {:count? true}] authority)))
          (is (= 2 (rd s [:links owns {:count? true}] deny)))))
      (testing "an object count is the authority's too, and a kept one is never another's"
        (let [s2 (seeded make)]
          (submit s2 {:tx-vector [{:op :create-object :type other
                                   :payload {:id (id 8) :values {:name "chi" :gross 1 :tare 1}
                                             :markings [:secret]}}]})
          (let [all (rd s2 [:objects/count other] authority)]
            (is (= all (rd s2 [:objects/count other] authority)) "asked twice, answered the same")
            (is (= (dec all) (rd s2 [:objects/count other] deny))
                "a denied marking is counted away — never served from what the admin was told"))))
      (testing ":search-around follows a link side"
        (is (= #{"alpha" "omega"} (names (q s (hop {:op :static :root widget :ids [(id 1)]})))))
        (is (= #{"alpha" "omega"} (names (q s (hop {:op :static :ids [(id 1)]}))))
            "an untyped static set takes the hop's near type")
        (is (= #{"alpha"} (names (q s {:op :search-around :link owns :dir :from
                                       :in {:op :static :root other :ids [(id 6)]}}))))
        (is (= #{"alpha" "omega" "psi"}
               (names (q s {:op :search-around :link :holdings
                            :in {:op :filter :where [:= :owner "ava"]
                                 :in {:op :scan :root widget}}})))
            "a side named by its API name"))
      (testing "a denied edge is no path"
        (is (= #{"alpha" "omega"} (names (q s (hop {:op :scan :root widget}) deny)))))
      (testing "composed sets order, page and subtract"
        (is (= ["alpha"] (vals-of :name (q s {:op :page :limit 1
                                              :in {:op :order :order-by :name
                                                   :in (hop {:op :scan :root widget})}}))))
        (is (= #{"psi"} (names (q s {:op :subtract
                                     :in [{:op :scan :root other}
                                          (hop {:op :static :root widget :ids [(id 1)]})]})))))
      (testing ":with adds a column over a link side that the set filters and orders by"
        (let [n   (fn [& {:as more}] (merge {:op :with :name :n :fn :count :link owns :dir :to
                                             :in {:op :scan :root widget}} more))
              by  (fn [rows]
                    (into {} (map (juxt #(get-in % [:values :name]) #(get-in % [:values :n])))
                          rows))]
          (is (= {"alpha" 2 "alphabet" 0 "beta" 1 "gamma" 0} (by (q s (n)))))
          (is (= #{"alpha" "beta"} (names (q s {:op :filter :where [:> :n 0] :in (n)}))))
          (is (= ["alpha" "beta" "gamma" "alphabet"]
                 (vals-of :name (q s {:op :order :order-by :n :sort-dir :desc :in (n)})))
              "ties on the column break on the id, in the same direction")
          (is (= {"alpha" 1 "alphabet" 0 "beta" 0 "gamma" 0}
                 (by (q s (n :where [:= :name "omega"])))))
          (is (= #{"alpha"} (names (q s {:op :filter :where [:> :n 0] :in (n)} deny)))
              "a denied edge is no neighbour")))
      (testing ":compute is arithmetic over attributes and columns; a nil operand or zero divisor is nil"
        (let [n   {:op :with :name :n :fn :count :link owns :dir :to :in {:op :scan :root widget}}
              c   (fn [e] {:op :compute :name :t :expr e :in n})
              by  (fn [rows]
                    (into {} (map (juxt #(get-in % [:values :name]) #(get-in % [:values :t])))
                          rows))]
          (is (= {"alpha" 12 "alphabet" 20 "beta" 31 "gamma" nil} (by (q s (c [:+ :size :n])))))
          (is (= {"alpha" 5.0 "alphabet" nil "beta" 30.0 "gamma" nil}
                 (by (q s (c [:div :size :n])))))
          (is (= #{"alphabet" "beta"}
                 (names (q s {:op :filter :where [:> :t 15] :in (c [:+ :size :n])}))))
          (is (= ["gamma" "beta" "alphabet" "alpha"]
                 (vals-of :name
                          (q s {:op :order :order-by :t :sort-dir :desc :in (c [:+ :size :n])})))
              "nil sorts first under :desc")))
      (testing ":concat-strings joins text in the store, so a derived title orders"
        (let [c  {:op :compute :name :t :expr [:concat-strings " – " :name :name]
                  :in {:op :scan :root widget}}
              by (fn [rows]
                   (into {} (map (juxt #(get-in % [:values :name]) #(get-in % [:values :t])))
                         rows))]
          (is (= {"alpha" "alpha – alpha" "alphabet" "alphabet – alphabet"
                  "beta" "beta – beta" "gamma" "gamma – gamma"}
                 (by (q s c))))
          (is (= ["gamma" "beta" "alphabet" "alpha"]
                 (vals-of :name (q s {:op :order :order-by :t :sort-dir :desc :in c}))))))
      (testing "a cast :compute column is exact and typed, the same value from every store"
        (let [n   {:op :with :name :n :fn :count :link owns :dir :to :in {:op :scan :root widget}}
              c   (fn [e cast] {:op :compute :name :t :expr e :cast cast :in n})
              by  (fn [rows]
                    (into {} (map (juxt #(get-in % [:values :name]) #(get-in % [:values :t])))
                          rows))]
          (is (= {"alpha" 5.00M "alphabet" nil "beta" 30.00M "gamma" nil}
                 (by (q s (c [:div :size :n] [:decimal 2])))))
          (is (= {"alpha" 3.3333M "alphabet" 6.6667M "beta" 10.0000M "gamma" nil}
                 (by (q s (c [:div :size 3] [:decimal 4]))))
              "a quotient that does not terminate, rounded to the cast's scale")
          (is (= {"alpha" -3 "alphabet" -5 "beta" -8 "gamma" nil}
                 (by (q s (c [:div [:- 0 :size] 4] :integer))))
              "half away from zero, also below it")
          (is (= {"alpha" 2.5 "alphabet" 5.0 "beta" 7.5 "gamma" nil}
                 (by (q s (c [:div :size 4] :double)))))
          (is (= #{"beta"}
                 (names (q s
                           {:op :filter :where [:> :t 6.6667M]
                            :in (c [:div :size 3] [:decimal 4])})))
              "a filter compares the cast value, not the unrounded quotient")))
      (testing "an aggregate folds and groups by columns as by attributes"
        (let [n   {:op :with :name :n :fn :count :link owns :dir :to :in {:op :scan :root widget}}
              agg (fn [aggs & [group-by in]] (q s (cond-> {:op :aggregate :aggs aggs :in (or in n)}
                                                    group-by (assoc :group-by group-by))))]
          (is (sem/same-data? {:n-sum 3 :n-avg 0.75M}
                              (:metrics (first (agg [{:fn :sum :attr :n} {:fn :avg :attr :n}])))))
          (is (sem/same-data? [[{:n 0} {:count 2}] [{:n 1} {:count 1}] [{:n 2} {:count 1}]]
                              (mapv (juxt :group :metrics) (agg [{:fn :count}] [:n]))))
          (is (sem/same-data? {:t-max 31}
                              (:metrics
                               (first (agg [{:fn :max :attr :t}] nil
                                           {:op :compute :name :t :expr [:+ :size :n] :in n})))))))
      (testing ":with folds an :expr over the FAR row, as a :compute of the far type"
        (submit s {:tx-vector [(create other 8 {:name "sparse" :tare 1}) (link 33 1 8)]})
        (let [n   (fn [& {:as more}] (merge {:op :with :name :n :link owns :dir :to
                                             :in {:op :scan :root widget}} more))
              by  (fn [rows]
                    (into {} (map (juxt #(get-in % [:values :name]) #(get-in % [:values :n])))
                          rows))
              net (fn [& {:as more}] (n (merge {:fn :sum :expr [:- :gross :tare]} more)))]
          (is (sem/same-data? {"alpha" 10 "alphabet" 0 "beta" 50 "gamma" 0} (by (q s (net))))
              "the fold is over the value each neighbour computes, and of none it is 0")
          (is (sem/same-data? {"alpha" 6 "alphabet" 0 "beta" 50 "gamma" 0}
                              (by (q s (net :where [:> :gross 8]))))
              "a :where picks the neighbours, the :expr says what of each is folded —
               alpha keeps only the one of its three whose gross is over 8")
          (testing "a neighbour whose expression is nil contributes nothing, as a nil attribute does"
            (is (sem/same-data? {"alpha" 2 "alphabet" 0 "beta" 1 "gamma" 0}
                                (by (q s (n :fn :count :expr [:- :gross :tare]))))
                "counted are the values, not the rows — alpha reaches three, one of them nil")
            (is (sem/same-data? {"alpha" 3 "alphabet" 0 "beta" 1 "gamma" 0}
                                (by (q s (n :fn :count))))
                "and a count with nothing to read counts the rows")
            (is (sem/same-data? {"alpha" 5 "alphabet" nil "beta" 50 "gamma" nil}
                                (by (q s (net :fn :avg))))
                "so a nil leaves the denominator too: 10 over 2, not over 3")
            (is (sem/same-data? {"alpha" 4 "alphabet" nil "beta" 50 "gamma" nil}
                                (by (q s (net :fn :min)))))
            (is (sem/same-data? {"alpha" 6 "alphabet" nil "beta" 50 "gamma" nil}
                                (by (q s (net :fn :max))))))
          (testing "a cast :expr is exact and rounded per far ROW, where the property declares it"
            (is (sem/same-data? {"alpha" 5.50M "alphabet" 0 "beta" 2.00M "gamma" 0}
                                (by (q s (net :expr [:div :gross :tare] :cast [:decimal 2]))))
                "3.00 + 2.50 over alpha, each quotient rounded before the fold"))
          (testing "the column a filter and an order read is the folded expression"
            (is (= #{"alpha" "beta"} (names (q s {:op :filter :where [:> :n 5] :in (net)}))))
            (is (= ["beta" "alpha" "gamma" "alphabet"]
                   (vals-of :name (q s {:op :order :order-by :n :sort-dir :desc :in (net)})))
                "ties on the column break on the id, in the same direction")
            (is (= #{"alpha"} (names (q s {:op :filter :where [:> :n 5] :in (net)} deny)))
                "a denied edge is no neighbour here either"))))
      (testing ":with folds over a far relation that carries folds of its own (:far-in)"
        (submit s {:tx-vector [(link 34 2 5)]})
        (let [
              held  {:op :with :name :held :fn :count :link owns :dir :from
                     :in {:op :scan :root other}}
              n     (fn [& {:as more}]
                      (merge {:op :with :name :n :fn :sum :attr :held :far-in held
                              :link owns :dir :to :in {:op :scan :root widget}}
                             more))
              by    (fn [rows]
                      (into {} (map (juxt #(get-in % [:values :name]) #(get-in % [:values :n])))
                            rows))]
          (testing "a fold OF a fold: each neighbour's own count, summed over the neighbours"
            (is (sem/same-data? {"alpha" 4 "alphabet" 2 "beta" 1 "gamma" 0} (by (q s (n))))
                "alpha reaches 5 (held by 2), 6 and 8 (held by 1 each); alphabet reaches only 5")
            (is (sem/same-data? {"alpha" 2 "alphabet" 2 "beta" 1 "gamma" nil}
                                (by (q s (n :fn :max))))
                "and :max is the largest of those counts — 2, the one neighbour held
                 twice — not the number of neighbours and not a max of anything
                 flattened; of no neighbours it is nil, as a max of nothing is")
            (is (sem/same-data? {"alpha" 3 "alphabet" 1 "beta" 1 "gamma" 0}
                                (by (q s (n :fn :count))))
                "a count counts the neighbours that have a value, two levels or one"))
          (testing "the inner fold sees only what the reader may see"
            (is (sem/same-data? {"alpha" 4 "alphabet" 2 "beta" 0 "gamma" 0} (by (q s (n) deny)))
                "beta's only edge is denied, so its neighbour is unreachable and the sum is 0"))
          (testing "a :where may name a column of that relation, as it names an attribute —
                    the one shape that used to filter on a column the store keeps NULL"
            (is (sem/same-data? {"alpha" 2 "alphabet" 2 "beta" 0 "gamma" 0}
                                (by (q s (n :where [:> :held 1]))))
                "only the neighbour held more than once contributes")
            (is (sem/same-data? {"alpha" 1 "alphabet" 1 "beta" 0 "gamma" 0}
                                (by (q s (n :fn :count :where [:> :held 1]))))))
          (testing "and the column a filter and an order read is still that fold"
            (is (= #{"alpha" "alphabet"} (names (q s {:op :filter :where [:> :n 1] :in (n)}))))
            (is (= ["alpha" "alphabet" "beta" "gamma"]
                   (vals-of :name (q s {:op :order :order-by :n :sort-dir :desc :in (n)})))))))
      (testing "a deleted object takes its links with it"
        (submit s {:tx-vector [{:op :delete-object :type widget :payload {:id (id 1)}}]})
        (is (= #{[(id 3) (id 7)] [(id 2) (id 5)]} (pairs (rd s [:links owns] authority))))
        (is (= #{} (names (q s (hop {:op :static :root widget :ids [(id 1)]}))))))
      (testing "a deleted link is gone"
        (submit s {:tx-vector [{:op :delete-link :type owns :payload {:id (id 32)}}]})
        (is (= #{[(id 2) (id 5)]} (pairs (rd s [:links owns] authority)))
            "that one and no other — the edge it shared its type with stays")))))

(defn check-questions [make]
  (testing "a malformed question is the asker's error"
    (let [s (seeded make)]
      (is (anomaly/client-error? (thrown-data #(rd s [:bogus widget]))))
      (is (anomaly/client-error? (thrown-data #(rd s [:objects]))))
      (is (anomaly/client-error? (thrown-data #(api/snapshot s "main"))))
      (is (anomaly/client-error? (thrown-data #(submit s {:tx-vector {:op :create-object}}))))
      (testing "a question of a vocabulary the store does not speak"
        (let [d (thrown-data #(rd s [:conformance/nonsense 1]))]
          (is (anomaly/client-error? d))
          (is (= :conformance/nonsense (:store/unsupported d)))))
      (testing "every store names what it answers, objects among them"
        (is (contains? (rd s [:store/capabilities] nil) :objects))))))

(defn- check-objects
  [make]
  (check-world make)
  (check-reads make)
  (check-writes make)
  (check-versions make)
  (check-query make)
  (check-authority make)
  (check-object-keys make)
  (check-links make)
  (check-questions make))

(def collections
  [{:coll :conformance/note :scope :branch :key :string
    :value [:map [:text :string] [:owner {:optional true} :string]] :index [:owner]}
   {:coll :conformance/setting :scope :global :key :keyword :value :any}
   {:coll :conformance/entry :scope :branch :key any? :value [:map [:text :string]]}
   {:coll :conformance/run :scope :branch :key :string
    :value [:map [:at :int] [:state :string]] :index [[:at :range] :state]}
   {:coll :conformance/numbered :scope :global :key any? :value :map :auto :seq}
   {:coll :conformance/handle :scope :global :key :string :value :map :unique [[:owner :name]]}
   {:coll :conformance/pointer :scope :global :key :string :value :map
    :drops-with :branch :kept-under "keep/"}])

(def ^:private actor {:principal "conformance"})

(defn- head [db b]
  (with-open [s (api/snapshot db {:branch b})] (api/basis s)))

(defn- tx!
  ([db tx] (tx! db "main" tx))
  ([db b tx]
   (api/transact! db (merge {:basis (head db b) :actor actor} tx))))

(defn- ask
  ([db query] (ask db "main" query))
  ([db b query] (with-open [s (api/snapshot db {:branch b})] (api/read s authority query))))

(defn- refused [f]
  (try (let [r (f)] (when (anomaly/refusal? r) (anomaly/data r)))
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- fresh [make]
  (doto (make collections)
    (tx! {:schema {:authored world :migration []} :ops seed :origin :index})))

(defn check-snapshots [make]
  (testing "snapshots"
    (let [db (fresh make)
          s  (api/snapshot db {:branch "main"})]
      (try
        (tx! db {:ops [(create widget 40 {:name "late"})]})
        (testing "a snapshot keeps answering at its revision"
          (is (not (contains? (names (api/read s authority [:objects widget])) "late")))
          (is (= world (api/read s nil [:schema/world]))))
        (testing "a new snapshot sees the commit"
          (is (contains? (names (ask db [:objects widget])) "late")))
        (testing "as-of answers at the old revision or refuses as unsupported"
          (let [old (api/basis s)
                r   (try {:rows (with-open [o (api/snapshot db old)]
                                  (api/read o authority [:objects widget]))}
                         (catch clojure.lang.ExceptionInfo e
                           {:refusal (:store/refusal (ex-data e))}))]
            (is (or (= #{"alpha" "alphabet" "beta" "gamma"} (names (:rows r)))
                    (= :as-of-unsupported (:refusal r))))))
        (finally (api/close s))))))

(defn check-history
  [make]
  (testing "history"
    (let [db       (fresh make)
          history? (contains? (api/capabilities db "main") :objects/history)
          at       (fn [b q] (with-open [s (api/snapshot db b)] (api/read s authority q)))
          r1       (head db "main")
          _        (tx! db
                        {:ops [{:op :update-object :type widget
                                :payload {:id (id 1) :values {:size 11}}}
                               (link 80 1 5)]
                         :log [{:kind :conformance/a :payload 2}]})
          r2       (head db "main")
          w3       (assoc-in world [:object-types other :description] "r3")
          _        (tx! db {:schema {:authored w3 :migration []}
                            :ops [{:op :delete-object :type widget :payload {:id (id 2)}}]
                            :records [{:coll :conformance/note :key "h" :value {:text "r3"}}]})
          r3       (head db "main")
          _        (tx! db
                        {:ops [(create widget 81 {:name "late"})
                               {:op :delete-link :type owns :payload {:id (id 80)}}]})
          r4       (head db "main")]
      (testing "an old revision is a snapshot like any other: its basis is that revision"
        (with-open [s (api/snapshot db r2)] (is (= (:rev r2) (:rev (api/basis s))))))
      (testing "the world answers as of every revision"
        (is (= world (at r1 [:schema/world])))
        (is (= world (at r2 [:schema/world])))
        (is (= w3 (at r3 [:schema/world])))
        (is (= w3 (at r4 [:schema/world]))))
      (testing "the log as of a revision is its prefix"
        (is (= [2] (map :payload (at r2 [:log/range {:after (:rev r1)}]))))
        (is (empty? (at r1 [:log/range {:after (:rev r1)}]))))
      (testing "records answer at the head only"
        (is (= :as-of-unsupported
               (:store/refusal (refused #(at r3 [:records/get :conformance/note "h"]))))))
      (if-not history?
        (testing "without :objects/history an old revision's objects are refused, typed"
          (doseq [q [[:objects widget] [:links owns] [:select {:root widget}]]]
            (is (= :as-of-unsupported (:store/refusal (refused #(at r1 q)))) (pr-str q))))
        (testing "with :objects/history objects and links answer as of every revision"
          (let [size  (fn [b n] (get-in (first (at b [:objects widget [(id n)]])) [:values :size]))
                ids   (fn [b] (set (map :id (at b [:objects widget]))))
                links (fn [b] (pairs (at b [:links owns])))]
            (is (= [10 11 11 11] (map #(size % 1) [r1 r2 r3 r4]))
                "an update is visible from its revision on")
            (is (= [true true false false] (map #(contains? (ids %) (id 2)) [r1 r2 r3 r4]))
                "so is a delete")
            (is (= [false false false true] (map #(contains? (ids %) (id 81)) [r1 r2 r3 r4]))
                "and a create")
            (is (= [false true true false]
                   (map #(contains? (links %) [(id 1) (id 5)]) [r1 r2 r3 r4]))
                "links have the same history")
            (is (= #{"alphabet" "beta"}
                   (names (at r1 [:select {:root widget :where [:>= :size 11]}]))))
            (is (= #{"alpha" "alphabet" "beta"}
                   (names (at r2 [:select {:root widget :where [:>= :size 11]}])))
                "a select answers as of the revision")
            (testing "an overlay on an old revision folds over that revision's rows"
              (with-open [s (api/snapshot db r1)]
                (is (= #{"alpha" "alphabet" "beta" "gamma" "staged"}
                       (names (api/read (api/overlay s [(create widget 82 {:name "staged"})])
                                        authority
                                        [:objects widget]))))))
            (testing "a revision not on the branch is malformed, not a state"
              (api/branch! db {:op :fork :name "hx" :from (head db "main")})
              (tx! db "hx" {:ops [(create widget 83 {:name "on-hx"})]})
              (is (= :malformed
                     (:store/refusal (refused #(at {:branch "main" :rev (:rev (head db "hx"))}
                                                   [:objects widget]))))))))))))

(defn check-interface
  [make]
  (testing "interface"
    (let [db (fresh make)]
      (testing "A2 a link count"
        (tx! db {:ops [(link 90 1 5) (link 91 2 5)]})
        (is (= 2 (ask db [:links owns {:count? true}])))
        (is (= 1 (ask db [:links owns {:from [(id 1)] :count? true}])))
        (tx! db {:ops [(create widget 93 {:name "cascaded"}) (link 92 93 5)]})
        (is (= 3 (ask db [:links owns {:count? true}])))
        (tx! db {:ops [{:op :delete-object :type widget :payload {:id (id 93)}}]})
        (is (= 2 (ask db [:links owns {:count? true}]))
            "a cascade moves the count: the object is gone, so its stored link is too"))
      (testing "A2 an object count, and what moves it"
        (let [n (ask db [:objects/count widget])]
          (is (= n (ask db [:objects/count widget])) "asked twice, answered the same")
          (tx! db {:ops [(create widget 94 {:name "counted"})]})
          (is (= (inc n) (ask db [:objects/count widget])) "a row written moves it")
          (tx! db {:ops [{:op :delete-object :type widget :payload {:id (id 94)}}]})
          (is (= n (ask db [:objects/count widget])) "and a row deleted moves it back")))
      (testing "A4 the history names the commits that changed the world"
        (let [before (head db "main")]
          (tx! db {:ops [(create widget 92 {:name "no-schema"})]})
          (tx! db
               {:schema {:authored (assoc-in world [:object-types other :description] "a4")
                         :migration []}})
          (let [changed (ask db [:schema/history {:schema? true :after (:rev before)}])]
            (is (= 1 (count changed)))
            (is (every? :schema? changed)))
          (is (= 2 (count (ask db [:schema/history {:after (:rev before)}])))
              "oldest first after a revision")))
      (testing "A5 the layout of a branch"
        (let [l (ask db [:store/layout])]
          (is (empty? (get-in l [:missing :object-types])) "every declared type has storage")
          (is (set? (:local l)))))
      (testing "A6 records by range, order and count"
        (let [db (make collections)
              put (fn [k at] {:coll :conformance/run :key k :value {:at at :state "x"}})]
          (tx! db {:records [(put "a" 30) (put "b" 10) (put "c" 20)]})
          (is (= ["b" "c"]
                 (map first (ask db [:records/list :conformance/run {:range [:at 5 25]}]))))
          (is (= ["a" "c" "b"]
                 (map first (ask db [:records/list :conformance/run {:order [:at :desc]}]))))
          (is (= ["a"]
                 (map first
                      (ask db [:records/list :conformance/run {:order [:at :desc] :limit 1}]))))
          (is (= 3 (ask db [:records/list :conformance/run {:count? true}])))
          (is (= {"x" 3} (ask db [:records/list :conformance/run {:group-by :state}]))
              "counts per value")
          (is (= [["a" {:at 30}] ["c" {:at 20}]]
                 (ask db [:records/list :conformance/run
                          {:order [:at :desc] :limit 2 :project #{:at}}]))
              "a projection reads only the attributes it names")
          (tx! db {:records [(assoc (put "d" 40) :value {:at 40 :state "y"})]})
          (is (= ["d"] (map first (ask db [:records/list :conformance/run {:where {:state "y"}}])))
              "A6d where")
          (is (= ["a" "d"]
                 (map first
                      (ask db
                           [:records/list :conformance/run
                            {:where {:state #{"x" "y"}} :range [:at 30 50]}])))
              "A6d a set and a range together")
          (is (= :malformed
                 (:store/refusal (refused #(ask db
                                                [:records/list :conformance/note
                                                 {:range [:owner "a" "b"]}]))))
              "a range needs a declared range index")))
      (testing "A6c the store assigns keys that rise with every commit"
        (let [r1 (tx! db {:records [{:coll :conformance/entry :key api/auto :value {:text "a"}}
                                    {:coll :conformance/entry :key api/auto :value {:text "b"}}]})
              r2 (tx! db {:records [{:coll :conformance/entry :key api/auto :value {:text "c"}}]})
              ks (mapv :key (concat (:records r1) (:records r2)))]
          (is (= 3 (count (distinct ks))))
          (is (= ks (sort ks)) "in commit order, then tx order")
          (is (= {:text "c"} (ask db [:records/get :conformance/entry (last ks)])))))
      (testing "explain says whether the engine lowers a select ( A9)"
        (let [e (ask db [:select/explain {:root widget :where [:>= :size 10]}])]
          (is (boolean? (:pushdown? e)))
          (is (or (nil? (:engine e)) (map? (:engine e))))))
      (testing "what is not implemented yet is refused, never ignored"
        (is (= :unsupported
               (:store/refusal (refused #(tx! db {:restore {:rev (:rev (head db "main"))}})))))))))

(defn check-edits
  [make]
  (testing "edits and the index (A3)"
    (let [db  (fresh make)
          upd (fn [n vs] {:op :update-object :type widget :payload {:id (id n) :values vs}})
          r0  (head db "main")]
      (tx! db {:ops [(upd 1 {:size 111})]})
      (tx! db {:ops [(create widget 95 {:name "by-hand"})]})
      (tx! db {:ops [{:op :delete-object :type widget :payload {:id (id 3)}}]})
      (testing "the edits index names what edits wrote"
        (let [e (ask db [:objects/edited widget [(id 1) (id 95) (id 3) (id 2)]])]
          (is (= #{:size} (get-in e [(id 1) :attrs])))
          (is (get-in e [(id 95) :created?]))
          (is (get-in e [(id 3) :deleted?]))
          (is (not (contains? e (id 2))) "the seed was the index's, not an edit")))
      (testing "an :index commit keeps what edits wrote"
        (tx! db {:origin :index
                 :ops [(create widget 1 {:name "alpha-src" :size 1 :owner "src"})
                       (upd 2 {:size 2})
                       {:op :delete-object :type widget :payload {:id (id 95)}}
                       (create widget 3 {:name "beta-src" :size 3})]})
        (let [v1 (:values (first (ask db [:objects widget [(id 1)]])))]
          (is (= 111 (:size v1)) "an edited attribute stays")
          (is (= "alpha-src" (:name v1)) "an unedited one takes the index's value"))
        (is (= 2 (get-in (first (ask db [:objects widget [(id 2)]])) [:values :size]))
            "unedited objects follow the index")
        (is (seq (ask db [:objects widget [(id 95)]]))
            "an object an edit created survives the index's delete")
        (is (empty? (ask db [:objects widget [(id 3)]]))
            "an object an edit deleted is not re-created"))
      (testing "the change feed lists every commit's changes with their origin"
        (let [cs (ask db [:objects/changes widget {:after (:rev r0)}])]
          (is (= [:edit :edit :edit :index]
                 (mapv :origin (distinct (map #(select-keys % [:rev :origin]) cs))))
              "one entry per commit and type, in commit order")
          (is (= #{(id 1)}
                 (set (map :id
                           (ask db
                                [:objects/changes widget
                                 {:after (:rev r0) :ids [(id 1)] :origin :edit}])))))
          (is (= {:size 111}
                 (:values (first (ask db
                                      [:objects/changes widget
                                       {:after (:rev r0) :ids [(id 1)] :origin :edit}]))))
              "an edit's entry carries the values it wrote (A3c)")))
      (testing "the source drops an edited row and emits it again: the edit applies again (T4–T5)"
        (tx! db {:origin :index :ops [{:op :delete-object :type widget :payload {:id (id 1)}}]})
        (is (empty? (ask db [:objects widget [(id 1)]])) "an edited row the source drops goes")
        (tx! db {:origin :index :ops [(create widget 1 {:name "alpha-back" :size 7})]})
        (is (= {:name "alpha-back" :size 111}
               (select-keys (:values (first (ask db [:objects widget [(id 1)]]))) [:name :size]))))
      (testing "an object an edit created no longer follows the source (T9–T10)"
        (tx! db {:origin :index :ops [(create widget 95 {:name "from-source" :size 9})]})
        (is (= ["by-hand" nil]
               ((juxt :name :size) (:values (first (ask db [:objects widget [(id 95)]])))))))
      (testing "a rename migration carries the edits along"
        (let [w (update-in world [:object-types widget :attribute-list]
                           (partial mapv
                                    #(if (= :size (:attribute-name %))
                                       (assoc % :attribute-name :bulk) %)))]
          (tx! db {:schema {:authored w :migration [[:rename-attribute widget :size :bulk]]}})
          (is (= #{:bulk} (get-in (ask db [:objects/edited widget [(id 1)]]) [(id 1) :attrs])))
          (is (= [{:bulk 111}]
                 (mapv :values
                       (ask db
                            [:objects/changes widget
                             {:after (:rev r0) :ids [(id 1)] :origin :edit}])))
              "the history reads under today's names")
          (tx! db {:schema {:authored world :migration [[:rename-attribute widget :bulk :size]]}})))
      (testing "forgotten edits let the index win again (A3d)"
        (tx! db {:forget-edits [widget]})
        (is (empty? (ask db [:objects/edited widget])))
        (tx! db {:origin :index :ops [(upd 1 {:size 1})]})
        (is (= 1 (get-in (first (ask db [:objects widget [(id 1)]])) [:values :size]))))
      (testing "adopted edits are edits: the rows take them back and the index keeps them"
        (let [size #(get-in (first (ask db [:objects widget [(id %)]])) [:values :size])]
          (tx! db {:adopt-edits [{:type widget :id (id 1) :kind :update :values {:size 222} :at 1}
                                 {:type widget :id (id 2) :kind :delete}
                                 {:type widget :id (id 96) :kind :create
                                  :values {:name "lost" :size 6}}]})
          (is (= 222 (size 1)) "an edited row takes the edit back")
          (is (empty? (ask db [:objects widget [(id 2)]])) "a deleted row goes again")
          (is (= 6 (size 96)) "a created object the index dropped comes back")
          (is (get-in (ask db [:objects/edited widget [(id 96)]]) [(id 96) :created?]))
          (tx! db
               {:origin :index :ops [(upd 1 {:size 1}) (create widget 2 {:name "beta-src" :size 2})
                                     {:op :delete-object :type widget :payload {:id (id 96)}}]})
          (is (= 222 (size 1)) "the index keeps an adopted edit")
          (is (empty? (ask db [:objects widget [(id 2)]])))
          (is (= 6 (size 96)))
          (tx! db {:adopt-edits [{:type widget :id (id 1) :kind :update :values {:size 333}}]})
          (is (= 222 (size 1)) "an object the index holds an edit of keeps it")
          (is (thrown? clojure.lang.ExceptionInfo
                       (tx! db
                            {:adopt-edits [{:type widget :id (id 5) :kind :delete}]
                             :ops [(upd 1 {:size 4})]}))))))))

(defn check-world-value [make]
  (testing "the world is a value"
    (let [db (make collections)
          r  (tx! db {:schema {:authored world :migration []}})]
      (is (= world (ask db [:schema/world])) "read ∘ write = identity")
      (is (= (set (keys (:object-types world)))
             (set (keys (get-in r [:schema-diff :object-types :added])))))
      (tx! db {:ops seed})
      (testing "a rename migration keeps the values under the new name"
        (let [w (update-in world [:object-types widget :attribute-list]
                           (partial mapv
                                    #(if (= :owner (:attribute-name %))
                                       (assoc % :attribute-name :holder) %)))]
          (tx! db {:schema {:authored w :migration [[:rename-attribute widget :owner :holder]]}})
          (is (= w (ask db [:schema/world])))
          (is (= "ava" (get-in (first (ask db [:objects widget [(id 1)]])) [:values :holder])))
          (is (not (contains? (:values (first (ask db [:objects widget [(id 1)]]))) :owner)))
          (testing "a drop migration removes the attribute"
            (let [w2 (update-in w [:object-types widget :attribute-list]
                                (partial filterv #(not= :size (:attribute-name %))))]
              (tx! db {:schema {:authored w2 :migration [[:drop-attribute widget :size]]}})
              (is (not-any? #(contains? (:values %) :size) (ask db [:objects widget]))))))))))

(defn check-resource-history [make]
  (testing "each resource has its own history (an Ontology resource's History tab)"
    (let [db  (make collections)
          rw  (fn [w d] (assoc-in w [:object-types widget :description] d))
          r1  (:rev (:basis (tx! db {:schema {:authored world :migration []}})))
          r2  (:rev (:basis (tx! db {:schema {:authored (rw world "zwei") :migration []}})))
          _   (tx! db {:schema {:authored (assoc-in (rw world "zwei")
                                                    [:object-types other :description] "x")
                                :migration []}})
          hw  (ask db [:schema/resource-history [:object-types widget]])]
      (is (= [r2 r1] (mapv :rev hw)) "a commit that left it alone is no version of it")
      (is (= ["zwei" nil] (mapv (comp :description :value) hw)))
      (is (= [r1 nil] (mapv :from hw)) "each version names the one it replaced")
      (testing "a branch continues its parent's history before the fork"
        (api/branch! db {:op :fork :name "rh" :from (head db "main")})
        (let [r4 (:rev (:basis (tx! db "rh"
                                    {:schema {:authored (-> (ask db "rh" [:schema/world])
                                                            (update :object-types dissoc widget))
                                              :migration []}})))
              hb (ask db "rh" [:schema/resource-history [:object-types widget]])]
          (is (= [r4 r2 r1] (mapv :rev hb)))
          (is (nil? (:value (first hb))) "a removal is a version without a value")
          (is (= [r2 r1] (mapv :rev (ask db [:schema/resource-history
                                             [:object-types widget]])))
              "the parent never sees the branch's version")
          (testing "what a merge compares: the resources each side changed"
            (is (= #{[:object-types widget]} (ask db "rh" [:schema/changed {}]))
                "a branch since its fork")
            (is (= #{[:object-types other]} (ask db [:schema/changed {:after r2}]))
                "its parent after the base")
            (is (= #{} (ask db [:schema/changed {:after r4}])) "nothing after the head")))))))

(defn check-fences [make]
  (testing "fences"
    (let [db    (fresh make)
          base  (head db "main")
          upd   (fn [n v] {:op :update-object :type widget :payload {:id (id n) :values {:size v}}})
          fail  (fn [b fences] (refused #(api/transact! db {:basis b :actor actor :fences fences
                                                            :ops [(upd 2 0)]})))
          v1    (object-version (first (ask db [:objects widget [(id 1)]])))]
      (testing "every fence holds when nothing changed"
        (is (nil? (fail base [[:head] [:schema] [:type widget] [:link-type owns]
                              [:object widget (id 1) v1] [:record :conformance/note "n" api/absent]
                              [:lease "l" 1]]))))
      (let [base (head db "main")]
        (tx! db {:ops [(upd 1 99)]})
        (testing "a write to the type trips [:head], [:type] and [:object]"
          (is (= [:head] (:store/fence (fail base [[:head]]))))
          (is (= :conflict (:store/refusal (fail base [[:type widget]]))))
          (is (:retryable? (fail base [[:type widget]])))
          (is (= :stale-object (:store/refusal (fail base [[:object widget (id 1) v1]])))))
        (testing "it trips neither [:schema] nor another type's fence"
          (is (nil? (fail base [[:schema] [:type other] [:link-type owns]])))))
      (let [base (head db "main")]
        (tx! db
             {:schema {:authored (assoc-in world [:object-types other :description] "x")
                       :migration []}})
        (testing "a schema commit trips [:schema]"
          (is (= [:schema] (:store/fence (fail base [[:schema]]))))))
      (let [base (head db "main")]
        (tx! db {:records [{:coll :conformance/note :key "n" :value {:text "t"}}]})
        (testing "a record write trips [:record]"
          (is (= :conflict
                 (:store/refusal (fail base [[:record :conformance/note "n" api/absent]]))))
          (is (nil? (fail (head db "main") [[:record :conformance/note "n" {:text "t"}]])))))
      (testing "a lease epoch below the highest seen is refused, and not retried"
        (tx! db {:fences [[:lease "l" 5]]})
        (is (= :fenced (:store/refusal (fail (head db "main") [[:lease "l" 4]]))))
        (is (not (:retryable? (fail (head db "main") [[:lease "l" 4]]))))
        (is (nil? (fail (head db "main") [[:lease "l" 5]]))))
      (testing "[:branch-head b r] holds while b is at r, a write to b trips it"
        (api/branch! db {:op :fork :name "bh" :from (head db "main")})
        (let [r (:rev (head db "bh"))]
          (is (nil? (fail (head db "main") [[:branch-head "bh" r]])))
          (tx! db "bh" {:ops [(upd 1 7)]})
          (is (= :conflict (:store/refusal (fail (head db "main") [[:branch-head "bh" r]]))))
          (is (nil? (fail (head db "main") [[:branch-head "bh" (:rev (head db "bh"))]]))))))))

(defn check-atomicity [make]
  (testing "atomicity"
    (let [db    (fresh make)
          log0  (ask db [:log/range])
          base  (head db "main")
          stale {:op :update-object :type widget :payload {:id (id 1) :values {:size 0}}
                 :expected-version :not-the-version}
          big   {:basis base :actor actor
                 :schema {:authored (assoc-in world [:object-types other :description] "x")
                          :migration []}
                 :ops [(create widget 41 {:name "nope"}) stale]
                 :log [{:kind :conformance/event :payload 1}]
                 :records [{:coll :conformance/note :key "a" :value {:text "a"}}
                           {:coll :conformance/setting :key :k :value 1}]}]
      (testing "a refused tx leaves every vocabulary as it was"
        (is (= :stale-object (:store/refusal (refused #(api/transact! db big)))))
        (is (= base (head db "main")))
        (is (= world (ask db [:schema/world])))
        (is (empty? (ask db [:objects widget [(id 41)]])))
        (is (= log0 (ask db [:log/range])))
        (is (nil? (ask db [:records/get :conformance/note "a"])))
        (is (nil? (ask db [:records/get :conformance/setting :k]))))
      (testing "a refused tx leaves no layout: storage it laid out, or dropped, is as it was"
        (let [fresh-type "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000c0f7"
              grown (assoc-in world [:object-types fresh-type]
                              {:object-type-name :grown
                               :attribute-list [{:attribute-name :n :attribute-type :integer}]})
              n     (count (ask db [:objects widget]))]
          (is (= :stale-object
                 (:store/refusal
                  (refused #(api/transact! db {:basis (head db "main") :actor actor
                                               :schema {:authored grown :migration []}
                                               :ops [(create fresh-type 45 {:n 1}) stale]})))))
          (is (= {:orphans [] :missing {:object-types [] :link-types []}}
                 (-> (ask db [:store/layout]) (select-keys [:orphans :missing])
                     (update :missing select-keys [:object-types :link-types])))
              "the new type's storage is gone with the refused tx")
          (is (= :stale-object
                 (:store/refusal
                  (refused #(api/transact! db
                                           {:basis (head db "main") :actor actor
                                            :schema
                                            {:authored (update world :object-types dissoc widget)
                                             :migration [[:drop-unclaimed]]}
                                            :ops [stale]})))))
          (is (= n (count (ask db [:objects widget]))) "the dropped type's rows are back")))
      (testing "prepare then abort leaves nothing, and the store takes the next tx"
        (let [p (api/prepare! db (assoc big :ops [(create widget 42 {:name "aborted"})]))]
          (api/abort! p)
          (is (= base (head db "main")))
          (is (empty? (ask db [:objects widget [(id 42)]])))
          (is (= :prepare-expired (:store/refusal (refused #(api/commit! p)))))
          (tx! db {:ops [(create widget 43 {:name "after"})]})
          (is (seq (ask db [:objects widget [(id 43)]])))))
      (testing "prepare then commit lands all of it"
        (let [p (api/prepare! db
                              (assoc big :basis (head db "main")
                                     :ops [(create widget 44 {:name "all"})]))
              r (api/commit! p)]
          (is (= (:basis r) (head db "main")))
          (is (seq (ask db [:objects widget [(id 44)]])))
          (is (= {:text "a"} (ask db [:records/get :conformance/note "a"])))
          (is (= 1 (ask db [:records/get :conformance/setting :k])))
          (is (= [:conformance/event] (map :kind (ask db [:log/range {:after (:rev base)}])))))))))

(defn check-branches [make]
  (testing "branches"
    (let [db (fresh make)]
      (tx! db {:records [{:coll :conformance/note :key "n" :value {:text "main"}}]})
      (let [at  (head db "main")
            row (api/branch! db {:op :fork :name "f" :from at :meta {:kind :feature}})]
        (is (= (:rev at) (:rev row)))
        (is (= {:kind :feature} (:meta (ask db [:branch/get "f"]))))
        (testing "world and records of a fork are its parent's at the fork"
          (doseq [q [[:schema/world] [:records/get :conformance/note "n"]]]
            (is (= (ask db "main" q) (ask db "f" q)) (pr-str q))))
        (testing "so are its objects and links, which it reads live"
          (doseq [q [[:objects widget] [:links owns]]]
            (is (= (ask db "main" q) (ask db "f" q)) (pr-str q))))
        (testing "a fork of a fork is a branch like any other"
          (api/branch! db {:op :fork :name "g" :from (head db "f")})
          (is (= (ask db "f" [:schema/world]) (ask db "g" [:schema/world])))
          (tx! db "g" {:ops [(create widget 52 {:name "on-g"})]})
          (is (= #{"on-g"} (names (ask db "g" [:objects widget [(id 52)]])))))
        (testing "what a branch writes stays on it"
          (tx! db "main" {:ops [(create widget 50 {:name "on-main"})]})
          (tx! db "f" {:ops [(create widget 51 {:name "on-f"})]})
          (is (= #{"on-main"} (names (ask db "main" [:objects widget [(id 50) (id 51)]]))))
          (is (contains? (names (ask db "f" [:objects widget [(id 50) (id 51)]])) "on-f")))
        (testing "a fork onto a taken name is refused"
          (is (= :conflict
                 (:store/refusal (refused #(api/branch! db {:op :fork :name "f" :from at}))))))
        (testing ":parents and :reparent land in the history"
          (let [from-f (head db "f")]
            (tx! db "main" {:parents [from-f] :message "merge f"})
            (let [h (first (ask db [:schema/history {:limit 1}]))]
              (is (= "merge f" (:message h)))
              (is (some #{(:rev from-f)} (:parents h)))))
          (let [onto (head db "main")]
            (tx! db "f" {:reparent onto})
            (is (= (:rev onto) (get-in (ask db [:branch/get "f"]) [:base :rev])))
            (is (= (:rev onto)
                   (get-in (first (ask db "f" [:schema/history {:limit 1}])) [:reparent :rev])))))
        (testing "retire is a CAS on the head"
          (is (= :conflict
                 (:store/refusal
                  (refused #(api/branch! db {:op :retire :name "f" :expect-rev -1})))))
          (api/branch! db {:op :retire :name "f" :expect-rev (:rev (head db "f"))})
          (is (= :no-branch (:store/refusal (refused #(head db "f"))))))))))

(defn check-inheritance
  [make]
  (testing "inherited rows on a branch"
    (let [db (fresh make)]
      (api/branch! db {:op :fork :name "f" :from (head db "main")})
      (is (= 10 (get-in (first (ask db "f" [:objects widget [(id 1)]])) [:values :size]))
          "a fork reads its parent's row; what a driver kept of that read must not survive the write below")
      (let [b0 (head db "f")]
        (tx! db "f"
             {:ops [{:op :update-object :type widget :payload {:id (id 1) :values {:size 101}}}
                    {:op :delete-object :type widget :payload {:id (id 2)}}]})
        (is (= 101 (get-in (first (ask db "f" [:objects widget [(id 1)]])) [:values :size]))
            "an update of an inherited row is the branch's")
        (is (empty? (ask db "f" [:objects widget [(id 2)]]))
            "a delete of an inherited row stays deleted")
        (is (= 10 (get-in (first (ask db "main" [:objects widget [(id 1)]])) [:values :size]))
            "the parent's row is untouched")
        (is (= #{"alphabet"} (names (ask db "main" [:objects widget [(id 2)]]))))
        (tx! db "main"
             {:ops [{:op :update-object :type widget :payload {:id (id 3) :values {:size 33}}}]})
        (let [seen (get-in (first (ask db "f" [:objects widget [(id 3)]])) [:values :size])
              trip (refused #(tx! db "f" {:basis (head db "f") :fences [[:type widget]]}))
              old  (refused #(tx! db "f" {:basis b0 :fences [[:type widget]]}))]
          (is (= 33 seen)
              "a parent's later update shows on the branch, also for a type the branch wrote")
          (is (nil? trip) "a fence taken after the parent's write holds")
          (is (= :conflict (:store/refusal old))
              "a fence from before the branch's own write fails"))
        (let [b1 (head db "f")]
          (tx! db "main"
               {:ops [{:op :update-object :type widget :payload {:id (id 4) :values {:size 44}}}]})
          (let [r (refused #(tx! db "f" {:basis b1 :fences [[:type widget]]}))]
            (is (= :conflict (:store/refusal r))
                "the parent's write to a type the branch reads live fails the branch's fence")))))))

(defn- renamed [w T a b]
  (update-in w [:object-types T :attribute-list]
             (partial mapv #(if (= a (:attribute-name %)) (assoc % :attribute-name b) %))))

(defn check-lineage
  [make]
  (testing "branch lineage"
    (let [db   (fresh make)
          upd  (fn [n vs] {:op :update-object :type widget :payload {:id (id n) :values vs}})
          v    (fn [b n a] (get-in (first (ask db b [:objects widget [(id n)]])) [:values a]))
          r0   (head db "main")]
      (tx! db {:ops [(upd 1 {:size 111})]})
      (api/branch! db {:op :fork :name "f" :from (head db "main")})
      (tx! db "f"
           {:ops [(upd 2 {:size 222}) {:op :delete-object :type widget :payload {:id (id 3)}}]})
      (testing "a fork of a branch starts from what the branch holds"
        (api/branch! db {:op :fork :name "g" :from (head db "f")})
        (is (= [111 222] [(v "g" 1 :size) (v "g" 2 :size)]))
        (is (empty? (ask db "g" [:objects widget [(id 3)]])))
        (is (= #{(id 1) (id 2) (id 3)} (set (keys (ask db "g" [:objects/edited widget]))))))
      (testing "a branch renames an attribute: rows, edits and history carry the new name there only"
        (tx! db "g"
             {:schema {:authored (renamed world widget :size :bulk)
                       :migration [[:rename-attribute widget :size :bulk]]}})
        (is (= [111 222] [(v "g" 1 :bulk) (v "g" 2 :bulk)]))
        (is (seq (ask db "g" [:objects widget [(id 4)]]))
            "an unedited row it read from its parent stays")
        (is (= {:attrs #{:bulk} :values {:bulk 111}}
               (select-keys (get (ask db "g" [:objects/edited widget]) (id 1)) [:attrs :values])))
        (is (= [{:bulk 111}]
               (mapv :values
                     (ask db "g"
                          [:objects/changes widget
                           {:after (:rev r0) :ids [(id 1)] :origin :edit}]))))
        (is (= [111 #{:size}]
               [(v "main" 1 :size) (get-in (ask db [:objects/edited widget]) [(id 1) :attrs])]))
        (is (= [{:size 111}]
               (mapv :values
                     (ask db
                          [:objects/changes widget
                           {:after (:rev r0) :ids [(id 1)] :origin :edit}]))))
        (tx! db {:ops [(upd 4 {:size 44})]})
        (is (nil? (v "g" 4 :bulk))
            "the parent's later writes do not reach the type the branch keeps"))
      (testing "a branch forgets edits: its own view only, and the index wins there again"
        (tx! db "f" {:forget-edits [widget]})
        (is (empty? (ask db "f" [:objects/edited widget])))
        (is (= #{:size} (get-in (ask db [:objects/edited widget]) [(id 1) :attrs])))
        (tx! db "f" {:origin :index :ops [(upd 1 {:size 1})]})
        (is (= [1 111] [(v "f" 1 :size) (v "main" 1 :size)]))))))

(defn- copy-edits!
  [db from b]
  (let [h (engine/begin db :write nil)]
    (commit/apply! db h (mapv (fn [[k v]] (kv/kv-fx [:edited b k] v))
                              (kv/kv-pages db h [:edited from] {})))
    (engine/commit! db h)))

(defn check-inherited-edits
  [make]
  (testing "inherited edits"
    (let [db     (fresh make)
          upd    (fn [n vs] {:op :update-object :type widget :payload {:id (id n) :values vs}})
          edited #(ask db % [:objects/edited widget])]
      (tx! db {:ops [(upd 1 {:size 111}) (upd 2 {:size 22})]})
      (api/branch! db {:op :fork :name "f" :from (head db "main")})
      (tx! db "f" {:ops [(upd 2 {:size 222}) (upd 4 {:size 444})]})
      (api/branch! db {:op :fork :name "f-stage" :from (head db "f")})
      (testing "a fork of a branch reads exactly the edits its parent reads"
        (is (= (edited "f") (edited "f-stage")))
        (is (= #{(id 1) (id 2) (id 4)} (set (keys (edited "f-stage"))))))
      (testing "an adoption skips an object its root's edits hold"
        (tx! db "f" {:adopt-edits [{:type widget :id (id 1) :kind :update :values {:size 9}}]})
        (is (= 111 (get-in (edited "f") [(id 1) :values :size]))))
      (testing "an edit on a fork lands over the root's as it did over a copy"
        (tx! db "f" {:ops [(upd 1 {:name "renamed"})]})
        (is (= {:size 111 :name "renamed"} (get-in (edited "f") [(id 1) :values]))))
      (when (instance? sutra.store.core.Db db)
        (testing "dropping an older fork's copies of its root's edits changes none of its reads"
          (api/branch! db {:op :fork :name "old" :from (head db "main")})
          (copy-edits! db "main" "old")
          (tx! db "old" {:ops [(upd 2 {:size 2222})]})
          (let [before (edited "old")
                out    (inherited-edits/compact! db)]
            (is (= 1 (get out "old")) "the unchanged copy of the root's edit of 1 is dropped")
            (is (= before (edited "old")))
            (is (nil? (inherited-edits/compact! db))
                "it runs once")))))))

(defn check-names [make]
  (testing "names: a type addressed by its name is the type W keeps under its RID"
    (let [db (fresh make)]
      (tx! db {:ops [(create :widget 60 {:name "by-name"})]})
      (is (= #{"by-name"} (names (ask db [:objects widget [(id 60)]])))
          "an op naming the type writes the RID's rows")
      (is (= #{"by-name"} (names (ask db [:objects :widget [(id 60)]]))))
      (is (= (ask db [:objects widget]) (ask db [:objects :widget])))
      (is (contains? (names (ask db [:select {:root :widget}])) "by-name")
          "a select naming the type reads the RID's rows")
      (testing "a fence naming the type is the RID's fence"
        (let [b (head db "main")]
          (tx! db {:ops [(create widget 61 {:name "moved"})]})
          (is (= :conflict
                 (:store/refusal (refused #(tx! db {:basis b :fences [[:type :widget]]}))))))))))

(defn check-overlay [make]
  (testing "overlay: a snapshot under staged ops"
    (let [db (fresh make)]
      (with-open [s (api/snapshot db {:branch "main"})]
        (let [o (api/overlay s [(create widget 70 {:name "staged" :size 70})
                                {:op :update-object :type widget
                                 :payload {:id (id 1) :values {:size 99}}}
                                {:op :delete-object :type widget :payload {:id (id 3)}}
                                (link 71 70 5)])]
          (is (= (api/basis s) (api/basis o)) "the basis is the snapshot's")
          (is (= #{"alpha" "alphabet" "gamma" "staged"}
                 (names (api/read o authority [:objects widget])))
              "creates appear, deletes are gone")
          (is (= 99
                 (get-in (first (api/read o authority [:objects widget [(id 1)]])) [:values :size]))
              "updates merge over the stored row")
          (is (= #{"staged"} (names (api/read o authority [:objects widget [(id 70) (id 3)]]))))
          (is (contains? (pairs (api/read o authority [:links owns])) [(id 70) (id 5)]))
          (is (= #{"staged" "alpha"}
                 (names (api/read o authority [:select {:root widget :where [:>= :size 70]}])))
              "a select over a staged type answers under the overlay, staged updates included")
          (is (= (names (api/read s authority [:objects widget]))
                 #{"alpha" "alphabet" "beta" "gamma"})
              "the snapshot under it is unchanged")
          (testing "nothing is written"
            (is (= #{"alpha" "alphabet" "beta" "gamma"} (names (ask db [:objects widget])))))
          (testing "overlays stack"
            (let [o2 (api/overlay o [{:op :delete-object :type widget :payload {:id (id 70)}}])]
              (is (not (contains? (names (api/read o2 authority [:objects widget])) "staged")))
              (is (not (contains? (pairs (api/read o2 authority [:links owns])) [(id 70) (id 5)]))
                  "a staged delete cascades into staged links"))))))))

(def ^:private gizmo "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000c0f9")

(defn check-overlay-world [make]
  (testing "overlay under an uncommitted world ( A1)"
    (let [db (fresh make)
          W' (-> world
                 (assoc-in [:object-types gizmo]
                           {:object-type-name :gizmo
                            :attribute-list [{:attribute-name :label :attribute-type :string}]})
                 (update-in [:object-types widget :attribute-list]
                            (fn [as]
                              (-> (mapv #(if (= :name (:attribute-name %))
                                           (assoc % :attribute-name :title) %) as)
                                  (conj {:attribute-name :colour :attribute-type :string})))))]
      (with-open [s (api/snapshot db {:branch "main"})]
        (let [o (api/overlay s {:world W' :migration [[:rename-attribute widget :name :title]]
                                :ops [{:op :create-object :type gizmo
                                       :payload {:id (id 90) :values {:label "new"}}}
                                      {:op :update-object :type widget
                                       :payload {:id (id 2) :values {:colour "red"}}}]})
              titles #(set (map (comp :title :values) %))]
          (is (= W' (api/read o authority [:schema/world])) "the world read is the overlaid one")
          (is (= [{:label "new"}] (mapv :values (api/read o authority [:objects gizmo])))
              "a type new in the world reads empty plus staged rows")
          (is (= #{"alpha" "alphabet" "beta" "gamma"}
                 (titles (api/read o authority [:objects widget])))
              "a renamed attribute reads under its new name")
          (is (not-any? #(contains? (:values %) :name) (api/read o authority [:objects widget])))
          (is (= "red"
                 (get-in (first (api/read o authority [:objects widget [(id 2)]]))
                         [:values :colour]))
              "an attribute new in the world reads what the overlay staged")
          (is (= #{"alphabet"}
                 (titles (api/read o authority [:select {:root widget :where [:= :colour "red"]}])))
              "a select under the overlaid world answers in its terms")
          (testing "nothing is written"
            (is (= world (ask db [:schema/world])))
            (is (= [] (ask db [:objects gizmo])))))))))

(defn check-records [make]
  (testing "records"
    (let [db  (make collections)
          put (fn [k v & [expect]] {:coll :conformance/note :key k :value v
                                    :expect (if (some? expect) expect api/absent)})]
      (tx! db {:records [(put "a" {:text "1" :owner "ava"}) (put "b" {:text "2" :owner "bob"})
                         (put "c" {:text "3" :owner "ava"})]})
      (testing "CAS on absence refuses a key that exists"
        (is (= :conflict (:store/refusal (refused #(tx! db {:records [(put "a" {:text "x"})]}))))))
      (testing "CAS on a value lands when the value is current"
        (tx! db {:records [(put "a" {:text "1b" :owner "ava"} {:text "1" :owner "ava"})]})
        (is (= "1b" (:text (ask db [:records/get :conformance/note "a"])))))
      (testing "list by index, prefix and page"
        (is (= ["a" "c"]
               (map first (ask db [:records/list :conformance/note {:index [:owner "ava"]}]))))
        (is (= ["b"] (map first (ask db [:records/list :conformance/note {:prefix "b"}]))))
        (is (= ["b"] (map first (ask db [:records/list :conformance/note {:after "a" :limit 1}])))))
      (testing "a nil value deletes"
        (tx! db {:records [{:coll :conformance/note :key "b" :value nil}]})
        (is (nil? (ask db [:records/get :conformance/note "b"]))))
      (testing "an undeclared collection or a value against its schema is malformed"
        (is (= :malformed
               (:store/refusal (refused #(tx! db
                                              {:records
                                               [{:coll :conformance/nope :key "x" :value 1}]})))))
        (is (= :malformed
               (:store/refusal (refused #(tx! db
                                              {:records
                                               [{:coll :conformance/note :key "x"
                                                 :value {:text 1}}]}))))))
      (testing "an assigned key is judged apart from the value: an :expect holds whether it
                carries the collection's :auto attribute or not, wherever the engine keeps it"
        (tx! db {:records [{:coll :conformance/numbered :key api/auto :value {:text "n1"}}
                           {:coll :conformance/numbered :key api/auto :value {:text "n2"}}]})
        (let [[[k1 v1] [k2 v2]] (ask db [:records/list :conformance/numbered {}])]
          (is (and (int? k1) (int? k2) (< k1 k2)) "an assigned key is an integer, rising")
          (tx! db {:records [{:coll :conformance/numbered :key api/auto :value {:text "n3"}}]})
          (is (< k2 (ffirst (ask db [:records/list :conformance/numbered {:after k2}])))
              "a later commit assigns a greater one")
          (is (= [k1]
                 (map first (ask db [:records/list :conformance/numbered {:where {:seq #{k1}}}])))
              "a where on the :auto attribute selects by the assigned key")
          (is (= [k2 k1]
                 (map first
                      (ask db [:records/list :conformance/numbered {:order [:seq :desc] :limit 2
                                                                    :range [:seq k1 k2]}])))
              "an order and a range on the :auto attribute go by the assigned key")
          (tx! db
               {:records [{:coll :conformance/numbered :key k1 :expect (assoc v1 :seq k1)
                           :value nil}
                          {:coll :conformance/numbered :key k2 :expect (dissoc v2 :seq)
                           :value nil}]})
          (is (= ["n3"] (map (comp :text second) (ask db [:records/list :conformance/numbered {}])))
              "both deletes held")))
      (testing "a write of global records alone is service state: it moves no branch's head,
                and a later one still assigns a greater key"
        (let [h0 (head db "main")
              [{k1 :key}] (:records (tx! db {:records [{:coll :conformance/numbered :key api/auto
                                                        :value {:text "s1"}}]}))
              [{k2 :key}] (:records (tx! db {:records [{:coll :conformance/numbered :key api/auto
                                                        :value {:text "s2"}}]}))]
          (is (= h0 (head db "main")))
          (is (< k1 k2)))))))

(defn check-log [make]
  (testing "log"
    (let [db (make collections)
          r1 (tx! db
                  {:log [{:kind :conformance/a :payload 1}
                         {:kind :conformance/b :payload 2 :outbox? true}]})
          r2 (tx! db {:log [{:kind :conformance/a :payload 3}]})
          l  (ask db [:log/range])]
      (is (= [1 2 3] (map :payload l)) "commit order, then entry order")
      (is (= [[(-> r1 :basis :rev) 0] [(-> r1 :basis :rev) 1] [(-> r2 :basis :rev) 0]]
             (map (juxt :rev :index) l)) "(rev, index) names an entry")
      (is (= actor (:actor (first l))))
      (is (= [2] (map :payload (ask db [:log/range {:kinds #{:conformance/b}}]))))
      (is (= [3] (map :payload (ask db [:log/range {:after (-> r1 :basis :rev)}]))))
      (testing ":newest? with :limit reads backward"
        (is (= [3 2] (map :payload (ask db [:log/range {:newest? true :limit 2}]))) "newest first")
        (is (= [3 1]
               (map :payload
                    (ask db [:log/range {:kinds #{:conformance/a} :newest? true :limit 2}])))
            "the kinds filter sits ABOVE the scan: the newest two OF THAT KIND, not of the log")
        (is (= [3 2 1] (map :payload (ask db [:log/range {:newest? true :limit 99}])))
            "a limit past the end is the whole log, reversed")
        (tx! db {:log (vec (for [i (range 200)] {:kind :conformance/b :payload (+ 100 i)}))})
        (is (= [3]
               (map :payload
                    (ask db [:log/range {:kinds #{:conformance/a} :newest? true :limit 1}])))
            "a window that holds none of the kind grows until it does")))))

(defn check-log-own [make]
  (testing "log, own entries"
    (let [db  (fresh make)
          own (fn [b & [o]] (map :payload (ask db b [:log/range (merge {:own? true} o)])))]
      (tx! db {:log [{:kind :conformance/a :payload 1}]})
      (api/branch! db {:op :fork :name "f" :from (head db "main")})
      (let [rf (tx! db "f"
                    {:log [{:kind :conformance/a :payload 2} {:kind :conformance/b :payload 3}]})]
        (tx! db "main" {:log [{:kind :conformance/a :payload 4}]})
        (api/branch! db {:op :fork :name "s" :from (head db "main")})
        (tx! db "s" {:log [{:kind :conformance/a :payload 5}]})
        (tx! db "f" {:log [{:kind :conformance/a :payload 6}]})
        (testing "neither the parent's nor a sibling's entries are own"
          (is (= [1 4] (own "main")))
          (is (= [2 3 6] (own "f")))
          (is (= [5] (own "s"))))
        (testing "equal to the history prefix filtered by branch"
          (doseq [b ["main" "f" "s"]
                  :let [o  (ask db b [:log/range {:own? true}])
                        on (set (map :branch o))]]
            (is (= 1 (count on)) b)
            (is (= (filter #(on (:branch %)) (ask db b [:log/range])) o) b)))
        (testing ":kinds and :after apply"
          (is (= [3] (own "f" {:kinds #{:conformance/b}})))
          (is (= [6] (own "f" {:after (-> rf :basis :rev)}))))
        (testing ":newest? reads backward and still answers only with own entries"
          (is (= [6 3] (own "f" {:newest? true :limit 2})))
          (is (= [6 2] (own "f" {:kinds #{:conformance/a} :newest? true :limit 2})))
          (is (= [4 1] (own "main" {:newest? true :limit 5}))
              "a sibling's newer entries are not own")))
      (testing "a branch forked again under a retired name starts with no entries"
        (api/branch! db {:op :retire :name "s" :expect-rev (:rev (head db "s"))})
        (api/branch! db {:op :fork :name "s" :from (head db "main")})
        (is (empty? (own "s")))))))

(defn- kv-write! [db fx]
  (let [h (engine/begin db :write nil)] (commit/apply! db h fx) (engine/commit! db h)))

(defn- kv-of [db k]
  (let [h (engine/begin db :read nil)]
    (try (kv/kv-get db h k) (finally (engine/rollback! db h)))))

(defn- unkinded!
  [db b]
  (let [h  (engine/begin db :read nil)
        ks (try (kv/kv-scan db h [:log-kind b] {}) (finally (engine/rollback! db h)))]
    (kv-write! db (conj (mapv (fn [[k]] (kv/kv-fx [:log-kind b k] nil)) ks)
                        (kv/kv-fx (kv/log-kinded-k b) nil)))))

(defn- old-commit!
  [db b log]
  (let [m  (kv-of db (kv/log-kinded-k b))
        r  (tx! db b {:log log})
        rv (-> r :basis :rev)]
    (kv-write! db (conj (vec (map-indexed (fn [i {:keys [kind]}]
                                            (kv/kv-fx (kv/log-kind-k b kind rv i) nil))
                                          log))
                        (kv/kv-fx (kv/log-kinded-k b) m)))
    r))

(defn check-log-kind [make]
  (testing "log by kind: one kind of a branch's own log, through its mark and after it"
    (let [db    (fresh make)
          own   (fn [b o] (ask db b [:log/range (merge {:own? true} o)]))
          agree (fn [b k after]
                  (doseq [o [{} {:after after} {:newest? true :limit 2} {:limit 1}
                             {:newest? true :limit 99} {:after after :limit 1}]]
                    (is (= (own b (assoc o :kinds #{k :conformance/none}))
                           (own b (assoc o :kinds #{k})))
                        [b k o])))
          all   (fn [b after]
                  (doseq [k [:conformance/a :conformance/b]] (agree b k after)))
          mark  #(:rev (kv-of db (kv/log-kinded-k %)))
          a     (fn [p] {:kind :conformance/a :payload p})]
      (tx! db {:log [(a 1) {:kind :conformance/b :payload 2}]})
      (api/branch! db {:op :fork :name "f" :from (head db "main")})
      (let [r3 (tx! db "f" {:log [(a 3)]})]
        (tx! db "f" {:log [{:kind :conformance/b :payload 4} (a 5)]})
        (testing "a commit moves the mark with its branch"
          (is (= (:rev (head db "f")) (mark "f")))
          (is (= (:rev (head db "main")) (mark "main")))
          (all "main" 0) (all "f" (-> r3 :basis :rev)))
        (testing "what code that writes no index committed is read after the mark"
          (old-commit! db "f" [(a 6)])
          (is (< (mark "f") (:rev (head db "f"))))
          (is (= [3 5 6] (map :payload (own "f" {:kinds #{:conformance/a}}))))
          (tx! db "f" {:log [(a 7)]})
          (is (< (mark "f") (:rev (head db "f"))) "a mark behind its head stays behind")
          (all "f" (-> r3 :basis :rev)))
        (testing "a branch older than the index reads its whole log"
          (unkinded! db "main")
          (is (nil? (mark "main")))
          (all "main" 0))
        (testing "index! makes every branch whole through its head, once"
          (let [out (log-kind/index! db)]
            (is (= #{"main" "f"} (set (keys out))))
            (is (= (:rev (head db "main")) (mark "main")))
            (is (= (:rev (head db "f")) (mark "f")))
            (all "main" 0) (all "f" (-> r3 :basis :rev))
            (is (empty? (log-kind/index! db)))))
        (testing "an adopted branch reads the entries it took over"
          (api/branch! db {:op :adopt :name "f" :from {:branch "main"}
                           :expect-rev (:rev (head db "f"))})
          (is (= [1] (map :payload (own "f" {:kinds #{:conformance/a}}))))
          (all "f" 0))
        (testing "a branch forked again under a retired name reads none of the old"
          (api/branch! db {:op :fork :name "s" :from (head db "main")})
          (tx! db "s" {:log [(a 8)]})
          (api/branch! db {:op :retire :name "s" :expect-rev (:rev (head db "s"))})
          (api/branch! db {:op :fork :name "s" :from (head db "main")})
          (is (empty? (own "s" {:kinds #{:conformance/a}}))))))))

(defn check-read-at [make]
  (testing "read-at: an answer is a function of the basis"
    (let [db (fresh make)
          b0 (head db "main")
          w0 (ask db [:schema/world])
          h0 (ask db [:schema/history {}])]
      (let [b1 (:basis (tx! db {:ops [(create widget 90 {:name "n"})]}))]
        (is (identical? (api/read-at db b0 nil [:schema/world])
                        (api/read-at db b1 nil [:schema/world]))
            "bases that fix one world share one value"))
      (is (= w0 (api/read-at db b0 nil [:schema/world])) "what a snapshot at the basis answers")
      (is (= h0 (api/read-at db b0 nil [:schema/history {}])))
      (tx! db {:schema {:authored (assoc-in world [:object-types other :description] "moved")
                        :migration []}})
      (testing "once the head moves, the basis still answers what it fixed"
        (is (= w0 (api/read-at db b0 nil [:schema/world])))
        (is (= h0 (api/read-at db b0 nil [:schema/history {}]))))
      (testing "and the new head answers the new world"
        (is (= (ask db [:schema/world]) (api/read-at db (head db "main") nil [:schema/world])))
        (is (not= w0 (api/read-at db (head db "main") nil [:schema/world]))))
      (testing "an object read at a basis needs an authority, as on a snapshot"
        (is (= :no-authority
               (:store/refusal (refused #(api/read-at db b0 nil [:objects widget])))))))))

(defn check-fork-history [make]
  (testing "a fork shares its parent's world history, it does not copy it"
    (let [db  (fresh make)
          w   (fn [d] (assoc-in world [:object-types other :description] d))
          r1  (:rev (:basis (tx! db {:schema {:authored (w "a") :migration []}})))
          r2  (:rev (:basis (tx! db {:schema {:authored (w "b") :migration []}})))
          _   (api/branch! db {:op :fork :name "f" :from (head db "main")})
          r3  (:rev (:basis (tx! db "f" {:schema {:authored (w "c") :migration []}})))
          _   (api/branch! db {:op :fork :name "g" :from (head db "f")})
          _   (tx! db "main" {:schema {:authored (w "after") :migration []}})
          at  (fn [b r] (get-in (api/read-at db {:branch b :rev r} nil [:schema/world])
                                [:object-types other :description]))]
      (is (= ["a" "b" "c"] [(at "f" r1) (at "f" r2) (at "f" r3)]) "before, at and after the fork")
      (is (= ["a" "b" "c"] [(at "g" r1) (at "g" r2) (at "g" r3)]) "a fork of a fork, through both")
      (is (= "c" (get-in (ask db "g" [:schema/world]) [:object-types other :description]))
          "the head")
      (is (= "after" (get-in (ask db "main" [:schema/world]) [:object-types other :description]))
          "the parent moves on alone"))))

(defn check-refusals [make]
  (testing "refusals"
    (let [db (fresh make)]
      (testing "an effect is no store op"
        (is (= :malformed (:store/refusal (refused #(tx! db {:ops [{:op :conformance/noop}]}))))))
      (testing "an object key taken keeps its 409 and its data"
        (tx! db {:ops [(create keyed 60 {:code "K"})]})
        (let [d (refused #(tx! db {:ops [(create keyed 61 {:code "K"})]}))]
          (is (= :object-key-taken (:store/refusal d)))
          (is (= (id 60) (:taken-by d)))))
      (testing "an update of an object that is gone is stale, not silently lost"
        (let [d (refused #(tx! db {:ops [{:op :update-object :type widget
                                          :payload {:id (id 99) :values {:size 1}}}]}))]
          (is (= :stale-object (:store/refusal d)))
          (is (:retryable? d))))
      (testing "an id is the same id whether a caller holds it as UUID or string"
        (tx! db {:ops [{:op :update-object :type widget
                        :payload {:id (parse-uuid (id 1)) :values {:size 12}}}]})
        (is (= 12
               (get-in (first (ask db [:objects widget [(parse-uuid (id 1))]])) [:values :size]))))
      (testing "an object read without authority is 403"
        (is (= :no-authority
               (:store/refusal (refused #(with-open [s (api/snapshot db {:branch "main"})]
                                           (api/read s nil [:objects widget])))))))
      (testing "an unknown branch is 404"
        (is (= :no-branch (:store/refusal (refused #(head db "nowhere"))))))
      (testing "a question nobody answers is unsupported"
        (is (= :unsupported (:store/refusal (refused #(ask db [:conformance/nonsense]))))))
      (testing "capabilities name the four vocabularies"
        (is (every? (api/capabilities db "main") [:objects :schema :log :records]))))))

(defn check-layout [make]
  (testing "layout ( S2, S4)"
    (let [db (fresh make)
          n  #(count (ask db [:objects widget]))
          n0 (n)]
      (testing "storage of a declared type is no orphan"
        (is (empty? (:orphans (ask db [:store/layout])))))
      (testing "[:drop-unclaimed] keeps every type the world declares"
        (tx! db {:schema {:authored world :migration [[:drop-unclaimed]]}})
        (is (= n0 (n))))
      (testing "it drops the storage of a type the world no longer declares"
        (tx! db {:schema {:authored (update world :object-types dissoc widget) :migration []}})
        (is (= 1 (count (:orphans (ask db [:store/layout])))) "named as the engine stores it")
        (tx! db
             {:schema {:authored (update world :object-types dissoc widget)
                       :migration [[:drop-unclaimed]]}})
        (is (empty? (:orphans (ask db [:store/layout]))))))))

(def fk-world
  (let [plane "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000fa001"
        part  "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000fa002"
        crew  "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000fa003"]
    {:object-types
     {plane {:object-type-name :plane
             :attribute-list [{:attribute-name :reg :attribute-type :string}]}
      part  {:object-type-name :part  :attribute-list [{:attribute-name :sn :attribute-type :string}
                                                       {:attribute-name :reg
                                                        :attribute-type :string}]}
      crew  {:object-type-name :crew
             :attribute-list [{:attribute-name :regs :attribute-type [:array :string]}]}}
     :link-types
     {"ri.sutra.main.link-type.00000000-0000-0000-0000-0000000fa004"
      {:link-type-name :part-plane :cardinality :many-to-one :from-object-type part
       :to-object-type plane
       :forward-api-name :plane :reverse-api-name :parts
       :fk {:holder part :target plane :side :from :attr :reg :target-attr :reg}}
      "ri.sutra.main.link-type.00000000-0000-0000-0000-0000000fa005"
      {:link-type-name :crew-plane :cardinality :many-to-one :from-object-type crew
       :to-object-type plane
       :forward-api-name :planes :reverse-api-name :crews
       :fk {:holder crew :target plane :side :from :attr :regs :target-attr :reg}}}}))

(defn check-fk-links
  [make]
  (testing "fk-derived links"
    (let [db      (make collections)
          [plane part crew] (sort (keys (:object-types fk-world)))
          [pp cp] (sort (keys (:link-types fk-world)))
          links   (fn [L] (pairs (rd db [:links L])))]
      (submit db {:world fk-world
                  :tx-vector [(create plane 1 {:reg "D-A"}) (create plane 2 {:reg "D-B"})
                              (create part 3 {:sn "S1" :reg "D-A"})
                              (create part 4 {:sn "S2" :reg nil})
                              (create crew 5 {:regs ["D-A" "D-B"]})]})
      (is (= #{[(id 3) (id 1)]} (links pp)) "the key's value names the pair; a nil names none")
      (is (= #{[(id 5) (id 1)] [(id 5) (id 2)]} (links cp)) "an array key links each element")
      (is (= 1 (rd db [:links pp {:count? true}])) "a derived pair is counted without a link table")
      (is (= 2 (rd db [:links cp {:count? true}]))
          "an array key's count is its PAIRS, not the rows that hold the key")
      (submit db
              {:tx-vector [{:op :update-object :type part
                            :payload {:id (id 3) :values {:reg "D-B"}}}]})
      (is (= #{[(id 3) (id 2)]} (links pp)) "the pair follows the key's value")
      (let [l (first (rd db [:links pp]))]
        (is (= :unsupported
               (:store/refusal (refused #(submit db
                                                 {:tx-vector
                                                  [{:op :delete-link :type pp
                                                    :payload {:id (:id l)}}]}))))
            "a derived pair is not deleted: its foreign key is set")
        (is (= :unsupported
               (:store/refusal
                (refused #(submit db
                                  {:tx-vector [{:op :create-link :type pp
                                                :payload
                                                {:id "x" :from-id (id 4) :to-id (id 1)}}]}))))
            "nor written")
        (is (= #{[(id 3) (id 2)]} (links pp)) "a refused op leaves the pairs"))
      (submit db {:tx-vector [{:op :delete-object :type crew :payload {:id (id 5)}}]})
      (is (= #{} (links cp)) "a holder deleted takes its pairs along")
      (submit db {:tx-vector [(create crew 5 {:regs ["D-A" "D-B"]})]})
      (is (= #{[(id 5) (id 1)] [(id 5) (id 2)]} (links cp))
          "a holder created again under its id has its pairs again: its deletion left no pair gone")
      (is (= 2 (rd db [:links cp {:count? true}]))
          "a holder gone and back moves the count both ways")
      (submit db
              {:tx-vector [{:op :update-object :type part
                            :payload {:id (id 4) :values {:reg "D-C"}}}]})
      (is (= 1 (rd db [:links pp {:count? true}])) "a key that names no plane makes no pair")
      (submit db {:tx-vector [(create plane 6 {:reg "D-C"})]})
      (is (= 2 (rd db [:links pp {:count? true}]))
          "a row on the TARGET end moves the count with no holder touched"))))

(defn check-drops-with
  [make]
  (testing "records that go with a retired branch"
    (let [db   (make collections)
          sfx  (subs (str (random-uuid)) 0 8)
          gone (str "gone-" sfx)
          kept (str "keep/" sfx)]
      (doseq [b [gone kept]]
        (api/branch! db {:op :fork :name b :from (head db "main")}))
      (tx! db {:records [{:coll :conformance/pointer :key (str "m" sfx) :value {:branch "main"}}
                         {:coll :conformance/pointer :key (str "g" sfx) :value {:branch gone}}
                         {:coll :conformance/pointer :key (str "k" sfx) :value {:branch kept}}]})
      (doseq [b [gone kept]]
        (api/branch! db {:op :retire :name b :expect-rev (:rev (head db b))}))
      (is (= #{(str "m" sfx) (str "k" sfx)}
             (into #{} (comp (map first) (filter #(.endsWith ^String % sfx)))
                   (ask db [:records/list :conformance/pointer {}])))
          "the retired branch's record is gone, the kept one and main's stay"))))

(defn check-unique
  [make]
  (testing "unique groups"
    (let [db (make collections)
          put (fn [k v] {:coll :conformance/handle :key k :value v})]
      (tx! db {:records [(put "a" {:owner "o" :name "n"})]})
      (is (= :conflict
             (:store/refusal (refused #(tx! db {:records [(put "b" {:owner "o" :name "n"})]}))))
          "a second key for a taken group is refused")
      (is (= :conflict
             (:store/refusal (refused #(tx! db {:records [(put "c" {:owner "p" :name "n"})
                                                          (put "d" {:owner "p" :name "n"})]}))))
          "so are two in one commit")
      (tx! db {:records [(put "a" {:owner "o" :name "n" :note 1})]})
      (tx! db {:records [(put "e" {:owner "o" :name nil}) (put "f" {:owner "o" :name nil})]})
      (tx! db {:records [(put "a" nil) (put "g" {:owner "o" :name "n"})]})
      (is (= #{"e" "f" "g"} (set (map first (ask db [:records/list :conformance/handle {}]))))
          "its own rewrite, groups with a nil, and a group freed in the same commit are fine"))))

(defn check-clock
  [make]
  (testing "a type's clock"
    (let [db    (fresh make)
          upd   (fn [T n vs] {:op :update-object :type T :payload {:id (id n) :values vs}})
          clock (fn [b T] (ask db b [:objects/clock T]))
          rev   (fn [b] (:rev (head db b)))
          r0    (rev "main")]
      (is (= r0 (clock "main" widget) (clock "main" other)) "the seed changed both")
      (tx! db {:ops [(upd widget 1 {:size 5})]})
      (let [r1 (rev "main")]
        (is (= [r1 r0] [(clock "main" widget) (clock "main" other)]))
        (tx! db {:ops [(create other 7 {:name "x"})]})
        (is (= [r1 (rev "main")] [(clock "main" widget) (clock "main" other)])
            "a commit that does not touch widget leaves its clock")
        (is (= r1 (clock "main" widget)) "asked again, the same")
        (api/branch! db {:op :fork :name "f" :from (head db "main")})
        (is (= r1 (clock "f" widget)) "a fork starts with its parent's clock")
        (tx! db "f" {:ops [(upd widget 2 {:size 6})]})
        (is (= (rev "f") (clock "f" widget)))
        (is (= r1 (clock "main" widget)) "a branch's change is not main's")))))

(defn check-records-clock
  [make]
  (testing "a records collection's clock"
    (let [db    (fresh make)
          clock #(ask db [:records/clock %])
          note  (fn [k t] {:coll :conformance/note :key k :value {:text t}})
          c0    (clock :conformance/note)]
      (tx! db {:records [(note "a" "1")]})
      (let [c1 (clock :conformance/note)]
        (is (not= c0 c1) "a write moves it")
        (is (= c1 (clock :conformance/note)) "asked again, the same")
        (tx! db {:records [{:coll :conformance/entry :key api/auto :value {:text "e"}}]})
        (is (= c1 (clock :conformance/note)) "another collection's write leaves it")
        (tx! db {:ops [(create widget 9 {:name "w"})]})
        (is (= c1 (clock :conformance/note)) "a commit without records leaves it")
        (tx! db {:records [(note "a" "2")]})
        (is (not= c1 (clock :conformance/note)) "an update moves it like an insert")))))

(def priced "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000c0f5")

(def ^:private decimal-world
  {:object-types
   {priced {:object-type-name :priced
            :attribute-list [{:attribute-name :price   :attribute-type [:decimal 12 2]}
                             {:attribute-name :tiny    :attribute-type [:decimal 4 2]}
                             {:attribute-name :plain   :attribute-type :decimal}
                             {:attribute-name :amounts :attribute-type [:array [:decimal 12 2]]}
                             {:attribute-name :parts
                              :attribute-type [:struct {:p [:decimal 12 2] :d :double}]}
                             {:attribute-name :ratios  :attribute-type [:array :double]}]}}})

(defn- as-written
  [values]
  (clojure.walk/postwalk #(if (decimal? %) (.toPlainString ^java.math.BigDecimal %) %) values))

(defn check-decimals [make]
  (testing "a value is held as its type declares, alike in every store: a decimal at its scale, also in an array or a struct; an array without nil"
    (let [s   (doto (make collections) (submit {:world decimal-world}))
          get #(:values (first (rd s [:objects priced [(id %)]])))]
      (submit s {:tx-vector [(create priced 1 {:price 1.005M :plain 1.12345678905M
                                               :amounts [1.005M 2.5M] :parts {:p 1.005M :d 2.5}
                                               :ratios [0.5 1.25]})
                             (create priced 2 {:price -1.005M})]})
      (is (= {:price "1.01" :plain "1.1234567891" :amounts ["1.01" "2.50"] :parts {:p "1.01" :d 2.5}
              :ratios [0.5 1.25]}
             (as-written (select-keys (get 1) [:price :plain :amounts :parts :ratios])))
          "rounded half away from zero to the scale, a double element left a double")
      (is (= "-1.01" (as-written (:price (get 2)))) "half away from zero below it too")
      (testing "a value with more whole digits than the precision leaves is refused"
        (is (= :malformed
               (:store/refusal
                (thrown-data #(submit s {:tx-vector [(create priced 3 {:tiny 123.45M})]})))))
        (is (nil? (get 3))))
      (testing "an array holds no nil element"
        (is (= :malformed
               (:store/refusal
                (thrown-data #(submit s {:tx-vector [(create priced 4 {:ratios [0.5 nil]})]})))))
        (is (nil? (get 4)))))))

(def valued "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000c0f6")

(def ^:private value-world
  {:object-types
   {valued {:object-type-name :valued
            :object-key {:properties [:code] :source :primary-key}
            :attribute-list [{:attribute-name :code :attribute-type :string}
                             {:attribute-name :t    :attribute-type :string}
                             {:attribute-name :x    :attribute-type :double}
                             {:attribute-name :b    :attribute-type :boolean}
                             {:attribute-name :at   :attribute-type :timestamp}]}}})

(def ^:private long-name (keyword (apply str (repeat 32 "ü"))))

(defn check-values
  [make]
  (testing "values: a store hands back what it was given, or the core refuses it"
    (let [s    (doto (make collections) (submit {:world value-world}))
          get  #(:values (first (rd s [:objects valued [(id %)]])))
          at   #inst "2024-02-29T23:59:59.123Z"
          rows {1 {:code "Müller" :t "Müller 😀 ≥ ½" :x -0.0   :b true  :at at}
                2 {:code "muller" :t ""              :x 1.0E300 :b false}
                3 {:code "a"      :t "a"             :x 5.0E-324}
                4 {:code "a "     :t "a "}}]
      (submit s {:tx-vector (for [[n vs] rows] (create valued n vs))})
      (doseq [[n vs] rows, [k v] vs :when (not= :at k)]
        (is (= (pr-str v) (pr-str (k (get n)))) (str k " of " n " reads back unchanged")))
      (is (= (inst-ms at) (inst-ms (:at (get 1)))) "an instant is its millisecond")
      (is (= 4 (count (rd s [:objects valued (mapv id (keys rows))])))
          "keys that differ only in case, accent or a trailing blank are four objects")
      (testing "text compares exactly and orders by code point, in every store"
        (is (= [(id 2)] (mapv :id (q s {:root valued :where [:= :code "muller"]}))))
        (is (= [(id 3)] (mapv :id (q s {:root valued :where [:= :t "a"]}))) "\"a \" is not \"a\"")
        (submit s
                {:tx-vector (map-indexed (fn [i t]
                                           (create valued (+ 20 i) {:code (str "o" i) :t t}))
                                         ["😀" "\uFFFD" "ä" "a" "Z" "B"])})
        (is (= ["B" "Z" "a" "ä" "\uFFFD" "😀"]
               (vals-of :t (q s {:root valued :where [:starts-with :code "o"] :order-by :t})))
            "by code point: an emoji after U+FFFD, which UTF-16 order puts before it"))
      (testing "what not every store keeps is refused before any store sees it"
        (doseq [[n vs] {5 {:code "nul" :t "a\u0000b"}
                        6 {:code "sur" :t (str "x" (char 0xD800))}
                        7 {:code "nan" :x ##NaN}
                        8 {:code "inf" :x ##-Inf}
                        9 {:code "rat" :t "r" :x 1/3}}]
          (let [r (thrown-data #(submit s {:tx-vector [(create valued n vs)]}))]
            (is (= :malformed (:store/refusal r)) (pr-str vs))
            (is (= (id n) (:id r)) "named by id")
            (is (nil? (get n)))))
        (testing "a key longer than every store can index"
          (let [k (apply str (repeat 451 "ü"))]
            (is (= :malformed
                   (:store/refusal
                    (thrown-data #(submit s {:tx-vector [(create valued 10 {:code k})]})))))
            (is (= :malformed
                   (:store/refusal
                    (thrown-data
                     #(submit s {:tx-vector [{:op :create-object :type valued
                                              :payload {:id k :values {:code "long-id"}}}]})))))
            (submit s {:tx-vector [(create valued 11 {:code (subs k 1)})]})
            (is (= (subs k 1) (:code (get 11))) "900 bytes are a key")))
        (is (= :malformed
               (:store/refusal
                (thrown-data #(submit s
                                      {:world
                                       (update-in value-world
                                                  [:object-types valued :attribute-list]
                                                  conj
                                                  {:attribute-name long-name
                                                   :attribute-type :string})}))))
            "an attribute name of more than 63 bytes")))))

(defn check-bulk
  [make]
  (testing "a commit larger than any statement"
    (let [db (fresh make)
          n  17000]
      (tx! db
           {:ops (for [i (range n)]
                   (create widget (+ 1000 i) {:name (str "w" i) :size i :owner "bulk"}))})
      (is (= [{:group {} :metrics {:count n}}]
             (q db {:root widget :where [:= :owner "bulk"] :aggregate :count :limit 1}))))))

(def ^:private cost-sizes
  [150 450])

(def ^:private cost-slack
  12)

(def ^:private request-sizes
  [1 50])

(defn- make-of
  [{:keys [engine db]}]
  (fn [collections] (db (engine) collections)))

(defn- cold
  [{:keys [db]} e]
  (db e collections))

(defn- history-engine
  [o n]
  (let [e  (:engine o)
        e  (e)
        db (doto (cold o e)
             (tx! {:schema {:authored world :migration []} :ops seed :origin :index}))]
    (api/branch! db {:op :fork :name "early" :from (head db "main")})
    (dotimes [i n]
      (let [entry {:log [{:kind :conformance/a :payload i}]}]
        (if (zero? (mod i 10))
          (tx! db
               (assoc entry
                      :schema
                      {:authored (assoc-in world [:object-types other :description] (str "d" i))
                       :migration []}))
          (tx! db (assoc entry :ops [(create widget (+ 1000 i) {:name (str "w" i)})])))))
    e))

(def cost-currencies
  [:calls :rows])

(defn- slope-verdict
  [{:keys [what sizes]} currency a b allowed why lesson]
  (when (> b allowed)
    {:what what :currency currency :small a :big b :allowed allowed :sizes sizes
     :error (str what ": " (name currency) " went " a " -> " b " when " why
                 " — allowed at most " allowed ". " lesson)}))

(defn cost-verdicts
  ([m] (cost-verdicts m cost-slack))
  ([{:keys [what sizes small big unit] :as m} slack]
   (let [[small-n big-n] sizes
         why (if unit
               (str "the data grew " small-n " -> " big-n " " unit)
               (str "the history grew " small-n " -> " big-n " commits"))]
     (or (seq (keep (fn [[side cost]]
                      (when-let [u (:uncounted cost)]
                        {:what what :currency :uncounted :sizes sizes
                         :error (str what ": " u " answer(s) at the " (name side)
                                     " size could not be measured without forcing them, so"
                                     " this law was not judged — a currency the counter"
                                     " never reported is not a free pass")}))
                    {:small small :big big}))
         (into [] (keep (fn [currency]
                          (let [a (long (get small currency 0))
                                b (long (get big currency 0))]
                            (slope-verdict m currency a b (+ a slack) why
                                           (str "A cost that follows the answer stays flat;"
                                                " this one followed the data behind it.")))))
               cost-currencies)))))

(defn proportional-verdicts
  ([m] (proportional-verdicts m cost-slack))
  ([{:keys [sizes small big] :as m} slack]
   (let [[small-n big-n] sizes
         ratio (/ (double big-n) (max 1 small-n))
         why   (str "the request grew " small-n " -> " big-n " items")]
     (into [] (keep identity)
           [(slope-verdict m :calls (long (get small :calls 0)) (long (get big :calls 0))
                           (+ (long (get small :calls 0)) slack) why
                           "A batch is one op, however many items it names.")
            (slope-verdict m :rows (long (get small :rows 0)) (long (get big :rows 0))
                           (long (+ (* ratio (get small :rows 0)) slack)) why
                           "Rows may grow with the request, but only with it.")]))))

(def ^:private cost-laws
  [{:what "a world at a fork base far behind the head"
    :why  "the basis is placed from the store's own bookkeeping, never by walking between"
    :run  (fn [db] (let [base (get-in (ask db [:branch/get "early"]) [:base :rev])]
                     #(api/read-at db {:branch "main" :rev base} nil [:schema/world])))
    :ok   map?}
   {:what "the newest commits of a long history"
    :why  "a descending window over a frontier, stopping at :limit (newest-commits)"
    :run  (fn [db] #(ask db [:schema/history {:limit 5}]))
    :ok   #(= 5 (count %))}
   {:what "branch tips for one branch and for many"
    :run  (fn [db] #(ask db [:branch/tips ["main" "early" "nope"]]))
    :ok   #(= 3 (count %))}
   {:what "the newest log entries of a long log"
    :why  "a window from one end, never the log (windowed-log)"
    :run  (fn [db] #(ask db [:log/range {:newest? true :limit 5 :own? true}]))
    :ok   #(= 5 (count %))}
   {:what "the oldest log entries of a long log"
    :why  "either end needs the window: a limit applied after the scan is no limit"
    :run  (fn [db] #(ask db [:log/range {:limit 5 :own? true}]))
    :ok   #(= 5 (count %))}
   {:what "a type's clock"
    :why  "a committed revision's clock never changes, so a new head costs one key"
    :run  (fn [db] #(ask db [:objects/clock widget]))
    :ok   some?}
   {:what "one record by its key"
    :run  (fn [db] #(ask db [:records/get :conformance/note "k"]))
    :prepare (fn [db] (tx! db {:records [{:coll :conformance/note :key "k" :value {:text "v"}
                                          :expect api/absent}]}))
    :ok   #(= "v" (:text %))}
   {:what "how many objects of a type there are"
    :why  "a number is asked of the engine, not counted row by row — where the engine can"
    :needs :select
    :run  (fn [db] #(ask db [:objects/count widget]))
    :ok   pos?}
   {:what "one object committed onto a long history"
    :why  "a commit is fenced on the heads it read, not on everything behind them"
    :run  (fn [db] #(tx! db {:ops [(create widget 7777 {:name "z"})]}))
    :ok   #(some? (:basis %))}])

(def ^:private request-laws
  [{:what "objects named by id"
    :why  "ids go to the engine in one request"
    :run  (fn [db ids] #(ask db [:objects widget ids]))
    :ok   (fn [n v] (= n (count v)))}])

(defn- measure
  [o e law]
  (when-let [p (:prepare law)] (p (cold o e)))
  (let [db (cold o e)
        [v t] (api/tallying ((:run law) db))]
    [v (api/totals t)]))

(defn measure-cost
  [o laws]
  (let [caps (api/capabilities (fresh (make-of o)) "main")
        [small-n big-n] cost-sizes
        engines (into {} (for [n cost-sizes] [n (history-engine o n)]))]
    (for [{:keys [what why needs ok] :as law} laws]
      (if (and needs (not (contains? caps needs)))
        {:what what :why why :skipped (str "the engine declares no " needs)}
        (let [[v-small small] (measure o (engines small-n) law)
              [v-big big]     (measure o (engines big-n) law)]
          {:what what :why why :sizes cost-sizes :small small :big big
           :answers-ok (boolean (and (ok v-small) (ok v-big)))})))))

(defn measure-requests
  [o laws]
  (let [[small-n big-n] request-sizes
        e (history-engine o (first cost-sizes))]
    (for [{:keys [what why ok run]} laws]
      (let [
            made (->> (range) (remove #(zero? (mod % 10))) (map #(id (+ 1000 %))))
            ids  (fn [n] (vec (take n made)))
            m   (fn [n] (let [db (cold o e)
                              [v t] (api/tallying (run db (ids n)))]
                          [v (api/totals t)]))
            [v-small small] (m small-n)
            [v-big big]     (m big-n)]
        {:what what :why why :sizes request-sizes :small small :big big
         :answers-ok (boolean (and (ok small-n v-small) (ok big-n v-big)))}))))

(defn- report-cost
  [measured verdicts-of]
  (doseq [{:keys [what why skipped answers-ok] :as m} measured]
    (testing (str what (when why (str " — " why)))
      (if skipped
        (is true (str what ": not judged here, " skipped))
        (do (is answers-ok (str what ": the answer itself, at both sizes"))
            (let [broken (verdicts-of m)]
              (doseq [currency (conj cost-currencies :uncounted)
                      :let [v (first (filter #(= currency (:currency %)) broken))]]
                (when (or v (not= :uncounted currency))
                  (is (nil? v)
                      (or (:error v) (str what ": " (name currency) " kept its law"))))))))))
  measured)

(defn check-cost
  [o]
  (testing "cost: a question follows its answer, not the data behind it"
    (binding [sutra.store.core.engine/*verify* false]
      (concat (report-cost (vec (measure-cost o cost-laws)) cost-verdicts)
              (report-cost (vec (measure-requests o request-laws)) proportional-verdicts)))))

(declare check-store*)

(defn check-store
  [o]
  (check-cost o)
  (check-store* (make-of o)))

(defn- check-store*
  [make]
  (check-objects #(make collections))
  (check-snapshots make)
  (check-history make)
  (check-interface make)
  (check-edits make)
  (check-world-value make)
  (check-resource-history make)
  (check-fences make)
  (check-atomicity make)
  (check-branches make)
  (when (nil? (:base (ask (fresh make) [:branch/get "main"])))
    (check-inheritance make))
  (check-lineage make)
  (check-inherited-edits make)
  (check-clock make)
  (check-records-clock make)
  (check-names make)
  (check-overlay make)
  (check-overlay-world make)
  (check-records make)
  (check-layout make)
  (check-log make)
  (check-log-own make)
  (check-log-kind make)
  (check-read-at make)
  (check-fork-history make)
  (check-refusals make)
  (check-fk-links make)
  (check-unique make)
  (check-decimals make)
  (check-values make)
  (check-bulk make))
