(ns sutra.ref.objectset-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.ref.objectset :as os]
            [sutra.ref.run :as run]))

(def K "ri.sutra.main.object-type.kunde")
(def A "ri.sutra.main.object-type.auftrag")
(def P "ri.sutra.main.interface.partner")
(def L "ri.sutra.main.link-type.kunde-auftrag")

(def world
  {:object-types
   {K {:object-type-name :kunde :implements [P] :title-key :name
       :attribute-list (mapv #(hash-map :attribute-name %) [:name :ort :umsatz :seit])}
    A {:object-type-name :auftrag
       :attribute-list (mapv #(hash-map :attribute-name %) [:titel :betrag :datum])}}
   :interfaces {P {:property-list [{:attribute-name :name}]}}
   :link-types {L {:link-type-name :kunde-auftrag :from-object-type K :to-object-type A
                   :forward-api-name :auftraege :reverse-api-name :kunde}}})

(defn- obj [T id values] {:id id :object-type-ref T :values values})

(def kunden
  [(obj K "k1" {:name "Alma" :ort "Wien" :umsatz 10 :seit "2026-08-05"})
   (obj K "k2" {:name "Bodo" :ort "Graz" :umsatz 30 :seit "2026-08-11"})
   (obj K "k3" {:name "Clara" :ort nil    :umsatz nil})])

(def auftraege
  [(obj A "a1" {:titel "x" :betrag 5})
   (obj A "a2" {:titel "y" :betrag 7})
   (obj A "a3" {:titel "z" :betrag -7})])

(def edges [["k1" "a1"] ["k1" "a2"] ["k2" "a2"] ["k9" "a3"]])

(def sigma
  {:extension (fn [_W T] (get {K kunden A auftraege} T []))
   :edges     (fn [_W l] (if (= l L) edges []))})

(def cx {:sigma sigma})

(defn- read [ir] (os/read cx world ir))
(defn- ids [ir] (set (map :id (read ir))))
(defn- kind [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:ref/kind (ex-data e)))))

(def scan-k {:op :scan :root K})
(def scan-a {:op :scan :root A})
(defn- where [in p] {:op :filter :in in :where p})

(deftest s3-set-operators
  (is (= #{"k1" "k2" "k3"} (ids scan-k)))
  (is (= #{"k2"} (ids {:op :static :root K :ids ["k2" "k7"]})) "ids that are not visible drop out")
  (is (= #{} (ids {:op :static :root K :ids []})) "∅ is the empty set, not 'no filter'")
  (is (= #{"k1"} (ids (where scan-k [:= :ort "Wien"]))))
  (is (= #{"k1" "k2"}
         (ids {:op :union :in [(where scan-k [:= :ort "Wien"]) (where scan-k [:> :umsatz 20])]})))
  (is (= #{"k2"}
         (ids {:op :intersect
               :in [(where scan-k [:nil? :ort false]) (where scan-k [:> :umsatz 20])]})))
  (is (= #{"k3"}
         (ids {:op :subtract
               :in [scan-k (where scan-k [:= :ort "Wien"]) (where scan-k [:= :ort "Graz"])]}))))

(deftest s3-search-around
  (let [from-k1 {:op :static :root K :ids ["k1"]}]
    (is (= #{"a1" "a2"} (ids {:op :search-around :in from-k1 :link :auftraege})) "side name")
    (is (= #{"a1" "a2"} (ids {:op :search-around :in from-k1 :link L :dir :to})) "stored side")
    (is (= #{"a1" "a2"} (ids {:op :search-around :in from-k1 :link :kunde-auftrag}))
        "link key, one direction fits")
    (is (= #{"k1" "k2"}
           (ids {:op :search-around :in {:op :static :root A :ids ["a2"]} :link :kunde})))
    (is (= 2
           (count (read {:op :search-around :in {:op :static :root K :ids ["k1" "k2"]}
                         :link :auftraege})))
        "a set: a2, reached from two origins, is in once")
    (is (= #{} (ids {:op :search-around :in {:op :static :root A :ids ["a3"]} :link :kunde}))
        "an edge to a missing object contributes nothing")
    (is (= :unknown-side (kind #(read {:op :search-around :in scan-k :link :nix}))))))

(deftest s3-interface-extension
  (is (= #{"k1" "k2" "k3"} (ids {:op :scan :root P})))
  (is (= [K] (distinct (map :object-type-ref (read {:op :scan :root P}))))
      "members keep their concrete T")
  (is (= {:name "Alma"} (:values (first (read {:op :scan :root P}))))
      "values are the read type's attributes"))

(deftest d35-one-type-per-set-operator
  (doseq [op [:union :intersect :subtract]]
    (is (= :type-mismatch (kind #(read {:op op :in [scan-k scan-a]}))) (str op)))
  (is (= #{"k1"} (ids {:op :intersect :in [scan-k {:op :static :ids ["k1" "a1"]}]}))
      "an untyped static operand takes the common type"))

(deftest d38-hops-only-over-visible-ends
  (is (= #{} (ids {:op :search-around :in {:op :static :ids ["k9"]} :link L :dir :to}))
      "k9 does not exist: its edge is not traversed")
  (is (= #{"a1" "a2"}
         (ids {:op :search-around :in {:op :static :ids ["k1" "k9"]} :link L :dir :to})))
  (is (= :untyped-read (kind #(read {:op :static :ids ["k1"]})))))

(defn- hv [C]
  (ids {:op :filter :in scan-k :where [:having-all :as C] :include [{:as :as :link :auftraege}]}))

(deftest s3-having-all
  (is (= #{"k1"} (hv ["a1" "a2"])))
  (is (= #{"k1" "k2"} (hv ["a2"])))
  (is (= :unknown-include (kind #(ids {:op :filter :in scan-k :where [:having-all :b ["a1"]]})))))

(deftest d39-having-all-is-a-subset-test
  (is (= #{"k1" "k2"} (hv ["a2" "a2"])) "a repeated id changes nothing")
  (is (= #{"k1" "k2" "k3"} (hv [])) "∅ ⊆ anything: every object")
  (is (= :malformed-object-set
         (kind #(read {:op :filter :in scan-k :where [:having-all :as :titel ["a1"]]
                      :include [{:as :as :link :auftraege}]})))
      "the match attribute is gone"))

(deftest s4-malformed
  (doseq [ir [{:op :nope}
              {:op :scan}
              {:op :scan :root K :in scan-k}
              {:op :scan :root K :extra 1}
              {:op :filter :in scan-k :where [:= :ort]}
              {:op :filter :in scan-k :where [:like :ort "x"]}
              {:op :union :in []}
              {:op :template :param-slots [:x] :in scan-k}
              {:op :page :in scan-k :offset 2}
              {:op :page :in scan-k :limit -1}
              {:op :aggregate :in scan-k :aggs [{:fn :sum}]}
              {:op :aggregate :in scan-k :aggs :count :accuracy :approximate}
              {:op :aggregate :in scan-k :aggs [{:fn :count} {:fn :count}]}
              {:op :aggregate :in scan-k :aggs :count :group-by [[:bucket :seit :decade]]}]]
    (is (= :malformed-object-set (kind #(read ir))) (pr-str ir))))

(deftest d44-unknown-attributes
  (is (= :unknown-attribute (kind #(read (where scan-k [:= :farbe 1])))))
  (is (= :unknown-attribute (kind #(read {:op :order :in scan-k :order-by :farbe}))))
  (is (= :unknown-attribute (kind #(read {:op :project :in scan-k :select [:name :farbe]})))
      ":select too")
  (is (= :unknown-attribute
         (kind #(read {:op :aggregate :in scan-k :aggs [{:fn :sum :attr :farbe}]})))))

(deftest s5-order-page-project
  (is (= ["k1" "k2" "k3"] (map :id (read {:op :order :in scan-k :order-by :umsatz})))
      "nil last under asc")
  (is (= ["k3" "k2" "k1"]
         (map :id (read {:op :order :in scan-k :order-by :umsatz :sort-dir :desc})))
      "nil first under desc")
  (is (= ["k2"]
         (map :id
              (read {:op :page :in {:op :order :in scan-k :order-by :umsatz} :limit 1 :offset 1}))))
  (is (= ["k2" "k3"] (map :id (read {:op :page :in scan-k :limit 5 :offset 1})))
      "offset without order: id ascending")
  (is (= [{:id "k1" :object-type-ref K :values {:ort "Wien"}}]
         (read {:op :project :in (where scan-k [:= :name "Alma"]) :select [:ort]})))
  (is (= {:name "Clara" :ort nil :umsatz nil :seit nil} (:values (last (read scan-k))))
      "every declared attribute is present, unset ones as nil"))

(deftest d34-shaping-only-at-the-top
  (doseq [ir [{:op :union :in [{:op :page :in scan-k :limit 1}]}
              {:op :union :in [{:op :order :in scan-k :order-by :umsatz}]}
              {:op :page :limit 3 :in {:op :page :limit 1 :in scan-k}}
              {:op :aggregate :aggs :count :in {:op :page :limit 1 :in scan-k}}
              {:op :project :select [:name] :in {:op :aggregate :aggs :count :in scan-k}}]]
    (is (= :malformed-object-set (kind #(read ir))) (pr-str ir)))
  (is (= ["k3"]
         (map :id
              (read {:op :order :order-by :umsatz :sort-dir :desc
                     :in {:op :page :limit 1 :in scan-k}})))
      "order and page in either nesting: sort, then window"))

(defn- agg [in aggs & {:as more}] (read (merge {:op :aggregate :in in :aggs aggs} more)))

(deftest s6-buckets-and-metrics
  (is (= [{:group {} :metrics {:umsatz-sum 40 :umsatz-count 2 :count 3}}]
         (agg scan-k [{:fn :sum :attr :umsatz} {:fn :count :attr :umsatz} {:fn :count}])))
  (is (= [{:group {:ort "Graz"} :metrics {:count 1}} {:group {:ort "Wien"} :metrics {:count 1}}
          {:group {:ort nil} :metrics {:count 1}}]
         (agg scan-k [{:fn :count}] :group-by [:ort])) "nil is a bucket of its own, sorted last")
  (is (= [{:group {} :metrics {:umsatz-sum 0 :umsatz-avg nil :umsatz-max nil}}]
         (agg (where scan-k [:= :ort "Linz"])
              [{:fn :sum :attr :umsatz} {:fn :avg :attr :umsatz} {:fn :max :attr :umsatz}]))
      "G = []: one bucket even over ∅; sum of nothing is 0, avg/max nil")
  (is (= 20M (get-in (first (agg scan-k [{:fn :avg :attr :umsatz}])) [:metrics :umsatz-avg]))
      "avg is exact")
  (is (= [{:group {:seit-week "2026-W32"} :metrics {:n 1}}
          {:group {:seit-week "2026-W33"} :metrics {:n 1}}
          {:group {:seit-week nil} :metrics {:n 1}}]
         (agg scan-k [{:fn :count :name :n}] :group-by [[:bucket :seit :week]]))))

(deftest s6-order-and-window
  (let [by-ort (fn [_ k] (read {:op :order :order-by k
                                :in {:op :aggregate :in scan-k :group-by [:ort]
                                     :aggs [{:fn :sum :attr :umsatz}]}}))]
    (is (= [nil "Wien" "Graz"] (map #(get-in % [:group :ort]) (by-ort :order-by :umsatz-sum)))
        "the nil-ort bucket sums to 0")
    (is (= ["Graz" "Wien"] (map #(get-in % [:group :ort])
                                (read {:op :page :limit 2
                                    :in {:op :order :order-by :umsatz-sum :sort-dir :desc
                                         :in {:op :aggregate :in scan-k :group-by [:ort]
                                              :aggs [{:fn :sum :attr :umsatz}]}}}))))
    (is (= :bad-aggregate-order (kind #(by-ort :order-by :name))))))

(deftest d40-width-floors
  (is (= #{5 -10}
         (set (map #(get-in % [:group :betrag-w5])
                   (agg scan-a :count :group-by [[:width :betrag 5]]))))
      "-7 lands in [-10, -5)"))

(deftest d42-sum-of-nothing-is-zero
  (is (= {:ort nil} (:group (last (agg scan-k [{:fn :sum :attr :umsatz}] :group-by [:ort])))))
  (is (= {:umsatz-sum 0} (:metrics (last (agg scan-k [{:fn :sum :attr :umsatz}] :group-by [:ort]))))
      "k3 is the only member of the nil-ort bucket and has no umsatz"))

(deftest d46-count-format
  (is (= [{:group {} :metrics {:count 3}}] (agg scan-k :count)))
  (is (= [{:group {} :metrics {:count 3}}] (agg scan-k [{:fn :count}]))))

(deftest d47-bucket-limit-before-the-window
  (let [many (mapv #(obj K (str "x" %) {:umsatz %}) (range 10001))
        cx   {:sigma (assoc sigma :extension (fn [_W T] (if (= K T) many [])))}]
    (is (= :bucket-limit-exceeded
           (try (os/read cx world
                         {:op :page :limit 1
                          :in {:op :aggregate :in scan-k :aggs :count :group-by [:umsatz]}})
                nil (catch clojure.lang.ExceptionInfo e (:ref/kind (ex-data e)))))
        "a page of one bucket does not hide 10 001")))

(deftest d49-strings-by-code-point
  (let [cx {:sigma (assoc sigma :extension
                          (fn [_W T]
                            (if (= K T)
                              (map-indexed #(obj K (str "k" %1) {:name %2})
                                           ["alma" "Bodo" "\uFFFD" "\uD83D\uDE00"]) [])))}]
    (is (= ["Bodo" "alma" "\uFFFD" "\uD83D\uDE00"]
           (map #(get-in % [:values :name])
                (os/read cx world {:op :order :in scan-k :order-by :name})))
        "upper before lower; U+1F600 after U+FFFD (UTF-16 units would put it before)")
    (is (= ["Bodo"]
           (map #(get-in % [:values :name]) (os/read cx world (where scan-k [:< :name "alma"]))))
        "a range comparison in P uses the same order")))

(deftest s6-origin-and-link-terms
  (let [hop {:op :search-around :in {:op :static :root K :ids ["k1" "k2"]} :link :auftraege}]
    (is (= [{:group {:origin "k1"} :metrics {:betrag-sum 12}}
            {:group {:origin "k2"} :metrics {:betrag-sum 7}}]
           (agg hop [{:fn :sum :attr :betrag}] :group-by [[:origin]]))
        "a2 counts once per origin")
    (is (= :no-origin (kind #(agg scan-a :count :group-by [[:origin]])))))
  (is (= [{:group {:kunde {:id "k1" :title "Alma"}} :metrics {:count 2}}
          {:group {:kunde {:id "k2" :title "Bodo"}} :metrics {:count 1}}
          {:group {:kunde nil} :metrics {:count 1}}]
         (agg scan-a [{:fn :count}] :group-by [[:link :kunde]]))
      "a3 has no visible kunde and lands in the nil bucket"))

(deftest d48-parallel-edges-count-once
  (let [cx  {:sigma (assoc sigma :edges (fn [_W l] (if (= l L) (conj edges ["k1" "a1"]) [])))}
        rd  (fn [ir] (os/read cx world ir))
        hop {:op :search-around :in {:op :static :root K :ids ["k1"]} :link :auftraege}]
    (is (= #{"a1" "a2"} (set (map :id (rd hop)))))
    (is (= [{:group {:origin "k1"} :metrics {:count 2}}]
           (rd {:op :aggregate :in hop :aggs [{:fn :count}] :group-by [[:origin]]})))
    (is (= {:group {:kunde {:id "k1" :title "Alma"}} :metrics {:count 2}}
           (first (rd {:op :aggregate :in scan-a :aggs [{:fn :count}]
                       :group-by [[:link :kunde]]}))))))

(defn- query [sets n args]
  (run/run-rules sigma {} (assoc world :object-sets sets) {}
                 [{:op :query :object-set n :arguments args :as :r}]))

(deftest s7-k-query
  (let [sets {"ri.sutra.main.object-set.wien"
              {:object-set-name :in-ort :description "flat form with a slot"
               :root K :where [:= :ort [:param :wo]] :order-by :name
               :param-slots [{:name :wo :type :string}]}}
        r    (query sets :in-ort {:wo "Wien"})]
    (is (= ["k1"] (map :id (get-in r [:bindings :r]))))
    (is (= [{:op :query :object-set :in-ort :result-count 1}]
           (map #(select-keys % [:op :object-set :result-count]) (:events r))))
    (is (= ["k1" "k2"] (map :id (get-in (query sets :in-ort {:wo ["Wien" "Graz"]}) [:bindings :r])))
        "a list argument becomes a disjunction")
    (is (= [] (get-in (query sets :in-ort {:wo []}) [:bindings :r]))
        "an empty list matches nothing")
    (is (= :unknown-object-set (get-in (query sets :nix {}) [:error :kind])))))

(deftest s7-load-limit
  (let [sets {:alle {:root K}}
        two  [{:op :query :object-set :alle :as :r1} {:op :query :object-set :alle :as :r2}]
        run  #(run/run-rules sigma {:objects-loaded-max %} (assoc world :object-sets sets) {} two)]
    (is (nil? (:error (run 6))) "3 + 3 = 6 is within the limit")
    (is (= :limit (get-in (run 5) [:error :kind])) "the second read crosses it")
    (let [r (run/run-rules sigma {:objects-loaded-max 1}
                           (assoc world :object-sets {:n {:root K :aggregate :count}})
                           {} [{:op :query :object-set :n :as :r}])]
      (is (= [{:group {} :metrics {:count 3}}] (get-in r [:bindings :r]))
          "an aggregate loads its buckets, not its members"))))

(deftest d37-typed-value-slots
  (let [t    (fn [where & [slots]]
               {:s {:op :template :param-slots (or slots [{:name :x :type :string}])
                   :in {:op :filter :in scan-k :where where}}})
        q    (fn [sets args] (query sets :s args))
        kind (fn [r] (get-in r [:error :kind]))]
    (is (= :malformed-object-set (kind (q (t [:= :ort :x] [{:name :x :optional? true}]) {})))
        "a declared slot used bare (the form before) is refused, never read as a literal")
    (is (= [] (get-in (q (t [:= :ort :y] [{:name :x :optional? true}]) {}) [:bindings :r]))
        "a keyword that is no slot is a literal")
    (is (= :missing-argument (kind (q (t [:= :ort [:param :x]]) {}))))
    (is (= :unknown-argument (kind (q (t [:= :ort [:param :x]]) {:x "Wien" :y 1}))))
    (is (= :type-mismatch (kind (q (t [:= :ort [:param :x]]) {:x 7}))))
    (is (= ["k1"]
           (map :id
                (get-in (q (t [:= :ort [:param :x]] [{:name :x :optional? true :default "Wien"}])
                           {})
                        [:bindings :r]))) "an optional slot takes its default")
    (is (= :malformed-object-set (kind (q (t [:= [:param :x] "Wien"]) {:x "ort"})))
        "never in a name position")
    (is (= ["k2"] (map :id (get-in (query {:s {:op :template :param-slots [:ids]
                                              :in {:op :static :root K :ids [:param :ids]}}} :s
                                          {:ids ["k2"]})
                                   [:bindings :r]))) "static :ids")))

(deftest d45-flat-keys
  (is (= :unknown-key (get-in (query {:s {:root K :bogus 1}} :s {}) [:error :kind])))
  (is (= :unknown-key (get-in (query {:s {:root K :via [:x]}} :s {}) [:error :kind])))
  (is (= 3
         (count (get-in (query {:s {:root K :description "resource keys are fine" :id :s}} :s {})
                        [:bindings :r])))))

(defn- with [in nm f & {:as more}] (merge {:op :with :in in :name nm :fn f :link :auftraege} more))

(deftest with-adds-a-column-the-set-reads-filters-and-orders-by
  (let [n (with scan-k :n :count)]
    (testing "every member keeps its place; the column is the count of its neighbours"
      (is (= {"k1" 2 "k2" 1 "k3" 0} (into {} (map (juxt :id #(get-in % [:values :n]))) (read n)))))
    (testing "a filter above it reads it as an attribute"
      (is (= #{"k1"} (ids (where n [:> :n 1]))))
      (is (= #{"k3"} (ids (where n [:= :n 0])))))
    (testing "an order and a projection too"
      (is (= ["k1" "k2" "k3"] (map :id (read {:op :order :order-by :n :sort-dir :desc :in n}))))
      (is (= [{:n 2} {:n 1} {:n 0}]
             (map :values (read {:op :project :select [:n]
                                 :in {:op :order :order-by :n :sort-dir :desc :in n}})))))))

(deftest with-folds-like-an-aggregate
  (testing "its :where narrows the neighbours, its :fn folds an attribute of theirs"
    (let [r (fn [f & more] (into {} (map (juxt :id #(get-in % [:values :x])))
                                 (read (apply with scan-k :x f more))))]
      (is (= {"k1" 1 "k2" 1 "k3" 0} (r :count :where [:> :betrag 6])))
      (is (= {"k1" 12 "k2" 7 "k3" 0} (r :sum :attr :betrag)) "a sum of nothing is 0")
      (is (= {"k1" 7 "k2" 7 "k3" nil} (r :max :attr :betrag)) "a max of nothing is nil")
      (is (= {"k1" 6M "k2" 7M "k3" nil} (r :avg :attr :betrag)))))
  (testing "the edge to a3 starts at k9, which is not visible: it counts for nobody"
    (is (= 3 (reduce + (map #(get-in % [:values :n]) (read (with scan-k :n :count)))))))
  (testing "it answers exactly what :aggregate grouped by [:origin] answers"
    (let [by-origin (read {:op :aggregate :aggs [{:fn :sum :attr :betrag :name :s}]
                           :group-by [[:origin]]
                           :in {:op :search-around :link :auftraege :in scan-k}})]
      (is (= (into {} (map (juxt #(get-in % [:group :origin]) #(get-in % [:metrics :s]))) by-origin)
             (into {}
                   (keep (fn [o]
                           (when (pos? (get-in o [:values :s])) [(:id o) (get-in o [:values :s])])))
                   (read (with scan-k :s :sum :attr :betrag))))))))

(deftest with-is-checked
  (is (= :malformed-object-set (kind #(read (with scan-k :name :count))))
      "a column may not shadow an attribute")
  (is (= :malformed-object-set (kind #(read (with (with scan-k :n :count) :n :count))))
      "nor another column")
  (is (= :malformed-object-set (kind #(read (with scan-k :n :sum)))) "a sum needs :attr")
  (is (= :unknown-attribute (kind #(read (with scan-k :n :sum :attr :preis)))) "of the far type")
  (is (= :unknown-attribute
         (kind #(read (where {:op :union :in [(with scan-k :n :count) scan-k]} [:> :n 0]))))
      "a set operator ends what the column is seen by"))

(defn- compute [in nm e] {:op :compute :in in :name nm :expr e})

(deftest compute-is-arithmetic-over-columns-and-attributes
  (let [cols (-> scan-k (with :n :count) (with :s :sum :attr :betrag))
        val  (fn [ir k] (into {} (map (juxt :id #(get-in % [:values k]))) (read ir)))]
    (testing "two columns and a number"
      (is (= {"k1" 14 "k2" 8 "k3" 0} (val (compute cols :t [:+ :n :s]) :t)))
      (is (= {"k1" 4 "k2" 2 "k3" 0} (val (compute cols :t [:* 2 :n]) :t))))
    (testing "an attribute beside a column"
      (is (= {"k1" 8 "k2" 29 "k3" nil} (val (compute cols :t [:- :umsatz :n]) :t))
          "a nil operand is nil, not an error: k3 has no :umsatz"))
    (testing "a quotient is a double; a zero divisor is nil"
      (is (= {"k1" 6.0 "k2" 7.0 "k3" nil} (val (compute cols :t [:div :s :n]) :t))))
    (testing "coalesce takes the first value"
      (is (= {"k1" 10 "k2" 30 "k3" 0} (val (compute cols :t [:coalesce :umsatz :n]) :t))))
    (testing "a filter and an order read it"
      (let [t (compute cols :t [:+ :n :s])]
        (is (= #{"k1"} (ids (where t [:> :t 10]))))
        (is (= ["k1" "k2" "k3"] (map :id (read {:op :order :order-by :t :sort-dir :desc :in t}))))))
    (testing "a column computed from a computed column"
      (is (= {"k1" 28 "k2" 16 "k3" 0}
             (val (compute (compute cols :t [:+ :n :s]) :u [:* 2 :t]) :u))))))

(deftest compute-is-checked
  (is (= :malformed-object-set (kind #(read (compute scan-k :t [:pow :umsatz 2]))))
      "an op it does not know")
  (is (= :malformed-object-set (kind #(read (compute scan-k :t [:div :umsatz])))) "arity")
  (is (= :malformed-object-set (kind #(read (compute scan-k :t {:a 1})))) "a map is no operand")
  (is (= :unknown-attribute (kind #(read (compute scan-k :t [:+ :n 1]))))
      "a column it does not see")
  (is (= :malformed-object-set (kind #(read (compute scan-k :ort [:+ :umsatz 1]))))
      "nor shadow an attribute"))
