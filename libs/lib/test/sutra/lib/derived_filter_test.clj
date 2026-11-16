(ns sutra.lib.derived-filter-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.store.derived :as derived]
            [sutra.lib.object-set-test :as os-test]
            [sutra.lib.store :as store]
            [sutra.lib.test-store :as ts]
            [sutra.store.api :as api]))

(def ent
  (update-in os-test/ent [:schema :objects :aircraft :attributes] into
             [[:any-open :boolean
               {:derived {:expression [:connected? :defects [:= :status "open"]]}}]
              [:last-summary :string {:derived {:expression [:connected-max :defects :summary]}}]
              [:twice-open :integer
               {:derived {:expression [:* 2 [:connected-count :defects [:= :status "open"]]]}}]
              [:open-share :double
               {:derived {:expression [:div [:connected-count :defects [:= :status "open"]]
                                            [:connected-count :defects]]}}]
              [:open-plus :integer {:derived {:expression [:+ :open-defects 1]}}]
              [:rounded :integer {:derived {:expression [:round [:connected-count :defects]]}}]]))

(deftest derived-properties-are-computed-where-the-rows-are
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower ent) {:clock clock})
        ac    (lib/type-rid W :aircraft)
        sub!  #(store/submit! db W %1 %2 {:clock clock})
        scan  {:op :scan :root ac}
        regs  #(mapv (fn [o] (get-in o [:values :registration])) (store/select db W %))
        where #(regs {:op :order :order-by :registration :in {:op :filter :where % :in scan}})]
    (store/put! db ac (for [r ["D-AINA" "D-AIXB" "OE-LBA"]] {:id r :values {:registration r}}))
    (doseq [[reg summary] [["D-AINA" "Seat loose"] ["D-AINA" "Bird strike"]
                           ["D-AINA" "Window cracked"]
                           ["D-AIXB" "Scratch"]]]
      (sub! :report-defect {:aircraft reg :summary summary :severity "minor"}))
    (sub! :defer-all-minor {:aircraft "D-AIXB"})
    (let [read (into {} (map (juxt #(get-in % [:values :registration]) :values))
                     (store/select db W scan))]
      (testing "the reference: every aircraft read with its derived values"
        (is (= {"D-AINA" 3 "D-AIXB" 0 "OE-LBA" 0} (update-vals read :open-defects)))
        (is (= {"D-AINA" true "D-AIXB" false "OE-LBA" false} (update-vals read :any-open)))
        (is (= {"D-AINA" "Window cracked" "D-AIXB" "Scratch" "OE-LBA" nil}
               (update-vals read :last-summary))))
      (testing "any comparison on a count"
        (is (= ["D-AINA"] (where [:> :open-defects 2])))
        (is (= [] (where [:> :open-defects 3])))
        (is (= ["D-AIXB" "OE-LBA"] (where [:= :open-defects 0])))
        (is (= ["D-AIXB"] (where [:and [:starts-with :registration "D-"] [:< :open-defects 1]]))))
      (testing "connected? compared with a boolean"
        (is (= ["D-AINA"] (where [:= :any-open true])))
        (is (= ["D-AIXB" "OE-LBA"] (where [:= :any-open false]))))
      (testing "a max over a text attribute, by code point"
        (is (= ["D-AINA"] (where [:> :last-summary "Seat"]))))
      (testing "ordering on a derived property, ties broken by id"
        (is (= ["D-AINA" "OE-LBA" "D-AIXB"]
               (regs {:op :order :order-by :open-defects :sort-dir :desc :in scan})))
        (is (= ["D-AINA" "D-AIXB"]
               (regs {:op :order :order-by :last-summary :sort-dir :desc
                      :in {:op :filter :where [:nil? :last-summary false] :in scan}}))
            "by the column the filter already computed; OE-LBA has no summary"))
      (testing "the rows carry the declared values only — no plan column leaks"
        (is (every? #(every? simple-keyword? (keys (:values %)))
                    (store/select db W {:op :filter :where [:> :open-defects 0] :in scan})))))
    (testing "arithmetic over folds is a :compute column"
      (is (= ["D-AINA"] (where [:> :twice-open 4])))
      (is (= ["D-AINA"] (where [:= :open-share 1.0])))
      (is (= ["D-AIXB"] (where [:= :open-share 0.0]))
          "OE-LBA has no defects: 0/0 is nil, which no comparison holds of")
      (is (= ["D-AIXB" "D-AINA" "OE-LBA"] (regs {:op :order :order-by :open-share :in scan}))
          "0.0, 1.0, and nil last under :asc")
      (is (= ["D-AINA"] (where [:> :open-plus 1])) "a derived property over another"))
    (testing "the read agrees with the columns"
      (is (= {"D-AINA" 1.0 "D-AIXB" 0.0 "OE-LBA" nil}
             (into {}
                   (map (juxt #(get-in % [:values :registration])
                              #(get-in % [:values :open-share])))
                   (store/select db W scan)))))
    (testing "an aggregate folds and groups by derived properties, under their own names"
      (let [agg #(first (store/select db W (merge {:op :aggregate :in scan} %)))]
        (is (== 1
                (get-in (agg {:aggs [{:fn :avg :attr :open-defects}]})
                        [:metrics :open-defects-avg])))
        (is (== 1.0
                (get-in (agg {:aggs [{:fn :max :attr :open-share}]}) [:metrics :open-share-max])))
        (is (= [[0 2] [3 1]]
               (mapv (fn [b]
                       [(long (get-in b [:group :open-defects]))
                        (long (get-in b [:metrics :count]))])
                     (store/select db W
                                   {:op :aggregate :aggs [{:fn :count}] :group-by [:open-defects]
                                    :in scan}))))))
    (testing "what the plan language cannot state is refused"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"rounded, a derived property .* cannot state"
                            (store/select db W {:op :filter :where [:> :rounded 0] :in scan})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"rounded, a derived property .* cannot state"
                            (store/select db W
                                          {:op :aggregate :aggs [{:fn :sum :attr :rounded}]
                                           :in scan}))
          "in an aggregate too, where the store would have folded nils"))
    (when ts/pg?
      (testing "Postgres takes the plans whole"
        (with-open [s (api/snapshot db {:branch "main"})]
          (doseq [ir [{:op :filter :where [:> :open-defects 2] :in scan}
                      {:op :order :order-by :open-defects :sort-dir :desc :in scan}
                      {:op :filter :where [:= :any-open false] :in scan}
                      {:op :filter :where [:> :twice-open 4] :in scan}
                      {:op :order :order-by :open-share
                       :in {:op :filter :where [:> :open-plus 1] :in scan}}
                      {:op :aggregate :aggs [{:fn :count}] :group-by [:open-defects] :in scan}
                      {:op :aggregate :aggs [{:fn :avg :attr :open-share}] :in scan}]]
            (let [e (api/read s store/system [:select/explain (derived/plan W ir)])]
              (is (:pushdown? e) (pr-str ir))
              (is (= [[]] (:claims e)) (pr-str ir)))))))))
