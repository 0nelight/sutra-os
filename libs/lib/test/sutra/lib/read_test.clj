(ns sutra.lib.read-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.lib.object-set-test :as os-test]
            [sutra.lib.store :as store]
            [sutra.lib.test-store :as ts]
            [sutra.store.api :as api]))

(def ent
  (update-in os-test/ent [:schema :objects :aircraft :attributes] into
             [[:hours :integer {}]
              [:half :integer {:derived {:expression [:round [:div :hours 2]]}}]
              [:per-zero :integer
               {:derived {:expression [:round [:div [:connected-count :defects] 0]]}}]]))

(deftest derived-values-read-nil-or-refuse
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower ent) {:clock clock})
        ac    (lib/type-rid W :aircraft)
        sub!  #(store/submit! db W :report-defect {:aircraft "D-AINA" :summary % :severity "minor"}
                              {:clock clock})]
    (store/put! db ac [{:id "D-AINA" :values {:registration "D-AINA"}}
                       {:id "OE-LBA" :values {:registration "OE-LBA" :hours 7}}])
    (testing "an unset attribute and a zero divisor read nil; a set one computes"
      (let [by-reg (into {} (map (juxt :id :values)) (store/read db W ac ["D-AINA" "OE-LBA"]))]
        (is (nil? (get-in by-reg ["D-AINA" :half])))
        (is (= 4 (get-in by-reg ["OE-LBA" :half])))
        (is (nil? (get-in by-reg ["D-AINA" :per-zero])))
        (is (nil? (get-in by-reg ["D-AINA" :hours])))))
    (testing "a refusal that is not about one row's data is raised"
      (sub! "Seat loose")
      (sub! "Bird strike")
      (let [bad (update-in W [:object-types ac :attribute-list] conj
                           {:attribute-name :one :attribute-type :string
                            :opts {:derived {:expression [:connected-one :defects :summary]}}})
            e   (try (store/read db bad ac ["D-AINA"]) nil
                     (catch clojure.lang.ExceptionInfo e e))]
        (is (= {:ref/kind :not-one :type ac :id "D-AINA" :attribute :one}
               (select-keys (ex-data e) [:ref/kind :type :id :attribute])))))))

(deftest reads-and-writes-name-their-branch
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower ent) {:clock clock})
        ac    (lib/type-rid W :aircraft)
        on-b  {:branch "b"}]
    (api/branch! db {:op :fork :name "b" :from {:branch "main"}})
    (store/put! db ac [{:id "D-AINA" :values {:registration "D-AINA"}}] on-b)
    (store/submit! db W :report-defect {:aircraft "D-AINA" :summary "Seat loose" :severity "minor"}
                   (assoc on-b :clock clock))
    (is (empty? (store/read db W ac ["D-AINA"])) "main never saw it")
    (is (= 1 (get-in (first (store/read db W ac ["D-AINA"] on-b)) [:values :open-defects])))
    (is (= ["D-AINA"] (map :id (store/select db W {:op :scan :root ac} on-b))))
    (is (= (lib/type-rid W :aircraft) (lib/type-rid (store/installed db on-b) :aircraft)))))
