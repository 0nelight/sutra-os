(ns sutra.lib.fence-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.lib.mro-test :as mro]
            [sutra.lib.store :as store]
            [sutra.store.api :as api]
            [sutra.lib.test-store :as ts]))

(defn- refusal [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:store/refusal (ex-data e)))))

(deftest a-stale-decision-is-refused-and-decided-again
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower mro/ent) {:clock clock})
        ac    (lib/type-rid W :aircraft)
        df    (lib/type-rid W :defect)
        opts  {:clock clock}
        _     (store/put! db ac [{:id "D-AINA" :values {:registration "D-AINA"}}])
        d1    (-> (store/submit! db W :report-defect
                                 {:aircraft "D-AINA" :summary "Smoke" :severity "minor"} opts)
                  :ops first :payload :id)
        head  #(with-open [s (api/snapshot db {:branch "main"})] (api/basis s))
        status #(get-in (first (store/read db W df [d1])) [:values :status])]
    (testing "Alma decides to defer the minor defect"
      (with-open [s (api/snapshot db {:branch "main"})]
        (let [{:keys [tx]} (store/decide s W :defer-defect {:defect d1} opts)]
          (is (some #(= [:object df d1] (subvec % 0 3)) (:fences tx))
              "the defect she read is fenced")
          (testing "— meanwhile Ben finds it is critical"
            (api/transact! db {:basis (head) :actor {:principal "ben"}
                               :ops [{:op :update-object :type df
                                      :payload {:id d1 :values {:severity "critical"}}}]}))
          (testing "Alma's commit is refused: what she decided on has moved"
            (is (= :stale-object (refusal #(api/transact! db (assoc tx :basis (api/basis s))))))
            (is (= "open" (status)))))))
    (testing "submit! decides again on what is there now, and the criterion refuses"
      (is (= "A critical defect cannot be deferred."
             (-> (store/submit! db W :defer-defect {:defect d1} opts) :error :message)))
      (is (= "open" (status))))))
