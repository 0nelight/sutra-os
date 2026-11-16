(ns ^:unit store.certify-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.certify :as certify]
            [sutra.store.core :as core]
            [sutra.store.core.engine :as core-engine]
            [sutra.store.kv :as kv]
            [sutra.store.kv-engine :as kv-engine]
            [sutra.store.mem :as mem]
            [sutra.store.model-engine :as model-engine]
            [sutra.store.shapes :as shapes]
            [sutra.store.spi :as spi]))

(defn- db [engine collections]
  (doto (core/db engine {:collections collections}) core/adopt!))

(defn- result [driver]
  (let [r (certify/certify driver)]
    (is (shapes/valid? :driver/certification r) (pr-str (shapes/explain :driver/certification r)))
    (is (:passed? r) (pr-str (into {} (remove #(= :proven (:status (val %)))) (:results r))))
    r))

(deftest the-kv-engine-over-mem-is-certified
  (let [r (result {:make mem/engine :db db :steps 200 :seed 7})]
    (is (= (:kv core-engine/certificates) (:claims r))
        "certificates.edn :kv is what certifying the kv-engine proves")))

(deftest a-claim-without-a-certificate-is-refused
  (let [e (mem/engine)
        bold (reify spi/Engine
               (-profile [_] (assoc (spi/-profile e) :explain? true)))]
    (is (= :duty/profile
           (:store/engine-contract
            (try (core/db bold {}) nil
                 (catch clojure.lang.ExceptionInfo x (ex-data x))))))))

(deftest the-model-certifies-itself-with-records
  (result {:make #(model-engine/engine #{:certify/notes :certify/counters}) :db db
           :records #{:certify/notes} :auto-records :certify/counters
           :steps 60 :seed 3}))

(deftest a-driver-that-forgets-a-delete-is-caught
  (let [forgetful (fn []
                    (let [e (kv-engine/engine (kv/mem))]
                      (reify spi/Engine
                        (-profile [_] (spi/-profile e))
                        (-begin [_ m l] (spi/-begin e m l))
                        (-lock! [_ h ks] (spi/-lock! e h ks))
                        (-next-rev [_ h] (spi/-next-rev e h))
                        (-commit! [_ h] (spi/-commit! e h))
                        (-rollback! [_ h] (spi/-rollback! e h))
                        (-kv-read [_ h ks] (spi/-kv-read e h ks))
                        (-kv-write! [_ h w] (spi/-kv-write! e h w))
                        (-kv-scan [_ h s p o] (spi/-kv-scan e h s p o))
                        (-kv-parts [_ h s] (spi/-kv-parts e h s))
                        (-ns-exists? [_ h n] (spi/-ns-exists? e h n))
                        (-ns-fork! [_ h f t c] (spi/-ns-fork! e h f t c))
                        (-ns-drop! [_ h n] (spi/-ns-drop! e h n))
                        (-layout! [_ h c ch] (spi/-layout! e h c ch))
                        (-inspect [_ h c] (spi/-inspect e h c))
                        (-rows [_ h c T ids] (spi/-rows e h c T ids))
                        (-links [_ h c L w] (spi/-links e h c L w))
                        (-write-rows! [_ h c T r] (spi/-write-rows! e h c T r))
                        (-delete-rows! [_ _ _ _ _] nil)
                        (-write-links! [_ h c L l] (spi/-write-links! e h c L l))
                        (-delete-links! [_ h c L i] (spi/-delete-links! e h c L i)))))
        r (certify/certify {:make forgetful :db db :steps 200 :seed 7})]
    (testing "the model check names the duty and the step"
      (is (not (:passed? r)))
      (is (= :failed (get-in r [:results :certify/model :status])))
      (is (#{:duty/rows :duty/history}
           (get-in r [:results :certify/model :duty]))))))
