(ns ^:unit platform.kernel-trace
  (:require [support.helpers :as h]
            [sutra.catalog.kernel :as kernel]))

(def ^:private world
  (h/simple-schema))

(defn- io [trace]
  {:trace trace})

(defn run-tests []
  (println "\n══ kernel trace contract ══")

  (h/section "flat trace incl. nested steps (matches v1's nested execute-rules)")
  (let [tr (atom [])]
    (kernel/eval-steps [{:op :create :id "p1" :object-type :person :values {:name "A" :alter 1}
                         :as :p}
                      {:op :for-each :collection :xs :as :x
                       :steps [{:op :create :id [:get :x :id] :object-type :person
                                :values {:name "n"}}]}]
                     world {:xs [{:id "a"} {:id "b"}]} {} (io tr))
    (h/check "four entries traced (incl. nested)" (= 4 (count @tr)))
    (let [e0 (first @tr)]
      (h/check "entry has :step/:op/:bindings-snapshot/:elapsed-ms"
               (every? e0 [:step :op :bindings-snapshot :elapsed-ms]))
      (h/check "step 0 is the :create" (= [0 :create] [(:step e0) (:op e0)])))
    (let [fe (first (filter #(= :for-each (:op %)) @tr))]
      (h/check ":for-each entry carries op-specific :iterations" (= 2 (:iterations fe)))))

  (h/section "no trace atom ⇒ zero overhead, no entries")
  (let [r (kernel/eval-steps [{:op :create :id "p1" :object-type :person :values {:name "A"}}]
                           world {} {})]
    (h/check "runs fine without :trace" (= 1 (count (:ops r)))))

  (h/section "failure records a self-locating entry and rethrows")
  (let [tr (atom [])
        msg (h/expect-error
             #(kernel/eval-steps [{:op :create :id "ok" :object-type :person :values {:name "A"}}
                                {:op :create :object-type :person :values {:bogus 1}}]
                               world {} {} (io tr)))]
    (h/check "the failing step threw" (some? msg))
    (let [fail (last @tr)]
      (h/check "failure entry is step 1" (= 1 (:step fail)))
      (h/check "failure entry carries :error + :error-class"
               (and (:error fail) (:error-class fail)))))

  (println "\n  kernel trace contract holds."))
