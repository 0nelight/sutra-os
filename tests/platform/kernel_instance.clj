(ns ^:unit platform.kernel-instance
  (:require [support.helpers :as h]
            [sutra.catalog.kernel :as kernel]
            [sutra.store.api :as api]
            [sutra.store.mem :as mem]))

(def ^:private world
  (h/simple-schema
   :link-types {:knows {:link-type-name :knows
                        :from-object-type :person
                        :to-object-type :person
                        :cardinality :many-to-many}}))

(defn- commit! [db ops]
  (let [basis (with-open [s (api/snapshot db {:branch "main"})] (api/basis s))]
    (api/transact! db {:basis basis :actor {:principal "kernel-instance"} :ops ops})))

(defn- stored [db]
  (with-open [s (api/snapshot db {:branch "main"})]
    {:objects (api/read s api/unrestricted [:objects :person])
     :links   (api/read s api/unrestricted [:links :knows])}))

(defn run-tests []
  (println "\n══ R from a run applies in sutra.store.mem ══")

  (h/section "THE PAYOFF — the kernel records values, the in-memory store applies them")
  (let [r   (kernel/eval-steps [{:op :create :object-type :person :values {:name "Bent" :alter 7}
                                 :as :p}
                              {:op :update  :target [:get :p :id] :values {:alter 8}}
                              {:op :create-link :link-type :knows
                               :from-id [:get :p :id] :to-id "other" :as :e}]
                             world {} {})
        db  (mem/store {:objects {} :links {} :world world})
        _   (commit! db (:ops r))
        s   (stored db)
        obj (first (:objects s))]
    (h/check "the store holds exactly one object" (= 1 (count (:objects s))))
    (h/check "object carries the create + update merged values"
             (= {:name "Bent" :alter 8} (:values obj)))
    (h/check "object recorded its type" (= :person (:object-type-ref obj)))
    (h/check "the store holds the created link" (= 1 (count (:links s))))
    (h/check "link links the created object"
             (= (:id obj) (:from-object-id (first (:links s)))))
    (let [del (kernel/eval-step {:op :delete-object :target (:id obj) :object-type :person}
                              world {} {})
          _   (commit! db (:ops del))
          s'  (stored db)]
      (h/check "delete-object removed the object" (empty? (:objects s')))
      (h/check "delete-object cascaded its link" (empty? (:links s')))))

  (println "\n  the recorded ops apply in sutra.store.mem: all checks passed."))
