(ns ^:api ontology.stream-shape-notice-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.store.access.watch :as watch]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:schema
   {:projects {:calls {}}
    :functions
    {:call-shape
     {:project  :calls
      :language :clojure
      :inputs   {:records {:kind :scalar}}
      :code     (str "(defn call-shape [{:keys [records]}]"
                     "  (mapv (fn [r] {:call-id (:id r)"
                     "                 :status (:state r)"
                     "                 :number (str \"43\" (subs (:number r) 1))})"
                     "        records))")}}
    :datasets
    {:call_journal
     {:project :calls
      :kind    :standalone
      :columns [{:attribute-name :id :attribute-type :string}
                {:attribute-name :state :attribute-type :string}
                {:attribute-name :number :attribute-type :string}]
      :stream  {:partitions 1 :primary-key {:columns [:id]} :shape :call-shape}}}
    :objects
    {:call {:project    :calls
            :symbol     "📞"
            :plural     "Calls"
            :attributes [[:call-id :string] [:status :string] [:number :string]]
            :backing    {:dataset :call_journal :primary-key :call-id}}}}})

(defn- calls []
  (into {}
        (map (fn [r] [(get-in r [:values :call-id]) (:values r)]))
        (:rows (:body (api/list-objects :call)))))

(defn- publish-and-flush! [record]
  (is (ok? (api/post! :stream-records {:name "call_journal"} {:records [record]})))
  (is (ok? (api/post! :stream-archive {:name "call_journal"} {}))))

(deftest a-shaped-stream-reaches-objects-and-tells-the-watchers
  (let [r (api/post! :install schema)]
    (is (ok? r) (pr-str (:body r))))
  (is (ok? (api/post! :staging)))
  (let [notices (atom [])]
    (with-redefs [watch/notify! (fn [_branch object-type payload]
                                  (swap! notices conj [object-type (:ids payload)]))]
      (testing "the object holds what the shape made of the record"
        (publish-and-flush! {:id "c1" :state "ringing" :number "0664111"})
        (is (= {:call-id "c1" :status "ringing" :number "43664111"}
               (select-keys (get (calls) "c1") [:call-id :status :number]))))
      (testing "a status change is the same object, and the watchers hear of it"
        (publish-and-flush! {:id "c1" :state "up" :number "0664111"})
        (is (= "up" (:status (get (calls) "c1"))))
        (is (= 1 (count (calls))))
        (is (= 2 (count @notices)))
        (is (every? (comp seq second) @notices) "the notice names the objects it wrote")))))

(deftest a-full-object-view-declares-its-auto-refresh
  (let [r (api/post! :install
                     (assoc-in schema [:schema :applications]
                               {:call-view-full
                                {:object-type :call
                                 :form-factor :full
                                 :project     :calls
                                 :live        {:types [:call] :min-seconds 3}
                                 :layout      {:kind :rows :items []}}}))]
    (is (ok? r) (pr-str (:body r))))
  (is (ok? (api/post! :staging)))
  (is (= {:types ["call"] :min-seconds 3}
         (-> (:body (api/get! :applications))
             :applications :call-view-full :live
             (update :types #(mapv name %))))))
