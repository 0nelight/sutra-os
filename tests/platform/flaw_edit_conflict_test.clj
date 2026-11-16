(ns ^:integration platform.flaw-edit-conflict-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.world :as world]
            [sutra.catalog.kernel :as kernel]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- W [strategy]
  (ri/derived {:sigma clock/system}
              {:object-types
               {:geraet (cond-> {:object-type-name :geraet
                                 :attribute-list [{:attribute-name :nr :attribute-type :string}
                                                  {:attribute-name :ort :attribute-type :string}
                                                  {:attribute-name :geaendert
                                                   :attribute-type :timestamp}]}
                          strategy (assoc :edit-conflict strategy))}}))

(defn- index!
  ([b T values] (index! b T values nil))
  ([b T values stamps]
   (world/commit! (ctx/store b)
                  {:origin :index :actor {:principal "test"} :at-head? true
                   :ops [(cond-> (store/create-object T {:id "g-1" :values values})
                           stamps (assoc-in [:payload :stamps] stamps))]})))

(defn- ort [b T] (get-in (store/object b T "g-1") [:values :ort]))
(defn- nr [b T] (get-in (store/object b T "g-1") [:values :nr]))

(def ^:private past   "2000-01-01T00:00:00Z")
(def ^:private far-future "2999-01-01T00:00:00Z")

(deftest apply-most-recent-is-honoured
  (store/with-fresh-branch [b "edit-conflict"]
    (let [w (W {:strategy :apply-most-recent :timestamp-attr :geaendert})]
      (store/install! b w)
      (let [T (lookup/rid-of w :object-types :geraet)]
        (index! b T {:nr "1" :ort "quelle" :geaendert past})
        (store/update-object! b T "g-1" {:ort "edit"})
        (testing "a source row older than the edit keeps the edit"
          (index! b T {:nr "1" :ort "quelle-alt" :geaendert past})
          (is (= "edit" (ort b T))))
        (testing "a source row newer than the edit overwrites it"
          (index! b T {:nr "1" :ort "quelle-neu" :geaendert far-future})
          (is (= "quelle-neu" (ort b T))))))))

(deftest each-source-is-dated-by-its-own-timestamp
  (store/with-fresh-branch [b "edit-conflict-sources"]
    (let [w (W {:strategy :apply-most-recent :timestamp-attr :geaendert})]
      (store/install! b w)
      (let [T (lookup/rid-of w :object-types :geraet)]
        (index! b T {:nr "1" :ort "quelle" :geaendert past})
        (store/update-object! b T "g-1" {:nr "edit" :ort "edit"})
        (index! b T {:nr "2" :ort "ergaenzt" :geaendert far-future}
                [{:at far-future :attrs [:nr :geaendert]} {:at past :attrs [:ort]}])
        (is (= "2" (nr b T)) "the base source changed :nr after the edit")
        (is (= "edit" (ort b T))
            "the supplement dates :ort before the edit, whatever the base row's timestamp")
        (index! b T {:nr "3" :ort "ergaenzt" :geaendert far-future}
                [{:at far-future :attrs [:nr :geaendert]} {:at nil :attrs [:ort]}])
        (is (= "edit" (ort b T)) "an undated source keeps the edit")))))

(deftest apply-user-edits-stays-the-default
  (store/with-fresh-branch [b "edit-default"]
    (let [w (W nil)]
      (store/install! b w)
      (let [T (lookup/rid-of w :object-types :geraet)]
        (index! b T {:nr "1" :ort "quelle" :geaendert past})
        (store/update-object! b T "g-1" {:ort "edit"})
        (index! b T {:nr "1" :ort "quelle-neu" :geaendert far-future})
        (is (= "edit" (ort b T)) "without a strategy the edit wins")))))

(defn- step-refusal [attrs ts]
  (try (kernel/eval-steps [{:op :create-object-type :name :geraet2 :attributes attrs
                            :edit-conflict {:strategy :apply-most-recent :timestamp-attr ts}
                            :plural "Geräte 2"}]
                          (assoc (ri/derived {:sigma clock/system} {}) :projects {:p {}}) {}
                          {:installer {} :project :p})
       nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest apply-most-recent-needs-a-timestamp-attribute
  (is (= 422 (anomaly/status (step-refusal [[:nr :string] [:ort :string]] :ort)))
      "a string attribute is no timestamp to compare")
  (is (nil? (step-refusal [[:nr :string] [:geaendert :timestamp]] :geaendert))))
