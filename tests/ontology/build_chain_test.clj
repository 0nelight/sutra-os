(ns ^:api ontology.build-chain-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [sutra.compute.sandbox :as sandbox]
            [support.build-scenario :as bs]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private txn-ds  "ri.sutra.main.dataset.b09e4c10-98c8-3452-a011-b640f3fa6790")
(def ^:private wm-ds   "ri.sutra.main.dataset.ad94deca-83fc-3d6b-bb24-b03e85929ddc")

(def ^:private ok? bs/ok?)
(def ^:private objs bs/objs)
(def ^:private col bs/col)

(deftest build-chain
  (if-not (sandbox/available?)
    (println "  (build-chain SKIPPED — python sandbox unavailable)")
    (do
      (bs/install-chain! {:echo :clojure})
      (let [import-1 (bs/run-and-assert-convergence!)
            run-id   (get-in import-1 [:body :run-id])]
        (testing "transaction ledger (A.5): do-run! stamped the build's writes"
          (let [txns  (:rows (:body (api/get! :dataset-preview {:id txn-ds})))
                wms   (:rows (:body (api/get! :dataset-preview {:id wm-ds})))
                types (map #(get-in % [:values :type]) txns)]
            (is (<= 3 (count txns))
                "one transaction per written object dataset (thing/summary/note)")
            (is (every? #{"SNAPSHOT"} types)
                "full-refresh object outputs are SNAPSHOT transactions")
            (is (every? #(= "COMMITTED" (get-in % [:values :status])) txns)
                "committed status")
            (is (some #(pos? (long (or (get-in % [:values :row_delta]) 0))) txns)
                "row_delta recorded (SNAPSHOT-everywhere is legible)")
            (is (every? #(seq (str (get-in % [:values :snapshot_id]))) txns)
                "every transaction carries the build's snapshot")
            (is (seq wms) "a consumer-pointer was advanced (mk-thing consumed :seed)")
            (is (every? #(seq (str (get-in % [:values :last_snapshot_id]))) wms)
                "the consumer-pointer points at a snapshot (the delta low-bound)")

            (testing "catalog read endpoints (A.7): transactions / consumption / delta"
              (let [ds-rid (get-in (first txns) [:values :dataset_rid])
                    via    (:body (api/get! :catalog-transactions
                                            (cond-> {:dataset ds-rid} nil (assoc :limit nil))))
                    cons   (:body (api/get! :catalog-consumption {:transform "mk-thing"}))
                    delta  (:body (api/get! :catalog-delta {:transform "mk-thing" :input "seed"}))]
                (is (seq (:transactions via))
                    "GET /catalog/transactions returns a written dataset's chain")
                (is (= "SNAPSHOT" (:type (first (:transactions via))))
                    "endpoint carries the typed transaction")
                (is (seq (:consumption cons))
                    "GET /catalog/consumption returns mk-thing's input consumer-pointer")
                (is (map? (:delta delta))
                    "GET /catalog/delta responds with a delta descriptor")))

            (testing "object-type name redirects to its producer's chain (not empty)"
              (let [by-ot (:body (api/get! :catalog-transactions
                                           (cond-> {:dataset "thing"} nil (assoc :limit nil))))]
                (is (seq (:transactions by-ot))
                    "an object-type name resolves to its producer's transaction chain")
                (is (= "SNAPSHOT" (:type (first (:transactions by-ot))))
                    "the redirected chain carries the real typed transactions")
                (is (= {:object-type "thing" :producer "mk-thing"} (:via by-ot))
                    "the response names the redirect (object-type → producing transform)")))))

        (testing "jobs log covers EVERY transform incl. the TERMINALS (rows_out per job)"
          (let [fk-rid (some (fn [[r t]]
                               (when (= "link-summary-of" (some-> (:transform-name t) name))
                                 (str (or (:id t) r))))
                             (:transforms (:body (api/get! :schema))))
                jobs   (remove #(= fk-rid (:transform_rid %))
                               (:jobs (:body (api/get! :run-jobs {:id run-id}))))]
            (is (<= 3 (count jobs)) "one Job per terminal transform (was: 0 — terminals invisible)")
            (is (every? #(= "COMPLETED" (str (:status %))) jobs) "every terminal Job COMPLETED")
            (is (every? #(= 2 (:rows_out %)) jobs)
                "each terminal wrote 2 rows — rows_out recorded (0 would flag a match bug)")))

        (testing "preview-transform runs a :clojure transform read-only (rows + count, no committed txn)"
          (let [p (:body (api/preview-transform :mk-echo 5))]
            (is (nil? (:error p)) "the Clojure preview ran clean")
            (is (= 2 (:count p)) "it returned the true row count over the current o_thing")
            (is (= 2 (count (:rows p))) "the rows are returned (≤ limit)")
            (is (= #{"t1" "t2"} (set (map :pk (:rows p))))
                "the transform produced the expected keys")))

        (testing "preview-transform runs a :python transform in the real sandbox (read-only)"
          (let [p (:body (api/preview-transform :mk-summary 5))]
            (is (= "python" (name (:language p))) "reports the transform's language")
            (is (nil? (:error p)) "the python preview ran clean in the sandbox")
            (is (= 2 (:count p)) "one summary row per thing, over the current 2 things")
            (is (= 2 (count (:rows p))) "the rows are returned (≤ limit)"))
          (let [nf (api/preview-transform :no-such-transform 5)]
            (is (= 404 (:status nf)) "an unknown transform is a 404")))

        (testing "--limit shapes the OUTPUT only: the transform still reads its inputs whole"
          (let [p (:body (api/preview-transform :mk-summary 1))]
            (is (= 2 (:count p)) "both input rows were read, so count is the true total")
            (is (= 1 (count (:rows p))) "--limit bounds the returned page")
            (is (nil? (:input-sample p)) "no input was sampled")
            (is (empty? (:input-caps p)))))

        (testing "a preview that SAMPLED an input says so, and why"
          (let [capped (:body (api/preview-transform :mk-summary 5 1))]
            (is (= 1 (:count capped)) "one input row in ⇒ one row out")
            (is (= {:rows 1 :why "requested"} (update (:input-sample capped) :why name)))
            (is (= [{:input "things" :kept 1 :total 2}]
                   (mapv #(-> % (update :input name) (select-keys [:input :kept :total]))
                         (:input-caps capped)))
                "the truncated input names itself: which input, kept, of how many"))
          (let [full (:body (api/preview-transform :mk-summary 1 100))]
            (is (= 2 (:count full)) "--sample above the input size feeds the full input")
            (is (= 1 (count (:rows full))) "--limit still bounds the returned page")
            (is (empty? (:input-caps full)) "nothing was capped, so nothing is reported")))))))

(def ^:private iso-good-py
  "from sutra_transforms import transform, Input, Output
@transform(inputs={'seed': Input(kind='dataset', dataset='iso_seed')},
           output=Output(kind='object-type', object_type='good', primary_key='pk'))
def compute(seed):
    return [{'pk': r['tid'], 'n': '1.00'} for r in seed]")

(def ^:private iso-bad-py
  "from sutra_transforms import transform, Input, Output
@transform(inputs={'seed': Input(kind='dataset', dataset='iso_seed')},
           output=Output(kind='object-type', object_type='bad', primary_key='pk'))
def compute(seed):
    return [{'pk': r['tid'], 'n': '-1.00'} for r in seed]")

(defn- iso-schema []
  {:schema
   {:objects
    {:good {:symbol "✅" :attributes [[:pk :string] [:n :decimal]] :plural "Goods"}
     :bad  {:symbol "❌" :attributes [[:pk :string] [:n :decimal {:constraints {:min 0}}]]
            :plural "Bads"}}
    :transforms
    {:mk-good {:language :python :code iso-good-py
               :inputs {:seed {:kind :dataset :dataset :iso_seed}}
               :primary-key :pk :output {:kind :object-type :object-type :good}}
     :mk-bad  {:language :python :code iso-bad-py
               :inputs {:seed {:kind :dataset :dataset :iso_seed}}
               :primary-key :pk :output {:kind :object-type :object-type :bad}}}}})

(deftest publish-isolation
  (if-not (sandbox/available?)
    (println "  (publish-isolation SKIPPED — python sandbox unavailable)")
    (do
      (is (ok? (api/mutate :create-project {:name :iso} {:stage? true}))
          "the project it sits in")
      (is (ok? (api/mutate :create-dataset
                           {:name :iso_seed :project :iso
                            :columns [(col :tid :string) (col :label :string)]}
                           {:stage? true})))
      (is (ok? (api/post! :staging)) "commit dataset")
      (is (ok? (api/post! :install (update (iso-schema) :schema #(in-project :iso %))))
          "install iso schema")
      (is (ok? (api/post! :staging)) "commit schema")
      (is (ok? (api/post! :dataset-write {:name "iso_seed"} {:rows [{:values {:tid "t1" :label "a"}}
                                          {:values {:tid "t2" :label "b"}}]
                                   :message "seed"})))
      (let [imp (api/post! :import-run)]
        (is (= 500 (:status imp))
            "the build FAILS — a group that published nothing is not a completed build")
        (testing "isolation still holds: the CLEAN type committed despite the sibling"
          (is (= 2 (count (objs :good))) "good landed its rows (was 0 under atomic publish)"))
        (testing "the FAILING type is skipped, not partially written"
          (is (= 0 (count (objs :bad))) "bad landed nothing — its group's tx rolled back"))
        (testing "the isolated failure is reported, not silent"
          (let [pf (get-in imp [:body :data :publish-failures])]
            (is (seq pf) "publish-failures names the skipped group")
            (is (some #(#{"bad" "create-bad"} (name (:fn %))) pf)
                "the reported failure names the bad type")))
        (testing "the red run NAMES a red job — no failure without a unit of work"
          (let [run  (first (:runs (:body (api/list-runs "import"))))
                jobs (:jobs (:body (api/get! :run-jobs {:id (:id run)})))
                failed (filter #(= "FAILED" (:status %)) jobs)]
            (is (= "failed" (:status run)))
            (is (seq failed) "the publish failure landed on a Job row")
            (is (some #(re-find #"published nothing" (str (:error %))) failed)
                "and the Job says WHY, in the same words the run does")
            (testing "…and the run row keeps the structured detail, not just a string"
              (is (seq (get-in run [:summary :publish-failures]))))))))))

(def ^:private dup-good-py
  "from sutra_transforms import transform, Input, Output
@transform(inputs={'seed': Input(kind='dataset', dataset='dup_seed')},
           output=Output(kind='object-type', object_type='dgood', primary_key='pk'))
def compute(seed):
    return [{'pk': r['tid'], 'label': r['label']} for r in seed]")

(def ^:private dup-bad-py
  "from sutra_transforms import transform, Input, Output
@transform(inputs={'seed': Input(kind='dataset', dataset='dup_seed')},
           output=Output(kind='object-type', object_type='dbad', primary_key='pk'))
def compute(seed):
    return [{'pk': 'same', 'label': r['label']} for r in seed]")

(defn- dup-schema []
  {:schema
   {:objects
    {:dgood {:symbol "✅" :attributes [[:pk :string] [:label :string]] :plural "Dgoods"}
     :dbad  {:symbol "❌" :attributes [[:pk :string] [:label :string]] :plural "Dbads"}}
    :transforms
    {:mk-dgood {:language :python :code dup-good-py
                :inputs {:seed {:kind :dataset :dataset :dup_seed}}
                :primary-key :pk :output {:kind :object-type :object-type :dgood}}
     :mk-dbad  {:language :python :code dup-bad-py
                :inputs {:seed {:kind :dataset :dataset :dup_seed}}
                :primary-key :pk :output {:kind :object-type :object-type :dbad}}}}})

(deftest data-restrictions-isolated-per-object-type
  (if-not (sandbox/available?)
    (println "  (data-restrictions-isolated-per-object-type SKIPPED — python sandbox unavailable)")
    (do
      (is (ok? (api/mutate :create-project {:name :dup} {:stage? true}))
          "the project it sits in")
      (is (ok? (api/mutate :create-dataset
                           {:name :dup_seed :project :dup
                            :columns [(col :tid :string) (col :label :string)]}
                           {:stage? true})))
      (is (ok? (api/post! :staging)) "commit dataset")
      (is (ok? (api/post! :install (update (dup-schema) :schema #(in-project :dup %))))
          "install dup schema")
      (is (ok? (api/post! :staging)) "commit schema")
      (is (ok? (api/post! :dataset-write {:name "dup_seed"} {:rows [{:values {:tid "d1" :label "a"}}
                                          {:values {:tid "d2" :label "b"}}]
                                   :message "seed"})))
      (let [imp (api/post! :import-run)]
        (is (= 500 (:status imp))
            "the build FAILS — a rejected type published nothing")
        (testing "the CLEAN sibling indexed normally"
          (is (= 2 (count (objs :dgood)))))
        (testing "the REJECTED type published nothing — not the last-write-wins
                  single row the pipeline used to land silently"
          (is (= 0 (count (objs :dbad)))))
        (testing "the rejection is reported, with the violation that caused it"
          (let [pf   (get-in imp [:body :data :publish-failures])
                mine (first (filter #(= "dbad" (name (:fn %))) pf))]
            (is (some? mine) "publish-failures names the rejected object type")
            (is (= 1 (:count mine)) "one violation: the duplicated key")
            (is (re-find #"duplicate-primary-key" (:error mine)))))
        (testing "the rejected type's own Job is the one that is FAILED"
          (let [run  (first (:runs (:body (api/list-runs "import"))))
                jobs (:jobs (:body (api/get! :run-jobs {:id (:id run)})))]
            (is (= "failed" (:status run)))
            (is (seq (filter #(= "FAILED" (:status %)) jobs)))))))))
