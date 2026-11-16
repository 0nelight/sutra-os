(ns ^:unit platform.cli-lineage-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lineage.graph :as lineage]
            [sutra.catalog.backing :as backing]
            [sutra.types.world-rules :as rules]
            [sutra.cli.view :as view]))

(def ^:private body
  {:object-types {"rid.ot.zeit"    {:object-type-name :zeitbuchung :plural "Zeitbuchungen"}
                  "rid.ot.email"   {:object-type-name :email :plural "Emails"}
                  "rid.ot.arbeit"  {:object-type-name :arbeitszeit :plural "Arbeitszeiten"}}
   :datasets     {"rid.ds.raw"     {:dataset-name :raw_messages}
                  "rid.ds.att"     {:dataset-name :raw_attachments}
                  "rid.ds.zeitraw" {:dataset-name :zeitbuchung_raw}}
   :link-types   {"rid.lt.zk"      {:link-type-name :zeitbuchung-kostenstelle}}
   :transforms
   {    "rid.t.rows"  {:transform-name :zeitbuchung-rows
                   :input  {:kind :datasource :datasource :mro}
                   :source {:table "Zeitbuchung"}
                   :output {:kind :object-type :object-type "rid.ot.zeit"}}
    "rid.t.sync"  {:transform-name :zeitbuchung_raw-sync
                   :input  {:kind :datasource :datasource :mro}
                   :source {:query "SELECT a,\n b\nFROM t WHERE …"}
                   :output {:kind :dataset :dataset "rid.ds.zeitraw"}}
    "rid.t.conn"  {:transform-name :link-zeitbuchung-kostenstelle
                   :input  {:kind :object-type :object-type "rid.ot.zeit"}
                   :output {:kind :link-type :link-type "rid.lt.zk"}}
    "rid.t.comp"  {:transform-name :arbeitszeit-rows
                   :inputs {:zeit {:kind :object-type :object-type "rid.ot.zeit"}
                            :raw  {:kind :dataset :dataset "rid.ds.raw"}
                            :llm  {:kind :language-model :model "grok-4"}}
                   :output {:kind :object-type :object-type "rid.ot.arbeit"}}
    "rid.t.pull"  {:transform-name :pull-mailbox
                   :outputs [{:kind :dataset :dataset "rid.ds.raw"
                              :name :raw_messages :primary-key :message_id}
                             {:kind :dataset :dataset "rid.ds.att"
                              :name :raw_attachments :primary-key :att_id}]}}})

(defn- edge [nm]
  (first (filter #(= (name nm) (str (:name %))) (lineage/edges body))))

(deftest lifted-transforms-show-their-datasource-origin
  (testing "object-type :external-source lift — was rendering input-less"
    (let [e (edge :zeitbuchung-rows)]
      (is (= 1 (count (:inputs e))))
      (is (= :datasource (:kind (first (:inputs e)))))
      (is (= "mro" (:name (first (:inputs e)))))
      (is (= "Zeitbuchung" (:table (first (:inputs e)))))
      (is (= "ds:mro.Zeitbuchung" (#'view/node-label (first (:inputs e))))
          "the table is what says WHICH source rows land")))

  (testing "dataset :external-source lift — the Source→Dataset sync"
    (let [e (edge :zeitbuchung_raw-sync)]
      (is (= :datasource (:kind (first (:inputs e)))))
      (is (= [:dataset] (mapv :kind (:outputs e))))
      (is (= ["d:zeitbuchung_raw"] (mapv #'view/node-label (:outputs e))))))

  (testing "link-type :endpoint-fk lift — a singular :input of object-type kind"
    (let [e (edge :link-zeitbuchung-kostenstelle)]
      (is (= "o:zeitbuchung" (#'view/node-label (first (:inputs e)))))
      (is (= ["lt:zeitbuchung-kostenstelle"] (mapv #'view/node-label (:outputs e)))))))

(deftest a-source-query-is-a-marker-not-its-text
  (let [in (first (:inputs (edge :zeitbuchung_raw-sync)))]
    (is (true? (:query? in)))
    (is (nil? (:query in)) "the query TEXT must not ride the DAG")
    (is (= "ds:mro(query)" (#'view/node-label in)))))

(deftest compute-inputs-still-resolve-every-kind
  (let [labels (set (map #'view/node-label (:inputs (edge :arbeitszeit-rows))))]
    (is (= #{"o:zeitbuchung" "d:raw_messages" "llm:grok-4"} labels)
        "the plural :inputs path is unchanged, language-model included")))

(deftest a-genuine-root-stays-input-less
  (is (empty? (:inputs (edge :pull-mailbox)))))

(defn- related-set [kind node]
  (into #{} (map (juxt :kind :name :via)) (:items (lineage/related body kind node))))

(deftest related-answers-for-a-dataset-not-only-an-object-type
  (testing "a dataset names the transforms on both of its sides"
    (let [r (related-set :dataset "raw_messages")]
      (is (contains? r ["transform" "pull-mailbox" "produces"]))
      (is (contains? r ["transform" "arbeitszeit-rows" "consumes"]))))
  (testing "and the object-types it BACKS — the hop that says which ontology
            this table ends up in, and the reason a dataset needed its own arm.
            The backing is declared on the OUTPUT (`:dataset` beside
            `:object-type`), which is where the transaction ledger reads it from
            too, so the two cannot disagree about which table backs which type."
    (let [sch {:object-types {"rid.ot.msg" {:object-type-name :message :plural "Messages"}}
               :datasets     {"rid.ds.raw" {:dataset-name :raw_messages}}
               :transforms
               {"rid.t.pull" {:transform-name :pull-mailbox
                              :output {:kind :object-type :object-type "rid.ot.msg"
                                       :dataset "rid.ds.raw"}}}}]
      (is (contains? (into #{} (map (juxt :kind :name :via))
                           (:items (lineage/related sch :dataset "raw_messages")))
                     ["object-type" "message" "backs"]))))
  (testing "a dataset nothing has written to is empty, not an error"
    (is (empty? (related-set :dataset "no_such_dataset")))))

(deftest related-names-the-relation-not-just-the-node
  (testing "a transform can sit on BOTH sides of a node, so the row has to say
            which — a list that cannot is a list nobody can act on"
    (let [r (related-set :object-type "zeitbuchung")]
      (is (contains? r ["transform" "zeitbuchung-rows" "produces"]))
      (is (contains? r ["transform" "link-zeitbuchung-kostenstelle" "consumes"]))
      (is (contains? r ["transform" "arbeitszeit-rows" "consumes"])))))

(deftest related-of-a-transform-is-the-data-it-touches
  (testing "a transform is an EDGE — its related set is what it reads and writes,
            not the transforms that touch it"
    (let [r (related-set :transform "arbeitszeit-rows")]
      (is (contains? r ["object-type" "zeitbuchung" "reads"]))
      (is (contains? r ["dataset" "raw_messages" "reads"]))
      (is (contains? r ["language-model" "grok-4" "reads"]))
      (is (contains? r ["object-type" "arbeitszeit" "writes"]))))
  (testing "a multi-output producer writes every output"
    (let [r (related-set :transform "pull-mailbox")]
      (is (contains? r ["dataset" "raw_messages" "writes"]))
      (is (contains? r ["dataset" "raw_attachments" "writes"])))))

(deftest related-never-lists-the-node-as-its-own-relation
  (doseq [[kind node] [[:transform "pull-mailbox"] [:dataset "raw_messages"]
                       [:object-type "zeitbuchung"]]]
    (is (not (some (fn [[k n _]] (and (= (name kind) k) (= node n)))
                   (related-set kind node)))
        (str kind "/" node " lists itself"))))

(deftest a-multi-output-producer-shows-every-output
  (testing "both landed datasets are edges, in declaration order"
    (is (= ["d:raw_messages" "d:raw_attachments"]
           (mapv #'view/node-label (:outputs (edge :pull-mailbox))))))

  (testing "the sugar shape still yields exactly one"
    (is (= ["o:zeitbuchung"] (mapv #'view/node-label (:outputs (edge :zeitbuchung-rows))))))

  (testing "…so each output resolves BACK to its producer"
    (let [edges (lineage/edges body)]
      (is (= ["pull-mailbox"] (:produced-by (lineage/anchored edges :raw_messages))))
      (is (= ["pull-mailbox"] (:produced-by (lineage/anchored edges :raw_attachments))))
      (is (= ["zeitbuchung-rows"] (:produced-by (lineage/anchored edges :zeitbuchung))))))

  (testing "a producer is still not its own consumer"
    (let [edges (lineage/edges body)]
      (is (= ["arbeitszeit-rows"] (:consumed-by (lineage/anchored edges :raw_messages)))))))

(deftest backing-key-reads-the-matching-output-not-the-first
  (testing "the PK comes from the output that actually targets the node"
    (is (= :message_id
           (rules/object-primary-key
            {:object-types {"rid.ot.msg" {:object-type-name :message :plural "Messages"}}
             :transforms
             {"rid.t.pull"
              {:transform-name :pull-mailbox
               :outputs [{:kind :dataset :dataset "rid.ds.att"
                          :name :raw_attachments :primary-key :att_id}
                         {:kind :object-type :object-type "rid.ot.msg"
                          :name :message :primary-key :message_id}]}}}
            "rid.ot.msg"))
        "the object-type output is SECOND — reading `(first outputs)` would answer att_id"))
  (testing "a :supplement backing does not define the key — it backfills BY it"
    (is (= :message_id
           (rules/object-primary-key
            {:object-types {"rid.ot.msg" {:object-type-name :message :plural "Messages"}}
             :transforms
             {"rid.t.supp" {:transform-name :message-supp
                            :output {:kind :object-type :object-type "rid.ot.msg"
                                     :role :supplement :primary-key :nicht_der_key}}
              "rid.t.pull" {:transform-name :pull-mailbox
                            :output {:kind :object-type :object-type "rid.ot.msg"
                                     :primary-key :message_id}}}}
            "rid.ot.msg")))))

(deftest link-backing-is-the-classification-the-gate-judges
  (let [schema {:link-types {"rid.lt.x" {:link-type-name :x}}
                :transforms
                {"rid.t.x" {:transform-name :x-rows
                            :input {:kind :object-type :object-type "rid.ot.a"}
                            :output {:kind :link-type :link-type "rid.lt.x"
                                     :fk-attribute :lieferant_id}}}}]
    (is (= {:kind :fk :producer :x-rows :fk-attribute :lieferant_id}
           (backing/link-backing schema "rid.lt.x"))))
  (testing "a link nothing feeds is nil, not a kind — its rows come from Actions"
    (is (nil? (backing/link-backing {:link-types {"rid.lt.y" {:link-type-name :y}}} "rid.lt.y")))))
