(ns ^:unit platform.export-field-coverage-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.project-emit :as pemit]
            [sutra.types.entities :as ents]
            [sutra.types.versions :as vers]))

(def ^:private exempt
  {[:object-types :attributes]      "author spelling of :attribute-list"
   [:interfaces :properties]        "author spelling of :property-list"
   [:interfaces :links]             "author spelling of :link-constraints"
   [:object-types :external-source] "lowered into a `<type>-rows` transform the export folds back"
   [:link-types
    :external-source]   "lowered into a `<link>-conn-rows` transform the export folds back"
   [:datasets :external-source]     "lowered into a `<name>-sync` transform the export folds back"
   [:projects
    :grants]
     "deliberately not carried: an install from the file must not clobber grants"
   [:spaces :grants]                "as :projects :grants"
   [:organizations :grants]         "as :projects :grants"
   [:templates :engine]             "single-valued enum; the default is elided"
   [:transforms :code-path]         "an inline export has no file; the other one writes :code-file"
   [:actions :edits]
   "derived from :rules (spec/derive-edits); the export keeps it only beside a function rule"
   [:actions :revision]
   "counted by the store on every change (a80607465): the text does not hold it"})

(def ^:private context
  {:object-types {"ri.ot.a" {:object-type-name :a
                             :attribute-list [{:attribute-name :id :attribute-type :string}]
                             :plural "As"}
                  "ri.ot.b" {:object-type-name :b
                             :attribute-list [{:attribute-name :id :attribute-type :string}]
                             :plural "Bs"}}
   :interfaces   {"ri.if.a" {:interface-name :ia}}
   :pipelines    {"ri.pl.a" {:pipeline-name :pa}}})

(defn- base [section field]
  (case section
    :object-types {:object-type-name :probe
                   :attribute-list [{:attribute-name :id :attribute-type :string}]}
    :link-types   {:link-type-name :probe :from-object-type "ri.ot.a" :to-object-type "ri.ot.b"
                   :cardinality :many-to-one}
    :transforms   (if (#{:query :parameters :board} field)
                    {:transform-name :probe :language :pg-sql :query "SELECT 1"}
                    {:transform-name :probe :language :python :code "x = 1\n"})
    :datasets     {:dataset-name :probe :kind :standalone}
    {(ents/name-field section) :probe}))

(def ^:private by-type
  {:string                        "probe-zz"
   :keyword                       :probe-zz
   :int                           4242
   :boolean                       true
   :any                           {:probe-zz "probe-zz"}
   :ent/status                    :experimental
   :ent/resource-status           :experimental
   :ent/deprecation               {:reason "probe-zz"}
   :ent/visibility                :hidden
   :ent/vtype                     :integer
   :ent/api-name                  :probeZz
   :ent/ref-id                    "ri.ot.b"
   :ent/edit-conflict             {:strategy :apply-most-recent}
   :ent/export-destination        {:table "probe_zz"}
   :ent/export-mode               :full
   :ent/semantic-search           {:property :id}
   :ent/object-checks             [{:name "probe-zz" :expr [:= 1 1]}]
   :ent/lifecycle                 {:property :id :states [:probe-a :probe-b]}
   :ent/backing                   {:dataset :probe-zz}
   :ent/constraints               {:one-of ["probe-zz"]}
   :ent/value-format              {:kind :percentage}
   :ent/inputs                    {:probe-in {:kind :object-type :object-type "ri.ot.b"}}
   :ent/input-spec                {:kind :object-type :object-type "ri.ot.b"}
   :ent/ops                       [{:op :probe-zz}]
   :ent/done-key                  {:input :probe-in :key :probe-zz}
   :ent/prompt-key                {:input :probe-in :columns [:probe-zz]}
   :ent/column-list               [{:attribute-name :probe-zz :attribute-type :string}]
   :ent/attr-list                 [{:attribute-name :id :attribute-type :string}
                                   {:attribute-name :probe-zz :attribute-type :string}]
   :ent/template-page             {:size :a5}
   :ent/template-limits           {:max-rows 5}
   :ent/oauth-client              {:grant-types [:client-credentials]}
   :ent/app-scope                 {:object-types [:a]}
   :ent/app-sdk                   {:namespace "probe-zz"}
   :ent/app-permissions           {:roles [:probe-zz]}
   :ent/app-hosting               {:subdomain "probe-zz"}
   :ent/stream                    {:partitions 2}
   :ent/security                  {:read [:probe-zz]}
   :ent/restricted-view           {:dataset "ri.ot.b" :policy {:probe-zz "probe-zz"}}
   :ent/virtual                   {:source :probe-zz :table "probe_zz"}
   :ent/agent-retrieval           {:kind :semantic-search :object-type "ri.ot.b"}
   :ent/transform-resources       {:cpu-cores 2 :memory-gb 4}
   :ent/media-set-sync            {:filters {:exclude-synced true}}
   [:vector :ent/ref-id]          ["ri.ot.b"]
   [:vector :ent/link-constraint] [{:link-name :probe-zz :cardinality :one-to-many
                                    :to-object-type "ri.ot.b"}]
   [:or :boolean :ent/incremental-opts] true
   [:or :ent/interface-link-membership [:vector :ent/interface-link-membership]]
   [{:interface "ri.if.a" :constraint :probe-zz}]})

(def ^:private by-field
  {[:transforms :output]   {:kind :object-type :object-type "ri.ot.b"}
   [:transforms :outputs]  [{:kind :object-type :object-type "ri.ot.b"}]
   [:transforms :pipeline] "ri.pl.a"
   [:transforms :timeout-ms] 30000
   [:transforms :allowed-run-seconds] 7200
   [:applications :live]   true
   [:actions :on-branch]   {:webhooks true}
   [:actions :form]        {:sections [{:title "probe-zz" :params [:probe-zz]}]}})

(defn- probe [section field t]
  (or (by-field [section field])
      (by-type t)
      (when (vector? t)
        (case (first t)
          :enum               (last t)
          (:vector :sequential) (let [v (by-type (second t) (by-type :keyword))]
                                  (if (vector? v) v [v]))
          :map-of             {:probe-zz "probe-zz"}
          :map                (into {} (for [e (rest t)
                                             :when (vector? e)
                                             :let [v (probe section field (last e))]
                                             :when (some? v)]
                                         [(first e) v]))
          nil))
      (when (fn? t) 4242)))

(defn- export [section spec]
  (pemit/export (merge context {section (merge {"ri.probe" spec} (get context section))})
             {:all-projects? true :inline? true}))

(defn- authorable-fields []
  (for [{:keys [section tree key fields]} ents/entities
        :when (and tree (= :rid key))
        [field {:keys [as author-only author-form type]}] (sort-by key fields)
        :when (not (or author-only (#{:minted :positional} author-form)
                       (exempt [section field])
                       (and (vers/publishes? section) (some #{field} vers/history-keys))))]
    {:section section :field field :canonical (or as field) :type type}))

(deftest every-authorable-field-moves-the-export
  (doseq [{:keys [section field canonical type]} (authorable-fields)]
    (testing (str section " " field)
      (let [b (base section field)
            v (probe section field type)]
        (is (some? v) "no probe value for this field type — add one to `by-type`")
        (when (some? v)
          (is (not= (export section b) (export section (assoc b canonical v)))
              "the exporter drops this field, so the write-back would retract it from the author's tree"))))))

(deftest an-export-carries-no-credentials
  (testing "a source's secrets stay in the instance: the text holds none, the export counts them"
    (let [{:keys [ent skipped]}
          (pemit/export {:sources {"ri.src.a" {:source-name :db :kind :postgres
                                               :secrets {:password "probe-zz"}
                                               :secret-cipher "probe-zz"}}}
                        {:all-projects? true :inline? true})
          src (get-in ent [:schema :sources :db])]
      (is (some? src))
      (is (not-any? #(contains? src %) [:secrets :secret-cipher]))
      (is (= 1 (:credentials skipped))))))
