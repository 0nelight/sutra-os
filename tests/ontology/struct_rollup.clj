(ns ^:integration ontology.struct-rollup
  (:require [support.system :as system]
            [sutra.builds.plan.producers :as producers]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.import.materialize :as mat]
            [sutra.store.access.substrate :as sub]
            [support.install :as inst]
            [sutra.store.access.row-shape :as row-shape]
            [support.store-catalog :as cat]
            [sutra.store.seat]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.types.coltype :as ctype]
            [sutra.types.naming :as naming]
            [sutra.objects.views :as views]
            [sutra.catalog.values :as values]
            [sutra.catalog.grants :as grants]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]
            [clojure.test :refer [deftest use-fixtures]]))

(use-fixtures :once pod/with-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defn- col-type [branch table col] (cat/column-type branch table col))

(defn- object-of [branch T id] (first (filter #(= id (:id %)) (store/objects branch T))))

(deftest struct-rollup
  (let [branch (str "sr-" (subs (uuid) 0 8))]
    (try
      (store/fork! branch)
      (let [run (fn [world rules] (inst/run-installing! branch world rules))
            empty-w {:object-types {} :link-types {} :interfaces {}
                     :transforms {} :actions {} :object-sets {}}]

        (section "install hub (:struct prop + 2 derived link-rollups) + children + links")
        (let [w
              (run empty-w
                   [{:op :create-object-type :name :rechnung
                     :primary-key :rnr
                     :attributes [[:rnr :string] [:netto [:decimal 12 2]] [:bestell-nr :string]]
                     :plural "Rechnungen"}
                    {:op :create-object-type :name :ereignis
                     :primary-key :enr
                     :attributes [[:enr :string] [:kind :string] [:doc-date :string]
                                  [:bestell-nr :string]]
                     :plural "Ereignisse"}
                    {:op :create-object-type :name :bestellung
                     :title :nr :primary-key :nr
                     :attributes [[:nr :string]
                                  [:adresse [:struct {:strasse :string :plz :integer}]]
                                  [:summe-netto [:decimal 12 2]
                                   {:derived {:expression [:connected-sum :rechnungen :netto]}}]
                                  [:status :string
                                   {:derived
                                      {:expression
                                         [:connected-arg-max :ereignisse :doc-date :kind]}}]
                                  [:summe-live [:decimal 12 2]
                                   {:derived {:expression [:connected-sum :rechnungen :netto]}}]
                                  [:doppelt [:decimal 12 2]
                                   {:derived {:expression [:* [:get :summe-live] 2]}}]]
                     :plural "Bestellungen"}
                    {:op :create-link-type :name :rechnungen
                     :from :bestellung :to :rechnung :cardinality :one-to-many}
                    {:op :create-link-type :name :ereignisse
                     :from :bestellung :to :ereignis :cardinality :one-to-many}
                    {:op :create-transform :name :link-rechnungen
                     :input {:kind :object-type :object-type :rechnung}
                     :output {:kind :link-type :link-type :rechnungen :fk-attribute :bestell-nr}}
                    {:op :create-transform :name :link-ereignisse
                     :input {:kind :object-type :object-type :ereignis}
                     :output {:kind :link-type :link-type :ereignisse :fk-attribute :bestell-nr}}])
              rid-of  (fn [nm] (some (fn [[rid s]] (when (= nm (:object-type-name s)) rid))
                                     (:object-types w)))
              best-rid (rid-of :bestellung) rech-rid (rid-of :rechnung) erg-rid (rid-of :ereignis)
              best-tbl (naming/obj-table best-rid)]

          (system/seat! branch)
          (system/ensure!)

          (section "WS1 — :struct column → JSON DDL + value round-trip")
          (check "adresse column is the store's JSON storage type"
                 (= (ctype/col-type :varchar) (col-type branch best-tbl "adresse")))
          (check "summe_netto column keeps its declared precision"
                 (= (ctype/col-type :decimal {:precision 12 :scale 2})
                    (col-type branch best-tbl "summe_netto")))
          (let [bid (uuid) adr {:strasse "Hauptstr 1" :plz 8001}]
            (store/insert-object! branch best-rid {:id bid :values {:nr "B-1" :adresse adr}})
            (let [obj (object-of branch best-rid bid)
                  v   (get-in obj [:values :adresse])]
              (check "struct round-trips to a keyword-keyed map" (= adr v))
              (check "decoded struct value is a map (not a string)" (map? v)))

            (section "WS4 — link rollups computed over real links")
            (let [r1 (uuid) r2 (uuid) e1 (uuid) e2 (uuid)]
              (store/insert-object! branch rech-rid
                                    {:id r1 :values {:netto 100.00M :bestell-nr "B-1"}})
              (store/insert-object! branch rech-rid
                                    {:id r2 :values {:netto 49.50M :bestell-nr "B-1"}})
              (store/insert-object! branch erg-rid
                                    {:id e1 :values {:kind "bestellung" :doc-date "2026-01-10"
                                                     :bestell-nr "B-1"}})
              (store/insert-object! branch erg-rid
                                    {:id e2 :values {:kind "bestaetigung" :doc-date "2026-03-05"
                                                     :bestell-nr "B-1"}})

              (let [pre (object-of branch best-rid bid)]
                (check "rollup columns NULL in the raw table"
                       (nil? (get-in pre [:values :summe-netto]))))

              (store/update-object! branch best-rid bid {:status "stale-from-a-past-build"
                                                         :summe-netto 999.99M})

              (section "the read computes what the build stores nowhere")
              (let [row
                      (->> (views/run-object-query (ctx/store branch grants/system-authority) w
                                                   {:find :bestellung :ids [bid]})
                           :rows first)]
                (check "status = kind of latest ereignis by doc-date = 'bestaetigung'"
                       (= "bestaetigung" (get-in row [:values :status])))
                (check "summe-netto = Σ rechnung.netto = 149.50, from the read"
                       (= 149.50M (get-in row [:values :summe-netto]))))

              (section "a derived property computed FROM another derived property")
              (let [row (->> (views/run-object-query (ctx/store branch grants/system-authority)
                                                     w
                                                     {:find :bestellung :ids [bid]
                                                      :show [:doppelt]})
                             :rows first)]
                (check "asking only for :doppelt still computes :summe-live under it"
                       (== 299.00M (get-in row [:values :doppelt]))))

              (section "aggregating a read-time derived property")
              (let [aggs (views/run-object-query (ctx/store branch grants/system-authority)
                                                 w {:find     :bestellung
                                                    :aggs     [{:fn :count :name :n}]
                                                    :group-by [:summe-netto]})]
                (check "grouped by a property no column holds, computed by the store"
                       (= [{:group {:summe-netto 149.50M} :metrics {:n 1}}]
                          (:aggregations aggs))))
              (check "grouping by one the store cannot compute (an arg-max) is refused"
                     (try (views/run-object-query (ctx/store branch grants/system-authority)
                                                  w {:find :bestellung :aggs [{:fn :count :name :n}]
                                                     :group-by [:status]})
                          false
                          (catch clojure.lang.ExceptionInfo e
                            (boolean (re-find #"the store cannot compute" (ex-message e))))))

              (section "aggregating a set GROUPED BY a linked object (title for free)")
              (let [aggs (views/run-object-query (ctx/store branch grants/system-authority)
                                                 w {:find     :rechnung
                                                    :aggs     [{:fn :sum :attr :netto :name :summe}]
                                                    :group-by [[:link :rechnungen]]})]
                (check "one bucket, keyed by the LINKED bestellung object with its title"
                       (= [{:group {:rechnungen {:id bid :title "B-1"}}
                            :metrics {:summe 149.50M}}]
                          (:aggregations aggs)))
                (check "the link dim is a GROUP key, kept apart from the metric"
                       (= {:group [:rechnungen] :metrics [:summe]}
                          (:keys aggs))))

              (section "an :object-type transform input carries stored properties only")
              (let [rows
                      (@#'inputs/stored-object-rows
                       (sub/reader (ctx/store branch grants/system-authority)) w best-rid)
                    row  (first (filter #(= "B-1" (get % :nr)) rows))]
                (check "a rollup is not in it"
                       (not (contains? row :summe_netto)))
                (check "nor an arg-max one"
                       (not (contains? row :status)))
                (check "nor a derived property defined over another one"
                       (not (contains? row :doppelt)))
                (check "a stored column is untouched and keeps its spelling"
                       (= "B-1" (get row :nr)))
                (check "the MAC label never reaches a transform"
                       (and (not (contains? row :_markings))
                            (not (contains? row :-markings))))))

            (section "MATERIALIZATION — the ontology's indexed state as a dataset")
            (let [cols   [{:attribute-name :nr      :attribute-type :string}
                          {:attribute-name :adresse
                           :attribute-type [:struct {:strasse :string :plz :integer}]}]
                  w2     (run w [{:op :create-dataset :name :bestellung_mat
                                  :kind :materialization :object-type :bestellung
                                  :columns cols}
                                 {:op :create-dataset :name :bestellung_alt
                                  :kind :materialization :object-type :bestellung
                                  :columns (conj cols {:attribute-name :summe-netto
                                                       :attribute-type [:decimal 12 2]})}
                                 {:op :create-dataset :name :bestellung_sub
                                  :kind :materialization :object-type :bestellung
                                  :properties [:nr]
                                  :columns (vec (take 1 cols))}])
                  ds-of  (fn [nm] (some (fn [[rid s]] (when (= nm (:dataset-name s)) rid))
                                        (:datasets w2)))
                  mat    (ds-of :bestellung_mat)
                  sub    (ds-of :bestellung_sub)
                  land!  (fn [rid]
                           (dsets/ensure-tables! (ctx/store branch)
                                                 {(naming/dataset-table rid)
                                                    (get-in w2 [:datasets rid :columns])})
                           (@#'producers/materialize-object-type!
                            (ctx/store branch grants/system-authority) w2 rid best-rid)
                           (let [t (naming/dataset-table rid)]
                             (dsets/row-values (ctx/store branch) t
                                               (row-shape/array-keys (ctx/store branch)
                                                                     (system/seated-schema) t))))]
              (let [rows (land! mat)
                    row  (first (filter #(= "B-1" (get % :nr)) rows))]
                (check "a derived rollup is no column in the materialization"
                       (not (contains? row :summe_netto)))
                (check "nor is the arg-max one"
                       (not (contains? row :status)))
                (check "a stored column comes through unchanged"
                       (= "B-1" (get row :nr))))
              (check "the row carries the OBJECT's id, not a minted one"
                     (= [bid]
                        (mapv :id
                              (mat/object-rows
                               (sub/reader (ctx/store branch grants/system-authority))
                               w2 best-rid))))
              (check "so a second materialization leaves ONE row, not two"
                     (= 1 (count (land! mat))))
              (let [alt (ds-of :bestellung_alt)
                    _   (dsets/ensure-tables! (ctx/store branch)
                                              {(naming/dataset-table alt)
                                               (get-in w2 [:datasets alt :columns])})
                    _   (dsets/write-rows! (ctx/store branch grants/system-authority) alt :snapshot
                                           [{:id bid :values {:nr "B-1" :summe_netto 7M}}])
                    _   (@#'producers/materialize-object-type!
                         (ctx/store branch grants/system-authority) w2 alt best-rid)
                    t   (naming/dataset-table alt)
                    row (first (dsets/row-values (ctx/store branch) t
                                                 (row-shape/array-keys (ctx/store branch)
                                                                     (system/seated-schema) t)))]
                (check "a rebuild drops a derived column its last snapshot still held"
                       (nil? (get row :summe_netto))))
              (let [row (first (land! sub))]
                (check ":properties carries the named subset…"
                       (= "B-1" (get row :nr)))
                (check "…and nothing else — an unnamed property is not a column here"
                       (nil? (get row :adresse)))))

            (section "value-ok? — struct accepted, non-map rejected")
            (check "vals-ok? passes a valid struct value"
                   ((values/vals-checker w best-rid) {:adresse {:strasse "X" :plz 9000}}))
            (check "vals-ok? rejects a non-map struct value"
                   (not ((values/vals-checker w best-rid) {:adresse "not-a-struct"}))))))

      (println "\n  struct + rollups: install + DDL + write/read + read-time derivation work.")
      (finally
        (system/unseat!)
        (try (store/retire! branch) (catch Exception _ nil))))))
