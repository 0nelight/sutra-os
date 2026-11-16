(ns ^:integration platform.kernel-schema-ops
  (:require [support.system :as tsys]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.signature :as signature]
            [clojure.test :refer [deftest use-fixtures]]
            [sutra.authoring.install.world :as onto-install]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.world :as world]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-stores-only)

(defn- columns
  [b t]
  (let [of (fn [s] (into #{} (map (comp keyword :column_name))
                         (jdbc/execute! (tsys/ds)
                                        ["SELECT column_name FROM information_schema.columns
                                          WHERE table_schema = ? AND table_name = ?" s (str t)]
                                        {:builder-fn rs/as-unqualified-maps})))]
    (not-empty (or (not-empty (of (pgns/own-schema b))) (of "main")))))

(defn- table? [b t] (some? (columns b t)))

(defn- install!
  [b W]
  (world/install! (ctx/store b) W {:actor {:principal "test"} :dataset-decls signature/datasets-of
                                  :steps [[:drop-unclaimed]] :drop-unclaimed? true}))

(defn- ot [nm attrs] {:object-type-name nm
                      :attribute-list (mapv (fn [[n t]] {:attribute-name n :attribute-type t})
                                            attrs)})

(defn- obj-table [W nm] (layout/object-table (lookup/rid-of W :object-types nm)))

(deftest rule-ops-e2e
  (store/with-fresh-branch [branch "sruleops"]
    (let [prepare (comp :ok onto-install/prepare-schema)
          run (fn [world rules]
                (let [r (kernel/eval-steps rules world {} {} {:installer {} :project :ops})]
                  (some->> (:installed r) (#(prepare % {})) (install! branch))
                  (:world r)))
          empty-w {:object-types {} :link-types {} :interfaces {}
                   :transforms {} :actions {} :object-sets {} :projects {:ops {}}}]

      (section "create interface + object-type implementing it")
      (let [w (run empty-w [{:op :create-interface :name :locatable
                             :properties [[:location :string]]}
                            {:op :create-object-type :name :host :primary-key :name
                             :attributes [[:name :string]] :implements [:locatable]
                             :plural "Hosts"}])
            host-tbl (layout/object-table (ffirst (:object-types w)))
            cols (columns branch host-tbl)]
        (check "host table created" (table? branch host-tbl))
        (check "host table has own :name column" (cols :name))
        (check "host table has INHERITED :location column (contribution)" (cols :location))

        (section "update :host adding :os, omitting :implements (preserved)")
        (let [w2 (run w [{:op :update-object-type :name :host
                          :attributes [[:name :string] [:os :string]]}])
              cols2 (columns branch host-tbl)]
          (check "host table gained :os (ALTER)" (cols2 :os))
          (check "host table kept inherited :location (omit-preserve + contribution)"
                 (cols2 :location))

          (section "delete :host")
          (run w2 [{:op :delete-object-type :name :host}])
          (check "host table dropped" (not (table? branch host-tbl))))))))

(deftest reconcile-drops
  (store/with-fresh-branch [branch "sdrop"]
    (section "ALTER: a column the new world no longer names is not dropped by an install")
    (let [old (ri/derived {:sigma clock/system}
                          {:object-types {:host (ot :host [[:name :string] [:location :string]])}})
          new (ri/derived {:sigma clock/system}
                          {:object-types {:host (ot :host [[:name :string] [:os :string]])}} old)
          t   (obj-table old :host)]
      (install! branch old)
      (check "host starts with :location, no :os"
             (let [c (columns branch t)] (and (c :location) (not (c :os)))))
      (install! branch new)
      (check "after the install :location is still there and :os was added"
             (let [c (columns branch t)] (and (c :location) (c :os)))))

    (section "DROP + orphan: deleting an object-type drops it + dependent link")
    (let [old (ri/derived {:sigma clock/system}
               {:object-types {:host  (ot :host [[:name :string]])
                               :place (ot :place [[:name :string]])}
                :link-types {:lives-in {:link-type-name :lives-in
                                        :from-object-type :host :to-object-type :place
                                        :cardinality :many-to-one}}})
          new (ri/derived {:sigma clock/system}
                          {:object-types {:place (ot :place [[:name :string]])}} old)
          host  (obj-table old :host)
          place (obj-table old :place)
          link  (layout/link-table (lookup/rid-of old :link-types :lives-in))]
      (install! branch old)
      (check "host + lives-in exist before" (and (table? branch host) (table? branch link)))
      (install! branch new)
      (check "host dropped" (not (table? branch host)))
      (check "orphan lives-in dropped" (not (table? branch link)))
      (check "place survives" (table? branch place)))))
