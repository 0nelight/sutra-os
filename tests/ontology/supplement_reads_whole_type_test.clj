(ns ^:api ontology.supplement-reads-whole-type-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private primary
  {:objects {:ev {:symbol "📨" :primary-key :pk :plural "Evs"
                  :attributes [[:pk :string] [:label :string] [:tag :string]
                               [:sql-tag :string]]}}
   :transforms
   {:mk-ev {:language :clojure
            :code "(fn [{:keys [src]}] (for [r src] {:pk (:tid r) :label (:label r)}))"
            :incremental {:read-mode :added :write-mode :modify}
            :inputs {:src {:kind :dataset :dataset :ev_src}}
            :primary-key :pk
            :output {:kind :object-type :object-type :ev}}}})

(defn- supplements [prefix]
  (update primary :transforms merge
          {:tag-ev {:language :clojure
                    :code (str "(fn [{:keys [e]}] (for [r e] {:pk (:pk r) :tag (str \""
                               prefix "t-\" (:pk r))}))")
                    :incremental {:read-mode :current :write-mode :replace}
                    :inputs {:e {:kind :object-type :object-type :ev}}
                    :output {:kind :object-type :object-type :ev :primary-key :pk
                             :role :supplement}}
           :sql-tag-ev {:language :pg-sql
                        :query (str "SELECT pk AS pk, '" prefix "s-' || pk AS sql_tag FROM e")
                        :incremental {:read-mode :current :write-mode :replace}
                        :inputs {:e {:kind :object-type :object-type :ev}}
                        :output {:kind :object-type :object-type :ev :primary-key :pk
                                 :role :supplement}}}))

(defn- install! [schema]
  (is (ok? (api/post! :install {:schema (in-project :suppwhole schema)})))
  (is (ok? (api/post! :staging))))

(defn- evs []
  (into {} (map (fn [r] (let [v (or (:values r) r)] [(:pk v) [(:tag v) (:sql-tag v)]])))
        (:rows (:body (api/list-objects :ev)))))

(defn- append! [tids]
  (is (ok? (api/post! :dataset-write {:name "ev_src"}
                      {:rows (mapv (fn [t] {:values {:tid t :label t}}) tids)
                       :mode :append :message "seed"}))))

(defn- tagged [prefix pks]
  (into {} (map (fn [p] [p [(str prefix "t-" p) (str prefix "s-" p)]])) pks))

(deftest a-supplement-merges-onto-every-object-of-its-type
  (is (ok? (api/mutate :create-project {:name :suppwhole} {:stage? true})))
  (is (ok? (api/mutate :create-dataset
                       {:name :ev_src :project :suppwhole
                        :columns [(col :tid :string) (col :label :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (install! primary)
  (append! ["a" "b"])
  (is (ok? (api/post! :import-run)))
  (is (= {"a" [nil nil] "b" [nil nil]} (evs)) "no supplement yet")
  (testing "new supplements in a run whose producer adds one object cover every object"
    (install! (supplements ""))
    (append! ["c"])
    (is (ok? (api/post! :import-run)))
    (is (= (tagged "" ["a" "b" "c"]) (evs))))
  (testing "changed supplements in a run that imports no new object cover every object"
    (install! (supplements "u"))
    (is (ok? (api/post! :import-run)))
    (is (= (tagged "u" ["a" "b" "c"]) (evs)))))
