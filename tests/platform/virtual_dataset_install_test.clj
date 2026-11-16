(ns ^:api platform.virtual-dataset-install-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]
            [support.wire :refer [ok?]]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:projects {:p {}}
   :sources  {:erp {:project :p :type :postgres :jdbc-url "jdbc:postgresql://db:5432/erp"
                    :egress ["db:5432"]}}
   :datasets {:postfach {:project :p :kind :virtual
                         :virtual {:source :erp :table "mails" :primary-key [:id]}
                         :columns [{:attribute-name :id :attribute-type :integer}
                                   {:attribute-name :betreff :attribute-type :string}]}}})

(defn- named [nm m] (some #(when (= (name nm) (some-> (:dataset-name %) name)) %) (vals m)))

(deftest a-virtual-dataset-keeps-its-declaration-through-an-install
  (is (ok? (api/post! :install {:schema schema})))
  (is (ok? (api/post! :staging)))
  (testing "the installed dataset names its source and table"
    (let [{:keys [kind virtual] :as ds} (named :postfach (:datasets (:body (api/get! :schema))))]
      (is (= "virtual" (some-> kind name)) (pr-str ds))
      (is (= ["erp" "mails"] [(some-> (:source virtual) name) (:table virtual)]) (pr-str ds))))
  (testing "the lineage shows where it reads"
    (let [r (:reads-source (:body (api/get! :lineage-graph {:node "postfach"})))]
      (is (= ["erp" "mails"] [(some-> (:source r) name) (:table r)]) (pr-str r)))))
