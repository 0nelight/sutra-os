(ns ^:unit grammar.kinds-parity-test
  (:require [clojure.test :refer [deftest testing is are]]
            [sutra.parser.spec :as spec]))

(def ^:private transform-input?  @#'spec/valid-transform-input?)
(def ^:private compute-input?    @#'spec/valid-compute-input?)
(def ^:private output-entry?     @#'spec/valid-output-entry?)

(deftest source-input-parity
  (are [expected in] (= expected (boolean (transform-input? in)))
    true  {:kind :datasource    :datasource :amos}
    false {:kind :datasource    :datasource "not-a-kw"}
    true  {:kind :transform     :transform  :t1}
    true  {:kind :object-type   :object-type :ot}
    true  {:kind :language-model :source :xai :model "grok-4"}
    false {:kind :language-model :source :xai}
    true  {:kind :dataset       :dataset :ds}
    false {:kind :dataset       :dataset "not-a-rid"}
    false {:kind :media-set     :dataset :ds}
    false {:kind :link-type     :link-type :l}
    false {:kind :bogus}
    false "not-a-map"))

(deftest compute-input-parity
  (are [expected in] (= expected (boolean (compute-input? in)))
    true  {:kind :datasource :datasource :amos :table "t"}
    true  {:kind :datasource :datasource :amos :query "select 1"}
    false {:kind :datasource :datasource :amos}
    false {:kind :datasource :datasource "not-kw" :table "t"}
    true  {:kind :transform  :transform :t1}
    true  {:kind :object-type :object-type :ot}
    true  {:kind :dataset    :dataset :ds}
    true  {:kind :media-set  :dataset :ds}
    false {:kind :media-set  :media-set :ds}
    true  {:kind :language-model :source :xai :model "grok-4"}
    false {:kind :link-type  :link-type :l}
    false {:kind :bogus}
    false "not-a-map"))

(deftest output-entry-parity
  (testing "object-type / dataset outputs (from a source-kind producer)"
    (are [expected out] (= expected (boolean (output-entry? :datasource out)))
      true  {:name :o :kind :object-type :object-type :ot :primary-key :id}
      false {:name :o :kind :object-type :object-type :ot}
      true  {:name :o :kind :dataset :dataset :ds}
      true  {:name :o :kind :dataset :dataset :ds :primary-key :id}
      true  {:name :o :kind :link-type :link-type :l :from-key-column :a :to-key-column :b}
      false {:name :o :kind :media-set :dataset :ds}
      false {:name :o :kind :transform :transform :t}
      false {:name :o :kind :datasource :datasource :d}
      false "not-a-map"))
  (testing "link-type output from an object producer (fk shape)"
    (is (true? (boolean (output-entry? :object-type
                                       {:name :l :kind :link-type :link-type :l
                                        :fk-attribute :ref})))))
  (testing "a DATASET input is a producer side — it can back an object type"
    (are [expected out] (= expected (boolean (output-entry? :dataset out)))
      true  {:name :o :kind :object-type :object-type :ot :primary-key :id}
      true  {:name :o :kind :object-type :object-type :ot :primary-key :id
             :column-map {:a :a}}
      false {:name :o :kind :object-type :object-type :ot}
      true  {:name :o :kind :dataset :dataset :ds}
      true  {:name :o :kind :link-type :link-type :l :from-key-column :a :to-key-column :b}))
  (testing "a LANGUAGE-MODEL input is NOT a producer side"
    (is (false? (boolean (output-entry? :language-model
                                        {:name :o :kind :object-type :object-type :ot
                                         :primary-key :id}))))))
