(ns ent-lsp.cursor-test
  (:require [clojure.test :refer [deftest is]]
            [ent-lsp.cursor :as c]))

(deftest where-the-cursor-stands
  (is (= {:path [:schema :objects :a] :at :key :keys #{:project}}
         (select-keys (c/locate "{:schema {:objects {:a {:project :p ") [:path :at :keys])))
  (is (= {:path [:schema :objects :a] :at :value :key :status :prefix ":ac"}
         (select-keys (c/locate "{:schema {:objects {:a {:status :ac") [:path :at :key :prefix])))
  (is (= {:path [:schema :links 0] :at :element :index 3}
         (select-keys (c/locate "{:schema {:links [[:l :a :b ") [:path :at :index])))
  (is (= {:path [:schema :actions :x :params :p] :at :value :key :prefill}
         (select-keys
          (c/locate "{:schema\n {:actions {:x {:params {:p {:type :string ; kommentar\n :prefill ")
          [:path :at :key])))
  (is (= [:schema :objects :a]
         (:path (c/locate "{:schema {:objects {:a {:description \"x [ { \" "))))
  (is (nil? (c/locate "{:schema {:objects {:a {:description \"offen"))))
