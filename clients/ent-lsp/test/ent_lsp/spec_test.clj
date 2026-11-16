(ns ent-lsp.spec-test
  (:require [clojure.test :refer [deftest is]]
            [ent-lsp.spec :as spec]))

(deftest patterns
  (is (spec/match? [:schema :objects :*] [:schema :objects :angebot]))
  (is (not (spec/match? [:schema :objects :*] [:schema :objects :angebot :project])))
  (is (spec/match? [:rules :** :object-type] [:rules :object-type]))
  (is (spec/match? [:rules :** :object-type] [:rules 0 :then 2 :object-type]))
  (is (not (spec/match? [:rules :** :object-type] [:params :object-type]))))

(deftest placeholders
  (is (= "verkauf/angebot/object.ent"
         (spec/fill "{project}/{name}/object.ent" {:project :verkauf :name :angebot})))
  (is (nil? (spec/fill "{project}/{name}.ent" {:name :x}))))
