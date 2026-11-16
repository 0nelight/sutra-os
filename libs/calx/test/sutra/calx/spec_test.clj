(ns sutra.calx.spec-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec.alpha :as s]
            [sutra.calx.spec]
            [sutra.calx.core :as c]
            [sutra.calx.io.edn :as edn-io]))

(defn- sample []
  (-> (c/workbook {"Sheet1" [[1 "x" "=A1*2" "={1,2;3,4}"] [true nil "=1/0" nil]]
                   "S2" [["=Sheet1!A1:B2"]]})
      (c/define-name "Rate" "=0.19")
      (c/set-style "A1" {:format "0.0"})
      (c/insert-rows "Sheet1" 1)
      (c/set-cell "A1" "=SUM(")
      (c/copy-range "C2" "C5")))

(deftest workbook-conforms
  (let [w (sample)]
    (is (s/valid? :calx/workbook w) (s/explain-str :calx/workbook w))
    (is (s/valid? :calx/log (c/edit-log w)) (s/explain-str :calx/log (c/edit-log w)))))

(deftest document-conforms
  (let [doc (edn-io/workbook->doc (sample))]
    (is (s/valid? :calx/document doc) (s/explain-str :calx/document doc))))

(deftest bad-shapes-are-rejected
  (is (not (s/valid? :calx/op [:set-cell])))
  (is (not (s/valid? :calx/op [:insert-rows 1 -1 2])))
  (is (not (s/valid? :calx/ast {:kind :op :op :plus :args []}))))
