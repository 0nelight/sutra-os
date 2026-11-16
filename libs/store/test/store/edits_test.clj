(ns ^:unit store.edits-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.edits :as edits]))

(def ^:private src {:pk 1 :a "src-a" :b "src-b"})

(defn- leaf [e source times]
  (let [r (edits/resolve-object e source times)]
    [(:why r) (:visible? r) (:values r)]))

(deftest apply-user-edits
  (testing "no times: the edit always wins"
    (doseq [[path e source want]
            [["latest delete, source holds it"
              {:deleted? true :at 10} src
              [:deleted false nil]]
             ["delete, then create: the create is the starting point"
              {:created? true :deleted? false :attrs #{:a} :values {:a "new"} :at 20} src
              [:created true {:a "new"}]]
             ["create, source absent: still visible"
              {:created? true :attrs #{:a} :values {:a "new"} :at 20} nil
              [:created true {:a "new"}]]
             ["modification, source absent: gone until restored"
              {:attrs #{:a} :values {:a "edit"} :at 10} nil
              [:absent-from-source false nil]]
             ["modification, source present: edited attribute keeps the edit"
              {:attrs #{:a} :values {:a "edit"} :at 10} src
              [:edits-applied true {:pk 1 :a "edit" :b "src-b"}]]
             ["unedited" nil src [:source-only true src]]
             ["unedited, source absent" nil nil [:absent-from-source false nil]]]]
      (is (= want (leaf e source nil)) path))))

(deftest apply-most-recent-value
  (doseq [[path e source times want]
          [["latest delete beats a newer source"
            {:deleted? true :at 10} src {:a 99 :b 99}
            [:deleted false nil]]
           ["create, source dated before it: the create holds"
            {:created? true :attrs #{:a} :values {:a "new"} :at 20 :created-at 20} src
            {:a 5 :b 5}
            [:created true {:a "new"}]]
           ["create, source dated after it: only source data"
            {:created? true :attrs #{:a} :values {:a "new"} :at 20 :created-at 20
             :attrs-at {:a 20}} src {:a 30 :b 30}
            [:create-superseded true src]]
           ["create, source absent: the create holds"
            {:created? true :attrs #{:a} :values {:a "new"} :at 20} nil nil
            [:created true {:a "new"}]]
           ["modification, source absent"
            {:attrs #{:a} :values {:a "edit"} :at 10} nil nil
            [:absent-from-source false nil]]
           ["modification after the source: edit applies"
            {:attrs #{:a} :values {:a "edit"} :at 10} src {:a 5 :b 5}
            [:edits-applied true {:pk 1 :a "edit" :b "src-b"}]]
           ["modification before the source: source data only"
            {:attrs #{:a} :values {:a "edit"} :at 10} src {:a 20 :b 20}
            [:edits-superseded true src]]
           ["source not dated: every edit applies"
            {:attrs #{:a} :values {:a "edit"} :at 10} src {}
            [:edits-applied true {:pk 1 :a "edit" :b "src-b"}]]]]
    (is (= want (leaf e source times)) path)))

(deftest each-property-against-its-own-edit
  (testing "Ticket 101: title edited 8:30, priority 9:30, type 10:30; source at 9:00"
    (let [src {:id 101 :title "Ticket One" :priority "P1" :type "Product Bug"}
          e   {:attrs #{:title :priority :type}
               :values {:title "Ticket" :priority "P0" :type "Unknown"}
               :at 1030 :attrs-at {:title 830 :priority 930 :type 1030}}
          r   (edits/resolve-object e src (zipmap (keys src) (repeat 900)))]
      (is (= {:id 101 :title "Ticket One" :priority "P0" :type "Unknown"} (:values r)))
      (is (= #{:title} (:superseded r)))))
  (testing "a property no dated source supplies (edit-only) always takes the edit"
    (let [r (edits/resolve-object {:attrs #{:team} :values {:team "Recruiting"} :at 1}
                                  {:id 102 :team "Sales"} {:id 2050})]
      (is (= "Recruiting" (get-in r [:values :team]))))))
