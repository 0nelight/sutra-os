(ns ^:unit platform.peering-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.peering :as peering]))

(def ^:private secret {:name "SECRET" :category "cls" :rank 2})
(def ^:private unclassified {:name "UNCLASSIFIED" :category "cls" :rank 0})

(deftest the-ceiling-decides-as-core-concepts-tables-it
  (let [bare  {:classification secret :markings []}
        wider {:classification secret :markings ["Operational" "Exercise"]}]
    (is (false? (peering/admitted? bare [unclassified {:name "Operational"}]))
        "a marking the connection does not carry keeps the resource back")
    (is (true? (peering/admitted? bare [secret])))
    (is (true? (peering/admitted? wider [unclassified {:name "Operational"}])))
    (is (true? (peering/admitted? wider [secret])))
    (is (false? (peering/admitted? bare [{:name "TOP" :category "cls" :rank 3}]))
        "a classification above the ceiling's")
    (is (false? (peering/admitted? {:markings []} [unclassified]))
        "a classification without one on the ceiling")
    (is (= [{:name "Operational"}]
           (peering/held-back bare [secret {:name "Operational"}])))))

(deftest a-ceiling-changes-only-with-the-other-sides-approval
  (let [c  {:ceiling {:markings ["a"]}}
        p  (peering/propose-ceiling c {:markings ["a" "b"]} :local)]
    (is (= {:markings ["a"]} (:ceiling p)) "the old ceiling holds until approved")
    (is (:refused (peering/approve-ceiling p :local)) "not by the side that proposed it")
    (is (= {:markings ["a" "b"]} (:ceiling (peering/approve-ceiling p :remote))))
    (is (= [:pending-ceiling nil]
           (find (peering/approve-ceiling p :remote) :pending-ceiling))
        "nil, not absent: the connection row is upserted and an absent column stays")
    (is (:refused (peering/approve-ceiling c :remote)) "nothing waits")))

(deftest directions-and-the-lifecycle
  (is (= {:source-data :import :action-data :both :media :none :datasets :none}
         (:data-types (peering/accepted-connection
                       {:connection-id "c" :from "fp"
                        :data-types {:source-data :export :action-data :both}}
                       {:trust "a"}))))
  (is (= [{:problem :source-data-one-way
           :why "source data peers in one direction only"}]
         (peering/data-type-problems {:source-data :both})))
  (is (= :accepted (peering/next-status :requested :accepted)))
  (is (= :active (peering/next-status :accepted :activate)))
  (is (nil? (peering/next-status :requested :activate)) "activated only once approved")
  (let [c {:status :active :data-types {:source-data :export :action-data :both}}
        m {:source-data :export :action-data :import}]
    (is (peering/may-flow? c m :source-data :out))
    (is (not (peering/may-flow? c m :action-data :out)) "the mapping blocks the export")
    (is (not (peering/may-flow? (assoc c :disabled-remote true) m :source-data :out))
        "either side disabling stops the data")))

(deftest markings-cross-by-their-wire-names
  (is (= [{:name "intern"} {:name "SECRET" :category "cls" :rank 2}]
         (peering/wire-markings [:vertraulich :secret] {:secret {:category "cls" :rank 2}}
                                {"intern" "vertraulich" "SECRET" "secret"})))
  (let [{:keys [local created]} (peering/marking-plan "abcdef12-x" [{:name "intern"}
                                                                    {:name "Neu Gut"}]
                                                      {"intern" "vertraulich"})]
    (is (= [:vertraulich :peer-abcdef12-neu-gut] local))
    (is (= {"Neu Gut" "peer-abcdef12-neu-gut"} created)
        "an unknown marking is created under a name of the connection's own")))

(deftest a-mapping-is-checked-against-both-sides
  (let [ok {:local-attrs {:nr :string :titel :string :betrag [:decimal 10 2]}
            :remote-attrs {:nummer :string :title :string :betrag [:decimal 12 2]}
            :local-product "vertraege" :remote-product "vertraege"}]
    (is (= [] (peering/mapping-problems {:properties {"nr" "nummer" "titel" "title"}
                                         :source-data :export}
                                        ok)))
    (is (= [{:problem :type-mismatch :property "nr" :local :string :remote :decimal}]
           (peering/mapping-problems {:properties {"nr" "betrag"}} ok)))
    (is (= [{:problem :mapped-twice :remote-property "nummer"}]
           (peering/mapping-problems {:properties {"nr" "nummer" "titel" "nummer"}} ok)))
    (is (= [{:problem :not-from-a-product :side :remote}]
           (peering/mapping-problems {:properties {"nr" "nummer"}}
                                     (dissoc ok :remote-product))))
    (is (= [{:problem :different-products :local "vertraege" :remote "andere"}]
           (peering/mapping-problems {:properties {"nr" "nummer"}}
                                     (assoc ok :remote-product "andere"))))
    (is (= [{:problem :unpeerable-type :property "v" :type :vector}]
           (peering/mapping-problems {:properties {"v" "v"}}
                                     (assoc ok :local-attrs {:v :vector}
                                            :remote-attrs {:v :vector}))))))

(deftest an-object-is-named-on-both-sides
  (is (= (peering/peered-id "c" "7") (peering/peered-id "c" "7")) "the same on every call")
  (is (not= (peering/peered-id "c" "7") (peering/peered-id "d" "7"))
      "two connections' objects never meet")
  (is (= {:id "x" :yours "7"} (peering/outbound-ref {"x" "7"} "x")))
  (is (= "7" (peering/inbound-id "c" {} {:id "x" :yours "7"}))
      "an object that came from here goes back under its own name")
  (is (= "x" (peering/inbound-id "c" {"9" "x"} {:id "9"})))
  (is (= (peering/peered-id "c" "9") (peering/inbound-id "c" {} {:id "9"})))
  (is (= {"nr" "1"} (peering/outbound-values {:properties {"nr" "nummer"}}
                                             {:nr "1" :geheim "x"}))
      "only mapped properties leave")
  (is (= {:nr "1"} (peering/inbound-values {:properties {"nr" "nr2"}} {"nr2" "1" "x" 2}))))

(deftest the-later-edit-stands
  (is (peering/edit-stands? {:at 5 :from "a"} nil))
  (is (peering/edit-stands? {:at 5 :from "a"} {:at 4 :from "b"}))
  (is (not (peering/edit-stands? {:at 3 :from "a"} {:at 4 :from "b"})))
  (testing "at the same instant both sides decide alike"
    (is (not= (peering/edit-stands? {:at 4 :from "a"} {:at 4 :from "b"})
              (peering/edit-stands? {:at 4 :from "b"} {:at 4 :from "a"})))))

(deftest health-and-backoff
  (is (= :idle (:state (peering/health []))))
  (is (= :failing (:state (peering/health [{:data-type :source-data :way :out
                                            :state :failed}
                                           {:data-type :source-data :way :out :state :ok}]))))
  (is (= :healthy (:state (peering/health [{:data-type :source-data :way :out :state :ok}
                                           {:data-type :source-data :way :out
                                            :state :failed}])))
      "only the newest job of a kind counts")
  (is (= 1000 (peering/backoff-ms 0)))
  (is (= 300000 (peering/backoff-ms 30))))
