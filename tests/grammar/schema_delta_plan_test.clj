(ns ^:unit grammar.schema-delta-plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.governance.merge-plan :as mp]))

(deftest link-backing-signature-detects-a-changed-backing
  (let [sig mp/link-backing-sig]
    (testing "a producer SWAP (join → differently-named endpoint-fk) changes the signature"
      (let [join {:transforms {:reconcile-x {:language :pg-sql :query "q"
                                             :output {:kind :link-type :link-type :L
                                                      :from-key-column :a :to-key-column :b
                                                      :dataset :ds1}}}}
            fk   {:transforms {:link-L {:input {:kind :object-type :object-type :x}
                                        :output {:kind :link-type :link-type :L
                                                 :fk-attribute :fk :dataset :ds2}}}}]
        (is (not= (get (sig join) :L) (get (sig fk) :L)))))
    (testing "an IN-PLACE flip (same producer, resolve-by-shared-property → endpoint-fk) changes it"
      (let [rsp {:transforms {:link-L {:language :pg-sql :strategy :resolve-by-shared-property
                                       :output {:kind :link-type :link-type :L
                                                :shared-property :sp}}}}
            fk  {:transforms {:link-L {:input {:kind :object-type :object-type :x}
                                       :output {:kind :link-type :link-type :L
                                                :fk-attribute :fk}}}}]
        (is (not= (get (sig rsp) :L) (get (sig fk) :L)))))
    (testing "ONLY the :dataset differing is NOT a backing change (dataset is excluded)"
      (let [a {:transforms {:link-L {:input {:x 1} :output {:kind :link-type :link-type :L
                                                            :fk-attribute :fk :dataset :ds9}}}}
            b {:transforms {:link-L {:input {:x 1} :output {:kind :link-type :link-type :L
                                                            :fk-attribute :fk :dataset :dsOTHER}}}}]
        (is (= (get (sig a) :L) (get (sig b) :L)))))
    (testing "a producer-less (writeback) link is absent — no backing to change"
      (is (nil? (get (sig {:transforms {}}) :L))))))

(deftest a-merge-plan-is-decided-without-a-store
  (let [old  {:transforms {:t {:semantic-version 1 :output {:kind :dataset :dataset :d}}}}
        full {:transforms {:t {:semantic-version 2 :output {:kind :dataset :dataset :d}}}
              :datasets   {:d {:columns [:a]}}}
        p    (mp/plan {:parent "main" :old old :full full
                       :staged-rows [{:seq 1 :action :plain :args {:x 1}}]
                       :applied {:ops [:op] :log [:entry] :records [:r]}
                       :retired [:gone]
                       :actor "ann"})]
    (testing "the commit carries the merged world, the applied rows and the staged rows' deletion"
      (is (= full (get-in p [:tx :world])))
      (is (= [:op] (get-in p [:tx :ops])))
      (is (= [:r :gone] (get-in p [:tx :records])))
      (is (= {:principal "ann"} (get-in p [:tx :actor]))))
    (testing "nothing staged deletes nothing"
      (is (= [:r] (get-in (mp/plan {:parent "main" :old {} :full {} :staged-rows []
                                    :applied {:records [:r]} :retired [:gone]})
                          [:tx :records]))))))

(deftest what-follows-a-commit-is-decided-without-a-store
  (let [join {:language :pg-sql :query "q"
              :output {:kind :link-type :link-type :L :from-key-column :a :to-key-column :b}}
        fk   {:input {:kind :object-type :object-type :x}
              :output {:kind :link-type :link-type :L :fk-attribute :fk}}
        head {:link-types {:L {}}
              :transforms {:mk-l fk
                           :t {:semantic-version 2 :output {:kind :dataset :dataset :d}}}
              :datasets   {:d {:columns [:a]}}}
        base (assoc head :transforms {:mk-l join :t {:semantic-version 1}})
        fp   (get (mp/link-backing-fingerprints head) :L)
        seen {:head head :at :t0 :kept-links {} :kept-versions {}}]
    (testing "against the commit's base: a changed producer re-registers, a risen version
              starts over, then the llm rates"
      (is (= [[:reregister :L fp :retract] [:keep-version :mk-l 1]
              [:reset :t 2 {:d {:columns [:a]}}] [:record-llm-rates]]
             (mp/after-commit seen base))))
    (testing "at boot, without a base, what is not kept yet is kept as it is"
      (is (= [[:keep-link :L fp] [:keep-version :mk-l 1] [:keep-version :t 2]
              [:record-llm-rates]]
             (mp/after-commit seen nil))))
    (testing "what is kept already is owed nothing; a head without a schema commit records no
              rates"
      (is (= [] (mp/after-commit (assoc seen :at nil :kept-links {"L" fp}
                                        :kept-versions {"mk-l" 1 "t" 2})
                                 base))))))

(deftest a-reset-is-owed-by-the-head-not-by-a-commit
  (let [head {:transforms {:t {:semantic-version 3 :output {:kind :dataset :dataset :d}}
                           :u {:semantic-version 1 :output {:kind :dataset :dataset :e}}}
              :datasets   {:d {:columns [:a]} :e {:columns [:b]}}}]
    (testing "a version above the one kept is owed a reset, with its outputs"
      (is (= [[:t 3 {:d {:columns [:a]}}]] (mp/owed-resets head {"t" 2 "u" 1} nil))))
    (testing "the version kept is owed nothing"
      (is (= [] (mp/owed-resets head {"t" 3 "u" 1} nil))))
    (testing "one not kept yet is judged against the commit's base"
      (is (= [[:t 3 {:d {:columns [:a]}}] [:u 1 nil]]
             (mp/owed-resets head {}
                             {:transforms {:t {:semantic-version 2} :u {:semantic-version 1}}}))))
    (testing "without a base, one not kept yet is kept as it is — a first boot resets nothing"
      (is (= [[:t 3 nil] [:u 1 nil]] (mp/owed-resets head {} nil))))))

(deftest a-re-registration-is-owed-by-the-head-not-by-a-commit
  (let [join  {:language :pg-sql :query "q"
               :output {:kind :link-type :link-type :L :from-key-column :a :to-key-column :b}}
        fk    {:input {:kind :object-type :object-type :x}
               :output {:kind :link-type :link-type :L :fk-attribute :fk}}
        world (fn [t] {:link-types {:L {} :M {}} :transforms (if t {:mk-l t} {})})
        fps   (mp/link-backing-fingerprints (world join))]
    (testing "every link type is fingerprinted, a producer-less one too"
      (is (= #{:L :M} (set (keys fps)))))
    (testing "a producer other than the one kept is owed: what the old one made goes"
      (is (= [[:L (get (mp/link-backing-fingerprints (world fk)) :L) :retract]]
             (mp/owed-reregisters (world fk) {"L" (fps :L) "M" (fps :M)} nil))))
    (testing "a first producer is owed the rows as edits: no producer made them"
      (is (= [[:L (fps :L) :adopt]]
             (mp/owed-reregisters (world join) {"L" (fps :M) "M" (fps :M)} nil)))
      (is (= [[:L (fps :L) :adopt] [:M (fps :M) nil]]
             (mp/owed-reregisters (world join) {} (world nil)))))
    (testing "the producer kept is owed nothing"
      (is (= [] (mp/owed-reregisters (world join) {"L" (fps :L) "M" (fps :M)} nil))))
    (testing "one not kept yet is judged against the commit's base"
      (is (= [[:L (get (mp/link-backing-fingerprints (world fk)) :L) :retract] [:M (fps :M) nil]]
             (mp/owed-reregisters (world fk) {} (world join)))))
    (testing "without a base, one not kept yet is kept as it is"
      (is (every? (complement last) (mp/owed-reregisters (world fk) {} nil))))))
