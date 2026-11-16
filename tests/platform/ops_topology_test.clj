(ns ^:unit platform.ops-topology-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.ops :as op]
            [sutra.foundation.rpc :as rpc]))

(def ^:private split
  {:profile :split
   :self    :api
   :processes {:api       {:services #{:platform :feedback :lineage}}
               :conductor {:services #{:automation :health} :url "http://conductor:8033/"}}})

(deftest reading-a-topology
  (is (= op/solo (op/read-topology nil)))
  (is (= op/solo (op/read-topology "  ")))
  (is (= split (op/read-topology (pr-str split))))
  (testing "a malformed topology is refused at start"
    (is (thrown? clojure.lang.ExceptionInfo (op/read-topology "{:profile :x}")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (op/read-topology (pr-str (assoc split :self :nowhere)))))))

(deftest placing-an-operation
  (testing "solo serves everything here"
    (is (every? #(= :local (op/placement op/solo %)) (keys (op/entries)))))
  (testing "a service of this process stays here"
    (is (= :local (op/placement split :lineage/feeds-line))))
  (testing "a service of another process goes there, its url without a trailing slash"
    (is (= {:process :conductor :url "http://conductor:8033"}
           (op/placement split :automation/list))))
  (testing "a service nobody serves, or a process without an address, is a fault"
    (is (thrown? clojure.lang.ExceptionInfo (op/placement split :canvas/saved-graph)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (op/placement (update-in split [:processes :conductor] dissoc :url)
                               :automation/list))))
  (testing "the services no process serves"
    (is (= [] (op/topology-problems op/solo)))
    (is (some #{:canvas} (op/topology-problems split)))))

(deftest a-remote-call-posts-and-checks-what-comes-back
  (let [sent (atom nil)]
    (op/set-peer-secret! "unit-secret")
    (binding [op/*topology* split]
      (with-redefs [rpc/call! (fn [opts payload] (reset! sent [opts payload]) [])]
        (is (= [] (op/query {:store {:branch "b1" :authority {:user "u"}}}
                            :automation/list {:branch "b1"}))))
      (let [[{:keys [url token]} envelope] @sent]
        (is (= "http://conductor:8033/api/ops" url))
        (is (= (op/peer-token) token))
        (is (= {:op :automation/list :kind :query :in {:branch "b1"} :branch "b1"
                :authority {:user "u"} :schema? false}
               (op/open "unit-secret" (:ts envelope) envelope))
            "the call travels signed, and opens under the same secret")
        (is (thrown? clojure.lang.ExceptionInfo
                     (op/open "unit-secret" (:ts envelope) envelope))
            "once"))
      (testing "an answer that breaks the :out contract is refused"
        (with-redefs [rpc/call! (fn [_ _] {:not "a vector"})]
          (is (thrown? clojure.lang.ExceptionInfo
                       (op/query {:store {:branch "main"}} :automation/list
                                 {:branch "main"}))))))))

(deftest a-process-serves-only-its-own-operations
  (binding [op/*topology* split]
    (is (thrown? clojure.lang.ExceptionInfo
                 (op/serve {} :automation/list :query {:branch "main"})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (op/serve {} :lineage/feeds-line :command {:ids []})))))
