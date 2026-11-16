(ns ^:unit platform.egress-secret-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [sutra.cli.seal :as seal]
            [sutra.sources.egress :as egress]))

(deftest fields-sealed-one-at-a-time-all-reach-the-source
  (let [kp     @#'egress/running*
        before @kp
        f      (io/file (System/getProperty "java.io.tmpdir") (str "egress-key-" (random-uuid)))]
    (try
      (with-out-str (#'egress/load-or-gen-keypair! (str f)))
      (let [pub   (#'egress/public-key-b64)
            c     #(seal/seal pub (pr-str %))
            spec  (#'egress/resolve-spec {:source-name :mail :type :imap
                                          :secret-cipher (str (c {:user "a" :password "alt"}) " "
                                                              (c {:password "neu"}))})]
        (is (= "a" (:user spec)) "the first field survives the second")
        (is (= "neu" (:password spec)) "a later cipher wins")
        (is (not (contains? spec :secret-cipher))))
      (finally
        (reset! kp before)
        (io/delete-file f true)))))
