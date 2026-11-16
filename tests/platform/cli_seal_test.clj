(ns ^:unit platform.cli-seal-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.seal :as seal]
            [sutra.sources.egress :as egress])
  (:import (java.security KeyFactory KeyPairGenerator)
           (java.util Base64)))

(defn- fresh-keypair!
  []
  (let [kp (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048)))]
    (swap! @#'egress/running* assoc :keypair {:private (.getPrivate kp) :public (.getPublic kp)})
    (.encodeToString (Base64/getEncoder) (.getEncoded (.getPublic kp)))))

(defn- open [b64] (#'egress/rsa-decrypt b64))

(deftest seals-what-the-agent-opens
  (let [pub (fresh-keypair!)]
    (testing "a credential map — the shape `source secret` seals"
      (let [secrets {:user "svc_reader" :password "hunter2-ünïcode-✓"}]
        (is (= secrets (read-string (open (seal/seal pub (pr-str secrets))))))))

    (testing "the padding is randomised: the same plaintext seals differently"
      (let [a (seal/seal pub "same") b (seal/seal pub "same")]
        (is (not= a b))
        (is (= "same" (open a) (open b)))))

    (testing "the boundary lengths RFC 8017 allows — empty, and the longest message
              a 2048-bit key can carry (k − 2·hLen − 2 = 190 bytes)"
      (is (= "" (open (seal/seal pub ""))))
      (let [m (apply str (repeat 190 "x"))]
        (is (= m (open (seal/seal pub m))))))

    (testing "one byte past the maximum is refused here, not silently truncated"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"too large to seal"
                            (seal/seal pub (apply str (repeat 191 "x"))))))))

(deftest keys-of-other-sizes-work
  (testing "k is read from the key, not assumed — a 3072-bit agent key seals too"
    (let [kp  (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 3072)))
          _   (swap! @#'egress/running* assoc
                     :keypair {:private (.getPrivate kp) :public (.getPublic kp)})
          pub (.encodeToString (Base64/getEncoder) (.getEncoded (.getPublic kp)))]
      (is (= "secret" (open (seal/seal pub "secret"))))
      (is (some? (.generatePublic (KeyFactory/getInstance "RSA")
                                  (java.security.spec.X509EncodedKeySpec.
                                   (.decode (Base64/getDecoder) pub))))))))
