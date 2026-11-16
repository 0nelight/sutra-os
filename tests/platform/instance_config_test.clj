(ns ^:unit platform.instance-config-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.store.storage.instance-config :as ic]
            [support.mem :as mem]))

(deftest a-setting-reads-back-what-was-written
  (let [store (mem/store)]
    (is (= "v1" (ic/set-cfg! store :some_key "v1")))
    (is (= "v1" (ic/get-cfg store :some_key)))
    (testing "a second write replaces the first"
      (ic/set-cfg! store :some_key "v2")
      (is (= "v2" (ic/get-cfg store :some_key))))
    (testing "an absent key falls back to the default"
      (is (nil? (ic/get-cfg store :never_set)))
      (is (= "fallback" (ic/get-cfg store :never_set "fallback"))))
    (testing "several settings land in one write"
      (ic/set-cfgs! store {:a "1" :b "2"})
      (is (= ["1" "2"] [(ic/get-cfg store :a) (ic/get-cfg store :b)])))))

(deftest set-present-leaves-blank-values-alone
  (let [store (mem/store)]
    (ic/set-cfg! store :url "ldap://x")
    (ic/set-present! store {:url "" :realm "EXAMPLE"})
    (is (= "ldap://x" (ic/get-cfg store :url)))
    (is (= "EXAMPLE" (ic/get-cfg store :realm)))))
