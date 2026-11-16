(ns ^:integration platform.flaw-release-rewritten-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.authoring.install.world :as onto-install]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private keeper
  {:object-types {"ri.t.keep" {:object-type-name :bleibt
                               :attribute-list [{:attribute-name :nr :attribute-type :string}]
                               :plural "Bleibende"}}})

(defn- fx [src]
  {:function-name :fx :language :clojure :source src :version "1.0.0"
   :releases {"1.0.0" {:language :clojure :source src}}})

(defn- install! [b w]
  (onto-install/install-world! (ctx/system-store b) b w {:actor {:principal "test"}}))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest release-rewritten
  (store/with-fresh-branch [b "release"]
    (install! b (assoc keeper :functions {"ri.f.1" (fx "(fn [] 1)")}))
    (install! b keeper)
    (is (empty? (:functions (world/authored (world/at (ctx/system-store) b))))
         "the function is gone")
    (testing "re-created under another rid, the same version with other code is refused"
      (is (= :release-conflict
             (:reason (refusal #(install! b
                                          (assoc keeper
                                                 :functions {"ri.f.2" (fx "(fn [] 2)")})))))))
    (testing "the same code under that version is the same release"
      (is (nil? (refusal #(install! b (assoc keeper :functions {"ri.f.3" (fx "(fn [] 1)")}))))))))
