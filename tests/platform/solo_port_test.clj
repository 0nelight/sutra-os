(ns ^:unit platform.solo-port-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]))

(load-file "distributions/sutra-solo/src/sutra/solo/port.clj")

(def ^:private choose  @(resolve 'sutra.solo.port/choose))
(def ^:private derived @(resolve 'sutra.solo.port/derived))
(def ^:private stored  @(resolve 'sutra.solo.port/stored))
(def ^:private persist! @(resolve 'sutra.solo.port/persist!))

(defn- tmp-dir []
  (doto (.toFile
         (java.nio.file.Files/createTempDirectory "solo-home"
                                                  (make-array java.nio.file.attribute.FileAttribute
                                                              0)))
    (.deleteOnExit)))

(deftest derived-port-stays-clear-of-the-dev-checkouts
  (doseq [d [(io/file "/home/a/.local/share/sutra") (io/file "/srv/zweite") (tmp-dir)]]
    (let [p (derived d)]
      (is (<= 18030 p 19029))
      (is (not (<= 7888 p 9880)) "bin/ports.sh: nREPL, HTTP and FE bases plus offsets 0..250")))
  (is (= (derived (io/file "/srv/zweite")) (derived (io/file "/srv/zweite")))
      "stable per directory"))

(deftest choice-order-env-then-stored-then-first-free
  (let [dir (tmp-dir)]
    (is (= {:port 8095 :via :env} (choose dir {:env-port "8095" :stored 18500})))
    (is (= {:port 18500 :via :stored} (choose dir {:stored 18500})))
    (testing "the derived port taken: the next free one, wrapping inside the range"
      (let [d (derived dir)]
        (is (= {:port d :via :derived} (choose dir {:free? (constantly true)})))
        (is (not= d (:port (choose dir {:free? #(not= d %)}))))
        (is (<= 18030 (:port (choose dir {:free? #(not= d %)})) 19029))))
    (is (nil? (:port (choose dir {:free? (constantly false)}))))))

(deftest chosen-port-is-kept-across-restarts
  (let [dir (tmp-dir)]
    (is (nil? (stored dir)))
    (persist! dir 18777)
    (is (= 18777 (stored dir)))
    (is (= {:port 18777 :via :stored} (choose dir {:stored (stored dir)})))))
