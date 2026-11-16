(ns ^:unit platform.hot-patches-replay-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [sutra.serve.hot-patches :as hot]))

(defn- tmp-dir []
  (str (java.nio.file.Files/createTempDirectory "hot"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))))

(defn- boot [dir image]
  (let [loaded (atom [])]
    (with-redefs [hot/armed?      (constantly true)
                  hot/hot-dir     (constantly dir)
                  hot/image-id    (constantly image)
                  hot/load-source! (fn [path code] (swap! loaded conj [path code]) path)
                  hot/reload-server! (fn [])]
      (hot/replay!))
    @loaded))

(defn- patch! [dir image files]
  (with-redefs [hot/hot-dir  (constantly dir)
                hot/image-id (constantly image)]
    (hot/record! files)))

(deftest a-restart-of-the-same-image-replays-its-patches
  (let [dir (tmp-dir)]
    (patch! dir "img-a" [{:path "src/a.clj" :code "(ns a)"}])
    (patch! dir "img-a" [{:path "src/b.clj" :code "(ns b)"} {:path "src/a.clj" :code "(ns a) 2"}])
    (testing "every patch, the latest code, in the order it was last sent"
      (is (= [["src/b.clj" "(ns b)"] ["src/a.clj" "(ns a) 2"]] (boot dir "img-a"))))
    (testing "and again on the next restart"
      (is (= 2 (count (boot dir "img-a")))))))

(deftest a-new-image-discards-them
  (let [dir (tmp-dir)]
    (patch! dir "img-a" [{:path "src/a.clj" :code "(ns a)"}])
    (is (= [] (boot dir "img-b")))
    (is (not (.exists (io/file dir "manifest.edn"))))
    (is (= [] (boot dir "img-a")) "nothing comes back after the discard")))

(deftest a-patch-on-a-new-image-starts-a-fresh-ledger
  (let [dir (tmp-dir)]
    (patch! dir "img-a" [{:path "src/a.clj" :code "(ns a)"}])
    (patch! dir "img-b" [{:path "src/b.clj" :code "(ns b)"}])
    (is (= [["src/b.clj" "(ns b)"]] (boot dir "img-b")))))
