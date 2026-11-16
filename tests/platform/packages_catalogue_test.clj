(ns ^:unit platform.packages-catalogue-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [sutra.packages.standard :as packages]))

(deftest every-entry-is-on-the-classpath-and-reads
  (is (= 5 (count packages/catalogue)))
  (doseq [{:keys [id file name description] :as p} packages/catalogue]
    (testing id
      (is (and (string? name) (seq name) (string? description) (seq description)))
      (doseq [f (packages/file-chain file)]
        (is (io/resource (str packages/resource-dir f))
            (str f " (required by " file ") is on the classpath")))
      (let [schema (packages/read-package file)]
        (is (seq (:objects schema)) "the :require chain merged into object types"))
      (let [by-file (into {} (map (juxt :file (comp keyword :id))) packages/catalogue)]
        (is (= (:requires p)
               (vec (keep by-file
                          (sort-by #(.indexOf ^java.util.List (mapv :file packages/catalogue) %)
                                    (rest (packages/file-chain file))))))
            "the declared :requires is the catalogued part of the file's :require chain")))))

(deftest the-catalogue-is-offered-as-data
  (let [offers (get-in packages/module [:contributes :products/offers])]
    (is (= #{:basis :privat :betrieb :einzelunternehmen :mail} (set (keys offers))))
    (is (every? fn? (map :schema (vals offers))))))

(deftest a-jar-resource-is-extracted-with-its-chain
  (testing "read-ent needs a File; a non-file: URL is copied out with its requires"
    (let [p {:file "3_privat.ent"}]
      (with-redefs [io/resource (let [orig io/resource]
                                  (fn [& args]
                                    (let [u ^java.net.URL (apply orig args)]
                                      (when u
                                        (java.net.URL. nil (str "jar:" u "!/")
                                                       (proxy [java.net.URLStreamHandler] []
                                                         (openConnection [_]
                                                           (.openConnection u))))))))]
        (is (not= "file"
                  (.getProtocol
                   ^java.net.URL (io/resource (str packages/resource-dir "3_privat.ent")))))
        (is (contains? (:objects (packages/read-package (:file p))) :person))))))
