(ns ^:unit grammar.export-facet-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.entities :as ents]))

(def ^:private not-exported
  {:applications "written as app and view files (export-units), not as a section"})

(deftest every-export-facet-is-well-formed
  (doseq [{:keys [section export]} ents/entities
          :when export]
    (testing section
      (is (#{:project :global :container :imports :repository :owned :all :none} (:scope export)))
      (is (contains? #{nil :units} (:into export)))
      (is (or (nil? (:kinds export)) (set? (:kinds export))))
      (is (contains? #{nil :transforms :query-functions} (:generated export)))
      (is (contains? #{nil :tuples} (:shape export)))
      (is (empty? (dissoc export :scope :into :kinds :generated :shape))
          "an export is said by these facets alone; a new need is a new facet, not an emitter"))))

(deftest an-owned-section-is-exported-or-named
  (testing "a section with a :project owner on a tree reaches the export, or says why not"
    (doseq [{:keys [section export tree fields]} ents/entities
            :when (and tree (contains? fields :project))]
      (is (or export (contains? not-exported section)) (str section " has no :export"))
      (is (not (and export (contains? not-exported section)))
          (str section " is exported and listed as not exported")))))

(deftest one-author-key-per-exported-section
  (let [ks (for [{:keys [section export]} ents/entities
                 :when (and export (not= :units (:into export)))]
             (ents/section->author section))]
    (is (= (count ks) (count (distinct ks))))))
