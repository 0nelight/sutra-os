(ns ^:unit ontology.importer-index-times-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.builds.import.phases :as phases]
            [sutra.projects.kompass :as cp]))

(def ^:private publish-phase #'phases/publish-phase)

(def ^:private w-rid "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000000a")
(def ^:private g-rid "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000000b")
(def ^:private l-rid "ri.sutra.main.link-type.00000000-0000-0000-0000-00000000000c")

(defn- publish! [fails]
  (with-redefs [cp/write-import-keys!        (fn [& _])
                phases/submit-partitioned!   (fn [& _] {:failures fails :interpret-ms 0})]
    (some (fn [{:keys [io types]}] (when (= :index-times io) (set types)))
          (:effects
           (publish-phase {:store {} :schema {} :run-id "r1"
                           :sub (fn [_ f] (f)) :cancel-at (constantly nil)}
                          {:decls [] :publish-failures [] :unpublished {:objects 0 :links 0}
                           :rid->name {w-rid :widget g-rid :gadget l-rid :owns}
                           :pk-index {w-rid {} g-rid {}}})))))

(deftest every-type-the-run-indexed-is-stamped
  (is (= #{w-rid g-rid} (publish! []))))

(deftest a-type-whose-objects-failed-to-publish-keeps-its-time
  (is (= #{g-rid}
         (publish! [{:kind :objects :verb :create :type w-rid :count 1 :fn :create-widget
                     :error "x"}
                    {:kind :links :verb :create :type l-rid :count 1 :fn :create-owns
                     :error "x"}]))))
