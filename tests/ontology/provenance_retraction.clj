(ns ^:integration ontology.provenance-retraction
  (:require [clojure.test :refer [deftest use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.projects.kompass :as cp]
            [sutra.builds.import.materialize :as mat]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private schema {:object-types {:host {:survivorship {:order [:a :b]}}}})

(defn- survived [store E] (mat/survived-after-retraction schema :host E
                                                      (cp/latest-provenance-by-source store E)))

(deftest per-source-retraction
  (let [branch (str "prov-" (subs (str (java.util.UUID/randomUUID)) 0 8))
        store #(ctx/store branch)
        E  "E-host-1"]
    (try
      (store/fork! branch)

      (section "two sources contribute to one entity; a outranks b")
      (cp/write-provenance!
       (store) [{:entity-id E :property :name :value "A-name" :source :a :run-id "r1"}
             {:entity-id E :property :os   :value "A-os"   :source :a :run-id "r1"}
             {:entity-id E :property :name :value "B-name" :source :b :run-id "r1"}
             {:entity-id E :property :site :value "B-site" :source :b :run-id "r1"}])
      (check "a wins name+os; b contributes site (survivorship precedence)"
             (= {:name "A-name" :os "A-os" :site "B-site"} (survived (store) E)))

      (section "retract source a → name falls back to b, os vanishes, site stays")
      (check "retracted a's two live properties" (= 2 (cp/retract-provenance! (store) E :a "r2")))
      (check "survivorship recomputed from the remaining source b"
             (= {:name "B-name" :site "B-site"} (survived (store) E)))

      (section "retract source b too → entity fully retracted (delete signal)")
      (cp/retract-provenance! (store) E :b "r3")
      (check "no contributor remains ⇒ nil (the object should be DELETED)"
             (nil? (survived (store) E)))

      (section "re-assert b after retraction resurrects its contribution")
      (cp/write-provenance!
       (store) [{:entity-id E :property :site :value "B-site-2" :source :b :run-id "r4"}])
      (check "the latest (post-retraction) b value wins again"
             (= {:site "B-site-2"} (survived (store) E)))

      (finally (try (store/retire! branch) (catch Exception _ nil))))))
