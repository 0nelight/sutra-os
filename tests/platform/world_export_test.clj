(ns ^:unit platform.world-export-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.world-export :as ke]
            [sutra.world.db :as kdb]
            [sutra.world.gen.near :as gn]
            [sutra.parser.project-emit :as emit]
            [sutra.parser.project-export :as pe]))

(defn- exports-or-threw [W]
  (try (emit/export W {:all-projects? true}) (pe/exports W {:all-projects? true})
       (catch Exception _ ::threw)))

(defspec the-held-export-is-the-export 1500
  (prop/for-all [W gn/world]
    (let [old (exports-or-threw W)
          d   (kdb/db W ke/rows)]
      (if (= ::threw old)
        (= ::threw (try @(:authored (ke/held d)) (catch Exception _ ::threw)))
        (let [h (ke/held d)]
          (and (= @(:laid old) @(:laid h))
               (= @(:units old) @(:units h))
               (= @(:authored old) @(:authored h))
               (= @(:inlined old) @(:inlined h))
               (= (:schema (:ent (emit/export (pe/without-layouts W)
                                               {:all-projects? true :inline? false})))
                  @(:laid-body h))))))))
