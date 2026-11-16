(ns ^:unit platform.context-project-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]))

(def ^:private vorher
  {:projects     {"ri.p.lager" {:project-name :lager} "ri.p.einkauf" {:project-name :einkauf}}
   :object-types {"ri.ot.alt" {:object-type-name :alt :plural "Alts"}}})

(def ^:private nachher
  (-> vorher
      (assoc-in [:object-types "ri.ot.neu"] {:object-type-name :neu})
      (assoc-in [:object-types "ri.ot.hat"] {:object-type-name :hat :project "ri.p.einkauf"})
      (assoc-in [:link-types "ri.lt.l"]
                {:link-type-name :l :from-object-type "ri.ot.hat" :to-object-type "ri.ot.neu"})))

(deftest what-nothing-places-goes-to-the-context
  (let [w (member/place-new forms/installed vorher nachher "lager")]
    (is (= "ri.p.lager" (get-in w [:object-types "ri.ot.neu" :project]))
        "new and unplaced: the context's project")
    (is (nil? (get-in w [:object-types "ri.ot.alt" :project]))
        "what existed is never moved, also when it has no project")
    (is (= "ri.p.einkauf" (get-in w [:object-types "ri.ot.hat" :project]))
        "a declared project stays")
    (is (nil? (get-in w [:link-types "ri.lt.l" :project]))
        "a link derives its project from its source type — the context does not override derivation")))

(deftest the-context-names-a-project-by-name-or-rid
  (is (= "ri.p.lager"
         (get-in (member/place-new forms/installed vorher nachher "ri.p.lager")
                 [:object-types "ri.ot.neu" :project])))
  (is (= nachher (member/place-new forms/installed vorher nachher "gibtsnicht"))
      "an unknown project places nothing"))
