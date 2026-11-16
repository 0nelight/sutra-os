(ns ^:unit grammar.protection-view-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.kit.remote :as rd]
            [sutra.types.protection :as protection]))

(def ^:private answer
  {:sections ["object-types" "actions"] :effective ["object-types/flug"]
   :trunk-protected true})

(deftest a-resource-before-and-after-the-answer
  (is (= :loading (protection/ref-state rd/loading "object-types/flug")))
  (is (= :failed (protection/ref-state (rd/failed "x") "object-types/flug")))
  (is (= :protected (protection/ref-state answer "object-types/flug")))
  (is (= :open (protection/ref-state answer "actions/buchen")))
  (is (= :unprotectable (protection/ref-state answer "sources/erp"))))

(deftest main-is-said-only-with-an-answer
  (is (= {:show :loading} (protection/trunk-view rd/loading)))
  (is (= {:show :failed :reason "x"} (protection/trunk-view (rd/failed "x"))))
  (is (= {:show :protected} (protection/trunk-view answer)))
  (is (= {:show :open} (protection/trunk-view (dissoc answer :trunk-protected)))))
