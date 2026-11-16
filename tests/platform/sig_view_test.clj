(ns ^:unit platform.sig-view-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [sutra.catalog.signature :as signature]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.world.gen.near :as gn]
            [sutra.types.rid :as rid]))

(def g-query-app
  (gen/let [n     (gen/elements [:a :b :c])
            root  (gen/elements [:a :b :c])
            where (gen/vector (gen/tuple (gen/elements [:x :y :z]) (gen/elements [:= :<])
                                         gen/nat)
                              0 2)
            order (gen/elements [nil :x :y])]
    [[:applications (gn/rid-of :applications n)]
     {:layout {:vars {:v (cond-> {:source root :where where} order (assoc :order-by order))}}}]))

(def g-change (gen/frequency [[5 gn/change] [1 g-query-app]]))

(def ^:private g-decl-change
  (gen/one-of
   [(gen/let [n (gen/elements [:a :b :c]) cols (gen/elements [nil [] [{:attribute-name :x}]])]
      [[:datasets (gn/rid-of :datasets n)] {:columns cols}])
    (gen/let [n (gen/elements [:a :b :c]) k (gen/elements [nil :object-backing :virtual])]
      [[:datasets (gn/rid-of :datasets n)] {:kind k}])
    (gen/let [n (gen/elements [:a :b :c]) cols (gen/elements [nil [{:attribute-name :y}]])]
      [[:transforms (gn/rid-of :transforms n)] {:output-columns cols}])
    (gen/let [n (gen/elements [:a :b :c])]
      [[:datasets (rid/dataset-rid-for (gn/rid-of :transforms n))]
       {:columns [{:attribute-name :z}]}])]))

(defspec stepped-declarations-are-the-whole-ones 1000
  (prop/for-all [W  gn/world
                 cs (gen/vector (gen/vector (gen/frequency [[3 gn/change] [2 g-decl-change]])
                                            1 3)
                                1 4)]
    (loop [W W v (signature/decls-init W) [c & more] cs]
      (if-not c
        true
        (let [W1 (reduce gn/apply-change W c)
              v1 (signature/decls-step v W W1 (map first c))]
          (if (= v1 (signature/decls-init W1))
            (recur W1 v1 more)
            false))))))
