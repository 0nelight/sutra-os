(ns sutra.repos.access
  (:require [sutra.catalog.grants :as grants]
            [sutra.parser.project-export :as pe]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn ontology-readable?
  [auth]
  (and (grants/all-projects? auth)
       (empty? (:hidden-object-types auth))
       (empty? (:denied-markings auth))))

(defn repo-readable?
  [auth schema repo-rid]
  (if (rid/ontology-repo? repo-rid)
    (ontology-readable? auth)
    (let [granted (grants/project-operations auth)
          super?  (grants/all-projects? auth)
          rspec   (get (:repositories schema) repo-rid)
          members (into [] (comp (mapcat #(vals (get schema %)))
                                 (filter #(= repo-rid (:repository %))))
                        pe/code-sections)]
      (boolean
       (and rspec
            (or super?
                (and (grants/resource-granted? schema granted rspec)
                     (every? #(grants/resource-granted? schema granted %) members))))))))
