(ns ^:unit platform.pull-local-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [platform.push-local-test]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.projection :as proj]))

(def ^:private schema @#'platform.push-local-test/schema)
(def ^:private g-memory @#'platform.push-local-test/g-memory)
(def ^:private source @#'platform.push-local-test/source)
(def ^:private paths @#'platform.push-local-test/paths)

(defn- outcome [f]
  (try {:ok (f)}
       (catch clojure.lang.ExceptionInfo e
         {:refused (or (:refusal/code (ex-data e)) (:kind (ex-data e)) (ex-message e))})))

(defn- seen
  [formats v]
  (-> v
      (update :files (fn [fs] (into (sorted-map)
                                    (for [[p t] fs]
                                      [p (try (:schema (authoring/read-file formats p t))
                                              (catch clojure.lang.ExceptionInfo _ [:text t]))]))))
      (update :unreadable (comp set keys))))

(def ^:private formats (:formats (proj/declared schema :ontologie)))

(def ^:private g-pick
  (gen/one-of [(gen/fmap (fn [ts] {:texts (vec (distinct ts))})
                         (gen/vector (gen/elements paths) 1 3))
               (gen/fmap (fn [p] {:project p})
                         (gen/elements [:controlling :konstruktion :lager]))]))

(defspec a-pull-of-some-texts-is-the-whole-pull-cut-to-them 1500
  (prop/for-all [memory g-memory
                 pick   g-pick]
    (let [whole (outcome #(seen formats (proj/view schema :ontologie memory pick)))
          local (outcome #(seen formats (proj/view schema :ontologie (source memory) pick)))]
      (or (= whole local)
          (do (println "pick" pick "\nmemory" (pr-str memory)
                       "\nwhole" (pr-str whole) "\nlocal" (pr-str local))
              false)))))
