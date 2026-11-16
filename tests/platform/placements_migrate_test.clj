(ns ^:integration platform.placements-migrate-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.authoring.placements :as placements]
            [sutra.parser.checkout :as co]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private old
  {:files      {"a.ent" :central}
   :placement  {[:object-types :T] "a.ent"}
   :fields     {[[:object-types :T] :description] "b.ent"}
   :attributes {}})

(deftest a-row-of-before-is-converted-once
  (let [store (ctx/store "main")
        p     (keyword (str "legacy-" (random-uuid)))
        w     {:branch ctl/trunk :projection p}]
    (ctl/insert-many! store :projection-placements [(assoc w :memory old)])
    (is (= {:files (:files old)} (placements/memory store p)) "a read has no fallback")
    (is (= 2 (get (placements/migrate! store) p)) "the migration moves both elements")
    (is (= #{:files} (set (keys (:memory (ctl/get store :projection-placements w))))))
    (is (= (dissoc old :attributes) (placements/memory store p)))
    (let [src (placements/source store p)]
      (is (= (select-keys old [:placement :fields])
             (select-keys (co/memory-for src [[:object-types :T]]) [:placement :fields]))
          "a push reads it by node")
      (is (= #{[:fields [[:object-types :T] :description]]} (co/in-text src "b.ent"))
          "and by text"))
    (is (nil? (get (placements/migrate! store) p)) "a second run finds nothing")))
