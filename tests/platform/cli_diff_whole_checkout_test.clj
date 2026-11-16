(ns ^:unit platform.cli-diff-whole-checkout-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is]]
            [sutra.cli.verbs.checks]
            [sutra.parser.authoring.standard :as standard]))

(def ^:private diff-files @#'sutra.cli.verbs.checks/diff-files)
(defn- candidate-schema [files]
  (@#'sutra.cli.verbs.checks/candidate-schema files standard/standard))

(defn- tmp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "diff-whole" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- checkout! [git? transforms]
  (let [d (tmp-dir)]
    (when git? (sh/sh "git" "init" "-q" (.getPath d)))
    (spit (io/file d "controlling.ent")
          (pr-str {:schema {:transforms (into {} (for [t transforms]
                                                   [t
                                                    {:language :pg-sql :query "select 1"
                                                     :output-columns [[:k :string]]}]))}}))
    (io/make-parents (io/file d "doc-templates" "x"))
    (spit (io/file d "doc-templates" "bericht.entt")
          (pr-str {:name :bericht :title "Bericht" :source "{{ x }}" :parameters {}}))
    d))

(deftest one-template-in-a-clone-plans-the-whole-tree
  (let [d      (checkout! true [:t1 :t2])
        entt   (.getPath (io/file d "doc-templates" "bericht.entt"))
        files  (diff-files [entt])
        schema (candidate-schema files)]
    (is (= [(.getPath (io/file d "controlling.ent"))] files))
    (is (= #{:t1 :t2} (set (keys (:transforms schema))))
        "nothing uninvolved reads as left out")
    (is (contains? (:templates schema) :bericht) "and the named file is in, as it stands")))

(deftest leaving-out-in-the-root-ent-still-shows
  (let [d      (checkout! true [:t1])
        schema
          (candidate-schema (diff-files [(.getPath (io/file d "doc-templates" "bericht.entt"))]))]
    (is (= #{:t1} (set (keys (:transforms schema)))))))

(deftest outside-a-clone-the-named-files-alone
  (let [d    (checkout! false [:t1])
        entt (.getPath (io/file d "doc-templates" "bericht.entt"))]
    (is (= [entt] (diff-files [entt])))))

(deftest a-checkout-s-texts-are-one-graph-with-their-owners-as-the-server-reads-them
  (let [d (tmp-dir)
        f #(.getPath (io/file d %))]
    (sh/sh "git" "init" "-q" (.getPath d))
    (spit (io/file d "ontology.ent") (pr-str {:schema {:projects {:werkzeug {}}}}))
    (spit (io/file d "werkzeug.objects.ent")
          (pr-str {:schema {:objects {:maschine {:attributes [[:m-nr :string]]
                                                 :plural "Maschinen"}}}}))
    (spit (io/file d "werkzeug.apps.ent") (pr-str {:schema {:workspaces {:werkstatt {}}}}))
    (let [schema (candidate-schema [(f "ontology.ent") (f "werkzeug.objects.ent")
                                    (f "werkzeug.apps.ent")])]
      (is (= :werkzeug (get-in schema [:objects :maschine :project]))
          "a facet text's resources carry its project, as a pushed tree's do")
      (is (= :werkzeug (get-in schema [:workspaces :werkstatt :project]))))))
