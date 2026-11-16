(ns ^:unit platform.template-coverage-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest is testing]]
            [sutra.repos.projection :as gp]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [sutra.parser.ent :as ent]
            [sutra.branching.push :as git-http]
            [sutra.authoring.install :as install]
            [sutra.parser.lint :as lint]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.project-export :as pe]
            [sutra.types.lifecycle :as lifecycle]
            [sutra.parser.ent-file :as ent-file]))

(def ^:private extensions ["ent" "enta" "entv" "entp" "entt"])

(defn- preset [family ext] (str "resources/presets/" family "." ext))

(deftest template-is-structurally-valid
  (doseq [family ["grammar" "template"]
          :let [p (preset family "ent")]]
    (testing (str p " passes the real read-ent door (validate + lower + resolve)")
      (is (map? (ent-file/read-ent p))))))

(def ^:private kind-of
  {"entv" :view "enta" :app "entp" :pipeline "entt" :doc-template})

(deftest die-schlanken-vorlagen-bestehen-ihre-eigene-grammatik
  (doseq [family ["grammar" "template"]
          ext    (rest extensions)
          :let   [p (preset family ext)]]
    (testing p
      (is (map? (ent/read-text (kind-of ext) p (slurp p)))
          (str p " is copied by authors — a form it shows that the door refuses is"
               " a form every author will copy")))))

(deftest die-vorlage-ist-sauber
  (let [s (:schema (ent-file/read-ent (preset "template" "ent")))]
    (testing "the lifecycle it teaches names real states, Actions and one object param each"
      (is (= [] (lifecycle/problems (:objects s) (:actions s)
                             (fn [t] (map #(if (vector? %) (first %) %)
                                          (get-in s [:objects t :attributes])))))))
    (testing "and lint finds nothing to warn about — the scaffold is what gets copied"
      (is (= [] (->> (lint/run-lint s (spec/vocabulary))
                     (filter (comp #{:error :warn} :severity))
                     (mapv (juxt :rule :message))))))
    (testing "nor do the OOUX rules, judged as a push judges it: every type is new"
      (is (= [] (git-http/ooux-advisories s {} (spec/vocabulary)))))
    (testing "and the WHOLE lint over the planned catalog — what `changeset diff` says"
      (is (= [] (->> (lint/run-lint (:schema (install/planned-ontology s {} {})) (spec/vocabulary))
                     (filter (comp #{:error :warn} :severity))
                     (remove (comp #{:backing-without-producer} :rule))
                     (mapv (juxt :rule :entity :message))))))))

(deftest die-app-vorlagen-sind-sauber
  (doseq [family ["template" "template-ablauf"]
          :let
            [types
               (:schema
                (install/planned-ontology (:schema (ent-file/read-ent (preset "template" "ent"))) {}
                                          {}))
             app   (:schema (ent/read-text :app (preset family "enta")
                                                  (slurp (preset family "enta"))))
             s     (assoc types :applications (:applications app))]]
    (testing family
      (is (= [] (->> (lint/run-lint s (spec/vocabulary))
                     (filter (comp #{:error :warn} :severity))
                     (remove (comp #{:backing-without-producer} :rule))
                     (mapv (juxt :rule :message))))
          "the app every author copies names a project and no variable after a type"))))

(deftest die-vorlagen-haben-ascii-api-namen
  (testing "a keyword is an API name, and API names stay ASCII — display names may not"
    (doseq [ext extensions
            :let [p   (preset "template" ext)
                  bad (atom #{})]]
      (walk/postwalk (fn [x] (when (and (keyword? x) (re-find #"[^\x00-\x7F]" (str x)))
                               (swap! bad conj x))
                       x)
                     (read-string (slurp p)))
      (is (= #{} @bad) p))))

(deftest beide-familien-landen-inert-im-ontologie-repo
  (doseq [family ["template" "grammar"]
          ext    extensions
          :let   [path (str "templates/" family "." ext)]]
    (testing path
      (is (contains? gp/platform-file-paths path) "the instance serves it beside the checkout")
      (is (= :file (pe/plane-of (registry/texts standard/standard :ontologie) path))
          "file-plane: never read as schema, a `.entv` there never publishes a view")
      (is (some? (io/resource (str "presets/" family "." ext)))))))
