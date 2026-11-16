(ns ^:unit platform.cli-tree-test
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.parse :as parse]
            [sutra.cli.table :as table]
            [sutra.cli.tree :as tree]
            [sutra.parser.entr :as entr]
            [sutra.parser.step-verbs :as sv]
            [sutra.types.endpoint-verbs :as ev]))

(deftest match-is-longest-prefix
  (is (= {:leaf (tree/leaf-index ["ontology" "object-type" "show"]) :remaining ["email"]}
         (tree/match ["ontology" "object-type" "show" "email"])))
  (testing "a flat verb that is not a tree path does not match (→ flat fallback)"
    (is (nil? (tree/match ["describe" "email"])))
    (is (nil? (tree/match ["source" "pdfsrc"]))))
  (testing "a bare namespace token is not a leaf"
    (is (nil? (tree/match ["ontology"])))))

(defn- grammar-keys
  [verb]
  (some (fn [[v _ cat]]
          (when (= v verb)
            (let [args (second (m/children cat))]
              (when (and args (= :map (m/type args)))
                (set (map first (m/children args)))))))
        (m/children (m/schema entr/Step))))

(defn- cli-keys [verb]
  (let [spec (get parse/arg-specs verb)]
    (into #{} (concat (map :key (:pos spec)) (map :key (vals (:flags spec)))))))

(deftest a-guard-flag-the-cli-offers-is-a-flag-the-grammar-admits
  (testing "reindex --force — the valve that was wired everywhere but here"
    (is (contains? (grammar-keys :reindex) :force?)
        "without it `reindex <type> --force` cannot be expressed at all, and the
         409 it exists to override becomes unappealable")
    (is (empty? (set/difference (cli-keys :reindex) (grammar-keys :reindex)))
        "and the whole verb passes its args through verbatim, so the two sets agree"))
  (testing "the same holds for the other pass-through verbs with a boolean guard"
    (doseq [verb [:prune-orphans :drop-edits]
            :let [allowed (grammar-keys verb)]
            :when allowed]
      (is (empty? (set/difference (cli-keys verb) allowed))
          (str verb " offers a flag its closed args map refuses")))))

(deftest every-runbook-step-has-a-step-method
  (dispatch/load-verbs!)
  (let [implemented (into (disj (set (keys (methods dispatch/step!))) :default)
                          (keys ev/cli-verbs))]
    (is (= sv/verbs (set/difference implemented table/client-verbs))
        "the CLI runs exactly the steps the runbook grammar knows")
    (is (empty? (set/intersection sv/verbs table/client-verbs))
        "a verb on local files is no runbook step")))

(deftest a-verb-lives-in-the-namespace-the-table-names
  (doseq [{:keys [verb home]} table/verbs
          :when home
          :let [f (io/resource (str "sutra/cli/verbs/" (str/replace (name home) "-" "_") ".clj"))]]
    (is (some? f) (str verb " names a home that does not exist: " home))
    (when f
      (is (re-find (re-pattern (str "\\(defmethod\\s+step!\\s+" verb "[\\s\\]]")) (slurp f))
          (str verb " has no step! method in " home)))))

(deftest the-tree-covers-the-verbs-a-line-knows
  (let [live  (into #{} (comp (filter #(= :live (tree/status %))) (keep :verb))
                    tree/command-tree)
        meta* (into #{} (comp (filter #(= :meta (tree/status %))) (keep :verb))
                    tree/command-tree)
        files (into #{} (comp (filter #(= :file-only (tree/status %))) (keep :verb))
                    tree/command-tree)
        client (into #{} (comp (filter #(= :client (tree/status %))) (keep :verb))
                     tree/command-tree)]
    (is (= (set (keys parse/arg-specs)) (into live (filter table/client-verbs) client))
        "every line verb has a live or a client leaf, and every live leaf a line form")
    (is (= (set (vals parse/meta-verbs)) meta*))
    (is (empty? (set/difference files sv/verbs)) "a runbook-only leaf names a runbook step")
    (is (empty? (->> tree/command-tree (map :path) frequencies (filter #(> (val %) 1))))
        "no path is given twice")
    (is (every? (set (map :key tree/sections)) (map :section tree/command-tree)))))
