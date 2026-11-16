(ns ^:unit platform.path-encoding-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [reitit.core :as r]
            [reitit.ring :as ring]
            [sutra.types.paths :as paths]))

(defn- round-trip
  [id params]
  (let [tmpl (paths/url id)
        rt   (ring/router [[tmpl {:get identity}]])]
    (some-> (r/match-by-path rt (paths/url id params)) :path-params)))

(deftest a-scenario-branch-name-survives-the-round-trip
  (let [nm "scenario/ri.sutra.main.application.5d1e/9f2c-4a.YmFzZTY0"]
    (testing "the name occupies exactly ONE segment — the whole of the bug"
      (is (str/includes? (paths/url :branch-roles {:name nm}) "%2F"))
      (is (= 5 (count (str/split (paths/url :branch-roles {:name nm}) #"/")))))
    (testing "and the router hands the handler back the ORIGINAL name"
      (is (= {:name nm} (round-trip :branch-roles {:name nm}))))))

(deftest characters-that-would-change-meaning-are-encoded
  (testing "space becomes %20, never `+` — `+` is a literal plus in a path"
    (let [u (paths/url :object-type-by {:name "my type"})]
      (is (str/includes? u "%20"))
      (is (not (str/includes? u "+")))
      (is (= {:name "my type"} (round-trip :object-type-by {:name "my type"})))))
  (testing "a literal % round-trips rather than being read as an escape"
    (is (= {:name "100%done"} (round-trip :object-type-by {:name "100%done"}))))
  (testing "? and # would otherwise start the query/fragment"
    (doseq [nm ["a?b" "a#b" "a&b"]]
      (is (= {:name nm} (round-trip :object-type-by {:name nm}))
          (str "round-trip failed for " (pr-str nm))))))
