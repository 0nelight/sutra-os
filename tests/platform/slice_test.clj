(ns ^:unit platform.slice-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [sutra.checkouts.slice :as slice]
            [sutra.cli.projection]
            [sutra.cli.slice :as cli]
            [sutra.cli.verbs.install :as install-verb]))

(def ^:private text
  (pr-str {:schema {:objects {:a {:plural "As"} :b {:plural "Bs"}}
                    :links [[:a-b :a :b :many-to-one] [:b-a :b :a :many-to-one]]
                    :actions {:mach {:rules []}}}}))

(defn- schema-of [t] (:schema (edn/read-string t)))

(deftest a-cut-holds-only-the-slice
  (is (= {:objects {:a {:plural "As"}} :links [[:a-b :a :b :many-to-one]]}
         (schema-of (slice/cut-text text #{[:objects "a"] [:links "a-b"]}))))
  (testing "a text that is one node's own document stays whole"
    (is (= "{:name :x}" (slice/cut-text "{:name :x}" (constantly false))))))

(deftest a-splice-touches-only-what-the-slice-held
  (let [held [[:objects "a"] [:objects "weg"] [:links "a-b"]]
        mine (pr-str {:schema {:objects {:a {:plural "Neu"} :neu {:plural "N"}}
                               :links [[:a-b :a :b :one-to-one]]}})
        out  (schema-of (slice/splice-text text mine held))]
    (is (= {:a {:plural "Neu"} :b {:plural "Bs"} :neu {:plural "N"}} (:objects out)))
    (is (= #{[:a-b :a :b :one-to-one] [:b-a :b :a :many-to-one]} (set (:links out))))
    (is (= {:mach {:rules []}} (:actions out)) "a section the slice never named stands")
    (testing "a node the slice held and no longer names is gone"
      (is (= {:b {:plural "Bs"}}
             (:objects (schema-of (slice/splice-text text
                                                     (pr-str {:schema {}})
                                                     [[:objects "a"]]))))))
    (testing "a lost file drops the slice's nodes, never the text"
      (let [out (schema-of (slice/splice-text text nil [[:objects "a"]]))]
        (is (= {:b {:plural "Bs"}} (:objects out)))
        (is (contains? (:actions out) :mach))))
    (testing "a document stays what was pushed"
      (is (= "{:name :x}" (slice/splice-text "{:name :y}" "{:name :x}" [[:applications "x"]]))))))

(deftest full-texts-leave-a-new-text-as-sent
  (is (= {"neu.ent" "{}"}
         (slice/full-texts {} {"neu.ent" "{}"} {}))))

(deftest a-part-holds-its-nodes-and-only-those-texts
  (let [view {:shape "s"
              :files {"x.ent" text "y.ent" (pr-str {:schema {:objects {:c {}}}})}
              :base  {"x.ent" {[:objects "a"] {:plural "1"} [:objects "b"] {:plural "2"}}
                      "y.ent" {[:objects "c"] {:plural "3"}}}}
        p    (slice/part-of view #{[:objects "a"]})]
    (is (= {"x.ent" {[:objects "a"] {:plural "1"}}} (:base p)))
    (is (= ["x.ent"] (keys (:files p))))
    (is (nil? (slice/part-of view #{[:objects "z"]})) "nothing held, no part")))

(deftest code-is-found-by-name-by-decorator-and-by-text
  (let [schema {:transforms {"ri.t.1" {:transform-name :nach-name :language :python
                                       :repository "ri.r"}
                             "ri.t.2" {:transform-name :anders :language :python
                                       :repository "ri.r" :code-path "transforms/kern.py"}
                             "ri.t.3" {:transform-name :sql :language :pg-sql :repository "ri.r"}}
                :functions  {"ri.f.1" {:function-name :fn1 :repository "ri.r"
                                       :code-file "transforms/fn1.clj"}}}]
    (is (= [["ri.r" "transforms/nach_name.py"]]
           (slice/code-files schema #{"transform/nach-name"})))
    (is (= [["ri.r" "transforms/kern.py"]] (slice/code-files schema #{"transform/anders"})))
    (is (= [] (slice/code-files schema #{"transform/sql"})))
    (is (= [["ri.r" "transforms/fn1.clj"]] (slice/code-files schema #{"function/fn1"})))))

(deftest a-transforms-sibling-modules-come-along
  (let [tree {"transforms/kern.py" {} "transforms/hilfe.py" {} "transforms/x.py" {}}]
    (is (= ["transforms/kern.py" "transforms/hilfe.py"]
           (slice/imports-in (str "import os\nfrom kern import a\n"
                                  "from transforms.hilfe import b\nfrom pandas import c\n")
                             tree)))))

(deftest code-that-moved-conflicts-until-someone-decides
  (let [schema  {:repositories {"a" {:repository-name :a}}}
        tree    (constantly {"x.py" {:blob "neu"}})
        text-of (constantly "ihr text")
        code    {"a" {"x.py" {:text "t" :blob "alt"} "y.py" {:text "t" :blob nil}}}
        by-path #(into {} (map (juxt :path :conflict?)) %)
        changes #(by-path (slice/code-changes schema tree text-of code %))]
    (is (= {"x.py" true "y.py" false} (changes nil)))
    (is (= {"x.py" false "y.py" false} (changes :mine)))
    (is (= {"y.py" false} (changes :theirs))
        "theirs keeps the file as it stands: nothing to commit")
    (testing "a tip that already holds what is sent is the push's own earlier landing"
      (is (= {"y.py" false}
             (by-path (slice/code-changes schema tree (constantly "t") code nil)))))))

(deftest a-push-leaves-out-what-the-repositorys-gitignore-names
  (let [ignored? (slice/ignored-by "__pycache__/\n*.pyc\n/build\n")]
    (is (ignored? "transforms/__pycache__/x.cpython-312.pyc"))
    (is (ignored? "a.pyc"))
    (is (ignored? "build/out.txt"))
    (is (not (ignored? "transforms/build/y.py")) "an anchored rule holds at the root only")
    (is (not (ignored? "transforms/x.py")))
    (is (not ((slice/ignored-by nil) "transforms/__pycache__/x.pyc"))))
  (let [schema {:repositories {"a" {:repository-name :a}}}
        tree   (constantly {".gitignore" {:blob "g"}})
        code   {"a" {"x.py" {:text "t" :blob nil}
                     "transforms/__pycache__/x.cpython-312.pyc" {:text "\u0000" :blob nil}}}]
    (is (= ["x.py"]
           (map :path (slice/code-changes schema tree (constantly "__pycache__/\n") code nil))))))

(deftest a-commit-reports-its-own-problems-and-counts-the-rest
  (let [own {:rule :a :entity "mein"}
        old {:rule :a :entity "alt" :inherited? true}]
    (is (= {:problems [own] :inherited-from-trunk 1}
           (install-verb/commit-problems [own old] {})))
    (is (= {:problems [] :inherited-from-trunk 1}
           (install-verb/commit-problems [old] {:x 1})))
    (is (= {:problems [{:x 1}]} (install-verb/commit-problems [] [{:x 1}])))))

(deftest a-slice-names-what-a-push-would-not-send
  (let [c {:projections {"ontologie" {:base {} :hashes {}}}
           :code {"r" {"transforms/a.py" {:blob "b"}}}}]
    (is (= ["code-repository/telefon.ent" "notiz.txt"]
           (cli/unheld-files c {"ontologie/t.ent" "{}"
                                "repos/r/transforms/a.py" "x"
                                "repos/neu/transforms/b.py" "y"
                                "code-repository/telefon.ent" "{}"
                                "notiz.txt" "z"})))))

(deftest the-cli-sends-only-what-differs
  (let [same    "{:a 1}"
        c       {:projections {"ontologie" {:base {"t.ent" {} "u.ent" {}} :shape "s"
                                            :hashes {"t.ent" (#'cli/->name "x")
                                                     "u.ent" (sutra.cli.projection/sha same)}}}
                 :code {"r" {"transforms/a.py" {:blob "b1"
                                                :hash (sutra.cli.projection/sha same)}
                             "transforms/weg.py" {:blob "b2"
                                                  :hash (sutra.cli.projection/sha same)}}}
                 :around {:branch []}}
        disk    {"ontologie/t.ent" "{:neu 1}" "ontologie/u.ent" same
                 "repos/r/transforms/a.py" same "repos/r/transforms/neu.py" "print()"}
        request (cli/request c disk {:message "m"})]
    (is (= {"t.ent" "{:neu 1}"} (get-in request [:projections "ontologie" :files])))
    (is (= {"transforms/neu.py" {:text "print()" :blob nil}
            "transforms/weg.py" {:text nil :blob "b2"}}
           (get-in request [:code "r"]))
        "a new file comes without a blob, a lost one without a text")
    (is (= "m" (:message request)))))
