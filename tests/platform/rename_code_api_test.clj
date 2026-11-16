(ns ^:api platform.rename-code-api-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.checkouts.rename :as rename]
            [sutra.parser.authoring.standard :as standard]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private schema
  {:schema {:projections standard/projections
            :projects    {:buero {}}
            :datasets    {:roh {:kind :standalone :project :buero
                                :columns [{:attribute-name :nr :attribute-type :string}]}}}})

(defn- writes [ds & [tag]]
  (str "from sutra_transforms import Input, Output, transform\n\n\n# " tag "\n"
       "@transform(\n    inputs={\"a\": Input(kind=\"dataset\", dataset=\"roh\")},\n"
       "    output=Output(kind=\"dataset\", dataset=\"" ds "\", primary_key=\"nr\"),\n)\n"
       "def rechne(a):\n    return a\n"))

(defn- push
  ([b code] (push b code {}))
  ([b code body] (binding [tx/*branch* b] (api/post! :slices nil (assoc body :code code)))))

(defn- file-of [b repo node path]
  (binding [tx/*branch* b]
    (get-in (api/get! :slices {:nodes node}) [:body :code repo path :text])))

(defn- setup [b repo]
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name (keyword repo) :project :buero} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :branches {:parent "main" :name b})))
  (let [r (push b {repo {"transforms/rechne.py" {:text (writes "ziel") :blob nil}}})]
    (is (ok? r) (pr-str (:body r)))))

(deftest a-pure-rewrite-touches-only-the-decorators-name
  (let [text "x = 'roh'\n@transform(\n  a=Input(kind=\"dataset\", dataset=\"roh\"),\n  b=Input(dataset='rohling'))\n"]
    (is (= [{:path "a.py"
             :text "x = 'roh'\n@transform(\n  a=Input(kind=\"dataset\", dataset=\"neu\"),\n  b=Input(dataset='rohling'))\n"}]
           (rename/rewrites {"a.py" text "b.py" "print()"} "dataset" :roh :neu)))))

(deftest a-check-follows-the-node-it-names
  (is (= {:node-kind "dataset" :node-key "neu" :query "SELECT 1 FROM {{dataset:neu}}"}
         (rename/check-after {:node-kind "dataset" :node-key "roh"
                              :query "SELECT 1 FROM {{dataset:roh}}"}
                             "dataset" :roh :neu)))
  (is (= "SELECT {{object-type:neu}}"
         (:query (rename/check-after {:node-kind "x" :node-key "y" :query "SELECT {{roh}}"}
                                     "object-type" :roh :neu))))
  (is (nil? (rename/check-after {:node-kind "dataset" :node-key "anders"
                                 :query "SELECT {{dataset:rohling}}"}
                                "dataset" :roh :neu))))

(deftest a-renamed-dataset-takes-the-code-with-it
  (let [b "umbenennen" repo "um-code" path "transforms/rechne.py"]
    (setup b repo)
    (let [rid (binding [tx/*branch* b]
                (some (fn [[r d]] (when (= "roh" (name (:dataset-name d))) (name r)))
                      (get-in (api/get! :schema) [:body :datasets])))
          r   (binding [tx/*branch* b] (api/rename :dataset :roh :roh_neu))]
      (is (ok? r) (pr-str (:body r)))
      (is (= 1 (count (get-in r [:body :code-rewritten]))) (pr-str (:body r)))
      (testing "the file in git says the new name"
        (is (str/includes? (str (file-of b repo "transform/ziel" path)) "dataset=\"roh_neu\"")))
      (testing "the RID did not move"
        (is (= rid (binding [tx/*branch* b]
                     (some (fn [[r d]] (when (= "roh_neu" (name (:dataset-name d))) (name r)))
                           (get-in (api/get! :schema) [:body :datasets]))))))
      (testing "a decorator writing to the former name is told, not obeyed"
        (let [r (binding [tx/*branch* b]
                  (api/rename :dataset :ziel :ziel_neu))
              blob (binding [tx/*branch* b]
                     (get-in (api/get! :slices {:nodes "transform/ziel"})
                             [:body :code repo path :blob]))
              back (push b {repo {path {:text (writes "ziel" "alt") :blob blob}}})]
          (is (ok? r) (pr-str (:body r)))
          (is (not (ok? back)))
          (is (str/includes? (pr-str (:body back)) "former name") (pr-str (:body back))))))))

(deftest a-decorator-may-name-its-output-by-rid
  (let [b "rid-output" repo "rid-code" path "transforms/rechne.py"]
    (setup b repo)
    (let [ds   (binding [tx/*branch* b] (get-in (api/get! :schema) [:body :datasets]))
          rid  (some (fn [[r d]] (when (= "ziel" (name (:dataset-name d))) (name r))) ds)
          blob (binding [tx/*branch* b]
                 (get-in (api/get! :slices {:nodes "transform/ziel"})
                         [:body :code repo path :blob]))
          r    (push b {repo {path {:text (writes rid "per rid") :blob blob}}})
          now  (binding [tx/*branch* b] (:body (api/get! :schema)))]
      (is (ok? r) (pr-str (:body r)))
      (is (= 1 (count (filter #(= "ziel" (name (:transform-name %))) (vals (:transforms now)))))
          "it stays the transform it was")
      (is (= 1 (count (filter #(= "ziel" (name (:dataset-name %))) (vals (:datasets now)))))
          "the dataset it was")
      (is (not-any? #(str/starts-with? (name (:dataset-name %)) "ri.") (vals (:datasets now)))
          "no second dataset named by the RID"))))

(deftest a-push-of-code-alone-says-which-inputs-moved
  (let [b "code-allein" repo "ca-code" path "transforms/rechne.py"]
    (setup b repo)
    (let [pulled (binding [tx/*branch* b] (:body (api/get! :slices {:nodes "transform/ziel"})))
          roh    (binding [tx/*branch* b] (:body (api/get! :slices {:nodes "dataset/roh"})))
          part   (get-in roh [:projections :ontologie])
          [p t]  (first (:files part))
          wider  (update-in (edn/read-string t) [:schema :datasets :roh :columns]
                            conj {:attribute-name :neu :attribute-type :string})
          moved  (push b {} {:projections {:ontologie {:files {(name p) (pr-str wider)}
                                                       :base  (:base part)
                                                       :shape (:shape part)}}})
          code   {repo {path {:text (writes "ziel" "weiter")
                              :blob (get-in pulled [:code repo path :blob])}}}
          r      (push b code {:around (:around pulled)})]
      (is (ok? moved) (pr-str (:body moved)))
      (is (ok? r) (pr-str (:body r)))
      (is (some #(= "dataset/roh" (:node %)) (get-in r [:body :inputs-changed]))
          (pr-str (:body r))))))

(deftest a-renamed-transform-keeps-its-file
  (let [b "transform-name" repo "tn-code" path "transforms/rechne.py"]
    (setup b repo)
    (let [r (binding [tx/*branch* b] (api/rename :transform :ziel :rechnung-ziel))]
      (is (ok? r) (pr-str (:body r)))
      (is (str/includes? (str (file-of b repo "transform/rechnung-ziel" path)) "dataset=\"ziel\"")
          "the code is still found under its old path")
      (let [blob (binding [tx/*branch* b]
                   (get-in (api/get! :slices {:nodes "transform/rechnung-ziel"})
                           [:body :code repo path :blob]))
            again (push b {repo {path {:text (writes "ziel" "weiter") :blob blob}}})
            now   (binding [tx/*branch* b] (:body (api/get! :schema)))]
        (is (ok? again) (pr-str (:body again)))
        (is (= #{"rechnung-ziel"} (set (map (comp name :transform-name)
                                            (vals (:transforms now)))))
            "one transform, under its new name")))))
