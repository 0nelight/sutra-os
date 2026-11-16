(ns ^:api platform.doc-intel-deploy-test
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.branching.push :as push]
            [sutra.catalog.grants :as grants]
            [sutra.evals.extraction :as extraction]
            [sutra.repos.api :as repos]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [sutra.repos.decorated :as decorated]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [support.pod :as pod]
            [sutra.wire.transport :as tx]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth pod/as-admin)

(def ^:private tricky "Sag \"nein\" \\ zu C:/x\nZeile 2 — 😀 ''' \"\"\"")
(def ^:private source {:kind "dataset" :name "raw_attachments"})

(defn- deployed [strategy]
  (extraction/deployment {:name "beleg-seiten" :source source :strategy strategy}))

(deftest the-template-is-counterparts
  (let [{:keys [path files readme]}
        (deployed {:strategy "vlm" :model "lesemodell" :preprocess "layout" :prompt tricky})
        code (get files path)
        prompts (get files "transforms/beleg_seiten_prompts.py")
        f (doto (java.io.File/createTempFile "prompts" ".py") .deleteOnExit)]
    (is (= "transforms/beleg_seiten.py" path))
    (is (str/includes? code "VisionLLMLayoutDocumentsExtractorInput(\"lesemodell\")"))
    (is (str/includes? code "include_layout_info=\"no_overlay\""))
    (is (str/includes? code "system_prompt=PROMPT"))
    (is (str/includes? code (str "dataset=\"" extraction/output-placeholder "\"")))
    (is (str/includes? readme "output dataset"))
    (spit f prompts)
    (let [r (sh/sh "python3" "-c"
                   (str "exec(open(" (pr-str (str f)) ", encoding='utf-8').read())\n"
                        "print(PROMPT, end='')")
                   :env {"PYTHONIOENCODING" "utf-8"})]
      (is (= tricky (:out r)) (:err r)))))

(deftest a-traditional-strategy-asks-for-no-model
  (let [{:keys [path files]} (deployed {:strategy "layout-ocr" :mode "AUTO"})]
    (is (not (str/includes? (get files path) "Extractor")))
    (is (= [path] (keys files)))))

(deftest an-entity-strategy-reads-a-layout-dataset
  (let [entities {:kommission "string" :netto "double"}
        {:keys [path files]} (extraction/deployment
                              {:name "beleg-felder" :source {:kind "dataset" :name "beleg_seiten"}
                               :strategy {:strategy "entities" :model "lesemodell"
                                          :entities entities :prompt tricky}})
        code (get files path)]
    (is (str/includes? code "EntityExtractorInput(\"lesemodell\")"))
    (is (str/includes? code "Input(kind=\"dataset\", dataset=\"beleg_seiten\")"))
    (is (str/includes? code "documents_of(pages)"))
    (is (str/includes? code "prompt=PROMPT"))
    (is (contains? files "transforms/beleg_felder_prompts.py"))
    (is (extraction/needs-model? "entities"))
    (is (extraction/layout-source? {:datasets {:beleg_seiten {}}}
                                   {:kind "dataset" :name "beleg_seiten"}
                                   {:strategy "entities"}))
    (is (not (extraction/layout-source? {:datasets {}}
                                        {:kind "dataset" :name "beleg_seiten"}
                                        {:strategy "entities"})))))

(deftest the-decorator-declares
  (let [code {"transforms/a_b.py" "@transform(\n    inputs={},\n)\ndef f(): pass\n"
              "transforms/hilfe.py" "def f(): pass\n"}
        formats (registry/texts {:projections standard/projections} :code-repository)
        with #(decorated/with-decorated formats (conj (keys code) "README.md") code
                                        {:repository :r :project :p} %)]
    (testing "a decorated file is a member, a helper module is not"
      (is (= {:a-b {:language :python :code-file "transforms/a_b.py"
                    :repository :r :project :p}}
             (:transforms (with {})))))
    (testing "a file a function binds stays the function's"
      (let [bound {:functions {:eigen {:code-file "transforms/a_b.py"}}}]
        (is (= bound (with bound)))))
    (testing "a file a node binds stays that node's"
      (let [bound {:transforms {:eigen {:code-file "transforms/a_b.py"}}}]
        (is (= bound (with bound)))))))

(defn- code-for [dataset]
  (str "from sutra_transforms import Input, Output, transform\n\n\n"
       "@transform(\n    inputs={\"a\": Input(kind=\"dataset\", dataset=\"dip_roh\")},\n"
       "    output=Output(kind=\"dataset\", dataset=\"" dataset "\", "
       "primary_key=\"page_id\"),\n)\ndef extract(a):\n    return a\n"))

(deftest deployed-then-pushed-by-git
  (let [b    (str "di-" (subs (str (random-uuid)) 0 8))
        _    (assert (ok? (api/mutate :create-project {:name :dip} {:stage? true})))
        _    (assert (ok? (api/mutate :create-repository {:name :dip-code :project :dip}
                                      {:stage? true})))
        _    (assert (ok? (api/mutate :create-dataset
                                      {:name :dip_roh :project :dip
                                       :columns [{:attribute-name :x :attribute-type :string}]}
                                      {:stage? true})))
        _    (assert (ok? (api/post! :staging)))
        _    (assert (ok? (api/post! :branches {:parent "main" :name b})))
        p    (api/post! :proposals {:source b :target "main" :title "deploy"})
        _    (assert (ok? p) (pr-str (:body p)))
        store (ctx/at (ctx/store nil {:user "u-admin" :username grants/admin-username
                                      :operations grants/all :project-operations grants/all
                                      :clearances grants/all})
                      b)
        live  (seat/branch-schema store)
        rr    (repos/repository-rid live "dip-code")
        path  "transforms/dip_seiten.py"
        push! (fn [text] (push/commit-files! store (seat/branch-schema store) rr b
                                             {:files {path text} :message "output"
                                              :author {:name "t" :email "t@t"}}))
        named (fn [section k nm] (some (fn [[_ v]] (when (= nm (some-> (get v k) name)) v))
                                       (get (seat/branch-schema store) section)))
        node  (fn [] (named :transforms :transform-name "dip-seiten"))
        ds?   (fn [nm] (some? (named :datasets :dataset-name nm)))]
    (testing "the template's placeholder is refused: no output dataset, no transform"
      (is (thrown-with-msg? Exception #"specify the output dataset"
                            (push! (code-for extraction/output-placeholder))))
      (is (nil? (node))))
    (testing "the push declares transform and dataset"
      (is (some? (:commit (push! (code-for "dip_seiten")))))
      (println "NODE" (pr-str (dissoc (node) :code)))
      (is (some? (node)))
      (is (ds? "dip_seiten") (pr-str (first (:datasets (seat/branch-schema store))))))
    (testing "on a later push the decorator still decides"
      (is (some? (:commit (push! (code-for "dip_seiten_zwei")))))
      (println "NODE2" (pr-str (dissoc (node) :code)))
      (is (ds? "dip_seiten_zwei")))
    (testing "committed, the repository's text stays terse and the decorator still decides"
      (is (ok? (tx/with-branch b (api/post! :staging))))
      (repos/project-branch! store (ctx/repos) (ctx/blobs) rr b)
      (let [tree (repos/read-tree (ctx/repos) rr (repos/ref-sha (ctx/repos) rr
                                                                (repos/branch->ref b)))
            ent  (some->> (keys tree) (filter #(str/ends-with? % ".ent")) first (get tree)
                          :blob (repos/blob-bytes (ctx/repos) rr) (String.))]
        (println "ENT" ent)
        (is (str/includes? (str ent) "dip_seiten.py"))
        (is (not (str/includes? (str ent) ":inputs")) ent))
      (is (some? (:commit (push! (code-for "dip_seiten_drei")))))
      (is (ds? "dip_seiten_drei")))))
