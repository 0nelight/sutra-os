(ns ^:unit grammar.decorator-declares-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.authoring.placements :as placements]
            [sutra.authoring.projections :as projections]
            [sutra.catalog.kernel :as kernel]
            [sutra.parser.lint :as lint]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]
            [sutra.parser.projection :as proj]
            [sutra.repos.checks :as repo-checks]
            [sutra.types.expectations :as expectations]
            [sutra.store.control :as ctl]
            [sutra.store.control.collections :as collections]
            [sutra.store.mem :as mem]
            [sutra.types.backing :as backing]
            [sutra.types.world-rules :as rules]))

(defn- introspecting
  [declared]
  {:introspect-python (fn [_] (assoc declared :sutra.code/entry :transform))})

(def ^:private body
  {:name :t :language :python :code "@transform(...)" :sutra.code/section :transforms})

(deftest the-decorator-beats-the-text
  (let [code (introspecting {:inputs {:a {:kind :dataset :dataset :neu}}})
        [b]  (kernel/introspect-code-transform
              code
              (assoc body :inputs {:a {:kind :dataset :dataset :alt}}
                     :incremental true :semantic-version 7 :description "bleibt"))]
    (testing "a field it states replaces the text's"
      (is (= :neu (get-in b [:inputs :a :dataset]))))
    (testing "a field it leaves out is gone, the text does not keep it alive"
      (is (not (contains? b :incremental)))
      (is (not (contains? b :semantic-version))))
    (testing "what no decorator can state stays the text's"
      (is (= "bleibt" (:description b))))
    (is (= :code (:spec-source b)))))

(deftest a-function-keeps-its-own-statements
  (let [code {:introspect-python (fn [_] {:sutra.code/entry :function
                                          :inputs {:a {:kind :dataset :dataset :neu}}})}
        [b]  (kernel/introspect-code-transform
              code
              (assoc body :sutra.code/section :functions
                     :inputs {:a {:kind :dataset :dataset :alt}}))]
    (is (= :alt (get-in b [:inputs :a :dataset])))
    (is (nil? (:spec-source b)))))

(deftest a-declared-transform-prints-nothing-the-decorator-says
  (let [[_ e] (:entry (pemit/emit-transform
                       {:transform-name :t :language :python :code "x" :spec-source :code
                        :inputs {:a {:kind :dataset :dataset :d}}
                        :incremental true :semantic-version 3 :max-per-run 5
                        :external-systems [:m] :description "bleibt"}
                       {:inline? false}))]
    (is (= #{:language :code-file :description} (set (keys e))))))

(def ^:private code-formats
  (registry/texts {:projections standard/projections} :code-repository))

(deftest a-push-declares-from-the-world-and-its-code
  (let [rendered {"etl.ent" (.getBytes (pr-str {:schema {:transforms
                                                         {:alt {:language :python
                                                                :code-file "transforms/alt.py"}
                                                          :weg
                                                          {:language :python
                                                           :code-file "transforms/weg.py"}}}}))}
        tree     {"etl.ent" (pr-str {:schema {:transforms {:von-hand {:language :python}}}})
                  "transforms/alt.py" "def f(): pass\n"
                  "transforms/neu_ds.py" "@transform(\n)\ndef f(): pass\n"}
        s (:schema (repo-checks/declaration code-formats rendered (keys tree) tree
                                            {:repository :etl :project :p}))]
    (is (= #{:alt :neu-ds} (set (keys (:transforms s))))
        "the tree's text declares nothing, a deleted file retires, a decorated one is new")
    (is (= "transforms/neu_ds.py" (get-in s [:transforms :neu-ds :code-file])))))

(defn- decorated [dataset f]
  (str "@transform(\n    inputs={},\n    output=Output(kind=\"dataset\", dataset=\"" dataset
       "\"),\n)\ndef " f "(): pass\n"))

(defn- declared [rendered tree owner]
  (:schema (repo-checks/declaration code-formats rendered (keys tree) tree owner)))

(deftest the-output-identifies-a-transform
  (let [text  {"etl.ent" (.getBytes (pr-str {:schema {:transforms
                                                      {:alt {:language :python
                                                             :description "bleibt"
                                                             :code-file "transforms/alt.py"}}}}))}
        owner {:repository :etl :project :p :outputs {[:dataset "ziel"] :alt}}]
    (testing "a moved file is the same transform, not a retired and a new one"
      (let [s (declared text {"transforms/umbenannt.py" (decorated "ziel" "f")} owner)]
        (is (= #{:alt} (set (keys (:transforms s)))))
        (is (= "transforms/umbenannt.py" (get-in s [:transforms :alt :code-file])))
        (is (= "transforms/umbenannt.py" (get-in s [:transforms :alt :code-path])))
        (is (= "bleibt" (get-in s [:transforms :alt :description])))))
    (testing "a new output is a new transform, named after it"
      (let [s (declared {} {"transforms/x.py" (decorated "neu_ds" "f")} owner)]
        (is (= #{:neu-ds} (set (keys (:transforms s)))))
        (is (nil? (get-in s [:transforms :neu-ds :entry-point])))
        (is (= "transforms/x.py" (get-in s [:transforms :neu-ds :code-path])))))
    (testing "the path the name gives is not stated"
      (let [s (declared {} {"transforms/neu_ds.py" (decorated "neu_ds" "f")} owner)]
        (is (nil? (get-in s [:transforms :neu-ds :code-path])))))
    (testing "an output moved to a new file does not take the transform along"
      (let [s (declared text
                        {"transforms/alt.py"  (decorated "rest" "f")
                         "transforms/neu.py"  (decorated "ziel" "g")}
                        owner)]
        (is (= #{:alt :ziel} (set (keys (:transforms s)))))
        (is (= "transforms/alt.py" (get-in s [:transforms :alt :code-file])))
        (is (= "transforms/neu.py" (get-in s [:transforms :ziel :code-file])))))
    (testing "two decorators in a file are two transforms, each with its entry point"
      (let [s (declared text
                        {"transforms/alt.py" (str (decorated "ziel" "eins") "\n"
                                                  (decorated "zweit" "zwei"))}
                        owner)]
        (is (= {:alt "eins" :zweit "zwei"}
               (update-vals (:transforms s) :entry-point)))
        (is (= "bleibt" (get-in s [:transforms :alt :description])))))
    (testing "a member back alone in its file loses the entry point"
      (let [s (declared {"etl.ent" (.getBytes (pr-str {:schema {:transforms
                                                                {:alt {:language :python
                                                                       :entry-point "eins"
                                                                       :code-file
                                                                       "transforms/alt.py"}}}}))}
                        {"transforms/alt.py" (decorated "ziel" "eins")}
                        owner)]
        (is (nil? (get-in s [:transforms :alt :entry-point])))))
    (testing "two transforms may not write one output"
      (is (thrown-with-msg? Exception #"same output: ziel"
                            (declared {}
                                      {"transforms/a.py" (decorated "ziel" "f")
                                       "transforms/b.py" (decorated "ziel" "g")}
                                      owner))))))

(defn- writes-type [t f]
  (str "@transform(\n    inputs={},\n    output=Output(kind=\"object-type\", object_type=\"" t
       "\"),\n)\ndef " f "(): pass\n"))

(deftest a-read-back-names-only-what-the-push-changed-and-did-not-take
  (let [lost    #'projections/not-stored
        n       [:objects "n"]
        before  {"t.ent" {n {:plural "1" :title "1"}}}]
    (testing "a node the push did not change is not its loss, even if it no longer reads"
      (is (= [] (lost before before {"t.ent" {}}))))
    (testing "an element it changed that still reads as before was dropped"
      (is (= [{:text "t.ent" :node n :elements [:plural]}]
             (lost {"t.ent" {n {:plural "2" :title "1"}}} before before))))
    (testing "an element that moved and prints in its own form was stored"
      (is (= [] (lost {"t.ent" {n {:plural "2" :title "1"}}}
                      before
                      {"t.ent" {n {:plural "3" :title "1"}}}))))))

(deftest a-check-sits-on-an-object-output-whose-run-emits-the-whole
  (let [check  {:name "x" :expectation [:col :a :non-null] :on-error :warn}
        t      (fn [incremental]
                 {:incremental incremental
                  :output {:kind :object-type :object-type :o :name :o :checks [check]}})
        refused? #(some (fn [p] (str/includes? p "incremental object/link"))
                        (expectations/transform-problems %))]
    (testing "a replace write emits the whole set"
      (is (not (refused? (t {:read-mode :current :write-mode :replace})))))
    (testing "a run that only adds emits a part"
      (is (refused? (t {:read-mode :added :write-mode :modify})))
      (is (refused? (t true))))
    (testing "without :incremental the emitted rows are the whole anyway"
      (is (not (refused? (t nil)))))))

(deftest a-read-back-reads-every-text-holding-part-of-its-nodes
  (let [holding #'projections/texts-holding
        n       [:objects "n"]
        memory  {:placement  {n "ontology.ent" [:objects "m"] "weg.ent"}
                 :attributes {[n "id"] "p.objects.ent"}
                 :fields     {[n :title] "titel.ent"}}]
    (is (= #{"ontology.ent" "p.objects.ent" "titel.ent"} (holding memory #{n})))
    (is (= #{} (holding memory #{[:objects "unbekannt"]})))))

(deftest an-app-naming-an-undeclared-function-is-a-finding
  (let [rule (some #(when (= :app-function-unknown (:id %)) %) lint/rules)
        app  (fn [f] {:custom-applications
                      {"ri.a" {:custom-application-name :cid :layout {:functions [f]}}}
                      :functions {"ri.f" {:function-name :da}}})]
    (is (empty? ((:scan rule) (app :da))))
    (is (= ["cid.weg"] (map :entity ((:scan rule) (app :weg)))))))

(deftest a-new-node-naming-a-missing-code-file-says-so
  (let [refuse #'projections/refuse-unread-code!]
    (is (thrown-with-msg? Exception #"no such code file on b .*functions f: transforms/f.clj"
                          (refuse {} "b" {:functions {:f {:code-file "transforms/f.clj"}}})))
    (is (nil? (refuse {} "b" {:functions {:f {:code-file "transforms/f.clj" :code "x"}}})))))

(deftest several-writers-of-a-type-stay-apart
  (let [text  {"etl.ent" (.getBytes (pr-str {:schema {:transforms
                                                      {:shape-t {:language :python
                                                                 :code-file "transforms/shape_t.py"}
                                                       :supp-t {:language :python
                                                                :code-file "transforms/supp_t.py"}}}}))}
        tree  {"transforms/shape_t.py" (writes-type "t" "f")
               "transforms/supp_t.py"  (writes-type "t" "g")}
        s     (declared text tree {:repository :etl :project :p})]
    (testing "a primary and a supplement of one type are no shared output"
      (is (= #{:shape-t :supp-t} (set (keys (:transforms s))))))
    (testing "each file stays with its own node"
      (is (= "transforms/shape_t.py" (get-in s [:transforms :shape-t :code-file])))
      (is (= "transforms/supp_t.py" (get-in s [:transforms :supp-t :code-file]))))))

(deftest a-materialization-stands-in-the-text
  (let [schema {:datasets   {"ri.d" {:dataset-name :m_mat :kind :materialization :object-type :m
                                     :columns [{:attribute-name :a :attribute-type :string}]}}
                :transforms {"ri.t" (backing/materialization-transform :m_mat :m)}}
        body   (:schema (:ent (pemit/export schema {:all-projects? true})))]
    (is (= {:kind :materialization :object-type :m} (get-in body [:datasets :m_mat]))
        "as it is declared: the columns follow the type")
    (is (not (contains? (:transforms body) :m_mat-materialize))
        "its lifted producer is no node of the text")))

(deftest a-push-may-not-change-a-text
  (let [t (fn [m] (update-vals m (fn [b] {:blob b})))]
    (testing "a changed text is named, a changed code file is not"
      (is (= ["etl.ent"]
             (repo-checks/texts-changed code-formats
                                        (t {"etl.ent" "neu" "transforms/a.py" "neu"})
                                        (t {"etl.ent" "alt" "transforms/a.py" "alt"})
                                        (t {"etl.ent" "alt"})))))
    (testing "a removed text is a change"
      (is (= ["etl.ent"]
             (repo-checks/texts-changed code-formats {}
                                        (t {"etl.ent" "alt"}) (t {"etl.ent" "alt"})))))
    (testing "a text as the base holds it comes from a merge of the trunk"
      (is (= []
             (repo-checks/texts-changed code-formats (t {"etl.ent" "main"}) (t {"etl.ent" "alt"})
                                        (t {"etl.ent" "main"})))))))

(deftest a-decorators-output-declares-its-dataset
  (let [schema {:transforms {:t {:language :python :code "x" :project :p}
                             :s {:language :pg-sql}}
                :datasets   {:da {}}}
        with   (fn [declared known]
                 (install/with-decorated-datasets schema (constantly declared) known))]
    (is (= {:neu {:project :p} :da {}}
           (:datasets (with {:output {:kind :dataset :dataset :neu}} (constantly false)))))
    (is (= {:da {}}
           (:datasets (with {:outputs [{:kind :dataset :dataset :da}
                                       {:kind :object-type :object-type :o}
                                       {:kind :dataset :dataset :live}]}
                            #{:live}))))
    (testing "the template's placeholder is no dataset"
      (is (thrown-with-msg? Exception #"specify the output dataset"
                            (with {:output {:kind :dataset :dataset :REPLACE_ME}}
                                  (constantly false)))))))

(deftest a-push-may-not-state-what-the-decorator-declares
  (let [schema {:transforms {"ri.t.1" {:transform-name :d :spec-source :code}
                             "ri.t.2" {:transform-name :alt}}}]
    (is (= ["d :inputs"]
           (projections/decorator-statements
            schema {:transforms {:d {:inputs {} :description "x"} :alt {:inputs {}}}})))))

(deftest what-a-seed-stores-reads-back
  (let [formats (registry/texts {:projections standard/projections} :ontologie)
        text    (pr-str {:schema {:objects {:auftrag {:primary-key :nr}}}})
        memory  (proj/memory-of (pe/placement-of formats {"ontology.ent" (delay text)}))]
    (is (= "ontology.ent" (get-in memory [:placement [:objects "auftrag"]])))
    (is (= memory (edn/read-string (pr-str memory)))
        "the kinds' functions stay out: the memory is stored as EDN")))

(deftest a-seed-overwrites-a-row-that-does-not-read
  (let [store {:db     (mem/store mem/empty-seed {:collections collections/all
                                                  :signature rules/signature})
               :clock  #(java.util.Date.)
               :branch "main"}
        found {:placement {[:objects "a"] "ontology.ent"}}
        kept  {:placement {[:objects "b"] "b.objects.ent"}}]
    (placements/remember! store :ontologie kept)
    (testing "a row that reads keeps what it names"
      (is (= {[:objects "a"] "ontology.ent" [:objects "b"] "b.objects.ent"}
             (:placement (placements/seed! store :ontologie found)))))
    (testing "a row that does not read is replaced"
      (let [real ctl/get]
        (with-redefs [ctl/get (fn [& _] (throw (ex-info "No reader function" {})))]
          (placements/seed! store :ontologie found))
        (is (= {} (:memory (real store :projection-placements
                                 {:branch "main" :projection :ontologie})))
            "the row keeps the kinds of the texts, the elements are rows of their own")
        (is (= {:placement {[:objects "a"] "ontology.ent" [:objects "b"] "b.objects.ent"}}
               (placements/memory store :ontologie))
            "the element rows read, so what they name stands")))))

(deftest the-role-of-an-output-stays-the-ontologys
  (let [code (introspecting {:output {:kind :object-type :object-type :o}})
        out  #(:output (first (kernel/introspect-code-transform code (assoc body :output %))))]
    (testing "as a declared transform's text shows it, and as a text states it in full"
      (is (= :supplement (:role (out {:role :supplement}))))
      (is (= :supplement (:role (out {:kind :object-type :object-type :o :role :supplement})))))
    (testing "it belonged to another target: the decorator moved the output"
      (is (nil? (:role (out {:kind :object-type :object-type :alt :role :supplement}))))))
  (testing "a declared transform's text shows it and only it"
    (let [[_ e] (:entry (pemit/emit-transform
                         {:transform-name :t :language :python :code "x" :spec-source :code
                          :output {:kind :object-type :object-type :o :role :supplement}}
                         {:inline? false}))]
      (is (= {:role :supplement} (:output e)))))
  (testing "a push may state it, nothing else of the output"
    (let [schema {:transforms {"ri.t.1" {:transform-name :d :spec-source :code}}}]
      (is (= [] (projections/decorator-statements
                 schema {:transforms {:d {:output {:role :supplement}}}})))
      (is (= ["d :output"]
             (projections/decorator-statements
              schema {:transforms {:d {:output {:role :supplement :dataset :x}}}}))))))
