(ns ^:api platform.slice-api-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.authoring.http.oms :as oms]
            [sutra.cli.slice :as slice]
            [sutra.parser.authoring.standard :as standard]
            [sutra.ref.install :as ri]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private schema
  {:schema {:projections standard/projections
            :projects    {:buero {} :werkstatt {}}
            :objects     {:auftrag  {:primary-key :nr :attributes [[:nr :string]]
                                     :match-keys [:nr] :project :buero :plural "Aufträge"}
                          :kunde    {:primary-key :nr :attributes [[:nr :string]]
                                     :match-keys [:nr] :project :buero :plural "Kunden"}
                          :maschine {:primary-key :nr :attributes [[:nr :string]]
                                     :match-keys [:nr] :project :werkstatt
                                     :plural "Maschinen"}}
            :actions     {:anlegen {:params {:nr {:name "Nr" :type :string}}
                                    :project :werkstatt
                                    :rules [{:op :create :object-type :auftrag
                                             :values {:nr [:get :nr]}}]
                                    :edits [{:object-type :auftrag}]}}}})

(defn- code-for [text]
  (str "from sutra_transforms import Input, Output, transform\n\n\n# " text "\n"
       "@transform(\n    inputs={\"a\": Input(kind=\"object-type\", object_type=\"auftrag\")},\n"
       "    output=Output(kind=\"dataset\", dataset=\"auftrag_kopie\", primary_key=\"nr\"),\n)\n"
       "def kopie(a):\n    return a\n"))

(defn- path-of
  [part section node]
  (some (fn [[p t]]
          (when (contains? (get-in (edn/read-string t) [:schema section]) node) p))
        (:files part)))

(defn- nodes-in [text section]
  (set (map name (keys (get-in (edn/read-string text) [:schema section])))))

(defn- pull [b nodes]
  (binding [tx/*branch* b]
    (let [r (api/get! :slices {:nodes nodes})]
      (is (ok? r) (pr-str (:body r)))
      (:body r))))

(defn- push [b body]
  (binding [tx/*branch* b] (api/post! :slices nil body)))

(defn- sent
  [slice f]
  (let [part (get-in slice [:projections :ontologie])]
    {:projections {:ontologie {:files (f (update-keys (:files part) name))
                               :base  (:base part) :shape (:shape part)}}
     :around (:around slice)}))

(deftest a-slice-holds-its-nodes-from-several-texts-and-leaves-the-rest
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [b "slice-texte"
        _ (is (ok? (api/post! :branches {:parent "main" :name b})))
        s (pull b "object-type/auftrag,anlegen")
        part (get-in s [:projections :ontologie])
        auftrag-text (path-of part :objects :auftrag)
        action-text  (path-of part :actions :anlegen)]
    (is (= ["action/anlegen" "object-type/auftrag"] (:nodes s)))
    (testing "cut: only the named nodes, out of two texts"
      (is (not= auftrag-text action-text))
      (is (= #{"auftrag"} (nodes-in (get (update-keys (:files part) name) auftrag-text) :objects))
          "kunde stands in the same text and is not handed out")
      (is (= #{"anlegen"} (nodes-in (get (update-keys (:files part) name) action-text) :actions))))
    (testing "one push changes both texts, placement stays, kunde is untouched"
      (let [edit (fn [text] (pr-str (assoc-in (edn/read-string text)
                                              [:schema :objects :auftrag :description] "neu")))
            r (push b (sent s (fn [fs] (update fs auftrag-text edit))))]
        (is (ok? r) (pr-str (:body r)))
        (is (nil? (get-in r [:body :proposal])) "no code, so no proposal is forced"))
      (let [after (binding [tx/*branch* b]
                    (update-keys (get-in (api/get! :projection-files {:name "ontologie"})
                                         [:body :files])
                                 name))]
        (is (str/includes? (get after auftrag-text) "neu"))
        (is (contains? (nodes-in (get after auftrag-text) :objects) "kunde"))
        (is (contains? (nodes-in (get after auftrag-text) :objects) "auftrag"))))
    (testing "main has not moved: the slice landed on the branch"
      (let [main (update-keys (get-in (api/get! :projection-files {:name "ontologie"})
                                      [:body :files])
                              name)]
        (is (not (str/includes? (get main auftrag-text) "neu")))))
    (testing "a node dropped from the slice is gone, one it never held stays"
      (let [s2   (pull b "object-type/kunde,object-type/auftrag")
            part (get-in s2 [:projections :ontologie])
            text (path-of part :objects :kunde)
            drop-kunde (fn [t] (pr-str (update-in (edn/read-string t)
                                                  [:schema :objects] dissoc :kunde)))
            r (push b (sent s2 (fn [fs] (update fs text drop-kunde))))]
        (is (ok? r) (pr-str (:body r)))
        (let [after (binding [tx/*branch* b]
                      (update-keys (get-in (api/get! :projection-files {:name "ontologie"})
                                           [:body :files])
                                   name))
              objs  (nodes-in (get after text) :objects)]
          (is (not (contains? objs "kunde")))
          (is (contains? objs "auftrag")))))))

(deftest the-cli-pulls-edits-and-pushes-one-slice
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [b   "slice-cli"
        _   (is (ok? (api/post! :branches {:parent "main" :name b})))
        dir (str (java.nio.file.Files/createTempDirectory
                  "slice" (make-array java.nio.file.attribute.FileAttribute 0)))
        opts {:branch b}]
    (is (zero? (slice/pull! opts ["object-type/auftrag" "anlegen"] {:to dir})))
    (let [f (some (fn [^java.io.File x]
                    (when (and (.isFile x) (str/includes? (slurp x) "Aufträge")) x))
                  (file-seq (java.io.File. dir)))]
      (is (some? f) "the type's text lies in the slice")
      (is (zero? (slice/pull! opts ["object-type/auftrag"] {:to dir})) "unchanged: pulls again")
      (spit f (pr-str (assoc-in (edn/read-string (slurp f))
                                [:schema :objects :auftrag :description] "aus der cli")))
      (testing "a pull does not overwrite an edit"
        (is (= 1 (slice/pull! opts ["object-type/auftrag"] {:to dir}))))
      (testing "a preview changes nothing; a push lands on the branch and settles"
        (is (zero? (slice/push! opts dir {:preview? true})))
        (is (zero? (slice/push! opts dir {})))
        (is (str/includes? (slurp f) "aus der cli"))
        (is (zero? (slice/pull! opts ["object-type/auftrag"] {:to dir}))
            "after the push the slice is clean again"))
      (let [on (fn [branch] (binding [tx/*branch* branch]
                              (pr-str (get-in (api/get! :projection-files {:name "ontologie"})
                                              [:body :files]))))]
        (is (str/includes? (on b) "aus der cli"))
        (is (not (str/includes? (on "main") "aus der cli")))))))

(defn- part-edit
  [s text desc]
  (sent s (fn [fs] (update fs text #(pr-str (assoc-in (edn/read-string %)
                                                      [:schema :objects :auftrag :description]
                                                      desc))))))

(deftest a-push-whose-code-is-refused-can-be-sent-again
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name :wieder-code :project :buero} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [b    "slice-wieder"
        _    (is (ok? (api/post! :branches {:parent "main" :name b})))
        s    (pull b "object-type/auftrag")
        text (path-of (get-in s [:projections :ontologie]) :objects :auftrag)
        body (part-edit s text "wieder")
        bad  {"wieder-code" {"transforms/a.py" {:text (code-for "a") :blob nil}
                             "transforms/b.py" {:text (code-for "b") :blob nil}}}
        good {"wieder-code" {"transforms/a.py" {:text (code-for "a") :blob nil}}}
        r1   (push b (assoc body :code bad))]
    (is (not (ok? r1)) "two files declaring one output are refused")
    (testing "the texts stayed on the branch, and the same push goes through"
      (let [r2 (push b (assoc body :code good))]
        (is (ok? r2) (pr-str (:body r2)))
        (is (get-in r2 [:body :commits 0 :commit]) (pr-str (:body r2)))))
    (let [on (binding [tx/*branch* b]
               (pr-str (get-in (api/get! :projection-files {:name "ontologie"}) [:body :files])))]
      (is (str/includes? on "wieder")))))

(deftest a-branch-tells-its-own-findings-from-what-it-inherited
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (let [b "slice-befunde"
        _ (is (ok? (api/post! :branches {:parent "main" :name b})))
        findings (fn [] (let [r (api/get! :schema-problems)]
                          (is (ok? r) (pr-str (:body r)))
                          (get-in r [:body :findings])))
        mark #'oms/inherited-findings
        old  {:rule :r :entity "alt"}
        mine {:rule :r :entity "neu"}]
    (is (empty? (findings)) "the trunk answers")
    (is (empty? (binding [tx/*branch* b] (findings))) "a branch answers, reading the trunk too")
    (is (= [(assoc old :inherited? true) mine] (mark [old mine] [old])))
    (is (= [mine] (mark [mine] nil)) "on the trunk nothing is inherited")))

(deftest a-preview-says-what-the-checks-of-the-code-would-refuse
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name :vor-pruefung :project :buero} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [b    "slice-pruefung"
        _    (is (ok? (api/post! :branches {:parent "main" :name b})))
        s    (pull b "object-type/auftrag")
        text (path-of (get-in s [:projections :ontologie]) :objects :auftrag)
        body (assoc (part-edit s text "vorschau") :preview? true)
        bad  {"vor-pruefung" {"transforms/a.py" {:text (code-for "a") :blob nil}
                              "transforms/b.py" {:text (code-for "b") :blob nil}}}
        good {"vor-pruefung" {"transforms/a.py" {:text (code-for "a") :blob nil}}}
        r    (push b (assoc body :code bad))]
    (is (ok? r) (pr-str (:body r)))
    (is (str/includes? (str (get-in r [:body :code-refused 0 :why])) "same output")
        (pr-str (:body r)))
    (is (nil? (get-in r [:body :commits])) "a preview commits nothing")
    (is (empty? (get-in (push b (assoc body :code good)) [:body :code-refused])))
    (let [on (binding [tx/*branch* b]
               (pr-str (get-in (api/get! :projection-files {:name "ontologie"}) [:body :files])))]
      (is (not (str/includes? on "vorschau")) "nor does it land the texts"))))

(deftest a-push-refused-in-its-second-repository-can-be-sent-again
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name :erst :project :buero} {:stage? true})))
  (is (ok? (api/mutate :create-repository {:name :zweit :project :buero} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [b     "slice-zwei"
        _     (is (ok? (api/post! :branches {:parent "main" :name b})))
        s     (pull b "object-type/auftrag")
        text  (path-of (get-in s [:projections :ontologie]) :objects :auftrag)
        body  (part-edit s text "zwei")
        first {"transforms/eins.py" {:text (code-for "eins") :blob nil}}
        two   (fn [& names]
                (into {} (for [n names]
                           [(str "transforms/" n ".py")
                            {:text (str/replace (code-for n) "auftrag_kopie" "zweit_kopie")
                             :blob nil}])))
        r1    (push b (assoc body :code {"erst" first "zweit" (two "a" "b")}))]
    (is (not (ok? r1)))
    (is (str/includes? (pr-str (:body r1)) "already committed: erst") (pr-str (:body r1)))
    (let [r2 (push b (assoc body :code {"erst" first "zweit" (two "a")}))]
      (is (ok? r2) (pr-str (:body r2)))
      (is (= ["zweit"] (map :repo (get-in r2 [:body :commits])))
          "what landed the first time is not a conflict and not a second commit"))))

(deftest a-function-and-its-code-file-go-through-one-push
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name :fn-code :project :buero} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [b    "slice-fn"
        _    (is (ok? (api/post! :branches {:parent "main" :name b})))
        src  (str "(defn fn-liste\n  [{:keys [auftraege nr]}]\n"
                  "  (filter #(= nr (:nr %)) auftraege))\n")
        file {"transforms/fn_liste.clj" {:text src :blob nil}}
        r1   (push b {:code {"fn-code" (assoc file "transforms/kopie.py"
                                              {:text (code-for "k") :blob nil})}
                      :message "code"})
        _    (is (ok? r1) (pr-str (:body r1)))
        s    (pull b "transform/auftrag-kopie")
        part (get-in s [:projections :code-repository])
        path (name (first (keys (:files part))))
        text (get (update-keys (:files part) name) path)
        fun  {:code-file "transforms/fn_liste.clj"
              :inputs {:nr {:kind :scalar}
                       :auftraege {:kind :object-type :object-type :auftrag}}
              :language :clojure :project :buero :repository :fn-code}
        body {:projections {:code-repository
                            {:files {path (pr-str (assoc-in (edn/read-string text)
                                                            [:schema :functions :fn-liste] fun))}
                             :base (:base part)
                             :shape (:shape part)}}
              :around (:around s)
              :message "funktion"}]
    (testing "alone the file does not stay, and the refusal says what to do"
      (let [r (push b body)]
        (is (not (ok? r)))
        (is (str/includes? (pr-str (:body r)) "in one `slice push`") (pr-str (:body r)))))
    (testing "text and file in one push declare the function"
      (let [preview (push b (assoc body :code {"fn-code" file} :preview? true))
            r       (push b (assoc body :code {"fn-code" file}))]
        (is (ok? preview) (pr-str (:body preview)))
        (is (empty? (get-in preview [:body :code-refused])) (pr-str (:body preview)))
        (is (ok? r) (pr-str (:body r)))
        (let [texts (binding [tx/*branch* b]
                      (pr-str (get-in (api/get! :projection-files {:name "code-repository"})
                                      [:body :files])))]
          (is (str/includes? texts "fn-liste") texts))))))

(deftest a-file-named-like-the-type-it-writes-declares-one-dataset
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name :typ-code :project :buero} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [b    "slice-typname"
        _    (is (ok? (api/post! :branches {:parent "main" :name b})))
        s    (pull b "object-type/kunde")
        part (get-in s [:projections :ontologie])
        text (path-of part :objects :kunde)
        neu  {:primary-key :id :attributes [[:id :string]] :match-keys [:id]
              :project :buero :plural "Zuordnungen"}
        body (sent s (fn [fs] (update fs text
                                      #(pr-str (assoc-in (edn/read-string %)
                                                         [:schema :objects :zuordnung] neu)))))
        r1   (push b body)
        _    (is (ok? r1) (pr-str (:body r1)))
        code (str "from sutra_transforms import Input, Output, transform\n\n\n"
                  "@transform(\n    inputs={\"a\": Input(kind=\"object-type\","
                  " object_type=\"auftrag\")},\n"
                  "    output=Output(kind=\"object-type\", object_type=\"zuordnung\","
                  " primary_key=\"id\"),\n)\n"
                  "def zuordnung(a):\n    return a\n")
        r2   (push b {:code {"typ-code" {"transforms/zuordnung.py" {:text code :blob nil}}}
                      :message "typ"})]
    (is (ok? r2) (pr-str (:body r2)))
    (is (get-in r2 [:body :commits 0 :commit]) (pr-str (:body r2)))
    (testing "the transform keeps the file's name"
      (is (some #{"transform/zuordnung"} (:nodes (pull b "transform/zuordnung")))))
    (testing "the type's dataset keeps its name, a transform's gives way, once"
      (let [name-of #'ri/output-name
            w       {:datasets {"typ" {:dataset-name :zuordnung}}}]
        (is (= :zuordnung-output (name-of w "tr" :zuordnung)))
        (is (= :zuordnung (name-of w "typ" :zuordnung)))
        (is (= :frei (name-of w "tr" :frei)))))))

(deftest a-preview-judges-a-text-against-what-the-others-define
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name :vor-code :project :werkstatt} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [b     "slice-vorschau"
        _     (is (ok? (api/post! :branches {:parent "main" :name b})))
        s     (pull b "action/anlegen")
        part  (get-in s [:projections :ontologie])
        text  (path-of part :actions :anlegen)
        code  (binding [tx/*branch* b] (:body (api/get! :projection-files
                                                        {:name "code-repository"})))
        path  "vor-code.ent"
        fn-src (str "(defn ^{:sutra/function {:edits [:auftrag]}} neu [nr] "
                    "[(OntologyClient/create :auftrag {:nr nr})])")
        fn-text (pr-str {:schema {:functions {:neu-buchen {:language :clojure :source fn-src
                                                           :project :werkstatt
                                                           :repository :vor-code}}}})
        call  (fn [t] (pr-str (update-in (edn/read-string t) [:schema :actions :anlegen] assoc
                                         :params {:nr {:type :string}}
                                         :edits [{:object-type :auftrag}]
                                         :rules [{:op :apply-edit-function :function :neu-buchen
                                                  :args {:nr [:get :nr]}}])))
        body  {:projections
               {:ontologie {:files {text (call (get (update-keys (:files part) name) text))}
                            :base (:base part) :shape (:shape part)}
                :code-repository {:files {path fn-text} :base {} :shape (:shape code)}}
               :around (:around s)}
        r     (push b (assoc body :preview? true))]
    (is (ok? r) (pr-str (:body r)))
    (is (nil? (get-in (binding [tx/*branch* b] (api/get! :staging)) [:body :staged 0]))
        "a preview stages nothing")))

(deftest one-push-brings-text-and-code-and-opens-the-proposal
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :create-repository {:name :buero-code :project :buero} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [b "slice-code"
        _ (is (ok? (api/post! :branches {:parent "main" :name b})))
        s (pull b "object-type/auftrag")
        part (get-in s [:projections :ontologie])
        text (path-of part :objects :auftrag)
        edit (fn [t] (pr-str (assoc-in (edn/read-string t)
                                       [:schema :objects :auftrag :description] "mit code")))
        new-code {"buero-code" {"transforms/kopie.py" {:text (code-for "eins") :blob nil}}}
        r (push b (assoc (sent s (fn [fs] (update fs text edit)))
                         :code new-code :message "auftrag und kopie"))]
    (is (ok? r) (pr-str (:body r)))
    (is (get-in r [:body :proposal :id]) "a push with code opens the proposal it needs")
    (is (some? (get-in r [:body :commits 0 :commit])) (pr-str (:body r)))
    (testing "the decorator declared the transform, and the slice now reads its code"
      (let [s2 (pull b "transform/auftrag-kopie")]
        (is (= ["transform/auftrag-kopie"] (:nodes s2)))
        (is (str/includes? (str (get-in s2 [:code "buero-code" "transforms/kopie.py" :text]))
                           "eins")
            (pr-str (:code s2)))))
    (testing "a file that moved since the pull conflicts; --theirs keeps it, --mine overrides"
      (let [stale (binding [tx/*branch* b]
                    (get-in (pull b "transform/auftrag-kopie") [:code]))
            blob  (some (comp :blob val) (mapcat val stale))
            _     (is (ok? (push b {:code {"buero-code" {"transforms/kopie.py"
                                                         {:text (code-for "zwei") :blob blob}}}})))
            again {"buero-code" {"transforms/kopie.py" {:text (code-for "drei") :blob blob}}}
            r     (push b {:code again})]
        (is (= 409 (:status r)) (pr-str (:body r)))
        (is (str/includes? (pr-str (:body r)) "transforms/kopie.py"))
        (is (ok? (push b {:code again :resolve "mine"})))))))
