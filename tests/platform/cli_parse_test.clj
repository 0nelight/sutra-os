(ns ^:unit platform.cli-parse-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing are]]
            [sutra.parser.entr :as entr]
            [sutra.parser.ent :as ent]
            [sutra.cli.parse :as parse]
            [sutra.cli.projection]
            [sutra.cli.main :as main]
            [sutra.cli.verbs.observe]
            [sutra.cli.verbs.watch]
            [sutra.cli.session :as session]))

(defn- sess []
  (doto (session/new-session {:identity {:username "emil" :role :owner}})
    (session/put-register! [{:id "ereignis/8801" :label "a"}
                      {:id "ereignis/8830" :label "b"}
                      {:id "obj/3"         :label "c"}])))

(defn- step [line] (first (:steps (parse/line->cmds (sess) line))))

(defn- refusal
  [line]
  (try (step line) nil
       (catch clojure.lang.ExceptionInfo e (:refusal/code (ex-data e)))))

(deftest tokenize-respects-quotes
  (is (= ["explore" "a b" "--type" "t"]
         (parse/tokenize "explore \"a b\" --type t")))
  (is (= ["a" "b" "c"] (parse/tokenize "  a   b c ")))
  (is (= ["one two"] (parse/tokenize "'one two'")))
  (is (= [] (parse/tokenize "   "))))

(deftest query-emits-every-op-the-backend-executes
  (is (= {:kind {:not= "rechnung"}}
         (:where (second (step "query auftrags-ereignis --where kind!=rechnung")))))
  (is (= {:von-adresse {:starts-with "Mül"}}
         (:where (second (step "query auftrags-ereignis --where von-adresse^=Mül")))))
  (is (= {:von-adresse {:contains "müller"}}
         (:where (second (step "query auftrags-ereignis --where von-adresse~müller")))))
  (testing "two-character ops win over the single-character ones"
    (is (= {:kind {:not= "x"}}
           (:where (second (step "query auftrags-ereignis --where kind!=x"))))))
  (testing "ordering ops still coerce numerics; text ops keep the string"
    (is (= {:wo-art-id {:>= 8}}
           (:where (second (step "query auftrags-ereignis --where wo-art-id>=8")))))
    (is (= {:kind {:contains "8"}}
           (:where (second (step "query auftrags-ereignis --where kind~8"))))))
  (testing "a linked object's property takes the new ops too"
    (is (= {:kommission {:name {:contains "bau"}}}
           (:where (second (step "query auftrags-ereignis --where kommission.name~bau")))))))

(deftest query-addresses-links-by-side
  (testing "--around names link SIDES; no direction rides along"
    (is (= [:assigned-aircraft :operator]
           (:around (second (step "query flight --around assigned-aircraft,operator"))))))
  (testing "--show side.attr is a one-hop column, grouped per side, in order"
    (is (= [:tail {:crew [:name :rank]} :gate]
           (:show (second (step "query flight --show tail,crew.name,gate,crew.rank"))))))
  (testing "two clauses on one side land in ONE sub-filter"
    (is (= {:crew {:name {:= "x"} :rank {:>= 3}}}
           (:where (second (step "query flight --where crew.name=x,crew.rank>=3")))))))

(deftest sqls-not-equal-spelling-is-refused-by-name
  (is (thrown-with-msg? Exception #"!="
                        (step "query zeitbuchung --where wo-art-id<>3")))
  (testing "the ops it does have are untouched"
    (is (= {:wo-art-id {:< 3}}
           (:where (second (step "query zeitbuchung --where wo-art-id<3")))))
    (is (= {:wo-art-id {:not= "3"}}
           (:where (second (step "query zeitbuchung --where wo-art-id!=3")))))))

(deftest pronoun-single-and-range-and-set
  (is (= [:around {:ids ["ereignis/8801"]}] (step "around #1")))
  (is (= [:around {:ids ["ereignis/8801" "ereignis/8830" "obj/3"]}] (step "around #1-3")))
  (is (= [:around {:ids ["ereignis/8801" "ereignis/8830" "obj/3"]}] (step "around $"))))

(deftest fanout-expands-to-n-steps
  (let [{:keys [steps]} (parse/line->cmds (sess) "action beleg-freigeben beleg=$")]
    (is (= 3 (count steps)))
    (is (= #{"ereignis/8801" "ereignis/8830" "obj/3"}
           (set (map #(get-in % [1 :args :beleg]) steps))))))

(deftest fanout-rejects-two-multi-args
  (is (thrown? Exception (parse/line->cmds (sess) "action foo a=$ b=$"))))

(deftest friendly-errors
  (is (thrown-with-msg? Exception #"no #9" (parse/line->cmds (sess) "around #9")))
  (is (thrown-with-msg? Exception #"missing argument text" (parse/line->cmds (sess) "explore")))
  (is (thrown-with-msg? Exception #"unknown command" (parse/line->cmds (sess) "frobnicate")))
  (is (thrown-with-msg? Exception #"unknown flag" (parse/line->cmds (sess) "explore x --nope y")))
  (is (thrown-with-msg? Exception #"no saved set" (parse/line->cmds (sess) "action a x=$gone"))))

(deftest global-flags-ride-on-globals-not-the-arg-spec
  (is (= {:steps [[:overview {}]] :globals {:fields ["branch"] :json? true}}
         (parse/line->cmds (sess) "overview --fields branch")))
  (is (= {:globals {:yes? true}}
         (select-keys (parse/line->cmds (sess) "action delete-rechnung k=v --yes") [:globals])))
  (testing "position-independent + combinable"
    (is (= {:json? true :fields ["a" "b"]}
           (:globals (parse/line->cmds (sess) "query t --json --fields a,b")))))
  (testing "a genuinely unknown verb-flag is still rejected"
    (is (thrown-with-msg? Exception #"unknown flag" (parse/line->cmds (sess) "query t --nope x")))))

(deftest noun-paths-equal-their-flat-form
  (testing "a noun→verb path emits the SAME step as the historical flat verb"
    (are [noun-path flat] (= (step noun-path) (step flat))
      "ontology object-type show email" "describe email"
      "object query bestellung"         "query bestellung"
      "object show #1"                  "show #1"
      "object search foo"               "explore foo"
      "link list --type refers-to"      "links --type refers-to"
      "build list --kind import"        "runs --kind import"
      "build show 42"                   "run 42"
      "dataset history raw_messages"    "transactions raw_messages"
      "source show pdfsrc"              "source pdfsrc"
      "access show emil"                "access emil"
      "branch checkout main"            "checkout main"
      "changeset review"               "review")))

(deftest transform-create-emits-a-valid-install
  (testing "a python object-output transform with a source input round-trips sutra.parser.ent"
    (let [f (second (step (str "transform create parse-invoices --language python "
                               "--input raw:source:erp:raw_invoices --output-type invoice "
                               "--column doc-number:string --pk doc-number")))]
      (is (= f (ent/validate! f :cli)))
      (let [t (get-in f [:schema :transforms :parse-invoices])]
        (is (= :python (:language t)))
        (is (= {:kind :datasource :datasource :erp :table "raw_invoices"}
               (get-in t [:inputs :raw])))
        (is (= {:kind :object-type :object-type :invoice} (:output t)))
        (is (= :doc-number (:primary-key t)))
        (is (= [[:doc-number :string]] (:output-columns t)))
        (is (= [:erp] (:external-systems t)))
        (is (string? (:code t))))))
  (testing "an --llm input adds a language-model input + folds its source into egress"
    (let [t (get-in
             (second (step (str "transform create enrich --input up:transform:parse-invoices "
                                "--llm model:xai:grok-4 --output-dataset enriched")))
             [:schema :transforms :enrich])]
      (is (= {:kind :language-model :source :xai :model "grok-4"} (get-in t [:inputs :model])))
      (is (= [:xai] (:external-systems t)))
      (is (= {:kind :dataset :dataset :enriched} (:output t)))))
  (testing "a sql --incremental transform reading a dataset round-trips"
    (let [f (second (step (str "transform create rollup --language pg-sql "
                               "--query \"SELECT 1 AS a\" --input d:dataset:raw "
                               "--output-dataset totals --incremental")))
          t (get-in f [:schema :transforms :rollup])]
      (is (= f (ent/validate! f :cli)))
      (is (= :pg-sql (:language t)))
      (is (= "SELECT 1 AS a" (:query t)))
      (is (true? (:incremental t)))
      (is (nil? (:external-systems t)))))
  (testing "a sql link-EDGE transform (--output-link) round-trips sutra.parser.ent"
    (let [f (second
             (step
              (str "transform create link-position-artikel --language pg-sql "
                   "--query \"SELECT rp.position_id AS p_id, a.artikel_id AS a_id "
                   "FROM positionen rp JOIN artikel a ON rp.artnr = a.artnr\" "
                   "--input positionen:object-type:rechnungsposition,artikel:object-type:artikel "
                   "--output-link position-artikel --from-key p_id --to-key a_id")))
          t (get-in f [:schema :transforms :link-position-artikel])]
      (is (= f (ent/validate! f :cli)))
      (is (= {:kind :link-type :link-type :position-artikel
              :from-key-column :p_id :to-key-column :a_id} (:output t)))
      (is (nil? (:primary-key t)))))
  (testing "--code-file loads the body from a file (no shell-quoting hazard)"
    (let [tmp (java.io.File/createTempFile "xform" ".py")]
      (spit tmp "def run():\n    return []  # `backticks` and \"quotes\" survive\n")
      (let [t (get-in (second (step (str "transform create loaded --language python "
                                         "--input a:dataset:b --output-dataset out "
                                         "--code-file " (.getPath tmp))))
                      [:schema :transforms :loaded])]
        (is (re-find #"backticks" (:code t))))))
  (testing "and reports bad input up front"
    (is (= :refusal.scaffold/needs-input (refusal "transform create x --output-dataset d")))
    (is (= :refusal.scaffold/object-output-needs-pk
           (refusal "transform create x --input a:dataset:b --output-type t")))
    (is (= :refusal.scaffold/one-output
           (refusal (str "transform create x --input a:dataset:b --output-type t "
                         "--output-dataset d --pk k"))))
    (is (= :refusal.scaffold/link-output-needs-keys
           (refusal "transform create x --input a:object-type:b --output-link l --from-key p")))
    (is (thrown-with-msg? Exception #"label:kind:ref"
                          (step "transform create x --input bad --output-dataset d")))
    (is (thrown-with-msg? Exception #"needs label:source"
                          (step "transform create x --input a:source:erp --output-dataset d")))))

(deftest function-create-emits-a-valid-install
  (testing "a QUERY function (default kind) is a compute shape under :functions"
    (let [f (second (step (str "function create active-orders --language python "
                               "--code \"def f(o): return o\" --input o:object-type:order")))]
      (is (= f (ent/validate! f :cli)))
      (let [w (get-in f [:schema :functions :active-orders])]
        (is (= :python (:language w)))
        (is (= {:kind :object-type :object-type :order} (get-in w [:inputs :o])))
        (is (nil? (:output w)))
        (is (nil? (:source w))))))
  (testing "an EDIT function is a :source program over a --target-type"
    (let [f (second
             (step
              "function create approve --kind edit --target-type invoice --param threshold:number"))]
      (is (= f (ent/validate! f :cli)))
      (let [w (get-in f [:schema :functions :approve])]
        (is (= :invoice (:target-type w)))
        (is (= :clojure (:language w)))
        (is (string? (:source w)))
        (is (= {:threshold :number} (:param-types w))))))
  (testing "a Function runs user code — :sql is a compute language, not a code one"
    (is (= :refusal.scaffold/function-language
           (refusal (str "function create x --language pg-sql --query \"SELECT 1\" "
                         "--input a:object-type:t")))))
  (testing "a Function has no output — one would make it a BACKING transform"
    (doseq [flag ["--output-dataset d" "--output-type t --pk id"]]
      (is (thrown-with-msg? Exception #"a Function has no output"
                            (step (str "function create x --input a:object-type:t " flag))))))
  (testing "the query kind still reuses transform create's grammar (input required)"
    (is (= :refusal.scaffold/needs-input (refusal "function create x")))
    (is (= :refusal.scaffold/object-output-needs-pk
           (refusal "transform create x --input a:object-type:t --output-type t"))))
  (testing "edit kind needs a target-type; bad --kind is rejected"
    (is (= :refusal.scaffold/edit-function-needs-target
           (refusal "function create x --kind edit")))
    (is (thrown-with-msg? Exception #"--kind must be query or edit"
                          (step "function create x --kind bogus")))))

(deftest emitted-steps-validate-through-entr
  (testing "every REPL-emitted step is a canonical entr step"
    (doseq [line ["explore \"x\" --type t" "around #1" "checkout main"
                  "runs --kind import" "health" "audit" "links --type refers-to"
                  "action foo x=#1 --stage" "review --verbose" "check" "commit"
                  "eval classify --iterations 3" "eval suites"
                  "eval history classify"
                  "eval save classify.eval.edn" "eval compare a b"]]
      (let [{:keys [steps]} (parse/line->cmds (sess) line)]
        (is (= {:steps steps} (entr/validate! {:steps steps} :repl))
            (str line " should validate"))))))

(deftest a-push-refusal-reads-its-reasons-as-lines
  (testing "a reference refusal brings strings, a grammar refusal the humanized
            tree — the push prints both as `path — reason`"
    (let [lines #'sutra.cli.projection/reason-lines]
      (is (nil? (lines nil)))
      (is (= ["rollup/derived on geraet.angebote traverses link :x"]
             (lines ["rollup/derived on geraet.angebote traverses link :x"])))
      (is (= ["objects > rechnung > display-name — disallowed key"]
             (lines {:objects {:rechnung {:display-name ["disallowed key"]}}}))))))

(deftest a-refused-runbook-names-the-key-and-the-path
  (testing "a runbook written against an older grammar (`:concern` before it became
            `:title`) is refused with `path — reason` in the message, not only in
            the ex-data a stacktrace shows"
    (let [msg (try (entr/validate! {:concern "x" :steps [[:commit]]} "automationen.entr")
                   (catch clojure.lang.ExceptionInfo e (ex-message e)))]
      (is (string? msg))
      (is (str/includes? msg "concern — disallowed key"))
      (is (str/includes? msg "automationen.entr"))
      (is (str/includes? msg "unknown/misspelled key")))))

(deftest the-live-filters-select-the-side-asked-for
  (let [llm   {:kind :llm/request :caller "c" :model "m" :at 0}
        build {:kind :build/phase :phase "transforms" :at 0}
        render (fn [opts ev] (#'sutra.cli.verbs.watch/live-line {} opts ev))]
    (testing "no flag ⇒ both"
      (is (seq (render {} llm)))
      (is (seq (render {} build))))
    (testing "--llm ⇒ only model calls; --build ⇒ only build boundaries"
      (is (seq (render {:llm? true} llm)))
      (is (nil? (render {:llm? true} build)))
      (is (seq (render {:build? true} build)))
      (is (nil? (render {:build? true} llm))))))

(deftest errors-filter-keeps-warn-and-error-only
  (let [render (fn [ev] (#'sutra.cli.verbs.watch/live-line {} {:errors? true} ev))]
    (is (seq (render {:kind :llm/response :level :error :caller "c" :error "HTTP 403" :at 0})))
    (is (seq (render {:kind :llm/response :level :warn  :caller "c"
                      :error "daily LLM budget reached" :at 0})))
    (is (nil? (render {:kind :llm/request  :level :debug :caller "c" :model "m" :at 0})))
    (is (nil? (render {:kind :build/phase  :level :info  :phase "transforms" :at 0})))))

(deftest ein-argument-mit-anfuehrungszeichen-ueberlebt-den-einzeiler
  (let [durch (fn [& argv]
                (parse/tokenize (clojure.string/join " " (map main/requote-arg argv))))]
    (testing "GeoJSON hat kein Leerzeichen und wurde trotzdem entkernt"
      (is (= ["action" "grundriss-umriss-aendern"
              "umriss={\"type\":\"Polygon\",\"coordinates\":[[[0,0],[1,0],[1,1]]]}"]
             (durch "action" "grundriss-umriss-aendern"
                    "umriss={\"type\":\"Polygon\",\"coordinates\":[[[0,0],[1,0],[1,1]]]}"))))
    (testing "gewöhnliche Token bleiben unangetastet"
      (is (= ["describe" "raum"] (durch "describe" "raum")))
      (is (= ["explore" "Versand avis"] (durch "explore" "Versand avis"))))
    (testing "ein Apostroph im Wert ueberlebt ebenso"
      (is (= ["note=Alma's"] (durch "note=Alma's"))))))
