(ns ^:unit ontology.clojure-transform-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.languages :as langs]
            [sutra.parser.spec :as spec]
            [sutra.parser.ent :as ent]
            [sutra.compute.clojure :as cljt]
            [sutra.compute.runner :as runner]))

(defn- transform
  [code & {:as extra}]
  (merge {:transform-name :demo
          :language :clojure
          :code code
          :inputs {:rows {:kind :dataset :dataset :src}}
          :output {:kind :object-type :object-type :things}
          :primary-key :id}
         extra))

(defn- run
  [code rows & {:as opts}]
  (runner/run-code! :clojure (transform code)
                    (merge {:inputs {:rows {:kind :dataset :rows rows}}} opts)))

(deftest the-sandbox-has-no-host-access
  (testing "every escape from the interpreter is absent, not merely discouraged"
    (doseq [[what code]
            {"static interop"   "(fn [_] (.exec (Runtime/getRuntime) \"id\"))"
             "constructors"     "(fn [_] [{:x (java.io.File. \"/etc/passwd\")}])"
             "slurp"            "(fn [_] [{:x (slurp \"/etc/passwd\")}])"
             "spit"             "(fn [_] (spit \"/tmp/x\" \"pwned\"))"
             "eval"             "(fn [_] (eval '(+ 1 2)))"
             "load-string"      "(fn [_] (load-string \"(+ 1 2)\"))"
             "resolve"          "(fn [_] ((resolve 'clojure.java.io/file) \"/etc\"))"
             "alter-var-root"   "(fn [_] (alter-var-root #'clojure.core/+ (constantly -)))"}]
      (is (thrown? clojure.lang.ExceptionInfo (run code []))
          (str what " must not be reachable from transform code")))))

(deftest value-classes-reach-the-sandbox-but-nothing-behind-them
  (testing "exact decimals, commercial rounding and the calendar"
    (let [[row] (:rows (run "(fn [_]
                               [{:cent  (.setScale (bigdec \"2.345\") 2 RoundingMode/HALF_UP)
                                 :third (with-precision 4 :rounding HALF_EVEN (/ 1M 3))
                                 :dec   (sutra/dec \" 0.125 \" 2)
                                 :ende  (.with (LocalDate/of 2024 2 10)
                                               (TemporalAdjusters/lastDayOfMonth))
                                 :monat (YearMonth/of 2024 1)
                                 :tag   (.getDayOfWeek (java.time.LocalDate/parse \"2026-09-29\"))
                                 :frist (Period/ofDays 30)}])"
                            []))]
      (is (= 2.35M (:cent row)))
      (is (= 0.3333M (:third row)))
      (is (= 0.13M (:dec row)))
      (is (= "2024-02-29" (:ende row)))
      (is (= "2024-01" (:monat row)))
      (is (= "TUESDAY" (:tag row)))
      (is (= "P30D" (:frist row)))))
  (testing "a permitted instance does not open its Class"
    (doseq [code ["(fn [_] [{:x (.getClassLoader (.getClass (bigdec 1)))}])"
                  "(fn [_] [{:x (.forName Class \"java.lang.Runtime\")}])"]]
      (is (thrown? clojure.lang.ExceptionInfo (run code []))))))

(deftest sutra-dec-is-exact-where-sutra-num-is-not
  (is (= [{:d 0.3M}] (:rows (run "(fn [_] [{:d (+ (sutra/dec 0.1) (sutra/dec \"0.2\"))}])" []))))
  (is (thrown? clojure.lang.ExceptionInfo (run "(fn [_] [{:d (sutra/dec :x)}])" []))))

(deftest a-transform-cannot-see-another-transforms-defs
  (is (= [{:v 1}] (:rows (run "(defn helper [] 1) (fn [_] [{:v (helper)}])" []))))
  (is (thrown? clojure.lang.ExceptionInfo
               (run "(fn [_] [{:v (helper)}])" []))))

(deftest rows-bind-by-alias-and-come-back-as-rows
  (is (= [{:id 1 :doubled 42} {:id 2 :doubled 6}]
         (:rows (run "(fn [{:keys [rows]}] (for [r rows] {:id (:id r) :doubled (* 2 (:n r))}))"
                     [{:id 1 :n 21} {:id 2 :n 3}])))))

(deftest a-lazy-body-is-realized-inside-the-run
  (is (= [{:seen 2}]
         (:rows (run "(fn [_] (for [_ [1]] {:seen (count (sutra/previous-output))}))"
                     [] :prev-rows [{:a 1} {:a 2}])))))

(deftest returned-values-are-coerced-to-storable-ones
  (testing "the coercion JSON used to perform for the python path"
    (let [[row] (:rows (run "(fn [_] [{:kw :beleg :sym 'x :ratio 1/4 :set #{1} :seq (map inc [1 2])
                                        :s \"keep\" :n 42 :nil nil :bool true}])"
                            []))]
      (is (= "beleg" (:kw row)))
      (is (= "x" (:sym row)))
      (is (= 0.25 (:ratio row)))
      (is (= [1] (:set row)))
      (is (= [2 3] (:seq row)))
      (testing "and nothing else is touched"
        (is (= "keep" (:s row)))
        (is (= 42 (:n row)))
        (is (nil? (:nil row)))
        (is (true? (:bool row)))))))

(deftest keys-are-returned-as-written
  (let [[row] (:rows (run "(fn [_] [{:doc-number \"D-1\" :doc_id \"7\"}])" []))]
    (is (= "7" (:doc_id row)))))

(deftest string-keys-are-accepted
  (is (= [{:id 1}] (:rows (run "(fn [_] [{\"id\" 1}])" [])))))

(deftest a-multi-output-transform-returns-a-map-of-outputs
  (let [{:keys [outputs]}
        (runner/run-code! :clojure
                          (-> (transform
                               "(fn [{:keys [rows]}] {:msgs (map #(hash-map :id (:id %)) rows)
                                               :atts [{:id 9}]})")
                              (dissoc :output)
                              (assoc :outputs [{:kind :object-type :object-type :msgs}
                                               {:kind :object-type :object-type :atts}]))
                          {:inputs {:rows {:kind :dataset :rows [{:id 1}]}}})]
    (is (= {:msgs [{:id 1}] :atts [{:id 9}]} outputs))))

(deftest previous-output-is-the-incremental-done-set
  (is (= [{:done 2}]
         (:rows (run "(fn [_] [{:done (count (sutra/previous-output))}])"
                     [] :prev-rows [{:id 1} {:id 2}])))))

(deftest output-enum-reads-the-ontologys-allowed-values
  (is (= [{:kinds ["rechnung" "gutschrift"]}]
         (:rows (run "(fn [_] [{:kinds (sutra/output-enum :beleg-kind)}])"
                     [] :output-enums {"beleg-kind" ["rechnung" "gutschrift"]})))))

(deftest cap-batch-withholds-the-consumer-cursor
  (let [result (run "(fn [{:keys [rows]}] (map #(hash-map :id (:id %)) (sutra/cap-batch rows 1)))"
                    [{:id 1} {:id 2} {:id 3}])]
    (is (= [{:id 1}] (:rows result)))
    (is (false? (:incremental-complete result))
        "a capped run must NOT advance its pointer — else rows 2..3 strand below it")))

(deftest a-drained-run-leaves-the-cursor-alone
  (let [result (run "(fn [{:keys [rows]}] (map #(hash-map :id (:id %)) (sutra/cap-batch rows 10)))"
                    [{:id 1}])]
    (is (not (contains? result :incremental-complete)))))

(deftest trace-breadcrumbs-only-on-a-traced-build
  (is (nil? (:trace (run "(fn [_] (sutra/trace \"looked\" :at 1) [{:id 1}])" []))))
  (is (= [{:event "looked" :at 1}]
         (:trace (run "(fn [_] (sutra/trace \"looked\" :at 1) [{:id 1}])" [] :trace? true)))))

(deftest the-last-form-must-be-a-function
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"function of one argument"
                        (run "(def x 1)" []))))

(deftest a-single-output-transform-returning-one-map-is-named-as-such
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must return a SEQUENCE"
                        (run "(fn [_] {:id 1 :n 2})" []))))

(deftest a-non-map-row-names-the-offender
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"row MAPS"
                        (run "(fn [_] [1 2 3])" []))))

(deftest an-egress-grant-is-refused-rather-than-silently-ignored
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #":external-systems"
       (runner/run-code! :clojure (transform "(fn [_] [])" :external-systems [:gls-inbox])
                         {:inputs {}}))))

(deftest a-csv-datasource-input-is-parsed-into-rows
  (let [f (java.io.File/createTempFile "sutra_test_" ".csv")]
    (try
      (spit f "id,name\n1,afra\n2,alan\n")
      (is (= [{:id "1" :name "afra"} {:id "2" :name "alan"}]
             (:rows (runner/run-code! :clojure (transform "(fn [{:keys [src]}] (vec src))")
                                      {:inputs
                                       {:src {:kind :datasource :path (.getAbsolutePath f)}}}))))
      (finally (.delete f)))))

(deftest a-non-csv-datasource-input-names-the-fix
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"only when it is a CSV file"
       (runner/run-code! :clojure (transform "(fn [_] [])")
                         {:inputs {:src {:kind :datasource :path "/tmp/x.pdf"}}}))))

(deftest a-body-declares-its-own-wiring
  (let [spec {:inputs {:raw {:kind :dataset :dataset :raw_belege}}
              :output {:kind :object-type :object-type :belege}
              :primary-key :beleg-id
              :incremental true}]
    (testing "decorator-shaped — (sutra/transform {…} (fn …))"
      (is (= spec (cljt/declared-transform-spec
                   (str "(sutra/transform " (pr-str spec) " (fn [{:keys [raw]}] raw))")))))
    (testing "defn-shaped — the attr-map, with the defn form LAST"
      (is (= spec (cljt/declared-transform-spec
                   (str "(defn beleg-rows {:sutra/transform " (pr-str spec) "}"
                        " [{:keys [raw]}] raw)")))))
    (testing "either way the fn is still the fn"
      (is (= [{:id 1}]
             (:rows (runner/run-code! :clojure (transform (str "(sutra/transform " (pr-str spec)
                                                               " (fn [{:keys [rows]}] rows))"))
                                      {:inputs {:rows {:kind :dataset :rows [{:id 1}]}}}))))
      (is
       (= [{:id 1}]
          (:rows
           (runner/run-code! :clojure
                             (transform
                              "(defn f {:sutra/transform {:primary-key :id}} [{:keys [rows]}] rows)") {:inputs {:rows {:kind :dataset :rows [{:id 1}]}}})))))))

(deftest declaring-is-optional
  (is (nil? (cljt/declared-transform-spec "(fn [x] x)"))
      "a body with no declaration reports none — its wiring is the .ent's job"))

(deftest a-misspelled-spec-field-is-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown :sutra/transform field"
                        (cljt/declared-transform-spec
                         "(sutra/transform {:primary_key :id} (fn [x] x))")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be a MAP"
                        (cljt/declared-transform-spec
                         "(with-meta (fn [x] x) {:sutra/transform :nope})"))))

(deftest sutra-transform-checks-its-own-arguments
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"spec MAP"
                        (cljt/introspect "(sutra/transform (fn [x] x) {:primary-key :id})")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"then a FUNCTION"
                        (cljt/introspect "(sutra/transform {:primary-key :id} :not-a-fn)"))))

(deftest the-validator-dispatches-off-the-language-table
  (testing "every code language gets its own dispatch tag, every SQL dialect the one of SQL"
    (doseq [lang langs/compute-language-names]
      (is (= (if (langs/query-language? lang) :compute/sql (keyword "compute" (name lang)))
             (spec/transform-kind {:language lang})))))
  (testing "a clojure transform validates like a python one"
    (is (spec/transform-valid?
         {:transform-name :beleg-rows :language :clojure
          :code "(fn [_] [])"
          :inputs {:raw {:kind :dataset :dataset :raw_belege}}
          :output {:kind :object-type :object-type :belege :primary-key :id}}))
    (testing "and is held to the same output-mutex"
      (is (not (spec/transform-valid?
                {:transform-name :beleg-rows :language :clojure :code "(fn [_] [])"
                 :inputs {:raw {:kind :dataset :dataset :raw_belege}}}))
          "no output at all"))
    (testing "and to the same nil-guard — a compute carries no datasource backing"
      (is (not (spec/transform-valid?
                {:transform-name :beleg-rows :language :clojure :code "(fn [_] [])"
                 :inputs {:raw {:kind :dataset :dataset :raw_belege}}
                 :input {:kind :datasource :datasource :csv}
                 :output {:kind :object-type :object-type :belege :primary-key :id}}))))))

(deftest the-ent-grammar-accepts-a-clojure-transform
  (let [schema (ent/validate-schema!
                '{:transforms {:beleg-rows
                               {:language :clojure
                                :code "(fn [{:keys [raw]}] raw)"
                                :inputs {:raw {:kind :dataset :dataset :raw_belege}}
                                :output {:kind :object-type :object-type :belege}
                                :primary-key :id}}
                  :objects    {:belege {:attributes [[:id :string]] :plural "Belege"}}}
                "test")]
    (is (= :clojure (get-in schema [:transforms :beleg-rows :language]))))
  (testing "a code-file body is accepted too (the clone/push authoring form)"
    (is (ent/validate-schema!
         '{:transforms {:beleg-rows
                        {:language :clojure
                         :code-file "transforms/beleg_rows.clj"
                         :inputs {:raw {:kind :dataset :dataset :raw_belege}}
                         :output {:kind :object-type :object-type :belege}
                         :primary-key :id}}}
         "test"))))
