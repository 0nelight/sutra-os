(ns ^:unit apps.module-derived-test
  (:require [sutra.serve.errors :as errors]
            [sutra.parser.authoring.standard :as standard]
            [clojure.test :refer [deftest testing is]]
            [sutra.capps.derived :as cd]
            [sutra.types.expr :as expr]
            [sutra.actions.dispatch]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-emit :as pemit]
            [sutra.authoring.http.oms]
            [sutra.apps.http.files :as files]
            [sutra.apps.http.view :as view]))

(def ^:private with-props #'view/app-with-properties)

(defn- stored
  [a]
  (get-in (ent/parse-enta a "t") [:schema :applications :wartungsquoten]))

(def ^:private module
  {:arbeitspaket {:wartung {:kind :aggregation :over :buchungen :agg :sum :of :wartungs-std}
             :gesamt    {:kind :aggregation :over :buchungen :agg :sum :of :std}
             :quote     {:kind :column-math :expression [:div :wartung :gesamt]}}})

(def ^:private ctx
  {:props        {:arbeitspaket #{:paket-nr} :stundenbuchung #{:std :wartungs-std :jahr}}
   :links        {:buchungen {:from :arbeitspaket :to :stundenbuchung}}
   :object-types [:arbeitspaket]})

(def ^:private app
  {:name :wartungsquoten :object-types [:arbeitspaket] :derived-properties module
   :layout {:kind :rows
            :items [{:widget :object-table :rows {:from :arbeitspaket}
                     :cols [{:prop :quote}]}]}})

(deftest a-sound-module-is-sound
  (is (= [] (cd/explain module ctx))))

(deftest column-math-may-read-natives-and-aggregations
  (testing "eine Aggregation und eine eigene Eigenschaft"
    (is (= [] (cd/explain (cd/put module :arbeitspaket :pro-karte
                                  {:kind :column-math :expression [:div :gesamt :paket-nr]})
                          ctx))
        "one aggregation, one native property")))

(deftest column-math-may-not-read-column-math
  (testing "the rule nobody would guess, so it is enforced rather than documented"
    (let [probs (cd/explain (cd/put module :arbeitspaket :prozent
                                    {:kind :column-math :expression [:* :quote 100]})
                            ctx)]
      (is (= 1 (count probs)))
      (is (= :prozent (:name (first probs))))
      (is (re-find #"keine andere Spaltenrechnung" (:error (first probs)))))))

(deftest column-math-may-not-traverse
  (testing "a link is the OTHER kind, and the message says so"
    (let [probs (cd/explain (cd/put module :arbeitspaket :x
                                    {:kind :column-math
                                     :expression [:connected-sum :buchungen :std]})
                            ctx)]
      (is (re-find #"Aggregation" (:error (first probs)))))))

(deftest a-reference-nobody-has-is-refused-only-with-a-schema
  (let [m (cd/put module :arbeitspaket :x
                  {:kind :column-math :expression [:+ :paket-nr :erfunden]})]
    (is (seq (cd/explain m ctx)) "with the schema in hand, the typo is caught")
    (is (= [] (cd/explain m {:object-types [:arbeitspaket]}))
        "without it the check is SKIPPED, not guessed — a checkout is no source of truth")))

(deftest a-derived-name-may-not-shadow-a-real-property
  (is (re-find #"bereits eine Eigenschaft"
               (:error (first (cd/explain (cd/put module :arbeitspaket :paket-nr
                                                  {:kind :column-math :expression [:+ :paket-nr 1]})
                                          ctx))))))

(deftest an-aggregation-folds-the-far-types-properties
  (testing "eine Aggregation faltet Eigenschaften der Gegenseite"
    (is (re-find #"keine Eigenschaft von stundenbuchung"
                 (:error (first (cd/explain {:arbeitspaket {:x {:kind :aggregation :over :buchungen
                                                           :agg :sum :of :paket-nr}}}
                                            ctx)))))
    (is (= :stundenbuchung (cd/far-type (:links ctx) :arbeitspaket :buchungen)))
    (is (= :arbeitspaket (cd/far-type (:links ctx) :stundenbuchung :buchungen))
        "the far side is read from THIS side, so one table answers both directions")))

(deftest the-arity-rule-is-the-ent-doors-own
  (is (nil? (cd/rollup-arity-error {:agg :sum :of :std})))
  (is (some? (cd/rollup-arity-error {:agg :sum})))
  (is (some? (cd/rollup-arity-error {:agg :one :pick :x :where [:= :a 1]}))
      ":one takes no filter — its at-most-one promise is checked at declaration time"))

(deftest refs-agree-with-the-evaluators-own-reader
  (testing "sutra.capps.derived/expr-refs mirrors sutra.types.expr/field-refs for the arithmetic subset"
    (doseq [e [[:div :a :b] [:+ [:get :a] [:* :b 2]] [:abs [:- :a [:get :b]]] [:div 1 2]]]
      (is (= (expr/field-refs e) (cd/expr-refs e)) (pr-str e)))))

(deftest editing-keeps-the-module-consistent
  (testing "a rename drags the expressions that read the old name"
    (is (= [:div :wart :gesamt]
           (get-in (cd/rename module :arbeitspaket :wartung :wart)
                   [:arbeitspaket :quote :expression]))))
  (testing "dropping the last property drops the type"
    (is (= {} (-> {:a {:x {:kind :column-math :expression [:+ :p 1]}}}
                  (cd/drop-property :a :x)))))
  (testing "aggregations come first — a column math may depend on one"
    (is (= [:gesamt :wartung :quote] (mapv first (cd/properties module :arbeitspaket))))))

(deftest the-enta-door-folds-the-module-into-the-layout
  (testing "the server reads an app's scope from :layout, so the file's key folds in"
    (is (= module (get-in (ent/parse-enta app "t")
                          [:schema :applications :wartungsquoten :layout :derived-properties])))))

(deftest the-door-refuses-what-the-panel-refuses-and-names-it
  (let [bad (assoc-in app [:derived-properties :arbeitspaket :prozent]
                      {:kind :column-math :expression [:* :quote 100]})
        msg (try (ent/parse-enta bad "t") nil (catch Exception e (.getMessage e)))]
    (is (string? msg))
    (is (re-find #"arbeitspaket\.prozent" msg) "the message names the offending property")
    (is (re-find #"keine andere Spaltenrechnung" msg) "and repeats the rule it broke")))

(deftest the-door-refuses-a-mistyped-key
  (is (re-find #"expresion"
               (try (ent/parse-enta (assoc-in app
                                              [:derived-properties :arbeitspaket :quote :expresion]
                                              [:div :a :b])
                                    "t")
                    ""
                    (catch Exception e (.getMessage e))))))

(deftest the-file-round-trips
  (testing "what enta->schema folds IN, app-unit lifts back OUT — else the next
            projection returns the author's file with the key in the wrong place"
    (let [emit pemit/app-unit
          file (:doc (emit (assoc (stored app) :application-name :wartungsquoten
                                  :name :wartungsquoten) {:inline? true}))]
      (is (= module (:derived-properties file)))
      (is (nil? (get-in file [:layout :derived-properties]))))))

(deftest the-read-lowers-both-kinds-through-one-lowering
  (let [pairs (into {} (get (with-props (stored app) {}) :arbeitspaket))]
    (is (= [:connected-sum :buchungen :wartungs-std] (:wartung pairs))
        "identical to a .ent property rollup's AST")
    (is (= (sugar/rollup->expr {:over :buchungen :agg :sum :of :wartungs-std}) (:wartung pairs)))
    (is (= [:div :wartung :gesamt] (:quote pairs))
        "a column math IS a sutra.types.expr AST — the grammar narrows it, it does not translate")))

(deftest the-read-orders-a-column-math-after-what-it-reads
  (testing "not by declaration order but by dependency — sutra.types.expr/eval-order"
    (let [pairs (get (with-props (stored app) {}) :arbeitspaket)
          order (mapv first (expr/eval-order pairs))]
      (is (< (.indexOf order :wartung) (.indexOf order :quote)))
      (is (< (.indexOf order :gesamt) (.indexOf order :quote))))))

(deftest a-module-property-and-a-var-property-share-one-type
  (let [a (assoc-in (stored app) [:layout :vars]
                    {:rows {:source :arbeitspaket
                            :with {:mx {:over :buchungen :agg :max :of :std}}}})]
    (is (= #{:wartung :gesamt :quote :mx}
           (set (map first (get (with-props a {}) :arbeitspaket))))
        "two authoring sites, one result map")))

(deftest an-unresolved-filter-drops-that-property
  (let [a (assoc-in (stored app)
                    [:layout :derived-properties :arbeitspaket :wartung :where]
                    [:= :jahr {:val :jahr}])]
    (is (= [:connected-sum :buchungen :wartungs-std [:= :jahr 2026]]
           (:wartung (into {} (get (with-props a {:jahr 2026}) :arbeitspaket)))))
    (is (nil? (:wartung (into {} (get (with-props a {}) :arbeitspaket))))
        "a filter with a hole computes a confidently wrong number, so it is not computed")))

(deftest save-goes-through-the-enta-door-and-only-stages
  (let [seen (atom nil)]
    (with-redefs [sutra.authoring.http.oms/install-preset
                    (fn [req] (reset! seen (:body-params req)) {:status 200})]
      (testing "a good file is lowered by parse-enta and handed to the install door"
        (let [resp ((errors/wrap-exceptions (var-get #'files/save-application-file))
                    {:path-params {:name "wartungsquoten"} :body-params {:file app}})]
          (is (= 200 (:status resp)))
          (is (= module
                 (get-in @seen [:schema :applications :wartungsquoten :layout :derived-properties]))
              "what reaches staging is the LOWERED schema — module keys folded into :layout")))
      (testing "a refused file never reaches staging, and carries the door's own sentence"
        (reset! seen nil)
        (let [bad  (assoc-in app [:derived-properties :arbeitspaket :prozent]
                             {:kind :column-math :expression [:* :quote 100]})
              resp ((errors/wrap-exceptions (var-get #'files/save-application-file))
                    {:path-params {:name "wartungsquoten"} :body-params {:file bad}})]
          (is (= 400 (:status resp)))
          (is (re-find #"keine andere Spaltenrechnung" (str (get-in resp [:body :error]))))
          (is (nil? @seen) "nothing was staged"))))))

(deftest the-builder-reads-the-file-the-exporter-writes
  (testing "GET returns the app's own .enta — not the visibility-pruned consumer layout"
    (let [spec (assoc (stored app) :application-name :wartungsquoten)
          resp ((var-get #'files/application-file)
                {:path-params {:name "wartungsquoten"}
                  :sutra/schema {:applications {"ri.app.1" spec}
                                 :projections standard/projections}
                  :sutra/branch "main"})
          file (get-in resp [:body :file])]
      (is (= module (:derived-properties file)) "module scope is at FILE level")
      (is (nil? (get-in file [:layout :derived-properties])))
      (is (= (:layout (stored app))
             (assoc (:layout file)
                    :object-types (:object-types file)
                    :derived-properties (:derived-properties file)))
          "the fold and the lift are inverses — what is read back re-installs identically"))))
