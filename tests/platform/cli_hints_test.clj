(ns ^:unit platform.cli-hints-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lineage.graph :as lineage]
            [sutra.cli.out :as out]
            [sutra.cli.hints :as hints]
            [sutra.i18n.catalog :as catalog]
            [sutra.cli.parse :as parse]
            [sutra.cli.session :as session]))

(deftest fields-projection-narrows-any-result
  (let [proj out/project-fields
        m {:branch "main" :counts {:objects 3 :links 5}
           :by-type {:a 1 :b 2} :sections {:actions ["x"] :datasets ["y"]}}]
    (testing "top-level fields select-key the result"
      (is (= {:by-type {:a 1 :b 2}} (proj ["by-type"] m)))
      (is (= {:branch "main" :counts {:objects 3 :links 5}}
             (proj ["branch" "counts"] m))))
    (testing "a dotted path keeps the subtree's STRUCTURE (composably)"
      (is (= {:sections {:actions ["x"]}}
             (proj ["sections.actions"] m))))
    (testing "paths absent from the result contribute nothing"
      (is (= {:branch "main"} (proj ["branch" "nope.missing"] m))))
    (testing "no fields pass the result through untouched"
      (is (= m (proj nil m)))
      (is (= m (proj [] m))))
    (testing "a non-map result (a bare vector) is never projected"
      (is (= [1 2 3] (proj ["x"] [1 2 3]))))))

(deftest dependents-of-shape-and-impact-are-stable
  (let [f    (fn [body _rid nm] (lineage/dependents body nm))
        body {:object-types {:ri.ot.widget {:object-type-name :widget :plural "Widgets"}
                             :ri.ot.gadget {:object-type-name :gadget :plural "Gadgets"}
                             :ri.ot.lonely {:object-type-name :lonely :plural "Lonelies"}}
              :link-types   {:ri.lt.wg {:from-object-type :ri.ot.widget
                                        :to-object-type   :ri.ot.gadget
                                        :link-type-name   :widget-gadget
                                        :cardinality      :many-to-one}}
              :transforms   {:ri.tf.b {:transform-name :build-widget
                                       :inputs {:in {:kind :object-type :object-type :ri.ot.widget}}
                                       :output {:kind :object-type :object-type :ri.ot.gadget}}}
              :actions      {:custom-widget-thing {:action-name :custom-widget-thing
                                                   :edits [{:object-type :ri.ot.widget}]
                                                   :rules []}}
              :applications {:ri.app.w {:application-name :widget-app
                                        :object-type :ri.ot.widget}}}
        r    (f body :ri.ot.widget "widget")]
    (testing "the result shape is stable"
      (is (= #{:object-type :rid :links :transforms :actions :apps :total} (set (keys r)))))
    (testing "each kind of dependent is found, reads/writes distinguished, :total sums them"
      (is (= 1 (count (:links r))))
      (is (= 1 (count (:transforms r))))
      (is (true? (:reads (first (:transforms r)))) "the transform READS widget (input), not writes")
      (is (= 1 (count (:actions r))))
      (is (= 1 (count (:apps r))))
      (is (= 4 (:total r))))
    (testing "an object-type nothing references has zero dependents (safe to change)"
      (is (= 0 (:total (f body :ri.ot.lonely "lonely")))))))

(deftest query-show-flag-accumulates
  (let [step (first (:steps (parse/line->cmds
                             (session/new-session {})
                             "query bestellung --show status --show lieferant --show nr")))]
    (is (= [:query {:find :bestellung :count? true
                    :show [:status :lieferant :nr]}] step)
        "repeated --show accumulates all columns instead of keeping only the last")))

(deftest query-order-parses-direction
  (let [sess (session/new-session {})
        step (fn [line] (first (:steps (parse/line->cmds sess line))))]
    (testing "bare column defaults to ascending"
      (is (= [:query {:find :email :count? true :order-by :gesendet-am :sort-dir :asc}]
             (step "query email --order gesendet-am"))))
    (testing "col:desc splits into :order-by + :sort-dir"
      (is (= [:query {:find :email :count? true :order-by :gesendet-am :sort-dir :desc}]
             (step "query email --order gesendet-am:desc"))))
    (testing "a mistyped direction is rejected at parse time (no bogus-column 500)"
      (is (thrown? clojure.lang.ExceptionInfo (step "query email --order gesendet-am:sideways"))))))

(deftest every-bundled-hints-registry-holds-its-schema
  (doseq [l catalog/locales]
    (is (nil? (hints/problems l)) (str "hints registry of " l))))
