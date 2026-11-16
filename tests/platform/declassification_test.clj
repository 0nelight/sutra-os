(ns ^:unit platform.declassification-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.outputs :as outputs]
            [sutra.governance.merge :as merge]
            [sutra.parser.board :as board]
            [sutra.types.authority :as authority]
            [sutra.types.granular :as granular]
            [sutra.types.review :as review]))

(def ^:private schema
  {:organizations {"ri.o.acme" {:organization-name :acme :members ["acme-staff"]
                                :expand-access ["acme-security"]}
                   "ri.o.lief" {:organization-name :lief :members ["lief-staff"]}}
   :spaces        {"ri.s.acme"   {:space-name :acme-privat :organization :acme}
                   "ri.s.shared" {:space-name :gemeinsam :organization :acme
                                  :shared-with [:lief]}}
   :projects      {"ri.p.kunden" {:project-name :kunden :space "ri.s.acme"
                                  :markings [:pii]}
                   "ri.p.share"  {:project-name :share :space "ri.s.shared"}}
   :markings      {:pii {:remove-access ["dpo"]}}
   :datasets      {"ri.ds.kunde" {:dataset-name :kunde :project "ri.p.kunden"}
                   "ri.ds.roh"   {:dataset-name :roh :project "ri.p.share"}
                   "ri.ds.frei"  {:dataset-name :frei :project "ri.p.share"}}
   :transforms    {"ri.t.roh"  {:project "ri.p.share"
                                :inputs {:k {:kind :dataset :dataset "ri.ds.kunde"}}
                                :output {:kind :dataset :dataset "ri.ds.roh"}}
                   "ri.t.frei" {:project "ri.p.share"
                                :inputs {:k {:kind :dataset :dataset "ri.ds.kunde"
                                             :stop-requiring [:acme]
                                             :stop-propagating [:pii]}}
                                :output {:kind :dataset :dataset "ri.ds.frei"}}}})

(defn- db [schema subject]
  (authority/database (authority/schema-facts schema)
                      (authority/role-facts grants/standard-roles)
                      (authority/subject-facts subject)))

(def ^:private ben {:who "ben" :memberships [["ben" "lief-staff"]] :clears [:pii]})

(deftest an-organization-wall-travels-along-the-lineage
  (let [blocked (authority/blocked (db schema ben) "ben")]
    (is (contains? blocked "ri.ds.roh")
        "derived from acme's private data, in a space shared with lief: still acme's")
    (is (not (contains? blocked "ri.ds.frei"))
        "the input stops requiring acme: lief reads the output")
    (is (= :blocked/organization
           (get-in (authority/decide (db schema ben) {:who "ben" :do :read :on "ri.ds.roh"})
                   [:because :rule]))
        "a dataset is no container, but its block still explains itself")))

(deftest stop-requiring-a-part-of-a-wall-keeps-it
  (let [s (assoc-in schema [:transforms "ri.t.frei" :inputs :k :stop-requiring] [:lief])]
    (is (contains? (authority/blocked (db s ben) "ben") "ri.ds.frei")
        "lifting an organization the wall does not hold lifts nothing")))

(deftest stop-propagating-lifts-a-marking
  (let [m (into #{} (keep (fn [[x mk]] (when (= mk :pii) x)))
                (get (authority/schema-database (authority/schema-facts schema)) :marked))]
    (is (contains? m "ri.ds.roh"))
    (is (not (contains? m "ri.ds.frei")))))

(deftest lifting-is-an-expansion-answered-by-the-holder
  (let [base   #(authority/schema-database (authority/schema-facts %))
        before (assoc-in schema [:transforms "ri.t.frei" :inputs :k]
                         {:kind :dataset :dataset "ri.ds.kunde"})
        delta  (authority/access-delta (base before) (base schema))
        lifted (authority/lifted delta (base before))]
    (is (= #{:pii} (get-in lifted ["ri.ds.frei" :markings])))
    (is (contains? (get-in lifted ["ri.ds.frei" :organizations]) "ri.o.acme"))
    (testing "the holders of Remove marking / Expand access approve, not an owner"
      (let [t  {:kind :breaking :expands-access true
                :access-gates [{:gate "marking/pii" :holders ["dpo"]}
                               {:gate "organization/ri.o.acme" :holders ["acme-security"]}]}
            t  (assoc t :requires (review/requires review/default-rules t {:author "anna"}))
            ok (fn [ap] (review/eligible? t ap {:author "anna"}))]
        (is (not (ok {:who "anna" :owner? true :groups #{}})))
        (is (not (ok {:who "dpo" :groups #{}})) "both gates must be held")
        (is (ok {:who "dpo" :groups #{"acme-security"}}))))
    (testing "a gate nobody holds falls to an owner"
      (let [t {:kind :breaking :expands-access true
               :access-gates [{:gate "marking/x" :holders []}]}
            t (assoc t :requires (review/requires review/default-rules t {:author "x"}))]
        (is (review/eligible? t {:who "anna" :owner? true} {:author "x"}))
        (is (not (review/eligible? t {:who "anna"} {:author "x"})))))))

(deftest a-disjunctive-category-needs-one-marking
  (let [s  {:projects  {"ri.p.a" {:project-name :a}}
            :markings  {:rel-de {:category "release" :disjunctive true}
                        :rel-at {:category "release" :disjunctive true}
                        :pii    {}}
            :datasets  {"ri.ds.x" {:dataset-name :x :project "ri.p.a"
                                   :markings [:rel-de :rel-at :pii]}}}
        b? (fn [clears] (contains? (authority/blocked (db s {:who "u" :clears clears}) "u")
                                   "ri.ds.x"))]
    (is (not (b? [:rel-at :pii])) "one releasability suffices")
    (is (b? [:rel-at]) "a conjunctive marking still needs holding")
    (is (b? [:pii]) "none of the disjunctive set blocks")))

(deftest a-mandatory-control-property-gates-the-row
  (let [g   (granular/object-guard {:mandatory-controls {:marks :markings :orgs :organizations}}
                                   (granular/subject {:organizations ["ri.o.acme"]})
                                   #(= "pii" (name %)))
        ok? (:admit? g)]
    (is (ok? {:marks [] :orgs nil}) "an empty control restricts nothing")
    (is (not (ok? {:marks "pii, intern"})) "a denied marking hides the row")
    (is (ok? {:marks ["intern"] :orgs ["ri.o.lief" "ri.o.acme"]}))
    (is (not (ok? {:orgs ["ri.o.lief"]})) "outside every named organization")
    (is (seq (granular/security-problems {:mandatory-controls {:nope :markings}} #{:marks} :id)))))

(deftest a-control-secures-only-its-own-datasource
  (let [ctl {:marks :markings :lief-orgs :organizations}
        sec {:mandatory-controls (granular/datasource-scoped ctl [#{:lief-orgs :preis :rabatt}])}
        g   (granular/object-guard sec
                                   (granular/with-org-names
                                    (granular/subject {:organizations ["ri.o.acme"]})
                                    {"ri.o.acme" "acme"})
                                   #(= "pii" (name %)))
        row {:id 1 :marks [] :lief-orgs ["lief"] :preis 9 :rabatt 2 :name "x"}]
    (is (= {:kind :organizations :properties [:lief-orgs :preis :rabatt]}
           (get-in sec [:mandatory-controls :lief-orgs]))
        "a supplement's control is scoped to the properties it fills")
    (is (= :markings (get-in sec [:mandatory-controls :marks]))
        "the primary datasource's control keeps the whole object")
    (is ((:admit? g) row) "a failed supplement control leaves the object visible")
    (is (= {:id 1 :marks [] :name "x"} ((:redact g) row))
        "only the supplement's properties are blanked")
    (let [member (assoc row :lief-orgs ["ri.o.acme"])]
      (is (= member ((:redact g) member)) "a member sees the supplement's properties"))
    (is (not ((:admit? g) (assoc row :marks ["pii"]))) "the primary control hides it all")
    (is (= (:preis row) (:preis ((:redact g) (assoc row :lief-orgs "acme"))))
        "an organization named by its name passes like its rid")
    (is (seq (granular/security-problems
              {:mandatory-controls {:marks {:kind :markings :properties [:id]}}}
              #{:marks :id} :id))
        "a control never secures the primary key")))

(deftest a-cross-project-read-is-referenced
  (is (= [{:transform "ri.t.frei" :project "ri.p.share" :input "ri.ds.kunde"
           :from "ri.p.kunden"}
          {:transform "ri.t.roh" :project "ri.p.share" :input "ri.ds.kunde"
           :from "ri.p.kunden"}]
         (sort-by :transform (authority/unreferenced-inputs schema))))
  (is (empty? (authority/unreferenced-inputs
               (assoc-in schema [:projects "ri.p.share" :references] ["ri.ds.kunde"]))))
  (is (empty? (authority/unreferenced-inputs
               (assoc-in schema [:projects "ri.p.share" :imports] [:kunden])))))

(deftest a-required-classification-is-an-invariant
  (is (empty? (authority/unclassified-projects schema)) "nothing required, nothing found")
  (let [s (assoc schema :markings {:intern {:category "class" :rank 1
                                            :required-on-projects true}})]
    (is (= #{"ri.p.kunden" "ri.p.share"} (set (map :project (authority/unclassified-projects s)))))
    (is (= ["ri.p.kunden"]
           (map :project (authority/unclassified-projects
                          (assoc-in s [:spaces "ri.s.shared" :markings] [:intern])))))))

(deftest a-board-output-removes-markings-under-security-approvals
  (let [brd  {:inputs {:a {:kind :dataset :dataset :kunden :stop-propagating [:intern]}}
              :ops [{:op :from :input :a}]
              :remove-markings [:pii] :remove-organizations [:acme]}
        body (outputs/recompile-board {} {:transform-name :frei :board brd})]
    (is (= {:kind :dataset :dataset :kunden :stop-propagating [:intern :pii]
            :stop-requiring [:acme]}
           (get-in body [:inputs :a]))
        "an output's removal is every input's stop_propagating / stop_requiring")
    (is (= [:removal-without-security-approvals] (map :problem (board/security-problems brd)))
        "a removal needs the pipeline to allow changes to security markings")
    (is (empty? (board/security-problems (assoc brd :security-approvals true))))
    (is (empty? (board/security-problems (dissoc brd :remove-markings :remove-organizations)))
        "nothing removed, nothing to allow"))
  (testing "a pipeline allowing security changes stays on reviewed proposals"
    (is (policy/protected-row? {} {:rid "ri.t.frei" :security-approvals true}))
    (is (not (policy/protected-row? {} {:rid "ri.t.frei"})))))

(deftest declaring-a-stop-is-the-declassification
  (let [before (update schema :transforms dissoc "ri.t.frei")
        row    (some #(when (= "ri.t.frei" (:rid %)) %) (merge/touched-rows before schema))]
    (testing "a transform the change adds, stopping a wall and a marking, waits for their holders"
      (is (:expands-access row))
      (is (= #{{:gate "organization/ri.o.acme" :holders ["acme-security"]}
               {:gate "marking/pii" :holders ["dpo"]}}
             (set (:access-gates row)))))
    (testing "a transform reading without a stop lifts nothing"
      (let [plain (some #(when (= "ri.t.roh" (:rid %)) %)
                        (merge/touched-rows (update schema :transforms dissoc "ri.t.roh") schema))]
        (is (not (:access-gates plain)))))))
