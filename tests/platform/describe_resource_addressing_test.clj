(ns ^:unit platform.describe-resource-addressing-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.view :as view]))

(defn- view [body sections token]
  (#'view/describe-token-view body sections nil token {}))
(def ^:private ->json #'view/describe-token->json)

(def ^:private body
  {:projects     {"ri.sutra.main.project.1"    {:project-name :controlling}}
   :repositories {"ri.sutra.main.repository.1" {:repository-name :controlling
                                                :type :transforms
                                                :project "ri.sutra.main.project.1"}
                  "ri.sutra.main.repository.2" {:repository-name :etl}}
   :actions      {:angebot-anlegen {:display-name "Angebot anlegen"
                                    :params {:datum {:type :date}}
                                    :status :active}}
   :object-types {}})

(def ^:private sections #{:projects :repositories :object-types :actions})

(deftest a-rid-addresses-its-resource-whatever-the-kind
  (let [v (view body sections (keyword "ri.sutra.main.repository.1"))]
    (is (= :resource (:kind v)))
    (is (= "repositories" (:kind (:view v))))
    (is (= "controlling" (:name (:view v))))
    (is (= "ri.sutra.main.repository.1" (:rid (:view v))))
    (testing "and the owning project rides along — containment is the permission
              fact, and it is the one thing a resource view must not hide"
      (is (= "ri.sutra.main.project.1" (:project (:view v)))))))

(deftest a-name-two-kinds-share-is-refused-with-both-rids
  (let [v (view body sections :controlling)]
    (is (= :ambiguous (:kind v)))
    (is (= #{"repositories" "projects"} (set (map :kind (:view v)))))
    (is (= #{"ri.sutra.main.repository.1" "ri.sutra.main.project.1"}
           (set (map :rid (:view v))))
        "both addresses are handed over — that is what makes the refusal useful
         rather than merely correct")
    (testing "and --json says the same thing"
      (is (= "ambiguous" (:error (->json v)))))))

(deftest a-name-only-one-kind-carries-resolves
  (let [v (view body sections :etl)]
    (is (= :resource (:kind v)))
    (is (= "repositories" (:kind (:view v))))
    (is (= "ri.sutra.main.repository.2" (:rid (:view v))))))

(deftest a-rid-nothing-holds-is-unknown
  (is (= :unknown (:kind (view body sections (keyword "ri.sutra.main.repository.9")))))
  (testing "a token that merely starts like a rid does not become one"
    (is (= :unknown (:kind (view body sections :ri.nonsense))))))

(deftest the-section-listing-still-answers-by-name
  (testing "`describe repositories` printed two RIDs, because `entity-names` fell
            back to the map key for the one section whose name field it did not
            know"
    (is (= ["controlling" "etl"] (:names (view body sections :repositories))))))

(deftest an-action-type-resolves-by-its-own-key
  (testing "`ontology action-type show <name>` answered {:error unknown} about 91
            live constructs: the resolver scanned name FIELDS, and the actions
            section carries none — it is keyed by the name itself"
    (let [v (view body sections :angebot-anlegen)]
      (is (= :action-type (:kind v)))
      (is (= "angebot-anlegen" (:name (:view v))))
      (is (= "Angebot anlegen" (:display-name (:view v))))
      (is (= {:datum {:type :date}} (:params (:view v))))))
  (testing "it has no RID, so the view carries no :rid key at all — a blank one
            reads as a missing value rather than none"
    (is (not (contains? (:view (view body sections :angebot-anlegen)) :rid))))
  (testing "and --json hands back the spec, not an error"
    (is (nil? (:error (->json (view body sections :angebot-anlegen)))))))

(deftest a-name-keyed-section-takes-part-in-the-ambiguity-refusal
  (testing "a name carried by BOTH a RID-keyed and a name-keyed section must
            still be refused — the resolver may not silently pick whichever it
            scans first just because one of the two has no RID to show"
    (let [b (assoc-in body [:actions :etl] {:display-name "ETL"})
          v (view b sections :etl)]
      (is (= :ambiguous (:kind v)))
      (is (= #{"repositories" "actions"} (set (map :kind (:view v)))))
      (testing "and the one address that exists is still handed over"
        (is (= #{"ri.sutra.main.repository.2" nil}
               (set (map :rid (:view v)))))))))

(deftest a-name-no-section-carries-is-still-unknown
  (is (= :unknown (:kind (view body sections :gibt-es-nicht)))))
