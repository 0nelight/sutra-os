(ns ^:unit platform.authority-decide-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.grants :as grants]
            [sutra.types.authority :as authority]))

(def ^:private schema
  {:projects {"ri.p.lager"   {:grants [{:principal "anna" :role :ontology-owner}
                                       {:principal "lager-team" :role :ontology-editor}]}
              "ri.p.einkauf" {:grants [{:principal "alle" :role :ontology-viewer}]}}})

(defn- db [subject]
  (authority/database (authority/schema-facts schema)
                      (authority/role-facts grants/standard-roles)
                      (authority/subject-facts subject)))

(defn- may? [subject op on]
  (:allow? (authority/decide (db subject) {:who (:who subject) :do op :on on})))

(deftest a-grant-holds-on-its-project-only
  (is (may? {:who "anna"} :manage-permissions "ri.p.lager"))
  (is (not (may? {:who "anna"} :read "ri.p.einkauf")))
  (testing "a role includes the operations of the roles below it"
    (is (may? {:who "anna"} :read "ri.p.lager"))))

(deftest a-groups-grant-holds-for-its-members
  (is (may? {:who "ben" :memberships [["ben" "lager-team"]]} :update "ri.p.lager"))
  (is (not (may? {:who "ben" :memberships [["ben" "lager-team"]]} :manage-permissions "ri.p.lager"))
      "an editor is no owner")
  (is (not (may? {:who "ben"} :update "ri.p.lager")) "without the membership nothing"))

(deftest groups-in-groups-reach-through
  (let [s {:who "carl"
           :memberships [["carl" "nachtschicht"] ["nachtschicht" "lager-team"]
                         ["lager-team" "alle"]]}]
    (is (may? s :update "ri.p.lager"))
    (is (may? s :read "ri.p.einkauf"))))

(deftest a-grant-on-a-container-inherits-to-what-it-contains
  (let [facts (update (authority/schema-facts schema) :grant conj
                      ["revision" :ontology-viewer authority/default-space])
        d     (authority/database facts (authority/role-facts grants/standard-roles)
                                  (authority/subject-facts {:who "revision"}))]
    (is (:allow? (authority/decide d {:who "revision" :do :read :on "ri.p.lager"})))
    (is (:allow? (authority/decide d {:who "revision" :do :read :on "ri.p.einkauf"})))
    (is (not (:allow? (authority/decide d {:who "revision" :do :update :on "ri.p.lager"}))))))

(deftest a-global-role-and-no-restriction-hold-everywhere
  (is (may? {:who "dora" :global-role :ontology-editor} :update "ri.p.einkauf"))
  (is (not (may? {:who "dora" :global-role :ontology-editor} :manage-permissions "ri.p.einkauf")))
  (is (may? {:who "root" :unrestricted? true} :manage-permissions "ri.p.einkauf")))

(deftest every-answer-explains-itself
  (let [e (:because (authority/decide (db {:who "ben" :memberships [["ben" "lager-team"]]})
                                      {:who "ben" :do :read :on "ri.p.lager"}))
        facts (set (map :fact (tree-seq :from :from e)))]
    (is (= :may/granted (:rule e)))
    (is (contains? facts [:member-of ["ben" "lager-team"]]) "names the membership it rests on")
    (is (contains? facts [:grant ["lager-team" :ontology-editor "ri.p.lager"]]) "and the grant"))
  (is (nil? (:because (authority/decide (db {:who "ben"}) {:who "ben" :do :read :on "ri.p.lager"})))
      "what is not allowed has no derivation"))

(defn- old-project-grants
  [schema principal]
  (get (reduce-kv
        (fn [idx proj-rid spec]
          (reduce (fn [idx {:keys [principal role]}]
                    (let [ops (grants/role-operations role)]
                      (if (seq ops) (update-in idx [principal proj-rid] (fnil into #{}) ops) idx)))
                  idx (:grants spec)))
        {} (:projects schema))
       principal {}))

(def ^:private gen-schema
  (let [principals ["anna" "ben" "carl"]
        roles      (conj (vec (keys grants/standard-roles)) :unbekannt)]
    (gen/fmap (fn [ps] {:projects (into {} ps)})
              (gen/vector
               (gen/tuple (gen/fmap #(str "ri.p." %) gen/nat)
                          (gen/hash-map :grants
                                        (gen/vector
                                         (gen/hash-map :principal (gen/elements principals)
                                                       :role (gen/elements roles))
                                         0 4)))
               0 6))))

(deftest on-todays-data-it-answers-as-before
  (let [r (tc/quick-check 200
                          (prop/for-all [s gen-schema p
                                         (gen/elements ["anna" "ben" "carl" "niemand"])]
                                        (= (old-project-grants s p) (grants/project-grants s p))))]
    (is (:pass? r) (pr-str (:shrunk r)))))
