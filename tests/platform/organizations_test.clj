(ns ^:unit platform.organizations-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.product :as product]
            [sutra.catalog.grants :as grants]
            [sutra.types.authority :as authority]
            [sutra.types.directory :as directory]
            [sutra.types.schema :as sch]))

(def ^:private schema
  {:organizations {"ri.o.acme" {:organization-name :acme :members ["acme-staff"]}
                   "ri.o.lief" {:organization-name :lief :members ["lief-staff"]}}
   :spaces        {"ri.s.flug" {:space-name :flug :organization :acme
                                :grants [{:principal "flug-team" :role :ontology-viewer}]}}
   :projects      {"ri.p.wartung" {:project-name :wartung :space "ri.s.flug"
                                   :grants [{:principal "anna" :role :ontology-editor}
                                            {:principal "lief-ben" :role :ontology-viewer}]}
                   "ri.p.einkauf" {:project-name :einkauf :space :flug :shared-with [:lief]
                                   :grants [{:principal "lief-ben" :role :ontology-viewer}]}
                   "ri.p.solo"    {:project-name :solo
                                   :grants [{:principal "ohne" :role :ontology-viewer}]}}
   :object-types  {"ri.t.mangel" {:project "ri.p.wartung"}}})

(defn- db
  ([subject] (db schema subject))
  ([schema subject]
   (authority/database (authority/schema-facts schema)
                       (authority/role-facts grants/standard-roles)
                       (authority/subject-facts subject))))

(defn- may? [subject op on]
  (:allow? (authority/decide (db subject) {:who (:who subject) :do op :on on})))

(def ^:private anna {:who "anna" :memberships [["anna" "acme-staff"]]})

(deftest an-organization-walls-what-stands-in-it
  (is (may? anna :update "ri.p.wartung"))
  (testing "a grant without membership of the organization is blocked"
    (is (not (may? {:who "anna"} :update "ri.p.wartung")))
    (is (contains? (authority/blocked (db {:who "anna"}) "anna") "ri.t.mangel")
        "the wall reaches the resources in the project")
    (is (= :blocked/organization
           (get-in (authority/decide (db {:who "anna"}) {:who "anna" :do :read :on "ri.p.wartung"})
                   [:because :rule]))))
  (testing "a directory may supply the organization edge instead of a declared member"
    (is (may? {:who "anna" :organizations [["anna" "ri.o.acme"]]} :update "ri.p.wartung"))))

(deftest sharing-admits-a-second-organization
  (let [ben {:who "lief-ben" :memberships [["lief-ben" "lief-staff"]]}]
    (is (may? ben :read "ri.p.einkauf") "shared into lief")
    (is (not (may? ben :read "ri.p.wartung"))
        "not shared: the grant alone does not pass the wall")))

(deftest the-default-organization-is-no-wall
  (is (may? {:who "ohne"} :read "ri.p.solo"))
  (is (= [:space/default :org/default] (rest (authority/container-chain schema "ri.p.solo")))))

(deftest a-space-grant-inherits-to-its-projects
  (is (may? {:who "flug-team" :memberships [["flug-team" "acme-staff"]]} :read "ri.p.einkauf"))
  (is (= ["ri.p.wartung" "ri.s.flug" "ri.o.acme"] (authority/container-chain schema :wartung))))

(deftest a-disabled-principal-is-blocked-everywhere
  (is (not (may? (assoc anna :disabled? true) :read "ri.p.wartung")))
  (is (not (may? {:who "root" :unrestricted? true :disabled? true} :read "ri.p.solo"))
      "even nothing-restricts-them yields to offboarding"))

(deftest access-delta-names-what-widens
  (let [base     #(authority/schema-database (authority/schema-facts %)
                                             (authority/role-facts grants/standard-roles))
        before   (base schema)
        shared   (base (assoc-in schema [:projects "ri.p.wartung" :shared-with] [:lief]))
        unwalled (base (assoc-in schema [:spaces "ri.s.flug" :organization] nil))
        granted  (base (update-in schema [:projects "ri.p.solo" :grants] conj
                                  {:principal "x" :role :ontology-owner}))]
    (is (some #(= ["ri.p.wartung" ["ri.o.acme" "ri.o.lief"]] (:fact %))
              (:expands (authority/access-delta before shared))))
    (is (empty? (:restricts (authority/access-delta before shared))))
    (is (some #(= :removed (:change %)) (:expands (authority/access-delta before unwalled))))
    (is (some #(= :added (:change %)) (:restricts (authority/access-delta unwalled before))))
    (is (some #(= ["x" :manage-permissions "ri.p.solo"] (:fact %))
              (:expands (authority/access-delta before granted))))))

(deftest a-directory-reconciles-to-a-plan
  (let [current  {:users  {"anna" {:attributes {:region "wien"} :groups #{"acme-staff"}}
                           "carl" {:groups #{"acme-staff"}}}
                  :groups {"acme-staff" {} "alt" {}}}
        snapshot {:users  {"anna" {:attributes {:region "graz"} :groups #{"acme-staff"}}
                           "dora" {:groups #{"acme-staff"}}}
                  :groups {"acme-staff" {}}}
        plan     (directory/reconcile snapshot current)]
    (is (= #{"dora"} (set (keys (get-in plan [:users :add])))))
    (is (= {:region "graz"} (get-in plan [:users :change "anna" :to :attributes])))
    (is (= #{"carl"} (get-in plan [:users :disable]))
        "gone from the directory: disabled, not deleted")
    (is (= #{"alt"} (get-in plan [:groups :retire])))
    (is (= #{[:user "dora" "acme-staff"]} (get-in plan [:edges :add])))
    (is (= #{[:user "carl" "acme-staff"]} (get-in plan [:edges :remove])))
    (is (:refused (directory/reconcile {:users {}} current)) "an empty directory disables no one")))

(def ^:private ot  "ri.sutra.main.object-type.11111111-1111-1111-1111-111111111111")
(def ^:private ds  "ri.sutra.main.dataset.22222222-2222-2222-2222-222222222222")
(def ^:private prj "ri.sutra.main.project.33333333-3333-3333-3333-333333333333")
(def ^:private ext "ri.sutra.main.dataset.44444444-4444-4444-4444-444444444444")

(def ^:private source
  {:projects     {prj {:project-name :controlling
                       :grants [{:principal "anna" :role :ontology-owner}]}}
   :object-types {ot {:object-type-name :kunde :project prj :dataset ext :plural "Kunden"}}
   :datasets     {ds {:dataset-name :kunden_raw :project prj}
                  ext {:dataset-name :erp_kunden :project "ri.sutra.main.project.other"}}})

(deftest a-product-packages-installs-and-upgrades
  (let [b1 (product/package source
                            {:product :controlling :version "1.0.0" :projects [:controlling]})]
    (is (= #{prj} (set (keys (get-in b1 [:resources :projects])))))
    (is (nil? (get-in b1 [:resources :projects prj :grants])) "grants never travel")
    (is (contains? (:inputs b1) ext) "a dataset of another project is an input")
    (let [opts   {:install-id "kunde-b" :space "ri.s.flug" :bindings {}}
          target
            {:datasets
               {"ri.sutra.main.dataset.55555555-5555-5555-5555-555555555555" {:dataset-name :erp}}}]
      (is (= [[:unbound-input ext]]
             (map (juxt :problem :input) (:problems (product/install-plan b1 target opts)))))
      (let [bound (assoc opts
                         :bindings
                           {ext "ri.sutra.main.dataset.55555555-5555-5555-5555-555555555555"})
            plan  (product/install-plan b1 target bound)
            ot'   (product/install-rid "kunde-b" ot)]
        (is (empty? (:problems plan)))
        (is (= plan (product/install-plan b1 target bound)) "reinstalling plans the same")
        (is (= (get-in bound [:bindings ext]) (get-in plan [:schema :object-types ot' :dataset])))
        (is (= "ri.s.flug"
               (get-in plan [:schema :projects (product/install-rid "kunde-b" prj) :space])))
        (testing "an upgrade keeps the target's own change and takes the release's"
          (let [installed (assoc-in (:schema plan) [:object-types ot' :description] "lokal")
                b2        (assoc-in b1 [:resources :datasets ds :description] "neu")
                {:keys [body conflicts]} (product/upgrade b1 b2 installed bound)]
            (is (empty? conflicts))
            (is (= "lokal" (get-in body [:object-types ot' :description])))
            (is (= "neu"
                   (get-in body [:datasets (product/install-rid "kunde-b" ds) :description])))))))))

(deftest releases-move-along-channels
  (is (= :test (product/next-channel {:channel :dev})))
  (is (nil? (product/next-channel {:channel :stable}))))

(deftest expanding-names-what-a-change-widens
  (let [base #(authority/schema-database (authority/schema-facts %)
                                         (authority/role-facts grants/standard-roles))]
    (testing "a grant is discretionary: no expansion"
      (is (empty? (:subjects
                   (authority/expanding
                    (authority/access-delta (base schema)
                                            (base
                                             (update-in schema [:projects "ri.p.solo" :grants] conj
                                                        {:principal "x"
                                                         :role :ontology-viewer}))))))))
    (testing "a marking removed names the resource it uncovers"
      (is (contains? (:subjects (authority/expanding
                                 (authority/access-delta
                                  (base (assoc-in schema [:projects "ri.p.solo" :markings]
                                                  [:geheim]))
                                  (base schema))))
                     "ri.p.solo")))
    (testing "an organization wall opened names what stood behind it"
      (is (contains?
           (:subjects (authority/expanding
                       (authority/access-delta (base schema)
                                               (base
                                                (assoc-in schema
                                                          [:projects "ri.p.wartung" :shared-with]
                                                          [:lief])))))
           "ri.p.wartung")))
    (testing "a member added to an organization names no resource of its own"
      (is (:unattributed?
           (authority/expanding
            (authority/access-delta (base schema)
                                    (base
                                     (update-in schema [:organizations "ri.o.acme" :members] conj
                                                "neu")))))))
    (testing "adding a marking widens nothing"
      (is (empty? (:subjects
                   (authority/expanding
                    (authority/access-delta (base schema)
                                            (base
                                             (assoc-in schema [:projects "ri.p.solo" :markings]
                                                       [:geheim]))))))))))

(deftest a-security-text-declares-markings-and-roles
  (let [s (-> schema
              (assoc :markings {:geheim {:category "vertraulich" :rank 2 :cleared ["acme-staff"]}
                                :intern {:category "vertraulich" :rank 1}})
              (assoc-in [:projects "ri.p.solo" :markings] [:intern])
              (assoc :roles {:auditor {:operations [:read]}})
              (update-in [:projects "ri.p.solo" :grants] conj {:principal "rita" :role :auditor}))
        db (fn [subject] (authority/database (authority/schema-facts s)
                                             (authority/role-facts (grants/roles s))
                                             (authority/subject-facts subject)))
        may? (fn [subject op on]
               (:allow? (authority/decide (db subject) {:who (:who subject) :do op :on on})))]
    (testing "a group the marking clears covers it, and by rank the lower one of its category"
      (is (may? {:who "ohne" :memberships [["ohne" "acme-staff"]]} :read "ri.p.solo"))
      (is (not (may? {:who "ohne"} :read "ri.p.solo")) "outside the group: blocked"))
    (testing "a declared role grants its operations"
      (is (may? {:who "rita" :memberships [["rita" "acme-staff"]]} :read "ri.p.solo"))
      (is (not (may? {:who "rita" :memberships [["rita" "acme-staff"]]} :update "ri.p.solo"))))))

(deftest names-are-scoped-by-space
  (let [s {:spaces       {"ri.s.flug" {:space-name :flug} "ri.s.ctl" {:space-name :controlling}}
           :projects     {"ri.p.w" {:project-name :wartung :space "ri.s.flug"}
                          "ri.p.k" {:project-name :kosten :space "ri.s.ctl"}}
           :object-types
             {"ri.sutra.main.object-type.1" {:object-type-name :mangel :project "ri.p.w"
                                             :plural "Mängel"}
              "ri.sutra.main.object-type.2" {:object-type-name :mangel :project "ri.p.k"
                                             :plural "Mängel"}
              "ri.sutra.main.object-type.3" {:object-type-name :ort :plural "Orte"}}}]
    (testing "a name two spaces give is ambiguous without a scope, and says where it is"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"several spaces"
                            (sch/rid-of (sch/index s) :object-types :mangel))))
    (testing "in a space it is that space's"
      (is (= "ri.sutra.main.object-type.1"
             (sch/rid-of (sch/index (sch/in-scope s "ri.s.flug")) :object-types :mangel)))
      (is (= "ri.sutra.main.object-type.2"
             (sch/rid-of (sch/index (sch/in-scope s "ri.s.ctl")) :object-types :mangel))))
    (testing "a name only one resource has resolves everywhere"
      (is (= "ri.sutra.main.object-type.3" (sch/rid-of (sch/index s) :object-types :ort))))
    (testing "the scoped view is one value per space"
      (let [s (sch/indexed s)]
        (is (identical? (sch/in-scope s "ri.s.flug") (sch/in-scope s "ri.s.flug")))))))
