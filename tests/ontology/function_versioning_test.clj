(ns ^:unit ontology.function-versioning-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.semver :as semver]
            [sutra.catalog.function-contract :as contract]
            [sutra.catalog.functions :as functions]
            [sutra.types.versions :as vers]
            [sutra.parser.spec :as spec]
            [sutra.parser.project-emit :as pemit]
            [sutra.authoring.install :as install]))

(defn- src
  [tag]
  (str "(defn escalate [t] (-> OntologyClient/action (.update-ticket t {:stand \""
       tag "\"})))"))

(deftest a-version-is-parsed-or-it-is-not-a-version
  (is (semver/valid? "1.2.3"))
  (is (semver/valid? "0.0.1"))
  (is (semver/valid? "1.2.3-rc.1"))
  (is (not (semver/valid? "1.2")) "a two-part version is not SemVer")
  (is (not (semver/valid? "v1.2.3")) "no tag prefixes")
  (is (not (semver/valid? "01.2.3")) "no leading zeroes")
  (is (not (semver/valid? nil))))

(deftest versions-order-numerically-not-lexically
  (is (neg? (semver/compare-versions "1.9.0" "1.10.0"))
      "the string comparison says the opposite, which is the entire point")
  (is (neg? (semver/compare-versions "1.0.0" "2.0.0")))
  (is (zero? (semver/compare-versions "1.0.0" "1.0.0")))
  (is (= "1.10.0" (semver/newest ["1.9.0" "1.10.0" "1.2.0"]))))

(deftest a-prerelease-precedes-its-release
  (is (neg? (semver/compare-versions "1.0.0-rc.1" "1.0.0")))
  (is (neg? (semver/compare-versions "1.0.0-alpha" "1.0.0-beta")))
  (is (neg? (semver/compare-versions "1.0.0-rc.1" "1.0.0-rc.2")))
  (is (semver/prerelease? "1.0.0-rc.1"))
  (is (not (semver/prerelease? "1.0.0"))))

(deftest a-pin-resolves-to-itself-and-an-auto-upgrade-to-the-band
  (let [available ["1.0.0" "1.1.0" "1.2.0" "2.0.0" "1.3.0-rc.1"]]
    (testing "pinned"
      (is (= "1.1.0" (semver/max-satisfying available {:version "1.1.0"})))
      (is (nil? (semver/max-satisfying available {:version "1.9.9"}))
          "a pin on an unpublished version resolves to nothing — it does not fall back"))
    (testing "auto-upgrade"
      (is (= "1.2.0" (semver/max-satisfying available {:version "1.0.0" :auto-upgrade true}))
          "the maximum satisfying release, and NOT across the major")
      (is (= "2.0.0" (semver/max-satisfying available {:version "2.0.0" :auto-upgrade true})))
      (is (nil? (semver/max-satisfying available {:version "3.0.0" :auto-upgrade true})))
      (is (not (contains? (set (filter #(semver/satisfies-range?
                                         {:version "1.0.0" :auto-upgrade true} %)
                                       available))
                          "1.3.0-rc.1"))
          "a prerelease is reachable only by pinning it: the publisher marked it unstable"))))

(defn- dep-problem [step] (spec/function-dep-problem step))

(deftest a-version-dependency-must-be-authorable
  (is (nil? (dep-problem {:op :apply-edit-function :function :f})))
  (is (nil? (dep-problem {:op :apply-edit-function :function :f :version "1.0.0"})))
  (is (nil? (dep-problem {:op :apply-edit-function :function :f
                          :version "1.0.0" :auto-upgrade true})))
  (testing "a version that is not a version"
    (is (re-find #"not a semantic version"
                 (dep-problem {:op :apply-edit-function :function :f :version "latest"}))))
  (testing "auto-upgrade needs a minimum to upgrade FROM"
    (is (re-find #"without a :version"
                 (dep-problem {:op :invoke-function :function :f :auto-upgrade true}))))
  (testing "auto-upgrade is refused under initial development"
    (is (re-find #"0\.y\.z"
                 (dep-problem {:op :apply-edit-function :function :f
                               :version "0.4.0" :auto-upgrade true})))
    (is (nil? (dep-problem {:op :apply-edit-function :function :f :version "0.4.0"}))
        "pinning a 0.y.z version is fine — only auto-upgrading into one is not")))

(deftest both-function-consuming-rules-carry-the-dependency
  (is (= #{:apply-edit-function :invoke-function} spec/function-dep-ops)
      "one Function primitive, so one dependency shape at both call sites")
  (is (= [{:op :apply-edit-function :function :escalate :version "1.0.0"}]
         (spec/function-deps {:rules [{:op :apply-edit-function :function :escalate
                                       :version "1.0.0"}]}))))

(def ^:private live
  {:functions
   {"ri.w.1" {:function-name :escalate
              :target-type   :ticket
              :language      :clojure
              :source        (src "UNRELEASED")
              :version       "1.1.0"
              :project       :ops
              :releases      {"1.0.0" {:source (src "V1") :language :clojure
                                        :target-type :ticket}
                              "1.1.0" {:source (src "V11") :language :clojure
                                        :target-type :ticket}}}}})

(deftest an-unversioned-consumer-runs-the-live-source
  (let [{:keys [spec version]} (functions/function-at-version live :escalate {})]
    (is (= (src "UNRELEASED") (:source spec)))
    (is (= "1.1.0" version) "…and reports what the source is published as")))

(deftest a-pin-runs-the-pinned-release-not-the-source
  (let [{:keys [spec version]} (functions/function-at-version live :escalate {:version "1.0.0"})]
    (is (= (src "V1") (:source spec)) "the whole promise of a pin")
    (is (= "1.0.0" version))
    (is (= :ops (:project spec))
        "ownership stays LIVE — a release captures code, not permissions")))

(deftest an-auto-upgrade-resolves-to-the-newest-compatible-release
  (let [{:keys [spec version]} (functions/function-at-version
                                live :escalate {:version "1.0.0" :auto-upgrade true})]
    (is (= "1.1.0" version))
    (is (= (src "V11") (:source spec)))))

(deftest an-unsatisfiable-dependency-says-what-is-available
  (let [{:keys [unsatisfied
                available]} (functions/function-at-version live :escalate {:version "9.9.9"})]
    (is (some? unsatisfied) "a typo'd pin resolves to nothing rather than silently to the source")
    (is (= ["1.0.0" "1.1.0"] available))))

(deftest an-unknown-function-is-not-this-checks-problem
  (is (nil? (functions/function-at-version live :nope {:version "1.0.0"}))
      "unknown-name has its own refusal, which can say which lifecycle was meant"))

(deftest a-dangling-pin-is-refused-at-the-door-not-at-the-first-submission
  (let [action {:rules [{:op :apply-edit-function :function :escalate :version "9.9.9"}]
                :edits [{:object-type :ticket}]}]
    (is (seq (functions/unresolvable-function-deps live action)))
    (is (re-find #"no release satisfies"
                 (first (functions/unresolvable-function-deps live action)))))
  (testing "a satisfiable one passes"
    (is (empty? (functions/unresolvable-function-deps
                 live {:rules [{:op :apply-edit-function :function :escalate :version "1.0.0"}]
                       :edits [{:object-type :ticket}]})))))

(deftest a-release-captures-code-and-not-the-resource
  (let [snap (vers/snapshot {:function-name :escalate :source (src "S") :language :clojure
                                     :project :ops :repository :r :version "1.0.0"
                                     :releases {"0.1.0" {}} :param-types {:t :ticket}})]
    (is (= {:source (src "S") :language :clojure :param-types {:t :ticket}} snap)
        "moving a function between projects must not mint a false release")))

(deftest a-signature-break-is-named
  (let [old {:params {:ticket :ticket :note :string} :target-type :ticket}]
    (is (empty? (contract/breaks old old)))
    (is (= ["input :note was dropped"]
           (contract/breaks old {:params {:ticket :ticket} :target-type :ticket})))
    (is (re-find #"changed type"
                 (first (contract/breaks
                         old {:params {:ticket :ticket :note :integer} :target-type :ticket}))))
    (is (empty? (contract/breaks
                 old {:params {:ticket :ticket :note :string :extra :string}
                      :target-type :ticket}))
        "an ADDED input is backward compatible here: Sutra-OS binds inputs by name and
         an unbound parameter simply stays absent")))

(defn- fn-schema [entry]
  {:functions {:escalate (merge {:target-type :ticket :language :clojure} entry)}})

(defn- live-fn [entry]
  {:functions {"ri.w.1" (merge {:function-name :escalate :target-type :ticket
                                :language :clojure} entry)}})

(defn- publish
  [desired-entry live-entry]
  (->> (install/plan-schema (fn-schema desired-entry) (live-fn live-entry) {})
       (filter #(= :edit-functions (:phase %)))
       first))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (.getMessage e))))

(deftest publishing-mints-an-immutable-release
  (let [{:keys [args]} (publish {:source (src "V1") :version "1.0.0"} {:source (src "OLD")})]
    (is (= {"1.0.0" {:source (src "V1") :language :clojure :target-type :ticket}}
           (:releases args))
        "the release is the code as installed — a consumer can now pin it")))

(deftest a-published-version-may-not-come-to-mean-other-code
  (let [msg (refusal #(publish {:source (src "CHANGED") :version "1.0.0"}
                               {:source (src "CHANGED")
                                :version "1.0.0"
                                :releases {"1.0.0" {:source (src "V1") :language :clojure
                                                    :target-type :ticket}}}))]
    (is (re-find #"immutable" msg))
    (is (re-find #"1\.0\.0" msg) "and it names the version")))

(deftest republishing-the-same-code-is-a-no-op-not-a-conflict
  (let [released {:source (src "V1") :language :clojure :target-type :ticket}
        edit     (publish {:source (src "V1") :version "1.0.0"}
                          {:source (src "V1") :version "1.0.0" :releases {"1.0.0" released}})]
    (is (= :skipped (:outcome edit))
        "every unchanged function passes through the publish gate on every push")))

(deftest a-breaking-change-may-not-ship-as-a-compatible-release
  (let [live {:source (src "V1") :version "1.0.0"
              :param-types {:ticket :ticket :note :string}
              :releases {"1.0.0" {:source (src "V1") :language :clojure :target-type :ticket
                                  :param-types {:ticket :ticket :note :string}}}}
        break {:source (src "V2") :param-types {:ticket :ticket}}]
    (testing "as a minor release: refused, with the break and the fix named"
      (let [msg (refusal #(publish (assoc break :version "1.1.0") live))]
        (is (re-find #"input :note was dropped" msg))
        (is (re-find #"2\.0\.0" msg) "the message names the version to release instead")))
    (testing "as a major release: allowed — that is what a major version IS"
      (is (some? (publish (assoc break :version "2.0.0") live))))))

(deftest initial-development-may-break-in-a-minor-release
  (let [live {:source (src "V1") :version "0.1.0"
              :param-types {:ticket :ticket :note :string}
              :releases {"0.1.0" {:source (src "V1") :language :clojure :target-type :ticket
                                  :param-types {:ticket :ticket :note :string}}}}]
    (is (some? (publish {:source (src "V2") :version "0.2.0" :param-types {:ticket :ticket}} live))
        "0.y.z is reserved for exactly this — refusing here would forbid the phase")))

(deftest releases-are-append-only
  (let [{:keys [args]} (publish {:source (src "V2")}
                                {:source (src "V1") :version "1.0.0"
                                 :releases {"1.0.0" {:source (src "V1") :language :clojure
                                                     :target-type :ticket}}})]
    (is (= #{"1.0.0"} (set (keys (:releases args))))
        "dropping :version from an .ent publishes nothing — it must not RETRACT
         what consumers are pinned to")))

(deftest a-build-transform-has-no-release-version
  (let [msg (refusal #(install/plan-schema
                       {:transforms {:roll-up {:language :pg-sql :query "SELECT 1"
                                               :version "1.0.0"}}}
                       {} {}))]
    (is (re-find #"semantic-version" msg)
        "the refusal names the word the author actually wanted")
    (is (re-find #":functions" msg)
        "…and the section that would make it a Function instead")))

(deftest a-query-function-is-versioned-like-any-other-function
  (let [edit (->> (install/plan-schema
                   {:functions {:tally {:language :python :code "def x(): pass"
                                        :version "1.0.0"}}}
                   {} {})
                  (filter #(= :functions (:phase %)))
                  first)]
    (is (= "1.0.0" (:version (:args edit))))
    (is (contains? (:releases (:args edit)) "1.0.0")
        "same primitive, same publish — it is only STORED as a transform")))

(deftest a-query-functions-data-inputs-are-not-its-signature
  (testing "its :inputs are the DATA it reads, resolved server-side — so comparing
            them would refuse a release for a change no consumer can observe"
    (is (empty? (contract/breaks
                 (contract/signature :query {:inputs {:a 1}})
                 (contract/signature :query {:inputs {:b 1}}))))
    (let [live {:transforms {"ri.t.1" {:transform-name :tally :function-name :tally
                                       :language :python :code "def x(): pass"
                                       :version "1.0.0"
                                       :releases {"1.0.0" {:language :python
                                                           :code "def x(): pass"
                                                           :inputs {:a 1}}}}}}
          edit (->> (install/plan-schema
                     {:functions {:tally {:language :python :code "def y(): pass"
                                          :inputs {:a 1 :b 2} :version "1.1.0"}}}
                     live {})
                    (filter #(= :functions (:phase %)))
                    first)]
      (is (some? edit) "a minor release that adds an input is not refused")
      (is (contains? (:releases (:args edit)) "1.1.0")))))

(defn- query-signature [inputs]
  (contract/signature :query {:inputs inputs}))

(deftest a-query-functions-call-inputs-are-its-signature
  (let [old (query-signature {:rows {:dataset :d} :year {:kind :scalar :type :integer}})
        breaks #(contract/breaks old (query-signature %))]
    (is (empty? (breaks {:year {:kind :scalar :type :integer}})))
    (is (= ["required input :month was added"]
           (breaks {:year {:kind :scalar :type :integer}
                    :month {:kind :scalar :type :integer}}))
        "every call must bind a :scalar, so an existing caller would be refused")
    (is (= ["input :year was dropped"] (breaks {})))
    (is (re-find #"changed type" (first (breaks {:year {:kind :scalar :type :string}}))))))

(deftest a-new-required-input-may-not-ship-as-a-compatible-release
  (let [year {:kind :scalar :type :integer}
        live {:transforms {"ri.t.1" {:transform-name :tally :function-name :tally
                                     :language :python :code "def x(): pass"
                                     :version "1.0.0"
                                     :releases {"1.0.0" {:language :python
                                                         :code "def x(): pass"
                                                         :inputs {:year year}}}}}}
        plan (fn [version]
               (install/plan-schema
                {:functions {:tally {:language :python :code "def y(): pass" :version version
                                     :inputs {:year year :month year}}}}
                live {}))]
    (is (re-find #"required input :month was added" (refusal #(plan "1.1.0"))))
    (is (nil? (refusal #(plan "2.0.0"))))))

(deftest an-exported-project-carries-its-release-history
  (let [exported (pemit/export (update-in live [:functions "ri.w.1"] dissoc :project)
                             {:inline? true})
        f        (get-in exported [:ent :schema :functions :escalate])]
    (is (= "1.1.0" (:version f)))
    (is (= #{"1.0.0" "1.1.0"} (set (keys (:releases f))))
        "an export that dropped the history would re-install a project whose
         consumers' pins resolve to nothing")))

(deftest the-git-tree-leaves-release-history-to-the-platform
  (testing "the authored file carries the version, not a copy of every past
            body — otherwise each install rewrites it and the author's next
            commit conflicts with `normalized by install`"
    (let [exported (pemit/export (update-in live [:functions "ri.w.1"] dissoc :project)
                               {:inline? false})
          f        (get-in exported [:ent :schema :functions :escalate])]
      (is (= "1.1.0" (:version f)))
      (is (not (contains? f :releases))))
    (let [tpl {:template-name :bericht :format :html :source "<p/>" :version "0.2.0"
               :releases {"0.1.0" {:source "<b/>"} "0.2.0" {:source "<p/>"}}}]
      (is (not (contains? (:doc (pemit/template-unit tpl {:inline? false})) :releases)))
      (is (contains? (:doc (pemit/template-unit tpl {:inline? false})) :version)))
    (testing "and a publish from that file keeps the history the platform holds"
      (let [{:keys [args]} (publish {:source (src "V12") :version "1.2.0"}
                                    (get-in live [:functions "ri.w.1"]))]
        (is (= #{"1.0.0" "1.1.0" "1.2.0"} (set (keys (:releases args)))))))))

(defn- publish-with-kern
  [kern-source desired-entry live-entry]
  (->> (install/plan-schema
        (assoc-in (fn-schema desired-entry) [:functions :kern]
                  {:library true :language :clojure :source kern-source})
        (live-fn live-entry) {})
       (filter #(and (= :edit-functions (:phase %)) (= :escalate (:entity %))))
       first))

(deftest a-release-freezes-the-libraries-it-uses
  (let [{:keys [args]} (publish-with-kern "(defn k [] 1)"
                                          {:source (src "V1") :version "1.0.0" :uses [:kern]}
                                          {:source (src "OLD")})
        release (get-in args [:releases "1.0.0"])]
    (is (= [{:name :kern :source "(defn k [] 1)"}] (:libraries release))
        "the release holds the library's code as it was published (a function
         version is published with the code it imports)")
    (is (not (contains? args :libraries)) "the head binds its libraries live")
    (testing "a released version runs the frozen library, not the one that stands now"
      (let [live-kern {:kern {:library true :language :clojure :source "(defn k [] 2)"}}
            run       (merge args release)]
        (is (= (str "(defn k [] 1)\n" (src "V1"))
               (functions/function-code live-kern :escalate run)))
        (is (= (str "(defn k [] 1)\n" (src "V1"))
               (functions/function-code live-kern :escalate args))
            "the head under its released version is that release")
        (is (= (str "(defn k [] 2)\n" (src "V2"))
               (functions/function-code live-kern :escalate
                                   (assoc args :source (src "V2") :version "1.1.0")))
            "an unreleased head binds the library as it stands")))))

(deftest a-changed-library-is-a-new-release-of-its-users
  (let [released {:source (src "V1") :language :clojure :target-type :ticket :uses [:kern]
                  :libraries [{:name :kern :source "(defn k [] 1)"}]}
        live     {:source (src "V1") :version "1.0.0" :uses [:kern] :releases {"1.0.0" released}}]
    (is (= :skipped (:outcome (publish-with-kern "(defn k [] 1)"
                                                 {:source (src "V1") :version "1.0.0" :uses [:kern]}
                                                 live)))
        "the same library again is no change")
    (is (re-find #"a library it :uses has changed"
                 (refusal #(publish-with-kern "(defn k [] 2)"
                                              {:source (src "V1") :version "1.0.0" :uses [:kern]}
                                              live))))))
