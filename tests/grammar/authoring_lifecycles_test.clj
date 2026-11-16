(ns ^:unit grammar.authoring-lifecycles-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [sutra.kit.lifecycle :as lc]
            [sutra.parser.checkout :as ck]
            [sutra.catalog.branch-policy :as policy]
            [sutra.actions.staging :as staging]))

(defn- refused [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:refusal/code (ex-data e)))))

(defn- walk
  [{:keys [states edges] :as t} inputs]
  (doseq [event (keys edges)
          from  (cons nil states)]
    (testing (str (:name t) " " event " from " from)
      (if (lc/allowed? t event from)
        (is (contains? states (lc/decide t "id" event from {} (get inputs event))))
        (is (= :refusal.lifecycle/transition-refused
               (refused #(lc/decide t "id" event from {} (get inputs event)))))))))

(defn- step [t event from & [input]] (lc/decide t "id" event from {} input))

(deftest branch
  (walk policy/branch-lifecycle {})
  (testing "a branch is forked only as a new identity, and an ended one takes nothing but purge"
    (is (= :active (step policy/branch-lifecycle :fork nil)))
    (doseq [s [:closed :merged :purged]]
      (is (not (lc/allowed? policy/branch-lifecycle :write s)))
      (is (not (lc/allowed? policy/branch-lifecycle :close s))))
    (is (every? #(not (lc/allowed? policy/branch-lifecycle % :purged))
                (keys (:edges policy/branch-lifecycle)))))
  (testing "a write keeps the state it found"
    (is (= :inactive (step policy/branch-lifecycle :write :inactive))))
  (testing "the governance row's state, and the write block it implies"
    (is (= :active (policy/branch-state nil)))
    (is (= :inactive (policy/branch-state {:inactive-at "t"})))
    (is (= :merged (policy/branch-state {:inactive-at "t" :merged-at "t"})))
    (is (= :purged (policy/branch-state {:closed-at "t" :purged-at "t"})))
    (is (nil? (policy/write-block {:inactive-at "t"} "b")))
    (is (re-find #"is closed" (:error (policy/write-block {:closed-at "t"} "b"))))
    (is (re-find #"is purged" (:error (policy/write-block {:closed-at "t" :purged-at "t"} "b"))))))

(deftest proposal
  (walk policy/proposal-lifecycle {})
  (testing "approval is no state: an open proposal takes verdicts and stays open"
    (is (= :open (step policy/proposal-lifecycle :review :open)))
    (is (= #{:open :merged :closed} (:states policy/proposal-lifecycle))))
  (testing "it lands or closes once, from open only"
    (doseq [e [:land :close :review] s [:merged :closed]]
      (is (not (lc/allowed? policy/proposal-lifecycle e s))))))

(deftest verdict
  (walk policy/verdict-lifecycle {:approve {:eligible? true} :reject {:eligible? true}})
  (testing "an ineligible reviewer is refused with what the task needs"
    (let [e (try (step policy/verdict-lifecycle :approve :none {:eligible? false :needs "owner"})
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= 409 (anomaly/status (ex-data e))))
      (is (re-find #"needs owner" (ex-message e)))))
  (testing "only an approval goes stale; a rejection stands"
    (is (= :stale (step policy/verdict-lifecycle :change :approved)))
    (is (not (lc/allowed? policy/verdict-lifecycle :change :rejected))))
  (testing "the state is the latest verdict against the task's fingerprint now"
    (let [t {:fingerprint "f1"}]
      (is (= :none (policy/verdict-state t nil)))
      (is (= :approved (policy/verdict-state t {:verdict "approved" :fingerprint "f1"})))
      (is (= :stale (policy/verdict-state t {:verdict "approved" :fingerprint "f0"})))
      (is (= :rejected (policy/verdict-state t {:verdict "rejected" :fingerprint "f0"}))))))

(deftest staging
  (walk staging/lifecycle {:unstage {:left 1}})
  (testing "a batch commits only while staged, and ends with its last row unstaged"
    (is (not (lc/allowed? staging/lifecycle :commit :empty)))
    (is (= :discarded (step staging/lifecycle :unstage :staged {:left 0})))
    (is (= :staged (step staging/lifecycle :unstage :staged {:left 2}))))
  (testing "an ended batch takes nothing"
    (doseq [e (keys (:edges staging/lifecycle)) s [:committed :discarded]]
      (is (not (lc/allowed? staging/lifecycle e s))))))

(deftest checkout
  (walk ck/lifecycle {:resolve {:resolve :mine}})
  (testing "a pull never overwrites an edit, a conflict or an orphan — only pull --force does"
    (doseq [s [:edited :conflicted :orphaned]]
      (is (not (lc/allowed? ck/lifecycle :pull s)))
      (is (= :clean (step ck/lifecycle :pull-force s)))))
  (testing "the guard of a pull names what it would overwrite"
    (is (re-find #"would overwrite a.ent"
                 (try (step ck/lifecycle :pull :clean {:stop ["a.ent"]})
                      (catch clojure.lang.ExceptionInfo e (ex-message e))))))
  (testing "a rehomed pull leaves the checkout edited: its push moves the nodes"
    (is (= :edited (step ck/lifecycle :pull :absent {:rehome? true}))))
  (testing "a conflict is decided only by naming a side"
    (is (not (lc/allowed? ck/lifecycle :push :conflicted)))
    (is (= :refusal.lifecycle/transition-refused
           (refused #(step ck/lifecycle :resolve :conflicted {}))))
    (is (= :staged (step ck/lifecycle :resolve :conflicted {:resolve :theirs}))))
  (testing "a move-only push stages nothing"
    (is (= :clean (step ck/lifecycle :move :edited)))
    (is (= :staged (step ck/lifecycle :move :staged))))
  (testing "an orphaned checkout is usable again only by pull --force"
    (is (= [:pull-force]
           (filterv #(lc/allowed? ck/lifecycle % :orphaned) (keys (:edges ck/lifecycle))))))
  (testing "the state: the sidecar's, unless a text differs from its hash"
    (is (= :absent (ck/state nil false)))
    (is (= :clean (ck/state {} false)) "a sidecar from before the state was recorded")
    (is (= :edited (ck/state {:state :staged} true)))
    (is (= :staged (ck/state {:state :staged} false)))
    (is (= :conflicted (ck/state {:state :conflicted} true)))
    (is (= :orphaned (ck/state {:state :orphaned} true)))))

(def ^:private spec-file (io/file "spec/lifecycles.edn"))

(deftest the-spec-tables-are-the-code-tables
  (if-not (.isFile spec-file)
    (is true "no spec in this checkout")
    (doseq [[k
             {:keys [im-code zustaende ende
                     uebergaenge]}] (:lebenslaeufe (edn/read-string (slurp spec-file)))
            :when (symbol? im-code)
            :let [{:keys [states final edges]} @(requiring-resolve im-code)]]
      (testing (str k " = " im-code)
        (is (= (set zustaende) states))
        (is (= ende final))
        (is (= (set (keys uebergaenge)) (set (keys edges))))
        (doseq [[e {:keys [von nach]}] uebergaenge
                :let [{:keys [from to]} (get edges e)]]
          (is (= von from) (str e " :von"))
          (if (keyword? to)
            (is (= nach to) (str e " :nach"))
            (is (or (= :selbst nach) (and (set? nach) (every? states nach)))
                (str e " :nach of a computed target"))))))))
