(ns ^:unit platform.jobspec-chain-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.jobspecs :as jobspecs]
            [sutra.builds.plan :as plan]
            [sutra.catalog.signature :as signature]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.seat :as seat]
            [sutra.store.world :as world]
            [support.mem :as mem]))

(def ^:private actor {:principal "test"})

(defn- install! [store b W]
  (world/install! (world/at store b) (ri/derived {:sigma clock/system} W)
                  {:dataset-decls signature/datasets-of :actor actor}))

(defn- world-with [q]
  {:datasets   {"ri.sutra.main.dataset.a" {:dataset-name :a :kind :standalone}
                "ri.sutra.main.dataset.b" {:dataset-name :b :kind :standalone}}
   :transforms {"ri.sutra.main.transform.ab"
                {:transform-name :ab :language :pg-sql :query q
                 :inputs {:a {:kind :dataset :dataset "ri.sutra.main.dataset.a"}}
                 :output {:kind :dataset :dataset "ri.sutra.main.dataset.b"}}}})

(defn- query-built [store b]
  (let [at (world/at store b)]
    (get-in (jobspecs/build-schema at (seat/branch-schema at))
            [:schema :transforms "ri.sutra.main.transform.ab" :query])))

(deftest a-branch-builds-mains-current-logic-until-it-publishes-its-own
  (let [store (mem/store)]
    (install! store "main" (world-with "SELECT 1 FROM a"))
    (world/fork! store "main" "feature")
    (install! store "main" (world-with "SELECT 2 FROM a"))
    (testing "untouched on the branch: main's JobSpec, as main has it now"
      (is (= "SELECT 1 FROM a" (get-in (seat/branch-schema (world/at store "feature"))
                                       [:transforms "ri.sutra.main.transform.ab" :query]))
          "the branch's own copy is still the forked one")
      (is (= "SELECT 2 FROM a" (query-built store "feature")))
      (is (= "main" (get-in (jobspecs/resolved (world/at store "feature"))
                            [[:datasets "ri.sutra.main.dataset.b"] :branch]))))
    (testing "committed on the branch: the branch's JobSpec"
      (install! store "feature" (world-with "SELECT 3 FROM a"))
      (is (= "SELECT 3 FROM a" (query-built store "feature")))
      (is (= "feature" (get-in (jobspecs/resolved (world/at store "feature"))
                               [[:datasets "ri.sutra.main.dataset.b"] :branch]))))
    (testing "main builds its own"
      (is (= "SELECT 2 FROM a" (query-built store "main"))))))

(def ^:private cd
  {"ri.sutra.main.transform.cd"
   {:transform-name :cd :language :pg-sql :query "SELECT 1 FROM c"
    :inputs {:c {:kind :dataset :dataset "ri.sutra.main.dataset.c"}}
    :output {:kind :dataset :dataset "ri.sutra.main.dataset.d"}}})

(defn- with-cd [w]
  (-> w
      (update :datasets assoc
              "ri.sutra.main.dataset.c" {:dataset-name :c :kind :standalone}
              "ri.sutra.main.dataset.d" {:dataset-name :d :kind :standalone})
      (update :transforms merge cd)))

(deftest a-dataset-main-gained-after-the-fork-is-read-through-the-fallback
  (let [store (mem/store)
        later (-> (world-with "SELECT 1 FROM n")
                  (assoc-in [:datasets "ri.sutra.main.dataset.n"]
                            {:dataset-name :n :kind :standalone})
                  (assoc-in [:transforms "ri.sutra.main.transform.ab" :inputs :a :dataset]
                            "ri.sutra.main.dataset.n"))]
    (install! store "main" (world-with "SELECT 1 FROM a"))
    (world/fork! store "main" "feature")
    (install! store "main" later)
    (let [at (world/at store "feature")
          {:keys [schema problems]} (jobspecs/build-schema at (seat/branch-schema at))]
      (is (empty? problems))
      (is (= :n (get-in schema [:datasets "ri.sutra.main.dataset.n" :dataset-name])))
      (is (nil? (:refusal (plan/build-plan schema {:only ["ab"] :strategy :selected}))))
      (is (nil? (get-in (seat/branch-schema at) [:datasets "ri.sutra.main.dataset.n"]))
          "the branch's own world stays as it is"))))

(deftest a-jobspec-the-branch-cannot-run-only-stops-a-build-that-runs-it
  (let [store (mem/store)
        later (assoc-in (with-cd (world-with "SELECT 1 FROM gone"))
                        [:transforms "ri.sutra.main.transform.ab" :inputs :a :dataset]
                        "ri.sutra.main.dataset.gone")]
    (install! store "main" (with-cd (world-with "SELECT 1 FROM a")))
    (world/fork! store "main" "feature")
    (install! store "main" later)
    (let [at (world/at store "feature")
          {:keys [schema problems]} (jobspecs/build-schema at (seat/branch-schema at))
          plan (fn [targets] (plan/build-plan schema targets))]
      (is (= ["ri.sutra.main.transform.ab"] (map :transform-rid problems)))
      (testing "another transform, selected: it plans and nothing refuses it"
        (let [{:keys [jobs refusal]} (plan {:only ["cd"] :strategy :selected})]
          (is (nil? refusal))
          (is (= ["ri.sutra.main.transform.cd"] (map str jobs)))
          (is (nil? (jobspecs/refuse-unrunnable! at problems jobs)))))
      (testing "the transform itself: refused, naming it"
        (let [{:keys [jobs]} (plan {:only ["ab"] :strategy :selected})]
          (is (thrown-with-msg? Exception #"build resolution.*transform.ab"
                                (jobspecs/refuse-unrunnable! at problems jobs)))))
      (testing "the whole branch includes it: refused"
        (is (thrown? Exception
                     (jobspecs/refuse-unrunnable! at problems (:jobs (plan {})))))))))
