(ns ^:unit platform.decisions-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.plan]
            [sutra.capps.portal]
            [sutra.capps.standard-view]
            [sutra.builds.run.schedule :as schedule]
            [sutra.foundation.decision-trace :as dtrace]
            [sutra.foundation.settings :as settings]
            [sutra.kit.decisions :as decisions]
            [sutra.serve.worker :as worker]
            [sutra.types.feedback :as fb]
            [sutra.types.feedback-szenario :as fz]))

(use-fixtures :each (fn [t] (try (t) (finally (dtrace/disarm!) (decisions/stop!)))))

(defn- round-trip [records] (decisions/read-text (decisions/text records)))

(deftest off-records-nothing
  (decisions/start! [:build/merge])
  (decisions/stop!)
  (is (= (schedule/step schedule/initial {:kind :none})
         (decisions/decide :build/schedule schedule/initial {:kind :none})))
  (is (empty? (decisions/records)) "a stopped recorder keeps nothing new"))

(deftest only-the-kinds-asked-are-kept
  (decisions/start! [:build/plan "view/portal" :no-such-kind])
  (is (= [:build/plan :view/portal] (:recording (decisions/status))))
  (decisions/decide :build/schedule schedule/initial {:kind :none})
  (is (empty? (decisions/records)))
  (decisions/start! [])
  (is (= (set (keys decisions/kinds)) (set (:recording (decisions/status))))
      "none named: every kind"))

(deftest a-poll-with-nothing-to-do-is-not-kept
  (decisions/start! [:build/schedule])
  (let [busy {:running #{"r1"} :tidied (System/currentTimeMillis) :stopping? false}
        e    {:kind :poll :waiting #{} :cap 1 :waiting-max 4 :now (System/currentTimeMillis)}]
    (decisions/decide :build/schedule busy e)
    (is (empty? (decisions/records)) "full lane, nothing to tidy: idle")
    (decisions/decide :build/schedule busy (assoc e :cap 2))
    (is (= 1 (count (decisions/records))) "room to take: kept")))

(deftest a-kind-keeps-its-newest
  (decisions/start! [:build/schedule])
  (dotimes [i (+ 5 decisions/kept-per-kind)]
    (decisions/decide :build/schedule schedule/initial
                      {:kind :finished :run-id (str i)}))
  (let [rs (decisions/records)]
    (is (= decisions/kept-per-kind (count rs)))
    (is (= (str (+ 4 decisions/kept-per-kind)) (-> rs last :inputs second :run-id)))))

(deftest the-worker-lane-records-at-its-one-boundary
  (testing "dispatch! keeps the step whose effects it carries out, and the record replays"
    (dtrace/arm! {:kinds [:build/schedule]})
    (let [sched (atom {:running #{"r1" "r2"} :tidied 0 :stopping? false})]
      (with-redefs [settings/value (fn [k] (case k :build-concurrency 2 :build-waiting-max 4))]
        (#'worker/dispatch! sched nil {:kind :quiesce}))
      (is (:stopping? @sched))
      (let [[r :as rs] (round-trip (decisions/records))]
        (is (= 1 (count rs)))
        (is (= [[:hand-back "r1"] [:hand-back "r2"] [:ring]] (second (:output r))))
        (is (:same? (decisions/replay r)))))))

(def ^:private schema
  {:datasets   {"ri.sutra.main.dataset.raw" {:dataset-name :raw :kind :transform-output}}
   :transforms {:pull {:transform-name :pull :language :python
                       :inputs {} :outputs [{:kind :dataset
                                             :dataset "ri.sutra.main.dataset.raw"
                                             :name :raw}]}}})

(deftest a-build-plan-replays-from-its-text
  (decisions/start! [:build/plan])
  (let [planned (decisions/decide :build/plan schema {:only nil})
        [r]     (round-trip (decisions/records))]
    (is (= [:pull] (:jobs planned)))
    (is (= {:kind :build/plan :same? true} (select-keys (decisions/replay r) [:kind :same?])))
    (testing "a changed input is decided differently"
      (is (false? (:same? (decisions/replay (assoc-in r [:inputs 0 :transforms] {}))))))))

(deftest every-recording-in-the-scenarios-replays
  (let [files  (some->> (io/file "tests/scenario/decisions") .listFiles
                        (filter #(.endsWith (.getName ^java.io.File %) ".edn")) sort)
        differ (for [f files
                     r (decisions/read-text (slurp f))
                     :when (not (:same? (decisions/replay r)))]
                 (str (.getName ^java.io.File f) ": " (:kind r) " at " (:at r)))]
    (is (empty? differ) (str "decided differently: " (vec differ)))))

(deftest a-feedback-click-replays-what-the-screen-showed
  (testing "the window decides through `decide`, the click keeps the newest of each kind,
            the server takes it, and the click's scenario replays it"
    (decisions/start! [:view/portal :view/object-page])
    (decisions/decide :view/portal [{:name :a :workspaces []}] [])
    (decisions/decide :view/portal [{:name :a :workspaces []}
                                                {:name :b :workspaces [:w]}]
                      [{:name :w}])
    (decisions/decide :view/object-page :thing
                      {:standard-view {:links [] :actions []}} nil {:actions? false})
    (let [text   (fb/entscheidungen (decisions/records))
          klick  {:id "k1" :art :klick :t 10 :ort {:pfad "/apps/a"} :punkt [1 2]
                  :ziel {:rolle :button :name "Weiter"} :entscheidungen text}
          {[got] :angenommen :keys [abgelehnt]} (fb/stapel [klick])
          sz     (fz/klick-szenario {:sitzung "s1" :verlauf [got]})]
      (is (empty? abgelehnt) (pr-str abgelehnt))
      (is (= text (:entscheidungen got)) "the server keeps the field")
      (is (= #{:view/portal :view/object-page} (set (map :kind (:entscheidungen sz)))))
      (is (= 2 (->> (:entscheidungen sz) (filter #(= :view/portal (:kind %)))
                    first :output :apps count))
          "only the newest portal decision rides along")
      (is (every? :same? (map decisions/replay (:entscheidungen sz)))))))

(deftest a-click-keeps-only-what-fits
  (decisions/start! [:view/portal])
  (decisions/decide :view/portal
                    (vec (for [i (range 3000)] {:name (keyword (str "app-" i))})) [])
  (is (nil? (fb/entscheidungen (decisions/records)))
      "a decision too long for the event is left out rather than cut"))
