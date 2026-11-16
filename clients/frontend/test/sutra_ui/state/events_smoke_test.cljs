(ns sutra-ui.state.events-smoke-test
  (:require [cljs.test :refer [deftest is testing async]]
            [sutra-ui.state.requests :as rq]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.state.branches]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.atlas]
            [sutra-ui.state.lineage-status]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.panel]
            [sutra-ui.state.sync]))

(defn- load-req [[_ fx]] (second (second (first (filter #(= :dispatch (first %)) fx)))))

(deftest branch-changes-on-main-are-absent-elsewhere-loading
  (let [[s fx] (step {} [:sutra.branches/changes "main"])]
    (is (rd/absent? (get-in s [:branches :branch-changes])))
    (is (empty? fx)))
  (let [[s fx] (step {} [:sutra.branches/changes "feature"])]
    (is (rd/loading? (get-in s [:branches :branch-changes])))
    (is (= "feature" (get-in s [:branches :changes-of])))
    (is (= [api/get! :branch-changes {:name "feature"}] (:ask (load-req [s fx]))))))

(deftest the-runs-query-follows-the-filters
  (let [[_ fx] (step {:data {:runs-window-hours 24 :runs-kind-filter :build}}
                     [:sutra.data/refresh-runs])]
    (is (= [api/list-runs {:kind :build :hours 24 :limit 2000}]
           (:ask (load-req [nil fx]))))))

(deftest a-snapshot-adopts-the-catalog-and-asks-what-follows-it
  (let [snap   {:schema-version 7
                :schema {:object-types {:kunde {:id "ri.ot.1" :object-type-name :kunde}}}
                :datasources {:pg {:name :pg}}}
        [s fx] (step {} [:sutra.sync/snapshot-read snap])]
    (is (= 7 (get-in s [:sync :schema-version])))
    (is (= {:pg {:name :pg}} (get-in s [:data :datasources])))
    (is (contains? (get-in s [:catalog :canonical-schema :object-types]) :kunde))
    (is (some #{[:dispatch [:sutra.lineage/refresh]]} fx)))
  (testing "a failed snapshot fails a catalog not yet here and says so"
    (let [s0              {:catalog {:rid->name rd/loading} :data {:datasources rd/loading}}
          [_ [[_ {:keys [err]}]]] (step s0 [:sutra.sync/snapshot nil])
          s               (run s0 (conj err "weg"))]
      (is (rd/failed? (get-in s [:catalog :rid->name])))
      (is (rd/failed? (get-in s [:data :datasources])))
      (is (= :invoke (ffirst (second (step s0 (conj err "weg")))))))))

(deftest a-favorite-turns-at-once-and-back-on-a-refusal
  (let [db0    {:kompass {:kompass-answer {:favorites #{}}}}
        [s fx] (step db0 [:sutra.kompass/favorite "project/1"])]
    (is (= #{"project/1"} (get-in s [:kompass :kompass-answer :favorites])))
    (is (= :call (ffirst fx)))
    (let [[s' _] (step s [:sutra.kompass/favorite-refused true "project/1" "nein"])]
      (is (= #{} (get-in s' [:kompass :kompass-answer :favorites])))))
  (is (nil? (machine/effects {:kompass {:kompass-answer rd/loading}}
                             [:sutra.kompass/favorite "x"]))
      "nothing turns while the favorites are not here"))

(deftest a-panel-answer-overtaken-by-a-later-fill-changes-nothing
  (let [slot          [:related :object-type :kunde]
        fill          [:sutra.panel/fill slot [api/get! :x] identity {:failed {:items []}}]
        [s1 [[_ c1]]] (step {} fill)
        [s2 [[_ c2]]] (step s1 fill)
        s3            (run s2 (conj (:ok c1) {:items [1]}))]
    (is (:loading? (get-in s3 [:panel :panels slot])))
    (is (= {:loading? false :error "kaputt" :items []}
           (get-in (run s3 (conj (:err c2) "kaputt")) [:panel :panels slot])))))

(deftest the-lineage-status-of-another-branch-is-dropped
  (let [[s _] (step {} [:sutra.lineage-status/refresh "b1" 5])]
    (is (= {:branch "b1" :at 5} (get-in s [:lineage-status :read])))
    (let [[s' _] (step s [:sutra.lineage-status/loaded "b0" {:nodes {:x 1}}])]
      (is (= s s')))
    (let [[s' _] (step s [:sutra.lineage-status/loaded "b1" {:nodes {:x 1}}])]
      (is (= {:nodes {:x 1}} (get-in s' [:lineage-status :lineage-status]))))))

(deftest a-cursor-reads-writes-and-is-watched-through-the-root
  (let [c    (db/cursor [::t :c] 1)
        seen (atom [])]
    (add-watch c ::w (fn [_ _ o n] (swap! seen conj [o n])))
    (swap! db/db assoc-in [::t :c] 2)
    (swap! c inc)
    (swap! db/db assoc ::other 1)
    (is (= 3 @c))
    (is (= [[1 2] [2 3]] @seen))
    (remove-watch c ::w)))

(deftest a-promise-settles-with-what-the-handler-answers
  (async done
    (db/reg ::answer (fn [_ [_ v reply]] {:fx [(rq/settle reply (* 2 v))]}))
    (.then (db/promise! [::answer 21])
           (fn [v] (is (= 42 v)) (done)))))

(deftest adopting-a-catalog-keeps-what-it-did-not-change
  (let [c (catalog/adopt {:schema-version 0} {:graphs {:g {:id "ri.g.1" :mode :lineage}}} nil)]
    (is (= 1 (:schema-version c)))
    (is (= "g" (get-in c [:views "ri.g.1" :name])))
    (is (= c (catalog/adopt c nil nil)) "no catalog changes nothing")))
