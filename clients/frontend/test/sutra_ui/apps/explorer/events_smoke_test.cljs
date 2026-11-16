(ns sutra-ui.apps.explorer.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.explorer.events]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]))

(defn- asks [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (asks r)))

(defn- asked
  [[_ fx]]
  (keep (fn [[k [e req]]] (when (and (= :dispatch k) (= :rf/request e)) req)) fx))

(defn- kinds [[_ fx]] (mapv first (remove nil? fx)))

(deftest a-change-the-server-sees-reloads-the-set-later
  (let [[s fx] (step {} [:explorer.set/change :kunde assoc :search "maier"])]
    (is (= "maier" (get-in s [:set :kunde :search])))
    (is (= [:after {:key [:reload :kunde] :ms 300 :event [:explorer.set/load :kunde]}]
           (first fx))))
  (testing "a change only the page sees reloads nothing"
    (is (= [] (kinds (step {} [:explorer.set/change :kunde assoc :filters-open? true]))))))

(deftest loading-a-set-forgets-its-reads-and-asks-the-panel
  (let [[s [[k [f ot]]]] (step {:reads {:kunde {{} {}} :lief {{} {}}}}
                               [:explorer.set/load :kunde])]
    (is (= {:lief {{} {}}} (:reads s)))
    (is (= [:invoke panel/refresh-panel-instances! :kunde] [k f ot]))))

(deftest a-read-is-asked-once
  (let [[s :as r] (step {} [:explorer.read/need :kunde {:find :kunde}])
        [{:keys [ok err]}] (asks r)]
    (is (= [[api/post! :ontology-query {:find :kunde}]] (calls r)))
    (is (= [] (calls (step s [:explorer.read/need :kunde {:find :kunde}]))))
    (let [s' (run s (conj ok {:rows []}))]
      (is (= {:rows []} (get-in s' [:reads :kunde {:find :kunde}])))
      (is (= [] (calls (step s' [:explorer.read/need :kunde {:find :kunde}])))))
    (testing "a failed read keeps its sentence"
      (is (= {:error "weg"} (get-in (run s (conj err "weg")) [:reads :kunde {:find :kunde}]))))))

(deftest a-saved-entry-opens-by-its-kind
  (let [r (step {} [:explorer/open-saved {:rid "ri.object-set.main.object-set.1"}])]
    (is (= [[api/post! :ontology-query {:object-set "ri.object-set.main.object-set.1"}]]
           (calls r)))
    (is (false? (:drawer? (first r)))))
  (testing "an object set without a type is refused"
    (is (= [:invoke] (kinds (step {} [:explorer.set-file/loaded {:rid "x"} {:rows []}])))))
  (testing "an object set restricts its type to its members"
    (let [[s] (step {} [:explorer.set-file/loaded {:rid "x" :name "A"}
                        {:object-type :kunde :rows [{:id 1} {:id 2}]}])]
      (is (= :kunde (:object-type s)))
      (is (= #{"1" "2"} (get-in s [:set :kunde :restrict :ids]))))))

(deftest saved-sets-load-once
  (is (= [[api/get! :object-sets]] (calls (step {} [:explorer.saved/ensure]))))
  (is (= [] (calls (step {:saved-sets {}} [:explorer.saved/ensure])))))

(deftest a-pivot-restricts-the-peer-type-when-the-members-arrive
  (let [[s fx] (step {} [:explorer/pivot :kunde {:side :auftraege :label "Aufträge"
                                                 :peer-type :auftrag} "3 Kunden"])]
    (is (= :auftrag (:object-type s)))
    (is (get-in s [:set :auftrag :restrict :pending?]))
    (is (= [:dispatch [:explorer.set/load :auftrag]] (first fx))))
  (let [[s] (step {:set {:auftrag {:restrict {:ids #{} :pending? true}}}}
                  [:explorer.pivot/loaded :auftrag {:rows [{:id 7}]}])]
    (is (= {:ids #{"7"}} (get-in s [:set :auftrag :restrict]))))
  (let [[{:keys [err]}] (asks (step {} [:explorer/pivot :kunde {:side :auftraege
                                                                :label "Aufträge"
                                                                :peer-type :auftrag} "3 K"]))
        s0 {:set {:auftrag {:restrict {:pending? true}}}}
        [_ fx] (step s0 (conj err "weg"))]
    (is (nil? (get-in (run s0 (conj err "weg")) [:set :auftrag :restrict])))
    (is (some #(= :invoke (first %)) fx))))

(deftest a-saved-file-lands-in-its-folder
  (let [[s :as r] (step {} [:explorer.save/file :kunde {:kind :exploration :name "A"
                                                       :project "p" :folder "f1"}])
        [{:keys [call ok err]}] (asks r)]
    (is (:saving? (get-in s [:set :kunde])))
    (is (= [api/post! :explorations] (take 2 call)))
    (let [r (step s (conj ok {:rid "e1"}))
          s' (run s (conj ok {:rid "e1"}))]
      (is (= {:rid "e1" :kind :exploration} (get-in s' [:set :kunde :saved])))
      (is (false? (get-in s' [:set :kunde :saving?])))
      (is (= [:explorer.save/saved :kunde :exploration {:folder "f1"} {:rid "e1"}]
             (second (first (second r))))))
    (is (= {:error "nein"} (get-in (run s (conj err "nein")) [:set :kunde :saved]))))
  (let [r (step {} [:explorer.save/saved :kunde :exploration {:folder "f1"} {:rid "e1"}])]
    (is (some #{[:invoke [ks/refresh! :explorations]]} (second r)))
    (is (= [[ks/kompass-place! "exploration/e1" "f1" nil]] (map :ask (asked r))))))

(deftest a-failure-shows-its-sentence
  (let [[{:keys [err]}] (asks (step {} [:explorer/export :kunde identity]))]
    (is (= [[:invoke [shell/fail! nil "kaputt"]]]
           (keep identity (second (step {} (conj err "kaputt"))))))))

(deftest the-seed-search-waits-for-two-letters
  (is (= [[:cancel :seed-search]] (second (step {} [:explorer.seed/type "a"]))))
  (let [[s [[k a]]] (step {} [:explorer.seed/type "ab"])]
    (is (= "ab" (get-in s [:seed :q])))
    (is (= [:after [:explorer.seed/search "ab"]] [k (:event a)])))
  (let [[s :as r] (step {} [:explorer.seed/search "ab"])
        [{:keys [ok]}] (asks r)
        s' (run s (conj ok {:groups [{:type :t :rows [{:id 1}]}]}))]
    (is (get-in s [:seed :busy?]))
    (is (= [{:type-ref :t :row {:id 1}}] (get-in s' [:seed :results])))
    (is (false? (get-in s' [:seed :busy?])))))

(deftest the-home-search-drops-a-stale-answer
  (let [[{:keys [ok]}] (asks (step {:home {:q "neu"}} [:explorer.home/search "alt"]))]
    (is (nil? (get-in (run {:home {:q "neu"}} (conj ok {:groups []})) [:home :hits]))))
  (let [[{:keys [err]}] (asks (step {:home {:q "x"}} [:explorer.home/search "x"]))]
    (is (= [] (get-in (run {:home {:q "x"}} (conj err "weg")) [:home :hits])))))
