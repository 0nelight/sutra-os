(ns ^:unit apps.enta-view-as-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.view-as :as va]))

(def ^:private tree
  {:kind :pages
   :select {:var :seite}
   :view-as [{:id :disposition :label "Disposition"}
             {:id :leitung  :label "Technische Leitung"}]
   :pages
   [{:id :uebersicht :label "Übersicht"
     :body {:kind :rows :items [{:widget :text :text "hallo"}]}}
    {:id :kosten :label "Kosten"
     :visibility {:roles [:cam]}
     :body {:kind :rows :items [{:widget :text :text "Kosten je Flugstunde"}]}}
    {:id :disposition :label "Teiledisposition"
     :visibility {:profiles [:disposition]}
     :body {:kind :tabs :select {:var :reiter}
            :tabs [{:id :offen :label "Offen"
                    :body {:widget :text :text "offen"}}
                   {:id :geheim :label "Geheim"
                    :visibility {:roles [:leitung]}
                    :body {:widget :text :text "geheim"}}]}}
    {:id :bedingt :label "Bedingt"
     :visibility {:when {:var :zeig-mich} :link "ri.link.main.x"}
     :body {:widget :text :text "vielleicht"}}]})

(defn- pane-ids [t] (mapv :id (:pages t)))

(defn- nested-tab-ids [t]
  (->> (:pages t)
       (some (fn [p] (when (= :disposition (:id p)) p)))
       :body :tabs (mapv :id)))

(deftest audiences-are-only-the-ones-the-tree-mentions
  (testing "roles, profiles and :view-as entries, unioned and id-sorted"
    (is (= [:cam :disposition :leitung] (mapv :id (va/audiences tree)))))

  (testing "a :view-as label wins over the derived one; provenance is carried"
    (let [by-id (into {} (map (juxt :id identity)) (va/audiences tree))]
      (is (= "Disposition" (:label (by-id :disposition))))
      (is (true? (:profile? (by-id :disposition))))
      (is (true? (:view-as? (by-id :disposition))))
      (is (= "cam" (:label (by-id :cam))))
      (is (true? (:role? (by-id :cam))))
      (is (false? (:view-as? (by-id :cam))))
      (is (true? (:role? (by-id :leitung))))
      (is (true? (:view-as? (by-id :leitung))))))

  (testing "a tree with no visibility and no :view-as offers nothing"
    (is (= [] (va/audiences {:kind :rows :items [{:widget :text :text "x"}]}))))

  (testing "a non-named id is not offered as an audience"
    (is (= [] (va/audiences {:kind :tabs
                             :tabs [{:id :a :visibility {:roles [42 nil]}}]})))))

(deftest prune-drops-exactly-the-panes-the-audience-loses
  (testing "\"alle\" is the identity — nil never touches the tree"
    (is (identical? tree (va/prune tree nil)))
    (is (= [] (va/hidden-panes tree nil))))

  (testing "a role sees the ungated panes plus its own"
    (is (= [:uebersicht :kosten :bedingt] (pane-ids (va/prune tree :cam)))))

  (testing "a profile sees the ungated panes plus its own, and the pick reaches NESTED panes"
    (let [t (va/prune tree :disposition)]
      (is (= [:uebersicht :disposition :bedingt] (pane-ids t)))
      (is (= [:offen] (nested-tab-ids t)))))

  (testing "the audience whose gate is nested keeps the nested pane"
    (is (= [:uebersicht :bedingt] (pane-ids (va/prune tree :leitung)))))

  (testing "an audience nobody gated on sees only the ungated panes"
    (is (= [:uebersicht :bedingt] (pane-ids (va/prune tree :fremd)))))

  (testing ":when and :link stay with the runtime — they never hide a pane here"
    (is (some #(= :bedingt %) (pane-ids (va/prune tree :cam))))
    (is (true? (va/pane-visible-for? {:id :bedingt
                                      :visibility {:when {:var :x} :link "ri.link.main.x"}}
                                     :fremd))))

  (testing "an empty gate list means all, exactly as pane-visible? reads it"
    (is (true? (va/pane-visible-for? {:id :a :visibility {:roles [] :profiles []}} :fremd))))

  (testing ":variants is NOT an audience slot — a :profiles container PICKS, it does not hide"
    (let [t {:kind :profiles :select {:var :p}
             :variants [{:id :a :visibility {:roles [:cam]} :body {:widget :text}}
                        {:id :b :body {:widget :text}}]}]
      (is (= [:a :b] (mapv :id (:variants (va/prune t :fremd))))))))

(deftest hidden-panes-names-what-the-pick-took-away
  (testing "the dropped panes are reported with their labels and slots"
    (is (= [{:slot :pages :id :kosten :label "Kosten"}
            {:slot :pages :id :disposition :label "Teiledisposition"}
            {:slot :tabs  :id :geheim  :label "Geheim"}]
           (va/hidden-panes tree :fremd))))

  (testing "the audience's own pane is not among them"
    (is (= [:kosten :geheim] (mapv :id (va/hidden-panes tree :disposition)))))

  (testing "a pane with no label falls back to its id"
    (is (= [{:slot :tabs :id :x :label "x"}]
           (va/hidden-panes {:kind :tabs :tabs [{:id :x :visibility {:roles [:r]}}]} :fremd)))))
