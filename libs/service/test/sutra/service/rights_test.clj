(ns sutra.service.rights-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.service.rights :as r]
            [sutra.world.db :as db]
            [sutra.world.gen :as kg]
            [sutra.world.guard :as guard]
            [sutra.world.policy :as p]
            [sutra.kit.datalog :as dl]
            [sutra.ref.install :as ri]
            [sutra.types.authority :as policy]))

(def ^:private pool
  {:organizations [:o1 :o2] :spaces [:s1 :s2] :projects [:p1 :p2 :p3]
   :sources [:q1 :q2] :datasets [:d1 :d2 :d3] :object-types [:t1 :t2]
   :transforms [:x1 :x2]})

(defn rid-of [sec n] (ri/rid (ri/tag-of sec) (name n)))

(def ^:private principals ["u1" "u2" "g1" "g2"])
(def ^:private markings [:m1 :m2 :m3 :m4])
(def ^:private roles-used [:ontology-viewer :ontology-editor :ontology-owner :consumer :custom
                           :unknown])

(def roles
  {:ontology-finder {:operations #{:discover}}
   :ontology-viewer {:includes :ontology-finder :operations #{:read}}
   :ontology-editor {:includes :ontology-viewer :operations #{:create :update}}
   :ontology-owner  {:includes :ontology-editor
                     :operations #{:delete :apply-marking :manage-permissions}}
   :consumer        {:operations #{}}
   :custom          {:includes :ontology-viewer :operations #{:export}}})

(defn- opt [g] (gen/frequency [[1 (gen/return nil)] [2 g]]))
(defn- some-of [xs] (gen/fmap #(vec (distinct %)) (gen/vector (gen/elements xs) 0 2)))

(defn- g-ref
  [sec]
  (gen/let [n (gen/elements (pool sec)) how (gen/elements [:rid :kw :str :str :none])]
    (case how :rid (rid-of sec n) :kw n :str (name n) :none :zz)))

(def ^:private g-grants
  (gen/vector (gen/let [pr (gen/elements principals) ro (gen/elements roles-used)]
                {:principal pr :role ro})
              0 2))

(def ^:private g-marks (some-of markings))

(def ^:private g-container
  (gen/let [grants g-grants ms g-marks sw (gen/vector (g-ref :organizations) 0 1)
            os (gen/vector (g-ref :organizations) 0 1)]
    (cond-> {:grants grants :markings ms}
      (seq sw) (assoc :shared-with sw)
      (seq os) (assoc :organizations os))))

(def ^:private g-input
  (gen/one-of
   [(gen/let [d (g-ref :datasets) sp (some-of markings) sr (gen/vector (g-ref :organizations) 0 1)]
      {:kind :dataset :dataset d :stop-propagating sp :stop-requiring sr})
    (gen/fmap (fn [t] {:kind :object-type :object-type t}) (g-ref :object-types))
    (gen/fmap (fn [s] {:kind :datasource :datasource s}) (g-ref :sources))
    (gen/fmap (fn [t] {:kind :transform :transform t}) (g-ref :transforms))]))

(defn- resource [n nf extra]
  (gen/let [pr (opt (g-ref :projects)) ms g-marks x extra]
    (merge (cond-> {nf n :markings ms} pr (assoc :project pr)) x)))

(def ^:private entry
  {:organizations (fn [n] (gen/let [c g-container ms (some-of principals)
                                    gs (some-of principals)]
                            (assoc c :organization-name n :members ms
                                   :guests (mapv (fn [g] {:principal g}) gs))))
   :spaces        (fn [n] (gen/let [c g-container o (opt (g-ref :organizations))]
                            (cond-> (assoc c :space-name n) o (assoc :organization o))))
   :projects      (fn [n] (gen/let [c g-container s (opt (g-ref :spaces))
                                    al (opt (some-of markings)) fb (opt (some-of markings))]
                            (cond-> (assoc c :project-name n)
                              s  (assoc :space s)
                              al (assoc :allowed-markings al)
                              fb (assoc :forbidden-markings fb))))
   :sources       (fn [n] (resource n :source-name (gen/return {})))
   :object-types  (fn [n] (resource n :object-type-name
                                    (gen/let [d (opt (g-ref :datasets)) s (opt (g-ref :sources))]
                                      (cond-> {} d (assoc :dataset d)
                                              s (assoc :external-source {:datasource s})))))
   :datasets      (fn [n] (resource n :dataset-name
                                    (gen/let [ot (opt (g-ref :object-types))
                                              rv (opt (g-ref :datasets))
                                              st (opt (gen/let [f (g-ref :datasets)
                                                                sp (some-of markings)
                                                                sr (gen/vector
                                                                    (g-ref :organizations) 0 1)]
                                                        {:from f :stop-propagating sp
                                                         :stop-requiring sr}))
                                              v (opt (g-ref :sources))]
                                      (cond-> {}
                                        ot (assoc :object-type ot)
                                        rv (assoc :restricted-view {:dataset rv})
                                        st (assoc :stream {:derive st})
                                        v  (assoc :virtual {:source v})))))
   :transforms    (fn [n] (resource n :transform-name
                                    (gen/let [ins (gen/vector g-input 0 2) one gen/boolean
                                              outs (gen/vector (g-ref :datasets) 0 2)
                                              ext (gen/vector (g-ref :sources) 0 1)]
                                      (cond-> {:external-systems ext}
                                        one       (assoc :input (first ins))
                                        (not one) (assoc :inputs (zipmap [:a :b] ins))
                                        (= 1 (count outs))
                                        (assoc :output {:kind :dataset :dataset (first outs)})
                                        (< 1 (count outs))
                                        (assoc :outputs (mapv (fn [d] {:kind :dataset :dataset d})
                                                              outs))))))})

(def ^:private g-marking
  (gen/let [c (opt (gen/elements ["c1" "c2"])) rank (opt (gen/choose 0 3))
            cl (some-of principals) dis (gen/frequency [[3 (gen/return false)]
                                                        [1 (gen/return true)]])
            req (gen/frequency [[3 (gen/return false)] [1 (gen/return true)]])]
    (cond-> {:cleared cl}
      c (assoc :category c) (some? rank) (assoc :rank rank) dis (assoc :disjunctive true)
      req (assoc :required-on-projects true))))

(defn- g-entry [sec n] ((entry sec) n))

(def world
  (gen/let [es (apply gen/tuple
                      (for [[sec ns] pool n ns]
                        (gen/fmap (fn [e] [sec n e])
                                  (gen/frequency [[1 (gen/return nil)] [2 (g-entry sec n)]]))))
            ms (gen/map (gen/elements markings) g-marking {:max-elements 4})]
    (reduce (fn [W [sec n e]]
              (cond-> W e (assoc-in [sec (rid-of sec n)] (assoc e :id (rid-of sec n)))))
            {:markings ms}
            es)))

(def edit
  (gen/one-of
   [(gen/let [sec (gen/elements (keys pool)) n (gen/elements (pool sec))
              e (gen/frequency [[1 (gen/return nil)] [3 (g-entry sec n)]])]
      [[sec (rid-of sec n)] (some-> e (assoc :id (rid-of sec n)))])
    (gen/let [m (gen/elements markings) e (opt g-marking)] [[:markings m] e])]))

(defn edited [W [node e]] (if e (assoc-in W node e) (update W (first node) dissoc (second node))))

(def subject
  (gen/let [who (gen/elements principals)
            ms (gen/vector (gen/tuple (gen/elements principals) (gen/elements principals)) 0 3)
            os (gen/vector (gen/tuple (gen/elements principals)
                                      (gen/fmap #(rid-of :organizations %)
                                                (gen/elements (pool :organizations))))
                           0 2)
            gr (opt (gen/elements roles-used))
            un (gen/frequency [[6 (gen/return false)] [1 (gen/return true)]])
            di (gen/frequency [[6 (gen/return false)] [1 (gen/return true)]])
            cl (some-of markings)
            cls (gen/map (gen/elements markings)
                         (gen/let [c (gen/elements ["c1" "c2"]) k (gen/choose 0 3)]
                           {:category c :rank k})
                         {:max-elements 2})]
    {:who who :memberships ms :organizations os :global-role (if (= :unknown gr) nil gr)
     :unrestricted? un :disabled? di :clears cl :classification cls}))

(defn- of-world [W] (db/db W p/rows))

(defn- oracle [W subj]
  (policy/database (policy/schema-facts W) (policy/role-facts roles)
                   (policy/subject-facts subj)))

(defn- firsts [odb rels] (into #{} (for [rel rels t (get odb rel)] (first t))))

(defn- answers
  [odb d subj]
  (let [u     (str (:who subj))
        local (r/subject d roles subj)
        cs    (firsts odb [:container])
        xs    (firsts odb [:marked :requires :container :resource-of :derived-from])
        ops   (conj (into #{} (mapcat :operations) (vals roles)) :nope)]
    {:oracle {:blocked   (policy/blocked odb u)
              :granted   (policy/granted-operations odb u)
              :may       (set (for [o ops c xs :when (:allow? (policy/decide odb {:who u :do o
                                                                                 :on c}))]
                                [o c]))
              :covers    (set (map '?m (dl/query odb [:covers u '?m])))
              :inside    (set (map '?o (dl/query odb [:inside u '?o])))
              :holders   (into {} (for [o ops c cs :let [h (policy/holders odb o c)]
                                        :when (seq h)]
                                    [[o c] h]))}
     :local  {:blocked   (into #{} (filter #(r/blocked? d local %)) xs)
              :granted   (r/granted-operations d local cs)
              :may       (set (for [o ops c xs :when (r/may? d local o c)] [o c]))
              :covers    (:covers local)
              :inside    (:inside local)
              :holders   (into {} (for [o ops c cs :let [h (r/holders d roles o c)]
                                        :when (seq h)]
                                    [[o c] h]))}}))

(defspec local-rights-equal-the-datalog-fixpoint 200
  (prop/for-all [W world subj subject]
    (let [{:keys [oracle local]} (answers (oracle W subj) (guard/guarded (of-world W)) subj)]
      (= oracle local))))

(defspec the-effective-controls-equal-the-fixpoint 200
  (prop/for-all [W world]
    (let [odb (policy/schema-database (policy/schema-facts W))
          by  (fn [rel] (reduce (fn [m [x v]] (update m x (fnil conj #{}) v)) {} (get odb rel)))
          eff (db/view (of-world W) :policy/effective)]
      (= {:marked (by :marked) :requires (by :requires)} eff))))

(defspec stepping-the-policy-views-equals-building-afresh 300
  (prop/for-all [W world es (gen/vector edit 1 4)]
    (let [W1 (reduce edited W es)
          d1 (db/db-with (of-world W) (kg/diff W W1))]
      (= (:views (of-world W1)) (:views d1)))))

(defspec a-step-reads-only-through-keys-and-indexes 200
  (prop/for-all [W world es (gen/vector edit 1 4)]
    (let [W1 (reduce edited W es)
          d1 (db/db-with (guard/guarded (of-world W)) (kg/diff W W1))]
      (= (:views (of-world W1)) (:views d1)))))

(deftest a-marking-travels-along-the-lineage-until-an-input-stops-it
  (let [ds  (fn [n] (rid-of :datasets n))
        t   (rid-of :transforms :x1)
        W   {:datasets   {(ds :d1) {:id (ds :d1) :dataset-name :d1 :markings [:m1 :m2]}
                          (ds :d2) {:id (ds :d2) :dataset-name :d2}}
             :transforms {t {:id t :transform-name :x1
                             :input {:kind :dataset :dataset (ds :d1) :stop-propagating [:m2]}
                             :output {:kind :dataset :dataset (ds :d2)}}}}
        d   (of-world W)]
    (is (= #{:m1} (p/marked d t) (p/marked d (ds :d2))))
    (let [d' (db/db-with d (kg/diff W (assoc-in W [:transforms t :input :stop-propagating] [])))]
      (is (= #{:m1 :m2} (p/marked d' (ds :d2)))))))
