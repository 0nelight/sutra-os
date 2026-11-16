(ns ^:integration ontology.edit-provenance-reconcile
  (:require [clojure.test :refer [deftest use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.builds.import.changelog :as changelog]
            [sutra.store.world :as world]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))
(defn- ot [nm attrs] {:object-type-name nm :id nm
                      :attribute-list (mapv (fn [[n t]] {:attribute-name n :attribute-type t})
                                            attrs)})

(defn- row-exists? [b T id] (some? (store/object b T id)))

(defn- col-val [b T id attr] (get-in (store/object b T id) [:values attr]))

(defn- source!
  [b t-name ids]
  (dsets/write-rows! (ctx/system-store b) t-name :snapshot
                      (mapv (fn [id] {:id id :values {}}) ids)))

(defn- index!
  [b T rows]
  (world/index! (ctx/system-store b) (mapv #(store/create-object T %) rows)))

(defn- removals [b T t-name opts]
  (changelog/object-removals (ctx/system-store b) T t-name opts))

(defn- boundary [b] (dsets/current-version (ctx/system-store b)))

(defn- reconcile!
  [b T {:keys [removed]}]
  (when (seq removed)
    (world/index! (ctx/system-store b) (mapv #(store/delete-object T %) removed))))

(defn- with-thing [prefix attrs f]
  (let [b (str prefix "-" (subs (uuid) 0 8))]
    (try
      (store/fork! b)
      (store/install! b {:object-types {:thing (ot :thing attrs)} :link-types {}})
      (f b)
      (finally (try (store/retire! b) (catch Exception _ nil))))))

(deftest deletion-candidates-preview-guards-reindex
  (with-thing "cand" [[:name :string]]
    (fn [b]
      (let [S (uuid) U (uuid) O (uuid) tname :thing-src]
        (section "seed: S in source, U user-created (an edit), O a source-orphan")
        (source! b tname [S])
        (index! b :thing
                [{:id S :values {:name "from-source"}} {:id O :values {:name "source-orphan"}}])
        (store/insert-object! b :thing {:id U :values {:name "user-created"}})

        (section "preview: candidates = only the orphan (guard would refuse without --force)")
        (let [cands (set (:removed (removals b :thing tname {:propagate-empty? true})))]
          (check "orphan O is a deletion candidate"          (contains? cands O))
          (check "source row S is NOT a candidate (backed)"  (not (contains? cands S)))
          (check "user row U is NOT a candidate (an edit created it)" (not (contains? cands U)))
          (check "candidates = exactly {O}"                  (= #{O} cands)))

        (section "the empty-source VALVE is the changelog's call, not the serving tier's")
        (source! b tname [])
        (check "empty producer + valve ⇒ EMPTY delta (a flaky read deletes nothing)"
               (= [] (:removed (removals b :thing tname nil))))
        (check "empty producer + authoritative override ⇒ every un-exempt row goes"
               (= #{S O} (set (:removed (removals b :thing tname {:propagate-empty? true})))))))))

(deftest changelog-derivation-tracks-the-transaction
  (with-thing "clog" [[:name :string]]
    (fn [b]
      (let [S1 (uuid) S2 (uuid) S3 (uuid) U (uuid) O (uuid)
            tname :thing-src newt :thing-src-v2]
        (section "seed: source has S1 S2 S3; the index has those plus a user row")
        (source! b tname [S1 S2 S3])
        (index! b :thing (for [id [S1 S2 S3]] {:id id :values {:name "from-source"}}))
        (store/insert-object! b :thing {:id U :values {:name "user-created"}})

        (section "one build: producer drops S3, changelog names S3, index applies")
        (let [low (boundary b)]
          (source! b tname [S1 S2])
          (let [delta (removals b :thing tname {:propagate-empty? true :low low})]
            (check "derived from the transaction diff, not a full compare"
                   (= :changelog (:via delta)))
            (check "the changelog names exactly the row the producer dropped"
                   (= [S3] (:removed delta)))
            (reconcile! b :thing delta)))
        (check "S3 swept from the index"       (not (row-exists? b :thing S3)))
        (check "S1 S2 still served"            (and (row-exists? b :thing S1)
                                                    (row-exists? b :thing S2)))
        (check "the user row survives (exempt under either derivation)"
               (row-exists? b :thing U))

        (section "an un-indexed orphan is INVISIBLE to the diff — and that is correct")
        (index! b :thing [{:id O :values {:name "drifted-in"}}])
        (let [low (boundary b)]
          (check "changelog derivation does not see the drifted row"
                 (= [] (:removed (removals b :thing tname {:propagate-empty? true :low low}))))
          (let [heal (removals b :thing tname {:propagate-empty? true})]
            (check "no boundary named ⇒ FULL COMPARE (what the reindex verb runs)"
                   (= :full-compare (:via heal)))
            (check "and the full compare does see it"
                   (= [O] (:removed heal)))))

        (section "a producer SWAP cannot slip through: no history at `low` ⇒ full compare")
        (let [low (boundary b)]
          (source! b newt [S1])
          (let [delta (removals b :thing newt {:propagate-empty? true :low low})]
            (check "brand-new backing ⇒ derivation falls back to the full compare"
                   (= :full-compare (:via delta)))
            (check "so the rows the OLD producer left behind are still swept"
                   (= #{S2 O} (set (:removed delta))))))))))

(deftest forgetting-edits-resets-to-source
  (with-thing "drop" [[:name :string]]
    (fn [b]
      (let [S (uuid) U (uuid) tname :thing-src]
        (section "seed: the index has S; source has only S; U is user-created")
        (source! b tname [S])
        (index! b :thing [{:id S :values {:name "from-source"}}])
        (store/insert-object! b :thing {:id U :values {:name "user-created"}})

        (section "provenance is readable, then forgotten")
        (let [prov #(world/edit-provenance (ctx/system-store b) :thing {})]
          (check "edit provenance names the user row before forgetting"
                 (= [U] (mapv :pk (prov))))
          (world/forget-edits! (ctx/system-store b) [:thing])
          (check "edit provenance is empty after forgetting"
                 (empty? (prov))))

        (section "reconcile now sweeps the un-exempted user row; source row survives")
        (reconcile! b :thing (removals b :thing tname {:propagate-empty? true}))
        (check "source row S survives"                       (row-exists? b :thing S))
        (check "user row U swept after its edits are forgotten" (not (row-exists? b :thing U)))))))

(deftest t0-t14-one-object-through-time
  (with-thing "t14" [[:col1 :string] [:col2 :string] [:col3 :string]]
    (fn [b]
      (let [P (uuid) tname :thing-src
            import! (fn [ids vals]
                      (source! b tname ids)
                      (when (seq ids) (index! b :thing (for [id ids] {:id id :values vals})))
                      (reconcile! b :thing (removals b :thing tname {:propagate-empty? true})))
            act!    (fn [op vals]
                      (store/commit-ops! b [(case op
                                              :delete-object (store/delete-object :thing P)
                                              :update-object (store/update-object :thing P vals)
                                              (store/create-object :thing {:id P :values vals}))]))
            v       (fn [attr] (col-val b :thing P attr))
            there?  (fn [] (row-exists? b :thing P))]
        (section "T0–T2 · the datasource alone: appear, vanish, return")
        (import! [P] {:col1 "val1" :col2 "val2"})
        (check "T0 object present" (there?))
        (import! [] {})
        (check "T1 row left the datasource ⇒ object gone" (not (there?)))
        (import! [P] {:col1 "val1" :col2 "val2"})
        (check "T2 row is back ⇒ object back" (there?))

        (section "T3–T6 · a MODIFIED row still belongs to the datasource")
        (act! :update-object {:col2 "newVal2"})
        (check "T3 the user's column holds" (= "newVal2" (v :col2)))
        (import! [] {})
        (check "T4 a modified row that leaves the datasource GOES" (not (there?)))
        (import! [P] {:col1 "val1" :col2 "val2"})
        (check "T5 it returns …" (there?))
        (check "T5 … carrying the earlier user edit" (= "newVal2" (v :col2)))
        (import! [P] {:col1 "newVal1" :col2 "val2"})
        (check "T6 an UNedited column takes the source update" (= "newVal1" (v :col1)))
        (check "T6 the edited one does not" (= "newVal2" (v :col2)))

        (section "T7–T8 · a user DELETE outranks a datasource that still emits the row")
        (act! :delete-object nil)
        (check "T7 gone though the source still has it" (not (there?)))
        (import! [P] {:col1 "newVal1" :col2 "val2" :col3 nil})
        (check "T8 and it stays gone across imports" (not (there?)))

        (section "T9–T12 · a user CREATE detaches the row from its datasource")
        (act! :create-object {:col3 "val3"})
        (check "T9 the object exists again" (there?))
        (check "T9 with the created value" (= "val3" (v :col3)))
        (check "T9 and NOT the source's other columns" (nil? (v :col1)))
        (import! [P] {:col1 "newVal1" :col2 "newVal2" :col3 "newVal3"})
        (check "T10 a source update is no longer considered" (= "val3" (v :col3)))
        (check "T10 the detachment holds for every column" (nil? (v :col1)))
        (act! :update-object {:col2 "newVal22"})
        (check "T11 a modify on a user-owned row still applies" (= "newVal22" (v :col2)))
        (import! [] {})
        (check "T12 a user-CREATED row survives leaving the datasource" (there?))
        (check "T12 … with its values intact" (= "newVal22" (v :col2)))

        (section "T13–T14 · the end of the line")
        (import! [P] {:col1 "newVal1" :col2 "newVal2" :col3 "newVal3"})
        (act! :delete-object nil)
        (check "T13 the user deletes it again" (not (there?)))
        (check "T14 a modify on a deleted object fails"
               (try (act! :update-object {:col2 "x"})
                    (not (there?))
                    (catch Exception _ true)))))))

(deftest the-edit-surface-finds-what-the-writer-wrote
  (let [b   (str "surf-" (subs (uuid) 0 8))
        U   (uuid) D (uuid)
        rid (str "ri.sutra.main.object-type." (uuid))
        W   {:object-types {rid (ot :thing [[:name :string]])} :link-types {}}]
    (try
      (store/fork! b)
      (store/install! b W)
      (index! b rid [{:id U :values {:name "from-source"}} {:id D :values {:name "from-source"}}])

      (section "an UPDATE written under the RID spelling, a DELETE under the name")
      (store/update-object! b rid U {:name "user-edited"})
      (store/delete-object! b :thing D)

      (let [{:keys [properties types]} (world/edit-surface (ctx/system-store b) W)]
        (check "the RID-spelled edit names its edited COLUMN"
               (= #{[:thing :name]} properties))
        (check "and the type is edited at the coarse grain too"
               (= #{:thing} types)))
      (check "a NAME-spelled delete resolves as well, and names no property"
             (= {(str D) {:deleted? true}}
                (update-vals (select-keys (world/edited (ctx/system-store b) rid) [(str D)])
                             #(select-keys % [:deleted? :attrs]))))
      (finally (try (store/retire! b) (catch Exception _ nil))))))
