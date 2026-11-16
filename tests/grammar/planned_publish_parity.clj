(ns ^:integration grammar.planned-publish-parity
  (:require [support.system :as system]
            [clojure.test :refer [deftest use-fixtures]]
            [sutra.actions.dispatch :as c]
            [sutra.catalog.changes :as changes]
            [sutra.authoring.install.world :as onto-install]
            [sutra.actions.publish :as onto-publish]
            [sutra.store.seat :as seat]
            [sutra.store.access.scope :as ctx]
            [sutra.actions.batch :as batch]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(def N-COLS 12)
(def N-ROWS 2400)

(defn- attrs []
  (mapv (fn [i] {:attribute-name (keyword (str "col_" i)) :attribute-type :string})
        (range N-COLS)))

(def SCHEMA
  {:object-types {:widget {:object-type-name :widget :attribute-list (attrs) :plural "Widgets"}
                  :gadget {:object-type-name :gadget :attribute-list (attrs) :plural "Gadgets"}}
   :link-types   {:fits {:link-type-name :fits :from-object-type :widget
                         :to-object-type :gadget :cardinality :many-to-one}}
   :interfaces {} :transforms {} :actions {} :object-sets {} :value-types {}})

(def WORLD (@#'onto-install/normalize-schema SCHEMA))

(def ^:private W (lookup/rid-of WORLD :object-types :widget))
(def ^:private G (lookup/rid-of WORLD :object-types :gadget))
(def ^:private F (lookup/rid-of WORLD :link-types :fits))

(defn- widgets [b] (set (store/objects b W)))
(defn- fits [b] (set (store/links b F)))

(defn- values [i]
  (into {} (map (fn [k] [(keyword (str "col_" k)) (str "v" i "-" k)])) (range N-COLS)))

(defn- decl->step
  [{:keys [kind verb type row]}]
  (case [kind verb]
    [:objects :create] {:op :create :object-type type :id (:id row) :values (:values row)}
    [:objects :update] {:op :update :object-type type :target (:id row) :values (:values row)}
    [:objects :delete] {:op :delete-object :object-type type :target (:id row)}
    [:links   :create] {:op :create-link :link-type type
                        :from-id (:from-id row) :to-id (:to-id row)}))

(defn- fresh-branch!
  [prefix]
  (let [b (store/fresh-branch! prefix)]
    (store/install! b WORLD)
    b))

(defn- interpret!
  [branch decls]
  (c/submit! (ctx/store branch grants/system-authority) WORLD (mapv decl->step decls)))

(defn- publish!
  [branch decls]
  (onto-publish/publish! (ctx/store branch grants/system-authority) WORLD decls
                         (changes/publish-group (first decls)) nil))

(defn- both-ways!
  ([decls dump] (both-ways! decls dump nil))
  ([decls dump seed]
   (let [a (fresh-branch! "interp") b (fresh-branch! "plan")
         h0 {:branch (seat/seated-branch)}]
     (try
       (system/seat! (:branch h0))
       (doseq [sdecls seed] (interpret! a sdecls) (publish! b sdecls))
       (interpret! a decls)
       (let [result (publish! b decls)]
         {:interpreted (dump a)
          :planned     (dump b)
          :result      result})
       (finally
         (system/seat! (:branch h0))
         (doseq [{:keys [name]} [a b]]
           (try (store/retire! name) (catch Exception _ nil))))))))

(defn- plan-for
  ([kind verb type] (plan-for SCHEMA kind verb type nil))
  ([schema kind verb type authorize]
   (plan-for schema kind verb type authorize grants/system-authority))
  ([schema kind verb type authorize authority]
   (batch/compile-plan schema {:kind kind :verb verb :type type} authorize authority)))

(deftest planned-create-matches-interpreter
  (section "2400 widget rows: published output ≡ interpreted output")
  (let [ids   (vec (repeatedly N-ROWS uuid))
        decls (mapv (fn [i] (changes/create-object (nth ids i) W (values i))) (range N-ROWS))
        {:keys [interpreted planned result]}
        (both-ways! decls widgets)]
    (check "the group partitioned"               (> (:partitions result) 0))
    (check "every row published"                 (= N-ROWS (:published result)))
    (check "interpreter wrote all rows"          (= N-ROWS (count interpreted)))
    (check "planned row count matches"           (= (count interpreted) (count planned)))
    (check "published rows are byte-identical"   (= interpreted planned))))

(deftest planned-connect-matches-interpreter
  (section "fits edges: derived link ids from link-id-fn ≡ link-id-for")
  (let [ws (vec (repeatedly N-ROWS uuid))
        gs (vec (repeatedly N-ROWS uuid))
        decls (mapv (fn [i] (changes/connect F (nth ws i) (nth gs i))) (range N-ROWS))
        seed  [(mapv #(changes/create-object % W (values 0)) ws)
               (mapv #(changes/create-object % G (values 0)) gs)]
        {:keys [interpreted planned]}
        (both-ways! decls fits seed)]
    (check "interpreter wrote all edges"         (= N-ROWS (count interpreted)))
    (check "edges (incl. derived ids) identical" (= interpreted planned))))

(deftest planned-update-matches-interpreter
  (section "a PARTIAL update — the retraction / §7b supplement shape")
  (let [ids   (vec (repeatedly 50 uuid))
        seed  [(mapv #(changes/create-object % W (values 0)) ids)]
        decls (mapv #(changes/update-object % W {:col_0 "patched"}) ids)
        {:keys [interpreted planned result]}
        (both-ways! decls widgets seed)]
    (check "every row published"                 (= (count ids) (:published result)))
    (check "the patch landed"
           (every? #(some #{"patched"} (vals (:values %))) planned))
    (check "untouched columns survived"
           (every? #(some #{"v0-1"} (vals (:values %))) planned))
    (check "identical to the interpreted write"  (= interpreted planned))))

(deftest a-published-update-of-an-absent-row-mints-no-ghost
  (section "an update whose target does not exist must write NOTHING")
  (let [live   (vec (repeatedly 5 uuid))
        absent (uuid)
        b      (fresh-branch! "ghost")
        h0     {:branch (seat/seated-branch)}]
    (try
      (system/seat! (:branch h0))
      (publish! b (mapv #(changes/create-object % W (values 0)) live))
      (publish! b (conj (mapv #(changes/update-object % W {:col_0 "patched"}) live)
                        (changes/update-object absent W {:col_0 "patched"})))
      (let [rows (widgets b)]
        (check "no row was minted for the absent id" (= (count live) (count rows)))
        (check "and the live rows still took the patch"
               (every? #(some #{"patched"} (vals (:values %))) rows)))
      (finally
        (system/seat! (:branch h0))
        (try (store/retire! b) (catch Exception _ nil))))))

(deftest two-updates-of-one-row-accumulate-per-column
  (section "a repeated id under UPDATE merges COLUMNS — it does not last-win the ROW")
  (let [ids   (vec (repeatedly 50 uuid))
        seed  [(mapv #(changes/create-object % W (values 0)) ids)]
        decls (into (mapv #(changes/update-object % W {:col_0 "first"}) ids)
                    (mapv #(changes/update-object % W {:col_1 "second"}) ids))
        {:keys [interpreted planned]}
        (both-ways! decls widgets seed)]
    (check "both columns survived on the planned path"
           (every? #(and (some #{"first"} (vals (:values %))) (some #{"second"} (vals (:values %))))
                   planned))
    (check "identical to the interpreted write"  (= interpreted planned))))

(deftest planned-delete-matches-interpreter
  (section "a delete group — the tombstone retraction shape")
  (let [ids   (vec (repeatedly 50 uuid))
        seed  [(mapv #(changes/create-object % W (values 0)) ids)]
        decls (mapv #(changes/delete-object % W) (drop 20 ids))
        {:keys [interpreted planned]}
        (both-ways! decls widgets seed)]
    (check "the survivors are what is left"      (= 20 (count planned)))
    (check "identical to the interpreted write"  (= interpreted planned))))

(deftest duplicate-ids-keep-last-wins
  (section "a repeated id must resolve last-wins on BOTH paths (id-hash partitioning)")
  (let [ids   (vec (repeatedly (quot N-ROWS 2) uuid))
        decls (into (mapv (fn [i] (changes/create-object (nth ids i) W (values i)))
                          (range (count ids)))
                    (mapv (fn [i] (changes/create-object (nth ids i) W (values (+ 100000 i))))
                          (range (count ids))))
        {:keys [interpreted planned]}
        (both-ways! decls widgets)]
    (check "one row per distinct id"             (= (count ids) (count interpreted)))
    (check "same winner on both paths"           (= interpreted planned))))

(deftest an-authored-action-cannot-shadow-the-publish-path
  (section "publishing resolves NO action — an authored `create-widget` is irrelevant here")
  (let [shadowed (assoc-in SCHEMA [:actions :create-widget]
                           {:params {:why {:required true}}
                            :submission-criteria [{:test {} :pred [:= 1 2]}]
                            :rules [{:op :notify :channel "x" :payload {}}]})
        plan (plan-for shadowed :objects :create :widget nil)
        row  ((:row-fn plan) {:id (uuid) :values (values 1)})]
    (check "the plan compiles regardless"        (some? plan))
    (check "and writes the row it was given"     (= (values 1) (:values row)))))

(deftest authorization-stays-per-row-when-markings-are-denied
  (section "a non-empty denied-marking set keeps the ACL verdict per-row")
  (let [calls (atom [])
        authz (fn [op target _] (swap! calls conj [op (:id target)]))]
    (check "no denied markings ⇒ hoisted, no per-row fn"
           (nil? (:authorize-row (plan-for SCHEMA :objects :create :widget authz))))
    (check "the group-level RBAC call still happened once"
           (= [[:create nil]] @calls))
    (let [per-row (:authorize-row (plan-for SCHEMA :objects :create :widget authz
                                            {:denied-markings #{:secret}}))]
      (check "a denied marking ⇒ a per-row check"  (fn? per-row))
      (per-row {:id "row-1"})
      (check "which authorizes THAT row"
             (= [:create "row-1"] (last @calls))))))

(deftest planner-enforces-constraints
  (section "a constraint violation must still abort the group")
  (let [schema (assoc-in SCHEMA [:object-types :widget :attribute-list]
                         (assoc-in (attrs) [0 :opts] {:required true}))
        plan   (plan-for schema :objects :create :widget nil)
        row-fn (:row-fn plan)]
    (check "plan compiled" (some? plan))
    (check "a row missing the required attr throws"
           (try (row-fn {:id (uuid) :values (dissoc (values 1) :col_0)}) false
                (catch clojure.lang.ExceptionInfo _ true)))
    (check "a complete row passes"
           (some? (row-fn {:id (uuid) :values (values 1)})))
    (check "the same row is fine as a PARTIAL update"
           (some? ((:row-fn (plan-for schema :objects :update :widget nil))
                   {:id (uuid) :values (dissoc (values 1) :col_0)})))))

(deftest an-unknown-type-is-refused-not-declined
  (section "a publish decl the write path cannot serve is a bug, not a fallback")
  (check "an unknown object-type throws"
         (try (plan-for SCHEMA :objects :create :nonesuch nil) false
              (catch clojure.lang.ExceptionInfo _ true)))
  (check "an unknown [kind verb] throws"
         (try (plan-for SCHEMA :links :update :fits nil) false
              (catch clojure.lang.ExceptionInfo _ true)))
  (check "a non-publish decl has no group"
         (nil? (changes/publish-group {:op :effect :fn :create-widget :args {}}))))

(def MIXED
  {:object-types
   {:thing {:object-type-name :thing
            :attribute-list [{:attribute-name :label  :attribute-type :string}
                             {:attribute-name :count  :attribute-type :long}
                             {:attribute-name :ratio  :attribute-type :double}
                             {:attribute-name :menge  :attribute-type :number}
                             {:attribute-name :active :attribute-type :boolean}]
            :plural "Things"}}
   :link-types {} :interfaces {} :transforms {} :actions {}
   :object-sets {} :value-types {}})

(deftest a-declared-type-round-trips-the-catch-all-does-not
  (section "what survives a publish → read, and what the changelog still cannot hash")
  (let [h0 {:branch (seat/seated-branch)}
        branch (let [nm (store/fresh-branch! "rt")]
             (store/install! nm MIXED)
             nm)
        id (uuid)
        written {:label "row-3" :count 3 :ratio (double 3/7) :menge 3 :active false}]
    (try
      (system/seat! (:branch h0))
      (onto-publish/publish! (ctx/store branch grants/system-authority) MIXED
                             [(changes/create-object id :thing written)]
                  {:kind :objects :verb :create :type :thing} nil)
      (let [live (:values (first (store/objects branch :thing)))]
        (check "a :string survives exactly"
               (= (:label written) (:label live)))
        (check "a declared :long survives exactly — class and value"
               (and (instance? Long (:count live))
                    (= (:count written) (:count live))))
        (check "a declared :double survives exactly — no scale to round to"
               (and (instance? Double (:ratio live))
                    (= (:ratio written) (:ratio live))))
        (check "a :boolean survives exactly — Postgres has the type, MySQL had TINYINT"
               (and (instance? Boolean (:active live))
                    (= (:active written) (:active live))))
        (check "the retired catch-all :number does not — DECIMAL(38,10) underneath"
               (and (instance? java.math.BigDecimal (:menge live))
                    (not= (:menge written) (:menge live))))
        (check "the read carries nothing the write did not have"
               (= (dissoc written :menge) (dissoc live :menge))))
      (finally
        (system/seat! (:branch h0))
        (try (store/retire! branch) (catch Exception _ nil))))))

(deftest unknown-attribute-is-rejected
  (section "an attribute the type does not declare must throw, as do-create does")
  (let [row-fn (:row-fn (plan-for :objects :create :widget))]
    (check "unknown column throws"
           (try (row-fn {:id (uuid) :values (assoc (values 1) :nope "x")}) false
                (catch clojure.lang.ExceptionInfo _ true)))))
