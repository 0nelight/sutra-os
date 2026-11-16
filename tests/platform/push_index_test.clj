(ns ^:unit platform.push-index-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.walk :as walk]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.plan :as plan]
            [sutra.authoring.stage :as stage]
            [sutra.world.db :as kdb]
            [sutra.world.gen.near :as gn]
            [sutra.parser.sugar :as sugar]
            [sutra.types.refs :as refs]
            [sutra.types.rid :as rid]))

(def ^:private secs (vec (keys rid/name-field-of)))

(def ^:private g-key
  (gen/one-of [(gen/elements [:a :b :c :zz "a" "b" nil])
               (gen/let [s (gen/elements secs) n (gen/elements [:a :b :c])]
                 (gn/rid-of s n))]))

(def ^:private g-touch
  (gen/let [s (gen/elements secs)
            n (gen/elements [:a :b :c])
            m (gen/elements [:a :b :c])
            t (gen/elements [:string :unnamed :keyed :spaced :shared])]
    (fn [W]
      (let [nf (rid/name-field-of s)
            r  (gn/rid-of s n)]
        (case t
          :string  (cond-> W (get-in W [s r nf]) (update-in [s r nf] name))
          :unnamed (cond-> W (get-in W [s r]) (update-in [s r] dissoc nf))
          :keyed   (assoc-in W [s (if (= n m) (name m) m)] {:id m})
          :shared  (let [r (gn/rid-of :shared-properties n)]
                     (assoc-in W [:shared-properties r]
                               {:id r :shared-property-name m :type :string}))
          :spaced  (let [sp (gn/rid-of :spaces m) p (gn/rid-of :projects n)]
                     (-> W
                         (assoc-in [:spaces sp] {:id sp :space-name m})
                         (assoc-in [:projects p] {:id p :project-name n :space sp}))))))))

(defn- one-column-keys
  [W]
  (walk/postwalk #(cond-> % (and (map? %) (vector? (:primary-key %)))
                    (update :primary-key first))
                 W))

(def ^:private world
  (gen/let [W  gn/world
            ts (gen/vector g-touch 0 8)
            one? (gen/frequency [[4 (gen/return true)] [1 (gen/return false)]])]
    (cond-> (reduce #(%2 %1) W ts) one? one-column-keys)))

(defn- index [W] (live/index-of (kdb/db W)))

(defn- outcome [f]
  (try {:ok (f)}
       (catch Exception e {:thrown [(class e) (ex-message e)]})))

(defspec a-section-looks-up-by-name-as-it-reads-whole 400
  (prop/for-all [W world ks (gen/vector g-key 1 6)]
    (let [{:keys [names]} (index W)
          whole (diff/live-by-name W)]
      (every? (fn [[kk nk]]
                (let [full (whole kk nk)
                      v    (live/section W names kk nk #(whole kk nk))
                      norm (delay (live/by-name (for [[k e] full] [(keyword (rid/->name k)) k e])))
                      nv   (live/normalized W names kk nk v norm)]
                  (every? (fn [k]
                            (and (= (get full k) (live/lookup v k))
                                 (= (contains? full k) (live/holds? v k))
                                 (= (when-let [e (find full k)] [(val e)]) (live/entry-of v k))
                                 (= (get @norm k) (live/lookup nv k))
                                 (or (not (string? k))
                                     (= (sort-by (comp str key)
                                                 (filter #(= k (rid/->name (key %))) full))
                                        (map (fn [[a b]] (clojure.lang.MapEntry/create a b))
                                             (live/named W names kk nk v k))))))
                          ks)))
              rid/name-field-of))))

(defn- crashes?
  [W]
  (contains? (outcome #(#'stage/live-objects W)) :thrown))

(defspec the-lowering-borrows-the-types-it-names 300
  (prop/for-all [W world ks (gen/vector g-key 1 6)]
    (or (crashes? W)
        (let [{:keys [names]} (index W)
              full (#'stage/live-objects W)
              v    (#'stage/live-objects W names)]
          (every? #(= (get full %) (live/lookup v %)) ks)))))

(def ^:private g-ref (gen/elements [:a :b :c :neu]))

(def ^:private g-text
  (gen/let [os  (gen/vector (gen/tuple g-ref (gen/elements [nil :a :b])) 0 3)
            ls  (gen/vector (gen/tuple (gen/elements [:a :b :c :x]) g-ref g-ref) 0 2)
            ts  (gen/vector (gen/tuple (gen/elements [:a :b :t]) (gen/elements [:a :b :c])) 0 2)
            act (gen/elements [nil :a :b :c])]
    (cond-> {}
      (seq os) (assoc :objects (into {} (for [[n p] os]
                                          [n (cond-> {:attributes [[:x :string] :a]}
                                               p (assoc :project p))])))
      (seq ls) (assoc :links (vec (for [[n f t] ls] [n f t :many-to-one {}])))
      (seq ts) (assoc :transforms (into {} (for [[n l] ts]
                                             [n {:language :sql :code "select 1"
                                                 :output {:kind :link-type :link-type l}}])))
      act      (assoc :actions {:setz {:params {:p {:type act}}
                                       :rules [{:object-type act :op :update :set [:x]
                                                :target [:get :p]}]}}))))

(def ^:private g-projected
  (gen/let [os (gen/set (gen/elements ["a" "b" "c"]))
            ls (gen/set (gen/elements ["a" "b" "c"]))
            ts (gen/set (gen/elements ["a" "b" "c" "t"]))]
    {:objects os :links ls :transforms ts :functions #{"a"} :datasets #{"b"}}))

(defn- introspect [_code body] [body nil])

(defspec a-push-plans-by-name-as-over-whole-sections 400
  (prop/for-all [W world text g-text projected (gen/one-of [(gen/return nil) g-projected])]
    (let [ix    (index W)
          opts  (cond-> {:strict? true :introspect introspect}
                  projected (assoc :strict-projected (delay projected)))
          lower #(sugar/lower-schema-against text %)
          low   (outcome #(lower (#'stage/live-objects W)))]
      (and (or (crashes? W)
               (= low (outcome #(lower (live/as-lookup
                                         (#'stage/live-objects W (:names ix)))))))
           (let [s (or (:ok low) text)]
             (and (= (outcome #(plan/plan-schema s W opts))
                     (outcome #(plan/plan-schema s W (merge opts ix))))
                  (= (outcome #(plan/plan-retirements s W opts))
                     (outcome #(plan/plan-retirements s W (merge opts ix))))))))))

(defn- old-lower
  [schema live live-vts]
  (let [project  #(or (:project (get live %)) %2)
        schema   (sugar/derive-projects schema (reify clojure.lang.ILookup
                                                 (valAt [_ k] (project k nil))
                                                 (valAt [_ k nf] (project k nf))))
        declared (set (keys (:objects schema)))
        used     (into #{} (comp (mapcat #(concat (keep :object-type
                                                        (refs/steps (:rules %)))
                                                  (keep :object-type (:edits %))
                                                  (keep :type (vals (:params %)))))
                                 (filter keyword?))
                       (vals (:actions schema)))
        context  (select-keys live (remove declared used))
        vt-lent  (apply dissoc live-vts (keys (:value-types schema)))
        lent     (cond-> schema
                   (seq context) (update :objects merge context)
                   (seq vt-lent) (update :value-types #(merge vt-lent %))
                   true          (assoc :sutra.parser.sugar/live
                                        (select-keys live
                                                     (concat (filter declared used)
                                                             (#'sugar/log-types schema)))))
        lowered  (dissoc (sugar/lower-schema lent) :sutra.parser.sugar/live)]
    (cond-> lowered
      (seq context) (update :objects #(apply dissoc % (keys context)))
      (seq vt-lent) (update :value-types #(apply dissoc % (keys vt-lent)))
      (and (seq vt-lent) (not (contains? schema :value-types))) (dissoc :value-types))))

(def ^:private g-vt-name (gen/elements [:money :pct :code :nix]))

(def ^:private g-vt-world
  (gen/let [W   world
            vts (gen/vector (gen/tuple (gen/elements [:a :b :c :d :e])
                                       (gen/elements [:money :pct :code "pct" nil])
                                       (gen/elements [:string :integer :double])
                                       (gen/elements [:rid :rid :rid :name]))
                            0 5)]
    (reduce (fn [W [n nm t keying]]
              (let [r (gn/rid-of :value-types n)
                    k (if (and (= :name keying) nm) (keyword (name nm)) r)]
                (assoc-in W [:value-types k]
                          (cond-> {:id r :type t :constraints {:len 3}}
                            nm (assoc :value-type-name nm)))))
            W vts)))

(def ^:private g-vt-text
  (gen/let [as   (gen/vector (gen/tuple (gen/elements [:x :y :z])
                                        (gen/elements [:string :integer :money :pct :nix
                                                       :code])
                                        (gen/elements [nil :money :pct :nix]))
                             0 3)
            sp   (gen/elements [nil :money :nix :pct])
            sp-t (gen/elements [nil :string :integer])
            own  (gen/vector (gen/tuple g-vt-name (gen/elements [:string :integer])) 0 2)
            has? gen/boolean]
    (cond-> {}
      (seq as) (assoc :objects
                      {:o {:attributes (vec (for [[n t v] as]
                                              (cond-> [n t] v (conj {:value-type v}))))}})
      sp       (assoc :shared-properties {:sp (cond-> {:value-type sp} sp-t (assoc :type sp-t))})
      (or has? (seq own)) (assoc :value-types (into {} (for [[n t] own] [n {:type t}]))))))

(defspec the-lowering-looks-up-the-value-types-it-names 1000
  (prop/for-all [W g-vt-world text g-vt-text]
    (let [{:keys [names]} (index W)
          full  (#'stage/live-value-types W)
          seen  (delay (#'stage/live-value-types W))
          view  (live/by-field W names :value-types :value-type-name
                               (fn [_ v] (select-keys v [:type :constraints :format])) seen
                               {:own-key? true})
          old   (outcome #(old-lower text {} full))
          new   (outcome #(sugar/lower-schema-against text {} (live/as-lookup view)
                                                      (boolean (seq (:value-types W)))))]
      (and (= old new)
           (= old (outcome #(sugar/lower-schema-against text {} full)))
           (= old (outcome #(sugar/lower-schema-against
                             text {} (live/as-lookup (#'stage/live-value-types W names))
                             (boolean (seq (:value-types W))))))
           (every? #(and (= (get full %) (live/lookup view %))
                         (= (get full % ::nf) (live/lookup view % ::nf))
                         (= (when-let [e (find full %)] [(val e)]) (live/entry-of view %)))
                   [:money :pct :code :nix "pct"])
           (or (not (realized? seen))
               (:thrown old)
               (some #(< 1 (count %))
                     (vals (group-by (fn [[k v]] (or (:value-type-name v) k))
                                     (:value-types W))))
               (some (fn [[_ v]] (nil? (:value-type-name v))) (:value-types W)))))))
