(ns sutra.world.gen
  (:require [clojure.test.check.generators :as gen]
            [sutra.world.delta :as d]
            [sutra.world.gate :as gate]
            [sutra.ref.install :as ri]))

(defn sigma
  [prefix]
  (let [n (atom 0)] {:sigma {:fresh-uuid (fn [] (str prefix "-" (swap! n inc)))}}))

(def ^:private names [:a :b :c :d])

(def ^:private g-name (gen/elements names))

(def ^:private g-names (gen/vector g-name 0 2))

(defn- g-opt [g] (gen/one-of [(gen/return nil) g]))

(def ^:private g-attr
  (gen/tuple (gen/elements [:x :y :z :größe :x-y :n?])
             (gen/elements [:string :integer :timestamp])))

(def ^:private g-attrs (gen/vector g-attr 0 3))

(defn- body [m] (into {} (remove (comp nil? val)) m))

(def ^:private g-constraint
  (gen/let [n g-name to g-name card (gen/elements [:one :many]) req gen/boolean]
    {:link-name n :to-object-type to :cardinality card :required req}))

(def object-type-body
  (gen/let [n g-name attrs g-attrs impls g-names title (g-opt (gen/elements [:x :y :q]))
            project (g-opt g-name)
            edit (gen/frequency
                  [[5 (gen/return nil)]
                   [1 (gen/return {:strategy :apply-most-recent :timestamp-attr :x})]])]
    (body {:name n :attributes attrs :implements impls :title title :project project
           :edit-conflict edit})))

(def ^:private g-input
  (gen/one-of [(gen/fmap (fn [n] {:kind :object-type :object-type n}) g-name)
               (gen/fmap (fn [n] {:kind :transform :transform n}) g-name)
               (gen/fmap (fn [n] {:kind :dataset :dataset n}) g-name)]))

(def ^:private g-edit
  (gen/frequency [[4 (gen/return nil)]
                  [1 (gen/return {:strategy :apply-most-recent :timestamp-attr :x})]]))

(def ^:private g-output
  (gen/one-of [(gen/fmap (fn [n] {:kind :dataset :dataset n})
                         (gen/elements [:a :b :c :d :e :f]))
               (gen/return {:kind :dataset})
               (gen/let [n g-name e g-edit]
                 (body {:kind :object-type :object-type n :edit-conflict e}))]))

(def ^:private g-outputs
  (gen/let [os (gen/vector (gen/tuple (gen/elements [:o1 :o2 :a]) g-output) 1 3)]
    (mapv (fn [[n o]] (assoc o :name n)) os)))

(def ^:private transform-body
  (gen/let [n g-name lang (gen/elements [:sql :python])
            ins (gen/map g-name g-input {:max-elements 2})
            form (gen/elements [:output :outputs :output-columns :none])
            out g-output outs g-outputs cols g-attrs project (g-opt g-name)]
    (body (merge {:name n :language lang :query "select 1" :inputs ins :project project}
                 (case form
                   :output         {:output out}
                   :outputs        {:outputs outs}
                   :output-columns {:output-columns cols}
                   :none           {})))))

(def ^:private g-rule
  (gen/let [op (gen/elements [:create-object :modify-object :create-link])
            ot g-name lt g-name]
    (if (= :create-link op) {:op op :link-type lt} {:op op :object-type ot})))

(def ^:private action-body
  (gen/let [n g-name on g-name p g-name rules (gen/vector g-rule 0 2)]
    {:name n :on on :params {:p {:type p}} :rules rules}))

(def ^:private bodies
  {:space       (gen/let [n g-name] {:name n})
   :project     (gen/let [n g-name s (g-opt g-name)] (body {:name n :space s}))
   :interface   (gen/let [n g-name ext g-names props g-attrs cs (gen/vector g-constraint 0 2)]
                  {:name n :extends ext :properties props :link-constraints cs})
   :object-type object-type-body
   :link-type   (gen/let [n g-name from g-name to g-name
                          card (gen/elements [:many-to-one :one-to-many :many-to-many])
                          sat (g-opt (gen/let [i g-name c g-name] {:interface i :constraint c}))]
                  (body {:name n :from from :to to :cardinality card
                         :satisfies sat}))
   :transform   transform-body
   :action      action-body
   :value-type  (gen/let [n g-name t (gen/elements [:string :integer])] {:name n :type t})
   :shared-property (gen/let [n g-name vt (g-opt g-name) project (g-opt g-name)]
                      (body {:name n :type :string :value-type vt :project project}))
   :dataset     (gen/let [n g-name ot (g-opt g-name) project (g-opt g-name)]
                  (body {:name n :object-type ot :project project}))
   :function    (gen/let [n g-name ot (g-opt g-name)]
                  (body {:name n :language :python :target-type ot}))
   :repository  (gen/let [n g-name project (g-opt g-name)] (body {:name n :project project}))
   :graph       (gen/let [n g-name] {:name n})})

(def create
  (gen/let [tag (gen/elements (keys bodies)) b (bodies tag)] [tag b]))

(def step
  (gen/let [tag  (gen/elements (keys bodies))
            verb (gen/frequency [[6 (gen/return :create)] [2 (gen/return :update)]
                                 [1 (gen/return :delete)]])
            b    (bodies tag)]
    [verb tag (if (= :delete verb) (select-keys b [:name]) b)]))

(defn install
  [cx W [verb tag b]]
  (try (:world (ri/install {:world W :installed W} cx verb tag b))
       (catch clojure.lang.ExceptionInfo _ nil)))

(defn installed
  [steps]
  (let [cx (sigma "w")]
    (reduce (fn [W s] (or (install cx W s) W)) (ri/normalise cx {} nil) steps)))

(def world (gen/fmap installed (gen/vector step 0 14)))

(defn- one-per-name? [W sec nf]
  (every? #(= 1 %) (vals (frequencies (keep nf (vals (get W sec)))))))

(defn- one-per-scope? [W]
  (every? (fn [sec]
            (let [nf (ri/name-field sec)]
              (every? #(= 1 %)
                      (vals (frequencies (for [e (vals (get W sec)) :when (get e nf)]
                                           [(get e nf) (#'ri/scope-of W e)]))))))
          ri/rid-kinds))

(defn oracle-defined?
  [W]
  (and (every? #(keyword? (:transform-name %)) (vals (:transforms W)))
       (one-per-name? W :link-types :link-type-name)
       (one-per-name? W :value-types :value-type-name)
       (one-per-scope? W)))

(defn diff
  [W0 W1]
  (d/around (d/frame-delta W0 W1)
            (vec (for [sec (distinct (concat (keys W0) (keys W1)))
                       :when (or (map? (get W0 sec)) (map? (get W1 sec)))
                       :let [a (when (map? (get W0 sec)) (get W0 sec))
                             b (when (map? (get W1 sec)) (get W1 sec))]
                       k (distinct (concat (keys a) (keys b)))
                       c (d/entry-delta [sec k] (get a k) (get b k))]
                   c))))

(defn- rid-of [sec n] (ri/rid (ri/tag-of sec) (name n)))

(defn- g-rid [sec] (gen/fmap #(rid-of sec %) g-name))

(def ^:private raw-fields
  {:projects          {:space (g-opt g-name) :project-name g-name}
   :spaces            {:space-name g-name}
   :interfaces        {:extends (gen/vector (g-rid :interfaces) 0 2)
                       :interface-name g-name
                       :property-list (gen/fmap #(mapv gate/attr-row %) g-attrs)}
   :object-types      {:implements (gen/vector (g-rid :interfaces) 0 2)
                       :object-type-name g-name
                       :title-key (g-opt (gen/elements [:x :y]))
                       :project (g-opt (gen/one-of [g-name (g-rid :projects)]))
                       :attribute-list (gen/fmap #(mapv gate/attr-row %) g-attrs)}
   :link-types        {:from-object-type (g-rid :object-types)
                       :to-object-type (g-rid :object-types)
                       :link-type-name g-name}
   :transforms        {:transform-name g-name
                       :inputs (gen/map g-name
                                        (gen/one-of
                                         [(gen/fmap (fn [t] {:kind :transform :transform t})
                                                    (g-rid :transforms))
                                          (gen/fmap (fn [o] {:kind :object-type :object-type o})
                                                    (g-rid :object-types))
                                          (gen/fmap (fn [m] {:kind :language-model :model m})
                                                    (gen/elements ["m1" "m2"]))])
                                        {:max-elements 2})}
   :llm-models        {:model-name g-name}
   :value-types       {:value-type-name g-name :type (gen/elements [:string :integer])}
   :shared-properties {:shared-property-name g-name :value-type (g-opt g-name)
                       :type (gen/elements [:string :integer])}})

(def raw-change
  (gen/let [sec  (gen/elements (keys raw-fields))
            n    g-name
            drop (gen/frequency [[1 (gen/return true)] [8 (gen/return false)]])
            [f v] (gen/bind (gen/elements (keys (raw-fields sec)))
                            (fn [f] (gen/tuple (gen/return f) ((raw-fields sec) f))))]
    (let [k (if (= :llm-models sec) (name n) (rid-of sec n))]
      [[sec k] (when-not drop {f v})])))

(defn apply-raw
  [W [[sec k :as node] fv]]
  (if (nil? fv)
    (if (contains? W sec) (update W sec dissoc k) W)
    (let [[f v] (first fv)
          e     (merge {:id k} (get-in W node))]
      (assoc-in W node (if (nil? v) (dissoc e f) (assoc e f v))))))
