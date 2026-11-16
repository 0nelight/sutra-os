(ns sutra.world.gen.near
  (:require [clojure.test.check.generators :as gen]
            [sutra.ref.install :as ri]))

(def ^:private names [:a :b :c])

(def ^:private g-name (gen/elements names))

(defn rid-of [sec n] (ri/rid (ri/tag-of sec) (name n)))

(defn- g-ref
  [& secs]
  (gen/frequency [[4 (gen/let [s (gen/elements secs) n g-name] (rid-of s n))]
                  [2 g-name]
                  [1 (gen/return nil)]]))

(defn- g-opt [g] (gen/frequency [[1 (gen/return nil)] [2 g]]))

(defn- g-vec [g n] (gen/vector g 0 n))

(defn- body [m] (into {} (remove (comp nil? val)) m))

(def ^:private g-attr
  (gen/let [n (gen/elements [:x :y :z])
            t (gen/elements [:string :integer :boolean])
            derived (gen/frequency [[4 (gen/return nil)] [1 (gen/return {:expression [:+ 1 1]})]])
            fk (gen/frequency [[4 (gen/return nil)] [1 g-name]])]
    (body {:attribute-name n :attribute-type t :derived derived
           :opts (not-empty (body {:fk fk :derived derived}))})))

(def ^:private g-api (g-opt (gen/elements [:p :q])))

(def ^:private g-project
  (gen/frequency [[2 (gen/return nil)] [2 (gen/fmap #(rid-of :projects %) g-name)] [1 g-name]]))

(def ^:private g-end
  (gen/frequency [[6 (gen/fmap #(rid-of :object-types %) g-name)]
                  [2 (gen/fmap #(rid-of :interfaces %) g-name)]
                  [1 g-name]
                  [1 (gen/return nil)]]))

(def ^:private g-output
  (gen/one-of
   [(gen/let [l (g-ref :link-types) fk (gen/frequency [[3 (gen/return :x)]
                                                       [1 (gen/elements [nil :y :q])]])
              mc (gen/frequency [[5 (gen/return nil)] [1 (gen/return :x)]])
              fc (g-opt (gen/return :f)) tc (g-opt (gen/return :t))]
      (body {:kind :link-type :link-type l :fk-attribute fk :match-column mc
             :from-key-column fc :to-key-column tc}))
    (gen/let [t (g-ref :object-types) r (gen/elements [nil :supplement :primary])
              pk (g-opt (gen/elements [[:x] [:y]])) cm (g-opt (gen/elements [{:x "x"} {:q "q"}]))
              n (g-opt g-name)]
      (body {:kind :object-type :object-type t :role r :primary-key pk :column-map cm :name n}))
    (gen/let [d (g-ref :datasets) n (g-opt g-name)]
      (body {:kind :dataset :dataset d :name n}))
    (gen/fmap (fn [m] {:kind :media-set :media-set m}) (g-ref :media-sets))]))

(def ^:private g-input
  (gen/one-of
   [(gen/fmap (fn [t] {:kind :object-type :object-type t}) g-end)
    (gen/fmap (fn [t] {:kind :transform :transform t}) (g-ref :transforms))
    (gen/fmap (fn [d] {:kind :dataset :dataset d}) (g-ref :datasets))
    (gen/return {:kind :datasource :datasource :src})
    (gen/return {:kind :bogus})]))

(def ^:private g-version (gen/elements [nil "1.0.0" "1.2.0" "0.1.0" "x"]))

(def ^:private g-rule
  (gen/one-of
   [(gen/let [op (gen/elements [:create :update :delete-object]) t (g-ref :object-types)]
      {:op op :object-type t})
    (gen/fmap (fn [l] {:op :create-link :link-type l}) (g-ref :link-types))
    (gen/let [f g-name v g-version up (g-opt gen/boolean)]
      (body {:op :apply-edit-function :function f :version v :auto-upgrade up}))
    (gen/let [f g-name v g-version]
      (body {:op :invoke-function :function f :version v}))
    (gen/let [t (g-ref :object-types) p (gen/elements [:x :y]) v (gen/elements [:p :r 1])]
      {:op :update :object-type t :values {p v}})
    (gen/return {:op :frobnicate})]))

(def ^:private g-target
  (gen/one-of [(g-ref :object-types :link-types)
               (gen/fmap (fn [t] {:target-object-type t}) (g-ref :object-types))
               (gen/fmap (fn [l] {:target-link-type l}) (g-ref :link-types))
               (gen/return {:shape :odd})]))

(def ^:private g-param-type
  (gen/one-of [(gen/elements [:string :integer :boolean :nonsense])
               (g-ref :object-types :link-types :interfaces)]))

(def ^:private g-param
  (gen/let [t g-param-type nm (gen/elements [nil "" "P"]) ex (gen/elements [nil nil :hidden])
            d (gen/frequency [[3 (gen/return nil)]
                              [1 (gen/let [f (gen/elements [:p :r]) pr (gen/elements [:x :y])]
                                   {:from f :property pr})]])]
    (body {:type t :name nm :exposure ex :default d})))

(def ^:private g-layout
  (gen/let [ext (gen/elements [nil :e]) app g-name with (gen/elements [{} {:e 1} {:f 1}])
            tabs (gen/vector (gen/let [id (gen/elements [:t1 :t2]) r (gen/elements [:r1 :r2])]
                               {:id id :visibility {:roles [r]}})
                             0 2)]
    (cond-> {:vars (if ext {:v {:external-id ext}} {})
             :children [{:on-click {:do :open-app :app app :with with}}]}
      (seq tabs) (assoc :tabs tabs))))

(def ^:private g-sidebar
  (gen/fmap (fn [xs] [{:items xs}])
            (g-vec (gen/one-of [(gen/fmap (fn [a] {:app a}) g-name)
                                (gen/fmap (fn [a] {:action a}) g-name)])
                   2)))

(def ^:private g-releases
  (g-opt (gen/let [vs (gen/vector (gen/elements ["1.0.0" "1.1.0" "2.0.0"]) 0 2)]
           (into {} (map (fn [v] [v {:code v}])) vs))))

(def ^:private g-object-set
  (gen/let [root (g-ref :object-types :interfaces) inc (g-vec (g-ref :link-types) 2)
            slots (g-vec (gen/elements [:p {:name :p} :r]) 2)
            where (gen/elements [nil [:= :x 1] [:= :x [:param :p]] [:= :y [:param :s]]])]
    (body {:root root :include (not-empty inc) :param-slots (not-empty slots) :where where})))

(def ^:private fields
  {:object-types {:object-type-name g-name :attribute-list (g-vec g-attr 2)
                  :implements (g-vec (g-ref :interfaces) 2)
                  :resolve-entity (g-opt (gen/return true))
                  :plural (gen/elements [nil "" "As"]) :visibility (gen/elements [nil nil :hidden])
                  :primary-key (g-opt (gen/return [:x])) :project g-project
                  :dataset (g-opt (g-ref :datasets))}
   :interfaces   {:interface-name g-name :extends (g-vec (g-ref :interfaces) 2)
                  :property-list (g-vec g-attr 1)}
   :link-types   {:link-type-name g-name :project g-project
                  :from-object-type g-end
                  :to-object-type g-end
                  :cardinality (gen/frequency [[3 (gen/elements [:one-to-many :many-to-one])]
                                               [2 (gen/elements [:many-to-many :one-to-one])]
                                               [1 (gen/return :bogus)]])
                  :forward-api-name g-api :reverse-api-name g-api}
   :transforms   {:transform-name g-name :function-name (g-opt g-name)
                  :project g-project :repository (g-opt (g-ref :repositories))
                  :inputs (g-opt (gen/fmap (fn [d] {:in {:kind :dataset :dataset d}})
                                           (g-ref :datasets)))
                  :language (gen/elements [nil nil :python :sql])
                  :strategy (gen/elements [nil nil nil :polymorphic :resolve-by-shared-property])
                  :input g-input
                  :sources (g-vec (gen/fmap (fn [t] {:from-type t}) (g-ref :object-types)) 2)
                  :output (g-opt g-output)
                  :outputs (g-vec g-output 2)
                  :primary-key (g-opt (gen/return [:x]))}
   :actions      {:action-name g-name :target g-target :project g-project
                  :action-log (gen/frequency
                               [[3 (gen/return nil)] [1 (gen/return true)]
                                [1 (gen/fmap (fn [t] {:object-type t}) (g-ref :object-types))]])
                  :params (gen/map (gen/elements [:p :r]) g-param {:max-elements 2})
                  :rules (g-vec g-rule 2)
                  :edits (g-vec (gen/fmap (fn [t] {:object-type t}) (g-ref :object-types)) 1)}
   :functions    {:function-name g-name :releases g-releases :project g-project}
   :object-sets  (merge {:object-set-name g-name} {::definition g-object-set})
   :datasets     {:dataset-name g-name :project g-project
                  :kind (gen/elements [nil :object-backing])}
   :media-sets   {:media-set-name g-name}
   :applications {:application-name g-name :project g-project
                  :promoted (g-opt (gen/return true))
                  ::definition (gen/elements [{} {:published "1" :releases {"1" {}}}
                                              {:published 1 :releases {"1" {}}}])
                  :form-factor (gen/elements [nil nil :mobile])
                  :object-type (g-opt (g-ref :object-types))
                  :layout g-layout :sidebar g-sidebar}
   :workspaces   {:workspace-name g-name
                  :apps (g-vec (gen/one-of [g-name (gen/fmap #(rid-of :applications %) g-name)]) 2)
                  :landing (g-opt g-name)}
   :repositories {:repository-name g-name :project g-project}
   :projects     {:project-name g-name}})

(defn- g-entry [sec k]
  (gen/fmap (fn [kvs]
              (let [e (into {:id k} (remove (comp nil? val)) kvs)]
                (if-let [d (::definition e)] (merge (dissoc e ::definition) d) e)))
            (apply gen/hash-map (mapcat identity (fields sec)))))

(def ^:private base
  (let [a (rid-of :object-types :a) b (rid-of :object-types :b) l (rid-of :link-types :a)
        t (rid-of :transforms :a) u (rid-of :transforms :b) ac (rid-of :actions :a)
        x [{:attribute-name :x :attribute-type :string}]
        y {:attribute-name :y :attribute-type :string :opts {:derived {:expression [:+ 1 1]}}}]
    {:object-types {a {:id a :object-type-name :a :attribute-list x}
                    b {:id b :object-type-name :b :attribute-list (conj x y)}}
     :actions      {ac {:id ac :action-name :a
                        :params {:p {:type :integer :default {:from :r :property :x}}
                                 :r {:type a :name "R"}}
                        :rules [{:op :update :object-type b :values {:y :p}}]}}
     :link-types   {l {:id l :link-type-name :a :from-object-type a :to-object-type b
                       :cardinality :many-to-one}}
     :transforms   {t {:id t :transform-name :a :input {:kind :object-type :object-type a}
                       :output {:kind :link-type :link-type l :fk-attribute :x}}
                    u {:id u :transform-name :b :input {:kind :datasource :datasource :src}
                       :outputs [{:kind :object-type :object-type a :primary-key [:x]
                                  :column-map {:x "x"} :name :ta}]}}}))

(def world
  (gen/let [b    (gen/elements [{} base])
            secs (gen/vector (gen/elements (keys fields)) 0 16)
            ns   (gen/vector g-name (count secs))
            es   (apply gen/tuple (gen/return nil)
                        (map (fn [sec n] (g-entry sec (rid-of sec n))) secs ns))]
    (reduce (fn [W [sec n e]] (assoc-in W [sec (rid-of sec n)] e))
            b (map vector secs ns (rest es)))))

(def change
  (gen/let [sec  (gen/elements (keys fields))
            n    g-name
            drop (gen/frequency [[1 (gen/return true)] [6 (gen/return false)]])
            [f v] (gen/bind (gen/elements (keys (fields sec)))
                            (fn [f] (gen/tuple (gen/return f) ((fields sec) f))))]
    [[sec (rid-of sec n)] (when-not drop {f v})]))

(defn apply-change
  [W [[sec k :as node] fv]]
  (if (nil? fv)
    (if (contains? W sec) (update W sec dissoc k) W)
    (let [[f v] (first fv)
          e     (merge {:id k} (get-in W node))
          e     (if (= ::definition f)
                  (merge (select-keys e [:id :object-set-name]) v)
                  (if (nil? v) (dissoc e f) (assoc e f v)))]
      (assoc-in W node e))))

(defn- outputs [t] (filter map? (or (seq (:outputs t)) (some-> (:output t) vector))))

(defn- producers
  [W pred]
  (reduce (fn [m [k t]]
            (reduce (fn [m o]
                      (let [target (get o (:kind o))]
                        (if (and (some? target) (pred o)) (update m [(:kind o) target] conj k) m)))
                    m (outputs t)))
          {} (:transforms W)))

(defn- output-names [t]
  (if (seq (:outputs t))
    (keep #(when (map? %) (:name %)) (:outputs t))
    (when (some? (:output t)) [(:transform-name t)])))

(defn- log-names [a]
  (let [lt (when (map? (:action-log a)) (get-in a [:action-log :object-type]))]
    (cond-> [(str (name (or (:action-name a) (:id a)))  "-log")]
      (some? lt) (conj (if (keyword? lt) (name lt) (str lt))))))

(defn- unique? [xs] (every? #(= 1 %) (vals (frequencies (remove nil? xs)))))

(defn oracle-defined?
  [W]
  (let [ts        (vals (:transforms W))
        by-target (producers W (constantly true))
        primary   (producers W #(and (= :object-type (:kind %)) (not= :supplement (:role %))))
        picked    (into (set (for [t ts o (outputs t) :when (= :supplement (:role o))]
                               (:object-type o)))
                        (for [t ts :when (map? (:input t))] (:object-type (:input t))))
        fns       (concat (keep :function-name (vals (:functions W)))
                          (keep :function-name ts))]
    (and (every? #(<= (count %) 2) (vals by-target))
         (every? (fn [[[kind] ps]] (or (not= :link-type kind) (<= (count ps) 1))) by-target)
         (every? (fn [[[_ t] ps]] (or (<= (count ps) 1) (not (contains? picked t)))) primary)
         (every? #(<= % 2) (vals (frequencies (remove nil? (mapcat output-names ts)))))
         (unique? (map #(or (:transform-name (val %)) (key %)) (:transforms W)))
         (unique? (map #(some-> % name) fns))
         (every? (fn [[sec es]]
                   (unique? (for [[_ e] es :let [n (get e (ri/name-field sec))] :when (some? n)]
                              (name n))))
                 W)
         (unique? (mapcat log-names (vals (:actions W))))
         (every? (fn [[sec es]]
                   (>= 1 (count (for [[_ e] es :when (nil? (get e (ri/name-field sec)))] e))))
                 W))))
