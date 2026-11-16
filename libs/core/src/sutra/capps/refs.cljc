(ns sutra.capps.refs
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.bind :as bind]
            [sutra.capps.variables :as vars]
            [sutra.capps.walk :as walk]
            [sutra.types.set-query :as sq]))

(defn reserved-for
  [ff]
  (if (= :panel-set (->kw ff)) vars/view-objects vars/view-object))

(defn- link-ref? [x] (and (bind/named x) (= "link" (namespace (->kw x)))))

(defn- scope
  [body]
  (set (keys (sq/layout-specs body))))

(defn- lift-type-name
  [x {:keys [names own ff]}]
  (let [k (->kw x)]
    (cond
      (or (contains? names k) (link-ref? x)
          (contains? vars/reserved-names k)) [:from k]
      (and own (= k own))                    [:from (reserved-for ff)]
      own                                    [:linked k]
      :else                                  [:source k])))

(declare ^:private lift-inline)

(defn- lift-spec
  [spec ctx]
  (if-not (map? spec)
    spec
    (let [f      (:from spec)
          of     (:of spec)
          spec   (if (bind/named f)
                   (let [[k x] (lift-type-name f ctx)]
                     (-> (dissoc spec :from) (assoc k x)))
                   spec)
          spec   (if (and (bind/named of) (:own ctx) (= (->kw of) (:own ctx))
                          (not (contains? (:names ctx) (->kw of))))
                   (assoc spec :of (reserved-for (:ff ctx)))
                   spec)]
      (reduce-kv (fn [m k v]
                   (assoc m k (if (#{:from :source :linked :of} k) v (lift-inline v ctx))))
                 (empty spec) spec))))

(defn- lift-inline
  [x ctx]
  (walk/walk-tree
   (fn [x]
     (cond
       (and (= :from (bind/form-id x)) (bind/named (:from x)) (not (link-ref? (:from x))))
       [(let [[k nm] (lift-type-name (:from x) ctx)]
          (if (= :from k) {:val nm} {k nm}))]

       (and (map? x) (map? (:vars x)))
       [(reduce-kv (fn [m k v]
                     (assoc m k (if (= :vars k)
                                  (update-vals v #(lift-spec % ctx))
                                  (lift-inline v ctx))))
                   (empty x) x)]))
   x))

(defn lift-body
  [body view]
  (if-not (map? body)
    body
    (lift-inline body {:names (scope body)
                       :own   (some-> (:object-type view) name keyword)
                       :ff    (:form-factor view)})))

(def view-factors [:full :panel :panel-set :mobile])

(defn lift-view-doc
  [doc]
  (if-not (map? doc)
    doc
    (reduce (fn [d ff]
              (cond-> d
                (map? (get d ff))
                (update ff lift-body {:object-type (:object-type doc) :form-factor ff})))
            doc view-factors)))

(defn lift-app-doc
  [doc]
  (cond-> doc (map? (:layout doc)) (update :layout lift-body nil)))

(defn lift-application
  ([app] (lift-application app identity))
  ([app type-name]
   (if (map? (:layout app))
     (update app :layout lift-body
             (when (:form-factor app)
               (assoc app :object-type (or (type-name (:object-type app))
                                           (:object-type app)))))
     app)))

(defn lift-schema-applications
  [schema]
  (if-not (map? (:applications schema))
    schema
    (let [names (into {} (for [[k t] (:object-types schema)
                               :let [nm (:object-type-name t)]
                               :when nm
                               r [k (:id t)] :when r]
                           [(str r) nm]))
          tname (fn [t] (get names (str t)))]
      (update schema :applications update-vals #(lift-application % tname)))))

(defn- where-it-is [view] (if view "this object view" "this app"))

(defn- spelled-for
  [x view]
  (str "write {:source " x "} for every object of a type"
       (when view (str ", {:linked " x "} for the objects linked to this one, or "
                       (pr-str (reserved-for (:form-factor view)))
                       " for the object(s) the view is opened on"))))

(defn- search-around-start?
  [specs of]
  (let [up (get specs (->kw of))]
    (and (map? up) (:linked up) (empty? (:where up)) (nil? (:join up))
         (or (nil? (:of up)) (search-around-start? specs (:of up))))))

(defn hand-made-hop
  [specs spec]
  (when-let [t (and (map? spec) (some-> (:source spec) ->kw))]
    (some (fn [[_ op ref]]
            (when (#{:= :in} (->kw op))
              (if (bind/name-in :item ref)
                [t vars/view-object]
                (when-let [p (bind/name-in :val ref)]
                  (let [{:keys [object-property of]} (get specs (->kw p))
                        of (some-> of ->kw)]
                    (when (and object-property of
                               (or (= vars/view-object of) (:linked (get specs of))))
                      [t of]))))))
          (filter vector? (:where spec)))))

(defn- spec-problems
  [nm spec {:keys [names view specs]}]
  (let [ok?  (fn [x] (let [k (->kw x)]
                       (or (contains? names k)
                           (and view (= k (reserved-for (:form-factor view)))))))
        what (where-it-is view)]
    (when (map? spec)
      (cond-> []
        (contains? vars/reserved-names nm)
        (conj (str nm " is a reserved name — " (pr-str vars/view-object) " and "
                   (pr-str vars/view-objects) " are what an object view is opened on"))

        (and (bind/named (:from spec)) (not (link-ref? (:from spec))) (not (ok? (:from spec))))
        (conj (str nm " :from " (:from spec) " — " (name (->kw (:from spec)))
                   " is no variable of " what "; :from names a variable, never a type: "
                   (spelled-for (:from spec) view)))

        (and view (= vars/view-object (some-> (:from spec) ->kw)) (seq (:where spec)))
        (conj (str nm " filters " (pr-str vars/view-object) " — the ONE object this view is"
                   " opened on, so it can only ever be that object or nothing. Other objects"
                   " are {:linked t} (linked to it) or {:source t} (of the whole type), and"
                   " the clause's {:item p} reads this object's property"))

        (and (:linked spec) (not view))
        (conj (str nm " :linked " (:linked spec) " — :linked reads the neighbours of an"
                   " object view's object; an app has none: write {:source "
                   (:linked spec) "}"))

        (and view (:linked spec) (:of spec) (not (search-around-start? specs (:of spec))))
        (conj (str nm " :linked " (:linked spec) " :of " (:of spec) " — a link traversal"
                   " starts from a {:linked t} set (or a chain of them) without :where;"
                   " filter the set it lands on instead"))

        (and (bind/named (:of spec)) (not (ok? (:of spec))))
        (conj (str nm " :of " (:of spec) " — " (name (->kw (:of spec)))
                   " is no variable of " what))))))

(defn- spec-at?
  [path]
  (and (< 1 (count path)) (= :vars (peek (pop path))) (not (integer? (peek path)))))

(def ^:private spec-name-keys
  #{:from :of :source :linked})

(defn- inline-problems
  [x {:keys [view] :as ctx}]
  (walk/visit
   (fn [x {:keys [path decl?]}]
     (cond
       (and decl? (spec-at? path))
       (spec-problems (peek path) x ctx)

       (and decl? (spec-name-keys (peek path)) (spec-at? (pop path)))
       nil

       (and (= :from (bind/form-id x)) (bind/named (:from x)) (not (link-ref? (:from x))))
       [(str "{:from " (:from x) "} — a reference says what it reads: "
             "{:val " (:from x) "} for a variable, "
             (spelled-for (:from x) view))]

       (and (= :linked (bind/form-id x)) (not view))
       [(str (pr-str x) " — an app has no object to be linked to:"
             " {:source " (:linked x) "}")]))
   x
   {:references? true}))

(defn problems
  [body view]
  (when (map? body)
    (seq (distinct (inline-problems body {:names (scope body) :view view
                                          :specs (update-keys (sq/layout-specs body) ->kw)})))))
