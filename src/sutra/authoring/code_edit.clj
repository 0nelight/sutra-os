(ns sutra.authoring.code-edit
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.shape :as shape]
            [sutra.kit.time :as time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.catalog.outputs :as outputs]
            [sutra.types.world-rules :as rules]
            [sutra.catalog.kernel :as kernel]
            [sutra.actions.api :as actions]
            [sutra.catalog.program :as prog]
            [sutra.compute.api :as compute]
            [sutra.types.link-side :as ls]
            [sutra.types.object-set :as os]))

(set! *warn-on-reflection* true)

(def ^:private max-rounds 64)
(def ^:private prefetch-cap 200)

(defn- tid [k] (if (keyword? k) (name k) (str k)))
(defn- snake [x] (str/replace (name x) "-" "_"))
(defn- link-token [{:keys [link dir]}] (str (tid link) "|" (name dir)))

(defn- type-index
  [world]
  (let [ots (:object-types world)]
    (fn [t] (when (string? t) (some #(when (contains? ots %) %) [t (keyword t)])))))

(defn- pk-attr [world k]
  (let [{[p & more] :properties} (rules/object-key world k)]
    (when (and p (nil? more)) p)))

(defn- many? [{:keys [link-type dir]}]
  (let [[mo mi] (outputs/card-limits (:cardinality link-type) [nil nil])]
    (nil? (if (= :to dir) mo mi))))

(defn- type-entry [world k spec]
  {"api"   (prog/pascal-name (or (:object-type-name spec) k))
   "pk"    (some-> (pk-attr world k) name)
   "props" (into {} (for [{a :attribute-name} (:attribute-list spec)]
                      [(snake a) (name a)]))
   "links" (into {} (for [side (ls/outgoing world k)
                          :when (contains? (:object-types world) (:far-type side))]
                      [(snake (:api-name side))
                       {"link" (link-token side) "far" (tid (:far-type side))
                        "many" (many? side)}]))})

(defn- manifest [world]
  {"types" (into {} (for [[k spec] (:object-types world)] [(tid k) (type-entry world k spec)]))})

(defn- side-of [world near token]
  (let [[l d] (str/split token #"\|" 2)
        lts   (:link-types world)
        lk    (some #(when (contains? lts %) %) [l (keyword l)])]
    (when-not lk
      (refusal/refuse! :refusal.edit-function/unknown-link {:link l}))
    (ls/resolve-hop world near lk (keyword d))))

(def ^:private pred-ops
  {"=" := "not=" :not= "<" :< ">" :> "<=" :<= ">=" :>= "nil?" :nil?
   "starts-with" :starts-with "contains" :contains})

(defn- pred [w]
  (when w
    (let [[op & args] w]
      (case op
        "and" (into [:and] (map pred) args)
        "or"  (if (seq args) (into [:or] (map pred) args) [:id-in []])
        (if-let [k (pred-ops op)]
          (let [[attr v] args] [k (keyword attr) (if (= :nil? k) v (compute/<-wire v))])
          (refusal/refuse! :refusal.edit-function/unsupported-filter {:op op}))))))

(defn- reader
  [ctx io world]
  (fn [ir]
    (let [nm  :_edit_function_read
          res ((or (:read-fn ctx)
                   (anomaly/raise! :fault "edit function: no read path in this context" {}))
               (assoc ctx :schema (assoc-in world [:object-sets nm] ir)) io nm {})]
      (if (map? res) (:data res) res))))

(defn- row->wire [{:keys [id values]}]
  {"id" (some-> id str)
   "values" (compute/->wire (update-keys (or values {}) #(if (keyword? %) (name %) (str %))))})

(defn- search-ir [world types {:strs [type where order limit origin]}]
  (let [k    (types type)
        [ob] order]
    (os/qt->ir (cond-> {:root k}
                 where (assoc :where (pred where))
                 ob    (assoc :order-by (keyword (first ob))
                              :sort-dir (keyword (second ob)))
                 limit (assoc :limit limit))
               (fn [ir]
                 (if origin
                   (let [{:strs [type id link]} origin
                         side (side-of world (types type) link)]
                     (os/intersect-hops ir [{:op :search-around :link (:link side)
                                             :dir (:dir side)
                                             :in {:op :static :ids [id]}}]))
                   ir)))))

(defn- answer* [read world types {:strs [op type pk] :as req}]
  (let [k (or (types type)
              (refusal/refuse! :refusal.edit-function/unknown-type {:type type}))]
    (case op
      "get"    (let [p  (pk-attr world k)
                     ir (os/qt->ir {:root k :where (if p [:= p (compute/<-wire pk)] [:id-in [pk]])
                                    :limit 1})]
                 (first (read ir)))
      "search" (read (search-ir world types req)))))

(defn- answer [read world types {:strs [op] :as req}]
  (let [res (answer* read world types req)]
    (if (= "get" op) (some-> res row->wire) (mapv row->wire res))))

(defn- typed-ids [args cache type]
  (let [arg-ids (into #{} (keep (fn [o] (when (= type (get o "type")) (get o "id"))))
                      (keep #(get % "$object") (tree-seq coll? seq args)))
        cached  (into #{} (for [[req resp] cache
                                :when (= type (get req "type"))
                                o (if (map? resp) [resp] resp)
                                :when (map? o)]
                            (get o "id")))]
    (remove nil? (into arg-ids cached))))

(defn- fulfil [read world types args cache req]
  (let [hit  [[req (answer read world types req)]]
        orig (get req "origin")]
    (if-not orig
      hit
      (let [seen (into #{} (map first) cache)]
        (into hit
              (for [id   (take prefetch-cap (typed-ids args cache (get orig "type")))
                    :let [r (assoc-in req ["origin" "id"] id)]
                    :when (and (not= id (get orig "id")) (not (seen r)))]
                [r (answer read world types r)]))))))

(defn- encode-arg [world v]
  (cond
    (and (map? v) (contains? v :values) (contains? v :object-type-ref))
    {"$object" (assoc (row->wire v) "type" (tid (:object-type-ref v)))}
    (map? v)        (into {} (for [[k x] v] [(snake k) (encode-arg world x)]))
    (sequential? v) (mapv #(encode-arg world %) v)
    :else           (compute/->wire v)))

(def ^:private runner "import sutra_functions\nsutra_functions._main()\n")

(defn perform-python-edit
  [{:keys [payload]} _]
  (compute/run-python! {:label :python-edit :code runner :stdin (json/write-str payload)}))

(defn- python-run
  [ctx io]
  (fn [payload]
    (json/read-str
     (str/trim (str (:result (actions/ask ctx io {:kind :python-edit :payload payload})))))))

(defn- fail! [entry {:strs [user_facing message]}]
  (if user_facing
    (refusal/refuse! :refusal.authoring/edit-function-user-error
                     {:function entry :user-facing? true} {:why user_facing})
    (refusal/refuse! :refusal.authoring/edit-function-failed {:function entry}
                     {:why message})))

(defn- run-function [run read world {:keys [source entry]} args]
  (let [types (type-index world)
        base  {"source" source "entry" entry "manifest" (manifest world) "args" args}]
    (loop [cache [] round 0]
      (when (>= round max-rounds)
        (refusal/refuse! :refusal.edit-function/too-many-rounds {:function entry}
                         {:rounds max-rounds}))
      (let [{:strs [needs error] :as res} (run (assoc base "cache" cache))]
        (cond
          error       (fail! entry error)
          (seq needs) (recur (reduce (fn [c req] (into c (fulfil read world types args c req)))
                                     cache needs)
                             (inc round))
          :else       res)))))

(defn- coerce [world k attr v]
  (let [typ (some #(when (= attr (:attribute-name %)) (:attribute-type %))
                  (get-in world [:object-types k :attribute-list]))
        v   (compute/<-wire v)]
    (if-not (string? v)
      v
      (case typ
        :date      (java.time.LocalDate/parse (subs v 0 (min 10 (count v))))
        :timestamp (or (time/->instant v) (java.time.Instant/parse v))
        v))))

(defn- values [world k vs]
  (into {} (for [[a v] vs :let [attr (keyword a)]] [attr (coerce world k attr v)])))

(defn- edit-plan [read world types edits]
  (let [k-of    #(or (types %)
                     (refusal/refuse! :refusal.edit-function/unknown-type {:type %}))
        id-of   (fn [created {:strs [type pk id]}]
                  (if-let [b (created [type pk])]
                    [:get b :id]
                    (or id
                        (get (answer read world types {"op" "get" "type" type "pk" pk}) "id")
                        (refusal/refuse! :refusal.edit-function/no-object {:type type :pk pk}))))
        target  (fn [created {:strs [type pk id] :as ref}]
                  (or (created [type pk]) id (compute/<-wire pk) (id-of created ref)))
        modify  (fn [created ref vs]
                  (let [k (k-of (get ref "type"))]
                    {:op :update :object-type k :target (target created ref)
                     :values (values world k vs)}))
        link    (fn [created {:strs [op link source] tgt "target"}]
                  (let [near (k-of (get source "type"))
                        side (side-of world near link)
                        fk   (rules/fk-link-spec world (:link side))
                        near-fk? (and fk (= (:fk-side fk) (if (= :to (:dir side)) :from :to)))
                        pk-of #(compute/<-wire (get % "pk"))]
                    (cond
                      near-fk?
                      [(modify created source {(name (:fk-attribute fk))
                                               (when (#{"add" "set"} op) (pk-of tgt))})]

                      fk
                      (case op
                        "add"    [(modify created tgt {(name (:fk-attribute fk)) (pk-of source)})]
                        "remove" [(modify created tgt {(name (:fk-attribute fk)) nil})]
                        (refusal/refuse! :refusal.edit-function/set-on-multi-link
                                         {:link link} {:op op}))

                      :else
                      (let [ends   (fn [a b] (if (= :to (:dir side)) [a b] [b a]))
                            unlink (fn [a b]
                                     (let [[f t] (ends a b)]
                                       {:op :delete-link :link-type (:link side)
                                        :target
                                        (shape/link-id-for
                                         world (:link side) {:from-id f :to-id t})}))
                            sid    (id-of created source)
                            existing #(for [o
                                            (answer read world types
                                                    {"op" "search" "type" (tid (:far-type side))
                                                     "where" nil "order" nil "limit" nil
                                                     "origin"
                                                     {"type" (get source "type") "id" sid
                                                      "link" link}})]
                                        (unlink sid (get o "id")))
                            add    (fn []
                                     (let [[f t] (ends sid (id-of created tgt))]
                                       {:op :create-link :link-type (:link side) :from-id f
                                        :to-id t}))]
                        (case op
                          "add"    [(add)]
                          "remove" [(unlink sid (id-of created tgt))]
                          "set"    (conj (vec (existing)) (add))
                          "clear"  (vec (existing)))))))
        plan    (fn [{:keys [created] :as acc} {:strs [kind type pk] vs "values" :as e}]
                  (case kind
                    "create" (let [k (k-of type)
                                   b (keyword (str "_py_edit_" (inc (:n acc))))]
                               (-> acc
                                   (update :n inc)
                                   (assoc-in [:created [type pk]] b)
                                   (update :steps conj
                                           {:op :create :object-type k :values (values world k vs)
                                            :as b})))
                    "modify" (update acc :steps conj (modify created e vs))
                    "delete" (update acc :steps conj
                                     {:op :delete-object :object-type (k-of type)
                                      :target (target created e)})
                    "link"   (update acc :steps into (link created e))))]
    (:steps (reduce plan {:created {} :n 0 :steps []} edits))))

(defn- edited-types [edits]
  (into #{} (map (fn [{:strs [kind type source]}]
                   (if (= "link" kind) (get source "type") type)))
        edits))

(defn- type-name [world t]
  (or (some-> (get-in world [:object-types t :object-type-name]) name)
      (some-> (get-in world [:link-types t :link-type-name]) name)
      t))

(defn- check-declared! [world entry {:strs [edits declared]} how]
  (let [stray (remove (set declared) (edited-types edits))]
    (when (seq stray)
      (refusal/refuse! :refusal.authoring/edit-undeclared-types
                       {:function entry :undeclared (vec (sort stray))}
                       {:types (str/join ", " (sort (map #(type-name world %) stray))) :how how}))))

(defn python-edit-verb
  [{:keys [entry args] :as step} world bindings ctx io]
  (let [encoded (into {} (for [[py b] args :when (contains? bindings b)]
                           [py (encode-arg world (get bindings b))]))
        read    (reader ctx io world)
        res     (run-function (python-run ctx io) read world step encoded)
        _       (check-declared! world entry res "@function(edits=[...])")
        steps   (edit-plan read world (type-index world) (get res "edits"))
        result  (kernel/eval-steps steps world {} ctx io)]
    {:world world :bindings bindings :ops (:ops result) :side-effects (:side-effects result)}))

(defn- type-kw [world k]
  (keyword (name (or (get-in world [:object-types k :object-type-name]) k))))

(defn- name-index [world]
  (into {} (for [k (keys (:object-types world))] [(type-kw world k) k])))

(defn- type-key [world names t]
  (cond
    (contains? (:object-types world) t) t
    (some? t) (or (names (keyword (name t)))
                  (refusal/refuse! :refusal.edit-function/unknown-type {:type t}))))

(defn- ->obj [world k {:keys [id values]}]
  (with-meta (or values {}) {:sutra/type (type-kw world k) :sutra/id (some-> id str)}))

(defn- arg->obj [world names v]
  (cond
    (and (map? v) (contains? v :values) (contains? v :object-type-ref))
    (->obj world (type-key world names (:object-type-ref v)) v)
    (map? v)        (update-vals v #(arg->obj world names %))
    (sequential? v) (mapv #(arg->obj world names %) v)
    :else           v))

(defn- clj-pred [w]
  (when w
    (let [[op & args] w]
      (case op
        (:and :or) (if (seq args) (into [op] (map clj-pred) args)
                       (if (= :or op) [:id-in []] [:and]))
        :in        (let [[attr vs] args]
                     (if (seq vs) (into [:or] (map #(vector := attr %)) vs) [:id-in []]))
        (if (contains? (set (vals pred-ops)) op)
          (vec w)
          (refusal/refuse! :refusal.edit-function/unsupported-filter {:op op}))))))

(defn- clj-ir [k {:keys [where order-by limit]} hop]
  (let [[ob dir] (if (sequential? order-by) order-by [order-by])]
    (os/qt->ir (cond-> {:root k}
                 where    (assoc :where (clj-pred where))
                 ob       (assoc :order-by ob :sort-dir (or dir :asc))
                 limit    (assoc :limit limit))
               #(if hop (os/intersect-hops % [hop]) %))))

(defn- clj-host [read world names]
  (fn [{:keys [op type pk source side] :as req}]
    (case op
      :get    (let [k (type-key world names type)]
                (some->> (answer* read world (type-index world)
                                  {"op" "get" "type" (tid k) "pk" (compute/->wire pk)})
                         (->obj world k)))
      :search (let [k (type-key world names type)]
                (mapv #(->obj world k %) (read (clj-ir k req nil))))
      :links  (let [near (type-key world names (:type source))
                    sd   (ls/resolve-side world near side)
                    far  (:far-type sd)]
                (mapv #(->obj world far %)
                      (read (clj-ir far req {:op :search-around :link (:link sd) :dir (:dir sd)
                                             :in {:op :static :ids [(:id source)]}})))))))

(defn- ref->wire [world names {:keys [type pk id]}]
  {"type" (tid (type-key world names type)) "pk" (compute/->wire pk) "id" id})

(defn- vals->wire [vs]
  (compute/->wire (update-keys (or vs {}) name)))

(defn- edit->wire [world names {:keys [kind op side source target values] :as e}]
  (case kind
    :create (assoc (ref->wire world names e) "kind" "create" "values" (vals->wire values))
    :modify (assoc (ref->wire world names e) "kind" "modify" "values" (vals->wire values))
    :delete (assoc (ref->wire world names e) "kind" "delete")
    :link   {"kind" "link" "op" (name op)
             "link" (link-token (ls/resolve-side world (type-key world names (:type source)) side))
             "source" (ref->wire world names source)
             "target" (some->> target (ref->wire world names))}))

(defn clojure-edit-verb
  [{:keys [source entry args declared]} world bindings ctx io]
  (let [names    (name-index world)
        read     (reader ctx io world)
        pk-attrs (into {} (for [[n k] names] [n (pk-attr world k)]))
        res      (compute/run-edit-function! {:code source :entry entry :pk-attrs pk-attrs
                                              :args
                                              (mapv #(arg->obj world names (get bindings %)) args)}
                                             (clj-host read world names))
        edits    (mapv #(edit->wire world names %) (:edits res))
        decl     (mapv #(tid (type-key world names %)) declared)
        _        (check-declared! world entry {"edits" edits "declared" decl}
                                  "^{:sutra/function {:edits [...]}}")
        result   (kernel/eval-steps (edit-plan read world (type-index world) edits)
                                    world {} ctx io)]
    {:world world :bindings bindings :ops (:ops result) :side-effects (:side-effects result)}))

(def module
  {:name        :code-edit
   :contributes {:action/verbs   {:python-edit  python-edit-verb
                                  :clojure-edit clojure-edit-verb}
                 :run/performers {:python-edit perform-python-edit}}})
