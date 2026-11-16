(ns ^:unit platform.guard-held-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.authoring.code-edit]
            [sutra.catalog.interfaces :as interfaces]
            [sutra.catalog.world-columns :as kc]
            [sutra.catalog.link-ends :as link-ends]
            [sutra.catalog.problems :as problems]
            [sutra.catalog.publishing :as publishing]
            [sutra.catalog.retirement]
            [sutra.catalog.signature :as signature]
            [sutra.parser.project-export :as pe]
            [sutra.world.db :as kdb]
            [sutra.world.gen :as kg]
            [sutra.world.gen.near :as gn]
            [sutra.world.guard :as guard]
            [sutra.store.core.resources :as res]
            [sutra.store.kept :as kept]
            [sutra.store.world]
            [sutra.types.schema :as sch]
            [sutra.types.world-rules :as rules])
  (:import [sutra.world.guard Guard]))

(defn- guarded [W] (update-vals W #(if (map? %) (Guard. %) %)))

(defn- reads-by-key
  [f W & args]
  (try (apply f (guarded W) args)
       (catch Exception e (if (guard/walked? e) ::walked (throw e)))))

(def ^:private g-history
  (gen/tuple gn/world (gen/vector (gen/vector gn/change 1 3) 1 4)))

(defn- stepped-as-init?
  [init step [W cs]]
  (loop [W W v (init W) [c & more] cs]
    (if-not c
      true
      (let [W1 (reduce gn/apply-change W c)
            v1 (try (step v (guarded W) (guarded W1) (map first c))
                    (catch Exception e (if (guard/walked? e) ::walked (throw e))))]
        (and (not= ::walked v1) (= v1 (init W1)) (recur W1 v1 more))))))

(defspec the-signature-steps-by-key 300
  (prop/for-all [h g-history] (stepped-as-init? rules/sig-init rules/sig-step h)))

(defspec the-declarations-step-by-key 300
  (prop/for-all [h g-history] (stepped-as-init? signature/decls-init signature/decls-step h)))

(defspec resources-are-read-by-key 300
  (prop/for-all [W gn/world extra (gen/vector (gen/tuple (gen/elements [:object-types :bogus])
                                                         gen/string-alphanumeric)
                                              0 3)]
    (let [ids (concat [res/frame-id] extra (for [[s es] W :when (map? es) k (keys es)] [s k]))]
      (and (every? #(= (res/value W %) (reads-by-key res/value W %)) ids)
           (= (res/only W ids) (reads-by-key res/only W ids))))))

(defspec a-commit-reads-the-entries-its-changes-touch 300
  (prop/for-all [W gn/world cs (gen/vector gn/change 1 4)]
    (let [W1      (reduce gn/apply-change W cs)
          changes (kg/diff W W1)
          touched @#'sutra.store.world/touched]
      (= (touched W1 changes) (reads-by-key touched W1 changes)))))

(defspec the-wipe-guard-reads-section-sizes 300
  (prop/for-all [W gn/world]
    (let [populated? @#'sutra.store.world/populated?]
      (= (populated? W) (reads-by-key populated? W)))))

(defspec the-frame-and-object-sets-are-read-by-key 300
  (prop/for-all [W gn/world]
    (and (= (problems/frame-problems W) (reads-by-key problems/frame-problems W))
         (every? #(= (problems/object-set-ok? W %) (reads-by-key problems/object-set-ok? W %))
                 (vals (:object-sets W))))))

(defspec a-world-names-its-step-and-db-in-its-metadata 100
  (prop/for-all [W gn/world x gen/any-equatable]
    (let [W (with-meta W {:sutra.store.kept/step x :sutra.store.kept/held x})]
      (and (= x (reads-by-key kept/step-of W)) (= x (reads-by-key kept/held W))
           (let [m (reads-by-key kept/made-by W "b" x x)]
             (and (not= ::walked m) (= x (kept/held m))))))))

(defn- columns-of [W]
  (into {} (for [[id f] kc/column-fns] [id (try (f W) (catch Exception _ ::threw))])))

(defspec the-columns-step-by-key 300
  (prop/for-all [[W css] g-history]
    (let [d (try (kdb/db W kc/rows) (catch Exception _ nil))]
      (loop [W W d d [cs & more] css]
        (if (or (nil? d) (nil? cs))
          true
          (let [W1 (reduce gn/apply-change W cs)
                d1 (try (kdb/db-with (guard/guarded d) (kg/diff W W1))
                        (catch Exception e (if (guard/walked? e) ::walked ::threw)))]
            (case d1
              ::walked false
              ::threw  (boolean (some #{::threw} (vals (columns-of W1))))
              (and (= (kc/carried d1) (columns-of W1))
                   (recur W1 (update d1 :ents guard/unguarded) more)))))))))

(defspec retired-sections-are-read-by-their-size 300
  (prop/for-all [W gn/world extra (gen/map (gen/elements [:retired :objects :links])
                                           (gen/elements [{} {:x 1}]) {:max-elements 2})]
    (let [W (merge W extra)
          r (reads-by-key sutra.store.world/without-retired W)]
      (= (sutra.store.world/without-retired W) (if (map? r) (guard/unguarded r) r)))))

(defn- with-columns
  [W cols]
  (sch/with-views W cols))

(defspec the-link-types-reaching-a-type-are-read-by-key 300
  (prop/for-all [W gn/world]
    (let [cols (try {::link-ends/ends (link-ends/by-end (:link-types W))
                     :sutra.catalog.interfaces/ifaces (interfaces/iface-index W)}
                    (catch Exception _ nil))
          G    (when cols (with-columns (guarded W) cols))
          P    (when cols (with-columns W cols))]
      (or (nil? cols)
          (every? #(= (link-ends/reaching P %)
                      (try (link-ends/reaching G %)
                           (catch Exception e (if (guard/walked? e) ::walked (throw e)))))
                  (concat (keys (:object-types W)) (keys (:interfaces W)) [:none]))))))

(defspec what-a-removal-leaves-dangling-is-read-at-the-referrers 500
  (prop/for-all [W gn/world i gen/nat n (gen/choose 1 3)]
    (let [nodes (vec (sort-by str (for [[sec es] W :when (map? es) k (keys es)] [sec k])))
          gone  (into #{} (map second)
                      (when (seq nodes) (take n (drop (mod i (count nodes)) (cycle nodes)))))
          after (reduce-kv (fn [w sec es]
                             (assoc w sec (if (map? es) (apply dissoc es gone) es)))
                           {} W)
          d     (kdb/db W)
          ref   #(kdb/nodes d :refs-in %)
          f     @#'sutra.catalog.retirement/new-danglers]
      (= (f W after gone ref)
         (try (f (guarded W) (guarded after) gone ref)
              (catch Exception e (if (guard/walked? e) ::walked (throw e))))))))

(defn- outcome
  [guard? f W & args]
  (try (apply f (if guard? (guarded W) W) args)
       (catch Exception e
         (cond (guard/walked? e) ::walked
               (instance? clojure.lang.ExceptionInfo e) [::threw (ex-message e)]
               :else (throw e)))))

(def ^:private code-edit-fns
  (into {} (for [f '[type-index encode-arg coerce values type-kw type-name check-declared!
                     type-key ->obj arg->obj ref->wire name-index side-of search-ir]]
             [f @(ns-resolve 'sutra.authoring.code-edit f)])))

(defn- tid [k] (if (keyword? k) (name k) (str k)))

(def ^:private g-code-edit-world
  (gen/let [W gn/world kw? gen/boolean]
    (cond-> W kw? (update :object-types #(update-keys % keyword)))))

(defspec the-edit-function-path-reads-its-types-by-key 1000
  (prop/for-all [W     g-code-edit-world
                 wires (gen/vector (gen/one-of [gen/string-alphanumeric (gen/return nil)]) 0 3)]
    (let [{:syms [type-index encode-arg coerce values type-kw type-name check-declared!
                  type-key ->obj arg->obj ref->wire name-index side-of search-ir]} code-edit-fns
          tokens (concat (for [l (keys (:link-types W)) d ["to" "from"]] (str (tid l) "|" d))
                         ["none|to"])
          ks     (keys (:object-types W))
          names  (name-index W)
          same   (fn [f & args] (= (apply outcome false f W args) (apply outcome true f W args)))
          lookup (fn [W] (let [t (type-index W)] (fn [s] (t s))))
          looked (try (lookup (guarded W)) (catch Exception _ nil))
          old    (into {} (map (juxt tid identity)) ks)
          probes (concat (map tid ks) wires)
          attrs  (for [k ks a (get-in W [:object-types k :attribute-list])] [k (:attribute-name a)])
          ref    (fn [k] {:type k :pk 1 :id "i"})
          obj    (fn [k] {:object-type-ref k :values {:x 1} :id "i"})]
      (and (some? looked)
           (every? #(= (old %) (looked %)) probes)
           (same encode-arg {:a [(obj (first ks)) 1]})
           (every? (fn [[k a]] (and (same coerce k a "2026-01-02") (same values k {a 1}))) attrs)
           (every? #(and (same type-kw %) (same type-name %) (same type-key names %)
                         (same ->obj % {:id 1 :values {:x 1}}) (same arg->obj names [(obj %)])
                         (same ref->wire names (ref %)))
                   (concat ks [:none "none"]))
           (same check-declared! "f" {"edits" [{"kind" "create" "type" (some-> (first ks) tid)}]
                                       "declared" []}
                 "how")
           (every? (fn [tok]
                     (let [near (first ks)
                           req  {"type" (some-> near tid) "limit" 2
                                 "origin" {"type" (some-> near tid) "id" "i" "link" tok}}]
                       (and (same side-of near tok)
                            (= (outcome false search-ir W (type-index W) req)
                               (outcome true search-ir W (type-index (guarded W)) req)))))
                   tokens)))))

(defspec an-entrys-references-are-named-from-the-entries-they-name 300
  (prop/for-all [W gn/world]
    (every? (fn [[kk es]]
              (every? #(= ((publishing/ref-names W kk) %)
                          (reads-by-key (fn [G] ((publishing/ref-names G kk) %)) W))
                      (vals es)))
            (filter (comp map? val) W))))

(defspec an-export-is-held-and-read-by-key 300
  (prop/for-all [W gn/world x gen/any-equatable all? gen/boolean]
    (let [held-export @#'pe/held-export
          G           (reads-by-key pe/holding W x)
          read        (try (held-export G {:all-projects? all?})
                           (catch Exception e (if (guard/walked? e) ::walked (throw e))))]
      (and (not= ::walked G)
           (= (when all? x) read (held-export (pe/holding W x) {:all-projects? all?}))))))

(def held
  (let [rules "libs/dialect/src/sutra/types/world_rules.clj"
        sig   "src/sutra/catalog/signature.clj"]
    (merge
     (into {} (for [f ["sig-step" "outs-in" "ancestors-in" "index-of" "type-sig"
                       "interface-sig" "link-sig" "eff-implements" "section-map!"]]
                [[rules f] #'the-signature-steps-by-key]))
     (into {} (for [f ["decls-step" "output-decls" "output-columns" "filed" "filed-at" "table-decl"]]
                [[sig f] #'the-declarations-step-by-key]))
     (into {} (for [f ["type-index" "encode-arg" "coerce" "values" "type-kw" "type-name"
                       "check-declared!" "type-key" "->obj" "arg->obj" "ref->wire" "side-of"
                       "search-ir"]]
                [["src/sutra/authoring/code_edit.clj" f]
                 #'the-edit-function-path-reads-its-types-by-key]))
     {["libs/store/src/sutra/store/core/resources.clj" "value"]
      #'resources-are-read-by-key
      ["libs/store/src/sutra/store/core/resources.clj" "only"]
      #'resources-are-read-by-key
      ["libs/store/src/sutra/store/world.clj" "touched"]
      #'a-commit-reads-the-entries-its-changes-touch
      ["libs/store/src/sutra/store/world.clj" "populated?"]
      #'the-wipe-guard-reads-section-sizes
      ["libs/store/src/sutra/store/world.clj" "without-retired"]
      #'retired-sections-are-read-by-their-size
      ["src/sutra/catalog/problems.clj" "frame-problems"]
      #'the-frame-and-object-sets-are-read-by-key
      ["src/sutra/catalog/problems.clj" "object-set-ok?"]
      #'the-frame-and-object-sets-are-read-by-key
      ["src/sutra/catalog/link_ends.clj" "reaching"]
      #'the-link-types-reaching-a-type-are-read-by-key
      ["src/sutra/catalog/publishing.clj" "ref-names"]
      #'an-entrys-references-are-named-from-the-entries-they-name
      ["src/sutra/catalog/retirement.clj" "live-hit?"]
      #'what-a-removal-leaves-dangling-is-read-at-the-referrers
      ["src/sutra/store/kept.clj" "step-of"] #'a-world-names-its-step-and-db-in-its-metadata
      ["src/sutra/store/kept.clj" "held"]    #'a-world-names-its-step-and-db-in-its-metadata
      ["src/sutra/store/kept.clj" "made-by"] #'a-world-names-its-step-and-db-in-its-metadata
      ["src/sutra/catalog/world_columns.clj" "ot-array-cols"]  #'the-columns-step-by-key
      ["src/sutra/catalog/world_columns.clj" "table-cols"]     #'the-columns-step-by-key
      ["libs/core/src/sutra/parser/project_export.clj" "holding"]     #'an-export-is-held-and-read-by-key
      ["libs/core/src/sutra/parser/project_export.clj" "held-export"] #'an-export-is-held-and-read-by-key
      ["src/sutra/authoring/projections.clj" "held-schema"]           #'an-export-is-held-and-read-by-key
      ["libs/kernel/src/sutra/ref/install.clj" "effective-interface"]  #'the-columns-step-by-key
      ["libs/kernel/src/sutra/ref/install.clj" "effective-implements"] #'the-columns-step-by-key})))
