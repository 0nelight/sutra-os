(ns ^:unit grammar.transform-property-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [malli.generator :as mg]
            [sutra.builds.plan :as plan]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-emit :as pemit]
            [sutra.parser.sugar :as sugar]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.kernel :as kernel]
            [sutra.types.entities :as ents]
            [sutra.types.shape :as tshape]
            [sutra.authoring.install.plan :as install-plan]))

(def ^:private grammar
  (into {} (for [[k _ s] (m/children (m/schema @#'ent/TransformEntry))] [k s])))

(def ^:private not-generated
  #{:language :code :code-file :query :inputs :input :output :outputs :output-columns
    :sources :source :doc :match-column :shared-property :board :version :pipeline})

(def ^:private skeleton
  (gen/let [lang   (gen/elements [:python :clojure :pg-sql])
            two?   gen/boolean
            multi? gen/boolean]
    (cond-> {:language lang
             :inputs   (cond-> {:q {:kind :dataset :dataset :ein}}
                         two? (assoc :r {:kind :dataset :dataset :zwei}))}
      (= :pg-sql lang)  (assoc :query "select * from q")
      (= :python lang)  (assoc :code "def f(q):\n    return q\n")
      (= :clojure lang) (assoc :code "(fn [m] (:q m))")
      multi?            (assoc :outputs [{:name :aus :kind :dataset :dataset :aus}])
      (not multi?)      (assoc :output {:kind :dataset :dataset :aus}))))

(defn- nan-free?
  [v]
  (not-any? #(and (double? %) (Double/isNaN %)) (tree-seq coll? seq v)))

(defn- value-of
  [k]
  (gen/such-that nan-free? (mg/generator (grammar k)) 100))

(def ^:private extras
  (gen/bind (gen/vector-distinct (gen/elements (sort (remove not-generated (keys grammar))))
                                 {:max-elements 6})
            (fn [ks] (apply gen/hash-map (mapcat (fn [k] [k (value-of k)]) ks)))))

(def ^:private authored
  (gen/fmap (fn [[s e]] (merge e s)) (gen/tuple skeleton extras)))

(def ^:private empty-world
  {:object-types {} :link-types {} :interfaces {} :transforms {} :actions {}
   :object-sets {} :datasets {} :projects {:fixture {}}})

(def ^:private plan-opts
  {:introspect (fn [_ body] [body nil])})

(defn- install
  [world schema]
  (reduce (fn [w {:keys [action args entity]}]
            (:world (kernel/eval-steps (:rules (get actions/schema-actions action)) w
                                       {:arg0 (assoc args :name entity)}
                                       {:installer {} :project :fixture})))
          world
          (install-plan/plan-schema schema world plan-opts)))

(def ^:private minted
  (into #{} (keep (fn [[k row]] (when (:author-form row) k))) (:transforms ents/fields)))

(defn- lowered
  [schema]
  (let [low (sugar/lower-schema schema)]
    (reduce-kv (fn [s k t] (update-in s [:transforms k] merge (select-keys t minted)))
               low (:transforms schema))))

(defn- installed
  [t]
  (try (let [w (install empty-world (lowered {:transforms {:t t}}))]
         {:world w :stored (val (first (:transforms w)))})
       (catch clojure.lang.ExceptionInfo e
         (let [chain (take-while some? (iterate ex-cause e))]
           {:refused (str/join " · " (concat (keep ex-message chain)
                                             (keep :message (mapcat (comp :problems ex-data)
                                                                    chain))))}))))

(def ^:private rows (:transforms ents/fields))

(defn- meaningful?
  [k v]
  (and (tshape/present? (get-in rows [k :present]) v)
       (not (and (coll? v) (empty? v)))))

(defn- lost
  [authored arrived]
  (sort (filter #(and (contains? rows %) (meaningful? % (get authored %))
                      (not (contains? arrived %)))
                (keys authored))))

(defspec an-accepted-transform-is-stored-whole 100
  (prop/for-all [t authored]
                (let [{:keys [stored refused]} (installed t)]
                  (or refused (empty? (lost t stored))))))

(defspec a-stored-transform-reaches-the-runner-whole 100
  (prop/for-all [t authored]
                (let [{:keys [stored refused]} (installed t)]
                  (or refused
                      (empty? (lost (select-keys t plan/compute-fields)
                                    (plan/compute stored :t :plan)))))))

(defspec a-refusal-names-its-cause 100
  (prop/for-all [t authored]
                (let [{:keys [refused]} (installed t)]
                  (not (and refused (str/includes? refused "malformed transform"))))))

(def ^:private export-gaps
  {:modules   {:modules {"kern" "def g():\n    return 1\n"}}
   :code-path {:code-path "transforms/anders.py"}})

(def ^:private plain
  {:language :python :code "def f(q):\n    return q\n"
   :inputs {:q {:kind :dataset :dataset :ein}} :output {:kind :dataset :dataset :aus}})

(defn- reinstall-changes
  [world]
  (let [desired (sugar/lower-schema (:schema (:ent (pemit/export world {:inline? true}))))]
    (remove #(= :skipped (:outcome %)) (install-plan/plan-schema desired world plan-opts))))

(defn- gap-fields
  [t]
  (filter #(contains? t %) (keys export-gaps)))

(defspec an-export-reinstalls-unchanged-but-for-a-known-gap 100
  (prop/for-all [t authored]
                (let [{:keys [world refused]} (installed t)]
                  (or refused (empty? (reinstall-changes world)) (seq (gap-fields t))))))

(deftest every-known-export-gap-is-still-open
  (is (empty? (reinstall-changes (:world (installed plain))))
      "the plain transform must round-trip, or the examples below prove nothing")
  (doseq [[k extra] export-gaps]
    (testing k
      (is (seq (reinstall-changes (:world (installed (merge plain extra)))))
          (str k " round-trips now: strike it from export-gaps")))))

(deftest pipeline-membership-round-trips
  (let [w (install empty-world (lowered {:pipelines  {:haupt {:description "Kette"}}
                                         :transforms {:t (assoc plain :pipeline :haupt)}}))]
    (is (some? (:pipeline (val (first (:transforms w))))))
    (is (empty? (reinstall-changes w)))))
