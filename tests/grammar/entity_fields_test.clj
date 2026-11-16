(ns ^:unit grammar.entity-fields-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.repl :as repl]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.catalog.actions :as actions]
            [sutra.types.entities :as ents]
            [sutra.catalog.kernel :as kernel]
            [sutra.ref.install :as ri]))

(defn- keys-read
  [sym]
  (when-let [src (try (repl/source-fn sym) (catch Exception _ nil))]
    (let [ks (atom #{})]
      (walk/postwalk
       (fn [x]
         (when (map? x)
           (doseq [s (:keys x)] (swap! ks conj (keyword (str s)))))
         (when (and (seq? x) (= 2 (count x)) (keyword? (first x)) (symbol? (second x)))
           (swap! ks conj (first x)))
         x)
       (read-string src))
      @ks)))

(defn- kernel-reads [entity]
  (let [{:keys [section step-marks]} (ri/class-by-tag entity)
        spec (partial ri/spec kernel/install-cx entity)
        m (meta spec)]
    (into (if (var? spec)
            (or (keys-read (symbol (str (ns-name (:ns m))) (str (:name m)))) #{})
            (set (keys (ents/projected-fields section))))
          step-marks)))

(def ^:private known-internal
  {:object-type #{:ext-source :symbol-str :plural-str}
   :interface   #{:link-name :to-object-type :to-interface :cardinality :required}})

(def ^:private checked-entities
  [:object-type :link-type :interface :transform :source :dataset :function
   :object-type-group :shared-property :action])

(deftest every-field-the-kernel-reads-is-forwarded
  (doseq [entity checked-entities]
    (testing (name entity)
      (let [declared (set (get actions/entity-fields entity))
            dropped  (set/difference (kernel-reads entity)
                                     declared
                                     #{:name}
                                     (get known-internal entity #{}))]
        (is (empty? dropped)
            (str "sutra.catalog.kernel reads " (str/join ", " (sort dropped))
                 " for a " (name entity) ", but declare/entity-fields does not"
                 " forward " (if (= 1 (count dropped)) "it" "them")
                 " — the passthrough rule will drop the value silently, and the"
                 " action will still report success. Add to entity-fields."))))))
