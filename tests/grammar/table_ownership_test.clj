(ns ^:unit grammar.table-ownership-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.walk :as walk]
            [sutra.store.control.tables :as cc]))

(def ^:private roots ["src" "libs/core/src"])

(def ^:private exempt
  '#{sutra.store.control sutra.store.control.tables})

(def ^:private api
  '#{list get count counts written expect expecting swapping updating deleting rewriting
     plan-as! plan! swap! insert! upsert-as! upsert! insert-many! update-as! update!
     delete-as! delete! entry served inserting upserting})

(defn- modules [] (:modules (edn/read-string (slurp "cores.edn"))))

(defn- all-tables [] (set (map #(keyword (name (:coll %))) (vals cc/by-coll))))

(def ^:private sql-names
  (into {} (keep (fn [{:keys [table name]}] (when table [table name]))) cc/tables))

(def ^:private any-alias
  (reify
    clojure.lang.ILookup (valAt [_ k] k) (valAt [_ k _] k)
    clojure.lang.IFn (invoke [_ k] k) (invoke [_ k _] k)))

(defn- forms [f]
  (binding [r/*alias-map* any-alias r/*read-eval* false
            r/*default-data-reader-fn* (fn [_ v] v)]
    (let [rdr (rt/indexing-push-back-reader (slurp f))]
      (loop [acc []]
        (let [x (r/read {:eof ::eof :read-cond :allow :features #{:clj}} rdr)]
          (if (= ::eof x) acc (recur (conj acc x))))))))

(defn- ctl-alias [ns-form]
  (some (fn [clause]
          (when (and (seq? clause) (= :require (first clause)))
            (some #(when (and (vector? %) (= 'sutra.store.control (first %)))
                     (second (drop-while (complement #{:as}) %)))
                  (rest clause))))
        ns-form))

(defn- named-tables
  [tables f]
  (let [[nsf & body] (forms f)
        nsn (second nsf)
        al  (some-> (ctl-alias nsf) str)
        out (volatile! #{})]
    (doseq [x body]
      (walk/postwalk
       (fn [y]
         (cond
           (and al (seq? y) (symbol? (first y)) (= al (namespace (first y)))
                (api (symbol (name (first y)))))
           (doseq [a (take 4 (rest y)) :when (and (keyword? a) (tables a))]
             (vswap! out conj [nsn a]))
           (and (keyword? y) (= "ctl" (namespace y)) (tables (keyword (name y))))
           (vswap! out conj [nsn (keyword (name y))])
           (string? y)
           (doseq [[_ t] (re-seq #"\b(control_[a-z_]+)\b" y) :when (sql-names t)]
             (vswap! out conj [nsn (sql-names t)])))
         y)
       x))
    @out))

(defn- accesses []
  (let [tables (all-tables)]
    (into #{}
          (comp (mapcat #(file-seq (io/file %)))
                (filter #(re-find #"\.cljc?$" (.getName ^java.io.File %)))
                (mapcat #(named-tables tables %))
                (remove (comp exempt first)))
          roots)))

(defn- module-of [ns] (second (str/split (str ns) #"\.")))

(defn- owner-index [owned]
  (into {} (for [[m ts] owned t ts] [(keyword t) (str m)])))

(deftest every-table-has-one-owner
  (let [{:keys [tables tiers]} (modules)
        owned (for [[_ ts] tables t ts] (keyword t))]
    (testing "no table belongs to two modules"
      (is (= [] (vec (for [[t n] (frequencies owned) :when (> n 1)] t)))))
    (testing "every control table has an owner, every owned table exists"
      (is (= #{} (set (remove (set owned) (all-tables)))))
      (is (= #{} (set (remove (all-tables) owned)))))
    (testing "an owner is a module on a tier"
      (is (= #{} (set (remove (set (flatten tiers)) (keys tables))))))))

(deftest no-module-names-a-foreign-table
  (let [{:keys [tables table-allow]} (modules)
        owner   (owner-index tables)
        allowed (set (for [{:keys [edges]} table-allow [n t] edges] [n (keyword t)]))
        foreign (set (filter (fn [[n t]] (not= (module-of n) (owner t))) (accesses)))]
    (testing "a foreign access goes through the owner, or :table-allow says why it does not"
      (is (= #{} (set (remove allowed foreign)))))
    (testing "every allowed foreign access still exists"
      (is (= #{} (set (remove foreign allowed)))))))

(deftest cores-edn-is-one-value
  (let [rdr (java.io.PushbackReader. (io/reader "cores.edn"))]
    (edn/read rdr)
    (is (= ::end (edn/read {:eof ::end} rdr))
        "cores.edn holds more than one form — a stray closing bracket ends the map early")))
