(ns grammar.reference-walks-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [sutra.types.entities :as ents]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "plugins"])

(def ^:private home #{"libs/kernel/src/sutra/types/refs.cljc"
                      "libs/kernel/src/sutra/types/entities.cljc"})

(def reference-keys
  #{:to-object-type :from-object-type :target-type :implements :to-interface :interface
    :link-type :datasource :value-type :shared-property :media-set :options-from})

(def ^:private walkers '#{postwalk prewalk postwalk-replace prewalk-replace})

(defn forms [^java.io.File f]
  (with-open [rdr (java.io.PushbackReader. (io/reader f))]
    (binding [*read-eval* false *default-data-reader-fn* (fn [_ v] v)]
      (loop [acc []]
        (let [x (try (read {:eof ::eof :read-cond :allow :features #{:clj}} rdr)
                     (catch Exception _ ::eof))]
          (if (= ::eof x) acc (recur (conj acc x))))))))

(defn- nodes [x] (tree-seq coll? seq x))

(defn- names-refs? [x] (<= 2 (count (filter reference-keys (nodes x)))))

(defn- walks? [form]
  (some #(or (and (symbol? %) (walkers (symbol (name %))))
             (and (seq? %) (= 'fn (first %)) (symbol? (second %))
                  (re-find #"walk" (name (second %)))))
        (nodes form)))

(defn- key-table? [v]
  (and (map? v) (seq v)
       (every? (into (set (keys ents/author->section)) (vals ents/author->section)) (keys v))
       (every? set? (vals v))
       (names-refs? v)))

(defn offenders
  []
  (vec
   (for [r     roots
         ^java.io.File f (file-seq (io/file r))
         :let  [p (.getPath f)]
         :when (and (re-find #"\.clj[cs]?$" (.getName f)) (not (home p)))
         :let  [fs   (forms f)
                defs (into {} (for [x fs :when (and (seq? x) (#{'def 'def-} (first x)))]
                                [(second x) (last x)]))]
         x     fs
         :when (seq? x)
         :let  [nm  (second x)
                why (cond
                      (and (#{'def 'def-} (first x)) (key-table? (last x)))
                      :key-table
                      (and (walks? x)
                           (or (names-refs? x)
                               (some #(names-refs? (defs %)) (filter symbol? (nodes x)))))
                      :reference-walk)]
         :when why]
     [p nm why])))

(deftest no-namespace-walks-references-of-its-own
  (is (= [] (offenders))
      "read and rewrite references with sutra.types.refs over an ents spec"))

(deftest the-check-finds-what-it-is-for
  (let [f (java.io.File/createTempFile "walk" ".clj")]
    (try
      (spit f (pr-str '(def ^:private type-ref-keys #{:object-type :to-object-type :target-type}))
            :append false)
      (spit f (pr-str '(defn rename [m] (clojure.walk/postwalk #(if (type-ref-keys %) % %) m)))
            :append true)
      (spit f (pr-str '(def ref-keys {:datasets #{:dataset} :interfaces #{:interface :implements}
                                      :value-types #{:value-type}}))
            :append true)
      (let [fs   (forms f)
            defs (into {} (for [x fs :when (= 'def (first x))] [(second x) (last x)]))]
        (testing "a walk naming reference keys through a def of its namespace"
          (is (some #(names-refs? (defs %)) (filter symbol? (nodes (second fs)))))
          (is (walks? (second fs))))
        (testing "a table of the keys each section is named under"
          (is (key-table? (last (nth fs 2))))))
      (finally (.delete f)))))
