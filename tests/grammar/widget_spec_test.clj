(ns ^:unit grammar.widget-spec-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [malli.core :as m]
            [support.registry :as reg]
            [sutra.capps.bind :as bind]
            [sutra.capps.catalog :as cat]
            [sutra.parser.ent-view :as ent-view]))

(def ^:private sdk "clients/frontend/src/sutra_ui/apphost")

(def ^:private any-alias
  (reify
    clojure.lang.ILookup (valAt [_ k] k) (valAt [_ k _] k)
    clojure.lang.IFn (invoke [_ k] k) (invoke [_ k _] k)))

(defn- forms [f]
  (binding [r/*alias-map* any-alias r/*read-eval* false
            r/*default-data-reader-fn* (fn [_ v] v)]
    (let [rdr (rt/indexing-push-back-reader (slurp f))]
      (loop [acc []]
        (let [x (r/read {:eof ::eof :read-cond :allow :features #{:cljs}} rdr)]
          (if (= ::eof x) acc (recur (conj acc x))))))))

(defn- defined-maps
  [names]
  (into {}
        (for [^java.io.File f (file-seq (io/file sdk))
              :when (.endsWith (.getName f) ".cljs")
              x (forms f)
              :when (and (seq? x) (= 'def (first x)) (contains? names (second x))
                         (map? (last x)))]
          [[(subs (.getPath f) (inc (count sdk))) (second x)] (last x)])))

(def ^:private view-tables
  '#{widgets own-widgets})

(deftest every-widget-carries-its-record
  (reg/complete cat/widget-index [:widget :label :group :params])
  (doseq [{w :widget :as spec} cat/widget-specs]
    (testing w
      (is (contains? cat/group-labels (:group spec)))
      (when (contains? spec :loading)
        (is (contains? #{:spinner :table :kpi :kpi-row} (:loading spec))))
      (doseq [p (:params spec)]
        (testing (:key p)
          (is (keyword? (:key p)))
          (is (contains? cat/param-kinds (:kind p)) "a kind of param-kinds")
          (is (some? (cat/param-binds p)) "the binding forms it takes"))))))

(deftest every-param-kind-says-what-it-binds
  (reg/complete cat/param-kinds [:label :binds])
  (let [forms (into #{:literal :boolean} (map :id) bind/forms)]
    (doseq [[k {:keys [binds]}] cat/param-kinds
            :when (set? binds)]
      (testing k (is (= #{} (into #{} (remove forms) binds)))))))

(deftest every-container-carries-its-record
  (reg/complete cat/container-index [:kind :label :group :children :params]))

(deftest the-renderer-names-what-the-spec-names
  (let [tables (defined-maps view-tables)
        views  (apply merge (vals tables))]
    (is (= (count views) (reduce + (map count (vals tables)))) "no widget has two views")
    (reg/covers "a view for every widget, none for a widget the spec lacks"
                cat/widget-index views)
    (testing "a view written in its table is (cfg env), like every other"
      (doseq [[w v] views
              :when (and (seq? v) (= 'fn (first v)))]
        (is (= 2 (count (second v))) (str w)))))
  (testing "the renderer merges every view table of the SDK into its views"
    (let [render  (forms (io/file sdk "render.cljs"))
          aliases (into {} (for [[_ & clauses] (filter seq? (first render))
                                 spec clauses
                                 :when (vector? spec)
                                 :let [{as :as} (apply hash-map (rest spec))]
                                 :when as]
                             [(str as) (str (first spec))]))
          views   (some #(when (and (seq? %) (= 'def (first %)) (= 'views (second %)))
                           (last %))
                        render)
          merged  (into #{} (comp (filter symbol?)
                                  (map (fn [s] [(get aliases (namespace s) "render")
                                                (name s)])))
                        views)
          ns-of   (fn [file] (str (second (first (forms (io/file sdk file))))))]
      (doseq [[[file table] _] (defined-maps view-tables)]
        (testing file
          (is (contains? merged [(if (= "render.cljs" file) "render" (ns-of file))
                                 (str table)])
              (str table " in render/views"))))))
  (reg/covers "a loading view for every :loading, and the spinner a pending control shows"
              (into #{:spinner} (keep :loading) cat/widget-specs)
              (get (defined-maps '#{pending-views}) ["render.cljs" 'pending-views])))

(defn- all-params
  [ps]
  (mapcat (fn [p] (cons p (all-params (:item p)))) ps))

(deftest every-label-the-inspector-shows-is-the-catalogs
  (testing "every widget and container is named, in a group the palette names"
    (doseq [s (concat cat/widget-specs cat/container-specs)]
      (is (string? (:label s)) (pr-str (or (:widget s) (:kind s))))
      (is (contains? cat/group-labels (:group s)))))
  (testing "no param falls back to its raw keyword in the inspector"
    (is (= #{} (into (sorted-set)
                     (comp (remove :label) (map :key) (remove cat/param-labels))
                     (all-params (concat cat/common-params
                                         (mapcat :params (concat cat/widget-specs
                                                                 cat/container-specs))))))))
  (testing "what the editor shows first names widgets and params the spec holds"
    (is (= #{} (into #{} (remove #(or (cat/widget-spec %) (cat/container-spec %)))
                     (:ids cat/palette-common))))
    (is (= #{} (into #{} (remove (set (map :key (all-params
                                                 (mapcat :params (concat cat/widget-specs
                                                                         cat/container-specs))))))
                     cat/core-param-keys)))))

(defn- map-keys [schema] (set (map first (filter vector? (drop 2 schema)))))

(deftest the-parser-takes-what-the-spec-names
  (let [node-keys (set (map :key (concat cat/rider-params cat/common-params)))]
    (doseq [spec cat/widget-specs]
      (testing (:widget spec)
        (is (= #{} (into #{} (remove (map-keys (#'ent-view/leaf spec)))
                         (concat node-keys (map :key (:params spec))))))))
    (is (= #{} (into #{} (remove (map-keys (#'ent-view/container-schema)))
                     (concat node-keys (map :key cat/root-params)
                             (map :key (mapcat :params cat/container-specs))
                             (map :key (mapcat :children cat/container-specs)))))))
  (let [node (m/schema (#'ent-view/data-node))]
    (is (m/validate node {:kind :rows :mount :eager
                          :items [{:widget :badge :text "x" :unmount :never}]})
        ":mount/:unmount the inspector offered were refused before")
    (is (m/validate node {:kind :rows :items [{:widget :object-table :rows {:val :s}
                                               :export-csv? {:val :w}}]})
        "a :bool param takes a binding")
    (is (not (m/validate node {:kind :rows :items [{:widget :object-list :rows {:val :s}
                                                    :hide-null? "ja"}]}))
        "a :bool param refuses what is neither true/false nor a binding")))
