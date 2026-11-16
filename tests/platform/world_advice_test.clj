(ns platform.world-advice-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.catalog.world-advice :as ka]
            [sutra.world.db :as db]
            [sutra.world.gen.near :as gn]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]
            [sutra.commits.log :as log]))

(def ^:private names [:a :b :c])

(defn- rid-of [sec n] (gn/rid-of sec n))

(def ^:private g-name (gen/elements names))

(defn- g-ref [& secs]
  (gen/frequency [[4 (gen/let [s (gen/elements secs) n g-name] (rid-of s n))]
                  [2 g-name]
                  [1 (gen/fmap name g-name)]]))

(def ^:private type-names
  ["kunde" "status" "umsatz-monat" "table1" "a" "b" "auftrag-typ"])

(def ^:private g-attr
  (gen/let [n (gen/elements [:kunden-nr :name :titel :x :datum :notiz])
            t (gen/elements [:string :integer :text])
            vis (gen/elements [nil nil :hidden :prominent])]
    (cond-> {:attribute-name n :attribute-type t}
      vis (assoc :opts {:visibility vis}))))

(def ^:private g-lifecycle
  (gen/let [p (gen/elements [:x :status]) a g-name]
    {:property p :states [:offen :zu]
     :transitions [{:action a :from [:offen] :to :zu}]}))

(def ^:private dress
  {:object-types {:object-type-name (gen/fmap keyword (gen/elements type-names))
                  :display-name (gen/elements [nil "" "Kunde" "Status"])
                  :plural (gen/elements [nil "Kunden" "[LOG] x"])
                  :aliases (gen/elements [nil ["kunde"] ["status"]])
                  :description (gen/elements [nil "d"])
                  :match-keys (gen/elements [nil [:x] [:kunden-nr]])
                  :title-key (gen/elements [nil :kunden-nr :name :x])
                  :attribute-list (gen/vector g-attr 0 3)
                  :backing (gen/fmap (fn [d] {:dataset d}) (g-ref :datasets))
                  :lifecycle g-lifecycle
                  :status (gen/elements [nil :deprecated])
                  :deprecation (gen/elements [nil {:replacement :b}])
                  :project (g-ref :projects)}
   :datasets     {:kind (gen/elements [nil :standalone :object-backing :materialization])
                  :external-source (gen/elements [nil {:source :s}])
                  :status (gen/elements [nil :deprecated])}
   :sources      {:source-name g-name
                  :writeback (gen/fmap (fn [t] {"tbl" {:object-type t}})
                                       (gen/elements [:a :b "kunde" "status"]))
                  :type (gen/elements [nil :jdbc])
                  :config (gen/elements [nil {:url "(slurp x)"} {}])}
   :custom-applications {:custom-application-name g-name
                         :scope (gen/let [t (g-ref :object-types) f g-name]
                                  {:object-types [t] :functions [f]})
                         :layout (gen/fmap (fn [f] {:functions [f]}) g-name)}
   :actions      {:on (g-ref :object-types :interfaces)
                  :submission-criteria (gen/elements [nil [{:message ""}] [{:message "m"}]])
                  :status (gen/elements [nil :deprecated])}
   :applications {:layout (gen/let [ds (g-ref :datasets) l (gen/elements [:a :b :zz])
                                    app g-name]
                            {:upload {:dataset ds}
                             :children [{:visibility {:link l}}
                                        {:on-click {:do :open-app :app app}}]})
                  :form-factor (gen/elements [nil :mobile])
                  :published (gen/elements [nil "1"])
                  :object-type (g-ref :object-types)}
   :transforms   {:project (g-ref :projects)
                  :inputs (gen/let [k (gen/elements [:dataset :object-type :transform])
                                    x (g-ref :datasets :object-types :transforms)]
                            {:in {:kind k k x}})
                  :outputs (gen/let [d (g-ref :datasets) t (g-ref :object-types)]
                             [{:kind :dataset :dataset d} {:kind :object-type :object-type t}])}
   :projects     {:project-name g-name
                  :imports (gen/vector (g-ref :projects) 0 1)
                  :references (gen/elements [nil ["x"]])}
   :functions    {:function-name g-name}
   :interfaces   {:interface-name (gen/fmap keyword (gen/elements type-names))
                  :link-constraints (gen/elements [nil [{:link-name :l}]])}})

(def ^:private dressing
  (gen/let [sec (gen/frequency (for [k (keys dress)]
                                 [(if (#{:object-types :transforms} k) 4 1) (gen/return k)]))
            n   g-name
            f   (gen/elements (keys (dress sec)))
            v   ((dress sec) f)]
    [[sec (rid-of sec n)] {f v}]))

(def world
  (gen/let [W  gn/world
            ds (gen/vector dressing 0 24)]
    (reduce gn/apply-change W ds)))

(def change (gen/one-of [gn/change dressing]))

(def ^:private vocab (delay (spec/vocabulary)))

(defn- platform [W] (frequencies (lint/advisories W @vocab)))

(defn- kernel [d] (frequencies (ka/advisories d)))

(defn defined?
  [W]
  (and (gn/oracle-defined? W)
       (let [ws (for [[_ s] (:sources W) [_ a] (:writeback s)] (str (:object-type a)))]
         (= (count ws) (count (distinct ws))))))

(defspec the-kernel-advises-as-the-platform 300
  (prop/for-all [W world]
    (or (not (defined? W))
        (= (platform W) (kernel (db/db W ka/views))))))

(deftest the-worlds-reach-the-advisories
  (let [ws  (filter defined? (for [seed (range 1500)] (gen/generate world 60 seed)))
        hit (frequencies (for [W ws f (lint/advisories W @vocab)] (:rule f)))]
    (doseq [r (disj ka/rule-ids :source-config :app-source :action-override-redundant
                    :writeback-form-hand-built :var-shadows-type :pane-blind-for-audience
                    :view-too-many-profiles :attributes-unprioritized :input-unreferenced)]
      (is (pos? (hit r 0)) (str r)))))

(deftest a-cross-project-read-moves-with-the-imports
  (let [pa (gn/rid-of :projects :a) pb (gn/rid-of :projects :b)
        ot (gn/rid-of :object-types :b) t (gn/rid-of :transforms :a)
        W  {:projects     {pa {:id pa :project-name :a} pb {:id pb :project-name :b}}
            :object-types {ot {:id ot :object-type-name :b :project :b}}
            :transforms   {t {:id t :transform-name :a :project pa
                              :inputs {:in {:kind :object-type :object-type ot}}}}}
        d0 (db/db W ka/views)
        W1 (assoc-in W [:projects pa :imports] [pb])
        W2 (assoc-in W [:object-types ot :project] :a)]
    (is (= 1 (count (filter #(= :input-unreferenced (:rule %)) (lint/advisories W @vocab)))))
    (is (= (platform W) (kernel d0)))
    (is (= (platform W1) (kernel (log/stepped d0 W1))))
    (is (= (platform W2) (kernel (log/stepped d0 W2))))
    (is (not= (platform W) (platform W1)))))
