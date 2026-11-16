(ns ^:unit platform.lineage-reach-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.lineage.facts :as facts]
            [sutra.lineage.graph :as graph]))

(def ^:private schema
  {:object-types {"ri.o.1" {:object-type-name :rechnung}
                  "ri.o.2" {:object-type-name :partner}
                  "ri.o.3" {:object-type-name :position}}
   :link-types   {"ri.l.1" {:link-type-name :rechnung-partner}}
   :datasets     {"ri.d.1" {:dataset-name :uploads}}
   :sources      {"ri.s.1" {:source-name :erp}}
   :functions    {"ri.f.1" {:function-name :kern :language :clojure}
                  "ri.f.2" {:function-name :buchen :language :clojure :uses [:kern]
                            :source (str "(defn ^{:sutra/function {:edits [:rechnung :partner]}}"
                                         " buchen [x] x)")}}
   :transforms   {"ri.t.1" {:transform-name :rows :language :python
                            :external-systems [:erp]
                            :inputs {:u {:kind :media-set :dataset "ri.d.1"}
                                     :n {:kind :scalar}
                                     :m {:kind :media}
                                     :q {:kind :datasource :datasource :erp}}
                            :output {:kind :object-type :object-type "ri.o.1"}}
                  "ri.t.2" {:transform-name :liste :function-name :liste :language :clojure
                            :inputs {:r {:kind :object-type :object-type "ri.o.1"}}}}
   :actions      {"ri.a.1" {:action-name :alt}
                  "ri.a.2" {:action-name :erfassen
                            :deprecation {:replacement :alt}
                            :params {:p {:type :partner}
                                     :x {:type :string}
                                     :o {:type :string :options-from {:object-type :position}}}
                            :rules [{:op :apply-edit-function :function "ri.f.2"}
                                    {:op :action :name :alt}]}}
   :applications {"ri.p.1" {:application-name :kasse
                            :layout {:object-types [:rechnung]
                                     :functions [:liste]
                                     :actions [{:action :erfassen}]
                                     :vars {:a {:source :partner :where []}
                                            :b {:from :a}}
                                     :items [{:widget :embed :app :detail}
                                             {:over :rechnung-partner}
                                             {:from :position}]}}
                  "ri.p.2" {:application-name :detail}}})

(def ^:private edges
  (into #{} (map (juxt :from :rel :to)) (facts/edges schema nil)))

(deftest an-app-reaches-all-it-names
  (is (every? edges
              [["object-type/rechnung" :shows "application/kasse"]
               ["object-type/partner" :shows "application/kasse"]
               ["action/erfassen" :calls "application/kasse"]
               ["transform/liste" :calls "application/kasse"]
               ["link-type/rechnung-partner" :shows "application/kasse"]
               ["application/detail" :embeds "application/kasse"]]))
  (is (edges ["object-type/position" :shows "application/kasse"])
      "a {:from t} no variable declares still reads as the type it always meant"))

(deftest an-action-reaches-its-function-chain-and-parameter-types
  (is (every? edges
              [["function/buchen" :calls "action/erfassen"]
               ["action/alt" :calls "action/erfassen"]
               ["action/alt" :replaces "action/erfassen"]
               ["object-type/partner" :param "action/erfassen"]
               ["object-type/position" :param "action/erfassen"]])))

(deftest a-function-reaches-what-it-edits-and-its-library
  (is (every? edges
              [["object-type/rechnung" :edits "function/buchen"]
               ["object-type/partner" :edits "function/buchen"]
               ["function/kern" :uses "function/buchen"]])))

(deftest a-workspace-shelves-its-apps
  (let [es (into #{} (map (juxt :from :rel :to))
                 (facts/edges (assoc schema :workspaces
                                     {:kontor {:apps [:kasse] :anchored [:detail :weg]}})
                              nil))]
    (is (es ["application/kasse" :shelves "workspace/kontor"]))
    (is (es ["application/detail" :shelves "workspace/kontor"]))
    (is (not-any? #(= "application/weg" (first %)) es) "a name no app has draws no edge")))

(deftest a-transforms-inputs-are-real-nodes
  (is (edges ["dataset/uploads" :input "transform/rows"]))
  (is (edges ["source/erp" :input "transform/rows"]))
  (is (edges ["source/erp" :calls "transform/rows"]))
  (is (not-any? (fn [[from]] (re-find #"^(scalar|media|media-set|datasource)/" from)) edges)
      "a caller's argument is no node: it would join every transform that takes one"))

(deftest the-graph-finds-a-ref-where-its-kind-says
  (let [sch   (assoc-in schema [:transforms "ri.t.3"]
                        {:transform-name :sync-plaene
                         :input  {:kind :datasource :datasource :erp}
                         :output {:kind :media-set :media-set :plaene}})
        by    (into {} (map (juxt :name identity)) (graph/edges sch))
        [in]  (filter #(= :media-set (:kind %)) (:inputs (get by "rows")))]
    (is (= {:kind :media-set :rid "ri.d.1" :name "uploads"} in)
        "a media set read as its dataset keeps its own kind")
    (is (= [{:kind :media-set :name "plaene"}]
           (mapv #(select-keys % [:kind :name]) (:outputs (get by "sync-plaene"))))
        "a set a sync writes is named, not the kind")))
