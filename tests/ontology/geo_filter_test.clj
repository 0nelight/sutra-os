(ns ^:unit ontology.geo-filter-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [support.geo :as g]
            [support.select-sql :as sel]
            [sutra.capps.variables :as vars]
            [sutra.cli.assemble :as assemble]
            [sutra.ref.geo :as geo]
            [sutra.ref.objectset :as ros]
            [sutra.store.core]
            [sutra.store.mem :as mem]
            [sutra.types.query :as q]))

(deftest distance-is-great-circle
  (is (< 37.0 (/ (geo/meters g/villach [46.6425 14.3377]) 1000) 38.0) "Villach–Klagenfurt airport")
  (is (< 20000 (/ (geo/meters [0 0] [0 180]) 1000) 20040) "half the equator")
  (is (zero? (geo/meters g/villach g/villach))))

(deftest a-point-reads-in-every-spelling
  (is (= [46.6 13.85] (geo/point "46.6, 13.85") (geo/point [46.6 13.85])
         (geo/point {:lat 46.6 :lon 13.85})))
  (is (nil? (geo/point "n/a")))
  (is (nil? (geo/point "1,2,3")))
  (is (nil? (geo/point nil))))

(deftest the-door-refuses-a-filter-that-cannot-mean-anything
  (doseq [[pred msg] [[[:within-distance :lage {:center [91 0] :distance 1}] #"latitude"]
                      [[:within-distance :lage {:center [0 181] :distance 1}] #"longitude"]
                      [[:within-distance :lage {:center "Villach" :distance 1}] #"must be a point"]
                      [[:within-distance :lage {:center [0 0] :distance -1}] #"≥ 0"]
                      [[:within-distance :lage {:center [0 0] :distance 1 :unit :parsec}]
                       #"is not one of"]
                      [[:within-distance [:lat] {:center [0 0] :distance 1}] #"pair"]
                      [[:within-bounding-box :lage {:top-left [10 0] :bottom-right [20 5]}]
                       #"south of"]
                      [[:within-distance :lage] #"field"]]]
    (is (re-find msg (str (:message (q/explain-predicate pred)))) (pr-str pred))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ros/read {} {} {:op :filter :where pred :in {:op :scan :root :t}}))
        "the kernel refuses it as malformed before it reads")))

(deftest the-reference-answers-every-case
  (let [db (mem/store)]
    (g/seed! db)
    (doseq [[where codes] g/cases]
      (is (= codes (g/codes db where)) (pr-str where)))))

(deftest a-param-fills-the-filter
  (is (q/valid-predicate? [:within-distance :lage {:center [:param :ort] :distance [:param :km]}])
      "a saved object set names its slots; they are judged once filled")
  (is (q/valid-predicate? [:within-bounding-box :lage [:param :box]]))
  (is (= [:within-distance :lage {:center [1 2] :distance 5}]
         (:where (ros/instantiate {:op :filter :param-slots [{:name :ort} {:name :km}]
                                   :where [:within-distance :lage
                                           {:center [:param :ort] :distance [:param :km]}]
                                   :in {:op :scan :root :t}}
                                  {:ort [1 2] :km 5})))))

(deftest pg-computes-the-distance-in-sql
  (let [[sql params] (sel/compile-sql g/world
                                      {:op :filter
                                       :where [:within-distance [:lat :lon]
                                               {:center g/villach :distance 300}]
                                       :in {:op :scan :root g/flughafen}})]
    (is (re-find #"asin\(sqrt" sql))
    (is (some #{300000.0} params) "the distance travels as a parameter, in meters"))
  (let [[sql] (sel/compile-sql g/world
                               {:op :filter
                                :where [:within-bounding-box :lage
                                        {:top-left [-10 170] :bottom-right [-40 -170]}]
                                :in {:op :scan :root g/flughafen}})]
    (is (re-find #">= \? OR .* <= \?" sql) "a box across the antimeridian is two half-planes")
    (is (str/includes? sql "split_part") "a geopoint is read from its text")))

(deftest the-cli-writes-a-geo-filter
  (is (= {[:lat :lon] {:within-distance {:center [46.61 13.85] :distance 300.0 :unit :km}}}
         (assemble/parse-where "lat+lon@46.61/13.85/300km")))
  (is (= {:lage {:within-distance {:center [46.61 13.85] :distance 20.0 :unit :mi}}}
         (assemble/parse-where "lage@46.61/13.85/20mi")))
  (is (= {:lage {:within-bounding-box {:top-left [48.0 12.0] :bottom-right [46.0 17.0]}}}
         (assemble/parse-where "lage@48/12/46/17")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"@ takes" (assemble/parse-where "lage@46/13"))))

(deftest an-app-var-filters-by-distance
  (let [rows (mapv (fn [r] {:values r}) g/rows)
        apply-where #'vars/apply-where
        codes (fn [clauses vals]
                (set (map (comp :code :values) (apply-where rows clauses #(get vals (:val %) %)))))]
    (is (= #{"KLU" "LJU" "TSF" "VIE"}
           (codes [[[:lat :lon] :within-distance {:center {:val :ort} :distance {:val :km}}]]
                  {:ort "46.6167,13.85" :km 300})))
    (is (= #{"KLU"}
           (codes [[:lage :within-distance {:center [{:val :lat} 13.85] :distance 50}]]
                  {:lat 46.6167})))
    (is (= (count rows)
           (count (codes [[:lage :within-distance {:center {:val :ort} :distance 50}]] {:ort ""})))
        "no center yet widens, as an empty :in selection does")))
