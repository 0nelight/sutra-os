(ns ^:unit grammar.canonical-print-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.canonical-print :as cp]))

(def ^:private ->canonical-value
  @(resolve (symbol "sutra.parser.canonical-print" "->canonical-value")))

(defn- durch-pprint
  ^String [ent]
  (let [txt (binding [*print-length*          nil
                      *print-level*           nil
                      *print-namespace-maps*  false
                      *print-readably*        true
                      pprint/*print-right-margin* 80
                      pprint/*print-miser-width*  40]
              (with-out-str (pprint/pprint (->canonical-value ent))))]
    (str (str/replace (str/trimr (str/replace txt "\r\n" "\n")) #"[ \t]+\n" "\n")
         "\n")))

(defn- zufallswert [^java.util.Random r tiefe]
  (case (.nextInt r (if (< tiefe 4) 9 5))
    0 (keyword (str "k" (.nextInt r 40)))
    1 (.nextInt r 100000)
    2 (apply str (repeat (.nextInt r 90) \x))
    3 (even? (.nextInt r 2))
    4 nil
    5 (mapv (fn [_] (zufallswert r (inc tiefe))) (range (.nextInt r 6)))
    6 (into {} (map (fn [i] [(keyword (str "k" i)) (zufallswert r (inc tiefe))]))
            (range (.nextInt r 6)))
    7 (set (map (fn [_] (.nextInt r 50)) (range (.nextInt r 5))))
    8 (into {} (map (fn [i] [(str "schluessel-" i) (zufallswert r (inc tiefe))]))
            (range (.nextInt r 4)))))

(defn- ent-dateien []
  (->> (concat (file-seq (io/file "tests")) (file-seq (io/file "resources")))
       (filter #(.isFile ^java.io.File %))
       (filter #(re-find #"\.(ent|enta|entv|entp)$" (.getName ^java.io.File %)))))

(deftest der-drucker-ist-bytegleich-zu-pprint
  (testing "Zufallswerte bis in die Umbruchgrenze hinein"
    (let [r (java.util.Random. 42)
          faelle (doall (map (fn [_] (zufallswert r 0)) (range 2000)))
          schlecht (remove #(= (durch-pprint %) (cp/canonical-ent-str %)) faelle)]
      (is (empty? (take 3 schlecht))
          "der eigene Drucker weicht bei einem Zufallswert von clojure.pprint ab")))
  (testing "jede .ent/.enta/.entv im Baum"
    (doseq [f (ent-dateien)
            :let [v (try (edn/read-string (slurp f)) (catch Throwable _ ::unlesbar))]
            :when (not= v ::unlesbar)]
      (is (= (durch-pprint v) (cp/canonical-ent-str v)) (.getPath ^java.io.File f)))))

(deftest ein-block-bricht-ganz-oder-gar-nicht
  (is (= "{:a 1, :b 2}\n" (cp/canonical-str {:a 1 :b 2})))
  (is (= (str "{:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 1,\n"
              " :bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 2,\n"
              " :cccccccccccccccccccccccccccccc 3}\n")
         (cp/canonical-str (sorted-map :aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 1
                                       :bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 2
                                       :cccccccccccccccccccccccccccccc 3)))))
