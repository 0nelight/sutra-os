(ns ^:api ontology.dataset-files-test
  (:require [support.wire :refer [ok?]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.compute.sandbox :as sandbox]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private columns
  [(col :id :string) (col :n :long) (col :amt [:decimal 12 2]) (col :d :date)
   (col :note :string)])

(def ^:private rows
  [{:values {:id "a" :n 1 :amt 12.5M :d "2026-11-11" :note "plain"}}
   {:values {:id "b" :n 2 :amt 0.1M :d "2026-01-02" :note "comma, \"quote\"\r\nbreak"}}])

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- preview [nm]
  (->> (:rows (:body (api/get! :dataset-preview {:id (dataset-rid nm)})))
       (map :values)
       (map #(-> % (update :amt (fn [v] (some-> v bigdec (.setScale 2))))
                 (update :d str)))
       (sort-by :id)))

(defn- export [nm fmt]
  (tx/get-bytes! (paths/address :dataset-export {:id (dataset-rid nm) :format (name fmt)})))

(defn- import! [nm fmt bytes]
  (tx/post-bytes! (paths/address :dataset-import {:name (name nm) :format (name fmt)
                                                  :mode "snapshot" :key "id"})
                  bytes))

(deftest a-dataset-crosses-as-a-file
  (testing "three datasets of the same columns"
    (is (ok? (api/mutate :create-project {:name :files} {:stage? true})))
    (doseq [nm [:src :from_csv :from_parquet :from_arrow]]
      (is (ok? (api/mutate :create-dataset {:name nm :project :files :columns columns}
                           {:stage? true}))))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/post! :dataset-write {:name "src"} {:rows rows :key "id"}))))

  (testing "CSV is RFC 4180: CRLF records, a field with , \" CR or LF quoted"
    (let [{:keys [status bytes content-type]} (export :src :csv)
          text (String. ^bytes bytes "UTF-8")]
      (is (= 200 status))
      (is (str/starts-with? (str content-type) "text/csv"))
      (is (str/starts-with? text "id,n,amt,d,note\r\n"))
      (is (str/includes? text "\"comma, \"\"quote\"\"\r\nbreak\"\r\n"))
      (is (= 200 (:status (import! :from_csv :csv bytes))))
      (is (= (preview :src) (preview :from_csv)) "the CSV gives back the rows")
      (is (= 400 (:status (export :src :xml))) "an unknown format is refused")))

  (cond
    (not (sandbox/available?))
    (println "  (Parquet/Arrow SKIPPED — python sandbox unavailable)")

    (str/includes? (String. ^bytes (:bytes (export :src :parquet)) "UTF-8")
                   "pyarrow is not installed")
    (println "  (Parquet/Arrow SKIPPED — the sandbox's python has no pyarrow; the image pins it)")

    :else
    (doseq [[fmt target] [[:parquet :from_parquet] [:arrow :from_arrow]]]
      (testing (str (name fmt) " round trip")
        (let [{:keys [status bytes]} (export :src fmt)]
          (is (= 200 status) (String. ^bytes bytes "UTF-8"))
          (is (= 200 (:status (import! target fmt bytes))))
          (is (= (preview :src) (preview target))))))))
