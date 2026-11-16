(ns ^:api ontology.incremental-snapshot-run-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [sutra.wire.api :as api]
            [sutra.compute.sandbox :as sandbox]
            [sutra.foundation.settings :as settings]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- py [f] (slurp (io/resource (str "support/fixtures/python/" f))))
(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- pks [ot]
  (set (map #(or (:pk %) (get-in % [:values :pk])) (:rows (:body (api/list-objects ot))))))

(defn- write! [ds mode tids]
  (is (ok? (api/post! :dataset-write {:name ds}
                      (cond-> {:rows (mapv (fn [t] {:values {:tid t :label t}}) tids)
                               :mode mode :message "seed"}
                        (= :seed_i ds) (assoc :key :tid))))))

(defn- labels [ot]
  (into {} (map (fn [r] [(or (:pk r) (get-in r [:values :pk]))
                         (or (:label r) (get-in r [:values :label]))]))
        (:rows (:body (api/list-objects ot)))))

(defn- pipeline [ot ds file]
  {:objects {ot {:symbol "🧩" :primary-key :pk :attributes [[:pk :string] [:label :string]]
                 :plural (str (name ot) "s")}}
   :transforms {(keyword (str "mk-" (name ot)))
                {:language :python :code (py file)
                 :inputs {:seed {:kind :dataset :dataset ds}}
                 :output {:kind :object-type :object-type ot}}}})

(def ^:private cases
  [[:widget_r :seed_r "incr_replace.py"]
   [:widget_q :seed_q "incr_require.py"]
   [:widget_s :seed_s "incr_snapshot_input.py"]
   [:widget_c :seed_c "incr_replace_capped.py"]
   [:widget_i :seed_i "incr_is_incremental.py"]
   [:widget_t :seed_t "incr_strict_append.py"]])

(deftest an-input-that-did-more-than-add-ends-the-increment
  (if-not (sandbox/available?)
    (println "  (incremental-snapshot-run SKIPPED — python sandbox unavailable)")
    (do
      (is (ok? (api/mutate :create-project {:name :snaprun} {:stage? true})))
      (doseq [[_ ds _] cases]
        (is (ok? (api/mutate :create-dataset
                             {:name ds :project :snaprun
                              :columns [(col :tid :string) (col :label :string)]}
                             {:stage? true}))))
      (is (ok? (api/post! :staging)))
      (doseq [[ot ds file] cases]
        (is (ok? (api/post! :install {:schema (in-project :snaprun (pipeline ot ds file))})))
        (is (ok? (api/post! :staging)))
        (write! ds :append ["a" "b"]))
      (is (ok? (api/post! :import-run)))
      (doseq [[ot ds] cases]
        (is (= #{"a" "b"} (pks ot)) (str ot))
        (write! ds :append ["c"]))

      (testing "only added: the output grows"
        (is (ok? (api/post! :import-run)))
        (doseq [[ot] (remove #(= :widget_c (first %)) cases)]
          (is (= #{"a" "b" "c"} (pks ot)) (str ot))))

      (testing "is_incremental: false on the first run, true on an incremental one"
        (is (= {"a" "False" "b" "False" "c" "True"} (labels :widget_i))))

      (testing "strict_append adds, and fails the run that would overwrite"
        (write! :seed_t :append ["dup"])
        (let [r (api/post! :import-run)]
          (is (= [:widget_t] (get-in r [:body :data :failed-jobs])))
          (is (= #{"a" "b" "c"} (pks :widget_t)))))

      (testing "a SNAPSHOT that rewrites the rows ends the increment"
        (is (ok? (api/post! :dataset-write {:name :seed_i}
                            {:rows (mapv (fn [t] {:values {:tid t :label (str t "-neu")}})
                                         ["a" "b" "c"])
                             :mode :snapshot :key :tid :message "rewrite"})))
        (is (= {"a" "False" "b" "False" "c" "False"}
               (do (api/post! :import-run) (labels :widget_i)))))

      (testing "rows removed"
        (doseq [[_ ds] (remove #(= :widget_i (first %)) cases)]
          (write! ds :snapshot ["a" "x" "y" "z"]))
        (let [r (api/post! :import-run)]
          (testing "the output is replaced, the previous output read empty"
            (is (= #{"a" "x" "y" "z"} (pks :widget_r))))
          (testing "require_incremental fails, the output stands"
            (is (= #{"a" "b" "c"} (pks :widget_q)))
            (is (some #{:widget_q} (get-in r [:body :data :failed-jobs]))))
          (testing "a snapshot input keeps the increment: read whole, previous output kept"
            (is (= #{"a" "b" "c" "x" "y" "z"} (pks :widget_s))))
          (testing "a replacing run may not cap: the rest of the output would be lost"
            (is (= #{"a" "b" "c"} (pks :widget_c)))
            (is (some #{:widget_c} (get-in r [:body :data :failed-jobs]))))
          (testing "strict_append runs a snapshot once an input ended the increment"
            (is (= #{"a" "x" "y" "z"} (pks :widget_t))))))

      (testing "the refusal is the setting refuse-capped-replace; off, the capped run replaces"
        (let [value settings/value]
          (with-redefs [settings/value #(if (= :refuse-capped-replace %) false (value %))]
            (api/post! :import-run)
            (is (= 2 (count (pks :widget_c))))))))))
