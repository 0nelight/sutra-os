(ns ^:api platform.cli-export-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.cli.checkout :as checkout]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private tmp
  (str (java.nio.file.Files/createTempDirectory
        "cli-archive" (make-array java.nio.file.attribute.FileAttribute 0))))

(deftest archive-writes-reinstallable-scope-ent
  (testing "seed a tiny ontology inside a project"
    (api/mutate :create-project {:name :kit} {:stage? true})
    (api/mutate :create-object-type
                {:name :widget
                 :project :kit
                 :primary-key :sku
                 :attributes [{:attribute-name :sku :attribute-type :string}
                              {:attribute-name :label :attribute-type :string}]
                 :plural "Widgets"}
                {:stage? true})
    (is (ok? (api/post! :staging)) "commit :kit + :widget"))

  (testing "`clone kit --archive` writes a single .ent that parses + carries the model"
    (let [out  (str tmp "/cli-archive-kit.ent")
          _    (io/delete-file out true)
          code (checkout/archive! {:yes? true} :kit out)]
      (is (= 0 code) "archive exits 0")
      (is (.exists (io/file out)) "the .ent file was written")
      (let [m (edn/read-string (slurp out))]
        (is (map? m) "valid EDN")
        (is (get-in m [:schema :objects :widget]) "the object model is present")
        (is (= [[:sku :string] [:label :string]]
               (get-in m [:schema :objects :widget :attributes]))
            "attributes round-trip in order"))))

  (testing "the archived .ent RE-INSTALLS cleanly"
    (let [out (str tmp "/cli-archive-kit.ent")
          m   (edn/read-string (slurp out))]
      (is (ok? (api/post! :install m)) "re-install stages")
      (is (ok? (api/post! :staging)) "re-install commits")))

  (testing "`clone kit --archive --zip` writes a real zip archive"
    (let [out  (str tmp "/cli-archive-kit.zip")
          _    (io/delete-file out true)
          code (checkout/archive! {:yes? true :zip? true} :kit out)]
      (is (= 0 code) "zip archive exits 0")
      (is (.exists (io/file out)) "the .zip file was written")
      (with-open [in (io/input-stream out)]
        (let [b0 (.read in) b1 (.read in)]
          (is (and (= 0x50 b0) (= 0x4B b1)) "starts with the ZIP magic \"PK\"")))))

  (testing "the DEPRECATED `export!` alias still works — warns, then delegates to archive!"
    (let [out  (str tmp "/cli-export-alias.ent")
          _    (io/delete-file out true)
          code (checkout/export! {:yes? true} :kit out)]
      (is (= 0 code) "export alias exits 0")
      (is (.exists (io/file out)) "the deprecated alias still writes the .ent"))))

(deftest whole-instance-backup-must-be-named-and-must-be-a-zip
  (testing "no --out is refused rather than defaulted"
    (is (= 2 (checkout/archive! {:yes? true} nil nil))
        "unnamed whole-instance backup exits 2")
    (is (= 2 (checkout/archive! {:yes? true} nil ""))
        "blank --out is treated as unnamed"))

  (testing "a non-zip --out is refused — the backup spans every scope"
    (is (= 2 (checkout/archive! {:yes? true} nil (str tmp "/backup.ent")))
        ".ent whole-instance backup exits 2")
    (is (not (.exists (io/file (str tmp "/backup.ent"))))
        "and nothing was written")))

(deftest whole-instance-backup-packs-every-scope-into-one-zip
  (testing "`clone --archive --out b.zip` writes one zip holding every scope"
    (let [out (str tmp "/whole-instance.zip")
          _   (io/delete-file out true)
          code (checkout/archive! {:yes? true} nil out)]
      (is (= 0 code) "backup exits 0")
      (is (.exists (io/file out)) "the .zip was written at the name we gave")
      (let [entries (with-open [zin (java.util.zip.ZipInputStream. (io/input-stream out))]
                      (doall (loop [acc []]
                               (if-let [e (.getNextEntry zin)]
                                 (recur (conj acc (.getName e)))
                                 acc))))]
        (is (some #(= "MANIFEST.txt" %) entries) "carries a MANIFEST")
        (is (some #(.startsWith ^String % "kit/") entries)
            "the :kit project's scope is present, prefixed by scope name")
        (is (some #(.startsWith ^String % "unassigned/") entries)
            "the unassigned scope is present too — that's why it can't be one .ent")

        (testing "every restore line in the MANIFEST names an entry that EXISTS"
          (let [manifest (with-open [zin (java.util.zip.ZipInputStream. (io/input-stream out))]
                           (loop []
                             (when-let [e (.getNextEntry zin)]
                               (if (= "MANIFEST.txt" (.getName e))
                                 (String. (.readAllBytes zin) "UTF-8")
                                 (recur)))))
                named    (->> (str/split-lines manifest)
                              (keep #(second (re-find #"sutra --yes (\S+)" %))))]
            (is (seq named) "the manifest lists restore commands")
            (doseq [p named]
              (is (some #{p} entries)
                  (str "manifest names " p " — it must be a real zip entry")))))))))
