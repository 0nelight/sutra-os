(ns ^:unit ontology.transform-transport-render-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.compute.external :as ext]))

(def ^:private render #'ext/render)

(defn- with-xfer
  [limit f]
  (let [dir (java.nio.file.Files/createTempDirectory
             "xfer-test" (into-array java.nio.file.attribute.FileAttribute []))
        d   (str dir)]
    (with-redefs [ext/xfer-dir (delay d) ext/spill-threshold (delay limit)]
      (try (f d)
           (finally (doseq [x (reverse (file-seq (io/file d)))] (io/delete-file x true)))))))

(defn- leftovers [d] (remove #(= (io/file d) %) (file-seq (io/file d))))

(deftest a-small-payload-keeps-the-old-wire-shape
  (with-xfer 1000000
    (fn [d]
      (let [v {:transform-name :t :inputs {:a [{:id 1} {:id 2}]}}
            [kind s] (render v)]
        (is (= :inline kind))
        (is (= v (edn/read-string s)) "and it is the SAME value, rendered")
        (is (empty? (leftovers d))
            "the probe file is dropped — a small job must not leave the volume dirty")))))

(deftest a-payload-past-the-threshold-becomes-a-ref-and-round-trips
  (with-xfer 64
    (fn [_]
      (let [v {:media-blobs (vec (repeat 200 {:sha "abc" :b64 "QUJDREVGRw=="}))}
            [kind path] (render v)]
        (is (= :ref kind))
        (is (.exists (io/file path)))
        (is (= v (edn/read-string (slurp path :encoding "UTF-8")))
            "the worker reads back exactly what the backend held — the whole point of
             spilling the JOB rather than chunking rows: a transform that is not
             row-independent keeps exact semantics")))))

(deftest spilling-off-inlines-everything
  (with-redefs [ext/xfer-dir (delay nil)]
    (let [v {:big (vec (range 10000))}
          [kind s] (render v)]
      (is (= :inline kind) "SUTRA_XFER_DIR unset ⇒ byte-for-byte the old path")
      (is (= v (edn/read-string s))))))

(deftest the-threshold-is-read-off-the-file-not-off-a-string
  (with-xfer 100
    (fn [_]
      (let [[kind path] (render {:rows (vec (repeat 50 {:col "0123456789"}))})]
        (is (= :ref kind))
        (is (> (.length (io/file path)) 100)
            "measured on disk, which is where the size became knowable")))))

(def ^:private ship-inputs #'ext/ship-inputs)

(defn- an-input!
  [content blobs]
  (let [dir  (str (java.nio.file.Files/createTempDirectory
                   "in-test" (into-array java.nio.file.attribute.FileAttribute [])))
        f    (io/file dir "in.dat")
        mdir (io/file dir "media")]
    (spit f content)
    (.mkdirs mdir)
    (doseq [[n b] blobs] (spit (io/file mdir n) b))
    {:path (.getAbsolutePath f) :media-dir (.getAbsolutePath mdir)}))

(deftest with-the-shared-volume-an-input-ships-as-a-path
  (with-xfer 1000000
    (fn [d]
      (let [entry   (an-input! "ROWS" {"a.pdf" "AAAA" "b.pdf" "BBBB"})
            shipped (get (ship-inputs {:mail entry}) :mail)]
        (testing "no bytes ride on the wire"
          (is (nil? (:content shipped)))
          (is (nil? (:media-blobs shipped))))
        (testing "and the paths point INTO the shared mount, where the worker will open them"
          (is (str/starts-with? (:path shipped) d))
          (is (str/starts-with? (:media-dir shipped) d)))
        (testing "the copy is faithful — this is a transport change, not a data change"
          (is (= "ROWS" (slurp (:path shipped))))
          (is (= {"a.pdf" "AAAA" "b.pdf" "BBBB"}
                 (into {} (for [^java.io.File f (.listFiles (io/file (:media-dir shipped)))]
                            [(.getName f) (slurp f)])))))))))

(deftest without-the-shared-volume-nothing-changes
  (with-redefs [ext/xfer-dir (delay nil)]
    (let [entry   (an-input! "ROWS" {"a.pdf" "AAAA"})
          shipped (get (ship-inputs {:mail entry}) :mail)]
      (is (= "ROWS" (:content shipped)) "the old inline path, byte for byte")
      (is (= 1 (count (:media-blobs shipped))))
      (is (nil? (:path shipped))))))

(deftest a-language-model-entry-never-ships-bytes-either-way
  (let [lm {:kind :language-model :source :xai :model "grok"}]
    (with-xfer 1000000 (fn [_] (is (= lm (get (ship-inputs {:grok lm}) :grok)))))
    (with-redefs [ext/xfer-dir (delay nil)]
      (is (= lm (get (ship-inputs {:grok lm}) :grok))
          "path-less by design — the credential travels the secret pull, not this"))))

(deftest an-input-with-no-path-is-refused-loudly
  (with-xfer 1000000
    (fn [_]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no concrete path"
                            (ship-inputs {:mail {:kind :dataset}}))))))

(def ^:private run-remote #'ext/run-transform-remote!)

(defn- captured-job
  [opts]
  (let [seen (atom nil)]
    (with-redefs [ext/post-edn (fn [_ payload & _] (reset! seen payload) {})]
      (run-remote "http://worker" {:transform-name :t :code "c"} opts))
    @seen))

(deftest the-previous-output-travels-as-a-path-over-the-shared-volume
  (with-xfer 1000000
    (fn [d]
      (let [prev (io/file d "src-prev.dat")
            _    (spit prev "PREVIOUS ROWS")
            job  (captured-job {:inputs-manifest {} :previous-path (.getAbsolutePath prev)})]
        (is (nil? (:previous-content job)) "no bytes on the wire")
        (is (str/starts-with? (:previous-path job) d))
        (is (not= (.getAbsolutePath prev) (:previous-path job))
            "a COPY on the volume, so the caller's own file is never the worker's to hold")))))

(deftest without-the-shared-volume-the-previous-output-still-inlines
  (let [tmp (java.nio.file.Files/createTempDirectory
             "prev-test" (into-array java.nio.file.attribute.FileAttribute []))
        prev (io/file (str tmp) "src-prev.dat")]
    (spit prev "PREVIOUS ROWS")
    (with-redefs [ext/xfer-dir (delay nil)]
      (let [job (captured-job {:inputs-manifest {} :previous-path (.getAbsolutePath prev)})]
        (is (= "PREVIOUS ROWS" (:previous-content job)) "the old path, unchanged")
        (is (nil? (:previous-path job)))))))

(deftest a-run-with-no-previous-output-carries-neither-key
  (with-xfer 1000000
    (fn [_]
      (let [job (captured-job {:inputs-manifest {}})]
        (is (nil? (:previous-content job)))
        (is (nil? (:previous-path job)))))))

(deftest the-backend-cleans-up-every-copy-it-made
  (with-xfer 1000000
    (fn [d]
      (let [prev (io/file d "src-prev.dat")]
        (spit prev "PREVIOUS ROWS")
        (captured-job {:inputs-manifest {:mail (an-input! "ROWS" {"a.pdf" "AAAA"})}
                       :previous-path (.getAbsolutePath prev)})
        (is (= #{"src-prev.dat"}
               (into #{} (map #(.getName ^java.io.File %))
                     (remove #(= (io/file d) %) (file-seq (io/file d)))))
            "only the caller's own file is left — every xfi-/xfd-/previous copy is gone,
             and the worker never deletes them because a dispatch retry must re-read them")))))
