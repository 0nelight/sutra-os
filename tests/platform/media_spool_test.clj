(ns ^:unit platform.media-spool-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [sutra.blobs.api :as blobs]
            [sutra.blobs.fs :as blobs-fs]
            [sutra.compute.external :as ext]
            [sutra.compute.runner :as runner])
  (:import [java.security MessageDigest]))

(def ^:private code
  "import io, random, shutil
from sutra_transforms import transform, Output, MediaSetOutput

@transform(inputs={}, output=Output(kind='dataset', dataset='aus'),
           media_outputs={'zips': MediaSetOutput('zips')})
def zieh(zips):
    random.seed(7)
    blob = random.randbytes(5 * 1024 * 1024)
    with zips.open('bev/x.zip', 'wb') as f:
        shutil.copyfileobj(io.BytesIO(blob), f)
    return [{'id': '1'}]
")

(defn- sha256 [^bytes bs]
  (apply str (map #(format "%02x" (bit-and % 0xff))
                  (.digest (MessageDigest/getInstance "SHA-256") bs))))

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "spool_test_" (into-array java.nio.file.attribute.FileAttribute []))))

(deftest a-large-item-lands-by-stream
  (let [root   (temp-dir)
        store  (blobs-fs/blobs root)
        sink   (atom [])
        spools (atom [])
        result (with-redefs [ext/media-spool! (let [f ext/media-spool!]
                                                (fn [] (doto (f) (->> (swap! spools conj)))))]
                 (runner/run-code! :python {:transform-name :zieh :code code}
                                   {:blobs store :media-sink sink}))
        [item] (:media-set-items result)]
    (try
      (testing "the set item names a blob the store holds, byte for byte"
        (is (= "bev/x.zip" (:path item)))
        (is (= (* 5 1024 1024) (:size item)))
        (let [bs (with-open [in (blobs/open store (:address item))]
                   (.readAllBytes ^java.io.InputStream in))]
          (is (= (:address item) (sha256 bs)))))
      (testing "the sink saw it once, and the spool is cleaned up"
        (is (= [(:address item)] (mapv :hash @sink)))
        (is (= 1 (count @spools)))
        (is (not (.exists (io/file (first @spools))))))
      (finally (ext/delete-tree! root)))))
