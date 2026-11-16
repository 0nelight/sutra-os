(ns ^:api platform.flaw-blob-gc-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.blobs.api :as blobs]
            [sutra.media.http.sets :as media]
            [sutra.store.access.scope :as ctx]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth pod/as-admin)

(defn- bytes-of [s] (.getBytes (str s " " (random-uuid)) "UTF-8"))

(deftest swept-live
  (let [store (ctx/blobs)
        bs    (bytes-of "live")
        {:keys [hash]} (blobs/put! store bs)
        t0    (blobs/stored-at store hash)
        cutoff (inc t0)]
    (testing "the sweep scanned the blob as stale; a re-upload refreshes its pin before the delete"
      (Thread/sleep 20)
      (blobs/put! store bs)
      (is (false? (blobs/delete! store hash cutoff)) "the delete re-checks and keeps it")
      (is (blobs/exists? store hash)))
    (testing "untouched since the scan, it goes"
      (is (true? (blobs/delete! store hash (+ (blobs/stored-at store hash) 1)))))))

(deftest swept-history
  (is (ok? (api/mutate :create-project {:name :blobgc} {:stage? true})))
  (is (ok? (api/mutate :create-dataset
                       {:name :gc_media
                        :project :blobgc
                        :columns [{:attribute-name :k :attribute-type :string}
                                  {:attribute-name :ref :attribute-type :media-reference}]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (let [store (ctx/blobs)
        a     (:hash (blobs/put! store (bytes-of "einst")))
        b     (:hash (blobs/put! store (bytes-of "jetzt")))]
    (is (ok? (api/post! :dataset-write {:name "gc_media"}
                        {:rows [{:values {:k "1" :ref a}}] :message "a"})))
    (is (ok? (api/post! :dataset-write {:name "gc_media"}
                        {:rows [{:values {:k "1" :ref b}}] :mode :snapshot :message "b"})))
    (let [swept (atom #{})]
      (with-redefs [blobs/delete! (fn [_ h & _] (swap! swept conj h) false)]
        (media/sweep! (ctx/store) {:apply? true :grace-days 0}))
      (is (not (contains? @swept b)) "the current row's blob is a root")
      (is (not (contains? @swept a))
          "the blob only the dataset's history references is a root too"))))
