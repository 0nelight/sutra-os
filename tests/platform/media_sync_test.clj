(ns ^:unit platform.media-sync-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.media.sets :as msets]
            [sutra.blobs.api :as blobs]
            [sutra.smb :as smb]
            [sutra.types.backing :as backing]
            [sutra.types.entities :as ents]
            [sutra.types.file-sync :as fs]
            [sutra.types.kinds :as kinds]))

(def ^:private files
  [{:path "a/plan.pdf" :modified "2024-05-01T00:00:00Z" :size 100}
   {:path "a/plan.dwg" :modified "2024-05-01T00:00:00Z" :size 900}
   {:path "b/note.pdf" :modified "2020-01-01T00:00:00Z" :size 50}
   {:path "b/x.tmp"    :modified "2025-01-01T00:00:00Z" :size 1}])

(defn- taken [filters & [synced]] (mapv :path (:files (fs/select filters files synced))))

(deftest the-filters-narrow-what-a-sync-takes
  (is (= ["a/plan.dwg" "a/plan.pdf" "b/note.pdf" "b/x.tmp"] (taken nil)))
  (is (= ["a/plan.pdf" "b/note.pdf"] (taken {:path-matches "\\.pdf$"})))
  (is (= ["a/plan.dwg" "a/plan.pdf" "b/note.pdf"] (taken {:path-not-matches "\\.tmp$"})))
  (is (= ["a/plan.dwg" "a/plan.pdf" "b/x.tmp"] (taken {:modified-after "2021-01-01"})))
  (is (= ["a/plan.pdf" "b/note.pdf"] (taken {:size-between [10 500]})))
  (testing "a limit is the same files each time, and leaves the rest for the next run"
    (is (= ["a/plan.dwg" "a/plan.pdf"] (taken {:limit 2})))))

(deftest what-holds-a-sync-back-is-named
  (is (= :at-least (:held-by (fs/select {:at-least 9} files))))
  (is (= :any-file-matches (:held-by (fs/select {:any-file-matches "DONE$"} files))))
  (is (= 4 (count (:files (fs/select {:any-file-matches "tmp$"} files))))))

(deftest excluding-what-was-synced
  (let [was {"a/plan.pdf" {:modified "2024-05-01T00:00:00Z" :size 100}
             "b/note.pdf" {:modified "2019-01-01T00:00:00Z" :size 50}}]
    (testing "by path, a changed file stays out"
      (is (= ["a/plan.dwg" "b/x.tmp"] (taken {:exclude-synced true} was))))
    (testing "by date, a changed file comes again"
      (is (= ["a/plan.dwg" "b/note.pdf" "b/x.tmp"] (taken {:exclude-synced [:modified]} was))))
    (testing "by size, the same one does not"
      (is (= ["a/plan.dwg" "b/x.tmp"] (taken {:exclude-synced [:size]} was))))))

(deftest paths-read-alike-on-every-connector
  (is (= "CADT26/a.pdf" (fs/normal-path "\\CADT26\\a.pdf")))
  (is (= "a.pdf" (fs/relative-path "CADT26" "CADT26\\a.pdf")))
  (is (= "CADT261\\a.pdf" (.replace ^String (fs/relative-path "CADT26" "CADT261\\a.pdf") "/" "\\")))
  (is (= "a/b.pdf" (fs/relative-path "" "a\\b.pdf"))))

(deftest a-format-can-be-any-extension
  (is (= "application/pdf" (fs/mime-of "x/PLAN.PDF")))
  (is (= "application/x-dwg" (fs/mime-of "x/plan.dwg")))
  (is (= "application/octet-stream" (fs/mime-of "x/plan"))))

(deftest a-sync-with-memory-is-declared-apart
  (is (m/validate fs/SyncFilters {:exclude-synced [:modified :size] :limit 5}))
  (testing "a dataset's sync lists again each run, so it has nothing to exclude"
    (is (not (m/validate fs/Filters {:exclude-synced true})))))

(deftest a-virtual-address-names-set-and-file
  (let [a (msets/virtual-address :plaene "a/plan.pdf")]
    (is (blobs/valid-address? a))
    (is (= a (msets/virtual-address :plaene "a/plan.pdf")))
    (is (not= a (msets/virtual-address :plaene "a/plan2.pdf")))
    (is (not= a (msets/virtual-address :andere "a/plan.pdf")))))

(deftest a-sync-is-a-build-node-that-fills-a-set
  (let [t (backing/media-set-transform :plaene {:source :nas :subfolder "CADT26"})]
    (is (= :plaene-sync (:transform-name t)))
    (is (= {:table "CADT26"} (:source t)))
    (is (= [:media-sets :plaene] (kinds/output-target (:output t)))))
  (is (= {:table "/"} (:source (backing/media-set-transform :plaene {:source :nas}))))
  (is (= [:media-sets :plaene] (kinds/input-target {:kind :media-set-items :media-set :plaene}))))

(deftest a-set-a-sync-fills-is-held-by-the-instance
  (is (ents/authored-instance? :media-sets {:source :nas :sync {}}))
  (is (not (ents/authored-instance? :media-sets {:formats [:pdf]}))))

(defn- reader-over
  [^bytes content offset length closed]
  (smb/bounded-reader (fn [^bytes buf at off len]
                        (if (>= at (alength content))
                          -1
                          (let [n (min len (- (alength content) at))]
                            (System/arraycopy content (int at) buf (int off) (int n))
                            n)))
                      offset length #(reset! closed true)))

(deftest a-file-is-read-in-pieces-from-an-offset
  (let [content (.getBytes "0123456789")
        closed  (atom false)]
    (testing "the whole file"
      (with-open [in (reader-over content nil nil closed)]
        (is (= "0123456789" (slurp in)))))
    (is @closed)
    (testing "a range ends where it was asked to"
      (with-open [in (reader-over content 3 4 closed)]
        (is (= "3456" (slurp in)))))
    (testing "one byte at a time"
      (with-open [in (reader-over content 8 nil closed)]
        (is (= [56 57 -1] [(.read in) (.read in) (.read in)]))))))
