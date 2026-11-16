(ns ^:unit platform.media-type-inline-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.foundation.media-type :as mt]))

(defn- bytes-of [& xs] (byte-array (map unchecked-byte xs)))

(def ^:private png (bytes-of 0x89 0x50 0x4E 0x47 0x0D 0x0A 0x1A 0x0A))
(def ^:private html (.getBytes "<html><script>alert(1)</script>" "UTF-8"))

(deftest only-types-that-run-no-script-are-shown-inline
  (doseq [m ["image/png" "IMAGE/JPEG" "application/pdf" "text/plain; charset=utf-8"
             "video/mp4" "audio/mpeg"]]
    (is (mt/inline-safe? m) m))
  (doseq [m ["image/svg+xml" "text/html" "application/xhtml+xml" "text/xml"
             "application/javascript" "application/octet-stream" "" nil "text/html;x=image/png"]]
    (is (not (mt/inline-safe? m)) (pr-str m))))

(deftest an-upload-is-stored-as-what-its-bytes-are
  (is (= "image/png" (mt/checked "text/html" png)))
  (is (= "application/octet-stream" (mt/checked "image/png" html)))
  (is (= "text/csv" (mt/checked "text/csv" (.getBytes "a,b\n1,2" "UTF-8"))))
  (is (= "image/png" (mt/checked nil png))))
