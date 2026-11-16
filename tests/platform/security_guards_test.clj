(ns ^:unit platform.security-guards-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sutra.calx.io.xlsx :as xlsx]
            [sutra.calx.io.zip :as zip]
            [sutra.compute.bounded :as bounded]
            [sutra.compute.clojure :as clj]
            [sutra.foundation.confine :as confine]
            [sutra.foundation.env :as env]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.serve.hot-patches :as hot]
            [sutra.sources.datasources :as ds])
  (:import (java.io ByteArrayOutputStream)
           (java.util.zip ZipEntry ZipOutputStream)))

(defn- zip-of
  ^bytes [entries]
  (let [bos (ByteArrayOutputStream.)]
    (with-open [z (ZipOutputStream. bos)]
      (doseq [[nm ^bytes bs] entries]
        (.putNextEntry z (ZipEntry. ^String nm))
        (.write z bs)
        (.closeEntry z)))
    (.toByteArray bos)))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest a-zip-bomb-stops-at-its-bound
  (let [bomb (zip-of [["xl/worksheets/sheet1.xml" (byte-array (* 8 1024 1024))]])]
    (is (< (alength bomb) (* 64 1024)) "small on the wire")
    (testing "the inflate ratio stops it before the entry is read whole"
      (is (= :ratio (:reason (refusal #(zip/read-entries bomb))))))
    (testing "and through the xlsx reader /api/kompass/tabular uses"
      (is (:zip/refused (refusal #(xlsx/read-xlsx bomb)))))
    (testing "per entry and in total"
      (is (= :entry-too-large
             (:reason (refusal #(zip/read-entries bomb {:max-entry-bytes 1000 :max-ratio 1e9})))))
      (is (= :total-too-large
             (:reason (refusal #(zip/read-entries
                                 (zip-of [["a" (byte-array 600)] ["b" (byte-array 600)]])
                                 {:max-total-bytes 1000}))))))
    (testing "entry count"
      (is (= :too-many-entries
             (:reason (refusal #(zip/read-entries
                                 (zip-of (for [i (range 5)] [(str "f" i) (byte-array 1)]))
                                 {:max-entries 3}))))))
    (testing "a well-formed zip reads"
      (is (= {"a/b.txt" "hi"}
             (update-vals (zip/read-entries (zip-of [["a/b.txt" (.getBytes "hi")]]))
                          #(String. ^bytes %)))))))

(deftest a-zip-slip-name-is-refused
  (doseq [nm ["../evil.sh" "a/../../evil" "/etc/passwd" "C:/x" "a\\..\\b"]]
    (is (= :unsafe-name (:reason (refusal #(zip/read-entries (zip-of [[nm (byte-array 1)]])))))
        nm)))

(deftest a-bounded-stream-refuses-past-its-limit
  (let [in (zip/bounded (java.io.ByteArrayInputStream. (byte-array 2000)) 1000)]
    (is (thrown? java.io.IOException (.readAllBytes in)))))

(deftest confine-path-holds-a-path-in-its-root
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "confine" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (is (= (.getCanonicalPath (io/file root "a/b.txt"))
           (.getPath (confine/confine-path root "a/b.txt"))))
    (doseq [[p why] [["../x" :dot-dot] ["a/../../x" :dot-dot] ["/etc/passwd" :absolute]
                     ["-rf" :leading-dash] ["" :empty-or-nul] ["a\u0000b" :empty-or-nul]]]
      (is (= why (:reason (refusal #(confine/confine-path root p)))) p))
    (testing "a symlink out of the root"
      (let [link (io/file root "out")]
        (java.nio.file.Files/createSymbolicLink
         (.toPath link) (.toPath (io/file "/tmp"))
         (make-array java.nio.file.attribute.FileAttribute 0))
        (is (= :outside (:reason (refusal #(confine/confine-path root "out/x")))))))))

(deftest a-trace-file-lands-in-the-trace-dir
  (let [dir (.getCanonicalPath (io/file (sqltrace/trace-dir)))]
    (is (= (str dir "/passwd") (sqltrace/trace-file "/etc/passwd")))
    (is (str/starts-with? (sqltrace/trace-file "../..") (str dir "/sql-")))
    (is (nil? (sqltrace/trace-file nil)))))

(defn- spin-ctx [stop]
  (sci/init {:namespaces {'user {'stop? #(deref stop)}}}))

(deftest a-cpu-loop-is-abandoned-at-its-deadline
  (let [stop (atom false)
        ctx  (spin-ctx stop)
        n0   (bounded/abandoned-count)]
    (try
      (is (= ::bounded/timeout
             (bounded/call "spin" 200
                           #(sci/eval-string* ctx "(loop [] (when-not (stop?) (recur)))"))))
      (is (= (inc n0) (bounded/abandoned-count)) "the loop ignored its interrupt and is counted")
      (testing "at the cap no new run starts"
        (with-redefs [bounded/max-abandoned (bounded/abandoned-count)]
          (is (= :sandbox-saturated (:reason (refusal #(bounded/call "x" 100 (constantly 1))))))))
      (finally (reset! stop true)))
    (testing "a value and an exception come back as they are"
      (is (= 3 (bounded/call "ok" 1000 #(+ 1 2))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                            (bounded/call "err" 1000 #(throw (ex-info "boom" {}))))))))

(deftest a-transform-that-compiles-forever-is-refused
  (with-redefs [clj/compile-ms 100]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"failed to compile: no result within"
                          (clj/introspect
                           "(loop [i 0] (if (< i 20000000) (recur (inc i)) (fn [_] {})))")))
    (is (re-find #"no result within"
                 (str (clj/compile-edit-function
                       "(loop [i 0] (if (< i 20000000) (recur (inc i)) nil))"))))))

(defn- with-env [m f]
  (with-redefs [env/getenv #(get m %)] (f)))

(deftest hot-reload-is-off-unless-armed-and-not-production
  (is (false? (with-env {} hot/armed?)))
  (is (true? (with-env {"SUTRA_HOT_RELOAD" "1"} hot/armed?)) "a dev process")
  (is (false? (with-env {"SUTRA_HOT_RELOAD" "1" "SUTRA_IMAGE_ID" "i"} hot/armed?))
      "a deployed image that does not name its instance")
  (is (false? (with-env {"SUTRA_HOT_RELOAD" "1" "SUTRA_IMAGE_ID" "i" "SUTRA_INSTANCE" "sutra"}
                hot/armed?)))
  (is (true? (with-env {"SUTRA_HOT_RELOAD" "1" "SUTRA_IMAGE_ID" "i" "SUTRA_INSTANCE" "staging"}
               hot/armed?)))
  (is (false? (with-env {"SUTRA_IMAGE_ID" "i"} hot/dev-surfaces?)) "nREPL too")
  (testing "a disarmed process loads nothing"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"disarmed"
                          (with-env {} #(hot/load-source! "src/a.clj" "(ns a)")))))
  (testing "an armed one only a .clj under the app root that holds its namespace"
    (with-redefs [hot/armed? (constantly true)]
      (doseq [p ["../etc/cron.d/x.clj" "/etc/x.clj" "src/../../x.clj" "src/a.sh"]]
        (is (thrown? clojure.lang.ExceptionInfo (hot/load-source! p "(ns a)")) p))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not hold namespace"
                            (hot/load-source! "src/sutra/b.clj" "(ns sutra.a)")))
      (is (thrown? RuntimeException
                   (hot/load-source! "src/a.clj" "(ns #=(java.lang.System/exit 1))"))
          "no read-eval"))))

(deftest an-ssh-field-never-reads-as-an-option
  (let [field #'ds/ssh-field]
    (is (= "bob" (field {:user "bob"} :user)))
    (is (= "host.example.org" (field {:host "host.example.org"} :host)))
    (doseq [[k v] [[:host "-oProxyCommand=sh"] [:user "-F/tmp/x"] [:host "a b"]
                   [:known-hosts "/x/../etc/passwd"] [:identity-file "relative"]]]
      (is (thrown? clojure.lang.ExceptionInfo (field {k v} k)) (str k " " v)))))
