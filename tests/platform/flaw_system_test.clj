(ns ^:unit platform.flaw-system-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.serve.server :as server]
            [sutra.repos.objects :as g]
            [sutra.wire.protocol :as protocol])
  (:import (java.io File)))

(def ^:private rid "ri.r.flaw")
(def ^:private ^:dynamic *repos* nil)

(defn- rm-rf [^File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(use-fixtures
  :each
  (fn [t]
    (let [d (doto (File/createTempFile "sutra-flaw-git" "") (.delete) (.mkdirs))]
      (binding [*repos* d]
        (try
          (t)
          (finally
            (g/forget-repo! *repos* rid)
            (rm-rf d)))))))

(defn- commit-tree!
  [entries parent]
  (g/with-inserter [ins *repos* rid]
    (let [t (g/write-tree! ins
                           (into {} (map (fn [[k v]] [k (.getBytes ^String v "UTF-8")])) entries))]
      (g/commit! ins {:tree t :parents (if parent [parent] [])
                          :author {:name "tester" :email "t@sutra" :when-ms 1700000000000}
                          :message "m"}))))

(deftest stale-cache
  (testing "flaw stale-cache: change the source, then read — the cache key carries the
            source version (the commit sha), so the read follows the change"
    (let [c1 (commit-tree! {"a.txt" "one"} nil)
          t1 (g/read-tree *repos* rid c1)
          c2 (commit-tree! {"a.txt" "two" "b.txt" "new"} c1)
          t2 (g/read-tree *repos* rid c2)]
      (is (= #{"a.txt"} (set (keys t1))))
      (is (= #{"a.txt" "b.txt"} (set (keys t2))) "the new source is read, not the cached one")
      (is (not= (get-in t1 ["a.txt" :blob]) (get-in t2 ["a.txt" :blob])))
      (is (= t1 (g/read-tree *repos* rid c1)) "the old version still reads as itself"))))

(defn- age-loose-objects!
  []
  (let [objs (File. (File. *repos* (str rid ".git")) "objects")
        past (- (System/currentTimeMillis) (* 30 86400000))]
    (doseq [^File f (file-seq objs) :when (.isFile f)]
      (.setLastModified f past))))

(deftest pruned-anchor-tip
  (testing "flaw pruned-anchor-tip: a tip only the anchor (control_repo_anchor) reaches
            survives git gc — anchors are roots"
    (let [c1 (commit-tree! {"a.txt" "one"} nil)
          _  (g/update-ref! *repos* rid "refs/heads/main" c1 {})
          c2 (commit-tree! {"a.txt" "two"} c1)]
      (g/pin-anchor! *repos* rid "main" c2)
      (age-loose-objects!)
      (g/gc! *repos* rid)
      (is (some? (g/commit-info *repos* rid c1)) "control: what the ref reaches stays")
      (is (some? (g/commit-info *repos* rid c2))
          "the anchored tip stays, so reconcile-tip! does not hit :projection-lost"))))

(defn- probe [version]
  (server/app {:request-method :post
               :uri            "/api/flaw-probe/mutate"
               :scheme         :http
               :server-name    "localhost"
               :server-port    8030
               :remote-addr    "127.0.0.1"
               :headers        {"x-sutra-protocol" (str version)
                                "content-type"     "application/transit+json"}}))

(deftest effect-despite-mismatch
  (testing "flaw effect-despite-mismatch: a mutation sent with a foreign protocol version
            is refused by the server before anything is applied (before routing)"
    (let [res (probe (inc protocol/version))]
      (is (contains? #{400 409 412 426} (:status res))
          (str "a foreign client version is refused, not routed — got " (:status res)))
      (is (str/includes? (str/lower-case (pr-str (:body res))) "protocol")
          "the refusal names the protocol"))))
