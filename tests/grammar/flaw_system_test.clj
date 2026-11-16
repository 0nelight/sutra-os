(ns ^:unit grammar.flaw-system-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [platform-sdk.client :as client]
            [platform-sdk.schema :as schema]
            [sutra.blobs.api :as blobs]
            [sutra.blobs.mem :as blobs-mem]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.repos.code-blobs :as cb]
            [sutra.wire.transport :as tx])
  (:import (java.io File)
           (java.lang.reflect InvocationHandler Proxy)
           (java.sql Connection PreparedStatement)))

(use-fixtures :each (fn [f]
                      (sqltrace/disarm!) (sqltrace/clear!)
                      (try (f) (finally (sqltrace/disarm!) (sqltrace/arm! nil)
                                        (sqltrace/disarm!) (sqltrace/clear!)))))

(defn- fake-ds
  [execute! cancelled]
  (let [stmt (fn []
               (Proxy/newProxyInstance
                (clojure.lang.RT/baseLoader) (into-array Class [PreparedStatement])
                (reify InvocationHandler
                  (invoke [_ _ m _]
                    (case (.getName m)
                      "execute"        (execute!)
                      "cancel"         (do (deliver cancelled true) nil)
                      "getUpdateCount" (int 0)
                      nil)))))
        conn (fn []
               (Proxy/newProxyInstance
                (clojure.lang.RT/baseLoader) (into-array Class [Connection])
                (reify InvocationHandler
                  (invoke [_ _ m _]
                    (case (.getName m)
                      ("prepareStatement" "createStatement" "prepareCall") (stmt)
                      nil)))))]
    (sqltrace/wrap-datasource
     (reify javax.sql.DataSource
       (getConnection [_] (conn))
       (getConnection [_ _ _] (conn)))
     "flaw")))

(defn- exec! [ds]
  (let [^Connection c (.getConnection ^javax.sql.DataSource ds)]
    (.execute ^PreparedStatement (.prepareStatement c "SELECT 1"))))

(deftest phantom-inflight
  (testing "flaw phantom-inflight: begin! enters :inflight, write-line! throws, end! never
            comes — :inflight then shows a statement that does not run"
    (let [dir (doto (File/createTempFile "sutra-flaw-trace" "") (.delete) (.mkdirs))
          ds  (fake-ds (constantly true) (promise))]
      (try
        (sqltrace/arm! {:file (.getAbsolutePath dir)})
        (try (exec! ds) (catch Throwable _ nil))
        (is (empty? (:inflight (sqltrace/since 0)))
            "after the statement is over, nothing of it is in :inflight")
        (finally (.delete dir))))))

(deftest unkillable-statement
  (testing "flaw unkillable-statement: clear! during a running statement — kill! must
            still reach it"
    (let [cancelled (promise)
          ds        (fake-ds #(do (deref cancelled 5000 nil) true) cancelled)]
      (sqltrace/arm!)
      (let [running (future (exec! ds))]
        (try
          (loop [n 0]
            (when (and (empty? (:inflight (sqltrace/since 0))) (< n 200))
              (Thread/sleep 10) (recur (inc n))))
          (let [sq (:seq (first (:inflight (sqltrace/since 0))))]
            (is (some? sq) "the statement is in :inflight while it runs")
            (sqltrace/clear!)
            (is (= sq (:killed (sqltrace/kill! sq))) "kill! still finds it after clear!")
            (is (true? (deref cancelled 500 false)) "and cancels it"))
          (finally (deliver cancelled :cleanup) (deref running 5000 nil)))))))

(defn- schema-of [label]
  {:object-types {(str "ri.ot." label) {:object-type-name label :plural "Objekte"}}})

(defn- fake-server
  [schema-for on-schema]
  (let [n (atom 0)]
    (fn send
      ([target method path] (send target method path nil))
      ([{:keys [token]} _method path body]
       (cond
         (re-find #"/api/login" path)  {:status 200 :body {:token (str "t-" (:username body))}}
         (re-find #"/api/logout" path) {:status 200 :body {:ok true}}
         (re-find #"/api/schema" path) (let [i (swap! n inc)]
                                         (on-schema token i)
                                         {:status 200 :body (schema-for token i)})
         :else {:status 404 :body {:error "no"}})))))

(deftest foreign-cache-sequential
  (testing "flaw foreign-cache: SDK as A, log out, log in as B — B sees only its schema"
    (with-redefs [tx/send-to! (fake-server (fn [tok _] (schema-of tok)) (fn [_ _]))]
      (let [c (client/client {:base-url "http://flaw.invalid" :username "a" :password "x"})]
        (is (= (schema-of "t-a") (client/schema c)))
        (client/logout! c)
        (client/login! c "b" "x")
        (is (= (schema-of "t-b") (client/schema c)))
        (is (= {:types {:t-b "ri.ot.t-b"}} (client/rids c)) "the index is B's too")))))

(deftest foreign-cache-in-flight
  (testing "flaw foreign-cache: A's schema request still in flight while B logs in on the
            same client — its answer must not land in B's cache"
    (let [entered (promise) release (promise)]
      (with-redefs [tx/send-to! (fake-server (fn [tok _] (schema-of tok))
                                             (fn [tok _] (when (= "t-a" tok)
                                                           (deliver entered true)
                                                           (deref release 5000 nil))))]
        (let [c  (client/client {:base-url "http://flaw.invalid" :username "a" :password "x"})
              t1 (future (client/schema c))]
          (deref entered 5000 nil)
          (client/login! c "b" "x")
          (deliver release true)
          @t1
          (is (= (schema-of "t-b") (client/schema c))
              "after B's login the client serves B's schema, not A's late answer"))))))

(deftest index-follows-a-schema-refresh
  (testing "flaw index-schema-skew (sequential): schema refreshed — index and schema agree"
    (with-redefs [tx/send-to! (fake-server (fn [_ i] (schema-of (str "v" i))) (fn [_ _]))]
      (let [c (client/client {:base-url "http://flaw.invalid" :token "t"})]
        (is (= {:types {:v1 "ri.ot.v1"}} (client/rids c)))
        (client/schema c true)
        (is (= (schema/rid-index (client/schema c)) (client/rids c)))))))

(deftest index-schema-skew
  (testing "flaw index-schema-skew: the index is computed from schema s1 while a refresh
            installs s2 — the cached index must not be s1's next to s2"
    (let [entered (promise) release (promise)
          orig    schema/rid-index]
      (with-redefs [tx/send-to!
                      (fake-server (fn [_ i] (schema-of (str "v" i))) (fn [_ _]))
                    schema/rid-index (fn [s]
                                           (when (= s (schema-of "v1"))
                                             (deliver entered true)
                                             (deref release 5000 nil))
                                           (orig s))]
        (let [c (client/client {:base-url "http://flaw.invalid" :token "t"})]
          (client/schema c)
          (let [t1 (future (client/rids c))]
            (deref entered 5000 nil)
            (client/schema c true)
            (deliver release true)
            @t1
            (is (= (orig (client/schema c)) (client/rids c))
                "index and schema in the SDK stem from the same version")))))))

(deftest sticky-miss
  (testing "flaw sticky-miss: the first lower finds the bytes missing, they arrive, the
            second lower succeeds (no remembered throw)"
    (let [text  (apply str "# flaw sticky-miss " (random-uuid) "\n"
                       (repeat 40 "print('a long enough body')\n"))
          bytes (.getBytes ^String text "UTF-8")
          sha   (:hash (blobs/put! (blobs-mem/blobs) bytes))
          b     (blobs-mem/blobs)
          world {:transforms {:t {:name "t" :code {:sutra/blob sha}}}}]
      (is (thrown? Exception (cb/lower b world)) "missing bytes: lower refuses")
      (blobs/put! b bytes)
      (is (= text (get-in (try (cb/lower b world) (catch Exception _ nil))
                          [:transforms :t :code]))
          "the bytes are there now: lower reads them"))))

(defn- cljs-branch
  [form]
  (cond
    (reader-conditional? form)
    (let [m (apply hash-map (:form form))]
      (when (contains? m :cljs) (cljs-branch (:cljs m))))
    (map? form)  (into {} (map (fn [[k v]] [(cljs-branch k) (cljs-branch v)])) form)
    (coll? form) (map cljs-branch form)
    :else form))

(defn- cljs-forms []
  (binding [*ns* (the-ns 'sutra.wire.transport)]
    (with-open [r (java.io.PushbackReader. (io/reader (io/resource "sutra/wire/transport.cljc")))]
      (mapv cljs-branch
            (doall (take-while #(not= ::eof %)
                               (repeatedly #(read {:read-cond :preserve :eof ::eof} r))))))))

(deftest unchecked-mismatch
  (testing "flaw unchecked-mismatch: the browser transport (cljs send!) must check the
            server's protocol header instead of reading a foreign-version answer"
    (let [syms (set (filter symbol? (tree-seq coll? seq (cljs-forms))))]
      (is (contains? syms 'js/fetch) "control: the browser's send! was read")
      (is (contains? syms 'protocol/mismatch)
          "the cljs side of sutra.wire.transport calls protocol/mismatch on the answer"))))
