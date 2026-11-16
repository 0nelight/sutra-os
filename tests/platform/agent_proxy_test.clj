(ns ^:unit platform.agent-proxy-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [org.httpkit.server :as http-kit]
            [sutra.sources.proxy.agent :as agent]
            [sutra.sources.proxy.client :as client]
            [sutra.sources.proxy.hub :as hub]
            [sutra.sources.proxy.seal :as seal])
  (:import (java.io File IOException)
           (java.net InetAddress InetSocketAddress ServerSocket Socket)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private allowed (atom #{}))

(def ^:private platform-allowlist (atom nil))

(def ^:private link-refused (atom nil))

(defn- authorize
  [{:keys [source host port]}]
  (if (contains? @allowed [source host port])
    {:agents ["nas"]}
    {:refused "not on the source"}))

(defn- params-of [req]
  (into {} (for [kv (.split (str (:query-string req)) "&")
                 :let [[k v] (.split ^String kv "=")]]
             [(keyword k) v])))

(defn- router [req]
  (let [req (assoc req :params (params-of req))]
    (case (:uri req)
      "/agent"  (hub/agent-handler {:token-ok? (fn [a t] (and (= a "nas") (= t "agent-t")))
                                    :allowlist-of (fn [_] (or @link-refused
                                                      {:allow-listed-cidrs @platform-allowlist}))}
                                   req)
      "/stream" (hub/client-handler {:token-ok? #(= % "worker-t") :authorize authorize} req))))

(def ^:private port (atom nil))

(use-fixtures :once
  (fn [f]
    (let [server (http-kit/run-server router {:port 0 :legacy-return-value? false})]
      (reset! port (http-kit/server-port server))
      (try (f) (finally (http-kit/server-stop! server))))))

(defn- echo-once! [^Socket s]
  (with-open [s s]
    (let [in (.getInputStream s) out (.getOutputStream s) b (byte-array 8192)]
      (loop []
        (let [n (.read in b)]
          (when-not (neg? n)
            (.write out b 0 n)
            (.flush out)
            (recur)))))))

(defn- echo-server
  ^ServerSocket []
  (let [ss (ServerSocket. 0 50 (InetAddress/getLoopbackAddress))]
    (future
      (try
        (loop []
          (let [s (.accept ss)]
            (future (echo-once! s))
            (recur)))
        (catch Exception _ nil)))
    ss))

(defn- start-agent! [config-file]
  (agent/start! {:url (str "ws://127.0.0.1:" @port "/agent") :token "agent-t" :agent "nas"
                 :config-file config-file}))

(defn- linked! []
  (loop [n 200]
    (when (and (pos? n) (not (contains? (hub/linked-agents) "nas")))
      (Thread/sleep 25)
      (recur (dec n))))
  (is (contains? (hub/linked-agents) "nas")))

(defn- connect ^Socket [source host p]
  (let [f (client/socket-factory {:url (str "ws://127.0.0.1:" @port "/stream")
                                  :token "worker-t" :source source})]
    (doto (.createSocket f)
      (.connect (InetSocketAddress. ^String host (int p)) 5000))))

(def ^:private no-file "/nonexistent/agentProxyConfig.edn")

(deftest bytes-cross-the-tunnel-with-credit-holding-the-sender-back
  (with-open [ss (echo-server)]
    (let [p    (.getLocalPort ss)
          stop (start-agent! no-file)
          size (* 8 1024 1024)
          data (byte-array (map unchecked-byte (take size (cycle (range 251)))))]
      (reset! allowed #{["nas" "127.0.0.1" p]})
      (linked!)
      (try
        (with-open [s (connect "nas" "127.0.0.1" p)]
          (.setSoTimeout s 20000)
          (let [out (.getOutputStream s)
                in  (.getInputStream s)
                w   (future (.write out data))
                got (.readNBytes in size)]
            @w
            (is (java.util.Arrays/equals ^bytes data ^bytes got))))
        (finally (stop))))))

(deftest a-name-stays-linked-until-its-last-link-ends
  (let [a (-> {} (hub/link "nas" :new) (hub/link "nas" :old))]
    (testing "the generation that dialled last carries new streams"
      (is (= :old (hub/carrier a "nas"))))
    (testing "retiring it leaves the other one linked and carrying"
      (is (= :new (hub/carrier (hub/unlink a "nas" :old) "nas"))))
    (testing "the end of an older link changes nothing for the newer one"
      (is (= :old (hub/carrier (hub/unlink a "nas" :new) "nas"))))
    (testing "the last link's end unlinks the name"
      (is (= {} (-> a (hub/unlink "nas" :old) (hub/unlink "nas" :new)))))))

(deftest an-agent-without-an-answered-allowlist-is-not-linked
  (reset! link-refused {:unavailable "no api"})
  (let [before (hub/linked-agents)
        stop   (start-agent! no-file)]
    (try
      (Thread/sleep 300)
      (is (= before (hub/linked-agents)))
      (finally (stop) (reset! link-refused nil)))))

(deftest a-new-allowlist-reaches-a-linked-agent-at-once
  (with-open [ss (echo-server)]
    (let [p    (.getLocalPort ss)
          stop (start-agent! no-file)]
      (reset! allowed #{["nas" "127.0.0.1" p]})
      (linked!)
      (try
        (with-open [_ (connect "nas" "127.0.0.1" p)]
          (is (pos? (hub/push-config! "nas" [{:cidr-block "10.0.0.0/8" :port 1}]))))
        (Thread/sleep 200)
        (is (thrown? IOException (connect "nas" "127.0.0.1" p)))
        (finally (hub/push-config! "nas" nil) (stop))))))

(deftest a-target-the-source-does-not-name-is-refused-before-the-agent
  (with-open [ss (echo-server)]
    (let [p    (.getLocalPort ss)
          stop (start-agent! no-file)]
      (reset! allowed #{})
      (linked!)
      (try
        (is (thrown? IOException (connect "nas" "127.0.0.1" p)))
        (finally (stop))))))

(deftest the-agents-allowlist-refuses-what-the-source-allows
  (with-open [ss (echo-server)]
    (let [p    (.getLocalPort ss)
          _    (reset! platform-allowlist [{:cidr-block "192.0.2.71/32" :port 445}])
          stop (start-agent! no-file)]
      (reset! allowed #{["nas" "127.0.0.1" p]})
      (linked!)
      (Thread/sleep 200)
      (try
        (is (thrown-with-msg? IOException #"agent-allowlist" (connect "nas" "127.0.0.1" p)))
        (finally (reset! platform-allowlist nil) (stop))))))

(deftest the-platform-key-rests-under-aes-gcm-of-the-master-secret
  (let [plain (.getBytes "der private key" "UTF-8")
        sealed (seal/wrap "master-a" plain)]
    (is (java.util.Arrays/equals ^bytes plain ^bytes (seal/unwrap "master-a" sealed)))
    (is (not= sealed (seal/wrap "master-a" plain)) "a fresh iv for every seal")
    (is (thrown? Exception (seal/unwrap "master-b" sealed)))))

(deftest cidr-and-port-judging
  (let [a       (InetAddress/getByName "192.0.2.71")
        entries [{:cidr-block "192.0.2.0/24" :port 400 :end-port 500}]]
    (testing "inside block and range" (is (agent/allowed? entries a 445)))
    (testing "outside the range" (is (not (agent/allowed? entries a 501))))
    (testing "outside the block"
      (is (not (agent/allowed? entries (InetAddress/getByName "192.168.1.71") 445))))
    (testing "no allowlist judges nothing" (is (agent/allowed? nil a 1)))))

(deftest a-writable-local-allowlist-stops-the-proxy
  (let [dir (Files/createTempDirectory "agentproxy" (into-array FileAttribute []))
        f   (File. (.toFile dir) "agentProxyConfig.edn")]
    (spit f "{:allow-listed-cidrs []}")
    (is (:refused (agent/local-config (str f))))
    (is (nil? (start-agent! (str f))))))
