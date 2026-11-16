(ns ^:unit platform.egress-protocol-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [sutra.foundation.outbound :as out]
            [sutra.foundation.rpc :as rpc]
            [sutra.sources.datasources :as ds]
            [sutra.sources.egress]
            [sutra.sources.egress-protocol :as ep]
            [sutra.sources.sidecar :as sidecar]))

(deftest the-agent-serves-exactly-the-described-routes
  (is (= (set (keys ep/routes))
         (set (keys @(resolve 'sutra.sources.egress/routes))))))

(deftest the-published-document-is-current
  (is (= (ep/render) (slurp ep/version-file))
      "stale — run clojure -M:test -e \"(require 'sutra.sources.egress-protocol)
       (sutra.sources.egress-protocol/-main)\""))

(deftest the-sidecar-document-is-current
  (is (= (ep/render (ep/sidecar-document)) (slurp ep/sidecar-file))))

(deftest the-sidecar-client-sends-what-the-schema-accepts
  (let [bodies (atom [])]
    (with-redefs [out/post! (fn [url {:keys [body]}]
                              (swap! bodies conj [(re-find #"/[a-z]+$" url)
                                                  (json/read-str body :key-fn keyword)])
                              {:status 200 :body (json/write-str {:result []})})]
      (sidecar/op! "http://s" "t" 1000 {:type :mailbox :host "h"}
                   {:op :scan :table "INBOX" :scan {:limit 5}} nil)
      (sidecar/op! "http://s" "t" 1000 {:type :mailbox} {:op :write} {:set {"A" 1}})
      (sidecar/describe! "http://s" "t" 1000))
    (is (= 3 (count @bodies)))
    (doseq [[route body] @bodies
            :let [body (cond-> body (:op body) (update :op keyword))]]
      (is (ep/valid-sidecar-request? route body) (pr-str route body)))))

(deftest the-document-is-openapi
  (let [doc (json/read-str (slurp ep/version-file))]
    (is (= "3.1.0" (get doc "openapi")))
    (is (= (count ep/routes) (count (get doc "paths"))))
    (is (every? #(get-in % ["post" "requestBody"]) (vals (get doc "paths"))))))

(def ^:private spec
  {:source-name "erp" :type :mssql :agent "default" :secret-cipher "AAAA BBBB"})

(defn- sent
  [stub f]
  (let [calls (atom [])]
    (with-redefs [rpc/call! (fn [{:keys [url]} payload]
                              (let [route (re-find #"/[a-z\-]+$" url)]
                                (swap! calls conj [route payload])
                                (stub route)))]
      (f))
    @calls))

(def ^:private stub
  {"/write-begin" {:tx "t1"}})

(deftest the-client-sends-what-the-schema-accepts
  (let [calls (sent #(get stub % {})
                    (fn []
                      (ds/reach! spec)
                      (ds/read-rows spec "select 1" [1])
                      (ds/agent-pubkey)
                      (ds/agent-connect "default" ["db:1433"] 1000)
                      (ds/agent-status "default" 1000)
                      (ds/trace-arm! true {:file nil})
                      (ds/trace-since "default:3")
                      (ds/trace-kill! {:all true})
                      (ds/write! spec {:table "t" :rows []})
                      (let [s (ds/write-session)]
                        (ds/write! spec {:table "t" :rows []} s)
                        (ds/end-write-session! s true))
                      (ds/invoke-webhook! spec :notify {:a 1}
                                          {:timeout-ms 5000 :idempotency-key "k1"
                                           :event-type "com.sutra.actions.webhook"})
                      (ds/rest-call! spec {:op :tables})
                      (ds/listen! spec {:window-ms 1000 :folders ["INBOX"]})))]
    (is (<= 14 (count calls)))
    (testing "every route the client calls is described"
      (is (= #{} (set (remove (set (keys ep/routes)) (map first calls))))))
    (doseq [[route payload] calls]
      (testing route
        (is (ep/valid-request? route payload) (pr-str (ep/explain-request route payload)))))))
