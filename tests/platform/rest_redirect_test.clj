(ns ^:unit platform.rest-redirect-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.sources.http-connector :as fhttp]))

(def ^:private spec {:egress ["api.example:443"]})

(defn- replying [& resps]
  (let [left (atom resps) seen (atom [])]
    [(fn [req] (swap! seen conj req) (let [r (first @left)] (swap! left rest) r)) seen]))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:refusal/code (ex-data e)))))

(deftest a-redirect-off-the-allowlist-is-refused
  (let [[send seen] (replying {:status 302 :headers {:location "https://evil.example/x"}})]
    (is (= :refusal.foreign/rest-egress-not-allowed
           (refusal #((fhttp/following-redirects send spec)
                      {:method :get :url "https://api.example/a"}))))
    (is (= 1 (count @seen)) "the off-list target is never asked")))

(deftest a-redirect-on-the-allowlist-is-followed
  (let [[send seen] (replying {:status 303 :headers {:location "/b?page=2"}}
                              {:status 200 :body "ok"})]
    (is (= 200 (:status ((fhttp/following-redirects send spec)
                         {:method :post :url "https://api.example/a" :body "x"
                          :query-params {"q" 1}}))))
    (is (= {:method :get :url "https://api.example/b?page=2"} (last @seen)))))

(deftest redirects-end
  (let [loop-resp {:status 307 :headers {:location "https://api.example/a"}}
        [send _]  (apply replying (repeat 10 loop-resp))]
    (is (= :refusal.foreign/rest-too-many-redirects
           (refusal #((fhttp/following-redirects send spec)
                      {:method :get :url "https://api.example/a"}))))))
