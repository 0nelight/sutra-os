(ns ^:unit platform.git-token-scope-test
  (:require [clojure.string :as str]
            [sutra.access.http.host :as host]
            [clojure.test :refer [deftest is testing]]
            [sutra.branching.http.git :as git-http]
            [sutra.repos.api :as repos])
  (:import (java.util Base64)))

(defn- ask [readable? scope]
  (let [h (git-http/make-handler {:host host/capabilities :principal-context identity
                                  :with-principal (fn [_ f] (f {} {} nil))
                                  :session-for (fn [_] {:scope scope})})]
    (binding [git-http/*enabled-override* true]
      (with-redefs [git-http/resolve-repo-rid (fn [_ _] "ri.r.1")
                    repos/repo-readable?   (fn [_ _ _] readable?)]
        (h {:uri "/git/r/ri.r.1.git/info/refs" :query-string "service=git-upload-pack"
            :request-method :get
            :headers {"authorization"
                      (str "Basic "
                           (.encodeToString (Base64/getEncoder)
                                            (.getBytes "sutra:tok" "UTF-8")))}})))))

(deftest a-token-for-another-repository-is-an-auth-failure
  (testing "401 + WWW-Authenticate, so git drops the cached credential instead of reporting a wrong path"
    (let [r (ask true "ri.r.2")]
      (is (= 401 (:status r)))
      (is (get-in r [:headers "WWW-Authenticate"]))
      (is (str/includes? (:body r) "neues Clone-Token"))))
  (testing "a repository the caller may not read stays a 404 — no existence leak"
    (is (= 404 (:status (ask false "ri.r.2"))))))
