(ns ^:unit platform.flaw-clone-token-overreach-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.branching.http.git :as git-http]
            [sutra.repos.api :as repos]
            [sutra.access.http.host :as host]
            [sutra.types.rid :as rid])
  (:import (java.util Base64)))

(defn- ask
  [scope]
  (let [h (git-http/make-handler {:host              host/capabilities
                                  :principal-context identity
                                  :with-principal    (fn [_ f] (f {} {} nil))
                                  :session-for       (fn [_] {:username "u" :scope scope})})]
    (binding [git-http/*enabled-override* true]
      (with-redefs-fn {#'git-http/resolve-repo-rid (fn [_ _] "ri.r.other")
                       #'repos/repo-readable?   (fn [_ _ _] true)
                       #'git-http/dispatch         (fn [& _] {:status 200 :dispatched true})}
        #(h {:uri "/git/r/ri.r.other.git/info/refs" :query-string "service=git-upload-pack"
             :request-method :get
             :headers {"authorization"
                       (str "Basic " (.encodeToString (Base64/getEncoder)
                                                      (.getBytes "sutra:tok" "UTF-8")))}})))))

(deftest an-ontology-clone-token-reaches-only-the-ontology-repository
  (testing "the scope predicate: an ontology-scoped token does not reach another repository"
    (is (not (git-http/token-reaches? host/capabilities {:scope (str rid/ontology-repo-rid)}
                                      "ri.r.other"))))
  (testing "the git surface refuses it (no dispatch into the other repository)"
    (let [r (ask (str rid/ontology-repo-rid))]
      (is (not (:dispatched r)))
      (is (contains? #{401 403} (:status r)))))
  (testing "control: the ontology token still reaches the ontology repository"
    (is (git-http/token-reaches? host/capabilities {:scope (str rid/ontology-repo-rid)}
                                 rid/ontology-repo-rid))))
