(ns ^:unit platform.in-process-api-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.serve.server :as server]
            [sutra.wire.api :as api]
            [sutra.wire.codec :as codec]
            [sutra.wire.transport :as tx]))

(deftest a-request-reaches-the-app-as-sent-and-its-answer-comes-back
  (let [seen (atom nil)]
    (with-redefs [server/app (fn [req]
                               (reset! seen (assoc req :body-read
                                                   (some-> (:body req) .readAllBytes codec/decode)))
                               {:status  201
                                :headers {"X-Sutra-Branch" "fde-x"}
                                :body    (java.io.ByteArrayInputStream.
                                          (codec/encode {:commit "abc" :lines ["ok"]}))})]
      (let [r (binding [tx/*exchange*   server/exchange
                        tx/*auth-token* "agent-tok"
                        tx/*branch*     "fde-x"]
                (api/post! :repo-commit {:rid "ri.repo.1"} {:files {"a.py" "x = 1"}}))]
        (testing "the request"
          (is (= :post (:request-method @seen)))
          (is (= "/api/repositories/ri.repo.1/commits" (:uri @seen)))
          (is (= "branch=fde-x" (:query-string @seen)))
          (is (= "Bearer agent-tok" (get-in @seen [:headers "authorization"])))
          (is (= {:files {"a.py" "x = 1"}} (:body-read @seen))))
        (testing "the answer"
          (is (= 201 (:status r)))
          (is (= {:commit "abc" :lines ["ok"]} (:body r)))
          (is (= "fde-x" (:branch r))))))))
