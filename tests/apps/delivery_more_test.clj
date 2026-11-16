(ns ^:unit apps.delivery-more-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.apps.http.delivery :as delivery]
            [sutra.objects.reads :as reads]
            [sutra.objects.views :as views]))

(deftest a-next-page-is-asked-from-its-offset
  (is (= 200 (:offset (delivery/page-query {} {:limit 200 :offset 200}))))
  (is (not (contains? (delivery/page-query {} {:limit 200 :offset 0}) :offset)))
  (is (not (contains? (delivery/page-query {} {:limit 200}) :offset))))

(deftest more-names-each-set-with-its-offset
  (let [req (fn [s] {:query-params {"more" s}})]
    (is (= {:auftraege 200} (delivery/request-more (req "{\"auftraege\":200}"))))
    (is (nil? (delivery/request-more (req "{\"auftraege\":-1}"))))
    (is (nil? (delivery/request-more {:query-params {}})))))

(deftest a-short-last-page-counts-what-came-before-it
  (with-redefs [views/run-object-query (fn [_ _ q]
                                         (if (:count-only? q)
                                           {:count 450}
                                           {:rows (vec (repeat (if (= 400 (:offset q)) 50 200)
                                                               {:id 1}))}))]
    (is (= 450 (:total (reads/page-matching nil nil :t {:limit 200 :offset 400} {})))
        "offset 400 plus the 50 rows of the last page")
    (is (= 450 (:total (reads/page-matching nil nil :t {:limit 200 :offset 200} {})))
        "a full page asks the store for the set's size")))
