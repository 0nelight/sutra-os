(ns ^:unit grammar.refusal-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.i18n.catalog :as catalog]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.http.refusals :as refusals]
            [sutra.sources.policy :as policy]))

(defn- caught [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest a-refusal-keeps-its-english-text-and-its-data
  (let [e (caught #(policy/write-plan nil {:access :read-write :writeback {}} {:table "t"}))]
    (is (= "table \"t\" not on :writeback allowlist" (ex-message e)))
    (is (= {:table "t" :allowed [] :refusal/code :refusal.foreign/table-not-writable}
           (anomaly/data e)))))

(deftest params-render-the-text-and-travel-with-the-data
  (let [e (caught #(policy/pk-parts [:a :b] "x"))]
    (is (= "composite key \"x\" has 1 part(s), [:a :b] needs 2" (ex-message e)))
    (is (= {:parts 1 :needs 2} (:refusal/params (ex-data e))))
    (testing "the data suffices to say it in another language"
      (let [d (ex-data e)]
        (is (= "zusammengesetzter Schlüssel \"x\" hat 1 Teil(e), [:a :b] braucht 2"
               (catalog/text :de (:refusal/code d) (merge d (:refusal/params d)))))))))

(deftest the-catalog-names-the-category
  (is (= :incorrect
         (anomaly/category (ex-data (refusal/refusal :refusal.foreign/parameter-required
                                                     {:parameter :p})))))
  (let [d (ex-data (refusal/refusal :refusal.foreign/pk-immutable {} {:pk "id" :type "T"}))]
    (is (= 422 (anomaly/status d)))
    (is (anomaly/client-error? d)))
  (testing "a code the catalog does not list is a fault, not a refusal"
    (let [e (refusal/refusal :refusal.foreign/sql-empty {:sql ""})]
      (is (= :fault (anomaly/category (ex-data e))))
      (is (nil? (anomaly/of e)))))
  (testing "a category the caller names wins over the catalog"
    (is (= 409
           (anomaly/status
            (ex-data (refusal/refusal :refusal.foreign/pk-immutable
                                      {::anomaly/category :conflict})))))))

(deftest every-category-names-a-code-of-the-catalog
  (doseq [code (keys refusal/categories)]
    (is (not (re-find #"^⟦" (refusal/message code {}))) (str code))))

(deftest a-placeholder-prints-by-str-or-by-pr-str
  (is (= "egress to \"h:5432\" not on allowlist"
         (refusal/message :refusal.foreign/egress-not-allowed {:target "h:5432"})))
  (is (= "unknown agent triage"
         (refusal/message :refusal.foreign/unknown-agent {:agent "triage"}))))

(deftest an-answer-speaks-the-language-the-request-asks-for
  (let [r   (anomaly/of (refusal/refusal :refusal.sheet/not-a-cell {:cell "ZZ"}))
        say (fn [lang] (get-in (refusals/response {:headers {"accept-language" lang}} r)
                               [:body :error]))]
    (is (= "keine Zelladresse: ZZ" (say "de-AT,de;q=0.9,en;q=0.8")))
    (is (= "not a cell address: ZZ" (say "en-US,en")))
    (is (= "not a cell address: ZZ" (say "fr-FR")))
    (is (= 400 (:status (refusals/response {} r))))))
