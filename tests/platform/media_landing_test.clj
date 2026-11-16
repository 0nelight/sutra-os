(ns ^:api platform.media-landing-test
  (:require [sutra.catalog.grants :as grants]
            [support.wire :refer [ok?]]
            [sutra.store.access.scope :as ctx]
            [sutra.blobs.api :as blobs]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth pod/with-scratch-blobs)

(defn- col [n t] {:attribute-name n :attribute-type t})
(defn- b64 [^String s] (.encodeToString (java.util.Base64/getEncoder) (.getBytes s "UTF-8")))
(defn- sha256 [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md (.getBytes s "UTF-8"))))))

(deftest land-media-and-sweep
  (let [admin-tok (-> (api/post! :login {:username grants/admin-username
                                  :password pr/default-admin-password})
                      :body :token)
        payload   "%PDF-1.4 fake invoice"
        expected  (sha256 payload)]
    (is (some? admin-tok) "admin login")
    (tx/with-token admin-tok
      (testing "a media-set dataset exists"
        (is (ok? (api/mutate :create-project {:name :belegablage} {:stage? true}))
            "the project it sits in")
        (is (ok? (api/mutate :create-dataset
                             {:name    :belege
                              :project :belegablage
                              :columns [(col :media_ref :media-reference)
                                        (col :dateiname :string)]}
                             {:stage? true}))
            "dataset stages")
        (is (ok? (api/post! :staging)) "dataset commits"))

      (testing "LANDING: one call puts the bytes in the CAS and the row in the dataset"
        (let [r (api/post! :media-land {:dataset      "belege"
                                 :media-column "media_ref"
                                 :bytes-b64    (b64 payload)
                                 :filename     "rechnung.pdf"
                                 :values       {:dateiname "rechnung.pdf"}})]
          (is (= 200 (:status r)) "land succeeds")
          (is (= expected (get-in r [:body :hash]))
              "the returned address is the sha256 of the bytes — the client can derive it")
          (is (= 1 (get-in r [:body :written])) "exactly one row written")
          (is (= [] (get-in r [:body :triggered-by]))
              "no schedule watches belege — the answer says so, nothing will build")
          (is (blobs/exists? (ctx/blobs) expected) "the bytes are in the CAS")))

      (testing "the landed row REFERENCES the blob (that reference is what keeps it alive)"
        (let [ds   (->> (:datasets (:body (api/get! :datasets)))
                        (some (fn [[rid d]]
                                (when (= "belege" (some-> (:dataset-name d) name)) rid))))
              rows (:rows (:body (api/get! :dataset-preview {:id ds})))]
          (is (= 1 (count rows)) "one row in the media set")
          (is (= expected (get-in (first rows) [:values :media_ref]))
              "the media-reference column carries the content-address")))

      (testing "TRIGGERED-BY names the armed schedules watching the dataset"
        (is (ok? (api/post! :schedules {:name "belege-folgen" :target "belege" :scope :self
                                       :trigger {:kind :parent-updated :of :belege}}))
            "schedule upserts")
        (is (ok? (api/post! :schedules {:name "belege-pausiert" :target "belege" :paused true
                                       :trigger {:kind :parent-updated :of :belege}})))
        (let [r (api/post! :media-land {:dataset "belege" :media-column "media_ref"
                                 :bytes-b64 (b64 "zweite rechnung") :filename "r2.pdf"
                                 :values {:dateiname "r2.pdf"}})]
          (is (= ["belege-folgen"] (get-in r [:body :triggered-by]))
              "a paused schedule builds nothing and is not named"))
        (doseq [s (:schedules (:body (api/get! :schedules)))
                :when (#{"belege-folgen" "belege-pausiert"} (:name s))]
          (api/delete! :schedule-of {:id (:id s)})))

      (testing "a landing that names no dataset is refused, not half-done"
        (is (= 400 (:status (api/post! :media-land {:bytes-b64 (b64 "x")})))
            "missing :dataset/:media-column → 400"))

      (testing "SWEEP (dry run): the referenced blob is referenced, and nothing is deleted"
        (let [r (api/post! :media-gc {})]
          (is (= 200 (:status r)) "gc reads")
          (is (false? (get-in r [:body :applied?])) "a bare call never deletes")
          (is (pos? (get-in r [:body :referenced])) "the landed hash counts as referenced")
          (is (zero? (get-in r [:body :deleted :count])) "nothing deleted on a dry run")))

      (testing "an ORPHAN blob (bytes uploaded, no row) is held through its grace window"
        (let [orphan (get-in (api/put-blob (.getBytes "waise" "UTF-8")) [:body :hash])
              r      (:body (api/post! :media-gc {}))]
          (is (blobs/exists? (ctx/blobs) orphan) "the orphan is in the CAS")
          (is (pos? (get-in r [:within-grace :count]))
              "freshly landed bytes are NOT swept — an upload's row lands after its bytes")
          (is (zero? (:count (:unreferenced r)))
              "and so it is not even counted as reclaimable yet")

          (testing "with the grace window closed, --apply reclaims the orphan and only the orphan"
            (let [a (:body (api/post! :media-gc {:apply? true :grace-days 0}))]
              (is (true? (:applied? a)) "the sweep ran")
              (is (nil? (:blocked a)) "nothing blocked it — every branch scanned cleanly")
              (is (= 1 (get-in a [:deleted :count])) "exactly one blob reclaimed")
              (is (not (blobs/exists? (ctx/blobs) orphan)) "the orphan's bytes are gone")
              (is (blobs/exists? (ctx/blobs) expected)
                  "the REFERENCED blob survives — a media-reference is what keeps bytes alive"))))))))
