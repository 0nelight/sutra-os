(ns ^:api ontology.mro-ingest-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [sutra.parser.sugar :as sugar]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.sources.egress :as egress]
            [sutra.compute.sandbox :as sandbox]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(def ^:private user "mechanic@aviation.test")
(def ^:private pass "anything")

(def ^:dynamic *imap* nil)

(defn- with-egress+mail [f]
  (let [kf      (doto (java.io.File/createTempFile "mro-egress" ".pkcs8") (.delete))
        keyfile (str kf)
        imap    (try (when (sandbox/available?) (pod/start-imap!))
                     (catch Exception _ nil))]
    (try
      (egress/stop!)
      (egress/start! {:keyfile keyfile :port (pod/egress-port)})
      (when imap
        (pod/send-mail! {:host (:host imap) :smtp-port (:smtp-port imap)
                         :to user :subject "Defect D-AINA — hydraulic leak (AOG)"
                         :body "Work order attached."
                         :attachments [{:filename "WO-1001.pdf" :mime "application/pdf"
                                        :bytes (.getBytes "%PDF-1.4 WO-1001\n%%EOF\n")}]})
        (pod/send-mail! {:host (:host imap) :smtp-port (:smtp-port imap)
                         :to user :subject "Defect D-AIBL — APU fault"
                         :body "See attached."
                         :attachments [{:filename "WO-1002.pdf" :mime "application/pdf"
                                        :bytes (.getBytes "%PDF-1.4 WO-1002\n%%EOF\n")}]}))
      (binding [*imap* imap] (f))
      (finally
        (when imap (pod/stop-imap! imap))
        (try (egress/stop!) (catch Exception _ nil))))))

(use-fixtures :once pod/with-mem-pod with-egress+mail)

(defn- py [f] (slurp (io/resource (str "support/fixtures/python/" f))))

(defn- schema []
  {:schema
   {:objects
    {:email {:symbol "✉️"
             :attributes [[:pk :string] [:message-id :string] [:subject :string]
                          [:from-addr :string] [:body-text :text]]
             :plural "Emails"}
     :work-order-pdf {:symbol "📎" :title :filename
                      :attributes [[:pk :string] [:email-pk :string] [:filename :string]
                                   [:mime :string] [:quell-pdf :media-reference]]
                      :plural "Work order pdfs"}
     :blob {:symbol "🗄️" :title :hash
            :primary-key :pk
            :attributes [[:pk :string] [:hash :string] [:mime :string] [:size :integer]]
            :plural "Blobs"}}
    :links [[:wo-pdf-aus-email :work-order-pdf :email :many-to-one
             {:fk :email-pk :forward-name "aus E-Mail" :reverse-name "Anhänge"}]]
    :transforms
    {:email-raw
     {:language :python :code (py "fetch_defect_emails.py")
      :external-systems [:defect-imap] :incremental true :primary-key :pk
      :output-columns [[:pk :string] [:message-id :string] [:subject :string]
                       [:from-addr :string] [:body-text :text]
                       [:anhaenge [:array [:struct {:filename :string :mime :string
                                                    :sha256 :string :size :integer}]]]]}
     :email-rows
     {:language :python :code (py "email_from_raw.py")
      :inputs {:raw {:kind :transform :transform :email-raw}} :primary-key :pk
      :column-map {:pk :pk :message-id :message_id :subject :subject
                   :from-addr :from_addr :body-text :body_text}
      :output {:kind :object-type :object-type :email}}
     :wo-pdf-rows
     {:language :python :code (py "wo_pdf_from_raw.py")
      :inputs {:raw {:kind :transform :transform :email-raw}} :primary-key :pk
      :column-map {:pk :pk :email-pk :email_pk :filename :filename
                   :mime :mime :quell-pdf :quell_pdf}
      :output {:kind :object-type :object-type :work-order-pdf}}}}})

(defn- objs [type] (:rows (:body (api/list-objects type))))
(defn- val-of [o k] (get-in o [:values k]))

(defn- create-source! []
  (let [pubkey (:public-key (:body (api/get! :egress-pubkey)))
        cipher (pod/seal-credential pubkey {:password pass})]
    (api/mutate :create-source
                {:name :defect-imap :project :mro :type :imap
                 :config {:host (:host *imap*) :port (str (:imap-port *imap*)) :user user}
                 :egress [(str (:host *imap*) ":" (:imap-port *imap*))]
                 :access :read-only :external-transform true :secret-cipher cipher}
                {:stage? true})))

(defn- blob-bytes
  ([hash] (blob-bytes hash nil))
  ([hash ctx]
   (let [q (when-let [{:keys [type id]} ctx]
             (str "?type=" (name type) "&id=" id))
         {:keys [status ^bytes bytes]} (tx/get-bytes! (str "/api/blob/" hash q))]
     {:status status :len (count bytes)})))

(deftest imap-defect-ingest
  (cond
    (not (sandbox/available?)) (println "  (mro-ingest SKIPPED — bwrap unavailable)")
    (nil? *imap*)          (println "  (mro-ingest SKIPPED — greenmail unavailable)")
    :else
    (do
      (testing "declare the sealed :imap external-transform source (committed first)"
        (is (ok? (api/mutate :create-project {:name :mro} {:stage? true}))
            "the project it sits in")
        (is (ok? (create-source!)) "the source stages")
        (is (ok? (api/post! :staging)) "commit source")
        (is (seq (:sources (:body (api/get! :sources)))) "source registered on schema"))

      (testing "install the intermediate→2-backing transform+link DAG over HTTP"
        (is (ok? (api/post! :install
                            (update (schema) :schema #(sugar/lower-schema (in-project :mro %)))))
            "install stages the schema")
        (is (ok? (api/post! :staging)) "commit schema")
        (is (= 4 (count (:transforms (:body (api/get! :schema))))) "four transforms installed"))

      (testing "run-import pulls IMAP through the egress agent + lands the objects"
        (is (ok? (api/post! :import-run)) "import ran")
        (is (= 2 (count (objs :email))) "two :email objects (one per message)")
        (is (= 2 (count (objs :work-order-pdf))) "two :work-order-pdf (one attachment each)")
        (is (= 2 (count (objs :blob))) "two :blob catalog objects minted from the media channel")
        (is (some #(re-find #"D-AINA" (str (val-of % :subject))) (objs :email))
            "the AINA defect email landed"))

      (testing "the work-order PDF is content-addressed and links back to its email"
        (let [wo   (first (objs :work-order-pdf))
              hash (val-of wo :quell-pdf)
              blob (first (filter #(= hash (val-of % :hash)) (objs :blob)))]
          (is (= 64 (count hash)) ":quell-pdf carries a sha256 content-address")
          (is (some? blob) ":quell-pdf resolves to a :blob catalog object")
          (is (= "application/pdf" (val-of blob :mime)) "blob mime preserved")
          (is (pos? (val-of blob :size)) "blob size recorded"))
        (is (= 2 (count (:body (api/list-links {:type :wo-pdf-aus-email}))))
            "each work-order links back to its source email — resolved in ONE pass")
        (is (= 1 (count (:body (api/list-links {:type :wo-pdf-aus-email :limit 1}))))
            "?limit bounds the read in SQL, not the caller afterwards")
        (is (= 1 (count (:body (api/list-links {:limit 1}))))
            "and bounds the whole-ontology walk too")
        (is (ok? (api/post! :import-run)) "second import is a stable no-op")
        (is (= 2 (count (:body (api/list-links {:type :wo-pdf-aus-email}))))
            "links stable on the second pass"))

      (testing "GET /api/blob/:hash streams the original PDF bytes through its object"
        (let [wo   (first (objs :work-order-pdf))
              hash (val-of wo :quell-pdf)
              {:keys [status len]} (blob-bytes hash {:type :work-order-pdf :id (:id wo)})]
          (is (= 200 status) "the object door serves the address that object holds")
          (is (pos? len) "PDF bytes stream back")
          (is (= 404 (:status (blob-bytes hash)))
              "without an object or a media set the door names no addresses"))))))
