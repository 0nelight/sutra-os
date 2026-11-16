(ns ^:api ontology.object-view-live-ui-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest do-report is use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api])
  (:import [java.io BufferedReader InputStreamReader]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private harness (io/file (System/getProperty "user.dir") "tests/frontend"))
(def ^:private script (io/file harness "src" "sutra_e2e" "object_view_live.js"))
(def ^:private first-paint-script (io/file harness "src" "sutra_e2e" "object_view_first_paint.js"))

(def ^:private own-view-label "Anrufverlauf")

(def ^:private schema
  {:schema
   {:projects {:calls {}}
    :datasets
    {:call_journal
     {:project :calls
      :kind    :standalone
      :columns [{:attribute-name :call_id :attribute-type :string}
                {:attribute-name :status :attribute-type :string}]
      :stream  {:partitions 1 :primary-key {:columns [:call_id]}}}}
    :objects
    {:call {:project    :calls
            :symbol     "📞"
            :plural     "Calls"
            :attributes [[:call-id :string] [:status :string]]
            :backing    {:dataset :call_journal :primary-key :call-id}}}
    :applications
    {:call-view-full
     {:object-type :call
      :form-factor :full
      :project     :calls
      :live        {:object-sets [{:from :call :where [[:call-id := {:this :call-id}]]}]}
      :layout      {:kind  :rows
                    :items [{:widget :object-table
                             :rows   {:source :call}
                             :cols   [{:label own-view-label :prop :status}]}]}}}}})

(defn- publish! [status]
  (is (ok? (api/post! :stream-records {:name "call_journal"}
                      {:records [{:call_id "c1" :status status}]})))
  (is (ok? (api/post! :stream-archive {:name "call_journal"} {}))))

(defn- answer!
  [^BufferedReader rdr prefixes]
  (let [f (future
            (loop []
              (when-let [l (.readLine rdr)]
                (println (str "  [browser] " l))
                (if (some #(str/starts-with? l %) prefixes) l (recur)))))
        v (deref f 90000 ::timeout)]
    (when (= v ::timeout) (future-cancel f))
    v))

(deftest an-object-page-stands-in-its-own-view-before-the-catalog-arrives
  (if-not (.exists (io/file harness "node_modules"))
    (do-report {:type :kaocha/pending :message "run: cd tests/frontend && npm install"})
    (do
      (let [r (api/post! :install schema)] (is (ok? r) (pr-str (:body r))))
      (is (ok? (api/post! :staging)))
      (publish! "ringing")
      (let [id   (-> (api/list-objects :call) :body :rows first :id)
            pb   (doto (ProcessBuilder. ["node" (.getPath first-paint-script) (pod/base-url)
                                         (str "/object/call/" id) own-view-label])
                   (.directory harness)
                   (.redirectErrorStream true))
            _    (.put (.environment pb) "NODE_PATH"
                       (.getPath (io/file harness "node_modules")))
            proc (.start pb)
            out  (BufferedReader. (InputStreamReader. (.getInputStream proc) "UTF-8"))]
        (try
          (let [l (str (answer! out ["BEFORE-CATALOG" "ERROR"]))]
            (is (str/starts-with? l "BEFORE-CATALOG") l))
          (let [l (str (answer! out ["SETTLED" "ERROR"]))]
            (is (str/starts-with? l "SETTLED") l)
            (is (str/includes? l "\"connected\":true") "the catalog did not rebuild the page")
            (is (str/includes? l "\"bodyFetches\":0") "the page fetched no layout of its own"))
          (finally (.destroy proc)))))))

(deftest a-live-object-page-shows-a-change-without-a-reload
  (if-not (.exists (io/file harness "node_modules"))
    (do-report {:type :kaocha/pending :message "run: cd tests/frontend && npm install"})
    (do
      (let [r (api/post! :install schema)] (is (ok? r) (pr-str (:body r))))
      (is (ok? (api/post! :staging)))
      (publish! "ringing")
      (let [id   (-> (api/list-objects :call) :body :rows first :id)
            pb   (doto (ProcessBuilder. ["node" (.getPath script) (pod/base-url)
                                         (str "/object/call/" id)])
                   (.directory harness)
                   (.redirectErrorStream true))
            _    (.put (.environment pb) "NODE_PATH"
                       (.getPath (io/file harness "node_modules")))
            proc (.start pb)
            out  (BufferedReader. (InputStreamReader. (.getInputStream proc) "UTF-8"))]
        (try
          (let [l (str (answer! out ["WATCHING" "ERROR"]))]
            (is (str/starts-with? l "WATCHING") l)
            (is (str/includes?
                 l "\"where\":{\"type\":\"eq\",\"field\":\"call-id\",\"value\":\"c1\"}")
                l)
            (is (str/includes? l "\"objectType\":\"call\"") "the page subscribed to its type"))
          (publish! "answered-live")
          (let [l (str (answer! out ["SAW" "MISSED" "ERROR"]))]
            (is (str/starts-with? l "SAW") l)
            (is (str/includes? l "\"reloads\":0") "the page changed in place"))
          (finally (.destroy proc)))))))

(def ^:private hidden-script (io/file harness "src" "sutra_e2e" "object_view_live_hidden.js"))

(def ^:private schema-with-hidden-set
  (assoc-in
   schema [:schema :applications :call-view-full]
   {:object-type :call
    :form-factor :full
    :project     :calls
    :live        {:object-sets [:calls]}
    :layout      {:kind   :pages
                  :select {:var :seite}
                  :vars   {:seite {:type :keyword :default :eins}
                           :calls {:source :call}}
                  :pages  [{:id :eins :label "Erste"
                            :body {:kind :rows :items [{:widget :badge :text "erste seite"}]}}
                           {:id :zwei :label "Zweite"
                            :body {:kind  :rows
                                   :items [{:widget :object-table
                                            :rows   {:val :calls}
                                            :cols   [{:label "Status" :prop :status}]}]}}]}}))

(deftest a-set-on-a-hidden-page-waits-until-that-page-is-shown
  (if-not (.exists (io/file harness "node_modules"))
    (do-report {:type :kaocha/pending :message "run: cd tests/frontend && npm install"})
    (do
      (let [r (api/post! :install schema-with-hidden-set)] (is (ok? r) (pr-str (:body r))))
      (is (ok? (api/post! :staging)))
      (publish! "ringing")
      (let [id   (-> (api/list-objects :call) :body :rows first :id)
            pb   (doto (ProcessBuilder. ["node" (.getPath hidden-script) (pod/base-url)
                                         (str "/object/call/" id)])
                   (.directory harness)
                   (.redirectErrorStream true))
            _    (.put (.environment pb) "NODE_PATH"
                       (.getPath (io/file harness "node_modules")))
            proc (.start pb)
            out  (BufferedReader. (InputStreamReader. (.getInputStream proc) "UTF-8"))]
        (try
          (let [l (str (answer! out ["WATCHING" "ERROR"]))]
            (is (str/starts-with? l "WATCHING") l))
          (publish! "answered-live")
          (let [l (str (answer! out ["QUIET" "ERROR"]))]
            (is (str/includes? l "\"reads\":0") (str "nothing re-read while hidden: " l)))
          (let [l (str (answer! out ["SAW" "ERROR"]))]
            (is (str/starts-with? l "SAW") l))
          (finally (.destroy proc)))))))
