(ns ^:api platform.media-set-sync-api-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.blobs.api :as blobs]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.system :as sys]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.access.datasets :as dsets]
            [sutra.media.http.blobs :as door]
            [sutra.media.sets :as msets]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.wire.api :as api])
  (:import (java.io ByteArrayInputStream)))

(def ^:private share (atom {}))
(def ^:private reads (atom {}))

(defn- row [[path {:keys [bytes modified]}]]
  {:path path :size (count bytes) :modified modified})

(def ^:private connector
  {:profile {:capabilities #{:catalog :scan :invoke} :route #{:direct} :paths? true :files? true
             :needs [:host]
             :scan {:preds {} :project? false :page? true :compound? false}}
   :config  [:map]
   :form    {:label "Files" :fields []}
   :open    (fn [spec _] spec)
   :tables  (fn [_] ["root"])
   :columns (fn [_ _] [{:name "path" :type "TEXT" :ontology-type :string}])
   :scan    (fn [_ table _]
              (let [prefix (if (= "/" table) "" (str table "\\"))]
                (mapv row (filter #(.startsWith ^String (key %) prefix) @share))))
   :invoke  (fn [_ op {:keys [path offset length]} _]
              (let [file (get @share (.replace ^String path "/" "\\"))]
                (case op
                  :stat (when file {:size (count (:bytes file)) :modified (:modified file)})
                  :read (let [bs (.getBytes ^String (:bytes file))
                              at (or offset 0)]
                          (swap! reads update (first (.split ^String path "/")) (fnil inc 0))
                          (ByteArrayInputStream. bs at (or length (- (alength bs) at)))))))})

(defn- with-file-connector [f]
  (swap! @#'sys/installed assoc-in [:tables :foreign/connectors :files] connector)
  (f))

(use-fixtures :each pod/with-mem-pod with-file-connector
              (fn [f]
                (reset! reads {})
                (reset! share
                        (into {}
                              (map (fn [[path bytes]]
                                     [path {:bytes bytes :modified "2024-01-01T00:00:00Z"}]))
                              {"docs\\a\\plan.pdf"  "%PDF-plan"
                               "docs\\a\\skip.tmp"  "tmp"
                               "kopie\\a\\plan.pdf" "%PDF-plan"
                               "kopie\\a\\skip.tmp" "tmp"
                               "other\\b.pdf"       "%PDF-b"}))
                (f)))

(defn- install! [schema]
  (let [r (api/post! :install {:schema schema})] (is (ok? r) (pr-str (:body r))))
  (is (ok? (api/post! :staging)) "commits"))

(defn- store [] (ctx/at (ctx/store) "main"))
(defn- schema [] (seat/rebuild-schema-from-storage (store)))
(defn- item-at [nm path]
  (some #(when (= path (:path %)) %) (msets/items (store) nm)))
(defn- read-body [{:keys [body]}]
  (with-open [in body] (slurp in)))

(deftest a-virtual-set-names-files-and-reads-them-when-asked
  (install! {:projects {:ablage {:description "Dateien."}}
             :sources {:freigabe {:project :ablage :type :files :host "h"}}
             :media-sets {:plaene {:project :ablage :source :freigabe :subfolder "docs"
                                   :virtual true
                                   :sync {:filters {:path-matches "\\.pdf$"}}}}})
  (is (ok? (api/post! :import-run)) "the sync runs in a build")
  (let [{:keys [address size mime physical-path source]} (item-at :plaene "a/plan.pdf")]
    (testing "the filter held the other file back, and the subfolder the other folder"
      (is (= ["a/plan.pdf"] (mapv :path (msets/items (store) :plaene)))))
    (testing "the item names the file and registering read nothing"
      (is (= "a/plan.pdf" physical-path))
      (is (= "freigabe" source))
      (is (= 9 size))
      (is (= "application/pdf" mime))
      (is (nil? (get @reads "docs")))
      (is (not (blobs/exists? (ctx/blobs) address)) "no bytes in the store"))
    (testing "the door reads it from the source, as a range too"
      (let [whole (door/stream-blob (store) address {})]
        (is (= 200 (:status whole)))
        (is (= "private, no-cache" (get-in whole [:headers "Cache-Control"])))
        (is (= "%PDF-plan" (read-body whole))))
      (let [part (door/stream-blob (store) address {"range" "bytes=5-8"})]
        (is (= 206 (:status part)))
        (is (= "bytes 5-8/9" (get-in part [:headers "Content-Range"])))
        (is (= "plan" (read-body part)))))
    (testing "any reader of the blob store goes through to the source"
      (with-open [in (blobs/open (ctx/blobs) address)]
        (is (= "%PDF-plan" (slurp in)))))
    (testing "a branch sees the items main holds, and only for that set"
      (let [branch (assoc (store) :branch "ein-zweig")]
        (is (= ["a/plan.pdf"] (mapv :path (msets/items branch :plaene))))
        (is (msets/holds? branch :plaene address))
        (is (not (msets/holds? branch :andere address)))))
    (testing "a file removed at the source stays an item and can no longer be read"
      (swap! share dissoc "docs\\a\\plan.pdf")
      (is (= 404 (anomaly/status (door/stream-blob (store) address {}))))
      (is (ok? (api/post! :import-run)))
      (is (some? (item-at :plaene "a/plan.pdf"))))))

(deftest a-copying-set-lands-the-bytes-and-takes-only-what-is-new
  (install! {:projects {:archiv {:description "Kopien."}}
             :sources {:kopierquelle {:project :archiv :type :files :host "h"}}
             :media-sets {:kopien {:project :archiv :source :kopierquelle :subfolder "kopie"
                                   :sync {:filters {:path-not-matches "\\.tmp$"
                                                    :exclude-synced [:modified]}}}}})
  (is (ok? (api/post! :import-run)))
  (let [{:keys [address]} (item-at :kopien "a/plan.pdf")]
    (is (= 1 (get @reads "kopie")))
    (is (blobs/exists? (ctx/blobs) address))
    (is (= (blobs/address-of (.getBytes "%PDF-plan")) address))
    (testing "nothing changed, nothing read"
      (is (ok? (api/post! :import-run)))
      (is (= 1 (get @reads "kopie"))))
    (testing "a changed file comes again under the same path"
      (swap! share assoc "kopie\\a\\plan.pdf"
             {:bytes "%PDF-neu" :modified "2025-01-01T00:00:00Z"})
      (is (ok? (api/post! :import-run)))
      (is (= 2 (get @reads "kopie")))
      (is (= (blobs/address-of (.getBytes "%PDF-neu"))
             (:address (item-at :kopien "a/plan.pdf")))))))

(def ^:private reading-code
  "from sutra_transforms import transform, MediaSetInput, MediaSetOutput, Output

@transform(inputs={'items': MediaSetInput(media_set='lesen')},
           output=Output(kind='dataset', dataset='lese_zeilen'),
           media_outputs={'verweise': MediaSetOutput('verweise'),
                          'kopien': MediaSetOutput('code-kopien')})
def run(items, verweise, kopien):
    verweise.register_media_item('a/plan.pdf', 'aus-code/plan.pdf')
    kopien.put_media_item(b'%PDF-aus-code', 'neu.pdf')
    return [{'path': r['path'], 'inhalt': items.bytes(r['media_ref']).decode()}
            for r in items]
")

(deftest a-transform-reads-a-set-and-fills-others
  (install! {:projects {:code {:description "Code."}}
             :sources {:codequelle {:project :code :type :files :host "h"}}
             :datasets {:lese_zeilen {:project :code :columns [{:attribute-name :path :attribute-type :string}
                                                           {:attribute-name :inhalt
                                                            :attribute-type :string}]}}
             :media-sets {:lesen {:project :code :source :codequelle :subfolder "docs"
                                  :virtual true
                                  :sync {:filters {:path-matches "plan"}}}
                          :verweise {:project :code :source :codequelle :subfolder "docs"
                                     :virtual true}
                          :code-kopien {:project :code}}
             :transforms {:lese-zeilen {:project :code :language :python :code reading-code
                                        :inputs {:items {:kind :media-set-items :media-set :lesen}}
                                        :output {:kind :dataset :dataset :lese_zeilen}
                                        :media-outputs {:verweise {:media-set :verweise}
                                                        :kopien {:media-set :code-kopien}}}}})
  (let [r (api/post! :import-run)]
    (is (ok? r) (pr-str (:body r))))
  (testing "the rows came from the set, and the bytes from the source"
    (let [rows (mapv :values (dsets/rows (store) (lookup/rid-of (schema) :datasets :lese_zeilen)))]
      (is (= [{:path "a/plan.pdf" :inhalt "%PDF-plan"}]
             (mapv #(select-keys % [:path :inhalt]) rows)))))
  (testing "register_media_item names a file and put_media_item lands bytes"
    (let [item (item-at :verweise "aus-code/plan.pdf")]
      (is (= "a/plan.pdf" (:physical-path item)))
      (is (= "%PDF-plan" (read-body (door/stream-blob (store) (:address item) {})))))
    (let [item (item-at :code-kopien "neu.pdf")]
      (is (= (blobs/address-of (.getBytes "%PDF-aus-code")) (:address item)))
      (is (blobs/exists? (ctx/blobs) (:address item))))))

(deftest a-dataset-sync-filters-the-files-it-lists
  (install! {:projects {:meta {:description "Metadaten."}}
             :sources {:metaquelle {:project :meta :type :files :host "h"}}
             :datasets {:dateien {:project :meta
                                  :columns [{:attribute-name :path :attribute-type :string}]
                                  :external-source {:datasource :metaquelle :table "docs"
                                                    :primary-key :path
                                                    :filters {:path-matches "\\.pdf$"}}}}})
  (let [r (api/post! :import-run)]
    (is (ok? r) (pr-str (:body r))))
  (is (= ["docs\\a\\plan.pdf"]
         (mapv (comp :path :values)
               (dsets/rows (store) (lookup/rid-of (schema) :datasets :dateien))))))

(deftest a-virtual-set-needs-a-source
  (let [r (api/post! :install {:schema {:projects {:leere {:description "x"}}
                                        :media-sets {:leer {:project :leere :virtual true}}}})]
    (is (not (ok? r)))
    (is (re-find #"names no :source" (str (get-in r [:body :error]))))))
