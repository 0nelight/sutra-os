(ns sutra.builds.run.media-sync
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.blobs.api :as blobs]
            [sutra.catalog.grants :as grants]
            [sutra.foundation.scan-progress :as scan-progress]
            [sutra.media.api :as msets]
            [sutra.sources.api :as sources]
            [sutra.store.access.scope :as ctx]
            [sutra.types.file-sync :as file-sync]
            [sutra.types.media :as mt]
            [sutra.types.transform-io :as tio]
            [sutra.catalog.transforms :as transforms]))

(set! *warn-on-reflection* true)

(defn nodes
  [schema in-scope?]
  (for [t-kw (apply concat (transforms/topo-levels schema))
        :let [ms (some :media-set (tio/outputs-of (get-in schema [:transforms t-kw])))]
        :when (and ms (in-scope? t-kw))]
    {:kind :media-sync :key t-kw :t-kw t-kw :spec {:media-set ms}}))

(defn- listed-files
  [source subfolder]
  (mapv (fn [{:keys [path modified size]}]
          {:path (file-sync/relative-path subfolder path) :modified modified :size size})
        (sources/scan source (or (not-empty subfolder) "/"))))

(defn- of-schema
  [ms nm {:keys [ignore-unmatched-schema]} files]
  (let [typed (map #(assoc % :mime (file-sync/mime-of (:path %))) files)
        {ok true other false} (group-by #(nil? (mt/format-problem ms (:mime %))) typed)]
    (when (and (seq other) (not ignore-unmatched-schema))
      (refusal/refuse! :refusal.builds/media-set-unmatched-files
                       {:media-set nm :files (count other)}
                       {:set     (name nm)
                        :formats (str/join ", " (map name (:formats ms)))
                        :path    (:path (first other))}))
    (vec ok)))

(defn- on-source-path
  [subfolder path]
  (let [sub (file-sync/normal-path subfolder)]
    (if (seq sub) (str sub "/" path) path)))

(defn- item-of
  [nm source-name {:keys [path size mime modified]}]
  {:media-set nm :path path :address (msets/virtual-address nm path) :size size :mime mime
   :filename (last (str/split path #"/")) :source source-name :physical-path path
   :source-modified modified})

(defn- copied-item
  [source blob-store nm subfolder {:keys [path mime modified]}]
  (let [{:keys [hash size]}
        (with-open [^java.io.Closeable in
                    (sources/invoke! source :read {:path (on-source-path subfolder path)})]
          (blobs/put! blob-store in))]
    {:media-set nm :path path :address hash :size size :mime mime
     :filename (last (str/split path #"/")) :source-modified modified}))

(defn run-sync!
  [store schema set-ref]
  (let [nm (msets/set-name schema set-ref)
        [_ {:keys [source subfolder virtual sync] :as ms}] (msets/declared schema nm)
        filters (:filters sync)
        spec    (get-in schema [:sources source])
        src-nm  (or (:source-name spec) (some-> source name))]
    (when-not (and spec (sources/file-source? spec))
      (refusal/refuse! :refusal.builds/media-set-source-not-files
                       {:media-set nm :source source}
                       {:set (name nm) :source src-nm}))
    (sources/with-source [s (ctx/connect (grants/source-authority store)) spec]
      (let [listed (listed-files s subfolder)
            {:keys [files] :as chosen}
            (file-sync/select filters (of-schema ms nm filters listed)
                              (when (:exclude-synced filters)
                                (msets/synced-files store nm)))
            blob-store (ctx/blobs)
            total  (count files)
            done   (reduce
                    (fn [{:keys [taken bytes]} file]
                      (let [item (if virtual
                                   (item-of nm src-nm file)
                                   (copied-item s blob-store nm subfolder file))
                            n    (inc taken)
                            b    (+ bytes (or (:size item) 0))]
                        (msets/record! store item)
                        (scan-progress/note! (str src-nm) {:table (name nm) :files n :total total
                                                           :mb (/ b 1048576.0)
                                                           :current (:path file)})
                        {:taken n :bytes b}))
                    {:taken 0 :bytes 0}
                    files)]
        (merge (select-keys chosen [:listed :excluded :held-by]) done)))))

(defn media-sync-job
  [{:keys [ctx schema]} _ {{:keys [media-set]} :spec}]
  (let [{:keys [taken listed excluded held-by bytes]} (run-sync! (:store ctx) schema media-set)]
    (println (format "  → media set %s: %d Dateien genommen (%.1f MB), %d gelistet, %d gefiltert%s"
                     (name (msets/set-name schema media-set)) taken (/ bytes 1048576.0)
                     listed excluded (if held-by (str ", zurueckgehalten von " (name held-by)) "")))
    {:facts [] :rows-out taken}))
