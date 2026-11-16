(ns sutra.smb
  (:require [clojure.string :as str])
  (:import (com.hierynomus.msdtyp AccessMask)
           (com.hierynomus.mserref NtStatus)
           (com.hierynomus.msfscc FileAttributes)
           (com.hierynomus.msfscc.fileinformation FileAllInformation FileDirectoryInformation)
           (com.hierynomus.mssmb2 SMB2CreateDisposition SMB2ShareAccess SMBApiException)
           (com.hierynomus.smbj SMBClient SmbConfig)
           (com.hierynomus.smbj.auth AuthenticationContext)
           (com.hierynomus.smbj.connection Connection)
           (com.hierynomus.smbj.session Session)
           (com.hierynomus.smbj.share DiskShare File)
           (java.io InputStream OutputStream)
           (java.time Instant)
           (java.util EnumSet)
           (java.util.concurrent Callable ExecutionException ExecutorCompletionService
                                 Executors)))

(set! *warn-on-reflection* true)

(def ^:private default-port 445)

(def ^:private io-buffer-bytes
  (* 8 1024 1024))

(def ^:private listing-threads
  8)

(defn share-path
  [path]
  (let [parts (remove str/blank? (str/split (str path) #"[/\\]+"))]
    (when (some #{".."} parts)
      (throw (ex-info "an SMB path may not contain .." {:path path})))
    (str/join "\\" parts)))

(defn- open-share
  [{:keys [host port share username password domain]}
   {:keys [assert-egress! socket-factory progress!]}]
  (let [port (or port default-port)
        _    (assert-egress! host port)
        ^SMBClient client (SMBClient.
                           (cond-> (SmbConfig/builder)
                             socket-factory (.withSocketFactory socket-factory)
                             true           (.withBufferSize (int io-buffer-bytes))
                             true           (.build)))
        ^Connection conn  (.connect client ^String host (int port))]
    (try
      (let [auth (AuthenticationContext. (str username) (.toCharArray (str password))
                                         (some-> domain str))
            ^Session session (.authenticate conn auth)]
        {:client client :conn conn :session session :progress! (or progress! (constantly nil))
         :share ^DiskShare (.connectShare session (str share))})
      (catch Exception e
        (.close conn)
        (.close client)
        (throw e)))))

(defn- close-share
  [{:keys [^SMBClient client ^Connection conn ^DiskShare share]}]
  (try (.close share) (catch java.io.IOException _ nil))
  (try (.close conn) (catch java.io.IOException _ nil))
  (.close client))

(defn- directory?
  [^FileDirectoryInformation e]
  (not (zero? (bit-and (.getFileAttributes e)
                       (.getValue FileAttributes/FILE_ATTRIBUTE_DIRECTORY)))))

(defn- entries
  [^DiskShare share ^String dir]
  (remove #(#{"." ".."} (.getFileName ^FileDirectoryInformation %))
          (.list share dir FileDirectoryInformation)))

(defn- file-row
  [parent ^FileDirectoryInformation e]
  (let [nm (.getFileName e)]
    {:path      (if (str/blank? parent) nm (str parent "\\" nm))
     :folder    parent
     :name      nm
     :extension (some-> (re-find #"\.([^.]+)$" nm) second str/lower-case)
     :size      (.getEndOfFile e)
     :modified  (str (Instant/ofEpochMilli (.toEpochMillis (.getLastWriteTime e))))}))

(defn- skippable
  [^SMBApiException e]
  (condp contains? (.getStatus e)
    #{NtStatus/STATUS_OBJECT_NAME_NOT_FOUND NtStatus/STATUS_OBJECT_PATH_NOT_FOUND} :gone
    #{NtStatus/STATUS_ACCESS_DENIED} :denied
    nil))

(defn- list-folder
  [share root ^String dir]
  (let [listed (try (entries share dir)
                    (catch SMBApiException e
                      (let [why (skippable e)]
                        (if (and why (not= dir root)) why (throw e)))))]
    (if (keyword? listed)
      {:dirs [] :rows [] :denied (= :denied listed)}
      (let [{dirs true files false} (group-by directory? listed)]
        {:dirs (mapv #(let [nm (.getFileName ^FileDirectoryInformation %)]
                        (if (str/blank? dir) nm (str dir "\\" nm)))
                     dirs)
         :rows (mapv #(file-row dir %) files)}))))

(defn walk-lazy
  [list-fn root on-folder]
  (letfn [(walk [pending]
            (lazy-seq
             (when-let [dir (first pending)]
               (let [{:keys [dirs rows] :as listed} (list-fn dir)]
                 (on-folder dir listed)
                 (concat rows (walk (concat dirs (rest pending))))))))]
    (walk (list root))))

(defn walk-parallel
  [list-fn root on-folder threads]
  (let [pool (Executors/newFixedThreadPool (int threads))
        done (ExecutorCompletionService. pool)
        go!  (fn [dir]
               (.submit done ^Callable (fn [] (assoc (list-fn dir) :dir dir))))]
    (try
      (go! root)
      (loop [running 1 out (transient [])]
        (if (zero? running)
          (persistent! out)
          (let [{:keys [dir dirs rows] :as listed}
                (try (.get (.take done))
                     (catch ExecutionException e (throw (or (.getCause e) e))))]
            (on-folder dir listed)
            (run! go! dirs)
            (recur (+ (dec running) (count dirs)) (reduce conj! out rows)))))
      (finally (.shutdownNow pool)))))

(defn- progress-counter
  [progress! table]
  (let [seen (atom {:table table :folders 0 :files 0 :mb 0.0 :denied []})]
    (fn [dir {:keys [rows denied]}]
      (progress! (swap! seen #(cond-> (-> %
                                          (assoc :current dir)
                                          (update :folders inc)
                                          (update :files + (count rows))
                                          (update :mb + (/ (reduce + 0 (map :size rows))
                                                           1048576.0)))
                                (and denied (< (count (:denied %)) 50))
                                (update :denied conj dir)))))))

(defn- top-folders
  [{:keys [share]}]
  (mapv #(.getFileName ^FileDirectoryInformation %)
        (filter directory? (entries share ""))))

(defn- scan-folder
  [{:keys [share progress!]} table {:keys [limit]}]
  (let [root      (share-path table)
        list-fn   #(list-folder share root %)
        on-folder (progress-counter progress! table)]
    (if limit
      (vec (take limit (walk-lazy list-fn root on-folder)))
      (walk-parallel list-fn root on-folder listing-threads))))

(def ^:private file-columns
  [{:name "path" :type "TEXT" :ontology-type :string}
   {:name "folder" :type "TEXT" :ontology-type :string}
   {:name "name" :type "TEXT" :ontology-type :string}
   {:name "extension" :type "TEXT" :ontology-type :string}
   {:name "size" :type "BIGINT" :ontology-type :long}
   {:name "modified" :type "TEXT" :ontology-type :string}])

(defn- open-file
  ^File [^DiskShare share path]
  (.openFile share ^String (share-path path)
             (EnumSet/of AccessMask/GENERIC_READ) nil SMB2ShareAccess/ALL
             SMB2CreateDisposition/FILE_OPEN nil))

(defn- copy-file
  [{:keys [share]} {:keys [path ^OutputStream to]}]
  (with-open [f (open-file share path)]
    (.read f to)
    {:path path}))

(defn bounded-reader
  ^InputStream [read-at offset length close-fn]
  (let [at   (atom (long (or offset 0)))
        left (atom (some-> length long))
        read-into
        (fn [^bytes buf ^long off ^long len]
          (let [want (if @left (min len (long @left)) len)]
            (if (or (zero? want) (and @left (zero? (long @left))))
              -1
              (let [n (long (read-at buf @at off want))]
                (when (pos? n)
                  (swap! at + n)
                  (when @left (swap! left - n)))
                n))))]
    (proxy [InputStream] []
      (read
        ([] (let [one (byte-array 1)
                  n   (long (read-into one 0 1))]
              (if (pos? n) (bit-and (aget one 0) 0xff) -1)))
        ([^bytes buf] (int (read-into buf 0 (alength buf))))
        ([^bytes buf off len] (int (read-into buf off len))))
      (close [] (close-fn)))))

(defn- read-file
  [{:keys [share]} {:keys [path offset length]}]
  (let [f (open-file share path)]
    (bounded-reader (fn [buf at off len] (.read f ^bytes buf (long at) (int off) (int len)))
                    offset length #(.close f))))

(defn- file-stat
  [{:keys [^DiskShare share]} {:keys [path]}]
  (try
    (let [^FileAllInformation i (.getFileInformation share ^String (share-path path))]
      {:size     (.getEndOfFile (.getStandardInformation i))
       :modified (str (Instant/ofEpochMilli
                       (.toEpochMillis (.getLastWriteTime (.getBasicInformation i)))))})
    (catch SMBApiException e
      (when-not (= :gone (skippable e)) (throw e)))))

(def connector
  {:profile {:capabilities #{:catalog :scan :invoke}
             :route #{:direct}
             :paths? true
             :files? true
             :endpoint :host
             :needs [:host :share :egress :egress-allowed]
             :scan {:preds {} :project? false :page? true :compound? false}}
   :config  [:map [:host :string] [:share :string]
             [:port {:optional true} :int]
             [:domain {:optional true} :string]]
   :form    {:label "SMB"
             :fields [{:key :host :label "Hostname" :kind :string :required true}
                      {:key :port :label "Port" :kind :number}
                      {:key :share :label "Share" :kind :string :required true}
                      {:key :domain :label "Domain" :kind :string}]}
   :open    open-share
   :close   close-share
   :probe   top-folders
   :tables  top-folders
   :columns (fn [_ _] file-columns)
   :scan    scan-folder
   :invoke  (fn [h op params _opts]
              (case op
                :copy (copy-file h params)
                :read (read-file h params)
                :stat (file-stat h params)
                (throw (ex-info (str "the SMB connector has no op " op) {:op op}))))})

(def module
  {:name        :smb-connector
   :contributes {:foreign/connectors {:smb connector}}})
