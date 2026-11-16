(ns sutra.repos.objects
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.cache :as cache]
            [clojure.string :as str]
            [sutra.types.branch-kinds :refer [projectable-branch?]])
  (:import (java.io ByteArrayOutputStream File InputStream OutputStream)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.locks ReentrantLock)
           (org.eclipse.jgit.api Git)
           (org.eclipse.jgit.dircache DirCache DirCacheEntry)
           (org.eclipse.jgit.lib CommitBuilder Constants FileMode ObjectId
                                 ObjectInserter ObjectInserter$Formatter PersonIdent RefUpdate
                                 RefUpdate$Result Repository)
           (org.eclipse.jgit.internal.storage.file FileRepository)
           (org.eclipse.jgit.revwalk RevWalk RevCommit)
           (org.eclipse.jgit.revwalk.filter RevFilter)
           (org.eclipse.jgit.transport ReceiveCommand ReceivePack RefFilter UploadPack)
           (org.eclipse.jgit.treewalk TreeWalk)
           (org.eclipse.jgit.util FS$FileStoreAttributes)))

(set! *warn-on-reflection* true)

(defn- repo-file
  ^File [^File repos repo-rid]
  (File. repos (str repo-rid ".git")))

(defn- repo-key [repos repo-rid]
  (.getAbsolutePath (repo-file repos repo-rid)))

(defonce ^:private repo-cache
  (ConcurrentHashMap.))

(defn open-repo
  ^Repository [repos repo-rid]
  (FS$FileStoreAttributes/setBackground true)
  (or (.get ^ConcurrentHashMap repo-cache (repo-key repos repo-rid))
      (let [^File dir (repo-file repos repo-rid)
            r         (FileRepository. dir)]
        (when-not (.exists (File. dir "HEAD"))
          (.mkdirs dir)
          (.create r true)
          (doto (.updateRef r Constants/HEAD true)
            (.disableRefLog)
            (.link (str Constants/R_HEADS "main"))))
        (or (.putIfAbsent ^ConcurrentHashMap repo-cache (.getAbsolutePath dir) r) r))))

(declare forget-trees!)

(defn forget-repo!
  [repos repo-rid]
  (some-> ^Repository (.remove ^ConcurrentHashMap repo-cache (repo-key repos repo-rid)) (.close))
  (forget-trees!)
  nil)

(defn forget-all-repos!
  []
  (doseq [^Repository r (vec (.values ^ConcurrentHashMap repo-cache))]
    (.close r))
  (.clear ^ConcurrentHashMap repo-cache)
  (forget-trees!)
  nil)

(defn repo-exists?
  [repos repo-rid]
  (.exists (File. ^File (repo-file repos repo-rid) "HEAD")))

(defonce ^:private locks (ConcurrentHashMap.))

(defn repo-lock
  ^ReentrantLock [repos repo-rid]
  (let [k (repo-key repos repo-rid)]
    (or (.get ^ConcurrentHashMap locks k)
        (let [l (ReentrantLock.)]
          (or (.putIfAbsent ^ConcurrentHashMap locks k l) l)))))

(defmacro with-repo-lock
  [repos repo-rid & body]
  `(let [^ReentrantLock l# (repo-lock ~repos ~repo-rid)]
     (.lock l#)
     (try ~@body (finally (.unlock l#)))))

(defn gc!
  [repos repo-rid]
  (when (repo-exists? repos repo-rid)
    (with-repo-lock repos repo-rid
      (let [^Repository r (open-repo repos repo-rid)
            props (.. (Git/wrap r) gc (setAggressive false) call)]
        (forget-repo! repos repo-rid)
        (into {} (map (fn [k] [(str k) (str (.get ^java.util.Properties props k))]))
              (.keySet ^java.util.Properties props))))))

(defn- pct
  [^Character c]
  (->> (.getBytes (String/valueOf c) StandardCharsets/UTF_8)
       (map #(format "%%%02X" (bit-and (long %) 0xff)))
       (apply str)))

(def ^:private ref-safe?
  (into #{} (concat (map char (range (int \a) (inc (int \z))))
                    (map char (range (int \A) (inc (int \Z))))
                    (map char (range (int \0) (inc (int \9))))
                    [\- \_ \+ \, \= \( \) \! \& \; \< \> \| \# \$ \' \" \@])))

(defn- escape-component
  [^String comp']
  (let [n     (count comp')
        lock? (str/ends-with? (str/lower-case comp') ".lock")]
    (apply str
           (for [i (range n)
                 :let [c (.charAt comp' i)]]
             (cond
               (= c \%) "%25"
               (= c \.) (if (or (zero? i)
                                (= i (dec n))
                                (= \. (.charAt comp' (dec i)))
                                (and (< (inc i) n) (= \. (.charAt comp' (inc i))))
                                (and lock? (= i (- n 5))))
                          "%2E" ".")
               (= c \@) (if (and (< (inc i) n) (= \{ (.charAt comp' (inc i)))) "%40" "@")
               (ref-safe? c) (str c)
               :else (pct c))))))

(defn branch->ref
  [branch]
  (when (projectable-branch? branch)
    (let [ref (str "refs/heads/"
                   (->> (str/split (str branch) #"/" -1)
                        (map escape-component)
                        (str/join "/")))]
      (when-not (Repository/isValidRefName ref)
        (refusal/refuse! :refusal.repos/bad-ref {:kind ::bad-ref :branch branch :ref ref}))
      ref)))

(defn- unescape [^String s]
  (let [out (ByteArrayOutputStream.)]
    (loop [i 0]
      (when (< i (count s))
        (let [c (.charAt s i)]
          (if (and (= c \%) (< (+ i 2) (count s)))
            (do (.write out (Integer/parseInt (subs s (inc i) (+ i 3)) 16))
                (recur (+ i 3)))
            (do (.write out (.getBytes (String/valueOf c) StandardCharsets/UTF_8))
                (recur (inc i)))))))
    (String. (.toByteArray out) StandardCharsets/UTF_8)))

(defn ref->branch
  [ref]
  (when (and ref (str/starts-with? (str ref) "refs/heads/"))
    (unescape (subs (str ref) (count "refs/heads/")))))

(defn- insert-blob ^ObjectId [^ObjectInserter ins ^bytes content]
  (.insert ins Constants/OBJ_BLOB content))

(def gitlink-mode
  (.toString FileMode/GITLINK))

(defn gitlink?
  [x]
  (boolean (or (and (map? x) (:gitlink x))
               (= gitlink-mode (some-> x :mode str str/trim))
               (= gitlink-mode (some-> x str str/trim)))))

(def regular-mode
  (.toString FileMode/REGULAR_FILE))

(defn- file-mode ^FileMode [mode]
  (if (str/blank? (some-> mode str))
    FileMode/REGULAR_FILE
    (FileMode/fromBits (Integer/parseInt (str/trim (str mode)) 8))))

(defn entry
  [^bytes bytes mode]
  (if (or (nil? mode) (= regular-mode (str/trim (str mode))))
    bytes
    {:bytes bytes :mode (str/trim (str mode))}))

(defn entry-bytes
  ^bytes [v]
  (if (map? v) (:bytes v) v))

(defn write-tree!
  ^ObjectId [^ObjectInserter ins entries]
  (let [dc (DirCache/newInCore)
        b  (.builder dc)]
    (doseq [[path v] (sort-by key entries)]
      (let [e (DirCacheEntry. ^String path)]
        (if-let [target (and (map? v) (:gitlink v))]
          (doto e
            (.setFileMode FileMode/GITLINK)
            (.setObjectId (ObjectId/fromString ^String target)))
          (doto e
            (.setFileMode (file-mode (when (map? v) (:mode v))))
            (.setObjectId (insert-blob ins (entry-bytes v)))))
        (.add b e)))
    (.finish b)
    (.writeTree dc ins)))

(defn commit!
  ^String [^ObjectInserter ins {:keys [tree parents author message]}]
  (let [{:keys [name email when-ms]} author
        ^String who-name  (or name "sutra")
        ^String who-email (or email "sutra@localhost")
        who (PersonIdent. who-name who-email
                          (long (or when-ms (System/currentTimeMillis)))
                          (int 0))
        cb  (doto (CommitBuilder.)
              (.setTreeId ^ObjectId tree)
              (.setAuthor who)
              (.setCommitter who)
              (.setMessage (str message)))]
    (.setParentIds cb ^java.util.List
                   (mapv #(ObjectId/fromString ^String %) (vec parents)))
    (let [id (.insert ins cb)]
      (.flush ins)
      (.name ^ObjectId id))))

(defmacro with-inserter
  [[sym repos repo-rid] & body]
  `(let [^Repository r# (open-repo ~repos ~repo-rid)]
     (with-open [~(vary-meta sym assoc :tag 'org.eclipse.jgit.lib.ObjectInserter)
                 (.newObjectInserter r#)]
       ~@body)))

(defn update-ref!
  [repos repo-rid ref sha {:keys [expected force?]}]
  (let [^Repository r (open-repo repos repo-rid)
        ^RefUpdate u  (.updateRef r ^String ref)]
    (.setNewObjectId u (ObjectId/fromString ^String sha))
    (when-not force?
      (.setExpectedOldObjectId u (if expected
                                   (ObjectId/fromString ^String expected)
                                   (ObjectId/zeroId))))
    (.setForceUpdate u (boolean force?))
    (let [res (.update u)]
      (if (contains? #{RefUpdate$Result/NEW RefUpdate$Result/FAST_FORWARD
                       RefUpdate$Result/FORCED RefUpdate$Result/NO_CHANGE}
                     res)
        sha
        (refusal/refuse! :refusal.repos/ref-update-refused
                         {:kind ::ref-refused :ref ref :sha sha :result (str res)})))))

(defn delete-ref! [repos repo-rid ref]
  (let [^Repository r (open-repo repos repo-rid)
        ^RefUpdate u  (.updateRef r ^String ref)]
    (.setForceUpdate u true)
    (.delete u)
    nil))

(defn ref-sha
  [repos repo-rid ref]
  (when (repo-exists? repos repo-rid)
    (some-> ^Repository (open-repo repos repo-rid) (.exactRef ^String ref) (.getObjectId) (.name))))

(defn all-refs
  [repos repo-rid]
  (if-not (repo-exists? repos repo-rid)
    {}
    (let [^Repository r (open-repo repos repo-rid)]
      (into {} (for [^org.eclipse.jgit.lib.Ref v (.getRefs (.getRefDatabase r))
                     :let  [id (.getObjectId v)]
                     :when (and id (not (.isSymbolic v)))]
                 [(.getName v) (.name id)])))))

(defmacro ^:private unless-absent
  [alt & body]
  `(try ~@body
        (catch java.io.IOException missing# ~alt)
        (catch IllegalArgumentException not-a-sha# ~alt)))

(defn commit-info
  [repos repo-rid sha]
  (when (and (repo-exists? repos repo-rid) sha)
    (let [^Repository r (open-repo repos repo-rid)]
      (with-open [rw (RevWalk. r)]
        (when-let [^RevCommit c (unless-absent nil
                                  (.parseCommit rw (ObjectId/fromString ^String sha)))]
          {:sha     (.name c)
           :tree    (.name (.getTree c))
           :parents (mapv #(.name ^RevCommit %) (vec (.getParents c)))
           :author  (.getName (.getAuthorIdent c))
           :email   (.getEmailAddress (.getAuthorIdent c))
           :when-ms (.getTime (.getWhen (.getAuthorIdent c)))
           :message (.getFullMessage c)})))))

(defn merge-base
  [repos repo-rid a b]
  (when (and (repo-exists? repos repo-rid) a b)
    (let [^Repository r (open-repo repos repo-rid)]
      (with-open [rw (RevWalk. r)]
        (unless-absent nil
          (.setRevFilter rw RevFilter/MERGE_BASE)
          (.markStart rw (.parseCommit rw (ObjectId/fromString ^String a)))
          (.markStart rw (.parseCommit rw (ObjectId/fromString ^String b)))
          (some-> (.next rw) .name))))))

(defonce ^:private tree-cache
  (cache/lru 256))

(defn forget-trees!
  []
  (cache/clear! tree-cache))

(defn- read-tree*
  [repos repo-rid sha]
  (when-let [^Repository r (and (repo-exists? repos repo-rid) (open-repo repos repo-rid))]
    (with-open [rw (RevWalk. r)]
      (let [^RevCommit c (.parseCommit rw (ObjectId/fromString ^String sha))]
        (with-open [tw (TreeWalk. r)]
          (.addTree tw (.getTree c))
          (.setRecursive tw true)
          (loop [acc (transient {})]
            (if (.next tw)
              (recur (assoc! acc (.getPathString tw)
                             {:blob (.name (.getObjectId tw 0))
                              :mode (.toString (.getFileMode tw 0))}))
              (persistent! acc))))))))

(defn read-tree
  [repos repo-rid sha]
  (when sha
    (cache/through tree-cache [(repo-key repos repo-rid) (str sha)]
                   #(read-tree* repos repo-rid sha))))

(defn- entry-fingerprint
  [v]
  (if (:gitlink v)
    [(str (:gitlink v)) :gitlink]
    [(.name (.idFor (ObjectInserter$Formatter.) Constants/OBJ_BLOB (entry-bytes v)))
     (or (when (map? v) (:mode v)) regular-mode)]))

(defn changed-paths
  [repos repo-rid sha entries]
  (let [prior (read-tree repos repo-rid sha)
        was   (fn [path]
                (let [{:keys [blob mode]} (get prior path)]
                  [(str blob) (if (gitlink? mode) :gitlink (str/trim (str mode)))]))
        kinds (group-by (fn [[path v]]
                          (cond (not (contains? prior path))        :added
                                (not= (was path) (entry-fingerprint v)) :changed))
                        entries)]
    {:added   (vec (sort (map first (:added kinds))))
     :changed (vec (sort (map first (:changed kinds))))
     :removed (vec (sort (remove #(contains? entries %) (keys prior))))}))

(defn blob-bytes
  ^bytes [repos repo-rid blob-sha]
  (when (repo-exists? repos repo-rid)
    (let [^Repository r (open-repo repos repo-rid)]
      (unless-absent nil
        (.getBytes (.open (.getObjectDatabase r) (ObjectId/fromString ^String blob-sha)))))))

(defn raw-object
  [repos repo-rid sha]
  (when (and (repo-exists? repos repo-rid) sha)
    (let [^Repository r (open-repo repos repo-rid)]
      (unless-absent nil
        (let [ldr (.open (.getObjectDatabase r) (ObjectId/fromString ^String sha))]
          {:type (.getType ldr) :bytes (.getBytes ldr)})))))

(defn insert-raw!
  [^ObjectInserter ins type ^bytes bytes]
  (.name ^ObjectId (.insert ins (int type) bytes)))
(def obj-commit Constants/OBJ_COMMIT)

(defn descends-from?
  [repos repo-rid new-sha old-sha]
  (cond
    (nil? old-sha)      true
    (= new-sha old-sha) true
    :else
    (let [^Repository r (open-repo repos repo-rid)]
      (with-open [rw (RevWalk. r)]
        (unless-absent false
          (.isMergedInto rw
                         (.parseCommit rw (ObjectId/fromString ^String old-sha))
                         (.parseCommit rw (ObjectId/fromString ^String new-sha))))))))

(def internal-ref-prefix
  "refs/sutra/")

(defn anchor-ref
  [branch]
  (str internal-ref-prefix "anchors/"
       (->> (str/split (str branch) #"/" -1) (map escape-component) (str/join "/"))))

(defn pin-anchor!
  [repos repo-rid branch sha]
  (when (and sha (repo-exists? repos repo-rid))
    (update-ref! repos repo-rid (anchor-ref branch) sha {:force? true}))
  sha)

(def ^:private serving-ref-filter
  (reify RefFilter
    (filter [_ refs]
      (let [m (java.util.HashMap.)]
        (doseq [e (.entrySet ^java.util.Map refs)]
          (when-not (.startsWith ^String (key e) internal-ref-prefix)
            (.put m (key e) (val e))))
        m))))

(defn upload-pack
  [repos repo-rid ^InputStream in ^OutputStream out {:keys [advertise-refs?]}]
  (let [^UploadPack up (UploadPack. (open-repo repos repo-rid))]
    (.setBiDirectionalPipe up false)
    (.setRefFilter up serving-ref-filter)
    (.upload up in out nil)
    (when advertise-refs? nil)))

(defn advertise
  [repos repo-rid service ^OutputStream out]
  (let [pw (org.eclipse.jgit.transport.PacketLineOut. out)]
    (.writeString pw (str "# service=" service "\n"))
    (.end pw)
    (case service
      "git-upload-pack"
      (let [^UploadPack up (UploadPack. (open-repo repos repo-rid))]
        (.setBiDirectionalPipe up false)
        (.setRefFilter up serving-ref-filter)
        (.sendAdvertisedRefs up
                             (org.eclipse.jgit.transport.RefAdvertiser$PacketLineOutRefAdvertiser.
                              pw)))
      "git-receive-pack"
      (let [^ReceivePack rp (ReceivePack. (open-repo repos repo-rid))]
        (.setBiDirectionalPipe rp false)
        (.setRefFilter rp serving-ref-filter)
        (.sendAdvertisedRefs rp
                             (org.eclipse.jgit.transport.RefAdvertiser$PacketLineOutRefAdvertiser.
                              pw))))))

(defn receive-pack
  [repos repo-rid ^InputStream in ^OutputStream out {:keys [pre-receive post-receive]}]
  (let [^ReceivePack rp (doto (ReceivePack. (open-repo repos repo-rid))
                          (.setRefFilter serving-ref-filter))
        cmd->map (fn [^ReceiveCommand c]
                   {:ref  (.getRefName c)
                    :old  (let [o (.getOldId c)]
                            (when-not (.equals (ObjectId/zeroId) o) (.name o)))
                    :new  (let [n (.getNewId c)]
                            (when-not (.equals (ObjectId/zeroId) n) (.name n)))
                    :type (str (.getType c))
                    :raw  c})]
    (.setBiDirectionalPipe rp false)
    (.setAtomic rp true)
    (.setPreReceiveHook
     rp (reify org.eclipse.jgit.transport.PreReceiveHook
          (onPreReceive [_ _rp commands]
            (let [cs       (mapv cmd->map commands)
                  rejected (or (when pre-receive (pre-receive cs)) {})]
              (doseq [{:keys [ref raw]} cs
                      :let [why (get rejected ref)]
                      :when why]
                (.setResult ^org.eclipse.jgit.transport.ReceiveCommand raw
                            org.eclipse.jgit.transport.ReceiveCommand$Result/REJECTED_OTHER_REASON
                            ^String why))))))
    (.setPostReceiveHook
     rp
     (reify org.eclipse.jgit.transport.PostReceiveHook
       (onPostReceive [_ rp' commands]
         (when post-receive
           (let [ok (filterv (fn [c] (= org.eclipse.jgit.transport.ReceiveCommand$Result/OK
                                        (.getResult ^org.eclipse.jgit.transport.ReceiveCommand c)))
                             commands)]
             (doseq [line (or (post-receive (mapv cmd->map ok)) [])]
               (.sendMessage ^ReceivePack rp' (str line))))))))
    (.receive rp in out nil)))
