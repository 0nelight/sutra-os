(ns sutra.compute.container
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [sutra.kit.digest :as digest]
            [sutra.foundation.env :as env]
            [sutra.foundation.outbound :as outbound])
  (:import [java.security MessageDigest]))

(set! *warn-on-reflection* true)

(defn entry-name
  [s]
  (-> (str s) (str/replace #"^(\./)+" "") (str/replace #"/+$" "")))

(defn- split-path [p]
  (let [i (str/last-index-of p "/")]
    (if i [(subs p 0 i) (subs p (inc i))] ["" p])))

(defn- under [dir nm] (if (str/blank? dir) nm (str dir "/" nm)))

(defn whiteouts
  [names]
  (reduce (fn [acc n]
            (let [p (entry-name n)
                  [dir base] (split-path p)]
              (cond
                (= ".wh..wh..opq" base)
                (-> acc (update :clear conj dir) (update :markers conj p))
                (str/starts-with? base ".wh.")
                (-> acc (update :remove conj (under dir (subs base 4)))
                    (update :markers conj p))
                :else acc)))
          {:clear [] :remove [] :markers []}
          names))

(defn- in-out [p] (if (str/blank? p) "/out" (str "/out/" p)))

(defn whiteout-commands
  [{:keys [clear remove]}]
  (cond-> (mapv (fn [d] ["find" (in-out d) "-mindepth" "1" "-maxdepth" "1"
                         "-exec" "rm" "-rf" "{}" "+"])
                clear)
    (seq remove) (conj (into ["rm" "-rf" "--"] (map in-out) remove))))

(defn marker-command
  [{:keys [markers]}]
  (when (seq markers) (into ["rm" "-f" "--"] (map in-out) markers)))

(def extract-command
  ["tar" "-x" "-f" "/layer" "-C" "/out" "--no-same-owner"
   "--exclude=dev/*" "--exclude=./dev/*"])

(def list-command ["tar" "-t" "-f" "/layer"])

(def writable-command ["chmod" "-R" "u+rwX" "/out"])

(defn unpack-jail
  [host-dirs layer out cmd]
  (vec (concat ["timeout" "900s" "bwrap" "--unshare-all" "--die-with-parent" "--new-session"]
               (mapcat (fn [d] ["--ro-bind" d d]) host-dirs)
               ["--proc" "/proc" "--dev" "/dev" "--tmpfs" "/tmp"]
               (when layer ["--ro-bind" layer "/layer"])
               (when out ["--bind" out "/out"])
               ["--clearenv" "--setenv" "PATH" "/usr/bin:/bin" "--"]
               cmd)))

(def host-mount "/sutra/host")

(defn io-path
  [host]
  (let [f (io/file host)]
    (str "/sutra/io/" (subs (digest/sha256-hex (str host)) 0 16) "-" (.getName f))))

(defn remap-manifest
  [manifest]
  (update-vals manifest (fn [e] (cond-> e
                                  (:path e)      (update :path io-path)
                                  (:media-dir e) (update :media-dir io-path)))))

(defn- in-host [p] (str host-mount p))

(defn driver-command
  [{:keys [loader python]}]
  (let [lib-dir (.getParent (io/file loader))]
    [(in-host loader)
     "--library-path" (str/join ":" (map in-host [lib-dir "/usr/lib" "/usr/local/lib"]))
     (in-host python)]))

(def ^:private reserved #{"proc" "dev" "tmp" "sys" "sutra" "work"})

(defn- root-mount
  [rootfs {:keys [name kind target]}]
  (when-not (reserved name)
    (case kind
      :symlink ["--symlink" target (str "/" name)]
      ["--ro-bind" (str rootfs "/" name) (str "/" name)])))

(defn image-env
  [lines]
  (merge {"PATH" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"}
         (into {} (keep #(let [[k v] (str/split (str %) #"=" 2)] (when (seq k) [k (or v "")])))
               lines)))

(defn run-argv
  [{:keys [prefix rootfs entries image network resolv? binds rw-binds tmpfs env cmd]}]
  (let [uid (or (not-empty (:user image)) "65534")]
    (vec (concat prefix
                 ["bwrap" "--unshare-user" "--unshare-ipc" "--unshare-pid" "--unshare-uts"
                  "--unshare-cgroup-try"]
                 (when-not network ["--unshare-net"])
                 ["--uid" uid "--gid" uid "--die-with-parent" "--new-session"]
                 (mapcat #(root-mount rootfs %) entries)
                 ["--proc" "/proc" "--dev" "/dev" "--tmpfs" "/tmp"]
                 (when (and network resolv?) ["--ro-bind" "/etc/resolv.conf" "/etc/resolv.conf"])
                 ["--ro-bind" "/usr" (in-host "/usr")]
                 (mapcat (fn [t] ["--tmpfs" t]) tmpfs)
                 (mapcat (fn [[h t]] ["--ro-bind" h t]) binds)
                 (mapcat (fn [[h t]] ["--bind" h t]) rw-binds)
                 ["--clearenv"]
                 (mapcat (fn [[k v]] ["--setenv" (str k) (str v)])
                         (merge (image-env (:env image))
                                {"PYTHONHOME" (in-host "/usr") "PYTHONDONTWRITEBYTECODE" "1"
                                 "HOME" "/work" "USER_WORKING_DIR" "/work"
                                 "SUTRA_CONTAINER" "1"}
                                env))
                 ["--chdir" "/work" "--"]
                 cmd))))

(def host-dirs
  (delay (filterv #(.exists (io/file %)) ["/usr" "/bin" "/lib" "/lib64"])))

(defn- on-usr! [what p]
  (let [c (.getCanonicalPath (io/file p))]
    (when-not (str/starts-with? c "/usr/")
      (throw (ex-info (str "the platform's " what " is not under /usr: " c) {:path c})))
    c))

(def host-driver
  (delay {:loader (on-usr! "loader" "/lib64/ld-linux-x86-64.so.2")
          :python (on-usr! "python3" "/usr/bin/python3")}))

(defn root-entries
  [rootfs]
  (vec (for [^java.io.File f (.listFiles (io/file rootfs))
             :let [p (.toPath f)]]
         (if (java.nio.file.Files/isSymbolicLink p)
           {:name (.getName f) :kind :symlink
            :target (str (java.nio.file.Files/readSymbolicLink p))}
           {:name (.getName f) :kind :dir}))))

(defn resolv? [rootfs] (.exists (io/file rootfs "etc/resolv.conf")))

(defn- sh! [argv]
  (let [r (apply sh/sh argv)]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "image unpack failed: " (str/trim (str (:err r))))
                      {:argv argv :exit (:exit r)})))
    (:out r)))

(defn save-verified!
  [^java.io.InputStream in address ^java.io.File file]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (with-open [in (java.security.DigestInputStream. in md)
                out (io/output-stream file)]
      (io/copy in out))
    (let [got (apply str (map #(format "%02x" %) (.digest md)))]
      (when-not (= got address)
        (io/delete-file file true)
        (throw (ex-info "an image layer arrived corrupt" {:want address :got got}))))
    file))

(defn url-opener
  [base-url grants]
  (fn [address]
    (let [[status body] (outbound/stream! (str base-url "/" address)
                                          {"Authorization" (str "Bearer " (get grants address))})]
      (when-not (= 200 status)
        (some-> ^java.io.InputStream body .close)
        (throw (ex-info (str "image layer " address " not served: " status)
                        {:address address :status status})))
      body)))

(defn- unpack-layer!
  [rootfs layer]
  (let [jail  (partial unpack-jail @host-dirs (str layer))
        plan  (whiteouts (str/split-lines (sh! (jail nil list-command))))]
    (run! #(sh! (jail rootfs %)) (whiteout-commands plan))
    (sh! (jail rootfs extract-command))
    (some->> (marker-command plan) (jail rootfs) sh!)
    (sh! (jail rootfs writable-command))))

(def ^:private cache-root
  (delay (or (not-empty (env/getenv "SUTRA_IMAGE_DIR"))
             (str (System/getProperty "java.io.tmpdir") "/sutra-images"))))

(defonce ^:private locks (java.util.concurrent.ConcurrentHashMap.))

(defn- lock-of [k]
  (.computeIfAbsent ^java.util.concurrent.ConcurrentHashMap locks k
                    (reify java.util.function.Function (apply [_ _] (Object.)))))

(defn rootfs!
  [{:keys [id layers]} open]
  (let [dir  (io/file @cache-root id)
        lock (lock-of id)]
    (locking lock
      (when-not (.isDirectory dir)
        (let [tmp (io/file @cache-root (str "." id "-" (random-uuid)))]
          (.mkdirs tmp)
          (try
            (doseq [address layers
                    :let [f (io/file @cache-root (str ".layer-" address "-" (random-uuid)))]]
              (try (save-verified! (open address) address f)
                   (unpack-layer! (.getAbsolutePath tmp) (.getAbsolutePath f))
                   (finally (io/delete-file f true))))
            (.renameTo tmp dir)
            (finally
              (when (.exists tmp) (sh/sh "rm" "-rf" "--" (.getAbsolutePath tmp)))))))
      (.getAbsolutePath dir))))
