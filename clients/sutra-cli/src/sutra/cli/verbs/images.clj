(ns sutra.cli.verbs.images
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.out :refer [emit! mark paint say!]]
            [sutra.cli.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn parse-ref
  [s]
  (let [[nm tag] (str/split (str s) #":" 2)]
    (when (and (seq nm) (seq tag)) {:name nm :tag tag})))

(defn archive-plan
  [manifest]
  (if (= 1 (count manifest))
    (let [{:keys [Config Layers]} (first manifest)]
      {:config Config :layers (vec Layers)})
    {:why (str "the archive holds " (count manifest) " images — save exactly one")}))

(defn- sha256-of
  [f]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        buf (byte-array 65536)]
    (with-open [in (io/input-stream f)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n) (.update md buf 0 n) (recur)))))
    (apply str (map #(format "%02x" %) (.digest md)))))

(defn- upload!
  [^java.io.File f]
  (let [local (sha256-of f)
        {:keys [status body]} (tx/post-bytes! "/api/blob"
                                              (java.nio.file.Files/readAllBytes (.toPath f)))]
    (when-not (and (<= 200 status 299) (= local (:hash body)))
      (throw (ex-info (str "upload of " (.getName f) " failed (" status ")")
                      {:status status :body body :local local})))
    local))

(defn- unpacked!
  [file]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "sutra-image-" (make-array java.nio.file.attribute.FileAttribute 0)))
        r   (sh/sh "tar" "-xf" (str file) "-C" (str dir))]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "cannot unpack " file ": " (str/trim (:err r))) {:file file})))
    dir))

(defmethod step! :image-push [ctx [_ {:keys [ref file project]}]]
  (let [{nm :name tag :tag} (or (parse-ref ref)
                                (throw (ex-info "image push <name>:<tag> <archive.tar>"
                                                {:ref ref})))
        dir  (unpacked! file)]
    (try
      (let [plan (archive-plan (json/read-str (slurp (io/file dir "manifest.json"))
                                              :key-fn keyword))
            _    (when (:why plan) (throw (ex-info (:why plan) {:file file})))
            config (upload! (io/file dir (:config plan)))
            layers (mapv (fn [l]
                           (let [a (upload! (io/file dir l))]
                             (say! ctx (mark :dim "↑" (str (subs a 0 12) "…  " l)))
                             a))
                         (:layers plan))
            {:keys [image]} (req! "image push"
                                  (api/post! :container-images
                                             {:image (str nm ":" tag) :project project
                                              :config config :layers layers}))]
        (say! ctx (mark :green "✓" (str (paint :bold (str nm ":" tag)) " pushed — "
                                        (count layers) " layers, user " (:user image))))
        (emit! ctx {:image image})
        nil)
      (finally (sh/sh "rm" "-rf" "--" (str dir))))))

(defmethod step! :image-delete [ctx [_ {:keys [ref]}]]
  (let [r (or (parse-ref ref) (throw (ex-info "image delete <name>:<tag>" {:ref ref})))]
    (emit! ctx (req! "image delete" (api/delete! :container-image r)))
    nil))
