(ns sutra.compute.images
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.blobs.api :as blobs]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.project-scope :as scope]
            [sutra.foundation.settings :as settings]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def ^:private name-pattern #"[a-z0-9][a-z0-9._-]{0,199}")
(def ^:private tag-pattern #"[A-Za-z0-9_][A-Za-z0-9._-]{0,127}")

(defn- head-bytes
  ^bytes [blob-store h n]
  (with-open [in (blobs/open blob-store h)]
    (let [buf (byte-array n)
          got (.readNBytes ^java.io.InputStream in buf 0 n)]
      (java.util.Arrays/copyOf buf got))))

(defn layer-format
  [^bytes head]
  (let [b #(bit-and 0xff (aget head %))]
    (cond
      (and (<= 2 (alength head)) (= 0x1f (b 0)) (= 0x8b (b 1))) :gzip
      (and (<= 262 (alength head))
           (= "ustar" (String. head 257 5 "US-ASCII")))         :tar
      :else nil)))

(defn- numeric-user
  [user]
  (let [uid (first (str/split (str user) #":" 2))]
    (when (re-matches #"\d{1,9}" uid) (parse-long uid))))

(defn requirement-problems
  [config formats tag]
  (let [user (get-in config [:config :User])
        uid  (numeric-user user)
        diff (get-in config [:rootfs :diff_ids])]
    (cond-> []
      (= "latest" tag)
      (conj "the tag `latest` names no fixed image — push a version tag")
      (nil? uid)
      (conj (str "the image's user must be numeric (USER 5001), got " (pr-str (or user ""))))
      (= 0 uid)
      (conj "the image's user must not be root (0)")
      (not= "linux" (:os config))
      (conj (str "the image must be built for linux, got " (pr-str (:os config))))
      (not= "amd64" (:architecture config))
      (conj (str "the image must be built for amd64 (--platform linux/amd64), got "
                 (pr-str (:architecture config))))
      (and (seq diff) (not= (count diff) (count formats)))
      (conj (str "the config names " (count diff) " layers, " (count formats) " were pushed"))
      (some nil? formats)
      (conj "a layer is neither tar nor gzip-compressed tar (zstd is not supported)"))))

(defn- run-config
  [config]
  (let [c (:config config)]
    {:user        (str (numeric-user (:User c)))
     :env         (vec (:Env c))
     :working-dir (not-empty (str (:WorkingDir c)))}))

(defn- parse-ref
  [s]
  (let [[nm tag] (str/split (str s) #":" 2)]
    {:name nm :tag tag}))

(defn- bad-ref
  [nm tag]
  (cond
    (not (and (string? nm) (re-matches name-pattern nm)))
    (str "an image name is lower-case letters, digits and . _ -, got " (pr-str nm))
    (not (and (string? tag) (re-matches tag-pattern tag)))
    (str "a tag is letters, digits and . _ -, got " (pr-str tag))))

(defn- row->image
  [{:keys [name tag project image-id config layers size pushed-by pushed-at]}]
  {:name name :tag tag :project project :id image-id
   :user (:user config) :env (:env config) :working-dir (:working-dir config)
   :layers layers :size size :pushed-by pushed-by :pushed-at pushed-at})

(defn one
  [store nm tag]
  (some-> (ctl/get store :container-images {:name nm :tag tag}) row->image))

(defn verdict
  [{:keys [name tag project config-id config missing formats sizes layers pushed-by]}]
  (let [why (or (bad-ref name tag)
                (when-not (seq layers) "an image has at least one layer")
                (when (seq missing) (str "not pushed yet: " (str/join ", " missing)))
                (not-empty (str/join "; " (requirement-problems config formats tag))))]
    (if why
      {:why why}
      {:row {:name name :tag tag :project project :image-id config-id
             :config (run-config config) :layers (vec layers) :size (reduce + 0 sizes)
             :pushed-by pushed-by}})))

(defn- blob-facts
  [blob-store config-id layers]
  (let [missing (vec (remove #(blobs/exists? blob-store %) (cons config-id layers)))]
    (if (seq missing)
      {:missing missing}
      {:config  (with-open [r (io/reader (blobs/open blob-store config-id))]
                  (json/read r :key-fn keyword))
       :formats (mapv #(layer-format (head-bytes blob-store % 512)) layers)
       :sizes   (mapv #(or (blobs/size-of blob-store %) 0) layers)})))

(defn register!
  [store auth {:keys [name tag project config layers]}]
  (let [facts (merge {:name name :tag tag :project project :config-id config
                      :layers layers
                      :pushed-by (str (or (grants/acting-username auth)
                                          (grants/acting-user auth)))}
                     (when-not (bad-ref name tag) (blob-facts (ctx/blobs) config layers)))
        {:keys [why row]} (verdict facts)]
    (when why
      (refusal/refuse! :refusal.foreign/image-invalid {:image (str name ":" tag) :why why}))
    (ctl/commit! store auth [[:upsert :container-images [(assoc row :pushed-at ctl/now)]]])
    (one store name tag)))

(defn readable
  [store auth]
  (into [] (comp (filter #(grants/allows? auth (:project %) false :read)) (map row->image))
        (ctl/list store :container-images {})))

(defn delete!
  [store auth nm tag]
  (when (ctl/get store :container-images {:name nm :tag tag})
    (ctl/delete-as! store auth :container-images {:name nm :tag tag})
    true))

(defn run-refusal
  [enabled? transform ref image]
  (cond
    (not enabled?) [:refusal.foreign/container-workflows-off {:transform transform}]
    (nil? image)   [:refusal.foreign/image-unknown {:transform transform :image ref}]))

(defn for-run!
  [store transform {:keys [image tag]}]
  (let [enabled? (settings/value :container-workflows)
        img      (when enabled? (one store image tag))]
    (when-let [[code data] (run-refusal enabled? transform (str image ":" tag) img)]
      (refusal/refuse! code data))
    img))

(defn row-hashes
  [rows]
  (into #{} (mapcat #(cons (:image-id %) (:layers %))) rows))

(defn- held-hashes [store] (row-hashes (ctl/list store :container-images {})))

(defn- list-images
  [{store :sutra/store auth :sutra/auth}]
  (ok {:images (readable store auth)}))

(defn- push-image
  [{:keys [body-params] store :sutra/store auth :sutra/auth schema :sutra/schema}]
  (let [{:keys [image config layers project]} body-params
        {nm :name tag :tag} (parse-ref image)
        project (scope/resolve-project schema project)]
    (cond
      (nil? project) (refusal/refused :refusal.compute/image-project {:image image})
      (not (grants/allows? auth project false :update))
      (refusal/refused :refusal.compute/image-push-denied {:project project})
      :else (ok {:image (register! store auth {:name nm :tag tag :project project
                                               :config config :layers (vec layers)})}))))

(defn- image-of
  [{:keys [path-params] store :sutra/store auth :sutra/auth}]
  (let [img (one store (:name path-params) (:tag path-params))]
    (when (and img (grants/allows? auth (:project img) false :read)) img)))

(defn- get-image
  [req]
  (if-let [img (image-of req)]
    (ok {:image img})
    {:status 404 :body {:error "no such image"}}))

(defn- delete-image
  [{store :sutra/store auth :sutra/auth :as req}]
  (if-let [{:keys [name tag]} (image-of req)]
    (ok {:deleted (delete! store auth name tag)})
    {:status 404 :body {:error "no such image"}}))

(def module
  {:name        :container-images
   :contributes {:http/routes {:container-images {:get list-images :post push-image}
                               :container-image  {:get get-image :delete delete-image}}
                 :blobs/roots {:container-images held-hashes}}})
