(ns sutra.store.storage.instance-config
  (:require [clojure.edn :as edn]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.digest :as digest]
            [clojure.string :as str]
            [sutra.store.control :as ctl]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(def max-key-length
  191)

(defn cfg-key
  [prefix & parts]
  (let [tail (str/join ":" parts)
        full (str prefix tail)]
    (if (<= (count full) max-key-length)
      full
      (str prefix (subs (digest/sha256-hex tail) 0 32)))))

(defn get-cfg
  ([store k] (get-cfg store k nil))
  ([store k default]
   (or (:v (ctl/get store :instance-config {:k (name k)})) default)))

(defn set-cfgs!
  [store kvs]
  (when (seq kvs)
    (let [pairs (mapv (fn [[k v]] [(name k) v]) kvs)]
      (doseq [[kk _] pairs]
        (when (> (count kk) max-key-length)
          (refusal/refuse! :refusal.store/config-key-too-long
                           {:key kk :length (count kk) :max max-key-length})))
      (ctl/plan! store (for [[kk v] pairs]
                         (fn [store at]
                           (ctl/swapping store :instance-config {:k kk} (constantly {:v v}) at))))))
  nil)

(defn set-cfg!
  [store k v]
  (set-cfgs! store {(name k) v})
  v)

(defn swap-cfg!
  [store k f]
  (let [out (volatile! nil)]
    (ctl/plan! store [(fn [store at]
                        (ctl/swapping store :instance-config {:k (name k)}
                                      (fn [row] {:v (vreset! out (f (:v row)))}) at))])
    @out))

(defn on? [store k] (= "true" (get-cfg store k)))

(defn set-present!
  [store kvs]
  (set-cfgs! store (into {} (filter (fn [[_ v]] (seq (str (or v ""))))) kvs))
  nil)

(def relay-token-key "llm_relay_token")
(def lm-service-url-key   "lm_service_url")

(defn ensure-relay-token!
  [store]
  (or (get-cfg store relay-token-key)
      (set-cfg! store relay-token-key
                (str (java.util.UUID/randomUUID) (java.util.UUID/randomUUID)))))

(def ops-secret-key "ops_peer_secret")

(defn ensure-ops-secret!
  [store]
  (or (get-cfg store ops-secret-key)
      (swap-cfg! store ops-secret-key
                 #(or (not-empty %)
                      (str (java.util.UUID/randomUUID) (java.util.UUID/randomUUID))))))

(defn relay-token
  [store]
  (get-cfg store relay-token-key))

(defn set-lm-service-url!
  [store url]
  (set-cfg! store lm-service-url-key url))

(defn lm-service-url
  [store]
  (or (get-cfg store lm-service-url-key)
      (not-empty (or (env/getenv "SUTRA_LM_SERVICE_URL") ""))))

(defn lm-service-config
  [store]
  (when-let [url (lm-service-url store)]
    (cond-> {:url (if-let [b (some-> (:branch store) str not-empty)]
                    (str url (if (str/includes? url "?") "&" "?") "branch="
                         (java.net.URLEncoder/encode ^String b "UTF-8"))
                    url)}
      (relay-token store) (assoc :token (relay-token store)))))

(def platform-title-key "platform_title")

(def logo-sizes [:favicon :small :medium :large])

(def logo-fallback
  {:favicon [] :small [:medium :large] :medium [:small :large] :large [:medium :small]})

(def ^:private logo-max-chars 1500000)

(def ^:private logo-data-uri
  #"^data:image/(png|jpeg|gif|webp|svg\+xml|x-icon|vnd\.microsoft\.icon);base64,[A-Za-z0-9+/=\s]+$")

(defn- logo-key [size] (str "platform_logo_" (name size)))

(defn resolve-logos
  [stored]
  (into {} (map (fn [s] [s (or (stored s) (some stored (logo-fallback s)))])) logo-sizes))

(defn platform-title
  [store]
  (not-empty (str/trim (str (get-cfg store platform-title-key "")))))

(defn- stored-logos
  [store]
  (into {} (keep (fn [s] (when-let [v (not-empty (str (get-cfg store (logo-key s) "")))] [s v])))
        logo-sizes))

(defn platform-experience
  [store]
  (let [stored (stored-logos store)]
    {:title    (platform-title store)
     :logo     (resolve-logos stored)
     :uploaded (vec (filter stored logo-sizes))}))

(defn checked-logo
  [size v]
  (let [v (str/trim (str v))]
    (cond
      (str/blank? v) ""
      (> (count v) logo-max-chars)
      (refusal/refuse! :refusal.store/logo-too-large
                       {:size size :chars (count v)}
                       {:size-name (name size) :kb (quot logo-max-chars 1000)})
      (not (re-matches logo-data-uri v))
      (refusal/refuse! :refusal.store/logo-not-data-uri {:size size} {:size-name (name size)})
      :else v)))

(defn set-platform-experience!
  [store {:keys [title logo]}]
  (when-let [unknown (seq (remove (set logo-sizes) (keys logo)))]
    (refusal/refuse! :refusal.store/unknown-logo-size {:sizes logo-sizes} {:unknown (vec unknown)}))
  (set-cfgs!
   store
   (cond-> (into {} (map (fn [[s v]] [(logo-key s) (checked-logo s v)])) logo)
     (some? title) (assoc platform-title-key (str/trim (str title)))))
  (platform-experience store))

(def home-page-key "home_page")

(def default-workspaces-key "default_workspaces")

(defn- edn-cfg
  [store k]
  (let [v (get-cfg store k "")]
    (if (str/blank? (str v)) nil (edn/read-string (str v)))))

(defn checked-home-url
  [v]
  (let [v (str/trim (str v))]
    (cond
      (str/blank? v) nil
      (or (not (str/starts-with? v "/")) (str/starts-with? v "//") (re-find #"\s" v))
      (refusal/refuse! :refusal.store/home-url-not-path {:url v})
      :else v)))

(defn home-page
  [store]
  (merge {:default nil :organizations {} :overrides []} (edn-cfg store home-page-key)))

(defn set-home-page!
  [store {:keys [default organizations overrides]}]
  (let [orgs (into (sorted-map)
                   (keep (fn [[o u]] (when-let [u (checked-home-url u)] [(name o) u])))
                   organizations)
        ovs  (vec (for [{:keys [groups url]} overrides
                        :let [gs (vec (distinct (map str groups)))
                              u  (checked-home-url url)]]
                    (if (and (seq gs) u)
                      {:groups gs :url u}
                      (refusal/refuse! :refusal.store/group-override-incomplete
                                       {:groups groups :url url}))))]
    (set-cfg! store home-page-key
              (pr-str {:default (checked-home-url default) :organizations orgs :overrides ovs}))
    (home-page store)))

(defn default-workspaces
  [store]
  (or (edn-cfg store default-workspaces-key) {}))

(defn set-default-workspace!
  [store ws groups]
  (let [gs (vec (distinct (map str groups)))]
    (set-cfg! store default-workspaces-key
              (pr-str (if (seq gs)
                        (assoc (default-workspaces store) (str ws) gs)
                        (dissoc (default-workspaces store) (str ws)))))
    (default-workspaces store)))
