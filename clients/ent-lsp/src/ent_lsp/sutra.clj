(ns ent-lsp.sutra
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.change.server :as srv]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private frist-ms 2000)

(def ^:private sutra-checkout
  (delay (some-> (io/resource "sutra/change/server.clj") io/file
                 (as-> f (nth (iterate #(.getParentFile ^java.io.File %) f) 6))
                 str)))

(def ^:private creds-like-the-cli
  (delay (alter-var-root #'srv/cred-paths
                         (fn [f]
                           (fn [dir sutra-home home]
                             (f dir (or sutra-home @sutra-checkout) home))))))

(defn connect
  [root]
  (try @creds-like-the-cli
       (srv/verbinden! root)
       (catch Throwable _ nil)))

(defn fetch
  [sess endpoint params]
  (binding [tx/*auth-token* (:token sess)]
    (api/get! endpoint params)))

(defn- call [sess nm kind]
  (let [r (fetch sess :schema-name-suggest (cond-> {:name (name nm)} kind (assoc :kind kind)))]
    (when (<= 200 (:status r) 299) (:candidates (:body r)))))

(defn suggester
  [root]
  (let [sess  (delay (connect root))
        cache (atom {})]
    (fn [nm kind]
      (let [k [nm kind]]
        (if (contains? @cache k)
          (@cache k)
          (let [f (future (when-let [s @sess] (try (call s nm kind) (catch Throwable _ nil))))
                v (deref f frist-ms ::zu-spaet)]
            (when-not (= ::zu-spaet v)
              (swap! cache assoc k v)
              v)))))))

(defn describe [{:keys [name owner why]}]
  (str ":" (clojure.core/name name)
       (when owner (str " (" (clojure.core/name owner) ")"))
       (when why (str " — " (str/replace (clojure.core/name why) "-" " ")))))
