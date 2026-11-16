(ns sutra.apps.http.comments
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.apps.http.session :refer [with-app]]
            [sutra.blobs.api :as blobs]
            [sutra.capps.comment-feed :as feed]
            [sutra.catalog.lookup :as lookup]
            [sutra.http.responses :refer [json-response]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [sutra.media.http.blobs :as blob-api]
            [sutra.notifications.api :as notifications]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def ^:private max-attachment-bytes (* 200 1024 1024))

(defn- shows-thread?
  [a]
  (some #(and (= :comments (some-> (:widget %) ->kw)) (:object %))
        (tree/nodes (dissoc (:layout a) :vars))))

(defn- readable?
  [store branch auth rid id]
  (boolean (seq (sub/read-objects (ctx/act-for (ctx/at store branch) auth) rid [(str id)]))))

(defn- thread
  [store rid id]
  (mapv (fn [c]
          {:id          (:id c)
           :author      (:author c)
           :body        (:body c)
           :at          (some-> (:created-at c) str)
           :attachments (some-> (:attachments c) edn/read-string)})
        (sort-by (comp str :created-at)
                 (ctl/list store :object-comments {:object-type (str rid) :object-id (str id)}))))

(defn- attachment!
  [{:keys [name mime bytes]}]
  (let [bs (blob-api/b64-bytes bytes)]
    (when (and bs (<= (alength ^bytes bs) max-attachment-bytes))
      (let [{:keys [hash size]} (blobs/put! (ctx/blobs) (java.io.ByteArrayInputStream. bs))]
        {:hash hash :size size :name (str name) :mime (str mime)}))))

(defn- notified!
  [store branch before body author rid id notify?]
  (when-not (false? notify?)
    (notifications/send! store branch (feed/recipients before body author)
                 {:source      :mention
                  :heading     (str author " hat kommentiert")
                  :message     (str/replace (str body) #"@\[([^\]]+)\]\([^)]*\)" "@$1")
                  :object-type (str rid)
                  :object-id   (str id)})))

(defn- add!
  [store branch rid id username {:keys [text attachments notify]}]
  (let [text   (str text)
        before (thread store rid id)
        files  (mapv attachment! attachments)]
    (cond
      (and (str/blank? text) (empty? files))
      (json-response 400 {:error "ein leerer Kommentar"})

      (some nil? files)
      (json-response 413 {:error "Anhang leer oder größer als 200 MB"})

      :else
      (let [row {:id          (str (random-uuid))
                 :object-type (str rid)
                 :object-id   (str id)
                 :author      (str username)
                 :body        text
                 :attachments (when (seq files) (pr-str files))}]
        (ctl/commit! store [[:insert :object-comments [row]]])
        (notified! store branch before text username rid id notify)
        (json-response 200 {:me       username
                            :users    (feed/mentioned-users text)
                            :comments (thread store rid id)})))))

(defn- deleted!
  [store rid id username comment-id]
  (let [c (ctl/get store :object-comments {:id (str comment-id)})]
    (cond
      (or (nil? c) (not= (:object-id c) (str id)) (not= (:object-type c) (str rid)))
      (json-response 404 {:error "kein solcher Kommentar"})

      (not (feed/deletable? c username))
      (json-response 403 {:error "nur der eigene Kommentar lässt sich löschen"})

      :else
      (do (ctl/commit! store [[:delete :object-comments {:id (str comment-id)}]])
          (json-response 200 {:me username :comments (thread store rid id)})))))

(defn- attachment-of
  [store rid id comment-id hash headers branch]
  (let [c (ctl/get store :object-comments {:id (str comment-id)})]
    (if (and c
             (= (:object-id c) (str id))
             (= (:object-type c) (str rid))
             (some #(= hash (:hash %)) (some-> (:attachments c) edn/read-string)))
      (blob-api/stream-blob (ctx/at store branch) hash headers)
      (refusal/refused :refusal.apps/no-attachment {:hash hash}))))

(defn my-application-comments
  [req]
  (with-app req {:json? true :branch? true :preview? true}
    (fn [{:keys [schema branch a body store auth]}]
      (let [username (ep/signed-in-user req)
            {:keys [op object-type id]} body
            rid      (some->> object-type ->kw (lookup/rid-of schema :object-types))]
        (cond
          (not (shows-thread? a))
          (json-response 403 {:error "this application shows no comment thread"})

          (or (nil? rid) (str/blank? (str id)))
          (json-response 400 {:error ":object-type und :id nennen das Elternobjekt"})

          (not (readable? store branch auth rid id))
          (json-response 404 {:error "kein solches Objekt"})

          :else
          (case (->kw op)
            :add        (add! store branch rid id username body)
            :delete     (deleted! store rid id username (:comment body))
            :attachment (attachment-of store rid id (:comment body) (:hash body)
                                       (:headers req) branch)
            (json-response 200 {:me username :comments (thread store rid id)})))))))

