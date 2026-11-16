(ns sutra.notifications.inbox
  (:require [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.kit.pubsub :as pubsub]
            [sutra.notifications.audience :as audience]
            [sutra.notifications.content :as content]
            [sutra.notifications.mail :as mail]
            [sutra.notifications.ports :as ports]
            [sutra.notifications.prefs :as prefs]
            [sutra.access.api :as access]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.store.control :as ctl])
  (:import (java.util.concurrent ExecutorService Executors)))

(set! *warn-on-reflection* true)

(def render content/render)

(defn recipients
  [{:keys [to to-property]} values]
  (audience/names (concat to (when to-property [(get values (keyword (name to-property)))]))))

(defonce ^:private listeners (pubsub/registry))

(defn subscribe!
  [username sink]
  (pubsub/subscribe! listeners (str username) sink))

(defn- publish-here! [n]
  (pubsub/publish! listeners (str (:recipient n)) n))

(defn- tell-others! [store n]
  (when-let [broadcast (ports/cluster-broadcast store)]
    (try (broadcast :notifications/new n)
         (catch InterruptedException e (throw e))
         (catch Exception t (println "[notifications] broadcast failed:" (ex-message t))))))

(defn- visible?
  [store branch username rid id]
  (if (grants/auth-off? store)
    true
    (when-let [auth (access/of-principal store branch username)]
      (boolean (seq (sub/read-objects (ctx/act-for (ctx/at store branch) auth) rid [(str id)]))))))

(defn admitted
  [store branch names {:keys [object-type object-id objects allowed?]}]
  (let [objects (or objects (when (some? object-id) [[object-type object-id]]))
        ok?     (fn [u] (and (every? (fn [[t id]] (visible? store branch u t id)) objects)
                             (or (nil? allowed?) (allowed? u))))
        {yes true no false} (group-by (comp boolean ok?) (audience/usernames store names))]
    {:allowed (vec yes) :denied (vec no)}))

(defn- public-url [url]
  (let [base (some-> (System/getenv "SUTRA_PUBLIC_URL") str/trim not-empty)]
    (if (and base url (str/starts-with? url "/")) (str (str/replace base #"/$" "") url) url)))

(defn- mail-text [{:keys [message link-url link-label]}]
  (str message
       (when link-url (str "\n\n" (or link-label "Öffnen") ": " (public-url link-url)))
       "\n"))

(defn row
  [n u]
  (let [n (content/sized n)]
    (cond-> (select-keys n [:heading :message :automation-id :branch])
      true                (assoc :recipient u)
      (:source n)         (assoc :source (name (:source n)))
      (:object-type n)    (assoc :object-type (str (:object-type n)))
      (some? (:object-id n)) (assoc :object-id (str (:object-id n)))
      (:link-url n)       (assoc :link-url (:link-url n)
                                 :link-label (content/truncate (or (:link-label n) "Öffnen")
                                                               255)))))

(defn plan
  [n users settings]
  (let [n (content/sized n)]
    (reduce (fn [acc u]
              (let [kind (keyword (name (or (:source n) :automation)))
                    cs   (prefs/channels (prefs/way (get settings u) kind (:automation-id n)))
                    r  (row n u)]
                (cond-> acc
                  (cs :inbox) (update :rows conj r)
                  (cs :live)  (update :live conj u)
                  (cs :email) (update :mails conj
                                      {:recipient u
                                       :subject   (or (:email-subject n) (:heading n))
                                       :body      (mail-text n)
                                       :html      (:email-html n)}))))
            {:rows [] :live #{} :mails []}
            users)))

(defn plan-each
  [base deliveries settings]
  (reduce (fn [acc d] (merge-with into acc (plan (merge base d) [(:recipient d)] settings)))
          {:rows [] :live #{} :mails []}
          deliveries))

(defonce ^:private ^ExecutorService mailer
  (Executors/newSingleThreadExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "sutra-notification-mail") (.setDaemon true))))))

(defn- mail-all!
  [store mails attachments]
  (when-let [cfg (and (seq mails) (mail/config))]
    (let [emails (access/user-emails store (map :recipient mails))]
      (.submit mailer
               ^Runnable
               (fn []
                 (let [atts (some-> attachments force)]
                   (doseq [{:keys [recipient] :as m} mails
                           :let [to (mail/address recipient (emails recipient) (:domain cfg))]
                           :when to]
                     (when-let [e (mail/send! cfg (assoc m :to to :attachments atts))]
                       (println "[notifications] mail to" to "failed:" e)))))))))

(defn deliver!
  [store {:keys [rows live mails]} attachments]
  (let [rows (mapv #(assoc % :id (str (random-uuid))) rows)
        at   (ctl/clock store)]
    (when (seq rows)
      (ctl/insert-many! store :notifications rows))
    (doseq [r rows :when (live (:recipient r)) :let [r (assoc r :created-at at)]]
      (publish-here! r)
      (tell-others! store r))
    (mail-all! store mails attachments)
    rows))

(defn send!
  [store branch names n]
  (let [{:keys [allowed]} (admitted store branch names n)]
    (deliver! store
              (plan (assoc n :branch branch) allowed (prefs/of-users store allowed))
              (:attachments n))))

(defn inbox
  [store username {:keys [limit unread?] :or {limit 50}}]
  (cond->> (ctl/list store :notifications {:recipient (str username)}
                     {:limit (if unread? 500 limit)})
    unread? (filterv #(nil? (:read-at %)))
    unread? (take limit)
    true    vec))

(defn unread-count [store username]
  (count (filter #(nil? (:read-at %))
                 (ctl/list store :notifications {:recipient (str username)} {:limit 500}))))

(defn mark-read!
  [store username ids]
  (let [mine (filter #(nil? (:read-at %))
                     (ctl/list store :notifications {:recipient (str username)} {:limit 500}))
        hit  (if (= :all ids) mine (filter (comp (set (map str ids)) :id) mine))
        at   (ctl/clock store)]
    (doseq [{:keys [id]} hit]
      (ctl/update! store :notifications {:id id} {:read-at at}))
    (count hit)))

(def module
  {:name        :notifications
   :contributes {:cluster/listeners {:notifications/new publish-here!}}})
