(ns platform-sdk.client
  (:require [clojure.string :as str]
            [platform-sdk.errors :as err]
            [platform-sdk.schema :as schema]
            [sutra.types.paths :as paths]
            [sutra.wire.transport :as tx]))

(set! *warn-on-reflection* true)

(def ^:dynamic *client* nil)

(defonce ^:private ambient-cache (atom {}))

(defn client? [c] (and (map? c) (string? (:base-url c)) (instance? clojure.lang.Atom (:token c))))

(defn- normalize-base [url] (some-> url str/trim (str/replace #"/+$" "")))

(defn- cache-of [c] (or (:cache c) ambient-cache))

(defn- next-generation!
  [cache]
  (swap! cache (fn [m] {:gen (inc (or (:gen m) 0))})))

(defn target [c] (let [c (or c *client*)]
                   (if c
                     {:base-url (:base-url c) :token (some-> ^clojure.lang.Atom (:token c) deref)}
                     (tx/ambient))))

(defn current [] *client*)

(defn check!
  [c label]
  (when (and (some? c) (not (client? c)))
    (err/raise! :usage
                (str label
                     ": the first argument is a client, or nil for the ambient connection — got "
                     (pr-str c))
                {:got c}))
  c)

(defn request
  ([c method endpoint] (request c method endpoint nil nil))
  ([c method endpoint params] (request c method endpoint params nil))
  ([c method endpoint params body]
   (let [c (or c *client*)]
     (check! c (str "request " endpoint))
     (let [path (paths/address endpoint params)
           resp (if c
                  (tx/send-to! (target c) method path body)
                  (tx/send! method path body))]
       (err/check! resp {:endpoint endpoint :method method :path path})))))

(defn token [c] (some-> (:token (or c *client*)) deref))

(defn login!
  [c username password]
  (let [body (request c :post :login nil {:username username :password password})
        tok  (:token body)]
    (when-not tok
      (err/raise! :auth "login returned no token" {:body body}))
    (reset! (:token c) tok)
    (next-generation! (cache-of c))
    c))

(defn client
  [{:keys [base-url token username password]}]
  (let [base (normalize-base base-url)]
    (when-not (seq base)
      (err/raise! :usage
                  ":base-url is required — platform-sdk.client/client {:base-url \"https://host\" …}" {}))
    (let [c {:base-url base :token (atom token) :cache (atom {})}]
      (if (and (nil? token) (some? username))
        (login! c username password)
        c))))

(defmacro with-client
  [c & body]
  `(binding [*client* ~c] ~@body))

(defn logout! [c]
  (let [r (request c :post :logout)]
    (next-generation! (cache-of (or c *client*)))
    r))

(defn me [c] (request c :get :me))

(defn schema
  ([c] (schema c false))
  ([c refresh?]
   (let [c (or c *client*)
         cache (cache-of c)]
     (if-let [s (and (not refresh?) (:schema @cache))]
       s
       (let [gen (:gen (if refresh? (next-generation! cache) @cache))
             s   (request c :get :schema)]
         (swap! cache (fn [m] (if (= gen (:gen m)) (assoc m :schema s :index nil) m)))
         s)))))

(defn- index
  [c]
  (let [c (or c *client*)
        cache (cache-of c)]
    (or (:index @cache)
        (let [s (schema c)
              idx {:rids      (schema/rid-index s)
                   :rid->type (schema/rid->name s)
                   :types     (schema/object-type-index s nil)}]
          (swap! cache (fn [m] (if (identical? s (:schema m)) (assoc m :index idx) m)))
          idx))))

(defn rids [c] (:rids (index c)))

(defn type-rid
  [c type-kw]
  (or (get-in (index c) [:rids :types type-kw])
      (err/raise! :not-found (str "no object type " (pr-str type-kw) " in this ontology")
                  {:type type-kw})))

(defn side
  [c type-kw side-kw]
  (let [sides (get-in (index c) [:types type-kw :links])]
    (or (get sides side-kw)
        (err/raise! :not-found (str (pr-str type-kw) " has no link side " (pr-str side-kw)
                                    " — known: " (pr-str (vec (keys sides))))
                    {:type type-kw :link side-kw :known (vec (keys sides))}))))

(defn type-of-rid [c rid] (get-in (index c) [:rid->type rid] rid))

(defn ontology-index
  ([c] (ontology-index c nil))
  ([c backing]
   (let [s (schema c)]
     {:object-types (schema/object-type-index s backing)
      :rids (schema/rid-index s)})))
