(ns sutra.peering.connections
  (:require [sutra.types.plan :as cp]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.access.api :as access]
            [sutra.authoring.api :as authoring]
            [sutra.kit.refusal :as refusal]
            [sutra.peering.objects :as objects]
            [sutra.peering.rows :as rows]
            [sutra.peering.wire :as wire]
            [sutra.types.authority :as policy]
            [sutra.types.peering :as peering])
  (:import (java.nio.charset StandardCharsets)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def peer-path "/api/peering/peer")

(defn- problems!
  ([code ps] (problems! code ps nil))
  ([code ps params]
   (cp/check! (cp/of-findings (cp/problems :peering/connection ps))
              {:code code :params params})
   nil))

(defn- kw [x] (when (some? x) (keyword (name x))))

(defn- markings-of [s]
  (cond (nil? s) []
        (string? s) (vec (remove str/blank? (map str/trim (str/split s #","))))
        :else (mapv name s)))

(defn ceiling-of
  [{:keys [classification markings]}]
  (let [c (if (string? classification)
            (let [[n c r] (str/split classification #":")]
              (when-not (str/blank? n) {:name n :category c :rank (some-> r parse-long)}))
            (some-> classification (update :rank #(if (string? %) (parse-long %) %))))]
    {:classification c :markings (markings-of markings)}))

(defn data-types-of [m]
  (peering/normal-data-types
   {:source-data (kw (or (:source-data m) (get-in m [:data-types :source-data])))
    :action-data (kw (or (:action-data m) (get-in m [:data-types :action-data])))
    :media       (kw (or (:media m) (get-in m [:data-types :media])))
    :datasets    (kw (or (:datasets m) (get-in m [:data-types :datasets])))}))

(defn view
  [store c]
  (-> (dissoc c :invite)
      (assoc :flowing (peering/flowing? c)
             :health (:state (rows/health store (:connection-id c)))
             :mappings (count (rows/mappings store (:connection-id c))))))

(defn- invite-code [envelope]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder))
                   (.getBytes (json/write-str envelope) StandardCharsets/UTF_8)))

(defn- envelope-of [code]
  (try (json/read-str (String. (.decode (Base64/getUrlDecoder) (str/trim (str code)))
                               StandardCharsets/UTF_8)
                      :key-fn keyword)
       (catch Exception _ nil)))

(defn connect!
  [store actor {nm :name :keys [trust space ingress] :as m}]
  (let [dts     (data-types-of m)
        ceiling (ceiling-of m)
        id      (str (random-uuid))]
    (when-not (get (access/instance-trusts store) (str trust))
      (refusal/refuse! :refusal.peering/no-trust {:trust trust}))
    (problems! :refusal.peering/data-types-invalid (peering/data-type-problems dts))
    (problems! :refusal.peering/ceiling-invalid (peering/ceiling-problems ceiling))
    (let [row  {:connection-id id :name (or (some-> nm str) id) :role :originator
                :status :requested :trust (str trust) :space (some-> space str)
                :data-types dts :ceiling ceiling :ingress (markings-of ingress)
                :marking-map {} :disabled-local false :disabled-remote false
                :profile-id (some-> (:profile-id m) str) :created-by actor}
          code (invite-code (wire/envelope store (peering/invite-payload
                                                  row (access/instance-identity store))))]
      (rows/put-connection! store (assoc row :invite code))
      (assoc (view store row) :invite code))))

(defn- tell!
  [store conn op body]
  (wire/deliver! store (:trust conn) peer-path
                 (assoc body :op op :connection-id (:connection-id conn))))

(defn accept!
  [store actor {:keys [invite space ingress] :as m}]
  (let [env    (or (envelope-of invite) (refusal/refuse! :refusal.peering/invite-unreadable {}))
        opened (wire/opened store env)
        _      (when (map? opened)
                 (refusal/refuse! :refusal.peering/invite-refused {} {:why (:refused opened)}))
        [signer payload] opened
        _      (problems! :refusal.peering/invite-invalid
                          (peering/invite-problems payload {:trust signer
                                                            :existing (rows/connection
                                                                       store
                                                                       (:connection-id
                                                                        payload))}))
        own    (when (or (:classification m) (:markings m)) (ceiling-of m))
        _      (when own
                 (problems! :refusal.peering/ceiling-invalid (peering/ceiling-problems own)))
        row    (cond-> (peering/accepted-connection payload {:space (some-> space str)
                                                             :trust signer :actor actor})
                 own     (assoc :ceiling own)
                 true    (assoc :ingress (markings-of ingress) :marking-map {}))]
    (tell! store row :accepted {:space (:space row) :ceiling (:ceiling row)})
    (rows/put-connection! store row)
    (view store row)))

(defn activate!
  [store id]
  (let [c (rows/by-name-or-id store id)]
    (when-not (= :originator (:role c))
      (refusal/refuse! :refusal.peering/activate-by-originator {:connection-id id}))
    (let [to (or (peering/next-status (:status c) :activate)
                 (refusal/refuse! :refusal.peering/not-activatable {:status (:status c)}
                                  {:status-name (name (:status c))}))
          c  (assoc c :status to)]
      (rows/put-connection! store c)
      (tell! store c :activated {})
      (view store c))))

(defn set-disabled!
  [store id disabled?]
  (let [c (assoc (rows/by-name-or-id store id) :disabled-local (boolean disabled?))]
    (rows/put-connection! store c)
    (try (tell! store c :disabled {:disabled (boolean disabled?)})
         (catch clojure.lang.ExceptionInfo e
           (when-not disabled? (throw e))))
    (view store c)))

(defn propose-ceiling!
  [store id m]
  (let [ceiling (ceiling-of m)
        c       (rows/by-name-or-id store id)]
    (problems! :refusal.peering/ceiling-invalid (peering/ceiling-problems ceiling))
    (let [c (peering/propose-ceiling c ceiling :local)]
      (rows/put-connection! store c)
      (tell! store c :ceiling-proposed {:ceiling ceiling})
      (view store c))))

(defn- resend-source!
  [store id]
  (doseq [m (rows/mappings store id)] (rows/put-mapping! store (assoc m :source-rev nil))))

(defn approve-ceiling!
  [store id]
  (let [c  (rows/by-name-or-id store id)
        c2 (peering/approve-ceiling c :local)]
    (when (:refused c2)
      (refusal/refuse! :refusal.peering/ceiling-refused {:connection-id id} {:why (:refused c2)}))
    (rows/put-connection! store c2)
    (tell! store c2 :ceiling-approved {:ceiling (:ceiling c2)})
    (resend-source! store (:connection-id c2))
    (view store c2)))

(defn set-ingress!
  [store id cidrs]
  (let [c (assoc (rows/by-name-or-id store id) :ingress (markings-of cidrs))]
    (rows/put-connection! store c)
    (view store c)))

(defn map-markings!
  [store id wire local]
  (let [c (rows/by-name-or-id store id)
        c (if (str/blank? (str local))
            (update c :marking-map dissoc (str wire))
            (assoc-in c [:marking-map (str wire)] (str local)))]
    (rows/put-connection! store c)
    (view store c)))

(defn- space-of [schema rid]
  (let [[_ s] (policy/container-chain schema (objects/project-of schema rid))]
    (some-> (get-in schema [:spaces s :space-name]) name)))

(defn describe-type
  [store conn t]
  (let [schema (objects/schema store)
        rid    (objects/type-rid schema t)]
    (when (and rid (or (nil? (:space conn)) (= (:space conn) (space-of schema rid))))
      {:type    (objects/type-name schema rid)
       :attrs   (into {} (for [[a ty] (objects/attr-types schema rid)]
                           [(name a) (if (vector? ty) (mapv #(if (keyword? %) (name %) %) ty)
                                         (some-> ty name))]))
       :product (authoring/product-of-project store (objects/project-of schema rid))})))

(defn- props-of [p]
  (cond (string? p) (into {} (for [pair (str/split p #"\s*,\s*") :when (not (str/blank? pair))]
                               (let [[l r] (str/split pair #"=" 2)] [l (or r l)])))
        (map? p) (into {} (map (fn [[l r]] [(name l) (name r)])) p)
        :else {}))

(defn map-type!
  [store actor id {:keys [local-type remote-type properties links] :as m}]
  (let [c      (rows/by-name-or-id store id)
        schema (objects/schema store)
        rid    (objects/type-rid schema local-type)
        kind   (when rid (objects/datasource-kind schema rid))
        remote (try (tell! store c :describe-type {:type (str remote-type)})
                    (catch clojure.lang.ExceptionInfo e
                      (refusal/refuse! :refusal.peering/remote-silent {:remote-type remote-type}
                                       {:why (ex-message e)})))
        row    {:connection-id (:connection-id c)
                :local-type    (or (objects/type-name schema rid) (str local-type))
                :remote-type   (str remote-type)
                :properties    (props-of properties)
                :links         (props-of links)
                :source-data   (or (kw (:source-data m)) :none)
                :action-data   (or (kw (:action-data m)) :none)
                :created-by    actor}
        rattrs (some-> (:attrs remote)
                       (update-keys keyword)
                       (update-vals #(if (sequential? %)
                                       (mapv (fn [x] (if (string? x) (keyword x) x)) %)
                                       (some-> % keyword))))]
    (problems! :refusal.peering/mapping-invalid
               (peering/mapping-problems
                row {:local-attrs    (when rid (objects/attr-types schema rid))
                     :remote-attrs   rattrs
                     :local-product  (when rid (authoring/product-of-project
                                                store (objects/project-of schema rid)))
                     :remote-product (:product remote)
                     :datasource     kind})
               {:local-type local-type :remote-type remote-type})
    (let [old   (rows/mapping store (:connection-id c) (:local-type row))
          put   (rows/put-mapping! store (merge row (select-keys old [:source-rev :action-rev])))
          notes (peering/datasource-notes row kind)]
      (cond-> put (and (map? put) (seq notes)) (assoc :notes (vec notes))))))

(defn unmap-type! [store id local-type]
  (let [c (rows/by-name-or-id store id)]
    (rows/drop-mapping! store (:connection-id c) local-type)
    {:unmapped (str local-type) :connection-id (:connection-id c)}))

(defn control!
  [store c {:keys [op] :as msg}]
  (let [id (:connection-id c)]
    (case (kw op)
      :accepted
      (let [to (or (peering/next-status (:status c) :accepted)
                   (refusal/refuse! :refusal.peering/no-approval-pending {:connection-id id}))]
        (rows/put-connection! store (assoc c :status to :remote-space (:space msg)
                                           :ceiling (or (:ceiling msg) (:ceiling c))))
        {:ok true})
      :activated
      (let [to (or (peering/next-status (:status c) :activated)
                   (refusal/refuse! :refusal.peering/not-approved {:connection-id id}))]
        (rows/put-connection! store (assoc c :status to))
        {:ok true})
      :disabled
      (do (rows/put-connection! store (assoc c :disabled-remote (boolean (:disabled msg))))
          {:ok true})
      :ceiling-proposed
      (do (problems! :refusal.peering/proposed-ceiling-invalid
                     (peering/ceiling-problems (:ceiling msg)))
          (rows/put-connection! store (peering/propose-ceiling c (:ceiling msg) :remote))
          {:ok true})
      :ceiling-approved
      (let [c2 (peering/approve-ceiling c :remote)]
        (when (:refused c2)
      (refusal/refuse! :refusal.peering/ceiling-refused {:connection-id id} {:why (:refused c2)}))
        (rows/put-connection! store c2)
        (resend-source! store id)
        {:ok true})
      :describe-type
      (or (describe-type store c (:type msg))
          (refusal/refuse! :refusal.peering/no-remote-type {:type (:type msg)}))
      (refusal/refuse! :refusal.peering/unknown-message {:op op}))))
