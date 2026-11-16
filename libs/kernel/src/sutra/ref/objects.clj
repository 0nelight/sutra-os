(ns sutra.ref.objects
  (:require [clojure.string :as str]
            [sutra.ref.err :refer [fail!]]
            [sutra.ref.params :as p]
            [sutra.ref.sides :as sides]
            [sutra.ref.sigma :as s]
            [sutra.ref.value :as v]))

(set! *warn-on-reflection* true)

(def dns-namespace (java.util.UUID/fromString "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))

(defn uuid-v5 [^java.util.UUID ns ^String text]
  (let [buf (java.nio.ByteBuffer/allocate 16)
        _   (.putLong buf (.getMostSignificantBits ns))
        _   (.putLong buf (.getLeastSignificantBits ns))
        md  (java.security.MessageDigest/getInstance "SHA-1")
        _   (.update md (.array buf))
        _   (.update md (.getBytes text "UTF-8"))
        b   (java.util.Arrays/copyOf (.digest md) 16)]
    (aset-byte b 6 (unchecked-byte (bit-or 0x50 (bit-and (aget b 6) 0x0f))))
    (aset-byte b 8 (unchecked-byte (bit-or 0x80 (bit-and (aget b 8) 0x3f))))
    (let [w (java.nio.ByteBuffer/wrap b)] (java.util.UUID. (.getLong w) (.getLong w)))))

(defn key-minted-id
  [cx W T values]
  (let [rid (or (s/call cx :rid-of W :object-types T) T)
        {[p & more] :properties src :source} (s/call cx :object-key W rid)]
    (when (and (= :primary-key src) (nil? more)
               (string? rid) (str/starts-with? rid "ri.")
               (not (get-in W [:object-types rid :resolve-entity])))
      (some->> (get values p) pr-str (str rid "/") (uuid-v5 dns-namespace) str))))

(def ^:private link-identity
  {:one-to-one [[:from-id :to-id] true] :many-to-one [[:from-id] false]
   :one-to-many [[:to-id] false]       :many-to-many [[:from-id :to-id] true]})

(defn link-id
  [W L from to]
  (let [card (or (get-in W [:link-types L :cardinality])
                 (fail! :unknown-link-type (str "link-id-for: unknown link-type " (pr-str L))
                        {:link-type L}))
        [cols sort?] (or (link-identity card)
                         (fail! :unknown-cardinality (str "unknown cardinality " card)
                                {:cardinality card}))
        row   {:from-id (str from) :to-id (str to)}
        parts (if sort? (sort (map row cols)) (mapv row cols))]
    (uuid-v5 dns-namespace (str (name L) "/" (str/join "|" parts)))))

(defn- record
  [run-state op T payload]
  (update run-state :ops (fnil conj []) {:op op :type T :payload payload}))

(defn- authorize! [cx op T id]
  (s/call cx :authorize op (when (or T id) {:type T :id id})))

(defn- validate! [cx W T values complete?]
  (when W (s/call cx :validate-row W T values complete?)))

(defn- created-in-run? [run-state T id]
  (let [x (str id)]
    (some #(and (= :create-object (:op %)) (= T (:type %)) (= x (str (get-in % [:payload :id]))))
          (:ops run-state))))

(defn resolve-ref
  [cx W T raw]
  (let [{:keys [properties source]} (s/call cx :object-key W T)
        k    (when (= 1 (count properties)) (first properties))
        kv   (when k
               (let [v (p/decode-text (get (s/call cx :attr-types W T) k) (str raw))]
                 (when-not (= p/unreadable v) v)))
        by-k (when (some? kv) (s/call cx :lookup W T {:key [k kv]}))
        miss {:key k :source source :value raw}]
    (cond
      (next by-k) {:problem (assoc miss :reason :ambiguous)}
      (seq by-k)  {:object (first by-k)}
      :else       (if-let [row (first (s/call cx :lookup W T {:id (str raw)}))]
                    {:object row}
                    {:problem (assoc miss :reason :unresolved)}))))

(defn- existing-target!
  [cx run-state op target T id handle?]
  (let [cx (s/under cx run-state)]
    (cond
      handle? id
      (not (s/can-read? cx)) id
      (and (some? id) (created-in-run? run-state T id)) id
      :else
      (let [W     (:world run-state)
            rid   (or (s/call cx :rid-of W :object-types T) T)
            by-id (when (some? id) (s/call cx :object T (str id)))
            by-ref (when (and (nil? by-id) (some? id) (not (map? id)))
                     (resolve-ref cx W rid (str id)))]
        (cond
          by-id id
          (:object by-ref) (str (:id (:object by-ref)))
          :else (fail! :rule-target-unresolved
                       (str "rule " op " targets " (pr-str id) ", which is no existing " T)
                       {:op op :type T :target target
                        :unresolved id}))))))

(defn- update-target
  [B t]
  (if (and (vector? t) (= :get (first t)))
    (let [[_ k a] t
          o (get B k)]
      (if (and (map? o) (or (nil? a) (= a :id)))
        [o (:id o) (:object-type-ref o)]
        [nil (v/resolve-binding B t) nil]))
    (let [r (v/resolve-binding B t)]
      (if (map? r)
        [r (:id r) (:object-type-ref r)]
        [nil (if (string? r)
               (try (java.util.UUID/fromString r) (catch IllegalArgumentException _ r))
               r)
         nil]))))

(defn target-id [B t]
  (second (update-target B t)))

(defn- key-held?
  [cx run-state T id]
  (or (created-in-run? run-state T id)
      (let [cx (s/under cx run-state)]
        (and (s/can-read? cx) (some? (s/call cx :object T (str id)))))))

(defn create-object [{:keys [object-type values id markings as]} run-state cx]
  (let [{W :world B :bindings} run-state
        T    (v/resolve-arg B object-type)
        vals (v/resolve-arg B values)
        kid  (when (and (nil? id) W) (key-minted-id cx W T vals))
        oid  (or (when id (v/resolve-arg B id)) kid (s/call cx :fresh-uuid))
        _    (validate! cx W T vals true)
        _    (when (and kid (key-held? cx run-state T kid))
               (fail! :key-taken
                      (str T " " (pr-str vals) " exists already — an object is its key;"
                           " edit the existing object instead")
                      {:type T :id kid}))
        mks  (when markings (set (v/resolve-arg B markings)))
        act  (get-in cx [:kappa :action-type])]
    (authorize! cx :create T oid)
    (-> run-state
        (record :create-object T {:id oid :values vals :markings mks})
        (v/emit {:op :create :target {:type T :id oid} :after vals :actor act})
        (cond-> as (assoc-in [:bindings as]
                             (cond-> {:object-type-ref T :values vals :id oid}
                               act (assoc :_action-type act)))))))

(defn update-object [{:keys [target values object-type]} run-state cx]
  (let [{W :world B :bindings} run-state
        [bound oid from-handle] (update-target B target)
        vals (v/resolve-arg B values)
        T    (or (when object-type (v/resolve-arg B object-type)) from-handle
                 (fail! :no-object-type "update: cannot resolve object type"))
        oid  (existing-target! cx run-state :update target T oid (v/handle? bound))]
    (validate! cx W T vals false)
    (authorize! cx :update T oid)
    (-> run-state
        (record :update-object T {:id oid :values vals})
        (v/emit {:op :update :target {:type T :id oid} :after vals}))))

(defn delete-object [{:keys [target object-type]} run-state cx]
  (let [B   (:bindings run-state)
        t   (v/resolve-arg B target)
        oid (if (v/handle? t) (:id t) t)
        T   (or (when object-type (v/resolve-arg B object-type))
                (when (v/handle? t) (:object-type-ref t))
                (fail! :no-object-type "delete-object: cannot resolve object type"))
        oid (existing-target! cx run-state :delete-object target T oid (v/handle? t))]
    (authorize! cx :delete-object T oid)
    (-> run-state
        (record :delete-object T {:id oid})
        (v/emit {:op :delete-object :target {:type T :id oid}}))))

(defn- end-types
  [cx W T]
  (if (and T (s/call cx :interface? W T)) (s/call cx :implementers W T) (some-> T vector)))

(defn- ends-exist!
  [cx run-state W L from to]
  (when-let [lt (and (s/can-read? cx) (get-in W [:link-types L]))]
    (doseq [[end id T] [[:from from (:from-object-type lt)] [:to to (:to-object-type lt)]]
            :let [ts (end-types cx W T)]
            :when (seq ts)
            :when (not-any? #(or (created-in-run? run-state % id)
                                 (some? (s/call (s/under cx run-state) :object % (str id)))) ts)]
      (fail! :link-end-missing
             (str "link " L ": its " (name end) " end " (pr-str id) " is no existing " T)
             {:link L :end end :id id :type T}))))

(defn- link-args
  [{:keys [link-type from-id to-id id markings]} W B]
  (let [L    (v/resolve-arg B link-type)
        from (v/resolve-arg B from-id)
        to   (v/resolve-arg B to-id)]
    {:L    L
     :from from
     :to   to
     :cid  (or (when id (v/resolve-arg B id)) (link-id W L from to))
     :mks  (when markings (set (v/resolve-arg B markings)))}))

(defn create-link [{:keys [as] :as step} run-state cx]
  (let [{W :world B :bindings} run-state
        {:keys [L from to cid mks]} (link-args step W B)]
    (ends-exist! cx run-state W L from to)
    (authorize! cx :create-link L cid)
    (-> run-state
        (record :create-link L {:id cid :from-id from :to-id to :markings mks})
        (v/emit {:op :create-link :target {:type L :id cid} :after {:from-id from :to-id to}})
        (cond-> as (assoc-in [:bindings as]
                             {:link-type-ref L :id cid :from-object-id from :to-object-id to})))))

(defn- link-target
  [{:keys [target link-type from-id to-id]} run-state]
  (let [B   (:bindings run-state)
        t   (when target (v/resolve-arg B target))
        L   (or (when link-type (v/resolve-arg B link-type)) (when (v/handle? t) (:link-type-ref t))
                (fail! :no-link-type "delete-link: cannot resolve link type"))
        cid (cond (v/handle? t) (:id t)
                  t t
                  (and from-id
                       to-id) (link-id (:world run-state) L (v/resolve-arg B from-id)
                                       (v/resolve-arg B to-id))
                  :else (fail! :no-link-target "delete-link needs :target, or :from-id and :to-id"
                               {:link L}))]
    [L cid t]))

(defn delete-link [step run-state cx]
  (let [[L cid t] (link-target step run-state)
        _   (authorize! cx :delete-link L cid)
        ends (if (v/handle? t)
               {:from-id (:from-object-id t) :to-id (:to-object-id t)}
               (when (s/can-read? cx)
                 (s/call (s/under cx run-state) :link-ends (:world run-state) L cid)))]
    (-> run-state
        (record :delete-link L (merge {:id cid} ends))
        (v/emit {:op :delete-link :target {:type L :id cid}}))))

(defn reclassify [dir {:keys [target object-type marking]} run-state cx]
  (let [{W :world B :bindings} run-state
        oid (v/resolve-arg B target)
        T   (or (s/call cx :rid-of W :object-types (v/resolve-arg B object-type))
                (fail! :no-object-type "reclassify: cannot resolve object type"))
        mk  (or (v/resolve-arg B marking) (fail! :no-marking "reclassify: no :marking given"))
        op  (if (= dir :add) :apply-marking :remove-marking)]
    (authorize! cx op T oid)
    (-> run-state
        (record :update-markings T
                (if (= dir :add) {:id oid :add [(keyword mk)]} {:id oid :remove [(keyword mk)]}))
        (v/emit {:op op :target {:type T :id oid} :after {:marking mk}
                 :actor (get-in cx [:kappa :action-type])}))))

(defn- label [W section k]
  (or (some (fn [[f v]] (when (str/ends-with? (name f) "-name") v)) (get-in W [section k])) k))

(defn- implementing! [cx W iface ot op]
  (let [d  {:op op :interface iface :object-type ot}
        il #(name (label W :interfaces %))
        tl #(name (label W :object-types %))]
    (when-not iface (fail! :no-interface (str op ": no :interface given") d))
    (when-not (s/call cx :interface? W iface)
      (fail! :not-an-interface (str op ": " (il iface) " is not an interface") d))
    (when-not ot (fail! :no-object-type (str op ": cannot resolve the object type to edit") d))
    (let [impls (set (s/call cx :implementers W iface))]
      (when-not (contains? impls ot)
        (fail! :not-implemented (str op ": " (tl ot) " does not implement " (il iface)
                                     " — implementers are: " (str/join ", " (sort (map tl impls))))
               d))))
  ot)

(defn- interface-values! [cx W iface ot vals op pk-writable?]
  (let [props (set (s/call cx :interface-properties W iface))
        stray (sort (remove props (keys vals)))
        il    (name (label W :interfaces iface))
        tl    (name (label W :object-types ot))]
    (when (seq stray)
      (fail! :stray-properties
             (str op ": " (str/join ", " (map name stray)) " is not declared by " il
                  " — an interface rule may only write interface properties ("
                  (str/join ", " (sort (map name props))) ")."
                  " Write type-specific properties from a rule on " tl " itself.")
             {:op op :interface iface :object-type ot :stray (vec stray)}))
    (when-not pk-writable?
      (when-let [pk (s/call cx :pk-attribute W ot)]
        (when (contains? vals pk)
          (fail! :pk-immutable
                 (str op ": " (name pk) " is the primary key of " tl
                      " and a primary key can never be modified. It is an ordinary"
                      " property on other implementers of " il
                      ", which is why this fails per type rather than at authoring.")
                 {:op op :interface iface :object-type ot :pk pk}))))))

(defn of-interface [kind {:keys [interface object-type values target] :as step} run-state cx]
  (let [{W :world B :bindings} run-state
        iface (v/resolve-arg B interface)
        op    (str (name kind) "-of-interface")
        ot    (if (= kind :create)
                (v/resolve-arg B object-type)
                (or (when object-type (v/resolve-arg B object-type))
                    (nth (update-target B target) 2)))
        vals  (when (= kind :create) (v/resolve-arg B values))
        step  (-> step (dissoc :interface) (assoc :object-type ot))]
    (implementing! cx W iface ot op)
    (case kind
      :create (do (interface-values! cx W iface ot vals op true)
                  (when-let [pk (s/call cx :pk-attribute W ot)]
                    (when-not (contains? vals pk)
                      (fail! :pk-missing
                             (str op ": " (name (label W :object-types ot)) " is keyed by "
                                  (name pk)
                                  ", and this rule does not set it — an object cannot be created"
                                  " without its primary key. Declare " (name pk) " on "
                                  (name (label W :interfaces iface))
                                  " so every implementer can be created through this action.")
                             {:op op :interface iface :object-type ot :pk pk})))
                  (create-object step run-state cx))
      :update (do (interface-values! cx W iface ot (v/resolve-arg B values) op false)
                  (update-object step run-state cx))
      :delete (delete-object step run-state cx))))

(defn- realisations!
  [W iface c T op one?]
  (let [d {:op op :interface iface :constraint c :object-type T}]
    (when-not c (fail! :no-constraint (str op ": no :constraint given") d))
    (let [sides (sides/constraint-sides W T iface c)
          il    (name (label W :interfaces iface))
          tl    (name (label W :object-types T))]
      (when (empty? sides)
        (fail! :constraint-unrealised
               (str op ": " tl " realises no link for " (name c) " of " il
                    " — declare a link type on " tl " that :satisfies {:interface " il
                    " :constraint " (name c) "}.")
               d))
      (when (and one? (< 1 (count sides)))
        (fail! :constraint-ambiguous
               (str op ": " tl " realises " (name c) " of " il " " (count sides)
                    " times (" (str/join ", " (map #(str (name (label W :link-types (:link %)))
                                                         " " (name (:dir %)))
                                                   sides))
                    ") — one link cannot be chosen, so the rule writes nothing. Give the"
                    " constraint one realisation per type, or link the concrete type.")
               (assoc d :links (mapv :link sides))))
      sides)))

(defn link-of-interface
  [kind {:keys [interface constraint object-type from-id to-id target as id markings]} run-state cx]
  (let [{W :world B :bindings} run-state
        iface (v/resolve-arg B interface)
        op    (keyword (str (name kind) "-link-of-interface"))]
    (if (and (= kind :delete) target (not from-id))
      (delete-link {:target target} run-state cx)
      (let [src (v/resolve-arg B from-id)
            sid (if (v/handle? src) (:id src) src)
            T   (or (when object-type (v/resolve-arg B object-type))
                    (when (v/handle? src) (:object-type-ref src))
                    (fail! :no-object-type
                           (str (name op) ": cannot tell which type " (pr-str from-id)
                                " is — the source end must be an object or interface"
                                " parameter (pass it as " (pr-str [:get :param])
                                "), or the rule must name :object-type.")
                           {:op op :interface iface :constraint constraint}))
            _   (implementing! cx W iface T op)
            dst (v/resolve-arg B to-id)
            did (if (v/handle? dst) (:id dst) dst)
            ends (fn [{:keys [dir]}]
                   (if (= :to dir) {:from-id sid :to-id did} {:from-id did :to-id sid}))]
        (reduce (fn [run-state s]
                  (let [step (merge {:link-type (:link s)} (ends s))]
                    (if (= kind :create)
                      (create-link (cond-> step as (assoc :as as) id (assoc :id id)
                                           markings (assoc :markings markings))
                                   run-state cx)
                      (delete-link step run-state cx))))
                run-state
                (realisations! W iface constraint T op (= kind :create)))))))
