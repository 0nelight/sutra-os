(ns sutra.peering.media
  (:require [sutra.blobs.api :as blobs]
            [sutra.kit.refusal :as refusal]
            [sutra.media.api :as media]
            [sutra.peering.objects :as objects]
            [sutra.peering.rows :as rows]
            [sutra.peering.wire :as wire]
            [sutra.store.access.scope :as scope]
            [sutra.types.authority :as policy]
            [sutra.types.peering :as peering])
  (:import (java.io InputStream)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def peer-path "/api/peering/peer")

(def batch-bytes
  (* 8 1024 1024))

(defn- kw [x] (when (some? x) (keyword (name x))))

(defn- blob-store [] (scope/blobs))

(defn- at [store br] (assoc store :branch br))

(defn set-markings
  [schema nm]
  (when-let [[_ ms] (media/declared schema nm)]
    (into (set (map keyword (:markings ms)))
          (when-let [p (:project ms)] (objects/type-markings schema p)))))

(defn may-flow?
  [c m way]
  (let [ok? (if (= :out way) peering/exports? peering/imports?)]
    (and (peering/flowing? c) (ok? (get-in c [:data-types :media])) (ok? (:direction m)))))

(defn map-set!
  [store actor id {:keys [local-set remote-set direction]}]
  (let [c   (rows/by-name-or-id store id)
        dir (kw direction)]
    (when-not (media/declared (objects/schema store) local-set)
      (refusal/refuse! :refusal.peering/no-media-set {:media-set local-set}))
    (when-not (#{:export :import} dir)
      (refusal/refuse! :refusal.peering/media-direction {:direction direction}))
    (when-not ((if (= :export dir) peering/exports? peering/imports?)
               (get-in c [:data-types :media]))
      (refusal/refuse! :refusal.peering/media-direction-not-allowed
                       {:connection-id (:connection-id c)}
                       {:dir (name dir) :media-types (get-in c [:data-types :media])}))
    (let [old (first (filter #(= (str local-set) (:local-set %))
                             (rows/media-mappings store (:connection-id c))))]
      (rows/put-media-mapping! store {:connection-id (:connection-id c)
                                      :local-set (str local-set)
                                      :remote-set (str (or remote-set local-set))
                                      :direction dir :sent (:sent old {})
                                      :created-by actor}))))

(defn- bytes-of ^bytes [address]
  (when-let [^InputStream in (blobs/open (blob-store) address)]
    (with-open [in in] (.readAllBytes in))))

(defn- batches
  [items]
  (reduce (fn [bs it]
            (let [cur (peek bs) size (reduce + 0 (map :size cur))]
              (if (and (seq cur) (> (+ size (long (:size it 0))) batch-bytes))
                (conj bs [it])
                (conj (pop bs) (conj cur it)))))
          [[]] items))

(defn send!
  [store c m]
  (let [schema (objects/schema store)
        marks  (peering/wire-markings (set-markings schema (:local-set m))
                                      (policy/classification-of schema) (:marking-map c))
        sent   (or (:sent m) {})
        fresh  (vec (remove #(= (:address %) (get sent (:path %)))
                            (media/items (at store objects/branch) (:local-set m))))
        job    {:connection-id (:connection-id c) :data-type :media :way :out
                :local-type (:local-set m)}]
    (when (seq fresh)
      (if-not (peering/admitted? (:ceiling c) marks)
        (rows/job! store (assoc job :state :ok :held (count fresh)))
        (try
          (doseq [batch (batches (vec (for [it fresh :let [bs (bytes-of (:address it))] :when bs]
                                        (assoc (select-keys it [:path :address :mime :filename])
                                               :size (alength bs)
                                               :bytes (.encodeToString (Base64/getEncoder)
                                                                       bs)))))
                  :when (seq batch)]
            (wire/deliver! store (:trust c) peer-path
                           {:op :batch :connection-id (:connection-id c) :data-type :media
                            :type (:local-set m) :markings marks :items batch}))
          (rows/put-media-mapping! store (assoc m :sent (into sent (map (juxt :path :address))
                                                              fresh)))
          (rows/job! store (assoc job :state :ok :items (count fresh)))
          (catch clojure.lang.ExceptionInfo e
            (rows/job! store (assoc job :state :failed :error (ex-message e)))))))))

(defn receive!
  [store c {:keys [type items markings]}]
  (let [m (rows/media-mapping-by-remote store (:connection-id c) type)]
    (when-not (and m (may-flow? c m :in))
      (refusal/refuse! :refusal.peering/media-not-taken
                       {:connection-id (:connection-id c) :type type}))
    (let [schema  (objects/schema store)
          local   (into #{} (for [{nm :name} markings]
                              (keyword (get (:marking-map c) nm nm))))
          carried (set-markings schema (:local-set m))
          held    (cond (not (peering/admitted? (:ceiling c) markings)) "above the ceiling"
                        (not-every? carried local)
                        (str "the set " (:local-set m) " does not carry "
                             (vec (sort (map name (remove carried local))))))
          job     {:connection-id (:connection-id c) :data-type :media :way :in
                   :local-type (:local-set m)}]
      (if held
        (do (rows/job! store (assoc job :state :ok :held (count items)
                                    :error (str (count items) " held: " held)))
            {:applied 0 :held (count items)})
        (let [landed (vec (for [{:keys [path address mime filename] b64 :bytes} items
                                :let [bs (.decode (Base64/getDecoder) (str b64))]]
                            (do
                              (when-not (= address (blobs/address-of bs))
                                (refusal/refuse! :refusal.peering/item-hash-mismatch {:path path}))
                              (when-let [p (media/land-problem schema (:local-set m) mime)]
                                (media/land-refusal! p {}))
                              (blobs/put! (blob-store) bs)
                              (media/record! (at store (objects/home (:connection-id c)))
                                             {:media-set (:local-set m) :path path
                                              :address address :size (alength bs) :mime mime
                                              :filename filename
                                              :landed-by (str "peer:" (:connection-id c))})
                              path)))]
          (rows/job! store (assoc job :state :ok :items (count landed)))
          {:applied (count landed) :held 0})))))

(defn round!
  [store c]
  (doseq [m (rows/media-mappings store (:connection-id c))
          :when (may-flow? c m :out)]
    (send! store c m)))
