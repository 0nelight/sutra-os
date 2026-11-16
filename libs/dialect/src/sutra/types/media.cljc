(ns sutra.types.media
  (:require [clojure.string :as str]
            [sutra.types.value-types :as vt]))

(def max-upload-bytes
  (* 64 1024 1024))

(def address-pattern
  #"[0-9a-f]{64}")

(defn address?
  [v]
  (boolean (and (string? v) (re-matches address-pattern v))))

(defn upload?
  [v]
  (boolean (and (map? v) (string? (:bytes-b64 v)) (seq (:bytes-b64 v)))))

(defn media-params
  [action-spec]
  (into #{}
        (keep (fn [[p spec]] (when (vt/media? (:type spec)) p)))
        (:params action-spec)))

(defn b64-bytes-estimate
  [b64]
  (let [s   (str/replace (str b64) #"\s" "")
        pad (count (take-while #{\=} (reverse s)))]
    (max 0 (- (quot (* 3 (count s)) 4) pad))))

(defn attribute-names
  [attr-types]
  (into #{} (keep (fn [[nm ty]] (when (vt/media? ty) nm))) attr-types))

(defn row-references
  [attr-names row]
  (let [vals (or (:values row) row)]
    (persistent!
     (reduce (fn [acc nm]
               (let [v (get vals nm)]
                 (cond
                   (nil? v)      acc
                   (address? v)  (conj! acc v)
                   (sequential? v) (reduce (fn [a x] (if (address? x) (conj! a x) a)) acc v)
                   :else         acc)))
             (transient #{})
             attr-names))))

(defn classify
  [action-spec args]
  (into {}
        (map (fn [p]
               (let [v (get args p)]
                 [p (cond
                      (nil? v)                  :absent
                      (and (string? v)
                           (str/blank? v))      :absent
                      (address? v)              :reference
                      (upload? v)               :upload
                      :else                     :invalid)])))
        (media-params action-spec)))

(defn uploads
  [action-spec args]
  (into []
        (comp (filter (fn [[_ k]] (= :upload k)))
              (map (fn [[p _]] [p (get args p)])))
        (sort-by key (classify action-spec args))))

(defn problem
  [action-spec args max-bytes]
  (let [kinds (classify action-spec args)]
    (or (when-let [[p _] (first (filter (fn [[_ k]] (= :invalid k)) kinds))]
          {:parameter p
           :reason    :not-a-media-value
           :message   (str "Parameter " p " hält eine Medienreferenz: entweder eine"
                           " gelandete Adresse (64 Hex) oder {:bytes-b64 … :filename …"
                           " :mime …} — nicht beides und nichts anderes.")})
        (when-let [[p _] (first (filter (fn [[p k]]
                                          (and (= :absent k)
                                               (get-in action-spec [:params p :required])))
                                        kinds))]
          {:parameter p
           :reason    :required
           :message   (str "Parameter " p " ist Pflicht und leer.")})
        (when max-bytes
          (when-let [[p env] (first (filter (fn [[_ env]]
                                              (> (b64-bytes-estimate (:bytes-b64 env)) max-bytes))
                                            (uploads action-spec args)))]
            {:parameter p
             :reason    :too-large
             :bytes     (b64-bytes-estimate (:bytes-b64 env))
             :max       max-bytes
             :message   (str "Parameter " p ": Datei zu groß.")})))))

(defn set-of
  [attr]
  (or (:media-set attr) (:media-set (:opts attr))))

(defn media-attributes
  [attr-rows]
  (into [] (filter #(vt/media? (:attribute-type %))) attr-rows))

(defn retention-days
  [media-set]
  (let [d (:days (:retention media-set))]
    (when (and (number? d) (pos? d)) (long d))))

(defn expired?
  [media-set landed-at-ms now-ms]
  (boolean (when-let [d (retention-days media-set)]
             (and (number? landed-at-ms)
                  (> (- now-ms (long landed-at-ms)) (* 86400000 d))))))

(defn format-problem
  [media-set mime]
  (let [fs (:formats media-set)]
    (when (seq fs)
      (let [m (str/lower-case (str mime))
            ok? (some (fn [f] (str/includes? m (str/lower-case (name f)))) fs)]
        (when-not ok?
          {:reason  :format-not-held
           :mime    (str mime)
           :formats (mapv name fs)
           :message (str "Dieses Media-Set hält " (str/join ", " (map name fs))
                         " — " (pr-str (str mime)) " gehört nicht dazu.")})))))
