(ns sutra.catalog.llm-models)

(set! *warn-on-reflection* true)

(defn language-model-input?
  [in]
  (= :language-model (:kind in)))

(defn- models-of
  [schema]
  (into {} (map (fn [[k v]] [(if (keyword? k) (subs (str k) 1) (str k)) v]))
        (or (:llm-models schema) {})))

(defn registered?
  [schema model]
  (contains? (models-of schema) (str model)))

(defn rate-of
  [schema model]
  (get (models-of schema) (str model)))

(defn source-of
  [schema model]
  (some-> (:source (rate-of schema model)) name keyword))

(defn provider-model
  [schema model]
  (or (some-> (:provider-model (rate-of schema model)) str not-empty) (some-> model str)))

(defn with-source
  [schema in]
  (if-let [s (and (language-model-input? in) (not (:source in))
                  (source-of schema (:model in)))]
    (assoc in :source s)
    in))

(defn resolved
  [schema t]
  (if-not (map? (:inputs t))
    t
    (update t :inputs update-vals
            (fn [in]
              (if (and (map? in) (language-model-input? in))
                (let [pm (some-> (:provider-model (rate-of schema (:model in))) str not-empty)]
                  (cond-> (with-source schema in) pm (assoc :provider-model pm)))
                in)))))

(def ^:private cached-share
  0.1)

(def ^:private cache-write-share 1.25)

(defn cost
  ([schema model prompt-tokens completion-tokens]
   (cost schema model prompt-tokens completion-tokens nil))
  ([schema model prompt-tokens completion-tokens {:keys [cached written]}]
   (when-let [{:keys [in out cached-in]} (rate-of schema model)]
     (let [cached  (double (or cached 0))
           written (double (or written 0))
           fresh   (max 0.0 (- (double (or prompt-tokens 0)) cached written))]
       (/ (+ (* fresh (double in))
             (* cached (double (or cached-in (* cached-share in))))
             (* written cache-write-share (double in))
             (* (double (or completion-tokens 0)) (double out)))
          1000000.0)))))

(defn valid-rate?
  [r]
  (and (map? r)
       (number? (:in r))  (not (neg? (:in r)))
       (number? (:out r)) (not (neg? (:out r)))
       (or (nil? (:currency r)) (string? (:currency r)))))
