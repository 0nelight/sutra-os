(ns sutra-ui.apphost.form-machine
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.types.submission :as csub]))

(defn- reseed
  [s seed object-id]
  (let [{old :seed vals :vals oid :object-id} s]
    (if (and (map? old) (= oid object-id))
      (reduce-kv (fn [m k v] (if (= v (get old k)) m (assoc m k v)))
                 (or vals {}) seed)
      seed)))

(defn- release-derived
  [specs base p vals]
  (let [current (some (fn [[q s]] (when (= :current-object (->kw (:prefill s))) q)) specs)]
    (reduce-kv (fn [m q spec]
                 (let [d (:default spec)]
                   (if (and (map? d) (:property d) (not= q p)
                            (= p (or (some-> (:from d) ->kw) current))
                            (= (get m q) (get base q)))
                     (dissoc m q)
                     m)))
               vals specs)))

(defn- counter [s k] (inc (or (get s k) 0)))

(defn seeded
  [s {:keys [seed object-id applied evaluate?]}]
  (when-not (and (some? s) (= seed (:seed s)))
    (let [same? (= object-id (:object-id s))]
      {:db (merge (select-keys s [:eval-seq :submit-seq])
                  (when same?
                    (select-keys s [:eff :filled :filled-objs :opts :submitting :eval-failed]))
                  {:seed         seed
                   :vals         (reseed s seed object-id)
                   :object-id    object-id
                   :busy         (boolean (and same? (:submitting s)))
                   :err          nil
                   :applied      applied
                   :base         seed
                   :base-pending (when evaluate? seed)})
       :fx [[:action-form/schedule 0]]})))

(defn fired
  [s]
  (let [{:keys [vals base-pending]} s
        n    (counter s :eval-seq)
        one? (and base-pending (= vals base-pending))
        own? (and base-pending (not one?))]
    {:db (cond-> (-> s (dissoc :base-pending) (assoc :eval-seq n))
           own? (assoc :base base-pending))
     :fx (cond-> []
           own? (conj [:action-form/baseline base-pending])
           true (conj [:action-form/evaluate {:seq n :vals vals :search (:search s)
                                              :pend (when one? base-pending)}]))}))

(defn evaluated
  [s {n :seq pend :pend} {:keys [params values objects options blocked? reason] :as resp}]
  (let [s (cond-> s (and pend (= pend (:seed s))) (assoc :base (merge values pend)))]
    (cond
      (not= n (:eval-seq s)) {:db s}
      (nil? resp)            {:db (assoc s :eval-failed true)}
      :else
      (let [drawn (into #{} (keep (fn [[p spec]]
                                    (when-not (= :hidden (->kw (:exposure spec))) p)))
                        params)
            prev  (:drawn s)]
        {:db (assoc s
                    :eff params
                    :filled values
                    :filled-objs objects
                    :opts options
                    :drawn drawn
                    :revealed (when prev (not-empty (into #{} (remove prev) drawn)))
                    :gate {:blocked? blocked? :reason reason}
                    :eval-failed false)}))))

(defn submitted
  [s id defaults]
  (when (= id (:submitting s))
    {:db (merge (select-keys s [:object-id :eff :opts :eval-failed])
                {:seed nil :vals defaults :busy false :err nil
                 :eval-seq (counter s :eval-seq) :submit-seq (:submit-seq s)})
     :fx [[:action-form/cancel-timer]]}))

(defn settled
  [s id err]
  (when (= id (:submitting s))
    {:db (-> s (dissoc :submitting) (assoc :busy false) (cond-> err (assoc :err err)))}))

(defn applied
  [s n]
  (when-not (= n (:applied s))
    {:db (assoc s :applied n) :fx [[:action-form/schedule 0]]}))

(defn put
  [s p v specs]
  {:db (-> s
           (assoc-in [:vals p] v)
           (update :vals #(release-derived specs (:base s) p %))
           (assoc :touched true))
   :fx [[:action-form/schedule 250]]})

(defn searched [s p term] {:db (assoc-in s [:search p] term) :fx [[:action-form/schedule 250]]})

(defn baselined
  [s seed values]
  (when (= seed (:seed s)) {:db (assoc s :base (merge values seed))}))

(defn submit
  [s]
  (when-not (:submitting s)
    (let [n (counter s :submit-seq)]
      {:db (assoc s :busy true :err nil :submit-seq n :submitting n)
       :fx [[:action-form/send {:id n :vals (:vals s)}]]})))

(defn shown-params
  [s specs evaluate?]
  (let [eff      (:eff s)
        waiting? (and evaluate? (nil? eff) (not (:eval-failed s)))]
    (into #{}
          (keep (fn [[p spec]]
                  (let [e (if eff (get eff p spec) spec)]
                    (when-not (or (= :hidden (->kw (:exposure e)))
                                  (and waiting? (:overrides spec)))
                      p))))
          specs)))

(defn effective-specs
  [{:keys [eff]} specs]
  (if eff
    (merge specs
           (into {} (map (fn [[p e]]
                           (let [f (get-in specs [p :format])]
                             [p (cond-> e (and f (map? e) (not (:format e)))
                                  (assoc :format f))])))
                 (select-keys eff (keys specs))))
    specs))

(defn- add-rows [rows extra]
  (let [have (into #{} (map #(str (:id %))) rows)]
    (into (vec rows) (remove #(have (str (:id %)))) extra)))

(defn options
  [{:keys [opts search filled-objs]} own default-objects]
  (let [srv  (update-vals (or opts {}) #(vec (:rows %)))
        own  (into {}
                   (keep (fn [[p rows]]
                           (when (some? rows)
                             [p (cond-> rows
                                  (not (str/blank? (str (get search p))))
                                  (add-rows (get srv p)))])))
                   (or own {}))
        base (merge srv own)]
    (merge-with add-rows base
                (merge-with add-rows filled-objs (select-keys default-objects (keys base))))))

(defn truncated
  [{:keys [opts search]}]
  (-> (into #{} (keep (fn [[p o]] (when (:truncated o) p))) (or opts {}))
      (into (keep (fn [[p t]] (when-not (str/blank? (str t)) p))) search)))

(defn shown-sections
  [s specs sections evaluate?]
  (let [shown-ps (shown-params s specs evaluate?)
        drawn?   (fn [p] (or (contains? shown-ps p) (not (contains? specs p))))]
    (->> (or (seq sections) (csub/form-sections nil specs))
         (keep (fn [sec]
                 (let [ps (filterv drawn? (:params sec))]
                   (when (seq ps) (assoc sec :params ps))))))))

(defn view-model
  [s {:keys [specs sections own-options default-objects evaluate?]}]
  (let [specs  (effective-specs s specs)
        shown  (shown-sections s specs sections evaluate?)]
    {:specs     specs
     :options   (options s own-options default-objects)
     :truncated (truncated s)
     :sections  shown
     :gate      (or (:gate s) (csub/submit-gate specs (:vals s)))
     :reveal    (when-let [rv (:revealed s)] (first (filter rv (mapcat :params shown))))}))
