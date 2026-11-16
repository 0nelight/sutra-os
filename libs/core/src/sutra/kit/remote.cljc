(ns sutra.kit.remote)

(def loading ::loading)

(def absent ::absent)

(defn failed
  [reason]
  {::failed (if (some? reason) reason "unbekannter Fehler")})

(defn loading? [rd] (= ::loading rd))

(defn absent? [rd] (= ::absent rd))

(defn failed? [rd] (and (map? rd) (contains? rd ::failed)))

(defn reason
  [rd]
  (when (failed? rd) (::failed rd)))

(defn state
  [rd]
  (cond
    (nil? rd)      (throw (ex-info (str "remote data is nil — say ::loading, ::absent or give"
                                        " the value")
                                   {:remote nil}))
    (loading? rd)  :loading
    (absent? rd)   :absent
    (failed? rd)   :failed
    :else          :value))

(defn value? [rd] (and (some? rd) (= :value (state rd))))

(defn fold
  [rd {:keys [loading absent failed value]}]
  (case (state rd)
    :loading (when loading (loading))
    :absent  (when absent (absent))
    :failed  (when failed (failed (reason rd)))
    :value   (when value (value rd))))

(defn show
  [rd branches]
  (fold rd (merge {:loading (constantly {:show :loading})
                   :absent  (constantly {:show :failed :reason nil})
                   :failed  (fn [r] {:show :failed :reason r})}
                  branches)))

(defn fmap
  [f rd]
  (if (value? rd) (f rd) rd))

(defn value-or
  [rd default]
  (if (value? rd) rd default))

(defn all
  [rds]
  (let [rds (vec rds)]
    (doseq [rd rds] (state rd))
    (or (some #(when (failed? %) %) rds)
        (when (some loading? rds) loading)
        (when (some absent? rds) absent)
        rds)))

(defn of-answer
  [v]
  (if (nil? v) absent v))

(defn list-view
  [rd]
  (show rd {:absent (constantly {:show :empty})
            :value  (fn [v] (if (seq v) {:show :list :rows (vec v)} {:show :empty}))}))

(defn after-failure
  [prev reason]
  (if (value? prev) prev (failed reason)))
