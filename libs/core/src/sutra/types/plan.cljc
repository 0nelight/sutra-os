(ns sutra.types.plan
  (:require [sutra.kit.lifecycle :as lc]
            [sutra.types.finding :as finding]))

(defn plan
  ([] (plan [] []))
  ([ops] (plan ops []))
  ([ops findings] {:ops (vec ops) :findings (vec findings)}))

(defn with-ops [p ops] (update p :ops (fnil into []) ops))

(defn with-findings [p findings] (update p :findings (fnil into []) findings))

(defn combine
  [& ps]
  (reduce (fn [acc p] (-> acc (with-ops (:ops p)) (with-findings (:findings p)))) (plan) ps))

(defn of-findings
  [findings]
  (plan [] findings))

(defn problem
  ([rule message] (problem rule message nil))
  ([rule message more]
   (finding/finding rule :error nil nil (assoc more :message (str message)))))

(defn problems
  [rule ps]
  (mapv (fn [p]
          (cond
            (and (map? p) (:rule p))    (finding/ranked p :error)
            (and (map? p) (:problem p)) (problem (keyword (name (:problem p)))
                                                 (or (:message p) (pr-str p)) p)
            :else                       (problem rule (str p))))
        ps))

(defn blocking [p] (filterv finding/blocking? (:findings p)))

(defn blocked? [p] (boolean (some finding/blocking? (:findings p))))

(defn refusal
  ([p] (refusal p nil))
  ([p opts] (finding/findings->refusal (:findings p) opts)))

(defn check!
  ([p] (check! p nil))
  ([p opts]
   (when-let [r (refusal p opts)] (throw r))
   p))

(defn preview
  [p]
  {:ops      (:ops p)
   :findings (finding/worst-first (:findings p))
   :blocked? (blocked? p)})

(defn apply!
  ([p commit] (apply! p commit nil))
  ([p commit opts]
   (check! p opts)
   (commit (:ops p))))

(defn transition
  [{:keys [edges] :as table} id event from record input op]
  (if-let [to (lc/next-state table event from record input)]
    (plan [(op to)])
    (let [why (lc/why-not table event from record input)
          r   (lc/refusal (:name table) id event from why)]
      (of-findings [(finding/finding (or (when (lc/allowed? table event from)
                                           (get-in edges [event :refusal]))
                                         :refusal.lifecycle/transition-refused)
                                     :error (:name table) id
                                     {:message (ex-message r)
                                      :event   event
                                      :from    (or from :absent)
                                      :reason  why})]))))
