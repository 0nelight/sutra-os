(ns sutra.kit.datalog)

(defn lvar? [x] (and (symbol? x) (= \? (first (name x)))))

(defn- unify
  [binding pattern tuple]
  (when (= (count pattern) (count tuple))
    (reduce (fn [b [p v]]
              (cond
                (not (lvar? p))    (if (= p v) b (reduced nil))
                (contains? b p)    (if (= (b p) v) b (reduced nil))
                :else              (assoc b p v)))
            binding
            (map vector pattern tuple))))

(defn- ground [binding terms]
  (mapv #(if (lvar? %) (get binding % %) %) terms))

(defn- negation? [lit] (= :not (first lit)))

(defn- solve
  [db body]
  (reduce (fn [solutions lit]
            (if (negation? lit)
              (let [[rel & pat] (second lit)]
                (filterv (fn [[b _]]
                           (not-any? #(unify b pat %) (get db rel)))
                         solutions))
              (let [[rel & pat] lit]
                (into []
                      (for [[b used] solutions
                            t        (get db rel)
                            :let     [b' (unify b pat t)]
                            :when    b']
                        [b' (conj used [rel t])])))))
          [[{} []]]
          body))

(defn- step
  [db rules]
  (reduce (fn [[db new?] {:keys [id head body]}]
            (let [[rel & terms] head]
              (reduce (fn [[db new?] [b used]]
                        (let [t (ground b terms)]
                          (if (contains? (get db rel) t)
                            [db new?]
                            [(-> db
                                 (update rel (fnil conj #{}) t)
                                 (assoc-in [:why [rel t]] {:rule id :from used}))
                             true])))
                      [db new?]
                      (solve db body))))
          [db false]
          rules))

(defn run
  [facts strata]
  (reduce (fn [db rules]
            (loop [db db]
              (let [[db' new?] (step db rules)]
                (if new? (recur db') db'))))
          (update facts :why #(or % {}))
          strata))

(defn holds? [db rel tuple] (contains? (get db rel) tuple))

(defn query
  [db [rel & pat]]
  (keep #(unify {} pat %) (get db rel)))

(defn explain
  ([db fact] (explain db fact #{}))
  ([db [rel tuple :as fact] seen]
   (when (holds? db rel tuple)
     (if-let [{:keys [rule from]} (get-in db [:why fact])]
       {:fact fact :rule rule
        :from (if (seen fact) [] (mapv #(explain db % (conj seen fact)) from))}
       {:fact fact}))))
