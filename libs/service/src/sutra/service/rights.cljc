(ns sutra.service.rights
  (:require [sutra.world.db :as db]
            [sutra.world.policy :as p]
            [sutra.world.view :as view]))

(defn- facts [d] (db/view d :policy/facts))

(defn role-ops
  [roles]
  (into {}
        (for [r (keys roles)]
          [r (into #{} (mapcat #(get-in roles [% :operations]))
                   (view/reach #(some-> (get-in roles [% :includes]) vector) [r]))])))

(defn- dominance
  [classification]
  (for [[n {c :category k :rank}] classification
        [m {c' :category j :rank}] classification
        :when (and c k j (= c c') (not= n m) (>= (long k) (long j)))]
    [n m]))

(defn subject
  [d roles {:keys [who memberships organizations global-role unrestricted? disabled? clears
                   classification]}]
  (let [fv      (facts d)
        u       (str who)
        groups  (reduce (fn [m [a b]] (update m a (fnil conj #{}) b)) {} memberships)
        members (set (view/reach #(get groups %) [u]))
        held    (into (set clears)
                      (mapcat (fn [g] (map #(nth % 2) (p/at fv :clears 0 g))))
                      members)
        own-dom (reduce (fn [m [n x]] (update m n (fnil conj #{}) x)) {}
                        (dominance classification))
        covers  (into held (mapcat #(concat (p/dominated d %) (get own-dom %))) held)
        outside (reduce (fn [m [a o]] (update m (str a) (fnil conj #{}) o)) {} organizations)
        inside  (into #{} (mapcat (fn [m] (concat (map #(nth % 2) (p/at fv :org-member 0 m))
                                                  (get outside m))))
                      members)
        ops     (role-ops roles)]
    {:who          u
     :members      members
     :covers       covers
     :inside       inside
     :role-ops     ops
     :operations   (into #{} (mapcat :operations) (vals roles))
     :global-role  global-role
     :unrestricted? (boolean unrestricted?)
     :disabled?    (boolean disabled?)}))

(defn- disjunctive [fv m] (into #{} (map #(nth % 2)) (p/at fv :disjunctive 0 m)))

(defn blocked?
  [d {:keys [covers inside unrestricted? disabled?]} x]
  (let [fv (facts d)]
    (boolean
     (or (and disabled? (or (p/container? fv x) (seq (p/projects-of fv x))))
         (and (not unrestricted?)
              (let [ms (p/marked d x)]
                (or (some #(and (empty? (disjunctive fv %)) (not (covers %))) ms)
                    (some (fn [k] (not-any? #(and ((disjunctive fv %) k) (covers %)) ms))
                          (mapcat #(disjunctive fv %) ms))
                    (some (fn [g] (not-any? inside (p/walled fv g))) (p/requires d x)))))))))

(defn- granted?
  [fv {:keys [members role-ops]} op c]
  (some (fn [a]
          (some (fn [[_ who role]] (and (members who) (contains? (get role-ops role) op)))
                (p/at fv :grant 2 a)))
        (p/within fv c)))

(defn may?
  [d {:keys [role-ops operations global-role unrestricted? disabled?] :as s} op c]
  (let [fv (facts d)]
    (boolean
     (or (and (granted? fv s op c) (not (blocked? d s c)))
         (and global-role (contains? (get role-ops global-role) op) (p/container? fv c)
              (not (blocked? d s c)))
         (and unrestricted? (not disabled?) (operations op) (p/container? fv c))))))

(defn decide
  [d s {op :do c :on}]
  {:allow? (may? d s op c)})

(defn granted-operations
  [d {:keys [members role-ops] :as s} among]
  (let [fv (facts d)]
    (into {}
          (for [c among
                :let [ops (into #{} (for [a (p/within fv c)
                                          [_ who role] (p/at fv :grant 2 a)
                                          :when (members who)
                                          o (get role-ops role)]
                                      o))]
                :when (and (seq ops) (not (blocked? d s c)))]
            [c ops]))))

(defn holders
  [d roles op c]
  (let [fv  (facts d)
        ops (role-ops roles)]
    (into (sorted-set)
          (for [a (p/within fv c)
                [_ who role] (p/at fv :grant 2 a)
                :when (contains? (get ops role) op)]
            who))))
