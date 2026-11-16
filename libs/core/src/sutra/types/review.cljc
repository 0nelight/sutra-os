(ns sutra.types.review
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.types.tree :as tree]))

(def default-rules
  [{:when [:not-editor] :by [:editor]}
   {:when [:expands-access] :by [:access]}
   {:when [:applies-organization] :by [:apply]}])

(defn- gates-held?
  [gates me owner?]
  (every? (fn [{:keys [holders]}]
            (if (seq holders) (boolean (some (comp me str) holders)) (boolean owner?)))
          gates))

(defn custom-rule
  [{:keys [reviewers count contributors?]}]
  (let [who (mapv (fn [{:keys [user group]}]
                    (if group [:group (str group)] [:principal (str user)]))
                  reviewers)]
    {:when  [:always]
     :by    (cond-> [(if (seq who) (into [:any-of] who) :editor)]
              (not contributors?) (conj :other))
     :count (or count 1)}))

(def access-fields
  #{:markings :allowed-markings :forbidden-markings})

(def cosmetic
  #{:description :display-name :plural-display-name :icon :color :help-text
    :title :documentation :visibility :group :groups :tags :aliases})

(declare grows?)

(defn- grows-by-key?
  [b a]
  (every? (fn [[k bv]]
            (and (contains? a k)
                 (cond (access-fields k) (= bv (get a k))
                       (cosmetic k)      true
                       :else             (grows? bv (get a k)))))
          b))

(defn grows?
  [b a]
  (cond
    (= b a)                    true
    (nil? b)                   true
    (and (map? b) (map? a))    (grows-by-key? b a)
    (and (set? b) (set? a))    (every? a b)
    (and (sequential? b) (sequential? a))
    (if-let [k (tree/element-key (concat b a))]
      (grows-by-key? (coll/index-by k b)
                     (coll/index-by k a))
      (every? (set a) b))
    :else                      false))

(defn change-kind
  [status before after]
  (case status
    "added"   :accretion
    "removed" :breaking
    (let [content #(cond-> % (map? %) (dissoc :revision))]
      (if (and (some? before)
               (or (= (content before) (content after))
                   (grows? (content before) (content after))))
        :accretion
        :breaking))))

(defn expands-access?
  [status before after]
  (boolean (and (= "modified" status)
                (seq (remove (set (:markings after)) (:markings before))))))

(defn custom-of
  [rules]
  (let [{:keys [by count] w :when :as r} (first rules)
        [who & more] by]
    (when (and r (= 1 (clojure.core/count rules)) (= [:always] (mapv keyword w))
               (or (= :editor who) (and (sequential? who) (= :any-of (first who))))
               (every? #{:other} more))
      {:reviewers     (if (sequential? who)
                        (mapv (fn [[k v]] (if (= :group k) {:group v} {:user v})) (rest who))
                        [])
       :count         (or count 1)
       :contributors? (empty? more)})))

(defn- owns? [ctx project]
  (or (nil? project) (boolean ((:author-owns? ctx (constantly true)) project))))

(defn- edits? [ctx project]
  (or (nil? project)
      (boolean ((or (:author-edits? ctx) (:author-owns? ctx) (constantly true)) project))))

(def predicates
  {:always    (fn [_ _] true)
   :breaking  (fn [t _] (= :breaking (:kind t)))
   :accretion (fn [t _] (= :accretion (:kind t)))
   :expands-access (fn [t _] (boolean (:expands-access t)))
   :applies-organization (fn [t ctx]
                           (boolean (and (seq (:apply-gates t))
                                         (not (gates-held? (:apply-gates t)
                                                           #{(str (:author ctx))} false)))))
   :foreign   (fn [t ctx] (not (owns? ctx (:project t))))
   :not-editor (fn [t ctx] (not (edits? ctx (:project t))))
   :section   (fn [t _ s] (= (name s) (some-> (:section t) name)))
   :project   (fn [t _ p] (= (name p) (some-> (:project t) name)))})

(declare call)

(def approvers
  {:owner     (fn [_ ap _] (boolean (:owner? ap)))
   :editor    (fn [_ ap _] (boolean (or (:editor? ap) (:owner? ap))))
   :other     (fn [t ap ctx]
                (not (contains? (into #{(str (:author ctx))} (map str) (:contributors t))
                                (str (:who ap)))))
   :principal (fn [_ ap _ p] (= (str (:who ap)) (name p)))
   :access    (fn [t ap _]
                (let [me    (into #{(str (:who ap))} (map str) (:groups ap))
                      gates (:access-gates t)]
                  (if (seq gates)
                    (every? (fn [{:keys [holders]}]
                              (if (seq holders)
                                (boolean (some (comp me str) holders))
                                (boolean (:owner? ap))))
                            gates)
                    (boolean (:owner? ap)))))
   :apply     (fn [t ap _]
                (gates-held? (:apply-gates t) (into #{(str (:who ap))} (map str) (:groups ap))
                             (:owner? ap)))
   :group     (fn [_ ap _ g] (contains? (set (map str (:groups ap))) (name g)))
   :any-of    (fn [t ap ctx & terms]
                (boolean (some #(call approvers % t ap ctx) terms)))})

(defn- call
  [registry term & xs]
  (let [[k & args] (if (sequential? term) term [term])
        f          (get registry (keyword k))]
    (when-not f
      (refusal/refuse! :refusal.types/unknown-review-term {:term term}
                       {:known (sort (keys registry))}))
    (apply f (concat xs args))))

(defn asks-contributors?
  [rules]
  (boolean (some #(some #{:other} (map keyword (filter (some-fn keyword? string?) (:by %))))
                 rules)))

(def needs-vocabulary
  #{:justification})

(defn problems
  [rules]
  (not-empty
   (for [[i r] (map-indexed vector (if (sequential? rules) rules [rules]))
         p     (cond
                 (not (map? r)) [(str "rule " i " is not a map")]
                 :else
                 (concat
                  (for [t (:when r) :let [k (if (sequential? t) (first t) t)]
                        :when (not (contains? predicates (keyword k)))]
                    (str "rule " i ": unknown predicate " (pr-str t)))
                  (when (and (empty? (:by r)) (empty? (:needs r)))
                    [(str "rule " i
                          ": names neither an approver (:by) nor what it needs (:needs)")])
                  (for [q (:needs r) :when (not (contains? needs-vocabulary (keyword q)))]
                    (str "rule " i ": unknown need " (pr-str q) " — known: "
                         (pr-str (sort needs-vocabulary))))
                  (for [t (:by r) :let [k (if (sequential? t) (first t) t)]
                        :when (not (contains? approvers (keyword k)))]
                    (str "rule " i ": unknown approver " (pr-str t)))
                  (when-not (pos-int? (:count r 1)) [(str "rule " i ": :count must be ≥ 1")])))]
     p)))

(defn requires
  [rules task ctx]
  (into []
        (comp (filter (fn [r] (every? #(call predicates % task ctx) (:when r))))
              (map #(assoc % :count (:count % 1))))
        rules))

(defn qualifies?
  [rule task approval ctx]
  (every? #(call approvers % task approval ctx) (:by rule)))

(defn eligible?
  [task approval ctx]
  (let [rs (:requires task)]
    (or (empty? rs) (boolean (some #(qualifies? % task approval ctx) rs)))))

(defn- justified? [ctx] (boolean (some-> (:justification ctx) str (.trim) seq)))

(defn unmet
  [task approvals ctx]
  (filterv (fn [r] (or (and (seq (:by r))
                            (< (count (filter #(qualifies? r task % ctx) approvals)) (:count r)))
                       (and (some #{:justification} (map keyword (:needs r)))
                            (not (justified? ctx)))))
           (:requires task)))

(defn describe
  [r]
  (str (when (seq (:by r)) (str (:count r 1) "× " (pr-str (vec (:by r)))))
       (when (seq (:needs r))
         (str (when (seq (:by r)) " and ") "a " (str/join ", " (map name (:needs r)))))
       " when " (pr-str (vec (:when r)))))
