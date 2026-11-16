(ns sutra.types.lifecycle
  (:require [sutra.types.refs :as refs]
            [clojure.string :as str]))

(defn state-value
  [s]
  (cond (keyword? s) (name s)
        (nil? s)     nil
        :else        (str s)))

(defn state-values
  [lc]
  (mapv state-value (:states lc)))

(defn transition-for
  [types action-name]
  (let [an (some-> action-name name keyword)]
    (first (for [[t ot] types
                 :let  [lc (:lifecycle ot)]
                 tr    (:transitions lc)
                 :when (= an (some-> (:action tr) name keyword))]
             {:object-type (keyword (name t))
              :property    (keyword (name (:property lc)))
              :from        (mapv state-value (:from tr))
              :to          (state-value (:to tr))}))))

(defn object-param
  [action type-pred]
  (let [ps (for [[p spec] (:params action)
                 :when (and (not (:multiple spec)) (type-pred (:type spec)))]
             p)]
    (when (= 1 (count ps)) (first ps))))

(defn- writes-property?
  [rules type-pred prop]
  (some (fn [r]
          (and (type-pred (:object-type r))
               (or (contains? (:values r) prop)
                   (some #{prop} (:set r)))))
        (refs/steps rules)))

(defn expand-action
  [action tr type-pred]
  (if-let [p (object-param action type-pred)]
    (let [{:keys [property from to]} tr
          crit {:test    {:stand [:get p property]}
                :pred    (into [:in :stand] from)
                :message (str "Nur möglich im Stand " (str/join ", " from) ".")}
          move {:op          :update
                :target      [:get p :id]
                :object-type (:object-type tr)
                :values      {property to}}]
      (cond-> (update action :submission-criteria (fnil conj []) crit)
        (not (writes-property? (:rules action) type-pred property))
        (update :rules (fnil conj []) move)))
    action))

(defn problems
  [objects actions attr-names]
  (vec
   (for [[t ot] objects
         :let  [lc (:lifecycle ot)]
         :when lc
         :let  [states (set (state-values lc))
                prop   (:property lc)
                t      (keyword (name t))]
         msg   (concat
                (when-not (contains? (set (attr-names t)) prop)
                  [(str "the lifecycle property " prop " is not an attribute of " t)])
                (when (not= (count states) (count (:states lc)))
                  ["a state is listed twice"])
                (for [tr (:transitions lc)
                      :let [an  (keyword (name (:action tr)))
                            act (get actions an)
                            bad (remove states (map state-value (cons (:to tr) (:from tr))))]
                      m (concat
                         (when-not act [(str "transition " an " names no Action")])
                         (when (seq bad)
                           [(str "transition " an " names unknown states " (vec bad))])
                         (when (and act (nil? (object-param act #(= t (some-> % name keyword)))))
                           [(str "Action " an " needs exactly one parameter of type " t
                                 " — the object whose state it moves")]))]
                  m))]
     {:object-type t :message msg})))
