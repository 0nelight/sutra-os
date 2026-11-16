(ns support.helpers
  (:require [clojure.test :as t]
            [sutra.catalog.problems :refer [empty-state]]
            [sutra.ref.install :as ri]))

(defn check
  [label ok?]
  (t/is ok? label))

(defn expect-error [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (.getMessage e))
       (catch Exception e (.getMessage e))))

(defn section [title]
  (println (str "\n  --- " title " ---")))

(def uid #(java.util.UUID/randomUUID))

(defn simple-schema
  [& {:keys [extra-types link-types extra-actions access-types]
      :or {link-types {}}}]
  (let [base-types {:person {:object-type-name :person
                             :attribute-list [{:attribute-name :name  :attribute-type :string}
                                              {:attribute-name :alter :attribute-type :number}
                                              {:attribute-name :aktiv :attribute-type :boolean}]}}
        ots (merge base-types extra-types)
        base-actions (vec (for [t (keys ots) op [:erstellen :aendern :loeschen :berechne
                                                 :verbinde-wenn :wiederhole-bis :wenn :workflow]]
                            {:operation op :target {:target-object-type t}}))
        conn-actions (vec (for [ct (keys link-types) op [:erstellen :loeschen]]
                            {:operation op :target {:target-link-type ct}}))
        ats (vec (concat base-actions conn-actions extra-actions))]
    (ri/ensure-actions
     (cond-> {:object-types ots
              :link-types link-types
              :action-types ats}
       access-types (assoc :access-types access-types)))))

(defn make-obj [type-kw id values]
  {:id id :object-type-ref type-kw :values values})

(defn make-state [& objects]
  (reduce (fn [s [type-kw id values]]
            (assoc-in s [:objects id] (make-obj type-kw id values)))
          empty-state
          objects))
