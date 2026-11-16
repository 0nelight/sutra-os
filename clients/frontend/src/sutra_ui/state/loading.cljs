(ns sutra-ui.state.loading
  (:require [sutra-ui.api :as api]))

(defn problem
  [e]
  (when-not (api/aborted-error? e)
    (if (string? e)
      e
      (or (not-empty (str (get-in (ex-data e) [:body :error])))
          (.-message e)
          (str e)))))

(defn fail!
  ([ui e] (fail! ui e nil))
  ([ui e settled]
   (let [p (problem e)]
     (swap! ui (fn [m] (cond-> (merge m settled) p (assoc :error p))))
     nil)))

(defn say!
  [holder e]
  (when-let [p (problem e)] (reset! holder p))
  nil)

(defn or-failure
  [f & args]
  (.catch (apply f args)
          (fn [e]
            (when (api/aborted-error? e) (throw e))
            (let [{:keys [status body]} (ex-data e)]
              {::failure {:status status :body body :sentence (problem e)}}))))

(defn failure
  [answer]
  (when (map? answer) (::failure answer)))
