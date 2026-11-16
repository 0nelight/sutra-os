(ns sutra.ref.err)

(def ^:private categories
  {:ambiguous              :unprocessable
   :collect-blocked        :unprocessable
   :constraint-ambiguous   :unprocessable
   :constraint-unrealised  :unprocessable
   :criterion-failed       :unprocessable
   :edit-conflict-timestamp :unprocessable
   :link-end-missing       :unprocessable
   :missing-required       :unprocessable
   :no-object-type         :unprocessable
   :no-link-target         :unprocessable
   :plan-problems          :unprocessable
   :propose-refused        :unprocessable
   :rule-target-unresolved :unprocessable
   :side-effect-edit       :unprocessable
   :unresolved-param       :unprocessable
   :key-taken              :conflict
   :name-taken             :conflict
   :protected-retire       :conflict
   :stale-change           :conflict
   :writeback-failed       :upstream
   :sigma-missing          :fault
   :sigma-unknown          :fault})

(defn fail!
  ([kind msg] (fail! kind msg {}))
  ([kind msg data]
   (throw (ex-info msg (assoc data :ref/kind kind
                              :sutra.kit.anomaly/category (categories kind :incorrect))))))

(defn kind [e]
  (:ref/kind (ex-data e)))
