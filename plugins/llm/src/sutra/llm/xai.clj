(ns sutra.llm.xai)

(set! *warn-on-reflection* true)

(def ^:private price-divisor 10000.0)

(defn parse
  [body]
  (vec (keep (fn [m]
               (let [in  (some-> (:prompt_text_token_price m) (/ price-divisor))
                     out (some-> (:completion_text_token_price m) (/ price-divisor))]
                 (when (and (:id m) in out)
                   {:id (:id m) :in (double in) :out (double out) :currency "USD"})))
             (:models body))))

(def module
  {:name        :llm-xai
   :contributes {:llm/catalogues
                 {:xai {:host "x.ai" :models-path "language-models" :parse parse}}}})
