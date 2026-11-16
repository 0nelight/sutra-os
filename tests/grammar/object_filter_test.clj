(ns grammar.object-filter-test
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.test :refer [deftest is testing]]
            [sutra.capps.object-filter :as f]))

(def ^:private golden-file "tests/grammar/object_filter_golden.edn")

(def terms
  {:is-one        {:op :is :prop :s :values ["alpha"]}
   :is-two        {:op :is :prop :s :values ["alpha" "beta"]}
   :is-nil        {:op :is :prop :s :values [nil]}
   :is-open       {:op :is :prop :s :values []}
   :null          {:op :null :prop :s}
   :contains      {:op :contains :prop :s :value "LP"}
   :contains-open {:op :contains :prop :s :value ""}
   :range         {:op :range :prop :n :min 2 :max 5}
   :range-min     {:op :range :prop :n :min 2}
   :range-open    {:op :range :prop :n}
   :keyword       {:op :keyword :value "alp"}
   :keyword-field {:op :keyword :value "alp" :fields ["s"]}
   :keyword-or    {:op :keyword :value "alpha OR gamma" :advanced? true}
   :keyword-and   {:op :keyword :value "alpha beta" :advanced? true}
   :keyword-not   {:op :keyword :value "alpha NOT beta" :advanced? true}
   :keyword-open  {:op :keyword :value "  "}
   :typed         {:op :is :prop :s :values ["alpha"] :type "kunde"}
   :foreign-prop  {:op :is :prop :elsewhere :values ["x"]}
   :link-has      {:op :has-link :link :kunde}
   :link-is       {:op :is :prop :s :values ["alpha"] :link :kunde}
   :link-is-two   {:op :is :prop :s :values ["alpha" "beta"] :link :kunde}
   :link-null     {:op :null :prop :s :link :kunde}
   :link-contains {:op :contains :prop :s :value "lp" :link :kunde}
   :link-range    {:op :range :prop :n :min 1 :max 3 :link :kunde}
   :link-unknown  {:op :zzz :prop :s :link :kunde}
   :unknown-op    {:op :zzz :prop :s}})

(def rows
  {:alpha   {:object-type "kunde" :values {:s "alpha" :n 3}}
   :beta    {:object-type "kunde" :values {:s "beta" :n 7}}
   :both    {:object-type "kunde" :values {:s "alpha beta" :n 1}}
   :blank   {:object-type "kunde" :values {:n 4}}
   :strkeys {:object-type "lager" :values {"s" "alpha" "n" 5}}
   :metrics {:group {:s "gamma"} :metrics {:n 2}}})

(def ^:private schema-of {:attr? #{:s :n} :strings [:s]})

(defn- outcome
  [f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         [:refused (or (:refusal/code (ex-data e)) (:code (ex-data e)) (ex-message e))])))

(defn- filters
  []
  (concat
   (for [[tn t] (sort terms) neg [false true]]
     [[tn neg] {:operator :and :terms [(assoc t :negate? neg)]}])
   (for [op [:and :or]
         [a b] [[:is-one :range] [:contains :null] [:keyword-or :is-two]
                [:link-is :is-one] [:link-has :link-null] [:typed :foreign-prop]]]
     [[op a b] {:operator op :terms [(terms a) (terms b)]}])))

(defn current
  []
  (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
        (for [[n x] (filters)]
          [n {:matches (into (sorted-map)
                             (for [[rn r] rows] [rn (outcome #(f/matches? x r))]))
              :where   (outcome #(f/->where x schema-of))}])))

(defn write-golden! []
  (spit golden-file (with-out-str (binding [*print-namespace-maps* false]
                                    (pp/pprint (current))))))

(deftest every-filter-answers-as-it-did
  (let [want (edn/read-string (slurp golden-file))
        have (edn/read-string (pr-str (current)))]
    (doseq [k (sort-by pr-str (into (set (keys want)) (keys have)))]
      (testing (pr-str k)
        (is (= (get want k) (get have k)))))))

(deftest the-refusals-are-the-documented-ones
  (let [refused (into #{} (keep (fn [[_ {:keys [where]}]]
                                  (when (and (vector? where) (= :refused (first where)))
                                    (second where))))
                      (current))]
    (is (= #{:refusal.object-filter/no-not-over-text :refusal.object-filter/negated-range
             :refusal.object-filter/linked-or :refusal.object-filter/linked-one-value}
           refused))))
