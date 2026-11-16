(ns support.build-scenario
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is testing]]
            [sutra.wire.api :as api]
            [support.in-project :refer [in-project]]))

(defn ok? [{:keys [status]}] (= 200 status))
(defn py  [f] (slurp (io/resource (str "support/fixtures/python/" f))))
(defn objs [t] (:rows (:body (api/list-objects t))))
(defn val-of [o k] (get-in o [:values k]))
(defn col [n t] {:attribute-name n :attribute-type t})

(declare ^:private chain-schema*)

(def ^:private clojure-echo
  {:language :clojure
   :code "(fn [{:keys [things]}] (for [r things] {:pk (:pk r) :echo (:label r)}))"
   :inputs {:things {:kind :object-type :object-type :thing}}
   :primary-key :pk
   :output {:kind :object-type :object-type :echo}})

(defn chain-schema
  ([] (chain-schema nil))
  ([{:keys [echo]}]
   (cond-> (chain-schema*)
     (= :clojure echo) (assoc-in [:schema :transforms :mk-echo] clojure-echo))))

(defn- chain-schema* []
  {:schema
   {:objects
    {:thing   {:symbol "🔩" :attributes [[:pk :string] [:label :string]] :plural "Things"}
     :summary {:symbol "📝" :attributes [[:pk :string] [:thing-pk :string] [:shout :string]]
               :plural "Summaries"}
     :note    {:symbol "🗒️" :attributes [[:pk :string] [:body :string]] :plural "Notes"}
     :echo    {:symbol "🔊" :attributes [[:pk :string] [:echo :string]] :plural "Echos"}}
    :links
    [[:summary-of :summary :thing :many-to-one
      {:forward-name "summarises" :reverse-name "summary" :fk :thing-pk}]]
    :transforms
    {:mk-thing
     {:language :python :code (py "chain_thing.py")
      :inputs {:seed {:kind :dataset :dataset :seed}}
      :primary-key :pk
      :output {:kind :object-type :object-type :thing}}
     :mk-echo
     {:language :pg-sql
      :query "SELECT pk AS pk, label AS echo FROM things"
      :inputs {:things {:kind :object-type :object-type :thing}}
      :primary-key :pk
      :output {:kind :object-type :object-type :echo}}
     :mk-summary
     {:language :python :code (py "chain_summary.py")
      :inputs {:things {:kind :object-type :object-type :thing}}
      :primary-key :pk
      :output {:kind :object-type :object-type :summary}}
     :mk-note
     {:language :python :code (py "chain_note.py")
      :inputs {:things {:kind :object-type :object-type :thing}}
      :primary-key :pk
      :output {:kind :object-type :object-type :note}}}}})

(defn committed!
  [r label]
  (is (ok? r) label)
  (is (= :ok (get-in r [:body :status]))
      (str label " — landed (not :conflicts/:no-base): " (pr-str (:body r))))
  (is (pos? (long (or (get-in r [:body :committed]) 0)))
      (str label " — committed a non-zero number of staged actions"))
  r)

(defn install-chain!
  ([] (install-chain! nil))
  ([opts]
  (testing "install seed dataset + the chained transforms"
    (is (ok? (api/mutate :create-project {:name :chain} {:stage? true}))
        "the project it sits in")
    (is (ok? (api/mutate :create-dataset
                         {:name :seed :project :chain
                          :columns [(col :tid :string) (col :label :string)]}
                         {:stage? true})))
    (committed! (api/post! :staging) "commit dataset")
    (is (ok? (api/post! :install (update (chain-schema opts) :schema #(in-project :chain %))))
        "install stages")
    (committed! (api/post! :staging) "commit schema"))
  (testing "seed two things"
    (is (ok? (api/post! :dataset-write {:name "seed"} {:rows [{:values {:tid "t1" :label "alpha"}}
                                        {:values {:tid "t2" :label "beta"}}]
                                 :message "seed"}))))))

(defn run-and-assert-convergence!
  []
  (let [import-1 (api/post! :import-run)]
    (is (ok? import-1) "import pass 1")
    (testing "converges in ONE pass — downstream backing transforms see this-pass :thing"
      (is (= 2 (count (objs :thing))) "two things produced")
      (is (= 2 (count (objs :summary)))
          "two summaries produced IN THE SAME import (this-pass hand-off)")
      (is (= 2 (count (objs :note)))
          "the sibling :note backing transform also converged this pass"))
    (testing "chain values are correct (upstream pk flows to downstream)"
      (let [s (into {} (for [o (objs :summary)] [(val-of o :pk) o]))]
        (is (= #{"s-t1" "s-t2"} (set (keys s))) "one summary per thing")
        (when-some [o (s "s-t1")]
          (is (= "t1" (val-of o :thing-pk)) "carries the upstream thing pk")
          (is (= "ALPHA" (val-of o :shout)) "derived value materialized"))))
    import-1))
