(ns ^:unit platform.function-contract-local-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.findings.functions :as functions]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.world-install :as world-install]
            [sutra.world.db :as kdb]
            [support.every-index :as every]))

(def ^:private fns [:f1 :f2 :f3])

(defn- fn-rid [i] (str "ri.sutra.main.function." i))

(def ^:private g-types
  (gen/map (gen/elements [:a :b :c]) (gen/elements [:integer :long :string]) {:max-elements 3}))

(def ^:private g-edit
  (gen/let [n (gen/elements fns) ps g-types] {:function-name n :param-types ps}))

(def ^:private g-query
  (gen/let [n (gen/elements fns) t (gen/elements [:integer :long]) ret? gen/boolean]
    (cond-> {:transform-name (keyword (str "q-" (name n))) :function-name n
             :inputs {:x {:kind :scalar :type t}}}
      ret? (assoc :returns {:y :string}))))

(defn- g-callee [world-rids?]
  (if world-rids? (gen/one-of [(gen/elements fns) (gen/fmap fn-rid (gen/choose 0 2))])
      (gen/elements fns)))

(defn- g-action [rids?]
  (gen/let [n (gen/elements [:a1 :a2 :a3]) f (g-callee rids?)
            ins (gen/set (gen/elements [:a :b :c]))]
    {:action-name n :params {:p {:type :string}}
     :rules [(cond-> {:op :apply-edit-function :function f}
               (seq ins) (assoc :inputs (zipmap ins (repeat [:get :p]))))]}))

(defn- g-app [rids?]
  (gen/let [n (gen/elements [:app1 :app2]) f (g-callee rids?)
            args (gen/set (gen/elements [:a :b :x]))]
    {:application-name n :vars {:v {:function f :args (zipmap args (repeat 1))}}}))

(def ^:private g-world
  (gen/let [es (gen/vector g-edit 0 3)
            qs (gen/vector g-query 0 2)
            as (gen/vector (g-action true) 0 3)
            ps (gen/vector (g-app true) 0 2)]
    {:functions    (into {} (map-indexed (fn [i e] [(fn-rid i) e])) es)
     :transforms   (into {} (map-indexed (fn [i q] [(str "ri.sutra.main.transform." i) q])) qs)
     :actions      (into {} (map-indexed (fn [i a] [(str "ri.sutra.main.action-type." i) a])) as)
     :applications (into {} (map-indexed (fn [i a] [(str "ri.sutra.main.application." i) a]))
                         ps)}))

(def ^:private g-step
  (gen/one-of
   [(gen/let [e g-edit oc (gen/elements [:created :updated :deleted])]
      {:kind :function :entity (:function-name e) :outcome oc :args e})
    (gen/let [q g-query oc (gen/elements [:created :updated :deleted])]
      {:kind :transform :entity (:transform-name q) :outcome oc :args q})
    (gen/let [a (g-action false) oc (gen/elements [:created :updated :deleted])]
      {:kind :action :entity (:action-name a) :outcome oc :args a})
    (gen/let [a (g-app false) oc (gen/elements [:created :updated])]
      {:kind :application :entity (:application-name a) :outcome oc :args a})]))

(defn- outcome [f] (try {:ok (vec (f))} (catch Exception e {:thrown (ex-message e)})))

(defn- found [W plan ix]
  (outcome #((:find functions/function-contract) (diff/diff {} W plan nil ix))))

(defspec the-contract-read-by-name-is-the-contract-read-whole 1500
  (prop/for-all [W g-world plan (gen/vector g-step 1 3)]
    (let [ix (live/index-of (kdb/db W world-install/views))]
      (= (found W plan (every/index W)) (found W plan ix)))))
