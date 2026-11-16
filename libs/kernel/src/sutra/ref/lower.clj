(ns sutra.ref.lower
  (:require [sutra.ref.install :as install]))

(set! *warn-on-reflection* true)

(def foreign-ops
  #{:invoke-webhook :invoke-function :use-llm})

(def write-ops
  #{:create :update :delete-object :create-link :delete-link :apply-marking :remove-marking
    :create-of-interface :update-of-interface :delete-of-interface
    :create-link-of-interface :delete-link-of-interface})

(def expand-ops #{:apply-edit-function :python-edit :clojure-edit})

(def surface-ops
  (into #{:set :dissoc :if :for-each :let :call :compute :query :notify
          :branch :action :effect :compute-on :create-or-update}
        (concat write-ops foreign-ops expand-ops)))

(def entity-ops
  (set (for [tag (keys install/class-by-tag) verb ["create" "update" "delete"]]
         (keyword (str verb "-" (name tag))))))

(declare lower lower-seq)

(defn- entity-verb [op]
  (when (keyword? op)
    (when-let [[_ verb tag] (re-matches #"(create|update|delete)-(.+)" (name op))]
      (when (install/class-by-tag (keyword tag)) [(keyword verb) (keyword tag)]))))

(defn- core [{:keys [op] :as s}]
  (case op
    :set      {:k :set :path (:path s) :value (:value s)}
    :dissoc   {:k :dissoc :path (:path s)}
    :if       {:k :if :env :bindings :pred (:pred s)
               :then (some-> (:then s) lower) :else (some-> (:else s) lower)}
    :for-each {:k :for-each
               :over (if (:collection s) {:binding (:collection s)} {:expr (:coll s)})
               :as (:as s) :body (lower-seq (:steps s)) :collect-ids (:collect-ids s)}
    :let      {:k :seq :body [{:k :bind :as (:bind s) :term [:expr (:value s)]}
                              (lower (:step s))]}
    :call     (let [n {:k :invoke :fn (:fn s) :args (:args s) :as (:as s)}]
                (if (:as s) n {:k :scope :body n}))
    :compute  {:k :bind :as (:as s) :term [:coll (:from s) (:expr s) (:default s)] :event :compute}
    :query    {:k :query :step s}
    :notify   {:k :notify :channel (:channel s) :payload (:payload s)}

    :branch   {:k :scope
               :body {:k :if :env {:resolve-bindings (:test s)} :pred (:pred s)
                      :then (lower-seq (:then s)) :else (lower-seq (:else s))}}
    :action   {:k :scope :body {:k :subst :steps (vec (:chain s))}}
    :effect   {:k :scope :body {:k :invoke :fn (:fn s) :args (:args s) :effect? true}}
    :compute-on {:k :compute-on :step s}
    :create-or-update
    (let [create (if (:object-type s)
                   {:k :write :op :create :step (dissoc s :target)}
                   {:k :fail :kind :no-object-type :op op
                    :message (str "create-or-update: without a target this creates, and a create"
                                  " needs :object-type on the rule — an update takes the type from"
                                  " its object parameter, a create has nothing to take it from")})]
      (if (:target s)
        {:k :if :env {:terms {:t [:target-id (:target s)]}} :pred [:not [:= :t nil]]
         :then {:k :write :op :update :step s}
         :else create}
        create))

    (cond
      (expand-ops op)  {:k :expand :via op :step s}
      (write-ops op)   {:k :write :op op :step s}
      (foreign-ops op) {:k :foreign :op op :step s}
      :else (if-let [[verb tag] (entity-verb op)]
              {:k :install :verb verb :tag tag :step s}
              {:k :fail :kind :unknown-step-op :op op :step s}))))

(defn lower
  [s]
  {:k :at :op (when (map? s) (:op s)) :src s
   :body (if (map? s) (core s) {:k :fail :kind :unknown-step-op :op nil})})

(defn lower-seq
  [steps]
  {:k :seq :body (into [] (map-indexed #(assoc (lower %2) :frame? true :i %1)) steps)})
