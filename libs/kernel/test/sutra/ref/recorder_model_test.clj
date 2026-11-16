(ns sutra.ref.recorder-model-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.ref.fixture :as f]
            [sutra.ref.run :as run]))

(def ^:private gen-id (gen/choose 0 3))

(def ^:private gen-leaf
  (gen/frequency [[4 (gen/fmap #(vector :create %) gen-id)]
                  [4 (gen/fmap #(vector :update %) gen-id)]
                  [1 (gen/return [:fail])]
                  [1 (gen/return [:boom])]
                  [2 (gen/return [:notify])]]))

(def ^:private gen-sending
  (gen/frequency [[3 (gen/return [:notify])] [1 (gen/return [:boom])]]))

(def ^:private gen-steps
  (gen/recursive-gen
   (fn [inner]
     (let [steps (gen/vector inner 0 4)]
       (gen/frequency [[3 gen-leaf]
                       [1 (gen/fmap #(vector :call %) steps)]
                       [1
                        (gen/fmap #(vector :side %)
                                  (gen/vector (gen/frequency [[4 gen-sending] [1 inner]]) 0 4))]])))
   gen-leaf))

(def ^:private gen-action (gen/vector gen-steps 0 5))

(defn- oid [n] (str "o" n))

(declare model-action model-seq)

(defn- failed [kind s] [::failed kind s])
(defn- failed? [r] (vector? r))

(defn- model-step
  [side? s [op x]]
  (case op
    (:create :update :fail)
    (cond
      side?                            (failed :side-effect-edit s)
      (= :create op)                   (-> s (update :ops conj [:create-object (oid x)])
                                           (update :visible conj (oid x))
                                           (update :events conj [:create (oid x)]))
      (and (= :update op) ((:visible s) (oid x)))
      (-> s (update :ops conj [:update-object (oid x)]) (update :events conj [:update (oid x)]))
      :else                            (failed :other s))
    :boom   (failed :other s)
    :notify (update s :events conj [:notify])
    :call   (model-action side? s x)
    :side   (model-seq side? s x)))

(defn- model-seq [side? s steps]
  (reduce (fn [s step] (let [r (model-step side? s step)] (if (failed? r) (reduced r) r))) s steps))

(defn- model-action
  [side? s steps]
  (let [{side true main false} (group-by #(= :side (first %)) steps)
        r (model-seq side? s main)]
    (if (failed? r)
      r
      (reduce (fn [s [_ region]]
                (let [r (model-seq true (assoc s :events []) region)]
                  (cond
                    (not (failed? r))                (update r :events #(into (:events s) %))
                    (= :side-effect-edit (second r)) (reduced r)
                    :else
                      (update s :events conj [:side-effect-failed]))))
              r side))))

(defn- model [steps]
  (let [r (model-action false {:ops [] :visible #{} :events []} steps)]
    (if (failed? r)
      {:failed (second r) :ops []}
      {:failed nil :ops (:ops r) :events (:events r)})))

(defn- lower-steps
  [steps actions]
  (reduce (fn [[rules actions] [op x]]
            (case op
              :create [(conj rules
                             {:op :create :object-type f/auftrag :id (oid x) :values {:titel "x"}})
                       actions]
              :update [(conj rules
                             {:op :update :object-type f/auftrag :target (oid x)
                              :values {:titel "y"}}) actions]
              :fail   [(conj rules {:op :update :object-type f/auftrag :target "nope" :values {}})
                       actions]
              :boom   [(conj rules {:op :call :fn :nirgends :args {:arg0 {}}}) actions]
              :notify [(conj rules {:op :notify :channel "c" :payload "p"}) actions]
              :call   (let [[body actions] (lower-steps x actions)
                            nm (keyword (str "a" (count actions)))]
                        [(conj rules {:op :call :fn nm :args {:arg0 {}}})
                         (assoc actions nm {:rules body})])
              :side   (let [[body actions] (lower-steps x actions)]
                        [(conj rules {:op :action :phase :side-effect :chain body}) actions])))
          [[] actions] steps))

(defn- kernel [steps]
  (let [[rules actions] (lower-steps steps {})
        world (reduce-kv f/with-action f/world (assoc actions :top {:rules rules}))
        r     (run/run-action (f/sigma) {} world :top {})
        ops   (mapv (fn [{:keys [op payload]}] [op (:id payload)]) (:ops r))]
    (if-let [kind (get-in r [:error :kind])]
      {:failed (if (= :side-effect-edit kind) kind :other) :ops ops}
      {:failed nil :ops ops
       :events (mapv (fn [{:keys [op target]}]
                       (if (#{:side-effect-failed :notify} op) [op] [op (:id target)]))
                     (:events r))})))

(deftest the-recorder-follows-the-model
  (let [r (tc/quick-check 500 (prop/for-all [steps gen-action] (= (model steps) (kernel steps)))
                          :seed 20261008)]
    (is (:pass? r) (pr-str (select-keys r [:seed :shrunk])))))
