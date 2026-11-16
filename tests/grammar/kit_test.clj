(ns ^:unit grammar.kit-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.cache :as cache]
            [sutra.kit.coerce :refer [->kw ->num ->num0]]
            [sutra.kit.digest :as digest]
            [sutra.kit.err :as err]
            [sutra.kit.pubsub :as pubsub]
            [sutra.kit.sql :as kit-sql]
            [sutra.kit.time :as time]
            [sutra.kit.tree :as tree]
            [sutra.types.refs :as refs]))

(def ^:private rules
  [{:op :branch
    :then [{:op :create :object-type :a}]
    :else {:op :update :object-type :b :steps [{:op :delete-object :object-type :c}]}}
   :not-a-step])

(deftest steps-walks-every-nested-rule-pre-order
  (is (= [:branch :create :update :delete-object] (map :op (refs/steps rules))))
  (is (= [:x] (map :op (refs/steps {:op :x})))))

(deftest update-steps-rewrites-parent-before-children
  (is (= [{:op :branch :n 1 :then [{:op :create :n 1}] :else {:op :update :n 1}}]
         (refs/update-steps #(assoc % :n 1)
                            [{:op :branch :then [{:op :create}] :else {:op :update}}]))
      "a single step under :else is rewritten as steps reads it"))

(deftest reach-is-breadth-first-and-includes-seeds
  (is (= [1 2 3 4] (tree/reach {1 [2 3] 2 [1 4]} [1])))
  (is (= [] (tree/reach {} []))))

(deftest canonical-sorts-maps-and-maps-leaves
  (is (= "{:a 1, :b #{1}}" (pr-str (tree/canonical {:b #{1} :a 1}))))
  (is (= "{:a [1 2]}"
         (pr-str (tree/canonical {:leaf #(if (set? %) (vec (sort %)) %)} {:a #{2 1}})))))

(deftest causes-walk-the-chain
  (let [e (ex-info "outer" {} (ex-info "inner" {:hit true}))]
    (is (= ["outer" "inner"] (map ex-message (err/causes e))))
    (is (= "inner" (ex-message (err/cause (comp :hit ex-data) e))))))

(deftest quoting-doubles-the-closing-mark
  (is (= "\"a\"\"b\"" (kit-sql/dq "a\"b")))
  (is (= "'it''s'" (kit-sql/sq "it's")))
  (is (= "\"s\".\"t\"" (kit-sql/qualified kit-sql/dq "s" "t"))))

(deftest coercions
  (is (= :a/b (->kw :a/b)))
  (is (= :x (->kw "x")))
  (is (nil? (->kw nil)))
  (is (= 1.5 (->num " 1.5 ")))
  (is (nil? (->num "x")))
  (is (= 0.0 (->num0 nil))))

(deftest salted-hash-round-trips
  (let [stored (digest/salted "0a1b2c3d" "pw")]
    (is (digest/salted-ok? "pw" stored))
    (is (not (digest/salted-ok? "nope" stored)))
    (is (= (digest/sha256-hex "x") (digest/sha256-hex (.getBytes "x" "UTF-8"))))))

(deftest lru-keeps-nil-and-evicts-least-recent
  (let [c     (cache/lru 2)
        calls (atom 0)]
    (is (nil? (cache/through c :a #(do (swap! calls inc) nil))))
    (is (nil? (cache/through c :a #(do (swap! calls inc) nil))))
    (is (= 1 @calls))
    (cache/through c :b (constantly 2))
    (cache/through c :c (constantly 3))
    (is (= #{:b :c} (set (keys c))))))

(deftest pubsub-drops-a-throwing-sink
  (let [reg (pubsub/registry)
        got (atom [])]
    (pubsub/subscribe! reg :t #(swap! got conj %))
    (pubsub/subscribe! reg :t (fn [_] (throw (ex-info "boom" {}))))
    (pubsub/publish! reg :t 1)
    (is (= [1] @got))
    (is (= 1 (pubsub/count reg :t)))))

(deftest temporal-values-land-on-utc
  (testing "an offset string, a space-separated one and a date agree on the UTC instant"
    (is (= 1783498800000 (time/->epoch-ms "2026-07-08T10:20:00+02:00")))
    (is (= 1783498800000 (time/->epoch-ms "2026-07-08 08:20:00")))
    (is (= 1783468800000 (time/->epoch-ms "2026-07-08"))))
  (is (nil? (time/->epoch-ms "05.08.2026"))))
