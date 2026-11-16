(ns sutra.store.core.kv
  (:require [clojure.edn :as edn]
            [sutra.store.api :as api]
            [sutra.store.spi :as spi]
            [sutra.store.core :as-alias core]))

(set! *warn-on-reflection* true)

(defn rev-key [r] (format "%019d" r))
(defn log-key [r i] (str (rev-key r) "/" (format "%09d" i)))
(defn enc [x] (pr-str x))
(defn record-part [coll b] (str (enc coll) "|" b))
(defn edit-prefix [T] (str (enc T) "|"))
(defn edit-key [T id] (str (edit-prefix T) (enc (str id))))

(defn branch-k [b] [:branch "" b])
(def layout-init
  "init")

(defn layout-k
  [] [:clock "" "layout"])
(defn rev-k [r] [:rev "" (rev-key r)])
(defn authored-k [b] [:schema b "authored"])
(defn schema-k [b r] [:schema b (rev-key r)])
(defn clock-k [b tick]
  [:clock b (enc tick)])
(defn lease-k [n] [:lease "" (enc n)])
(defn records-clock-k [coll]
  [:records-clock "" (enc coll)])
(defn schema-log-k [b r]
  [:schema-log b (rev-key r)])
(defn schema-logged-k [b]
  [:schema-logged "" b])
(defn log-k [r i] [:log "" (log-key r i)])
(defn log-of-k [b r i] [:log-of b (log-key r i)])
(defn log-kind-prefix [kind] (str (enc kind) "|"))
(defn log-kind-k [b kind r i] [:log-kind b (str (log-kind-prefix kind) (log-key r i))])
(defn edited-k [b T id] [:edited b (edit-key T id)])
(defn detached-k [b T] [:detached b (enc T)])
(defn change-k [T r] [:change (enc T) (rev-key r)])
(defn renames-k [T r] [:renames (enc T) (rev-key r)])
(def log-indexed-k [:store "" "log-of"])
(defn log-kinded-k
  [b]
  [:log-kinded "" b])
(defn kinded-through
  [row mark]
  (when (and mark row (= (:base mark) (:base row)) (<= (:rev mark) (:rev row)))
    (:rev mark)))

(defn running!
  []
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "interrupted — the work stops at its next store round trip"))))

(defn kv-read
  [{:keys [engine]} h ks]
  (running!)
  (spi/-kv-read engine h ks))

(defn kv-get [db h k] (get (kv-read db h [k]) k))

(defn kv-scan
  [{:keys [engine]} h [space part] opts]
  (running!)
  (spi/-kv-scan engine h space part opts))

(defn kv-parts
  [{:keys [engine]} h space]
  (running!)
  (spi/-kv-parts engine h space))

(defn kv-pages
  [db h sp opts]
  (let [rev? (:reverse? opts)]
    (letfn [(page [resume n]
              (lazy-seq
               (let [o    (cond-> (assoc opts :limit n)
                            resume (assoc (if rev? :upto :after) resume))
                     raw  (kv-scan db h sp o)
                     kvs  (cond->> raw (and resume rev?) (drop-while #(= resume (first %))))]
                 (if (< (count raw) n)
                   kvs
                   (concat kvs (page (first (peek raw)) (min 8192 (* 2 n))))))))]
      (page nil 256))))

(defn kv-fx [k v] [:kv k v])

(def ^:private lays-out?
  (comp #{:layout :ns-fork :ns-drop} first))

(def ^:private writes-storage?
  (comp #{:write-rows :write-links :delete-rows :delete-links} first))

(defn with-layout-stamp
  [effects stamp cow?]
  (cond-> (vec effects)
    (or (some lays-out? effects)
        (and cow? (some writes-storage? effects)))
    (conj (kv-fx (layout-k) stamp))))

(defn- edn-data?
  [v]
  (letfn [(plain? [x]
            (cond
              (or (nil? x) (string? x) (boolean? x) (uuid? x) (= java.util.Date (class x)))
              true
              (number? x)  (not (or (instance? Float x)
                                    (and (instance? Double x) (Double/isNaN x))))
              (keyword? x)
              (some?
               (re-matches #":[A-Za-z*+!_?<>=-][\w*+!?<>=.-]*(/[A-Za-z*+!_?<>=-][\w*+!?<>=.-]*)?"
                           (str x)))
              (record? x)  false
              (map? x)     (every? (fn [[k v]] (and (plain? k) (plain? v))) x)
              (or (vector? x) (set? x) (seq? x)) (every? plain? x)
              :else        false))]
    (or (number? v) (symbol? v) (plain? v)
        (= v (try (edn/read-string (pr-str v)) (catch RuntimeException _ ::core/unreadable))))))

(defn kv-writes
  [check? effects]
  (reduce (fn [w [op k v]]
            (if (not= :kv op)
              w
              (let [[space _ kk] k]
                (when (and check? (not (edn-data? v)))
                  (api/refuse! :malformed
                               (str "not EDN data, so no engine can keep it: " (name space))
                               {:store/rule :edn-data :space space :k kk}))
                (assoc w k v))))
          {} effects))
