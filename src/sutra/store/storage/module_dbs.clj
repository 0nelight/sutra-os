(ns sutra.store.storage.module-dbs
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.store.api :as api]
            [sutra.store.control.tables :as cc]))

(set! *warn-on-reflection* true)

(def setting "SUTRA_MODULE_DATABASES")

(defn owners
  []
  (edn/read-string (slurp (io/resource "sutra/table-owners.edn"))))

(defn routes
  [config]
  (into {}
        (for [part (str/split (str (get config setting "")) #",")
              :let [part (str/trim part)]
              :when (seq part)
              :let [[m url] (str/split part #"=" 2)]]
          [(symbol (str/trim m)) (some-> url str/trim)])))

(defn lane
  [m]
  (keyword "module" (str m)))

(defn colls-of
  [owned m]
  (into #{} (map #(keyword "ctl" (name %))) (get-in owned [:tables m])))

(defn refusals
  [owned routes]
  (for [[m url] routes
        why (concat
             (when-not (contains? (:tables owned) m)
               [(str "no module " m " owns control tables")])
             (when-not (and url (str/starts-with? url "jdbc:postgresql:"))
               [(str "needs a jdbc:postgresql: url, got " (pr-str url))])
             (for [{:keys [modules why]} (:cross-module-tx owned)
                   :when (some #{m} modules)]
               (str "commits together with " (vec (remove #{m} modules)) ": " why))
             (when-let [ts (seq (get-in owned [:raw-sql m]))]
               [(str "reads its tables by SQL past the store: " (vec ts))])
             (for [c (sort (colls-of owned m))
                   :let [d (cc/by-coll c)]
                   :when (or (not= :global (:scope d)) (:drops-with d))]
               (str c " lives and goes with the branches of the main store")))]
    {:module m :why why}))

(defn check!
  [owned routes]
  (when-let [bad (seq (refusals owned routes))]
    (refusal/refuse! :refusal.store/module-without-db
                     {:refusals (vec bad)}
                     {:setting setting
                      :why (str/join "; " (map #(str (:module %) " " (:why %)) bad))}))
  routes)

(defn- routed-coll [by-coll q]
  (when (and (vector? q) (= "records" (namespace (first q))))
    (get by-coll (second q))))

(def ^:private service-keys
  #{:basis :lane :actor :records})

(defn- home-of
  [by-coll {:keys [records] :as tx}]
  (let [homes (into #{} (map #(get by-coll (:coll %) ::main)) records)]
    (cond
      (= #{::main} homes) nil
      (empty? homes)      nil
      (and (= 1 (count homes)) (every? service-keys (keys tx))) (first homes)
      :else (let [d {:colls (sort (distinct (map :coll records)))}]
              (refusal/refused :refusal.store/mixed-module-commit d)))))

(defn- on-module [basis [_ l]] (assoc (select-keys basis [:branch]) :lane l))

(deftype RoutedSnapshot [snap by-coll]
  api/Snapshot
  (-basis [_] (api/-basis snap))
  (-answer [_ authority q]
    (if-let [[db :as home] (routed-coll by-coll q)]
      (with-open [^java.io.Closeable s (api/-snapshot db (on-module (api/-basis snap) home))]
        (api/-answer s authority q))
      (api/-answer snap authority q)))
  (-overlay [_ staged] (RoutedSnapshot. (api/-overlay snap staged) by-coll))
  (-close [_] (api/-close snap))
  java.io.Closeable
  (close [_] (api/-close snap)))

(defrecord Routed [db by-coll]
  api/Db
  (-snapshot [_ basis] (->RoutedSnapshot (api/-snapshot db basis) by-coll))
  (-prepare [_ tx]
    (let [home (home-of by-coll tx)]
      (cond (anomaly/refusal? home) home
            home (api/-prepare (first home) (assoc tx :lane (second home)))
            :else (api/-prepare db tx))))
  (-branch! [_ op] (api/-branch! db op))
  (-read-at [_ basis authority q]
    (if-let [[m :as home] (routed-coll by-coll q)]
      (api/-read-at m (on-module basis home) authority q)
      (api/-read-at db basis authority q))))

(defn routed
  [db owned dbs]
  (if (empty? dbs)
    db
    (->Routed db (into {} (for [[m mdb] dbs c (colls-of owned m)] [c [mdb (lane m)]])))))
