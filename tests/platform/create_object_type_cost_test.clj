(ns ^:integration
    ^{:clj-kondo/config '{:linters {:consistent-alias {:aliases {sutra.store.api store-api}}}}}
    platform.create-object-type-cost-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.serve.server :as server]
            [sutra.store.api :as store-api]
            [sutra.store.conformance :as store-conf]
            [sutra.access.principals :as pr]
            [sutra.wire.api :as api]
            [sutra.wire.codec :as codec]
            [sutra.wire.protocol :as protocol]
            [sutra.wire.transport :as tx]
            [support.pod :as pod])
  (:import (java.io ByteArrayInputStream InputStream)
           (java.lang.management ManagementFactory)))

(def ^:private sizes
  [20 80 320])

(def ^:private growth-tolerance
  1.5)

(def ^:private amplification-ceiling
  450)

(defn- bytes-of ^bytes [b]
  (cond (bytes? b)                b
        (nil? b)                  (byte-array 0)
        (instance? InputStream b) (.readAllBytes ^InputStream b)
        :else                     (.getBytes (str b) "UTF-8")))

(defn- in-process
  [target method path body]
  (let [[uri q] (str/split (tx/on-branch path (:branch target)) #"\?" 2)
        token   (:token target)
        headers (cond-> {"accept" tx/CT
                         (str/lower-case protocol/header) (str protocol/version)}
                  (seq token) (assoc "authorization" (str "Bearer " token))
                  body        (assoc "content-type" tx/CT))
        resp    (server/app (cond-> {:request-method method :uri uri :query-string q
                                     :scheme :http :server-name "localhost" :server-port 80
                                     :remote-addr "127.0.0.1" :headers headers}
                              body (assoc :body (ByteArrayInputStream. (codec/encode body)))))
        raw     (bytes-of (:body resp))
        status  (:status resp)]
    {:status status
     :body   (if (<= 200 status 299)
               (codec/decode raw)
               (try (codec/decode raw) (catch Exception _ (String. raw "UTF-8"))))}))

(defn- allocated
  ^long []
  (.getThreadAllocatedBytes
   ^com.sun.management.ThreadMXBean (ManagementFactory/getThreadMXBean)
   (.getId (Thread/currentThread))))

(def ^:private attributes
  [[:id :string] [:name :string] [:status :string] [:menge :integer] [:datum :date]])

(defn- ontology [n]
  {:schema {:projects {:kosten {}}
            :objects  (into {} (for [i (range n)]
                                 [(keyword (str "typ-" i))
                                  {:project :kosten :plural "Typen" :primary-key :id
                                   :attributes attributes}]))}})

(defn- ok? [{:keys [status]}] (= 200 status))

(defn- create!
  [nm]
  (and (ok? (api/mutate :create-object-type
                        {:name nm :project :kosten :attributes [[:id :string] [:name :string]]
                         :plural "Objekte" :primary-key :id}
                        {:stage? true}))
       (ok? (api/post! :staging))))

(defn- measure
  [n]
  (let [out (atom nil)]
    ((pod/mem-pod-fixture {:allow-anon false})
     (fn []
       (with-redefs-fn {#'tx/send-once! in-process}
         (fn []
           (let [token (get-in (api/post! :login {:username grants/admin-username
                                                  :password pr/default-admin-password})
                               [:body :token])]
             (tx/with-token token
               (let [installed (and (ok? (api/post! :install (ontology n)))
                                    (ok? (api/post! :staging)))
                     text      (count (pr-str (:schema (:body (api/get! :snapshot)))))
                     warm      (create! :warm)
                     a0        (allocated)
                     [ok t]    (store-api/tallying #(create! :gemessen))
                     alloc     (- (allocated) a0)]
                 (reset! out (merge {:n n :text text :alloc alloc
                                     :ok? (boolean (and installed warm ok))}
                                    (store-api/totals t))))))))))
    @out))

(defn- store-verdicts [small big]
  (store-conf/cost-verdicts {:what "one create-object-type" :unit "object types"
                       :sizes [(:n small) (:n big)]
                       :small (select-keys small [:calls :rows :uncounted])
                       :big   (select-keys big [:calls :rows :uncounted])}))

(defn- growth
  [[a b c]]
  (let [da1 (- (:alloc b) (:alloc a)) da2 (- (:alloc c) (:alloc b))
        dt1 (- (:text b) (:text a))   dt2 (- (:text c) (:text b))]
    (/ (/ (double da2) (max 1 da1)) (/ (double dt2) (max 1 dt1)))))

(defn- amplification
  [ms]
  (let [a (first ms) c (last ms)]
    (/ (double (- (:alloc c) (:alloc a))) (max 1 (- (:text c) (:text a))))))

(defn- fmt [s & args] (String/format java.util.Locale/ROOT s (to-array args)))

(defn- report [ms]
  (str/join "\n" (for [{:keys [n text calls rows alloc]} ms]
                   (fmt "%5d types  %9d B text  %5d calls  %7d rows  %,14d B allocated"
                        n text calls rows alloc))))

(deftest the-growth-law-tells-linear-from-quadratic
  (let [at (fn [f] (mapv (fn [t] {:text t :alloc (long (f t))}) [100 400 1600]))]
    (testing "a constant plus k passes over the world is linear, however large k is"
      (is (< (Math/abs (- 1.0 (growth (at #(+ 5000 (* 400 %)))))) 1e-9))
      (is (= 400.0 (amplification (at #(+ 5000 (* 400 %)))))))
    (testing "one pass per entity over the world is quadratic, and breaks the law"
      (is (< growth-tolerance (growth (at #(* % %))))))))

(deftest one-create-object-type-follows-its-answer-not-the-ontology
  (let [ms (mapv measure sizes)]
    (println (str "\ncreate-object-type cost by ontology size\n" (report ms)
                  (fmt "\ngrowth %.2f  amplification %.1f B/B" (growth ms)
                       (amplification ms))))
    (testing "every create, at every size, answered ok"
      (is (every? :ok? ms) (report ms)))
    (testing "store calls and rows stay flat"
      (let [vs (store-verdicts (first ms) (last ms))]
        (is (empty? vs) (str/join "\n" (map :error vs)))))
    (testing "allocation grows at most linearly with the ontology's text"
      (is (<= (growth ms) growth-tolerance)
          (fmt (str "the allocation's increment grew %.2fx faster than the text's —"
                    " some step does a pass over the world per entity of it\n%s")
               (growth ms) (report ms))))
    (testing "bytes allocated per byte of ontology stay under the ceiling"
      (is (<= (amplification ms) amplification-ceiling)
          (fmt "%.1f B allocated per byte of ontology, ceiling %s\n%s"
               (amplification ms) amplification-ceiling (report ms))))))
