(ns support.fe
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [support.pod :as pod]
            [sutra.wire.api :as api])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter]
           [java.util.concurrent TimeUnit]))

(def ^:private harness (io/file (System/getProperty "user.dir") "tests/frontend"))
(def ^:private bundle  (io/file harness "out" "driver.js"))
(def ^:private sources (mapv #(io/file harness "src" "sutra_e2e" %)
                             ["driver.cljs" "object_set.cljs"]))

(def ^:private supported-keys
  #{:find :where :search :show :order-by :group-by :aggs :export :pick :relations
    :search-objects :pivot :pivot-to :compare-where :action :save :login :open-saved
    :shot})

(def ^:dynamic *browser* nil)

(defn- stale-bundle? []
  (or (not (.exists bundle))
      (some #(> (.lastModified ^java.io.File %) (.lastModified bundle)) sources)))

(defn- compile-bundle! []
  (when-not (.exists (io/file harness "node_modules"))
    (throw (ex-info (str "the Playwright harness is not installed — run: "
                         "cd tests/frontend && npm install")
                    {:dir (str harness)})))
  (let [{:keys [exit out err]} (sh/sh "npx" "shadow-cljs" "compile" "driver"
                                         :dir harness)]
    (when-not (zero? exit)
      (throw (ex-info "shadow-cljs compile driver failed" {:out out :err err})))))

(defn- line! [^BufferedReader out ms]
  (let [f (future (.readLine out))
        v (deref f ms ::timeout)]
    (when (= v ::timeout)
      (future-cancel f)
      (throw (ex-info (str "the frontend driver went silent for " ms "ms") {})))
    (or v (throw (ex-info "the frontend driver closed its output" {})))))

(defn- start! [base]
  (let [pb (doto (ProcessBuilder. ["node" (.getPath bundle) "reads" base])
             (.directory harness)
             (.redirectErrorStream true))]
    (.put (.environment pb) "NODE_PATH" (.getPath (io/file harness "node_modules")))
    (let [proc (.start pb)
          out  (BufferedReader. (InputStreamReader. (.getInputStream proc) "UTF-8"))
          in   (OutputStreamWriter. (.getOutputStream proc) "UTF-8")]
      (loop []
        (let [l (str/trim (line! out 120000))]
          (cond
            (= "READY" l)                  {:proc proc :out out :in in}
            (str/starts-with? l "ERROR ")  (throw (ex-info (str "frontend driver: " l) {}))
            :else                          (recur)))))))

(defn- stop! [{:keys [^Process proc ^OutputStreamWriter in]}]
  (try (.close in) (catch Exception _ nil))
  (when-not (.waitFor proc 20 TimeUnit/SECONDS)
    (.destroyForcibly proc)))

(defn- attr-types [type-kw]
  (->> (vals (:object-types (:body (api/get! :schema))))
       (some #(when (= type-kw (:object-type-name %)) (:attribute-list %)))
       (into {} (map (juxt :attribute-name :attribute-type)))))

(defn- metric-num [v]
  (cond
    (not (string? v))               v
    (re-matches #"-?\d+" v)         (parse-long v)
    (re-matches #"-?\d+\.\d+" v)    (bigdec v)
    :else                           v))

(defn- cell [t v]
  (cond
    (nil? v)        nil
    (= "—" v)       nil
    (= "" v)        nil
    (not (string? v)) v
    :else      (case t
                 (:integer :int :long)            (parse-long v)
                 (:decimal :float :double :number) (try (bigdec v) (catch Exception _ v))
                 :boolean                          (contains? #{"true" "yes"} (str/lower-case v))
                 v)))

(defn- unsupported [spec]
  (let [extra (remove supported-keys (keys spec))
        hops  (filter map? (:show spec))]
    (cond
      (seq extra) (str "the Object Workspace has no surface for " (pr-str (vec extra))
                       " — it reads one type's set, filtered")
      (seq hops)  (str "the set table shows no link hop: " (pr-str (vec hops))
                       " — a one-hop neighbourhood is the Object View's Relations"
                       " card, read with :relations true"))))

(defn- project [rows show]
  (if-let [ks (seq (filter keyword? show))]
    (mapv #(update % :values select-keys ks) rows)
    rows))

(defn query [spec]
  (when-let [why (unsupported spec)]
    (throw (ex-info why {:spec spec})))
  (let [{:keys [in out]} (or *browser*
                             (throw (ex-info "support.fe/query needs the with-browser fixture"
                                             {:spec spec})))]
    (.write ^OutputStreamWriter in (str (pr-str (dissoc spec :show)) "\n"))
    (.flush ^OutputStreamWriter in)
    (loop []
      (let [l (line! out 180000)]
        (cond
          (str/starts-with? l "RESULT ")
          (let [payload (edn/read-string (subs l 7))
                types   (when (:find spec) (attr-types (:find spec)))]
            {:status 200
             :body   (cond
                       (:aggregations payload)
                       (update payload :aggregations
                               #(mapv (fn [a] (update a :metrics update-vals metric-num)) %))

                       (contains? payload :rows)
                       (assoc payload :rows
                              (project (mapv (fn [r]
                                               {:values (into {} (map (fn [[k v]]
                                                                        [k (cell (get types k) v)]))
                                                              r)})
                                             (:rows payload))
                                       (:show spec)))

                       :else payload)})

          (str/starts-with? l "ERROR ")
          (throw (ex-info (subs l 6) {:spec spec}))

          :else (recur))))))

(defn with-browser [f]
  (when (stale-bundle?) (compile-bundle!))
  (let [session (start! (pod/base-url))]
    (try
      (binding [*browser* session] (f))
      (finally (stop! session)))))
