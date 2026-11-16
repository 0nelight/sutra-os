(ns ^:unit platform.transform-protocol-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [sutra.compute.external :as ext]
            [sutra.compute.protocol :as protocol]
            [sutra.compute.runner :as runner]))

(def ^:private schema-file "boundaries/transform-runtime/protocol.schema.json")

(defn- render []
  (str (json/write-str (protocol/json-schema) :indent true :escape-unicode false) "\n"))

(defn- seen [x] (json/read-str (json/write-str x) :key-fn keyword))

(deftest the-schema-file-is-current
  (is (= (render) (slurp schema-file))
      (str schema-file " is stale — run (platform.transform-protocol-test/-main)")))

(def ^:private resolved
  {:orders {:kind :object-type :rows [{:id "1" :betrag 12.50M}]
            :aliases {"Betrag" "betrag"} :derived-names ["marge"]}
   :feed   {:kind :datasource :path "/in/feed.json"}
   :limit  {:kind :scalar :value 3.5M}
   :scan   {:kind :media :address "ri.media.1"}
   :model  {:kind :language-model :source :anthropic :model "claude-sonnet-5-5"}
   :docs   {:kind :media-set :rows [{:ref "a"}] :media-refs ["a"]}})

(defn- as-written
  [manifest]
  (update-vals manifest #(if (contains? % :rows)
                           (-> % (dissoc :rows) (assoc :path "/in/rows.json"))
                           %)))

(deftest the-host-manifest-conforms
  (let [{:keys [inputs-manifest]} (#'runner/python-inputs resolved nil)
        m (seen (as-written inputs-manifest))]
    (is (= (set (keys resolved)) (set (keys m))))
    (is (nil? (protocol/validate "manifest" m)))
    (testing "an entry with neither path nor kind is not a manifest"
      (is (some? (protocol/validate "manifest" {:x {:aliases {}}}))))))

(deftest the-environment-carries-the-version
  (let [env (#'ext/sandbox-env {:transform-name :t :parameters nil} {:trace? true} false)]
    (is (= protocol/version (get env "SUTRA_PROTOCOL_VERSION")))
    (is (nil? (protocol/validate "environment" (update-keys env keyword))))))

(deftest versions-are-judged-by-major
  (is (protocol/compatible? protocol/version))
  (is (protocol/compatible? (str (protocol/major protocol/version) ".99")))
  (is (protocol/compatible? nil) "a process from before the protocol was versioned")
  (is (not (protocol/compatible? "99.0"))))

(deftest a-result-of-another-major-is-refused
  (let [run (fn [v] (#'ext/process-result
                     "t" {} {:exit 0 :err ""
                             :out (json/write-str {:rows [{:id "1"}] :protocol_version v})}))]
    (is (= [{:id "1"}] (:rows (run protocol/version))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"speaks runtime protocol 99.0"
                          (run "99.0")))))

(deftest a-result-conforms
  (is (nil? (protocol/validate "result" (seen {:protocol_version protocol/version
                                               :rows [{:id "1" :n "~f1.5"}]
                                               :media []
                                               :incremental-complete false})))))

(defn -main [& _]
  (spit schema-file (render))
  (println "wrote" schema-file))
