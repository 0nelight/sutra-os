(ns ^:unit grammar.boundary-compat-test
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pp]
            [clojure.test :refer [deftest is testing]]
            [sutra.foundation.compat :as compat]
            [sutra.foundation.boundaries :as bnd]))

(def ^:private a-ref {"$ref" "#/components/schemas/A"})

(def ^:private a-props ["components" "schemas" "A" "properties"])

(def ^:private api
  {"openapi"    "3.1.0"
   "paths"      {"/api/a"
                 {"get" {"parameters" [{"in" "query" "name" "q" "schema" {"type" "string"}}]
                         "responses"  {"200" {"content" {"application/json"
                                                         {"schema" a-ref}}}}}}}
   "components" {"schemas" {"A" {"type" "object" "properties" {"x" {"type" "string"}}}}}})

(defn- facts [doc]
  (let [fs (compat/openapi-facts doc)] (set (concat fs (compat/component-facts doc fs)))))

(deftest a-change-is-classified
  (let [old (facts api)]
    (testing "nothing moved"
      (is (= :none (:kind (compat/change old old)))))
    (testing "a new property is an addition"
      (is (= :minor (:kind (compat/change old (facts (assoc-in api (conj a-props "y")
                                                               {"type" "integer"})))))))
    (testing "a property that goes away breaks"
      (is (= :breaking
             (:kind (compat/change old (facts (assoc-in api a-props {})))))))
    (testing "a parameter becoming required breaks"
      (is (= :breaking
             (:kind (compat/change old (facts (assoc-in api ["paths" "/api/a" "get"
                                                             "parameters" 0 "required"]
                                                        true)))))))
    (testing "a new operation is an addition"
      (is (= :minor (:kind (compat/change old (facts (assoc-in api ["paths" "/api/b" "get"]
                                                               {"responses" {}})))))))))

(deftest a-version-step-admits-a-change
  (is (compat/admits? "1.0.0" "2.0.0" :breaking))
  (is (not (compat/admits? "1.0.0" "1.1.0" :breaking)))
  (is (compat/admits? "1.0.0" "1.1.0" :minor))
  (is (not (compat/admits? "1.0.0" "1.0.0" :minor)))
  (is (compat/admits? "1.0.0" "1.0.0" :none)))

(deftest protocols-have-a-surface
  (is (= #{[:method "P" "f" 1] [:method "P" "f" 2] [:method "P" "g" 1]}
         (set (compat/protocol-facts "(ns x)\n(defprotocol P \"doc\" (f [a] [a b]) (g [a]))"
                                     "P")))))

(deftest every-boundary-is-locked-at-its-version
  (let [reg  (bnd/register)
        lock (compat/lock)]
    (doseq [id (compat/versioned reg)
            :let [row (get-in reg [:boundaries id])
                  l   (get lock id)]]
      (testing (str id)
        (is (some? l) "not in the lock — run (grammar.boundary-compat-test/-main)")
        (is (= (:version row) (:version l)) "the register names another version than the lock")
        (is (= (:surface l) (compat/digest (compat/surface reg id)))
            "the surface moved — run (grammar.boundary-compat-test/-main) to classify it")))
    (testing "a versioned boundary says so"
      (is (= [] (vec (for [[id row] (:boundaries reg)
                           :when (and (= :done (get-in row [:status :versioned]))
                                      (not (contains? lock id)))]
                       id)))))))

(defn- at-head
  [path]
  (let [{:keys [exit out]} (sh/sh "git" "show" (str "HEAD:" path))]
    (when (zero? exit) out)))

(defn -main [& _]
  (let [reg   (bnd/register)
        old-r (or (some-> (at-head "resources/sutra/boundaries.edn") edn/read-string) reg)
        lock  (compat/lock)
        steps (for [id (compat/versioned reg)
                    :let [v    (get-in reg [:boundaries id :version])
                          now  (compat/surface reg id)
                          was  (when (get-in old-r [:boundaries id])
                                 (compat/surface old-r id at-head))
                          l    (get lock id)
                          ch   (if (and l was) (compat/change was now) {:kind :none})
                          ok?  (or (nil? l) (compat/admits? (:version l) v (:kind ch))
                                   (= (:surface l) (compat/digest now)))]]
                {:id id :from (:version l) :to v :change ch :ok? ok? :surface (compat/digest now)})
        bad   (remove :ok? steps)]
    (doseq [{:keys [id from to change]} steps
            :when (not= :none (:kind change))]
      (println (str id " " from " -> " to ": " (name (:kind change))
                    " (" (count (:removed change)) " removed, " (count (:added change))
                    " added, " (count (:tightened change)) " tightened)")))
    (if (seq bad)
      (do (doseq [{:keys [id from to change]} bad]
            (println (str "REFUSED " id ": a " (name (:kind change)) " change needs more than "
                          from " -> " to))
            (doseq [f (take 10 (concat (:removed change) (:tightened change)))]
              (println "  " (json/write-str f))))
          (System/exit 1))
      (do (spit compat/lock-file
                (with-out-str
                  (pp/pprint (into (sorted-map)
                                   (for [{:keys [id to surface]} steps]
                                     [id {:version to :surface surface}])))))
          (println "wrote" compat/lock-file)))))

(defn relock!
  [& _]
  (let [reg (bnd/register)]
    (spit compat/lock-file
          (with-out-str
            (pp/pprint (into (sorted-map)
                             (for [id (compat/versioned reg)]
                               [id {:version (get-in reg [:boundaries id :version])
                                    :surface (compat/digest (compat/surface reg id))}])))))
    (println "wrote" compat/lock-file)))
