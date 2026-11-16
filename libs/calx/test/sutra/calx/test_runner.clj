(ns sutra.calx.test-runner
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- test-namespaces []
  (->> (file-seq (io/file "test"))
       (filter #(str/ends-with? (.getName ^java.io.File %) "_test.clj"))
       (map #(-> (.getPath ^java.io.File %)
                 (str/replace #"^test/" "")
                 (str/replace #"\.clj$" "")
                 (str/replace "/" ".")
                 (str/replace "_" "-")
                 symbol))
       sort))

(defn -main [& patterns]
  (let [nss (cond->> (test-namespaces)
              (seq patterns) (filter (fn [ns] (some #(re-find (re-pattern %) (str ns)) patterns))))]
    (doseq [ns nss] (require ns))
    (let [{:keys [fail error]} (apply t/run-tests nss)]
      (shutdown-agents)
      (System/exit (if (zero? (+ fail error)) 0 1)))))
