(ns conformance.report
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [conformance.json :as json]))

(defn skip [reason] {::skip reason})

(defn check
  [id title f]
  (let [r (try (f) (catch Throwable e (str (.getSimpleName (class e)) ": " (.getMessage e))))]
    (cond
      (or (nil? r) (true? r)) {:id id :title title :status :pass}
      (and (map? r) (contains? r ::skip)) {:id id :title title :status :skip
                                           :detail (str (::skip r))}
      :else {:id id :title title :status :fail :detail (str r)})))

(defn failed? [checks] (boolean (some #(= :fail (:status %)) checks)))

(defn counts [checks] (merge {:pass 0 :fail 0 :skip 0} (frequencies (map :status checks))))

(defn print-run
  [kit target checks]
  (println (str "conformance kit " kit " against " target))
  (doseq [{:keys [id title status detail]} checks]
    (println (format "  %-4s %s — %s%s" (str/upper-case (name status)) id title
                     (if detail (str "\n         " detail) ""))))
  (let [{:keys [pass fail skip]} (counts checks)]
    (println (format "%d passed, %d failed, %d skipped" pass fail skip))))

(defn- xml-escape [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn junit-xml
  [kit checks]
  (let [{:keys [fail skip]} (counts checks)]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
         "<testsuite name=\"" (xml-escape kit) "\" tests=\"" (count checks)
         "\" failures=\"" fail "\" skipped=\"" skip "\">\n"
         (str/join
          (for [{:keys [id title status detail]} checks]
            (str "  <testcase classname=\"" (xml-escape kit) "\" name=\"" (xml-escape id) "\">"
                 (case status
                   :fail (str "<failure message=\"" (xml-escape title) "\">"
                              (xml-escape detail) "</failure>")
                   :skip (str "<skipped message=\"" (xml-escape detail) "\"/>")
                   "")
                 "</testcase>\n")))
         "</testsuite>\n")))

(defn summary
  [kit target checks]
  {"kit" kit "target" target "counts" (into {} (map (fn [[k v]] [(name k) v])) (counts checks))
   "passed" (not (failed? checks))
   "checks" (mapv (fn [{:keys [id title status detail]}]
                    (cond-> {"id" id "title" title "status" (name status)}
                      detail (assoc "detail" detail)))
                  checks)})

(defn write-results!
  [dir kit target checks]
  (let [d (io/file dir)
        x (io/file d (str kit ".junit.xml"))
        j (io/file d (str kit ".summary.json"))]
    (.mkdirs d)
    (spit x (junit-xml kit checks))
    (spit j (json/write (summary kit target checks)))
    [(str x) (str j)]))
