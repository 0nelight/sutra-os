(ns ^:unit grammar.boundaries-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.foundation.boundaries :as bnd]
            [sutra.types.paths :as paths]))

(defn- capabilities []
  (-> "libs/dialect/resources/sutra/kit/plugin.malli.edn" slurp edn/read-string :capabilities keys
      set))

(defn- ops [] (set (keys (edn/read-string (slurp "resources/sutra/ops.edn")))))

(def ^:private pillars [:spec :versioned :conformance])

(def ^:private statuses #{:done :partial :none})

(def ^:private forms #{:openapi :json-schema :malli :protocol :prose})

(deftest every-row-is-complete
  (doseq [[id row] (:boundaries (bnd/register))]
    (testing (str id)
      (is (pos-int? (:n row)))
      (is (#{:external :component} (:class row)))
      (is (re-matches #"\d+\.\d+\.\d+" (str (:version row))) "a semantic version")
      (is (forms (get-in row [:spec :form])))
      (is (seq (:standards row)) "names the open standards it rests on")
      (is (= #{} (set (remove statuses (map #(get-in row [:status %]) pillars)))))
      (is (#{:unlimited :gpr} (:rights row)))
      (is (seq (str/trim (str (:about row))))))))

(deftest numbers-are-a-sequence
  (let [ns (sort (map (comp :n val) (:boundaries (bnd/register))))]
    (is (= (range 1 (inc (count ns))) ns))))

(deftest every-named-file-exists
  (let [reg (bnd/register)]
    (is (= [] (vec (remove (fn [[_ f]] (.exists (io/file f))) (bnd/named-files reg)))))
    (testing "conformance evidence names files that exist"
      (is (= [] (vec (for [[id row] (:boundaries reg)
                           f (:conformance row)
                           :when (not (.exists (io/file f)))]
                       [id f])))))
    (testing "a boundary claiming conformance names its evidence"
      (is (= [] (vec (for [[id row] (:boundaries reg)
                           :when (and (not= :none (get-in row [:status :conformance]))
                                      (empty? (:conformance row)))]
                       id)))))
    (testing "a finished spec ships its generated artifacts"
      (is (= [] (vec (for [[id row] (:boundaries reg)
                           :when (= :done (get-in row [:status :spec]))
                           f (get-in row [:spec :generated])
                           :when (not (.exists (io/file f)))]
                       [id f])))))))

(deftest every-endpoint-belongs-to-one-boundary
  (let [reg (bnd/register)
        by  (bnd/endpoints-by-boundary reg paths/endpoints)]
    (is (= #{} (get by nil #{})) "endpoints no boundary claims")
    (testing "every prefix claims an endpoint"
      (is (= [] (vec (for [[id row] (:boundaries reg)
                           p (get-in row [:covers :url-prefixes])
                           :when (not-any? #(str/starts-with? (str (:url %)) p)
                                           (vals paths/endpoints))]
                       [id p])))))
    (testing "two boundaries never claim the same prefix"
      (is (= [] (vec (for [[p n] (frequencies (mapcat #(get-in (val %) [:covers :url-prefixes])
                                                      (:boundaries reg)))
                           :when (< 1 n)]
                       p)))))))

(deftest every-capability-belongs-to-one-boundary
  (let [reg      (bnd/register)
        claims   (bnd/claimed-capabilities reg)
        internal (set (keys (:internal-capabilities reg)))
        all      (capabilities)]
    (is (= #{} (set (remove (into internal (map first claims)) all)))
        "capabilities neither a boundary claims nor the register calls internal")
    (is (= #{} (set (remove all (into internal (map first claims)))))
        "claims of capabilities the plugin registry does not know")
    (is (= [] (vec (for [[c n] (frequencies (concat (map first claims) internal))
                         :when (< 1 n)]
                     c)))
        "a capability is claimed once")))

(deftest every-protocol-of-an-boundary-file-is-claimed
  (let [reg    (bnd/register)
        claims (set (map first (bnd/claimed-protocols reg)))]
    (doseq [f (bnd/protocol-files reg)
            :let [declared (bnd/protocols-in (slurp f))]]
      (testing f
        (is (= #{} (set (remove #(claims [f %]) declared))) "declared but not claimed")
        (is (= #{} (set (remove declared (keep (fn [[cf p]] (when (= cf f) p)) claims))))
            "claimed but not declared")))))

(deftest the-operations-belong-to-an-boundary
  (let [rows (filter #(= :all (get-in (val %) [:covers :ops])) (:boundaries (bnd/register)))]
    (is (= 1 (count rows)) "exactly one boundary claims the operation table")
    (is (seq (ops)))))

