(ns ^:unit platform.i18n-bundles-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.i18n.inline :as inline]))

(defn- app-bundles
  [dir locale]
  (for [d (.listFiles (io/file dir "sutra_ui/apps"))
        :let [f (io/file d (str "i18n." (name locale) ".edn"))]
        :when (.exists f)]
    (edn/read-string (slurp f))))

(defn- bundle [dir surface locale]
  (let [rel (str "i18n/" surface "." (name locale) ".edn")]
    (apply merge (edn/read-string (slurp (or (io/resource rel) (io/file dir rel))))
           (when (= "ui" surface) (app-bundles dir locale)))))

(defn- keys-used
  [dir prefix]
  (into #{}
        (comp (filter #(re-find #"\.clj[cs]?$" (str %)))
              (remove #(re-find #"_test\.clj[cs]?$" (str %)))
              (mapcat #(re-seq (re-pattern (str prefix "/[a-zäöüß0-9-]+")) (slurp %)))
              (map #(keyword (subs % 1))))
        (file-seq (io/file dir))))

(deftest jeder-benutzte-schlüssel-ist-übersetzt
  (doseq [[dir prefix surface] [["clients/sutra-cli/src" ":cli" "cli"]
                                ["clients/sutra-change/src" ":change" "change"]
                                ["clients/frontend/src" ":ui" "ui"]
                                ["src" ":refusal\\.[a-z-]+" "refusal"]]
          :when (.exists (io/file dir))]
    (let [vorhanden (set (mapcat #(keys (bundle dir surface %)) inline/locales))
          benutzt   (keys-used dir prefix)]
      (is (empty? (remove vorhanden benutzt))
          (str dir " fragt ohne Übersetzung: " (sort (remove vorhanden benutzt)))))))

(defn- hints [suffix]
  (edn/read-string (slurp (io/resource (str "cli/hints" suffix ".edn")))))

(deftest die-hint-ueberlagerung-erfindet-keine-verben
  (let [base (hints "") ]
    (doseq [l (remove #{:en} inline/locales)
            :let [ov (hints (str "." (name l)))]
            :when ov
            sek [:verbs :concepts]]
      (is (empty? (remove (set (keys (get base sek))) (keys (get ov sek))))
          (str "hints." (name l) ".edn " sek " nennt, was es nicht gibt: "
               (sort (remove (set (keys (get base sek))) (keys (get ov sek)))))))))

(deftest jedes-verb-hat-in-jeder-sprache-ein-purpose
  (let [base (hints "")]
    (doseq [l (remove #{:en} inline/locales)
            :let [ov (hints (str "." (name l)))]
            :when ov
            sek [:verbs :concepts]]
      (is (empty? (remove #(seq (:purpose (get (get ov sek) %))) (keys (get base sek))))
          (str "hints." (name l) ".edn " sek " ohne :purpose: "
               (sort (remove #(seq (:purpose (get (get ov sek) %))) (keys (get base sek)))))))))

