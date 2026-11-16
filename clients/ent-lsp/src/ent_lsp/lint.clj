(ns ent-lsp.lint
  (:require [clojure.string :as str]
            [ent-lsp.analysis :as an]
            [ent-lsp.diff :as diff]
            [ent-lsp.spec :as spec]
            [ent-lsp.workspace :as ws]))

(def ^:private rank {:error 0 :warning 1 :information 2 :hint 3})

(defn- parse-args [args]
  (loop [[a & more] args opts {}]
    (cond (nil? a)          opts
          (= "--alle" a)    (recur more (assoc opts :all? true))
          (= "--gegen" a)   (recur (rest more) (assoc opts :base (first more)))
          (= "--datei" a)   (recur (rest more) (update opts :files (fnil conj #{}) (first more)))
          :else             (recur more (assoc opts :root a)))))

(defn -main [& args]
  (let [{:keys [all? root base files] :or {root "."}} (parse-args args)
        spec  (spec/load-spec)
        w     (ws/analyze (ws/open-workspace spec root))
        mine? (if files
                (let [rels (into #{} (map #(ws/rel-path root %)) files)] (comp rels :file))
                (constantly true))
        old   (when base
                (or (diff/index-at w base)
                    (do (binding [*out* *err*] (println (str "Revision " base " nicht lesbar")))
                        (System/exit 2))))
        fs    (->> (concat (apply concat (vals (:findings w)))
                           (when old
                             (map #(an/complete spec %) (diff/findings spec old (:index w) base))))
                   (filter mine?)
                   (filter #(or all? (<= (rank (:severity %)) (rank :information))))
                   (sort-by (juxt :file (comp :row :loc) (comp :col :loc))))]
    (doseq [{:keys [file loc severity rule message]} fs]
      (println (str file ":" (:row loc) ":" (:col loc) ": " (name severity) " [" (name rule) "] "
                    message)))
    (binding [*out* *err*]
      (println (str (count fs) " Befunde — "
                    (->> fs (map :rule) frequencies (sort-by (comp - val))
                         (map (fn [[r n]] (str (name r) " " n))) (str/join ", ")))))
    (System/exit (if (some #(= :error (:severity %)) fs) 1 0))))
