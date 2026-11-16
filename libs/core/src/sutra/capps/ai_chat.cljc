(ns sutra.capps.ai-chat
  (:require [clojure.string :as str]))

(defn running?
  [thread]
  (boolean (some #(= "running" (:status %)) (:messages thread))))

(defn- state-line
  [[k {:keys [description value]}]]
  (str "- " (name k)
       (when-not (str/blank? (str description)) (str " (" description ")"))
       ": " (pr-str value)))

(defn input-with-state
  [input state]
  (if (seq state)
    (str "Application state:\n" (str/join "\n" (map state-line (sort-by key state)))
         "\n\n" input)
    input))

(defn question
  [{:keys [agent version]} input zone]
  (cond-> {:agent (name agent) :input input}
    (not (str/blank? (str version))) (assoc :version (str version))
    zone                             (assoc :zone zone)))

(defn auto-send?
  [textbox typed]
  (and (not (str/blank? (str textbox))) (not= textbox typed)))

(defn last-answer
  [thread]
  (some #(when (and (= "assistant" (:role %)) (not= "running" (:status %))) (:content %))
        (rseq (vec (:messages thread)))))

(defn steps-of
  [message]
  (vec (get-in message [:detail :steps])))

(defn open-proposals
  [thread]
  (filterv #(= "open" (str (some-> (:status %) name))) (:proposals thread)))

(defn function-output
  [rows {:keys [column as-rows?]}]
  (let [row (first rows)
        cell (when (map? row)
               (or (when column (get row (keyword (name column))))
                   (let [vs (vals (or (:values row) row))] (when (= 1 (count vs)) (first vs)))))]
    (cond
      as-rows?    (vec rows)
      (some? cell) (str cell)
      :else       (vec rows))))

(defn clamped-temperature
  [t]
  (when (number? t) (max 0 (min 1 t))))

(defn generate-mode
  [{:keys [logic prompt prompt-function]}]
  (cond
    logic           :logic
    prompt-function :prompt-function
    (some? prompt)  :prompt))

(defn generate-transition
  [st [ev x]]
  (case ev
    :start  (assoc st :status :loading :error nil)
    :answer (assoc st :status :done :text x :error nil)
    :fail   (assoc st :status :failed :error x)
    :prompt (cond-> (assoc st :prompt x)
              (and (:clear? st) (contains? st :prompt) (not= x (:prompt st)))
              (assoc :status :idle :text nil :error nil))
    st))
