(ns sutra.capps.recorder)

(def initial {:phase :idle})

(defn step
  [{:keys [phase] :as s} [kind x]]
  (case [phase kind]
    [:idle :record]        [(assoc s :phase :asking :error nil) [[:start]]]
    [:asking :started]     [(assoc s :phase :recording) nil]
    [:asking :failed]      [(assoc s :phase :idle :error x) nil]
    [:recording :pause]    [(assoc s :phase :paused) [[:pause]]]
    [:paused :record]      [(assoc s :phase :recording) [[:resume]]]
    [:recording :stop]     [(assoc s :phase :stopping) [[:stop]]]
    [:paused :stop]        [(assoc s :phase :stopping) [[:stop]]]
    [:stopping :stopped]   [(assoc s :phase :done :url (:url x) :name (:name x)) nil]
    [:done :discard]       [initial [[:forget (:url s)]]]
    [:done :save]          [(assoc s :phase :saving :error nil) [[:upload (:name s)]]]
    [:saving :saved]       [(assoc initial :saved (:name s)) [[:forget (:url s)]]]
    [:saving :failed]      [(assoc s :phase :done :error x) nil]
    [s nil]))

(def ^:private formats
  [["audio/webm" "webm"] ["audio/mp4" "m4a"] ["audio/ogg" "ogg"]])

(defn format-for
  [supported?]
  (some #(when (supported? (first %)) %) formats))

(defn recording-name
  [iso extension]
  (str "aufnahme-" (subs (str iso) 0 19) "." extension))

(defn action-params
  [{:keys [media-param name-param]} hash filename]
  (cond-> {}
    media-param (assoc media-param hash)
    name-param  (assoc name-param filename)))
