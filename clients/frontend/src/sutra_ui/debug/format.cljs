(ns sutra-ui.debug.format
  (:require [clojure.string :as str]))

(def level->tag
  {:meta "·meta" :log "LOG " :click "CLICK" :state "STATE"
   :api "API  " :api-error "API✗ " :perf "PERF "
   :visible "VIS  " :invisible "VIS✗ "
   :console-log "log " :console-warn "WARN" :console-error "ERR "
   :uncaught "UNCAUGHT" :promise-rejection "REJECT" :render-error "RENDER✗"})

(def with-stack
  #{:uncaught :promise-rejection :render-error})

(defn- pad [n width]
  (let [s (str n)] (str (apply str (repeat (max 0 (- width (count s))) "0")) s)))

(defn hhmmss [t]
  (let [d (js/Date. t)]
    (str (pad (.getHours d) 2) ":" (pad (.getMinutes d) 2) ":"
         (pad (.getSeconds d) 2) "." (pad (.getMilliseconds d) 3))))

(defn format-stack [stack]
  (let [lines (->> (str/split (str stack) #"\n") (map str/trim) (remove str/blank?))
        app   (filter #(str/includes? % "sutra_ui") lines)
        keep  (if (seq app)
                (distinct (cons (first lines) (take 10 app)))
                (take 5 lines))]
    (str/join "\n    " keep)))

(defn line [{:keys [t level src msg stack]}]
  (str (hhmmss t) " " (level->tag level (name level)) " [" src "] " msg
       (when (and stack (contains? with-stack level))
         (str "\n    " (format-stack stack)))
       "\n"))
