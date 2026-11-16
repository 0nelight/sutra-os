(ns sutra-ui.debug)

(defmacro log
  [& args]
  (let [line (:line (meta &form))
        ns   (str (-> &env :ns :name))]
    `(sutra-ui.debug/record! :log ~(str ns ":" line)
                             (str ~@(interpose " " args)))))
