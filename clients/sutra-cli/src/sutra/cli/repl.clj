(ns sutra.cli.repl
  (:require [sutra.i18n.core :as i18n :refer [t]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.cli.auth :as auth]
            [sutra.cli.contract :as contract]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.errors :as errors]
            [sutra.cli.live :as live]
            [sutra.cli.out :as out]
            [sutra.cli.parse :as parse]
            [sutra.cli.readline :as readline]
            [sutra.cli.session :as session]))

(defn- discover-identity
  []
  (try
    (let [r (api/get! :me)]
      (when (errors/ok? r)
        (let [b (:body r)]
          (when (:username b) {:username (:username b) :role (:role b)}))))
    (catch Exception e (errors/rethrow-fatal! e) nil)))

(defn- refresh-staged! [sess]
  (try
    (let [r (api/get! :staging)]
      (when (errors/ok? r)
        (session/set-staged! sess (count (:staged (:body r))))))
    (catch Exception e (errors/rethrow-fatal! e) nil)))

(def ^:private read-verbs
  #{:explore :around :links :dependents :runs :health :audit :describe :show :query :review :check
    :eval
    :guide :explain :context :lint :overview :apps :projects :run :jobs
    :transactions :consumption :delta :edit-history :egress :access
    :sources :source :source-tables :source-columns :peek :source-sql :source-graph})

(defmacro ^:private in-session
  [ctx sess & body]
  `(binding [tx/*auth-token* @(:token ~ctx)
             tx/*branch*     (:branch @~sess)
             tx/*project*    (:project @~sess)
             tx/*headers*    (when-let [o# (:ontology @~sess)] {"x-sutra-ontology" (str o#)})]
     ~@body))

(defn with-session
  ([opts f] (with-session opts f true))
  ([{:keys [base-url] :as opts} f interactive?]
   (when base-url (tx/set-base! base-url))
   (let [sess (session/new-session opts)
         ctx  (dispatch/new-ctx (assoc opts :session sess))]
     (binding [tx/*auth-token* nil]
       (auth/ensure-prod-login! ctx))
     (swap! sess assoc :interactive? interactive?)
     (if interactive?
       (in-session ctx sess
                   (when-let [id (discover-identity)]
                     (session/set-identity! sess id))
                   (refresh-staged! sess))
       (session/set-identity! sess (delay (in-session ctx sess (discover-identity)))))
     (f ctx sess))))

(defn- capture!
  [sess ret]
  (when (map? ret)
    (case (:kind ret)
      :objects (session/put-register! sess (:rows ret))
      :branch  (session/set-branch! sess (:branch ret))
      :project (session/set-project! sess (:project ret))
      nil)))

(declare watch-eval!)

(defn- render-next!
  [nexts]
  (let [verbs (seq (keep :verb nexts))
        needs (seq (filter :need nexts))]
    (when verbs
      (println (str "  " (out/paint :dim (str (t :cli/repl-next) " → "))
                    (str/join (out/paint :dim "  ·  ")
                              (map #(out/paint :cyan (name %)) verbs)))))
    (doseq [{:keys [need options]} needs]
      (println (str "  " (out/paint :dim (str (t :cli/repl-need) " "))
                    (out/paint :cyan (name need))
                    (when (seq options)
                      (str (out/paint :dim " = ") (str/join "|" options))))))))

(defn- render-result!
  [ctx {:keys [errors warnings next]}]
  (doseq [e errors]
    (out/warn! (out/mark :red "✗" (:msg e)))
    (when-let [fix (:fix e)] (out/warn! (str "    " (out/paint :dim fix)))))
  (when-not (out/json? ctx)
    (doseq [w warnings]
      (out/say! ctx (out/mark :yellow "⚠" (:msg w))))
    (render-next! next)))

(defn- run-step!
  [ctx sess [verb :as step]]
  (if (and (= verb :eval) (:watch? (second step)))
    (watch-eval! ctx sess step)
    (let [res (in-session ctx sess (contract/run ctx step))]
      (capture! sess (:state res))
      (when (and (:interactive? @sess) (not (read-verbs verb)))
        (in-session ctx sess (refresh-staged! sess)))
      (render-result! ctx res)
      (:state res))))

(defn- print-error!
  [e]
  (errors/rethrow-fatal! e)
  (out/warn! (errors/explain-error
              e (or (not-empty (.getMessage e))
                    (t :cli/repl-backend-reachable {:klasse (.getName (class e))}))))
  (when-let [head (:verb (ex-data e))]
    (when-not (:file-only? (ex-data e))
      (when-let [near (seq (contract/suggest-verb head))]
        (out/warn! (str "  " (out/paint :dim (str (t :cli/repl-did-you-mean) " "))
                        (str/join (out/paint :dim " / ")
                                  (map #(out/paint :cyan (name %)) near))
                        (out/paint :dim " ?")))))))

(defn- mtime [path]
  (let [f (io/file (str path))] (when (.exists f) (.lastModified f))))

(defn- watch-eval!
  [ctx sess [_ args]]
  (let [step  [:eval (dissoc args :watch?)]
        file  (:suite args)
        run!  (fn [] (try (run-step! ctx sess step)
                          (catch Exception e (print-error! e))))]
    (println (out/mark :cyan "◉" (t :cli/repl-watching {:datei (out/paint :bold (str file))})))
    (run!)
    (loop [prev (mtime file)]
      (Thread/sleep 400)
      (if (pos? (.available System/in))
        (do (read-line) (println (out/mark :dim "◌" (t :cli/repl-watch-stopped))) nil)
        (let [now (mtime file)]
          (when (and now prev (not= now prev))
            (println (out/paint :dim (str "  ↻ " (t :cli/repl-file-changed {:datei file}))))
            (run!))
          (recur (or now prev)))))))

(defn- typed-confirm?
  [ctx word]
  (cond
    (:yes? ctx) true
    (not (out/tty?))
    (do (println (str "  " (out/paint :yellow "⏸ ")
                      (t :cli/repl-no-terminal {:zweig (out/paint :red "main")
                                                :rat (out/paint :cyan (t :cli/repl-pass-yes))})))
        false)
    :else
    (do (print (str "  " (t :cli/repl-type-to-save {:wort (out/paint [:bold :red] word)
                                                    :zweig (out/paint :red "main")}) ": ")) (flush)
        (= (str/trim (or (read-line) "")) word))))

(defn- save!
  [ctx sess]
  (in-session ctx sess
              (let [{:keys [valid? problems staged blast]} (live/check-report)]
                (cond
                  (zero? (or staged 0))
                  (println (out/mark :dim "•" (t :cli/repl-nothing-staged)))

                  (false? valid?)
                  (do (println (out/mark :red "✗" (t :cli/repl-refusing-invalid)))
                      (doseq [p (or problems [])]
                        (println (str "    " (out/paint :red "·") " " (pr-str p))))
                      (println (out/mark :dim "→" (t :cli/repl-fix-before-saving))))

                  :else
                  (let [_        (live/render-blast! ctx blast)
                        on-main? (= (:branch @sess) "main")
                        go? (if on-main?
                              (typed-confirm? ctx "main")
                              (out/prompt-yes? ctx (t :cli/repl-save-n-changes
                                                      {:n staged
                                                       :zweig (out/paint :bold (:branch @sess))})))]
                    (if-not go?
                      (println (out/mark :yellow "⏸" (t :cli/repl-save-cancelled)))
                      (do (errors/req! "save" (api/post! :staging))
                          (session/set-staged! sess 0)
                          (println (out/mark :green "✓" (t :cli/repl-saved))))))))))

(defn- status!
  [sess]
  (when-not (:interactive? @sess) (refresh-staged! sess))
  (let [{:keys [branch staged]} @sess
        identity (session/identity-of sess)
        body (fn [what f]
               (let [r (errors/optional what f)] (when (errors/ok? r) (:body r))))]
    (out/show!
     {:branch   branch
      :staged   (or staged 0)
      :as       (:username identity)
      :role     (:role identity)
      :health   (when-let [res (:results (body "health" #(api/post! :health-run-all)))]
                  (str (count (filter #(= "passed" (:status %)) res)) "/" (count res)))
      :last-run (some-> (body "runs" api/list-runs) :runs first
                        (select-keys [:kind :status :started-at]))
      :inbox    (some->> (body "proposals" #(api/list-proposals {:status "open"})) :proposals
                         (mapv #(select-keys % [:source-branch :target-branch])))})))

(defn- handle-meta!
  [ctx sess {:keys [meta args]}]
  (case meta
    :help    (do (println (str "  " (out/paint :bold (str (t :cli/repl-commands) ": "))
                               (str/join "  " parse/verbs)))
                 (println (str "  " (out/paint :dim "→ ")
                               (out/paint :cyan "guide")
                               (out/paint :dim (str " " (t :cli/repl-guide-does) " · "))
                               (out/paint :cyan "explain <verb|concept>")
                               (out/paint :dim (str " " (t :cli/repl-explain-does)))))
                 (println (str "  "
                               (out/paint :dim
                                          (str (t :cli/repl-pronouns)
                                               ": #n  #a-b  $  $name  @me")))))
    :status  (status! sess)
    :usage   (let [v (first args)]
               (if-let [u (parse/usage v)]
                 (println (str "  " (out/paint :dim (str (t :cli/repl-usage) ": ")) u))
                 (println (out/mark :yellow "⏸"
                                    (t :cli/repl-no-usage {:verb v})))))
    :save    (save! ctx sess)
    :quit    :quit
    :sets    (let [named (get-in @sess [:register :named])]
               (out/show! (into (sorted-map) (map (fn [[k v]] [(str "$" k) (count v)])) named)))
    :save-set (if-let [nm (first args)]
                (let [s (session/save-set! sess nm)]
                  (println (out/mark :green "✓"
                                     (t :cli/repl-saved-set
                                        {:name (out/paint :cyan (str "$" nm)) :n (count s)}))))
                (println (out/mark :yellow "⏸" (str (t :cli/repl-usage) ": save-set <name>"))))
    :discard  (let [r (api/delete! :staging)]
                (if (errors/ok? r)
                  (do (session/set-staged! sess 0)
                      (println (out/mark :red "−" (t :cli/repl-discarded))))
                  (println (out/mark :yellow "⏸" (t :cli/repl-nothing-to-discard)))))
    (println (out/mark :yellow "⏸" (t :cli/repl-unknown-command {:befehl meta})))))

(defn- line-ctx
  [ctx g]
  (when (:no-color? g) (out/no-colour!))
  (some-> (:lang g) i18n/from-tag i18n/set-locale!)
  (cond-> ctx
    (:yes? g)    (assoc :yes? true)
    (:strict? g) (assoc :strict? true)
    (:json? g)   (assoc :json? true)
    (:text? g)   (assoc :json? false)
    (:fields g)  (assoc :fields (:fields g))))

(defn run-line-with!
  [ctx sess line]
  (try
    (let [parsed (parse/line->cmds sess line)]
      (cond
        (:blank parsed) 0
        (:meta parsed)
        (let [ctx (line-ctx ctx (:globals parsed))]
          (in-session ctx sess (handle-meta! ctx sess parsed)) 0)
        :else
        (let [ctx (line-ctx ctx (:globals parsed))]
          (reduce (fn [code step]
                    (case (run-step! ctx sess step)
                      :abort (reduced 2)
                      :fail  1
                      code))
                  0 (:steps parsed)))))
    (catch Exception e (print-error! e) 1)))

(defn dispatch-line!
  [ctx sess line]
  (try
    (let [parsed (parse/line->cmds sess line)]
      (cond
        (:blank parsed) nil
        (:meta parsed)  (let [ctx (line-ctx ctx (:globals parsed))]
                          (in-session ctx sess (handle-meta! ctx sess parsed)))
        :else           (let [ctx (line-ctx ctx (:globals parsed))]
                          (doseq [step (:steps parsed)] (run-step! ctx sess step))
                          nil)))
    (catch Exception e (print-error! e) nil)))

(defn- banner [ctx sess]
  (println (out/paint :bold "sutra") (t :cli/repl-banner)
           (out/paint :dim (t :cli/repl-banner-hint)))
  (when (= "main" (:branch @sess))
    (println (out/mark :red "●" (out/paint :dim (t :cli/repl-on-main)))))
  (println)
  (try
    (binding [tx/*auth-token* @(:token ctx)] (live/print-landing! ctx))
    (catch Exception _ (out/print-start-here! ctx true))))

(defn run-landing!
  [opts]
  (try
    (with-session
      opts
      (fn [ctx _sess]
        (binding [tx/*auth-token* @(:token ctx)] (live/print-landing! ctx))
        0))
    (catch Exception e
      (errors/rethrow-fatal! e)
      (out/warn! (out/mark :yellow "ℹ"
                           (str (t :cli/repl-backend-unreachable)
                                (when-let [m (not-empty (.getMessage e))] (str " (" m ")")))))
      (dispatch/step! (dispatch/new-ctx opts) [:guide {}])
      0)))

(defn start!
  [opts]
  (with-session
    opts
    (fn [ctx sess]
      (banner ctx sess)
      (let [reader (readline/make-reader sess)]
        (try
          (loop []
            (let [line (readline/read-line! reader (session/prompt sess))]
              (if (nil? line)
                (do (println) 0)
                (if (= :quit (dispatch-line! ctx sess line))
                  0
                  (recur)))))
          (finally (readline/close! reader)))))))

(defn- branch-exists?
  [branch]
  (try
    (let [r (api/list-branches {})]
      (when (errors/ok? r)
        (let [names (into #{} (map (comp str :name)) (:branches (:body r)))]
          {:exists? (contains? names (str branch))
           :names   names})))
    (catch Exception e (errors/rethrow-fatal! e) nil)))

(defn- checkout-first!
  [ctx sess branch]
  (if (str/blank? (str branch))
    true
    (let [{:keys [exists? names]} (binding [tx/*auth-token* @(:token ctx)]
                                    (branch-exists? branch))]
      (if (false? exists?)
        (do (out/fail!
             (out/mark :red "✗"
                       (str (t :cli/repl-no-such-branch {:zweig (pr-str (str branch))})
                            (when-let [near (->> names
                                                 (filter #(or (str/starts-with? % (str branch))
                                                              (str/includes? % (str branch))))
                                                 sort seq)]
                              (t :cli/repl-branch-did-you-mean
                                 {:namen (str/join ", " (take 3 near))}))))
             (out/mark :dim "•" (t :cli/repl-nothing-ran)))
            false)
        (do (out/warn! (out/mark :dim "•" (str "branch " branch)))
            (session/set-branch! sess (str branch))
            true)))))

(defn run-lines!
  [opts lines]
  (with-session
    opts
    (fn [ctx sess]
      (when-not (checkout-first! ctx sess (:branch opts))
        (System/exit 1))
      (doseq [line lines
              :when (not (str/blank? line))
              :when (not (str/starts-with? (str/trim line) "#"))
              :while (not= :quit (dispatch-line! ctx sess line))])
      0)
    false))

(defn run-once!
  [opts line]
  (with-session opts
    (fn [ctx sess]
      (if (checkout-first! ctx sess (:branch opts))
        (run-line-with! ctx sess line)
        1))
    false))
