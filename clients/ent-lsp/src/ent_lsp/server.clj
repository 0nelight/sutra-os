(ns ent-lsp.server
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [ent-lsp.features :as feat]
            [ent-lsp.jsonrpc :as rpc]
            [ent-lsp.python :as python]
            [ent-lsp.spec :as spec]
            [ent-lsp.sutra :as sutra]
            [ent-lsp.workspace :as ws])
  (:import [java.net URI URLEncoder]))

(defonce state (atom {:ws nil :published {}}))
(def ^:private out System/out)
(defn- log [& xs] (binding [*out* *err*] (apply println "[ent-lsp]" xs)))
(defn- send! [msg] (rpc/write-message out msg))

(defn uri->path [uri] (.getPath (URI. uri)))

(defn path->uri [p]
  (str "file://" (str/join "/" (map #(.replace (URLEncoder/encode ^String % "UTF-8") "+" "%20")
                                    (str/split (str p) #"/" -1)))))

(defn- rel-of [uri]
  (let [root (get-in @state [:ws :root]) p (uri->path uri)]
    (if (and root (str/starts-with? p (str root "/"))) (subs p (inc (count root))) p)))

(defn- uri-of [rel]
  (path->uri (if (str/starts-with? rel "/") rel (str (get-in @state [:ws :root]) "/" rel))))

(defonce ^:private generation (atom 0))
(def ^:private analysis-lock (Object.))

(defn- publish! []
  (let [{:keys [ws published]} @state
        now (into {} (for [[rel fs] (:findings ws)] [(uri-of rel) (mapv feat/diagnostic fs)]))
        gone (remove now (keys published))]
    (doseq [[uri ds] now :when (not= ds (published uri))]
      (send! {:method "textDocument/publishDiagnostics" :params {:uri uri :diagnostics ds}}))
    (doseq [uri gone]
      (send! {:method "textDocument/publishDiagnostics" :params {:uri uri :diagnostics []}}))
    (swap! state assoc :published now)))

(defn- analyze-now! []
  (locking analysis-lock
    (let [t (System/nanoTime)]
      (swap! state update :ws ws/analyze)
      (log "analysiert in" (quot (- (System/nanoTime) t) 1000000) "ms")
      (publish!))))

(defn- schedule! [delay-ms]
  (let [g (swap! generation inc)]
    (future
      (try
        (Thread/sleep (long delay-ms))
        (when (= g @generation) (analyze-now!))
        (catch Throwable t (log "Analyse fehlgeschlagen:" (ex-message t)))))))

(defn- update-ws! [f & args] (swap! state (fn [s] (apply update s :ws f args))))

(def capabilities
  {:textDocumentSync        {:openClose true :change 1 :save {:includeText false}}
   :hoverProvider           true
   :definitionProvider      true
   :referencesProvider      true
   :documentSymbolProvider  true
   :workspaceSymbolProvider true
   :completionProvider      {:triggerCharacters [":"]}
   :codeActionProvider      {:codeActionKinds ["quickfix"]}
   :renameProvider          {:prepareProvider true}})

(defn- root-of [params]
  (or (some-> params :workspaceFolders first :uri uri->path)
      (some-> params :rootUri uri->path)
      (:rootPath params)
      "."))

(defn- in-scope? [rel]
  (let [w (:ws @state)]
    (and (not (str/starts-with? rel "/")) (ws/in-scope? w rel))))

(defn- pos [params] [(rel-of (get-in params [:textDocument :uri]))
                     (get-in params [:position :line])
                     (get-in params [:position :character])])

(defmulti handle (fn [msg] (:method msg)))

(defn- merge-capabilities
  [own py]
  (let [triggers (distinct (mapcat #(get-in % [:completionProvider :triggerCharacters])
                                   [own py]))]
    (-> (merge py own)
        (assoc-in [:completionProvider :triggerCharacters] (vec triggers))
        (cond-> (get-in py [:completionProvider :resolveProvider])
          (assoc-in [:completionProvider :resolveProvider] true)))))

(defn- pyright-argv []
  (let [cmd (System/getenv "ENT_LSP_PYRIGHT")]
    (cond (= "0" cmd) nil
          cmd (str/split cmd #"\s+")
          (fs/which "pyright-langserver") ["pyright-langserver" "--stdio"])))

(defn- start-python!
  [root params]
  (when-let [argv (and (python/has-python? root) (pyright-argv))]
    (log "Python über" (str/join " " argv))
    (try
      (let [py (python/start! argv [root] send!
                              #(do (log "pyright beendet, Python ist aus")
                                   (swap! state dissoc :py :triggers)))
            r  (python/request! py "initialize" params 30000)]
        (when r
          (swap! state assoc :py py
                 :triggers {:ent    (set (get-in capabilities [:completionProvider
                                                               :triggerCharacters]))
                            :python (set (get-in r [:capabilities :completionProvider
                                                    :triggerCharacters]))})
          (:capabilities r)))
      (catch java.io.IOException e
        (log "pyright startet nicht:" (ex-message e))))))

(defmethod handle "initialize" [{:keys [params]}]
  (let [root (str (fs/absolutize (root-of params)))]
    (log "Arbeitsbereich" root)
    (swap! state assoc :ws (assoc (ws/open-workspace (spec/load-spec) root)
                                  :suggest (sutra/suggester root))
           :published {})
    (future (try (analyze-now!) (catch Throwable t (log "Analyse fehlgeschlagen:" (ex-message t)))))
    {:capabilities (merge-capabilities capabilities (start-python! root params))
     :serverInfo   {:name "ent-lsp" :version "0.1"}}))

(defmethod handle "initialized" [_] nil)
(defmethod handle "shutdown" [_] nil)
(defmethod handle "exit" [_]
  (some-> (:py @state) python/stop!)
  (shutdown-agents)
  (System/exit 0))

(defonce ^:private request-id (atom 0))

(defn- request! [method params]
  (send! {:id (str "ent-lsp-" (swap! request-id inc)) :method method :params params}))

(defmethod handle "textDocument/didOpen" [{:keys [params]}]
  (let [{:keys [uri text]} (:textDocument params)
        rel (rel-of uri)]
    (update-ws! ws/set-text rel text (in-scope? rel))
    (when (str/blank? text)
      (when-let [t (feat/projekt-template (:ws @state) rel)]
        (request! "workspace/applyEdit"
                  {:label "Projekt-Gerüst"
                   :edit
                     {:changes {uri [{:range {:start {:line 0 :character 0}
                                              :end {:line 0 :character 0}}
                                      :newText t}]}}})))
    (schedule! 50)
    nil))

(defmethod handle "textDocument/didChange" [{:keys [params]}]
  (let [rel  (rel-of (get-in params [:textDocument :uri]))
        text (:text (last (:contentChanges params)))]
    (update-ws! ws/set-text rel text (in-scope? rel))
    (schedule! 300)
    nil))

(defmethod handle "textDocument/didSave" [_] nil)

(defmethod handle "textDocument/didClose" [{:keys [params]}]
  (let [rel (rel-of (get-in params [:textDocument :uri]))]
    (if (in-scope? rel)
      (update-ws! ws/reload-from-disk rel)
      (update-ws! update :files dissoc rel))
    (schedule! 50)
    nil))

(defmethod handle "workspace/didChangeWatchedFiles" [{:keys [params]}]
  (doseq [{:keys [uri]} (:changes params)
          :let [rel (rel-of uri)]
          :when (in-scope? rel)]
    (update-ws! ws/reload-from-disk rel))
  (schedule! 200)
  nil)

(defmethod handle "textDocument/hover" [{:keys [params]}]
  (let [[rel l c] (pos params)] (feat/hover (:ws @state) rel l c)))

(defmethod handle "textDocument/definition" [{:keys [params]}]
  (let [[rel l c] (pos params)] (feat/definition (:ws @state) uri-of rel l c)))

(defmethod handle "textDocument/references" [{:keys [params]}]
  (let [[rel l c] (pos params)]
    (feat/references (:ws @state) uri-of rel l c (get-in params [:context :includeDeclaration]))))

(defmethod handle "textDocument/completion" [{:keys [params]}]
  (let [[rel l c] (pos params)] (feat/completion (:ws @state) rel l c)))

(defmethod handle "textDocument/documentSymbol" [{:keys [params]}]
  (feat/document-symbols (:ws @state) (rel-of (get-in params [:textDocument :uri]))))

(defmethod handle "workspace/symbol" [{:keys [params]}]
  (feat/workspace-symbols (:ws @state) uri-of (:query params)))

(defmethod handle "textDocument/codeAction" [{:keys [params]}]
  (feat/code-actions (:ws @state) uri-of (rel-of (get-in params [:textDocument :uri]))
                     (get-in params [:context :diagnostics])))

(defmethod handle "textDocument/prepareRename" [{:keys [params]}]
  (let [[rel l c] (pos params)] (feat/prepare-rename (:ws @state) rel l c)))

(defmethod handle "textDocument/rename" [{:keys [params]}]
  (let [[rel l c] (pos params)] (feat/rename (:ws @state) uri-of rel l c (:newName params))))

(defmethod handle :default [msg]
  (when (:id msg) (throw (ex-info "Methode nicht unterstützt" {:code -32601}))))

(def ^:private both
  #{"initialized" "workspace/didChangeConfiguration" "workspace/didChangeWatchedFiles"
    "workspace/didChangeWorkspaceFolders" "$/setTrace"})

(defn- route
  [{:keys [id method params]}]
  (let [py (:py @state)
        uri (get-in params [:textDocument :uri])]
    (cond
      (nil? py) :ent
      (nil? method) (if (python/asked? py id) :python :ent)
      (both method) :both
      uri (if (str/ends-with? uri ".py") :python :ent)
      (contains? (methods handle) method) :ent
      :else :python)))

(defn- foreign-trigger?
  [side {:keys [method params]}]
  (let [{:keys [triggerKind triggerCharacter]} (:context params)]
    (when-let [own (get-in @state [:triggers side])]
      (and (= "textDocument/completion" method) (= 2 triggerKind)
           (not (contains? own triggerCharacter))))))

(defn- to-python! [msg]
  (try (python/forward! (:py @state) msg)
       (catch java.io.IOException e
         (log "pyright nicht erreichbar:" (ex-message e))
         (swap! state dissoc :py :triggers)
         (when (and (:id msg) (:method msg))
           (send! {:id (:id msg) :result nil})))))

(defn- not-ready? [msg]
  (and (nil? (:ws @state)) (:id msg) (:method msg) (not= "initialize" (:method msg))))

(defn- answer! [msg]
  (try
    (let [result (handle msg)]
      (when (:id msg) (send! {:id (:id msg) :result result})))
    (catch Throwable t
      (log (:method msg) "fehlgeschlagen:" (ex-message t))
      (when (:id msg)
        (send! {:id (:id msg)
                :error {:code (or (:code (ex-data t)) -32603)
                        :message (str (ex-message t))}})))))

(defn -main [& _]
  (loop []
    (when-let [msg (rpc/read-message System/in)]
      (cond
        (not-ready? msg) (send! {:id (:id msg)
                                 :error {:code -32002 :message "nicht initialisiert"}})
        :else
        (case (route msg)
          :python (if (foreign-trigger? :python msg)
                    (send! {:id (:id msg) :result []})
                    (to-python! msg))
          :both   (do (to-python! msg) (answer! msg))
          :ent    (cond (foreign-trigger? :ent msg) (send! {:id (:id msg) :result []})
                        (:method msg) (answer! msg))))
      (recur))))
