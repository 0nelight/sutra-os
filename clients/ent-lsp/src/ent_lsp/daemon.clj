(ns ent-lsp.daemon
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [ent-lsp.online :as online]
            [ent-lsp.place :as place]
            [sutra.change.server :as srv]
            [ent-lsp.spec :as spec]
            [ent-lsp.sutra :as sutra]
            [ent-lsp.workspace :as ws])
  (:import [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio.channels Channels FileChannel ServerSocketChannel SocketChannel]
           [java.nio.file OpenOption StandardOpenOption]))

(def ^:private poll-ms 30000)
(def ^:private rescan-ms 15000)
(def ^:private idle-ms (* 8 3600 1000))
(def ^:private shown-max 30)
(def ^:private elsewhere-max 10)
(def ^:private said-max 5)
(def ^:private clip-at 160)

(defn- clip [s] (let [s (str s)] (if (> (count s) clip-at) (str (subs s 0 clip-at) "…") s)))

(defonce ^:private state (atom nil))
(def ^:private lock (Object.))
(defn- log [& xs]
  (binding [*out* *err*] (apply println (str "[" (java.time.LocalTime/now) "]") xs)))
(defn- now [] (System/currentTimeMillis))

(defn- mtime [root rel]
  (let [f (fs/file root rel)] (when (.isFile f) (.lastModified f))))

(defn- in-scope? [w rel]
  (and (not (str/starts-with? rel "/")) (ws/in-scope? w rel)))

(defn sync-disk
  [{:keys [ws mtimes] :as st} wanted]
  (let [root (:root ws)
        rels (cond-> (set (keys (:files ws))) wanted (conj wanted))]
    (reduce (fn [st rel]
              (let [m (mtime root rel)]
                (cond (nil? m)
                        (-> st (update-in [:ws :files] dissoc rel) (update :mtimes dissoc rel))
                      (= m (get mtimes rel))          st
                      :else (-> st
                                (update :ws ws/set-text rel (slurp (fs/file root rel))
                                        (in-scope? ws rel))
                                (assoc-in [:mtimes rel] m)))))
            st rels)))

(defn- rescan [{:keys [ws] :as st}]
  (let [known (set (keys (:files ws)))]
    (reduce #(sync-disk %1 %2) st (remove known (ws/scan ws)))))

(def ^:private rank {:error 0 :warning 1 :information 2 :hint 3})

(defn- line [{:keys [file loc severity rule message]}]
  (str file ":" (:row loc) ":" (:col loc) ": " (name severity) " [" (name rule) "] " message))

(defn- by-place [fs] (sort-by (juxt :file (comp :row :loc) (comp :col :loc)) fs))
(defn- identity-of [f] [(:file f) (:loc f) (:rule f) (:message f)])

(def ^:private section->kind
  {:object-types :object :link-types :link :actions :action :interfaces :interface
   :shared-properties :shared-property :value-types :value-type :projects :project
   :repositories :repository :datasets :dataset :sources :source :transforms :transform
   :functions :function :pipelines :pipeline :object-sets :object-set
   :collections :collection :workspaces :workspace
   :custom-applications :custom-application :applications :application})

(defn- ago [ms] (let [s (quot ms 1000)] (if (< s 120) (str s " s") (str (quot s 60) " min"))))

(defn- instance-line [{:keys [online-status]} t]
  (let [{:keys [rev checked-at error]} online-status]
    (cond rev   (str "Instanz: Stand " rev ", geprüft vor " (ago (- t checked-at))
                     (when error (str " — letzte Abfrage gescheitert: " error)))
          error (str "Instanz: nicht gespiegelt — " error)
          :else "Instanz: Spiegel wird geladen")))

(defn render
  [{:keys [rel before after defs lint all?] :as st} t]
  (let [own      (by-place (get after rel))
        shown    (if all? own (filter #(<= (rank (:severity %)) (rank :information)) own))
        hints    (remove (set shown) own)
        old      (into #{} (map identity-of) (apply concat (vals before)))
        new-else (by-place (for [[f fs] after :when (not= f rel)
                                 x fs
                                 :when (<= (rank (:severity x)) (rank :warning))
                                 :when (not (old (identity-of x)))]
                             x))
        mine     (into #{} (map (juxt :kind (comp name :name))) defs)
        said     (for [x (:findings lint)
                       :when (mine [(section->kind (:entity-class x)) (str (:entity x))])]
                   x)]
    (str/join
     "\n"
     (concat
      [(str "ent-lsp zu " rel ":")]
      (if (seq shown) (map line (take shown-max shown)) ["keine Befunde"])
      (when (> (count shown) shown-max) [(str "… und " (- (count shown) shown-max) " weitere")])
      (when (seq hints)
        [(str "+ " (count hints) " Hinweise ("
              (->> hints (map :rule) frequencies (sort-by (comp - val))
                   (map (fn [[r n]] (str (name r) " " n))) (str/join ", "))
              ")")])
      (when (seq new-else)
        (cons "Neu in anderen Dateien seit dem letzten Aufruf:"
              (concat (map #(str "  " (line %)) (take elsewhere-max new-else))
                      (when (> (count new-else) elsewhere-max)
                        [(str "  … und " (- (count new-else) elsewhere-max) " weitere")]))))
      (when (seq said)
        (concat
         [(str "Die Instanz (Lint des Live-Stands) meldet " (count said)
               " zu Ressourcen dieser Datei ("
               (->> said (map #(name (:rule % "?"))) frequencies (sort-by (comp - val))
                    (map (fn [[r n]] (str r " " n))) (str/join ", "))
               "):")]
         (for [x (take said-max said)]
           (str "  " (name (:severity x :warning)) " " (:entity x) ": " (clip (:message x))))
         (when (> (count said) said-max)
           [(str "  … und " (- (count said) said-max) " weitere")])))
      [(instance-line st t)]))))

(defmacro ^:private timed [phases k & body]
  `(let [t0# (System/nanoTime) r# (do ~@body)]
     (swap! ~phases conj [~k (quot (- (System/nanoTime) t0#) 1000000)])
     r#))

(defn answer
  [{:keys [file all]}]
  (locking lock
    (let [t0     (System/nanoTime)
          phases (atom [])
          root   (get-in @state [:ws :root])
          rel    (ws/rel-path root file)
          before (get-in @state [:ws :findings])
          _      (timed phases :sync (swap! state sync-disk rel))
          _      (timed phases :analyze (swap! state update :ws ws/analyze))
          st     @state
          text   (timed phases :render
                        (render (assoc st :rel rel :before before :after (get-in st [:ws :findings])
                                       :defs (get-in st [:ws :index :defs-by-file rel]) :all? all)
                                (now)))
          wall   (quot (- (System/nanoTime) t0) 1000000)]
      (swap! state assoc :last-request (now))
      {:text (str text "\n" wall " ms ("
                  (str/join " · " (for [[k ms] @phases] (str (name k) " " ms))) ")")})))

(defn- rev-of [schema] (format "%08x" (hash schema)))

(defn- poll-once! [sess]
  (let [r (sutra/fetch sess :schema {})]
    (when-not (<= 200 (:status r) 299)
      (throw (ex-info (str "GET /api/schema → HTTP " (:status r)) {:status (:status r)})))
    (let [schema (:body r)
          rev    (rev-of schema)]
      (when (not= rev (get-in @state [:online-status :rev]))
        (let [t  (System/nanoTime)
              on (online/from-schema (get-in @state [:ws :spec]) schema)
              l  (try (let [lr (sutra/fetch sess :schema-lint {})]
                        (when (<= 200 (:status lr) 299) (:body lr)))
                      (catch Throwable _ nil))]
          (locking lock
            (swap! state #(-> %
                              (update :ws ws/set-online on)
                              (assoc :lint l)))
            (swap! state update :ws ws/analyze))
          (log "Instanz-Stand" rev "gespiegelt in" (quot (- (System/nanoTime) t) 1000000) "ms,"
               (count (get-in on [:facts :defs])) "Definitionen")))
      (swap! state update :online-status assoc :rev rev :checked-at (now) :error nil))))

(defn- poll-loop! [root]
  (loop [sess nil]
    (let [sess (or sess (sutra/connect root))
          ok   (try (if (:token sess)
                      (do (poll-once! sess) true)
                      (do (swap! state update :online-status assoc :error
                                 (or (try (srv/warum-nicht root)
                                          (catch Throwable _ nil))
                                     "keine Anmeldung"))
                          false))
                    (catch Throwable t
                      (swap! state update :online-status assoc :error (ex-message t))
                      false))]
      (Thread/sleep (long poll-ms))
      (recur (when ok sess)))))

(defn- code-stamp
  [here]
  [(->> (fs/glob here "{src,resources}/**") (map #(fs/file %))
        (map #(.lastModified ^java.io.File %)) (reduce max 0))
   (try (str/trim (:out (p/sh {:dir (str here "/../..")} "git" "rev-parse" "HEAD")))
     (catch Throwable _ nil))])

(defn- every! [ms f]
  (future (loop [] (Thread/sleep (long ms))
            (try (f) (catch Throwable t (log "Fehler:" (ex-message t)))) (recur))))

(defn- serve! [^ServerSocketChannel ss]
  (loop []
    (let [^SocketChannel c (.accept ss)]
      (future
        (with-open [c c]
          (let [r (java.io.BufferedReader. (Channels/newReader c "UTF-8"))
                w (java.io.PrintWriter. (Channels/newWriter c "UTF-8"))]
            (.println w (json/generate-string
                         (try (answer (json/parse-string (.readLine r) true))
                              (catch Throwable t {:text (str "ent-lsp-Daemon: " (ex-message t))}))))
            (.flush w)))))
    (recur)))

(defn -main [root]
  (let [root (str (fs/canonicalize root))
        here (str (fs/canonicalize "."))
        _    (fs/create-dirs (place/dir root))
        ch   (FileChannel/open (fs/path (place/lock root))
                               (into-array OpenOption
                                           [StandardOpenOption/CREATE StandardOpenOption/WRITE]))]
    (when-not (.tryLock ch)
      (log "läuft schon für" root)
      (System/exit 0))
    (let [t     (System/nanoTime)
          w     (ws/analyze (ws/open-workspace (spec/load-spec) root))
          stamp (code-stamp here)]
      (reset! state {:ws w :last-request (now)
                     :mtimes (into {} (for [rel (keys (:files w))] [rel (mtime root rel)]))})
      (log "Arbeitsbereich" root "analysiert in" (quot (- (System/nanoTime) t) 1000000) "ms")
      (future (poll-loop! root))
      (every! rescan-ms #(locking lock (swap! state rescan)))
      (every! 60000 #(when (or (not= stamp (code-stamp here))
                               (> (- (now) (:last-request @state)) idle-ms))
                       (locking lock (log "Code geändert oder untätig — Ende") (System/exit 0))))
      (let [sock (fs/path (place/socket root))
            _    (fs/delete-if-exists sock)
            ss   (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
                   (.bind (UnixDomainSocketAddress/of sock)))]
        (log "hört auf" (str sock))
        (serve! ss)))))
