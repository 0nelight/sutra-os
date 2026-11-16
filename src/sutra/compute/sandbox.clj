(ns sutra.compute.sandbox
  (:require [sutra.kit.refusal :as refusal]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.compute.cores :as cores]
            [sutra.compute.container :as ctr]
            [sutra.foundation.env :as env]
            [sutra.foundation.settings :as settings]))

(set! *warn-on-reflection* true)

(def ^:private sandbox-tool "bwrap")

(defn profiles
  []
  (settings/value :transform-profiles))

(defn limits-for
  [profile]
  (let [ps (profiles)]
    (get ps profile (:default ps))))

(defn limits-of
  [compute profile]
  (let [l (cond-> (limits-for (or profile (:profile compute)))
            (get-in compute [:resources :memory-gb])
            (assoc :address-space-bytes
                   (* (long (get-in compute [:resources :memory-gb])) 1024 1024 1024)))]
    (cond
      (:build-job? compute)
      (-> l (dissoc :cpu-seconds) (assoc :wall-seconds (:allowed-run-seconds compute)))

      (:timeout-ms compute)
      (let [s (min (long (Math/ceil (/ (double (:timeout-ms compute)) 1000)))
                   (long (:wall-seconds (limits-for :xlarge))))]
        (assoc l :wall-seconds s :cpu-seconds (max (long (:cpu-seconds l)) s)))

      :else l)))

(defn- on-path?
  [exe]
  (some (fn [dir]
          (let [f (io/file dir exe)]
            (and (.isFile f) (.canExecute f))))
        (str/split (or (env/getenv "PATH") "") #":")))

(def available?
  (memoize (fn [] (on-path? sandbox-tool))))

(def ^:private pinnable?
  (memoize (fn [] (on-path? "taskset"))))

(defn require-sandbox!
  [code data]
  (when-not (available?)
    (refusal/refuse! code (assoc data :sandbox-tool sandbox-tool))))

(defn- limit-prefix
  [{:keys [wall-seconds cpu-seconds address-space-bytes max-open-files cpus]} pinnable?]
  (vec
   (concat
    (when (and (seq cpus) pinnable?) ["taskset" "--cpu-list" (cores/format-cpu-list cpus)])
    (when wall-seconds ["timeout" (str wall-seconds "s")])
    ["prlimit" (str "--as=" address-space-bytes)]
    (when cpu-seconds [(str "--cpu=" cpu-seconds)])
    [(str "--nofile=" max-open-files)])))

(defn jail-argv
  [limits network host-files mounts env cmd]
  (vec
   (concat
    (limit-prefix limits (pinnable?))
    [sandbox-tool]
    (if network
      ["--unshare-user-try" "--unshare-ipc" "--unshare-pid"
       "--unshare-uts" "--unshare-cgroup-try"]
      ["--unshare-all"])
    ["--die-with-parent" "--new-session"
     "--ro-bind" "/usr" "/usr"
     "--ro-bind" "/bin" "/bin"]
    (mapcat (fn [host] (when (.exists (io/file host)) ["--ro-bind" host host]))
            (concat ["/lib" "/lib64" "/etc/ssl"]
                    (when network ["/etc/resolv.conf" "/etc/hosts"])
                    host-files))
    ["--proc" "/proc" "--dev" "/dev" "--tmpfs" "/tmp"]
    mounts
    ["--clearenv"
     "--setenv" "PATH" "/usr/bin:/bin"
     "--setenv" "PYTHONDONTWRITEBYTECODE" "1"
     "--setenv" "LANG" "C.UTF-8"]
    (mapcat (fn [[k v]] ["--setenv" (str k) (str v)]) env)
    ["--"]
    cmd)))

(defn container-argv
  [{:keys [inner-cmd lib-dir module-path sibling-paths input-paths rw-paths limits wheelhouse
           network env container]
    :or   {input-paths []}} pinnable?]
  (let [bound  (into (set input-paths) rw-paths)
        moved  #(if (bound %) (ctr/io-path %) %)]
    (ctr/run-argv
     (assoc (select-keys container [:rootfs :entries :image :resolv?])
            :prefix  (limit-prefix (or limits (limits-for :default)) pinnable?)
            :network network
            :tmpfs   ["/work"]
            :binds   (concat [[lib-dir "/sutra/python"] [module-path "/work/mod.py"]]
                             (map (fn [[m p]] [p (str "/work/" m ".py")]) sibling-paths)
                             (when wheelhouse [[wheelhouse "/sutra/wheelhouse"]])
                             (map (fn [p] [p (ctr/io-path p)]) input-paths))
            :rw-binds (map (fn [p] [p (ctr/io-path p)]) rw-paths)
            :env     (into {"PYTHONPATH" (if wheelhouse
                                           "/sutra/python:/sutra/wheelhouse"
                                           "/sutra/python")}
                           (map (fn [[k v]] [k (moved (str v))]))
                           env)
            :cmd     (into (ctr/driver-command (:driver container))
                           (map moved) (rest inner-cmd))))))

(defn wrap-argv
  [{:keys [inner-cmd lib-dir module-path sibling-paths input-paths rw-paths limits wheelhouse
           network env container]
    :or   {input-paths []}
    :as spec}]
  (if container
    (container-argv spec (pinnable?))
    (jail-argv (or limits (limits-for :default)) network
             ["/etc/fonts"]
             (concat ["--tmpfs" "/work"
                      "--ro-bind" lib-dir "/sutra/python"
                      "--ro-bind" module-path "/work/mod.py"]
                     (mapcat (fn [[m p]] ["--ro-bind" p (str "/work/" m ".py")]) sibling-paths)
                     (when wheelhouse ["--ro-bind" wheelhouse "/sutra/wheelhouse"])
                     (mapcat (fn [p] ["--ro-bind" p p]) input-paths)
                     (mapcat (fn [p] ["--bind" p p]) rw-paths)
                     ["--chdir" "/work"])
             (concat [["PYTHONPATH"
                       (if wheelhouse "/sutra/python:/sutra/wheelhouse" "/sutra/python")]
                      ["HOME" "/work"]]
                     env)
             inner-cmd)))
