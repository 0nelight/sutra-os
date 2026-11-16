(ns ent-lsp.place)

(defn- digest [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-1")]
    (apply str (map #(format "%02x" %) (take 6 (.digest md (.getBytes s "UTF-8")))))))

(defn root-of
  [file]
  (->> (iterate #(.getParentFile ^java.io.File %)
                (.getParentFile (.getCanonicalFile (java.io.File. (str file)))))
       (take-while some?)
       (filter #(.exists (java.io.File. ^java.io.File % ".git")))
       last
       (#(some-> ^java.io.File % str))))

(defn dir [root]
  (str (System/getProperty "user.home") "/.cache/ent-lsp/" (digest (str root))))

(defn socket [root] (str (dir root) "/daemon.sock"))
(defn lock [root] (str (dir root) "/daemon.lock"))
(defn log [root] (str (dir root) "/daemon.log"))
