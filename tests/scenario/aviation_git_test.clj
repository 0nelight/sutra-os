(ns ^:api scenario.aviation-git-test
  (:require [sutra.catalog.grants :as grants]
            [support.in-project :refer [install-in-project]]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.branching.http.git :as git-http]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-git [f]
  (alter-var-root #'git-http/*enabled-override* (constantly true))
  (try (f) (finally (alter-var-root #'git-http/*enabled-override* (constantly nil)))))

(use-fixtures :once pod/with-mem-pod-auth mit-git als-admin)

(defonce ^:private klon (atom nil))

(defn- git [& args]
  (let [{:keys [exit out err]} (apply sh/sh "git"
                                      (concat args [:dir @klon :env
                                                    {"GIT_TERMINAL_PROMPT" "0"
                                                     "HOME" (System/getProperty "java.io.tmpdir")
                                                     "PATH" (System/getenv "PATH")}]))]
    {:exit exit :out (str out err)}))

(defn- typen []
  (set (map :object-type-name (vals (:object-types (:body (api/get! :schema)))))))

(defn- warte-bis [pred]
  (loop [n 0]
    (cond (pred) true
          (> n 60) false
          :else (do (Thread/sleep 250) (recur (inc n))))))

(defn- resync! []
  (git "fetch" "-q" "origin")
  (git "reset" "-q" "--hard" "origin/main"))

(defn- push!
  [nachricht]
  (git "add" "--all" ".")
  (git "-c" "user.name=Wartung" "-c" "user.email=wartung@aviation.test" "commit" "-q" "-m"
       nachricht)
  (let [sha (str/trim (:out (git "rev-parse" "HEAD")))]
    (assoc (git "push" "origin" "HEAD:main") :sha sha)))

(defn- fern-kopf []
  (first (str/split (str/trim (:out (git "ls-remote" "origin" "refs/heads/main"))) #"\s")))

(defn- text-haelt? [s]
  (some #(and (.isFile ^java.io.File %) (str/ends-with? (.getName ^java.io.File %) ".ent")
              (str/includes? (slurp %) s))
        (file-seq (io/file @klon))))

(deftest der-baum-ist-eine-projektion
  (let [token (:token (:body (api/post! :repo-clone-token {:rid rid/ontology-repo-rid} {})))
        basis (str/replace (tx/current-base) #"^http://" (str "http://sutra:" token "@"))
        dir   (str (java.nio.file.Files/createTempDirectory
                    "aviation-git" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (is (some? token))
    (reset! klon dir)
    (let [{:keys [exit out]} (apply sh/sh "git" "clone" "-q" (str basis "/") dir
                                    [:env
                                     {"GIT_TERMINAL_PROMPT" "0" "PATH" (System/getenv "PATH")}])]
      (is (zero? exit) out))

    (testing "ein Push, der einen Text ändert, wird abgewiesen und ändert nichts"
      (let [vorher (fern-kopf)
            text   (io/file dir "ontology.ent")]
        (spit text (str (slurp text) "\n;; von Hand\n"))
        (let [{:keys [exit out]} (push! "Text von Hand")]
          (is (not (zero? exit)) out)
          (is (str/includes? out "ontology.ent") out))
        (is (= vorher (fern-kopf)))
        (is (empty? (get-in (api/get! :staging) [:body :staged])))
        (resync!)))

    (testing "ein Push ohne Textänderung läuft durch die Checks und deklariert nichts"
      (let [typen-vorher (typen)]
        (spit (io/file dir "notizen.txt") "Wartung\n")
        (let [{:keys [exit out sha]} (push! "Notiz")]
          (is (zero? exit) out)
          (let [b (:body (api/get! :repo-checks {:rid rid/ontology-repo-rid :commit sha}))]
            (is (= :passed (keyword (name (:status b)))) (pr-str b))))
        (is (= typen-vorher (typen)))
        (resync!)))

    (testing "was die Welt hält, steht nach dem Fetch im Baum"
      (is (ok? (install-in-project {:schema
                                    {:objects {:dock {:primary-key :nr
                                                      :attributes [[:nr :string]]
                                                      :plural "Docks"}}}})))
      (is (ok? (api/post! :staging)))
      (is (warte-bis #(do (resync!) (text-haelt? ":dock")))))))
