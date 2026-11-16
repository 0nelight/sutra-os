(ns ent-lsp.diff-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [ent-lsp.diff :as diff]
            [ent-lsp.fixture :as fixture]
            [ent-lsp.spec :as spec]
            [ent-lsp.workspace :as ws]))

(defn- git [dir & args]
  (apply p/shell {:dir dir :out :string :err :string}
         "git" "-c" "user.name=t" "-c" "user.email=t@t" args))

(deftest breaking-changes-against-a-revision
  (let [dir  (str (fs/create-temp-dir))
        spec (spec/load-spec)]
    (try
      (fs/copy-tree fixture/root dir)
      (spit (str dir "/stamm/intern/object.ent")
            (str/replace (slurp (str dir "/stamm/intern/object.ent")) ":project    :stamm"
                         ":project    :stamm :status :active"))
      (git dir "init" "-q")
      (git dir "add" "-A")
      (git dir "commit" "-qm" "base")
      (let [edit (fn [rel f] (spit (str dir "/" rel) (f (slurp (str dir "/" rel)))))]
        (edit "verkauf/angebot/object.ent"
              #(str/replace % ":primary-key :angebot-id}"
                            ":primary-key [:angebot-id :bezeichnung]}"))
        (edit "verkauf/angebot/object.ent"
              #(str/replace % "[:bezeichnung :string]" "[:bezeichnung :long]"))
        (fs/delete (str dir "/stamm/intern/object.ent")))
      (let [w   (ws/open-workspace spec dir)
            now (:index (ws/analyze w))
            fs  (diff/findings spec (diff/index-at w "HEAD") now "HEAD")]
        (is (= #{[:entfernt-geschuetzt "stamm/intern/object.ent"]
                 [:pk-geaendert "verkauf/angebot/object.ent"]
                 [:typ-geaendert "verkauf/angebot/object.ent"]}
               (set (map (juxt :rule :file) fs)))))
      (finally (fs/delete-tree dir)))))
