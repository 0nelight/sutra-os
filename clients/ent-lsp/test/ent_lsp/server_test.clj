(ns ent-lsp.server-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [ent-lsp.fixture :refer [analyzed pos-of]]
            [ent-lsp.jsonrpc :as rpc]
            [ent-lsp.server :as server]))

(defn- until [in pred]
  (loop [n 0]
    (when (< n 200)
      (let [m (rpc/read-message in)]
        (if (pred m) m (recur (inc n)))))))

(deftest round-trip
  (let [root (str (fs/absolutize "test/fixtures/ws"))
        uri  (server/path->uri (str root "/verkauf/angebot/object.ent"))
        proc (p/process ["bb" "--config" (str (fs/absolutize "bb.edn")) "lsp"] {:err :inherit})
        out  (:in proc)
        in   (:out proc)
        send (fn [m] (rpc/write-message out m))]
    (try
      (send {:id 1 :method "initialize"
             :params {:rootUri (server/path->uri root) :capabilities {}}})
      (is (true? (get-in (until in #(= 1 (:id %))) [:result :capabilities :hoverProvider])))
      (send {:method "initialized" :params {}})
      (let [d (until in #(and (= "textDocument/publishDiagnostics" (:method %))
                              (= uri (get-in % [:params :uri]))))]
        (is (some #(= "unbekannt" (:code %)) (get-in d [:params :diagnostics]))))
      (send {:method "textDocument/didOpen"
             :params {:textDocument {:uri uri :languageId "ent" :version 1
                                     :text (slurp (str root "/verkauf/angebot/object.ent"))}}})
      (let [[l c] (pos-of @analyzed "verkauf/angebot/object.ent" ":partner :many-to-one")]
        (send {:id 2 :method "textDocument/hover"
               :params {:textDocument {:uri uri} :position {:line l :character c}}}))
      (is (str/includes? (get-in (until in #(= 2 (:id %))) [:result :contents :value])
                         "`:partner`"))
      (send {:method "textDocument/didChange"
             :params {:textDocument {:uri uri :version 2}
                      :contentChanges
                        [{:text (str/replace (slurp (str root "/verkauf/angebot/object.ent"))
                                             ":lieferung" ":partner")}]}})
      (let [d (until in #(and (= "textDocument/publishDiagnostics" (:method %))
                              (= uri (get-in % [:params :uri]))))]
        (is (not-any? #(= "unbekannt" (:code %)) (get-in d [:params :diagnostics]))))
      (send {:id 3 :method "shutdown"})
      (until in #(= 3 (:id %)))
      (send {:method "exit"})
      (is (zero? (:exit (deref proc 10000 {:exit :timeout}))))
      (finally (p/destroy proc)))))
