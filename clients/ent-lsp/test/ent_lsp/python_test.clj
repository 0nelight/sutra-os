(ns ent-lsp.python-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [ent-lsp.jsonrpc :as rpc]
            [ent-lsp.python :as python]
            [ent-lsp.server :as server]))

(def ^:private sdk-root (str (fs/absolutize "../python-platform-sdk")))

(def ^:private module
  (str python/generated " — do not edit by hand.\n"
       "HarvestStatus = Literal['planted', 'paid']\n"
       "class Lot(OntologyObject):\n"
       "    sales: Many[Sale] = Many('lot', 'sales', 'sale')\n"
       "    in_barn: Attr[int, int] = Attr('lot', 'in-barn', 'integer')\n"
       "    def sell_hay(self, *, bales: int) -> SyncApplyActionResponse:\n"
       "class TypedActions(Actions):\n"
       "    def add_buyer(self, *, name: str) -> SyncApplyActionResponse:\n"
       "class TypedFunctions(Functions):\n"
       "    def margin_of(self, *, lot: str) -> Decimal:\n"
       "class Helper:\n"
       "    hidden: int\n"))

(deftest the-index-reads-the-model-out-of-a-generated-module
  (let [i (python/index-source module)]
    (is (= #{"Lot"} (:object-types i)))
    (is (= #{"sales" "in_barn" "sell_hay" "add_buyer"} (:members i)))
    (is (= #{"margin_of"} (:functions i)))
    (is (= #{"\"planted\"" "'planted'" "\"paid\"" "'paid'"} (:states i)))))

(deftest an-item-is-named-by-its-label-or-sent-to-be-resolved
  (let [i (python/index-source module)]
    (is (= "object-type" (python/kind-of i {:label "Lot" :kind 7})))
    (is (nil? (python/kind-of i {:label "Lot" :kind 6})) "a variable named Lot is not the type")
    (is (= "state" (python/kind-of i {:label "\"paid\"" :kind 21})))
    (is (= "function" (python/kind-of i {:label "margin_of" :kind 2})))
    (is (= ::python/resolve (python/kind-of i {:label "sales" :kind 10})))
    (is (nil? (python/kind-of i {:label "hidden" :kind 10})))))

(deftest a-module-is-read-again-only-when-it-changed
  (let [dir  (fs/create-temp-dir)
        f    (str dir "/farm_sdk.py")
        _    (spit f module)
        c1   (python/rescan {} [(str dir)])
        _    (spit (str dir "/own.py") "x = 1\n")
        c2   (python/rescan (update-vals c1 (fn [[m _]] [m ::cached])) [(str dir)])]
    (is (= #{"Lot"} (:object-types (second (c1 f)))))
    (is (= [::cached] (map second (vals c2))) "unchanged: kept; not generated: never read")
    (fs/delete-tree dir)))

(deftest a-member-is-classified-by-its-declared-descriptor
  (is (= "link-type" (python/classify "sales" "sales: Many[Sale]")))
  (is (= "link-type" (python/classify "field" "field: One[Field]")))
  (is (= "property" (python/classify "in_barn" "in_barn: Attr[int, int]\n\nIn the barn")))
  (is (= "action" (python/classify "sell_hay" (str "def sell_hay(\n    *,\n    bales: int\n) -> "
                                                   "SyncApplyActionResponse"))))
  (is (nil? (python/classify "name" "name: str")))
  (is (= "link-type" (python/classify "buyer" "```python\nbuyer: One[Buyer]\n```"))))

(defn- generate-farm! [dir]
  (let [r (p/shell {:dir sdk-root :out :string :err :string :continue true}
                   "python3" "-c"
                   (str "import sys\n"
                        "from sutra_platform_sdk.generate import generate_from_export\n"
                        "generate_from_export(sys.argv[1], sys.argv[2], ontology='farm')")
                   "tests/fixtures/farm-schema.transit.json" (str dir "/farm_sdk.py"))]
    (zero? (:exit r))))

(defn- answer [in send id]
  (loop []
    (let [m (rpc/read-message in)]
      (cond (and (= id (:id m)) (nil? (:method m))) (:result m)
            (and (:method m) (:id m))
              (do (send {:id (:id m) :result (vec (repeat (count (get-in m [:params :items]))
                                                          {}))})
                  (recur))
            :else (recur)))))

(deftest pyright-completions-carry-the-model-kind
  (let [dir (str (fs/create-temp-dir))]
    (if-not (and (fs/which "pyright-langserver") (generate-farm! dir))
      (println "übersprungen: pyright-langserver oder das Python-SDK fehlt")
      (let [_    (spit (str dir "/pyrightconfig.json")
                       (json/generate-string {:extraPaths [sdk-root]}))
            src  (str "from farm_sdk import Lot, Field\n\nlot = Lot.get(\"x\")\nlot.\n"
                      "for h in Field.get(\"x\").harvests:\n    if h.status == \n")
            uri  (server/path->uri (str dir "/probe.py"))
            proc (p/process ["bb" "--config" (str (fs/absolutize "bb.edn")) "lsp"]
                            {:dir dir :err :inherit})
            send (fn [m] (rpc/write-message (:in proc) m))
            kinds (fn [id line col]
                    (send {:id id :method "textDocument/completion"
                           :params {:textDocument {:uri uri}
                                    :position {:line line :character col}}})
                    (let [r (answer (:out proc) send id)]
                      (into {} (for [i (if (map? r) (:items r) r)]
                                 [(:label i) (get-in i [:labelDetails :description])]))))]
        (try
          (send {:id 1 :method "initialize"
                 :params {:processId nil :rootUri (server/path->uri dir) :capabilities {}
                          :workspaceFolders [{:uri (server/path->uri dir) :name "w"}]}})
          (is (get-in (answer (:out proc) send 1) [:capabilities :hoverProvider]))
          (send {:method "initialized" :params {}})
          (send {:method "textDocument/didOpen"
                 :params {:textDocument {:uri uri :languageId "python" :version 1 :text src}}})
          (let [m (kinds 2 3 4)]
            (is (= "link-type" (m "sales") (m "field")))
            (is (= "property" (m "in_barn")))
            (is (= "action" (m "sell_hay")))
            (is (nil? (m "values"))))
          (let [m (kinds 3 5 19)]
            (is (= "state" (m "\"planted\"")))
            (is (= "object-type" (m "Lot"))))
          (send {:id 4 :method "textDocument/completion"
                 :params {:textDocument {:uri uri} :position {:line 4 :character 34}
                          :context {:triggerKind 2 :triggerCharacter ":"}}})
          (is (= [] (answer (:out proc) send 4)) "`:` triggers .ent completion, not Python's")
          (finally (p/destroy-tree proc) (fs/delete-tree dir)))))))
