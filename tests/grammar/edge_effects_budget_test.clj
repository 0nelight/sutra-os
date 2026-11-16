(ns ^:unit grammar.edge-effects-budget-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "libs/lib/src"
   "plugins/dot/src" "plugins/llm/src" "plugins/packages/src" "plugins/sso/src"
   "libs/store/src" "libs/store-pg/src"])

(def ^:private direct-writes
  {"src/sutra/builds/run/lock.clj" 5
   "src/sutra/schedules/run_setups.clj" 2
   "src/sutra/store/control/cadence.clj" 3
   "src/sutra/builds/run/queue.clj" 5
   "src/sutra/agents/builder.clj" 2
   "src/sutra/agents/threads.clj" 6
   "src/sutra/canvas/graphs.clj" 7
   "src/sutra/documents/records.clj" 2
   "src/sutra/automation/executions.clj" 5
   "src/sutra/automation/logic_events.clj" 4
   "src/sutra/actions/proposals.clj" 3
   "src/sutra/health/checks.clj" 3
   "src/sutra/notifications/inbox.clj" 2
   "src/sutra/actions/http.clj" 1
   "src/sutra/feedback/http.clj" 4
   "src/sutra/authoring/http/products.clj" 1
   "src/sutra/store/access/streams.clj" 2
   "src/sutra/builds/ledger/retention.clj" 2
   "src/sutra/repos/projection.clj" 1
   "src/sutra/governance/requests.clj" 3
   "src/sutra/store/protection.clj" 1
   "src/sutra/governance/protection.clj" 1
   "src/sutra/governance/roles.clj" 1
   "src/sutra/llm/budget.clj" 1
   "src/sutra/media/sets.clj" 2
   "src/sutra/authoring/placements.clj" 1
   "src/sutra/store/migrations/type_identity.clj" 1
   "src/sutra/repos/records.clj" 5
   "src/sutra/store/control/import_keys.clj" 2
   "src/sutra/access/oauth.clj" 2
   "src/sutra/objects/saved_sets.clj" 2
   "src/sutra/builds/run/rows.clj" 3
   "src/sutra/actions/staged_rows.clj" 2
   "src/sutra/actions/staged.clj" 1
   "src/sutra/actions/effects.clj" 2
   "src/sutra/governance/merge.clj" 4
   "src/sutra/scenarios/state.clj" 1})

(def ^:private outside-calls
  {"src/sutra/foundation/outbound.clj" 2
   "src/sutra/serve/server.clj" 2})

(def ^:private world-commits
  {"src/sutra/builds/ledger.clj" 1
   "src/sutra/builds/import/reconcile.clj" 1
   "src/sutra/authoring/install/world.clj" 1})

(def ^:private locator-calls
  {"libs/dialect/src/sutra/parser/spec.clj" 3
   "src/sutra/sources/egress.clj" 1
   "src/sutra/serve/instance.clj" 5
   "src/sutra/serve/platform.clj" 3
   "src/sutra/serve/worker.clj" 1
   "src/sutra/store/storage/blob_store.clj" 1
   "src/sutra/store/access/row_shape.clj" 1
   "src/sutra/store/access/scope.clj" 5
   "src/sutra/store/access/watch.clj" 3
   "libs/store-pg/src/sutra/store/pg/bus.clj" 3})

(def ^:private checks
  [[#"\(ctl/(?:upsert|upsert-as|insert|insert-many|update|update-as|swap|delete)! "
    direct-writes
    "direct control writes — plan the records and commit them once (ctl/plan!)"]
   [#"org\.httpkit\.client|clj-http\.client|hato\.client|java\.net\.http\.HttpClient"
    outside-calls
    "HTTP clients — a call to the outside goes through sutra.kit.effects/call!"]
   [#"\(world/commit! "
    world-commits
    "world commits — decide on a snapshot and commit at its basis (machine/command!)"]
   [#"\(sys/(?:provided|required|table)\s"
    locator-calls
    "capability lookups — a module takes a capability through its ports namespace"
    #"^src/sutra/[^/]+/ports\.clj$"]])

(defn- sources []
  (for [r roots
        :let [d (io/file r)]
        :when (.isDirectory d)
        ^java.io.File f (file-seq d)
        :when (re-find #"\.clj[cs]?$" (.getName f))]
    [(str/replace (.getPath f) "\\" "/") (slurp f)]))

(deftest no-file-exceeds-its-edge-effect-budget
  (let [srcs (sources)]
    (doseq [[re budget what exempt] checks
            [p s] srcs
            :when (not (and exempt (re-find exempt p)))
            :let [n (count (re-seq re s)) b (get budget p 0)]]
      (testing p
        (is (<= n b) (str p ": " n " " what ", budget " b))))))

(deftest budgets-name-only-files-that-still-spend-them
  (let [srcs (into {} (sources))]
    (doseq [[re budget what] checks
            [p b] budget
            :let [n (count (re-seq re (get srcs p "")))]]
      (testing p
        (is (= n b) (str p ": budget " b " for " what ", spends " n
                         " — lower the budget to what is left"))))))
