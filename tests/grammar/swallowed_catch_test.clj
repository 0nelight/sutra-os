(ns ^:unit grammar.swallowed-catch-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "libs/lib/src"
   "plugins/dot/src" "plugins/llm/src" "plugins/packages/src" "plugins/sso/src"
   "libs/store/src" "libs/store-pg/src"])

(def ^:private budget
  {"libs/core/src/platform_sdk/gen.clj"                            1
   "libs/core/src/sutra/capps/format.cljc"                         3
   "libs/core/src/sutra/i18n/inline.clj"                           1
   "libs/core/src/sutra/parser/fn_params.clj"                      1
   "libs/core/src/sutra/parser/project_export.clj"                 4
   "libs/core/src/sutra/parser/projection.clj"                     3
   "libs/core/src/sutra/types/profile.cljc"                        1
   "libs/core/src/sutra/types/settings.clj"                        2
   "libs/core/src/sutra/viz/graph_layout.cljc"                     1
   "libs/core/src/sutra/viz/page.cljc"                             1
   "libs/core/src/sutra/wire/creds.clj"                            1
   "libs/core/src/sutra/wire/transport.cljc"                       2
   "libs/dialect/src/sutra/kit/coerce.cljc"                        1
   "libs/dialect/src/sutra/kit/time.clj"                           4
   "libs/dialect/src/sutra/parser/spec.clj"                        2
   "libs/dialect/src/sutra/parser/sugar.clj"                       1
   "libs/dialect/src/sutra/types/expectations.clj"                 1
   "libs/kernel/src/sutra/ref/action.clj"                          1
   "libs/kernel/src/sutra/ref/install.clj"                         1
   "libs/kernel/src/sutra/ref/objects.clj"                         1
   "libs/kernel/src/sutra/ref/objectset.clj"                       2
   "plugins/sso/src/sutra/sso/ldap.clj"                       7
   "src/sutra/actions/logic.clj"                              1
   "src/sutra/actions/proposals.clj"                          1
   "src/sutra/agents/builder.clj"                             1
   "src/sutra/agents/http.clj"                                2
   "src/sutra/agents/runtime.clj"                             1
   "src/sutra/apps/http/apps.clj"                             1
   "src/sutra/apps/http/query.clj"                            1
   "src/sutra/apps/http/scenarios.clj"                        1
   "src/sutra/authoring/stage.clj"                            1
   "src/sutra/automation/runs.clj"                            1
   "src/sutra/branching/http/git.clj"                         1
   "src/sutra/branching/proposals.clj"                        1
   "src/sutra/builds/run/stream_index.clj"                    1
   "src/sutra/catalog/branch_policy.clj"                      1
   "src/sutra/catalog/constraints.clj"                        1
   "src/sutra/catalog/function_contract.clj"                  1
   "src/sutra/catalog/outputs.clj"                            1
   "src/sutra/documents/widgets.clj"                          1
   "src/sutra/evals/evaluators.clj"                           2
   "src/sutra/evals/validate.clj"                             1
   "src/sutra/foundation/cron.clj"                            1
   "src/sutra/foundation/media_type.clj"                      1
   "src/sutra/foundation/profile.clj"                         5
   "src/sutra/foundation/sql_trace.clj"                       3
   "src/sutra/health/checks.clj"                              2
   "src/sutra/lineage/http.clj"                               1
   "src/sutra/llm/http.clj"                                   3
   "src/sutra/llm/wire.clj"                                   1
   "src/sutra/media/http/blobs.clj"                           5
   "src/sutra/media/sets.clj"                                 1
   "src/sutra/notifications/http.clj"                         3
   "src/sutra/objects/knn.clj"                                1
   "src/sutra/objects/planner.clj"                            1
   "src/sutra/objects/views.clj"                              2
   "src/sutra/repos/projection.clj"                           1
   "src/sutra/repos/riders.clj"                               1
   "src/sutra/serve/hot_patches.clj"                          1
   "src/sutra/serve/platform.clj"                             1
   "src/sutra/serve/server.clj"                               1
   "src/sutra/serve/stream.clj"                               7
   "src/sutra/serve/telemetry.clj"                            2
   "src/sutra/sources/imap_idle.clj"                          1
   "src/sutra/sources/jdbc.clj"                               2
   "src/sutra/sources/policy.clj"                             1
   "src/sutra/store/storage/node.clj"                         1
   "libs/store-pg/src/sutra/store/pg.clj"               1
   "libs/store-pg/src/sutra/store/pg/bus.clj"           2
   "libs/store-pg/src/sutra/store/pg/dataset.clj"       1
   "libs/store-pg/src/sutra/store/pg/dataset_store.clj" 1
   "libs/store-pg/src/sutra/store/pg/datasets.clj"      2
   "libs/store/src/sutra/store/core/kv.clj"             1
   "libs/store/src/sutra/store/core/reads.clj"          1
   "libs/store/src/sutra/store/derived.clj"             1
   "libs/store/src/sutra/store/values.clj"              1
   "src/sutra/builds/interchange.clj"                         1
   "src/sutra/builds/import/coerce.clj"                       1
   "libs/store-pg/src/sutra/store/pg/rows.clj"          1
   "src/sutra/catalog/streaming.clj"                          1
   "src/sutra/peering/objects.clj"                            1
   "libs/core/src/sutra/types/peering.clj"                         1
   "libs/core/src/sutra/parser/product.clj"                        2
   "src/sutra/peering/wire.clj"                               3
   "src/sutra/peering/connections.clj"                        1
   "src/sutra/access/federation.clj"                          5
   "src/sutra/access/oidc.clj"                                2
   "src/sutra/access/totp.clj"                                2
   "src/sutra/foundation/encoding.clj"                        4
   "src/sutra/serve/deadline.clj"                             1
   "src/sutra/serve/errors.clj"                               1
   "src/sutra/builds/import/phases.clj"                       1
   "src/sutra/builds/plan/dag.clj"                            2
   "src/sutra/builds/plan/freshness.clj"                      2
   "src/sutra/builds/run.clj"                                 1
   "src/sutra/foundation/outbound.clj"                        1
   "src/sutra/http/origin.clj"                                1
   "src/sutra/projects/http.clj"                              1
   "src/sutra/repos/checks.clj"                               1
   "src/sutra/sources/datasources.clj"                        5
   "src/sutra/sources/http.clj"                               1
   "libs/store/src/sutra/store/edits.clj"               3})

(def ^:private swallowed
  #"\(catch [A-Za-z.$]+ _[A-Za-z0-9-]*[ )]|\(attempt/(?:or-answer|silently)[\s)]")

(defn- sources []
  (for [r roots
        :let [d (io/file r)]
        :when (.isDirectory d)
        ^java.io.File f (file-seq d)
        :when (re-find #"\.clj[cs]?$" (.getName f))]
    [(str/replace (.getPath f) "\\" "/") (slurp f)]))

(deftest no-file-swallows-more-exceptions-than-its-budget
  (doseq [[p s] (sources)
          :let [n (count (re-seq swallowed s)) b (get budget p 0)]]
    (testing p
      (is (<= n b) (str p " drops " n " caught exceptions unnamed, its budget is " b
                        " — bind it and say why: log it, rethrow it, or turn it into a"
                        " refusal that carries it")))))

(deftest a-budget-shrinks-with-its-file
  (let [loose (into (sorted-map)
                    (for [[p s] (sources)
                          :let [n (count (re-seq swallowed s)) b (get budget p)]
                          :when (and b (< n b))]
                      [p n]))]
    (is (empty? loose) (str "these files drop fewer now — lower their budget to: " loose))))
