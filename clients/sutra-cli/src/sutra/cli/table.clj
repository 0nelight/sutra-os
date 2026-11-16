(ns sutra.cli.table
  (:require [sutra.cli.assemble :as assemble]))

(def verbs
  [{:verb :platform :home :platform
    :args   {:flags {"logo"  {:key :logo :kw? true}
                     "clear" {:key :clear :kw? true}}}
    :leaves [{:path ["platform"] :section :operate
              :sum "the instance's own name + logo (Admin Settings → Platform experience)"}]}

   {:verb :context :home :guide
    :leaves [{:path ["context"] :section :orient
              :sum "branch / staged batch / identity — the re-sync read"}
             {:path ["changeset" "show"] :section :changeset :sum "what's staged right now"}]}

   {:verb :guide :home :guide
    :leaves [{:path ["guide"] :section :orient :sum "list every command, by topic"}]}

   {:verb :explain :home :guide
    :args   {:pos [{:key :topic :kw? true :optional true}
                   {:key :item :kw? true :optional true}]}
    :leaves [{:path ["explain"] :section :orient :sum "hint for a command or concept"}]}

   {:verb :dependents :home :explore
    :args   {:pos [{:key :type :kw? true}]}
    :leaves [{:path ["dependents"] :section :orient
              :sum "impact-before-change: what references a type"}]}

   {:verb :apps :home :platform
    :leaves [{:path ["app" "list"] :section :orient :aliases ["apps"] :sum "the App Studio apps"}]}

   {:verb :app :home :apps
    :leaves [{:path ["app" "show"] :section :orient :aliases ["app"]
              :sum "one app's layout whitelists"}]}

   {:verb :app-data :home :apps
    :leaves [{:path ["app" "data"] :section :orient :aliases ["app-data"]
              :sum "the objects an app serves (--scenario <seg> reads a fork)"}]}

   {:verb :templates :home :platform
    :leaves [{:path ["template" "list"] :section :orient :aliases ["templates"]
              :sum "the document templates"}]}

   {:verb :template :home :platform
    :leaves [{:path ["template" "show"] :section :orient :aliases ["template"]
              :sum "one template: parameters, page, published versions"}]}

   {:verb :render-template :home :platform
    :args   {:flags {"value" {:key :values :repeat? true :vector? true}}}
    :leaves [{:path ["template" "render"] :section :orient :aliases ["render-template"]
              :sum "render a template read-only — --value k=v, --file x.entt, --layout baum (tries an arrangement on its :graph parameters), --pdf, --out FILE"}]}

   {:verb :documents :home :documents
    :leaves [{:path ["document" "list"] :section :orient :aliases ["documents"]
              :sum "the documents on this branch"}]}

   {:verb :document :home :documents
    :leaves [{:path ["document" "show"] :section :orient :aliases ["document"]
              :sum "one document: template, parameters, links, then the body (--out FILE)"}]}

   {:verb :document-export :home :documents
    :leaves [{:path ["document" "export"] :section :orient :aliases ["document-export"]
              :sum "export a document as PDF (default) or --html, --out FILE"}]}

   {:verb :document-create :home :documents
    :leaves [{:path ["document" "create"] :section :data :aliases ["document-create"]
              :sum "a new document from a local file — --file F [--name N] [--format markdown|html|text|use-case|diagram] [--project P]"}]}

   {:verb :document-update :home :documents
    :leaves [{:path ["document" "update"] :section :data :aliases ["document-update"]
              :sum "replace a document's body (--file) and/or cut a named version (--label, --endorse)"}]}

   {:verb :use-case-plan :home :documents
    :leaves [{:path ["use-case" "plan"] :section :data :aliases ["use-case-plan"]
              :sum "hold a use case's draft model (EDN document) against the ontology: what to create, change, retire — [--preview]"}]}

   {:verb :use-case-apply :home :documents
    :leaves [{:path ["use-case" "apply"] :section :data :aliases ["use-case-apply"]
              :sum "stage what the draft's plan may do by itself, on a branch only — [--retire] also deletes what it retires"}]}

   {:verb :use-case-diagram :home :documents
    :leaves [{:path ["use-case" "diagram"] :section :data :aliases ["use-case-diagram"]
              :sum "the draft's object model and statecharts as SVG files — [--out DIR]"}]}

   {:verb :doc-intel-run :home :documents
    :leaves [{:path ["document-intelligence" "run"] :section :data :aliases ["doc-intel-run"]
              :sum "one page of one media item under a strategy (--strategy-file F), optionally judged"}]}

   {:verb :doc-intel-deploy :home :documents
    :leaves [{:path ["document-intelligence" "deploy"] :section :data :aliases ["doc-intel-deploy"]
              :sum "a strategy as a transform template, committed to the branch of a code repository"}]}

   {:verb :document-generate :home :documents
    :args   {:flags {"value" {:key :values :repeat? true :vector? true}}}
    :leaves [{:path ["document" "generate"] :section :data :aliases ["document-generate"]
              :sum "generate AND save a document from a template — --value k=v"}]}

   {:verb :document-refresh :home :documents
    :leaves [{:path ["document" "refresh"] :section :data :aliases ["document-refresh"]
              :sum "re-render a generated document with its stored parameters (--latest: newest template version)"}]}

   {:verb :document-restore :home :documents
    :leaves [{:path ["document" "restore"] :section :data :aliases ["document-restore"]
              :sum "restore an earlier version of a document — --seq N"}]}

   {:verb :document-templatize :home :documents
    :leaves [{:path ["document" "templatize"] :section :data :aliases ["document-templatize"]
              :sum "save a document as a template DRAFT in the checkout (doc-templates/<n>.entt + body) — ships via git push"}]}

   {:verb :workspaces :home :apps
    :leaves [{:path ["workspace" "list"] :section :orient :aliases ["workspaces"]
              :sum "the Workspace Frame workspaces — one curated surface per user group"}]}

   {:verb :workspace :home :apps
    :leaves [{:path ["workspace" "show"] :section :orient :aliases ["workspace"]
              :sum "one workspace: landing, modules, anchored tabs, home page"}]}

   {:verb :scenarios :home :apps
    :leaves [{:path ["app" "scenario" "list"] :section :orient :aliases ["scenarios"]
              :sum "an app's saved scenarios (fork-tier branches)"}]}

   {:verb :scenario-new :home :apps
    :leaves [{:path ["app" "scenario" "new"] :section :orient :aliases ["scenario-new"]
              :sum "fork a new scenario off main: scenario-new <app> <name>"}]}

   {:verb :scenario-drop :home :apps
    :leaves [{:path ["app" "scenario" "drop"] :section :orient :aliases ["scenario-drop"]
              :sum "drop a scenario branch: scenario-drop <app> <seg>"}]}

   {:verb :scenario-actions :home :apps
    :leaves [{:path ["app" "scenario" "actions"] :section :orient :aliases ["scenario-actions"]
              :sum "what a scenario is made of: scenario-actions <app> <seg>"}]}

   {:verb :scenario-seal :home :apps
    :leaves [{:path ["app" "scenario" "seal"] :section :orient :aliases ["scenario-seal"]
              :sum "freeze a scenario — takes no further actions: scenario-seal <app> <seg>"}]}

   {:verb :scenario-rebuild :home :apps
    :args   {:flags {"force" {:key :force :bool? true}}}
    :leaves [{:path ["app" "scenario" "rebuild"] :section :orient :aliases ["scenario-rebuild"]
              :sum "re-evaluate a scenario against today's base (--force replays only what it can)"}]}

   {:verb :scenario-apply :home :apps
    :args   {:flags {"again" {:key :again :bool? true}}}
    :leaves [{:path ["app" "scenario" "apply"] :section :orient :aliases ["scenario-apply"]
              :sum "run a scenario's actions on main for real — writebacks included (--again repeats an applied one)"}]}

   {:verb :scenario-duplicate :home :apps
    :leaves [{:path ["app" "scenario" "duplicate"] :section :orient :aliases ["scenario-duplicate"]
              :sum "copy a scenario in order to modify it: scenario-duplicate <app> <seg> <new-name>"}]}

   {:verb :new-application :home :projects
    :args   {:pos   [{:key :name}]
             :flags {"project"     {:key :project     :kw? true}
                     "object-type" {:key :object-type :kw? true}
                     "description" {:key :description}
                     "stage"       {:key :stage? :bool? true}}}
    :leaves [{:path ["app" "create"] :section :orient
              :sum "scaffold an end-user app → stages: … create <name> [--project p] [--object-type T] [--description …] [--stage]"}]}

   {:verb :rename :home :constructs
    :args   {:pos [{:key :from :kw? true} {:key :to :kw? true}]
             :flags {"kind" {:key :kind :kw? true}}}
    :leaves [{:path ["app" "rename"] :section :orient :preset {:kind :application}
              :sum "RID-stable app rename — moves apps/<name>.enta + its .cljs too: … rename <from> <to>"}
             {:path ["ontology" "object-type" "rename"] :section :ontology
              :preset {:kind :object-type} :sum "RID-stable type rename: … rename <from> <to>"}
             {:path ["ontology" "link-type" "rename"] :section :ontology :preset {:kind :link-type}
              :sum "RID-stable link-type rename: … rename <from> <to>"}
             {:path ["ontology" "interface" "rename"] :section :ontology :preset {:kind :interface}
              :sum "RID-stable interface rename: … rename <from> <to>"}
             {:path ["repository" "rename"] :section :code :preset {:kind :repository}
              :sum "RID-stable repository rename: … rename <from> <to>"}
             {:path ["project" "rename"] :section :project :aliases ["rename"]
              :sum "RID-stable resource rename"}]}

   {:verb :edit :home :constructs
    :leaves [{:path ["app" "edit"] :section :code :preset {:kind :application}
              :sum "read the app's cljs view source (--print); write via the CODE checkout"}
             {:path ["ontology" "object-type" "edit"] :section :ontology :preset {:kind :object-type}
              :sum "edit the type's .ent fragment (attrs/title/status) in $EDITOR (--print / --from), stage → commit"}
             {:path ["function" "edit"] :section :code :preset {:kind :function}
              :sum "edit the function's body (:source) in $EDITOR (--print / --from), stage → commit"}
             {:path ["transform" "edit"] :section :code :preset {:kind :transform}
              :sum "edit the transform's code in $EDITOR (--print / --from <file>), stage → commit"}]}

   {:verb :describe :home :read
    :args   {:variadic {:key :sections}
             :flags    {"project" {:key :project}}
             :post assemble/describe}
    :leaves [{:path ["ontology" "list"] :section :ontology
              :sum "every construct, grouped by section"}
             {:path ["ontology" "object-type" "list"] :section :ontology
              :preset {:sections [:object-types]} :sum "which object-types exist"}
             {:path ["ontology" "object-type" "show"] :section :ontology :aliases ["describe"]
              :sum "one type's shape + live count"}
             {:path ["ontology" "link-type" "list"] :section :ontology
              :preset {:sections [:link-types]} :sum "which link-types exist"}
             {:path ["ontology" "link-type" "show"] :section :ontology :sum "one link-type's shape"}
             {:path ["ontology" "interface" "list"] :section :ontology
              :preset {:sections [:interfaces]} :sum "which interfaces exist"}
             {:path ["ontology" "interface" "show"] :section :ontology :sum "one interface's shape"}
             {:path ["ontology" "action-type" "list"] :section :ontology
              :preset {:sections [:actions]} :sum "which action-types exist"}
             {:path ["ontology" "action-type" "show"] :section :ontology
              :sum "one action-type's shape"}
             {:path ["ontology" "shared-property" "list"] :section :ontology
              :preset {:sections [:shared-properties]} :sum "which shared-properties exist"}
             {:path ["ontology" "shared-property" "show"] :section :ontology
              :sum "one shared-property"}
             {:path ["ontology" "value-type" "list"] :section :ontology
              :preset {:sections [:value-types]} :sum "which value-types exist"}
             {:path ["ontology" "value-type" "show"] :section :ontology :sum "one value-type"}
             {:path ["object-set" "show"] :section :data :sum "one saved object-set's definition"}
             {:path ["transform" "list"] :section :pipeline :preset {:sections [:transforms]}
              :sum "which transforms exist"}
             {:path ["transform" "show"] :section :pipeline :sum "one transform's definition"}
             {:path ["dataset" "list"] :section :pipeline :preset {:sections [:datasets]}
              :sum "which datasets exist"}
             {:path ["media-set" "list"] :section :pipeline :preset {:sections [:media-sets]}
              :sum "which media-sets exist (project, formats, retention)"}
             {:path ["media-set" "show"] :section :pipeline
              :sum "one media-set: whose project decides its reads, and how long it keeps an item"}
             {:path ["export" "list"] :section :pipeline :preset {:sections [:exports]}
              :sum "which exports exist (source, dataset, destination, mode)"}
             {:path ["export" "show"] :section :pipeline
              :sum "one export: which dataset leaves to which source table, and in which of the six modes"}]}

   {:verb :ontology-lint :home :checks
    :args   {:pos [{:key :rule :optional true :kw? true}]}
    :leaves [{:path ["ontology" "lint"] :section :ontology
              :sum "lint the LIVE ontology client-side (anti-patterns incl. the FK-backing invariant); [<rule-id>] narrows"}]}

   {:verb :restore-point :home :maintenance
    :leaves [{:path ["restore-point"] :section :operate :ops ["label" "to"]
              :sum "the data plane's restore points (take · list · roll back)"}]}

   {:verb :prune-orphans :home :maintenance
    :leaves [{:path ["ontology" "prune-orphans"] :section :ontology
              :sum "cleanup: drop the orphan d_/c_ tables `check` reports (previews rows first; --yes to drop)"}]}

   {:verb :retention-plan
    :args   {:pos [{:key :dataset :optional true}]}}

   {:verb :retention-run :home :maintenance
    :leaves [{:path ["retention" "run"] :section :data
              :sum "mark what the policies retire (reads below are refused) and sweep marks older than --grace-days (default 7); previews, --apply"}]}

   {:verb :retention-policy-set :home :maintenance
    :leaves [{:path ["retention" "policy" "set"] :section :data
              :sum "create/replace a custom policy: <name> --datasets all|a,b --older-than-days N --view-count N --transaction-count N --views-older-than-days N [--latest-view] (selectors AND, policies OR)"}]}

   {:verb :media-gc :home :maintenance
    :leaves [{:path ["media" "gc"] :section :ontology :aliases ["media-gc"]
              :sum "cleanup: reclaim CAS bytes no media-reference and no git commit points at (reports; --apply deletes)"}]}

   {:verb :replay-history :home :maintenance
    :leaves [{:path ["schema" "replay-history"] :section :ontology :aliases ["replay-history"]
              :sum "migration: replay the schema history's whole-world texts into per-resource history (--start: dry run; --apply writes; --verify checks every text against its branch; --repair resets, replays again and drops unheld versions; --drop-texts deletes the texts resources now cover; --dedupe makes a resource's versions of one text one version; bare: status)"}]}

   {:verb :schema-code-blobs :home :maintenance
    :leaves [{:path ["schema" "code-blobs"] :section :ontology :aliases ["code-blobs"]
              :sum "migration: move source bodies into the CAS and split the world snapshots in the schema history (reports; --apply writes)"}]}

   {:verb :prune-resolution :home :maintenance
    :leaves [{:path ["ontology" "prune-resolution"] :section :ontology :aliases ["prune-resolution"]
              :sum "cleanup: drop the entity-resolution crosswalk/provenance rows no resolved type claims (reports; --apply deletes)"}]}

   {:verb :new-object-type
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"field"      {:key :fields :repeat? true}
                     "pk"         {:key :pk :kw? true}
                     "display"    {:key :display}
                     "plural"     {:key :plural}
                     "symbol"     {:key :symbol}
                     "implements" {:key :implements :repeat? true}}
             :post  assemble/object-type}
    :leaves [{:path ["ontology" "object-type" "create"] :section :ontology
              :sum "create → stages: … create <name> --field n:type [--pk f] [--implements …]"}]}

   {:verb :delete-construct :home :constructs
    :args   {:pos   [{:key :name :kw? true}]}
    :leaves [{:path ["ontology" "object-type" "delete"] :section :ontology
              :preset {:kind :object-type} :sum "drop a type (immediate; reversible via branch)"}
             {:path ["ontology" "link-type" "delete"] :section :ontology :preset {:kind :link-type}
              :sum "drop a link-type (immediate; reversible via branch)"}
             {:path ["ontology" "action-type" "delete"] :section :ontology
              :preset {:kind :action-type}
              :sum "drop an authored action type (immediate; reversible via branch)"}
             {:path ["transform" "delete"] :section :pipeline :preset {:kind :transform}
              :sum "drop a transform (immediate; reversible via branch)"}
             {:path ["dataset" "delete"] :section :pipeline :preset {:kind :dataset}
              :sum "drop a dataset (immediate; reversible via branch)"}]}

   {:verb :set-status :home :constructs
    :args   {:pos   [{:key :name :kw? true} {:key :status}]
             :flags {"reason"      {:key :reason}
                     "deadline"    {:key :deadline}
                     "replacement" {:key :replacement}}
             :post  assemble/set-status}
    :leaves [{:path ["ontology" "object-type" "status"] :section :ontology
              :preset {:kind :object-type}
              :sum "set lifecycle status <name> <active|experimental|deprecated|endorsed|example> [--reason … --deadline … --replacement T]"}
             {:path ["ontology" "link-type" "status"] :section :ontology :preset {:kind :link-type}
              :sum "set lifecycle status <name> <status> [--reason … --deadline …]"}
             {:path ["ontology" "interface" "status"] :section :ontology :preset {:kind :interface}
              :sum "set lifecycle status <name> <status> [--reason … --deadline …]"}]}

   {:verb :new-link-type
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"from"        {:key :from :kw? true}
                     "to"          {:key :to :kw? true}
                     "cardinality" {:key :cardinality :kw? true}
                     "display"     {:key :display}
                     "fk"          {:key :fk :kw? true}
                     "fk-side"     {:key :fk-side :kw? true}
                     "join"        {:key :join :kw? true}
                     "from-key"    {:key :from-key :kw? true}
                     "to-key"      {:key :to-key :kw? true}}
             :post  assemble/link-type}
    :leaves [{:path ["ontology" "link-type" "create"] :section :ontology
              :sum "create → stages: … create <name> --from A --to B [--cardinality c] (--fk <attr> [--fk-side from/to] or --join <dataset> --from-key <col> --to-key <col>)"}]}

   {:verb :new-interface
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"field"   {:key :fields :repeat? true}
                     "extends" {:key :extends :repeat? true}
                     "display" {:key :display}}
             :post  assemble/interface}
    :leaves [{:path ["ontology" "interface" "create"] :section :ontology
              :sum "create → stages: … create <name> --field n:type [--extends i,…]"}]}

   {:verb :new-shared-property
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"type"    {:key :type :kw? true}
                     "display" {:key :display}}
             :post  assemble/shared-property}
    :leaves [{:path ["ontology" "shared-property" "create"] :section :ontology
              :sum "create → stages: … create <name> --type T [--display …]"}]}

   {:verb :new-value-type
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"type"    {:key :type :kw? true}
                     "one-of"  {:key :one-of}
                     "regex"   {:key :regex}
                     "format"  {:key :format}
                     "min"     {:key :min}
                     "max"     {:key :max}
                     "display" {:key :display}}
             :post  assemble/value-type}
    :leaves [{:path ["ontology" "value-type" "create"] :section :ontology
              :sum "create → stages: … create <name> --type T [--one-of a,b] [--regex R] [--format f]"}]}

   {:verb :query :home :read
    :args   {:pos   [{:key :find :kw? true :optional true}]
             :flags {"show"    {:key :show :repeat? true}
                     "columns" {:key :show :repeat? true}
                     "cols"    {:key :show :repeat? true}
                     "where"   {:key :where :repeat? true}
                     "limit"   {:key :limit :int? true}
                     "latest"  {:key :latest :int? true :optional-val 10}
                     "offset"  {:key :offset :int? true}
                     "count"   {:key :count-only? :bool? true}
                     "around"  {:key :around :repeat? true}
                     "explain" {:key :explain? :bool? true}
                     "object-set" {:key :object-set :kw? true}
                     "save"    {:key :save? :bool? true}
                     "freeze"  {:key :freeze? :bool? true}
                     "engine"   {:key :engine :kw? true}
                     "accuracy" {:key :accuracy :kw? true}
                     "order"   {:key :order-by}}
             :post  assemble/query}
    :leaves [{:path ["object" "query"] :section :data :aliases ["query"]
              :sum "list / filter / count ROWS, chosen columns; --around <side,…> hops along link sides, --explain plans; --save mints a temporary object set, --object-set reads one back"}]}

   {:verb :series :home :read
    :args   {:pos   [{:key :find :kw? true}]
             :flags {"property" {:key :property :kw? true}
                     "prop"     {:key :property :kw? true}
                     "where"    {:key :where :repeat? true}
                     "from"     {:key :from}
                     "until"    {:key :until}
                     "bucket"   {:key :bucket :kw? true}
                     "rollup"   {:key :rollup :kw? true}
                     "limit"    {:key :limit :int? true}}
             :post  assemble/series}
    :leaves [{:path ["object" "series"] :section :data :aliases ["series"]
              :sum "TIME SERIES property values: --property P [--bucket week --rollup sum] [--from D --until D]; a measurement per period WITHOUT a per-period object type"}]}

   {:verb :show :home :read
    :args   {:pos [{:key :objects :kw? true}] :no-flags #{:title}}
    :leaves [{:path ["object" "show"] :section :data :aliases ["show"] :sum "full objects by id"}]}

   {:verb :explore :home :explore
    :leaves [{:path ["object" "search"] :section :data :aliases ["explore"]
              :sum "free-text search, seeds the register"}]}

   {:verb :around :home :explore
    :args   {:variadic {:key :ids :ids? true}
             :flags    {"type" {:key :types :repeat? true :vector? true}}}
    :leaves [{:path ["object" "around"] :section :data :aliases ["around"]
              :sum "1-hop neighbourhood with full neighbours: --type <object type> names the seeds' type (from the register when its rows are the seeds); refused past 10000 links — then `query --around`"}]}

   {:verb :edit-history :home :data
    :args   {:pos [{:key :object-type :kw? true}]}
    :leaves [{:path ["object" "edits"] :section :data :aliases ["edit-history"]
              :sum "user-edit history"}]}

   {:verb :reindex :home :maintenance
    :args   {:pos [{:key :object-type :kw? true}]}
    :leaves [{:path ["object" "reindex"] :section :data :sum "re-sync the object index"}]}

   {:verb :links :home :explore
    :args   {:flags {"type"  {:key :type :kw? true} "from" {:key :from :id? true}}}
    :leaves [{:path ["link" "list"] :section :data :aliases ["links"]
              :sum "link instances (200; --limit/--all)"}]}

   {:verb :action :home :actions
    :args   {:pos   [{:key :name :kw? true}]
             :kv    :args}
    :leaves [{:path ["action" "submit"] :section :data :aliases ["action"]
              :sum "run an action-type (--plan/--stage)"}]}

   {:verb :function-list :home :automations
    :leaves [{:path ["function" "list"] :section :ontology
              :sum "every Function (kind = query|edit), merging the :functions + query-fn transforms"}]}

   {:verb :function-show :home :automations
    :args   {:pos [{:key :name :kw? true}]}
    :leaves [{:path ["function" "show"] :section :ontology
              :sum "one Function's definition (query or edit)"}]}

   {:verb :new-function
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"kind"           {:key :kind :kw? true}
                     "language"       {:key :language :kw? true}
                     "input"          {:key :inputs :repeat? true}
                     "llm"            {:key :llm :repeat? true}
                     "code"           {:key :code}
                     "query"          {:key :query}
                     "code-file"      {:key :code-file}
                     "query-file"     {:key :query-file}
                     "output-type"    {:key :output-type :kw? true}
                     "output-dataset" {:key :output-dataset :kw? true}
                     "column"         {:key :columns :repeat? true}
                     "pk"             {:key :pk :kw? true}
                     "incremental"    {:key :incremental :bool? true}
                     "target-type"    {:key :target-type :kw? true}
                     "source"         {:key :source}
                     "param"          {:key :params :repeat? true}}
             :post  assemble/function}
    :leaves [{:path ["function" "create"] :section :ontology
              :sum (str "scaffold a Function → stages: … create <name> [--kind query|edit] " "(query: transform create's flags, but --language python|clojure and NO output " "— a Function returns its rows, it does not write a table; an argument the " "caller passes is --input label:scalar[:type]; edit: --target-type T)")}]}

   {:verb :run-function :home :automations
    :args   {:pos [{:key :name :kw? true}]
             :kv  :inputs}
    :leaves [{:path ["function" "run"] :section :data :aliases ["run-function"]
              :sum "call a function by name — a query function answers its rows read-only, an edit function applies its edits"}]}

   {:verb :object-sets :home :read
    :leaves [{:path ["object-set" "list"] :section :data :aliases ["object-sets"]
              :sum "saved + temporary object sets; --delete <rid> drops a handle"}]}

   {:verb :preview-transform :home :automations
    :args   {:pos   [{:key :name :kw? true}]
             :flags {"param"  {:key :params :repeat? true :vector? true}
                     "output" {:key :output}}}
    :leaves [{:path ["transform" "preview"] :section :pipeline :aliases ["preview-transform"]
              :sum "read-only 'why 0 rows?'"}]}

   {:verb :test-transform :home :automations
    :args   {:pos [{:key :name :kw? true}]}
    :leaves [{:path ["transform" "test"] :section :pipeline :aliases ["test-transform"]
              :sum "run its :tests — hand-entered inputs, expected output"}]}

   {:verb :consumption :home :data
    :args   {:pos [{:key :transform :kw? true}]}
    :leaves [{:path ["transform" "consumption"] :section :catalog
              :sum "its input consumer-pointers"}]}

   {:verb :delta :home :data
    :args   {:pos [{:key :transform :kw? true} {:key :input :kw? true}]}
    :leaves [{:path ["transform" "delta"] :section :catalog
              :sum "rows to (re)process since last consume"}]}

   {:verb :new-transform
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"language"       {:key :language :kw? true}
                     "input"          {:key :inputs :repeat? true}
                     "llm"            {:key :llm :repeat? true}
                     "code"           {:key :code}
                     "query"          {:key :query}
                     "code-file"      {:key :code-file}
                     "query-file"     {:key :query-file}
                     "output-type"    {:key :output-type :kw? true}
                     "output-dataset" {:key :output-dataset :kw? true}
                     "output-link"    {:key :output-link :kw? true}
                     "from-key"       {:key :from-key :kw? true}
                     "to-key"         {:key :to-key :kw? true}
                     "column"         {:key :columns :repeat? true}
                     "pk"             {:key :pk :kw? true}
                     "incremental"    {:key :incremental :bool? true}
                     "semantic-version" {:key :semantic-version :int? true}}
             :post  assemble/transform}
    :leaves [{:path ["transform" "create"] :section :pipeline
              :sum "scaffold a batch transform → create <name> --input L:kind:ref (--output-type T --pk f | --output-dataset D | --output-link C --from-key c --to-key c) [--code-file P|--query-file P] [--llm L:src:model] [--incremental]"}]}

   {:verb :transform-check :home :automations
    :leaves [{:path ["transform" "check"] :section :pipeline
              :sum "run this transform's Data-Health checks (:health-checks riders authored :on :transform)"}]}

   {:verb :media-set-items :home :automations
    :leaves [{:path ["media-set" "items"] :section :pipeline
              :sum "the items a media-set holds: name, address, format"}]}

   {:verb :transactions :home :data
    :args   {:pos [{:key :dataset :kw? true}]}
    :leaves [{:path ["dataset" "history"] :section :catalog :aliases ["transactions"]
              :sum "the typed transaction chain (SNAPSHOT/APPEND/UPDATE/DELETE)"}]}

   {:verb :dataset-preview :home :data
    :args   {:pos [{:key :dataset :kw? true}]}
    :leaves [{:path ["dataset" "preview"] :section :pipeline :aliases ["dataset-preview"]
              :sum "physical rows + columns of a dataset's d_ table"}]}

   {:verb :force-snapshot
    :args   {:pos   [{:key :dataset :kw? true}]
             :flags {"message" {:key :message}}
             :post  assemble/force-snapshot}}

   {:verb :rollback :home :branches
    :args   {:pos   [{:key :name :kw? true}]}
    :leaves [{:path ["dataset" "rollback"] :section :catalog
              :sum "restore a node's table to an earlier version: … rollback <name> --to <ref> [--kind dataset|object-type|transform]"}]}

   {:verb :dataset-write :home :imports :status :file-only
    :leaves [{:path ["dataset" "write"] :section :pipeline :sum "bulk row write (.ent step)"}]}

   {:verb :upload :home :imports
    :args   {:pos   [{:key :file}]
             :flags {"to"           {:key :dataset}
                     "media-column" {:key :media-column}
                     "set"          {:key :set}
                     "on-upload"    {:key :on-upload :kw? true}
                     "arg"          {:key :arg}
                     "build"        {:key :build}}
             :post  assemble/upload}
    :leaves [{:path ["dataset" "upload"] :section :pipeline :aliases ["upload"]
              :sum "land a media row; --build only for the explicit 'and run it now'"}]}

   {:verb :bank-import :home :imports
    :leaves [{:path ["dataset" "bank-import"] :section :pipeline :aliases ["bank-import"]
              :sum "Accounting konten-CSV preset over upload"}]}

   {:verb :new-dataset
    :args   {:step  :install
             :pos   [{:key :name :kw? true}]
             :flags {"column"      {:key :columns :repeat? true}
                     "kind"        {:key :kind :kw? true}
                     "display"     {:key :display}
                     "description" {:key :description}}
             :post  assemble/dataset}
    :leaves [{:path ["dataset" "create"] :section :pipeline
              :sum "create → stages: … create <name> --column n:type [--kind standalone]"}]}

   {:verb :streams :home :data
    :leaves [{:path ["stream" "list"] :section :pipeline
              :sum "declared streams: partitions, backlog, archived rows"}]}

   {:verb :stream-publish :home :data
    :args   {:pos   [{:key :name :kw? true}]}
    :leaves [{:path ["stream" "publish"] :section :pipeline
              :sum "push records in: … publish <name> --record '{\"c\":1}' | --file rows.json (a batch is admitted or refused whole)"}]}

   {:verb :stream-listen-udp :home :data
    :args   {:pos [{:key :name :kw? true}]}
    :leaves [{:path ["stream" "listen-udp"] :section :pipeline
              :sum "forward UDP datagrams into a stream: … listen-udp <name> --port N (runs until stopped)"}]}

   {:verb :stream-tail :home :data
    :args   {:pos   [{:key :name :kw? true}]}
    :leaves [{:path ["stream" "tail"] :section :pipeline
              :sum "read the changelog BY OFFSET — the replayable read: … tail <name> [--partition N] [--from N] [--limit N]"}]}

   {:verb :stream-offsets :home :data
    :args   {:pos [{:key :name :kw? true}]}
    :leaves [{:path ["stream" "offsets"] :section :pipeline
              :sum "the next offset each partition will take"}]}

   {:verb :stream-view :home :data
    :args   {:pos   [{:key :name :kw? true}]}
    :leaves [{:path ["stream" "view"] :section :pipeline
              :sum "the CDC current view over hot + cold: one row per key, retractions applied"}]}

   {:verb :stream-reset :home :data
    :args   {:pos   [{:key :name :kw? true}]}
    :leaves [{:path ["stream" "reset"] :section :pipeline
              :sum "clear the buffer and both consumer cursors (the archived rows are left alone)"}]}

   {:verb :run-import :home :imports
    :args   {:flags {"only"     {:key :only}
                     "strategy" {:key :strategy :kw? true}
                     "from"     {:key :from}
                     "trace"    {:key :trace? :bool? true}
                     "profile"  {:key :profile? :bool? true}
                     "abort-on-failure" {:key :abort-on-failure? :bool? true}
                     "retry-count"   {:key :retry-count}
                     "retry-backoff" {:key :retry-backoff}
                     "full"     {:key :full? :bool? true}
                     "force"    {:key :force? :bool? true}
                     "detach"   {:key :detach? :bool? true}
                     "wait"     {:key :wait? :bool? true}
                     "again"    {:key :again? :bool? true}}
             :post  assemble/run-import}
    :leaves [{:path ["build" "run"] :section :pipeline :aliases ["run-import"]
              :sum "materialize (--only <object-type|transform|dataset>; --strategy ancestors|between|selected; --retry-count N --retry-backoff 1m --abort-on-failure; --profile: where this run's time went, in `build show`); returns once submitted, --wait waits here"}]}

   {:verb :build-estimate :home :llm
    :args   {:flags {"only"          {:key :only}
                     "strategy"      {:key :strategy :kw? true}
                     "from"          {:key :from}
                     "days"          {:key :days :int? true}
                     "calls-per-row" {:key :calls-per-row}}
             :post  assemble/build-estimate}
    :leaves [{:path ["build" "estimate"] :section :pipeline :aliases ["estimate"]
              :sum "what the model calls of that same build would COST, before running it (--only/--strategy/--from as `build run`; --days <window>, --calls-per-row <n>)"}]}

   {:verb :watch :home :watch
    :args   {:flags {"job" {:key :job}}}
    :leaves [{:path ["build" "watch"] :section :pipeline :aliases ["watch"]
              :sum "follow a live build, 1s: phases + every model call, prompt AND answer (--llm/--build narrow, --jobs adds the roster, --once snapshot, --full untruncated)"}]}

   {:verb :stop :home :observe
    :leaves [{:path ["build" "stop"] :section :pipeline :sum "cancel a running build"}]}

   {:verb :runs :home :observe
    :args   {:flags {"kind" {:key :kind :kw? true}}}
    :leaves [{:path ["build" "list"] :section :pipeline :aliases ["runs"] :sum "recent runs"}]}

   {:verb :run :home :observe
    :args   {:no-flags #{:diff?}}
    :leaves [{:path ["build" "show"] :section :pipeline :aliases ["run"]
              :sum "why one run did what"}]}

   {:verb :jobs :home :observe
    :leaves [{:path ["build" "jobs"] :section :pipeline :sum "the per-transform Job log"}]}

   {:verb :await :home :observe
    :leaves [{:path ["await"] :section :pipeline
              :sum "wait for ONE handle a --detach returned (eval job, build run) until it ends; exit 0 on success, ≠ 0 at once on an unknown handle"}]}

   {:verb :lineage :home :explore
    :leaves [{:path ["lineage" "show"] :section :pipeline :aliases ["lineage"]
              :sum "the transform DAG (inputs→transform→output); one node = its up/downstream"}]}

   {:verb :lineage-facts :home :explore
    :leaves [{:path ["lineage" "facts"] :section :pipeline :aliases ["lineage-facts"]
              :sum (str "edge list + per-node facts: [node|rid] --up/--down N (naming one leaves the" " other at 0) --to <node> (the way data takes) --project P --graph G" " --column C --out-of-date --modified --by F --schedules --dead --stale 7d --blocked" " --diff-live --detail all")}]}

   {:verb :branch-pipeline :home :install
    :args   {:pos [{:key :seed :kw? true} {:key :label :kw? true}]}
    :leaves [{:path ["pipeline" "branch"] :section :branch :aliases ["branch-pipeline"]
              :sum "branch a pipeline: same pipeline on a fresh branch to edit + merge back"}]}

   {:verb :graph-list :home :graphs
    :leaves [{:path ["lineage-graph" "list"] :section :graph :aliases ["lineage-graphs" "graphs"]
              :sum "saved lineage graphs: title · mode · path · branch"}]}

   {:verb :graph-show :home :graphs
    :leaves [{:path ["lineage-graph" "show"] :section :graph :aliases ["lineage-graph"]
              :sum "one lineage graph by <name|rid|path> — its location, scope, branch config, colours"}]}

   {:verb :sdk-generate :home :custom-apps
    :leaves [{:path ["sdk" "generate"] :section :project
              :sum "generate an client SDK project: … generate my.sdk [--out dir] [--package] [--ontology name] [--app custom-app] [--target clj|cljs]"}]}

   {:verb :sdk-verify :home :custom-apps
    :leaves [{:path ["sdk" "verify"] :section :project
              :sum "does a generated SDK still match the live ontology? … verify <dir|file>"}]}

   {:verb :sdk-fingerprint :home :custom-apps
    :leaves [{:path ["sdk" "fingerprint"] :section :project
              :sum "the live ontology's SDK fingerprint — the same 16 hex digits the Python Platform SDK computes"}]}

   {:verb :widget-set-create :home :widget-sets
    :leaves [{:path ["widget-set" "create"] :section :project
              :sum "a new widget set in a project: … create <name> --project <p>"}]}

   {:verb :widget-set-init :home :widget-sets
    :leaves [{:path ["widget-set" "init"] :section :project
              :sum "write a starting widget into a directory: widgets.edn and one .cljs"}]}

   {:verb :widget-set-publish :home :widget-sets
    :leaves [{:path ["widget-set" "publish"] :section :project
              :sum "release a directory as an immutable version: … publish <name> <dir> --version 1.0.0"}]}

   {:verb :widget-set-dev :home :widget-sets
    :leaves [{:path ["widget-set" "dev"] :section :project
              :sum "show a directory to you alone wherever the set is used, following each save: … dev <name> <dir> | dev <name> --stop"}]}

   {:verb :widget-set-pull :home :widget-sets
    :leaves [{:path ["widget-set" "pull"] :section :project
              :sum "write a release's source into a directory: … pull <name> <dir> [--version 1.0.0]"}]}

   {:verb :widget-set-delete :home :widget-sets
    :leaves [{:path ["widget-set" "delete"] :section :project
              :sum "delete a widget set with all its versions"}]}

   {:verb :custom-app :home :custom-apps
    :leaves [{:path ["custom-app"] :section :project :aliases ["custom-apps" "developer-apps"]
              :sum "the Developer Apps's read — custom applications (code OUTSIDE the platform) with every reference judged against the live schema: … custom-app [<key>] [--declaration]"}]}

   {:verb :custom-app-sdk :home :custom-apps
    :leaves [{:path ["custom-app" "sdk"] :section :project
              :sum "the application's SDK, generated from its SCOPE only: … sdk <name> [--out verzeichnis]"}]}

   {:verb :custom-app-docs :home :custom-apps
    :leaves [{:path ["custom-app" "docs"] :section :project
              :sum "SDK reference for the application, rendered from the same narrowing: … docs <name> [--out datei.md]"}]}

   {:verb :custom-app-client-mint :home :custom-apps
    :leaves [{:path ["custom-app" "client" "mint"] :section :project
              :sum "mint an authorization client — the SECRET is shown ONCE: … mint <name> [--grants a,b] [--redirect url]"}]}

   {:verb :graph-svg :home :graphs
    :leaves [{:path ["lineage-graph" "svg"] :section :graph
              :sum "draw one graph as a standalone .svg (server-rendered): … svg <graph> [--layout hierarchie|baum|…] [--richtung links-rechts|oben-unten] [--out datei.svg]"}]}

   {:verb :graph-dot :home :graphs
    :leaves [{:path ["lineage-graph" "dot"] :section :graph
              :sum "the same drawing as Graphviz source: … dot <graph> [--layout …] [--richtung …] [--out datei.dot]"}]}

   {:verb :graph-create :home :graphs
    :args   {:variadic {:key :nodes}
             :flags    {"node" {:key :node :repeat? true}}}
    :leaves [{:path ["lineage-graph" "create"] :section :graph
              :sum "save a new lineage graph: … create <title> [--mode lineage] [--node <ref>…] [--ancestors N|all] [--descendants N|all] [--into <folder>] [--project P] [--branch B] [--fallback a,b]"}]}

   {:verb :graph-save-as :home :graphs
    :leaves [{:path ["lineage-graph" "save-as"] :section :graph
              :sum "copy one under a new title: … save-as <graph> <new-title> [--into <folder>]"}]}

   {:verb :graph-rename :home :graphs
    :leaves [{:path ["lineage-graph" "rename"] :section :graph
              :sum "retitle a lineage graph (RID + placement stable)"}]}

   {:verb :graph-move :home :graphs
    :leaves [{:path ["lineage-graph" "move"] :section :graph
              :sum "relocate a lineage graph in the kompass tree: … move <graph> [--into <folder>]"}]}

   {:verb :graph-scope :home :graphs
    :args   {:flags {"add"         {:key :add    :repeat? true}
                     "remove"      {:key :remove :repeat? true}}}
    :leaves [{:path ["lineage-graph" "scope"] :section :graph
              :sum "edit which nodes it draws: … scope <graph> [--add <ref>…] [--remove <ref>…] [--ancestors N|all] [--descendants N|all]"}]}

   {:verb :graph-branch :home :graphs
    :leaves [{:path ["lineage-graph" "branch"] :section :graph
              :sum "set the branch config saved WITH the graph: … branch <graph> --branch B [--fallback a,b]"}]}

   {:verb :graph-arrange :home :graphs
    :leaves [{:path ["lineage-graph" "arrange"] :section :graph
              :sum "the layout a graph opens in where nothing is placed by hand: … arrange <graph> hier-lr|hier-tb|clustered|grid|circular|force|tree|none"}]}

   {:verb :graph-delete :home :graphs
    :leaves [{:path ["lineage-graph" "delete"] :section :graph
              :sum "delete a lineage graph (its layout, colours + placement are swept)"}]}

   {:verb :projection-list :home :checkouts :status :client
    :args   {}
    :leaves [{:path ["projection" "list"] :section :code :sum "the schema's declared projections"}]}

   {:verb :projection-find :home :checkouts :status :client
    :args   {:pos   [{:key :needle}]
             :flags {"projection" {:key :projection}}}
    :leaves [{:path ["projection" "find"] :section :code
              :sum "which text holds a string, at which line and node: … find <text> [--projection <p>]"}]}

   {:verb :projection-where :home :checkouts :status :client
    :args   {:variadic {:key :names}
             :flags    {"projection" {:key :projection}}}
    :leaves [{:path ["projection" "where"] :section :code
              :sum "which text of which projection holds a node — or that it is installed in no text: … where <name> … [--projection <p>]"}]}

   {:verb :projection-pull :home :checkouts :status :client
    :args   {:pos      [{:key :projection}]
             :variadic {:key :texts}
             :flags    {"project" {:key :project}
                        "to"      {:key :to}
                        "force"   {:key :force? :bool? true}
                        "rehome"  {:key :rehome? :bool? true}}}
    :leaves [{:path ["projection" "pull"] :section :code
              :sum "check out one text, a project's texts or all, with their base: … pull <projection> [<text> …] [--project <p>] [--to <dir>] [--force] [--rehome]"}]}

   {:verb :projection-push :home :checkouts :status :client
    :args   {:variadic {:key :texts}
             :flags    {"preview"        {:key :preview? :bool? true}
                        "adopt"          {:key :adopt? :bool? true}
                        "mine"           {:key :mine? :bool? true}
                        "theirs"         {:key :theirs? :bool? true}
                        "land"           {:key :land? :bool? true}
                        "stage"          {:key :stage? :bool? true}}}
    :leaves [{:path ["projection" "push"] :section :code
              :sum "merge a pulled text's checkout over its base and stage it: … push <text> … [--preview] [--mine|--theirs] [--land|--stage] [--adopt]"}]}

   {:verb :slice-pull :home :checkouts :status :client
    :args   {:variadic {:key :nodes}
             :flags    {"to"    {:key :to}
                        "force" {:key :force? :bool? true}}}
    :leaves [{:path ["slice" "pull"] :section :code
              :sum "check out nodes across texts and their code in one folder: … pull <node> … --to <dir> [--force]"}]}

   {:verb :slice-push :home :checkouts :status :client
    :args   {:pos   [{:key :dir}]
             :flags {"preview" {:key :preview? :bool? true}
                     "message" {:key :message :aliases ["-m"]}
                     "mine"    {:key :mine? :bool? true}
                     "theirs"  {:key :theirs? :bool? true}}}
    :leaves [{:path ["slice" "push"] :section :code
              :sum "send a slice's texts and code in one push, then pull it again: … push <dir> [--preview] [--mine|--theirs] [-m <message>]"}]}

   {:verb :clone :status :client
    :leaves [{:path ["code" "clone"] :section :code :aliases ["clone"]
              :sum "check a project's python transforms + app views out to editable files"}]}

   {:verb :push :status :client
    :leaves [{:path ["code" "push"] :section :code :aliases ["push"]
              :sum "push edited code files back through the install door"}]}

   {:verb :pull :status :client
    :leaves [{:path ["code" "pull"] :section :code :aliases ["pull"]
              :sum "3-way rebase the checkout onto the server's new HEAD"}]}

   {:verb :new-repository :home :projects
    :args   {:pos   [{:key :name}]
             :flags {"project"     {:key :project :kw? true}
                     "type"        {:key :type :kw? true}
                     "description" {:key :description}
                     "stage"       {:key :stage? :bool? true}}}
    :leaves [{:path ["repository" "create"] :section :code
              :sum "create a code repository in a project: … create <name> [--project p] [--type transforms|functions|applications] [--description …] [--stage]"}]}

   {:verb :repositories :home :projects
    :leaves [{:path ["repository" "list"] :section :code :aliases ["repositories"]
              :sum "the code repositories + members"}]}

   {:verb :repo-place :home :projects
    :args   {:pos      [{:key :repository :kw? true}]
             :variadic {:key :resources}}
    :leaves [{:path ["repository" "place"] :section :code
              :sum "file transforms/functions/apps INTO a repository (the checkout unit): … place <repo> <resource>…"}]}

   {:verb :repo-unplace :home :projects
    :args   {:variadic {:key :resources}}
    :leaves [{:path ["repository" "unplace"] :section :code
              :sum "take resources OUT of their repository (clears :repository): … unplace <resource>…"}]}

   {:verb :version-show :home :branches
    :args   {:pos   [{:key :kind :kw? true} {:key :name :kw? true}]}
    :leaves [{:path ["version" "show"] :section :versions
              :sum "one resource's definition at an earlier version: … show <object-type|transform|source> <name> --as-of <ref>"}]}

   {:verb :version-rows :home :branches
    :args   {:pos   [{:key :type :kw? true}]}
    :leaves [{:path ["version" "rows"] :section :versions
              :sum "an object type's ROWS at an earlier version: … rows <type> --as-of <ref> [--limit N]"}]}

   {:verb :sources :home :sources
    :leaves [{:path ["source" "list"] :section :source :aliases ["sources"]
              :sum "declared sources"}]}

   {:verb :source :home :sources
    :leaves [{:path ["source" "show"] :section :source :aliases ["source"]
              :sum "one source + live probe"}]}

   {:verb :source-add :home :sources
    :leaves [{:path ["source" "add"] :section :source :aliases ["source-add"]
              :sum "register a new source (seal+stage)"}]}

   {:verb :source-secret :home :sources
    :leaves [{:path ["source" "secret"] :section :source :aliases ["source-secret"]
              :sum "seal a credential onto an EXISTING source (the half git may not carry)"}]}

   {:verb :datasources :home :install :status :file-only
    :leaves [{:path ["source" "apply"] :section :source
              :sum "bulk-upsert source configs (.ent step)"}]}

   {:verb :source-tables :home :sources
    :leaves [{:path ["source" "tables"] :section :source :aliases ["source-tables"]
              :sum "introspect tables"}]}

   {:verb :source-columns
    :args   {:pos [{:key :name} {:key :table}]}}

   {:verb :peek :home :sources
    :leaves [{:path ["source" "peek"] :section :source :aliases ["peek"] :sum "sample live rows"}]}

   {:verb :source-adopt :home :projects
    :leaves [{:path ["source" "adopt"] :section :source
              :sum "a table as a virtual dataset (+ --as <Type> backed by it) in one change: <source> <table> [--key a,b] [--into <dataset>|--dataset <name>] [--project p] [--preview] [--land|--stage]"}]}

   {:verb :source-sql :home :sources
    :leaves [{:path ["source" "sql"] :section :source :aliases ["source-sql"]
              :sum "one ad-hoc read-only query"}]}

   {:verb :source-graph :home :sources
    :leaves [{:path ["source" "graph"] :section :source :aliases ["source-graph"]
              :sum "foreign-key graph"}]}

   {:verb :writeback-check :home :sources
    :leaves [{:path ["writeback" "check"] :section :source :aliases ["writeback-check"]
              :sum "the target table against its allowlist, before a new write action"}]}

   {:verb :sync :home :projects
    :args   {:flags {"as" {:key :as :kw? true}}}
    :leaves [{:path ["source" "sync"] :section :source :aliases ["sync"]
              :sum "codegen an :external-source type + stage"}]}

   {:verb :egress :home :maintenance
    :leaves [{:path ["source" "egress"] :section :source :sum "the kernel egress allowlist"}]}

   {:verb :egress-policy-propose :home :maintenance
    :leaves [{:path ["source" "egress" "policy" "propose"] :section :source
              :sum "propose a policy; it stays pending: … propose <host> <port> [--why text] [--agents a,b]"}]}

   {:verb :agent-proxy-allowlist :home :maintenance
    :leaves [{:path ["source" "egress" "agent-allowlist"] :section :source
              :sum "what an agent proxy may reach (admin): … agent-allowlist <agent> <cidr:port[-end],…>"}]}

   {:verb :egress-policy-adopt :home :maintenance
    :leaves [{:path ["source" "egress" "policy" "adopt"] :section :source
              :sum "make what sources declare active policies (admin): … adopt [--except host:port,…]"}]}

   {:verb :review :home :read
    :args   {:no-flags #{:title}}
    :leaves [{:path ["changeset" "review"] :section :changeset :sum "staged diff by entity class"}]}

   {:verb :check :home :checks
    :leaves [{:path ["changeset" "check"] :section :changeset :sum "pre-commit safety report"}]}

   {:verb :lint :home :checks
    :leaves [{:path ["changeset" "lint"] :section :changeset :sum "anti-pattern findings"}]}

   {:verb :diff-schema :home :checks
    :args   {:pos      [{:key :preset :optional true}]
             :variadic {:key :presets}
             :post     assemble/diff-schema}
    :leaves [{:path ["changeset" "diff"] :section :changeset :aliases ["diff-schema" "diff"]
              :sum "the staged diff (bare), or preview file(s) — .ent/.enta/.entv/.entp, siblings + code inlined (non-mutating)"}]}

   {:verb :commit :home :install
    :args   {:hint (str "a commit carries no message — the batch is auto-described;"
                        " describe the change on its proposal:"
                        " propose \"<title>\" --desc \"…\" --source <b> --target main")}
    :leaves [{:path ["changeset" "commit"] :section :changeset
              :sum "land it (raw; takes no args — the batch is auto-described, no -m)"}]}

   {:verb :save :status :meta
    :leaves [{:path ["changeset" "save"] :section :changeset
              :sum "land it — gated (check + typed confirm)"}]}

   {:verb :discard :status :meta
    :leaves [{:path ["changeset" "discard"] :section :changeset :sum "drop the batch"}]}

   {:verb :preview-staged :home :branches
    :leaves [{:path ["changeset" "preview"] :section :changeset
              :sum "would this land? — :clean | :conflicts | :no-base, per entity, BEFORE committing"}]}

   {:verb :rebase-staged :home :branches
    :args   {:flags {"mine"   {:key :mine   :repeat? true}
                     "theirs" {:key :theirs :repeat? true}}}
    :leaves [{:path ["changeset" "rebase"] :section :changeset
              :sum "settle conflicts on the branch: … rebase [--mine <entity>…] [--theirs <entity>…] (unnamed default to theirs)"}]}

   {:verb :discard-one :home :branches
    :leaves [{:path ["changeset" "drop"] :section :changeset
              :sum "drop ONE staged change by seq (`changeset discard` drops the whole batch)"}]}

   {:verb :projects :home :projects
    :leaves [{:path ["project" "list"] :section :project :aliases ["projects"]
              :sum "the ownership/permission containers"}]}

   {:verb :project-map :home :projects
    :args   {:pos [{:key :project :kw? true :optional true}]}
    :leaves [{:path ["project" "map"] :section :project :aliases ["project-map"]
              :sum "a project's app×type relational map"}]}

   {:verb :project-use :home :branches
    :leaves [{:path ["project" "use"] :section :project
              :sum "the project new resources go to: <project> for this session; --default for the instance (admin); bare shows it"}]}

   {:verb :project-policy :home :branches
    :leaves [{:path ["project" "policy"] :section :project
              :sum "a project's own policy: <project> [--review <edn>|--default-review] [--protect-all|--no-protect-all]"}]}

   {:verb :export :status :client
    :leaves [{:path ["project" "export"] :section :project
              :sum "DEPRECATED → use `code clone <project> --archive` (the single download verb → re-installable .ent/.zip)"}]}

   {:verb :new-project :home :projects
    :args   {:pos   [{:key :name}]
             :flags {"description" {:key :description}
                     "stage"       {:key :stage? :bool? true}}}
    :leaves [{:path ["project" "create"] :section :project
              :sum "create an ownership/permission container: … create <name> [--description …] [--stage]"}]}

   {:verb :image-push :home :images
    :leaves [{:path ["image" "push"] :section :pipeline
              :sum "push a `docker save` archive for with_container: <name>:<tag> <file.tar> --project P"}]}

   {:verb :image-delete :home :images
    :leaves [{:path ["image" "delete"] :section :pipeline
              :sum "delete a pushed image: <name>:<tag>"}]}

   {:verb :package-install :home :projects
    :leaves [{:path ["package" "install"] :section :project
              :sum "install a package: <id> [--preview] [--land|--stage] — lands at once on a klein trunk"}]}

   {:verb :package-upgrade :home :projects
    :leaves [{:path ["package" "upgrade"] :section :project
              :sum "move an installed package to the offered release, merged with what you changed since: <id> [--preview] [--mine|--theirs] [--land|--stage]"}]}

   {:verb :product-publish :home :projects
    :leaves [{:path ["product" "publish"] :section :project
              :sum "publish projects as a product release: <product> <version> --projects a,b [--channel dev|test|stable] [--release] [--requires other@^1.2] [--description …]"}]}

   {:verb :product-install :home :projects
    :leaves [{:path ["product" "install"] :section :project
              :sum "install a released product here: <product> [version] [--suffix S] [--prefix P] [--bind input=local] [--lock] [--channel C] [--auto-upgrade] [--window-days sat,sun --window-from 22:00 --window-to 04:00 --window-zone Europe/Vienna] [--with-linked] [--preview] [--land|--stage]"}]}

   {:verb :product-upgrade :home :projects
    :leaves [{:path ["product" "upgrade"] :section :project
              :sum "move an installation to the newest release its channel offers (or [version], also back), merged with your edits: <installation-id> [version] [--preview] [--mine|--theirs] [--land|--stage]"}]}

   {:verb :product-uninstall :home :projects
    :leaves [{:path ["product" "uninstall"] :section :project
              :sum "take an installation out through the staging door: what it installed, its documents once it lands (its projects stay, emptied); refused while another installation needs it: <installation-id> [--preview] [--land|--stage]"}]}

   {:verb :product-configure :home :projects
    :leaves [{:path ["product" "configure"] :section :project
              :sum "an installation's upgrade settings: <installation-id> [--channel C] [--auto-upgrade|--manual] [--window-days … --window-from … --window-to … --window-zone …|--no-window]"}]}

   {:verb :product-export :home :projects
    :leaves [{:path ["product" "export"] :section :project
              :sum "write a release as a signed bundle file for another instance: <product> <version> [--out file.edn]"}]}

   {:verb :product-import :home :projects
    :leaves [{:path ["product" "import"] :section :project
              :sum "take in a bundle file another instance exported (checked: digest, signature of a trusted issuer, no credentials): <file> [--channel C] [--require-signature]"}]}

   {:verb :project-files :home :projects
    :leaves [{:path ["project" "files"] :section :project
              :sum "the kompass filesystem tree (folders + placements)"}]}

   {:verb :place :home :projects
    :args   {:variadic {:key :resources}
             :flags    {"into" {:key :into :kw? true}
                        "as"   {:key :as   :kw? true}}}
    :leaves [{:path ["project" "place"] :section :project
              :sum "file a resource in the kompass tree: … place <resource…> [--into <folder>] [--as <name>]"}]}

   {:verb :unplace :home :projects
    :args   {:variadic {:key :resources}}
    :leaves [{:path ["project" "unplace"] :section :project
              :sum "remove a resource from the kompass tree (inverse of place; resource untouched)"}]}

   {:verb :folder :home :projects
    :leaves [{:path ["project" "folder" "create"] :section :project :preset {:op :create}
              :sum "new kompass folder: … folder create <name> [--into <parent>]"}
             {:path ["project" "folder" "rename"] :section :project :preset {:op :rename}
              :sum "rename a kompass folder: … folder rename <folder> <new-name>"}
             {:path ["project" "folder" "move"] :section :project :preset {:op :move}
              :sum "reparent a kompass folder: … folder move <folder> [--into <parent>]"}
             {:path ["project" "folder" "delete"] :section :project :preset {:op :delete}
              :sum "delete a kompass folder (contents reparented up)"}]}

   {:verb :trash :home :constructs
    :leaves [{:path ["project" "trash"] :section :versions :aliases ["trash"]
              :sum "recoverable deletions (delete IS soft — the commit graph keeps history)"}]}

   {:verb :restore :home :constructs
    :leaves [{:path ["project" "restore"] :section :versions :aliases ["restore"]
              :sum "undelete a construct from history: restore <kind> <name>"}]}

   {:verb :schedule :home :automations
    :args   {:kv    :args
             :flags {"do" {:key :do :kw? true}}}
    :leaves [{:path ["schedule" "run"] :section :operate :preset {:do :run}
              :sum "start one build schedule: <name> [param=value…] — on a parallelized schedule, the run setup of those values"}
             {:path ["schedule" "pause"] :section :operate :preset {:do :pause}
              :sum "pause one schedule"}
             {:path ["schedule" "unpause"] :section :operate :preset {:do :unpause}
              :sum "unpause one schedule"}
             {:path ["schedule" "delete"] :section :operate :preset {:do :delete}
              :sum "remove one schedule: <name>"}
             {:path ["schedule" "status"] :section :operate :preset {:do :status}
              :aliases ["schedule"] :sum "one schedule's state"}]}

   {:verb :schedule-list :home :automations
    :leaves [{:path ["schedule" "list"] :section :operate
              :sum "all build schedules: target · cadence · state · last-fired"}]}

   {:verb :schedule-candidates :home :explore
    :args   {:pos [{:key :node}]}
    :leaves [{:path ["schedule" "candidates"] :section :operate
              :sum "DAG-derived parent nodes a :parent-updated trigger on <target> can watch"}]}

   {:verb :schedule-create :home :explore
    :args   {:pos   [{:key :name}]
             :kv    :parameters
             :flags {"target"           {:key :target}
                     "run-setup-key"    {:key :run-setup-key :kw? true}
                     "full"             {:key :full? :bool? true}
                     "retry-count"      {:key :retry-count}
                     "retry-backoff"    {:key :retry-backoff}
                     "abort-on-failure" {:key :abort-on-failure? :bool? true}
                     "on-parent-update" {:key :on-parent-update}
                     "on-listener"      {:key :on-listener}
                     "listen-folders"   {:key :listen-folders}
                     "scope"            {:key :scope :kw? true}
                     "from"             {:key :from}
                     "cron"             {:key :cron}
                     "every-ms"         {:key :every-ms}
                     "trigger-mode"     {:key :trigger-mode :kw? true}}}
    :leaves [{:path ["schedule" "create"] :section :operate
              :sum "create/upsert a schedule: <name> --target T[,T…] [--on-parent-update P[,P…]] [--on-listener SOURCE [--listen-folders F[,F…]] = fire within seconds of an arrival (IMAP IDLE)] [--scope self|with-dependencies|downstream|connecting --from B] [--cron C|--every-ms N] [--trigger-mode any|all] [--full = run it with incrementality OFF, the reconciliation] [--retry-count N --retry-backoff 1m --abort-on-failure] [param=value… = its parameter values] [--run-setup-key P = parallelized: one run setup per value of P, started by `schedule run` or an action]"}]}

   {:verb :automation :home :automations
    :args   {:kv  :args
             :flags {"do"          {:key :do :kw? true}
                     "status"      {:key :status :kw? true}
                     "ids"         {:key :ids}
                     "where"       {:key :where}
                     "batch-size"  {:key :batch-size :int? true}
                     "parallelism" {:key :parallelism :int? true}}}
    :leaves [{:path ["automation" "run"] :section :operate :preset {:do :run}
              :sum "run one automation rule"}
             {:path ["automation" "status"] :section :operate :preset {:do :status}
              :aliases ["automation"] :sum "one automation's state"}
             {:path ["automation" "proposals"] :section :operate :preset {:do :proposals}
              :sum "agent proposals an automation staged for review: <name> [--status open] [--limit n --offset n]"}
             {:path ["automation" "accept"] :section :operate :preset {:do :accept}
              :sum "accept one proposal — its Action runs as you: <name> <proposal-id> [param=value… to edit first]"}
             {:path ["automation" "reject"] :section :operate :preset {:do :reject}
              :sum "reject one proposal: <name> <proposal-id>"}
             {:path ["automation" "mute"] :section :operate :preset {:do :mute}
              :sum "mute an automation: its condition runs, its effects do not"}
             {:path ["automation" "unmute"] :section :operate :preset {:do :unmute}
              :sum "unmute an automation (also one auto-mute muted)"}
             {:path ["automation" "subscribe"] :section :operate :preset {:do :subscribe}
              :sum "be told what an automation does: triggered, recovered, errors, other info"}
             {:path ["automation" "unsubscribe"] :section :operate :preset {:do :unsubscribe}
              :sum "stop being told what an automation does"}]}

   {:verb :automation-list :home :automations
    :leaves [{:path ["automation" "list"] :section :operate
              :sum "all automation rules: trigger · cadence · state · last-fired"}]}

   {:verb :health :home :maintenance
    :args   {:flags {"sql" {:key :sql :bool? true}}}
    :leaves [{:path ["health"] :section :operate :sum "the health question (status now)"}]}

   {:verb :telemetry :home :maintenance
    :leaves [{:path ["telemetry"] :section :operate
              :sum "what the deployment costs: each process's heap/RSS/swap/CPU/pools, pg's footprint"}]}

   {:verb :client-log :home :maintenance
    :args   {:flags {"errors" {:key :errors? :bool? true}
                     "level"  {:key :level}
                     "user"   {:key :user}
                     "since"  {:key :since}
                     "q"      {:key :q}
                     "stack"  {:key :stack? :bool? true}
                     "limit"  {:key :limit :int? true}}}
    :leaves [{:path ["client-log"] :section :operate
              :sum "what browsers reported: render errors, uncaught, failed calls — newest first, stack mapped to source"}]}

   {:verb :settings :home :maintenance
    :leaves [{:path ["settings"] :section :operate
              :sum "the instance's operating policy, set without a deploy: [<key> [<value>]] [--reset]"}]}

   {:verb :linter-snooze :home :automations
    :args   {:pos   [{:key :rule} {:key :resource}]
             :flags {"days" {:key :days :int? true} "reason" {:key :reason}
                     "off" {:key :off :bool? true}}}
    :leaves [{:path ["linter" "snooze"] :section :operate
              :sum "hide one until a date: <rule> <resource> --days N --reason R; --off ends the snooze"}]}

   {:verb :health-check-list :home :automations
    :leaves [{:path ["health-check" "list"] :section :operate
              :sum "declared checks: kind · target · severity · latest result"}]}

   {:verb :health-check-run :home :automations
    :leaves [{:path ["health-check" "run"] :section :operate
              :sum "run ONE check by name (health = run all)"}]}

   {:verb :health-check-set :home :automations
    :leaves [{:path ["health-check" "set"] :section :operate
              :sum "create/update a Data-Health check in-platform (the resource's health tab): <name> --target T [--on object-type|transform|dataset] --kind freshness|row-count|null-percentage|approximate-unique|custom|… [--severity moderate|critical] [--column c --within-hours n --min n --max n --comparison k --measure count|sum --tolerance n --query SELECT… --paused]; or --from <file.edn> with one rider map or a vector of them (for :against)"}]}

   {:verb :health-check-delete :home :automations
    :leaves [{:path ["health-check" "delete"] :section :operate
              :sum "remove one Data-Health check by name"}]}

   {:verb :health-check-watch :home :automations
    :leaves [{:path ["health-check" "watch"] :section :operate
              :sum "be told when a check fails (in-platform/mail per `notifications settings`): <name> [--level all|critical|none]"}]}

   {:verb :health-check-snooze :home :automations
    :leaves [{:path ["health-check" "snooze"] :section :operate
              :sum "silence a check's alerts for everyone: <name> --hours N | --until <iso> [--reason R]"}]}

   {:verb :health-check-group-list :home :automations
    :leaves [{:path ["health-check-group" "list"] :section :operate
              :sum "check groups: their checks and who gets their digest"}]}

   {:verb :health-check-group-set :home :automations
    :leaves [{:path ["health-check-group" "set"] :section :operate
              :sum "create/update a check group: <name> --checks a,b,c [--description D]"}]}

   {:verb :health-check-group-subscribe :home :automations
    :leaves [{:path ["health-check-group" "subscribe"] :section :operate
              :sum "get the group's digest: <name> [--every 15m|1h|daily|weekly] [--all: also when nothing fails]"}]}

   {:verb :notification-settings :home :automations
    :leaves [{:path ["notifications" "settings"] :section :operate
              :sum "how you are told, per kind: [--set health=both,action=email,default=platform] [--mute <automation-id>] [--unmute <automation-id>]; kinds action automation review branch mention health default, ways platform email both off"}]}

   {:verb :llm :home :llm
    :args   {:flags {"run" {:key :run}}}
    :leaves [{:path ["llm" "list"] :section :llm :preset {:sub "models"} :aliases ["llms"]
              :sum "registered models (+ rate timeline)"}
             {:path ["llm" "register"] :section :llm :preset {:sub "register"}
              :sum "set current price: <id> --in --out [--source <provider source> --provider openai-chat|anthropic-messages --provider-model <the provider's id, when <id> is a neutral name code refers to> --window <tokens> --tool-calling]"}
             {:path ["llm" "reprice"] :section :llm :preset {:sub "reprice"}
              :sum "record a dated rate"}
             {:path ["llm" "unregister"] :section :llm :preset {:sub "unregister"}
              :sum "drop from the allow-list"}
             {:path ["llm" "cost"] :section :llm :preset {:sub "cost"} :aliases ["cost"]
              :sum "the money view"}
             {:path ["llm" "budget"] :section :llm :preset {:sub "budget"}
              :sum "daily LLM budget, interactive reserve and project quotas: show, or --set <eur> (0 = unlimited), --reserve <pct>, --project <p> --quota <eur|none>, --source <provider> --quota <eur|none>, --source <provider> --resume once its account is topped up (a provider that answers 'no credits' is held back an hour at a time); a transform past a limit waits, it is not failed"}
             {:path ["llm" "sync-rates"] :section :llm :preset {:sub "sync-rates"}
              :sum "fetch a provider's catalog → register rates (--source, --apply)"}]}

   {:verb :audit :home :llm
    :args   {:pos   [{:key :request :optional true}]
             :flags {"caller"  {:key :caller}
                     "model"   {:key :model}
                     "errors"  {:key :errors? :bool? true}
                     "since"   {:key :since}
                     "until"   {:key :until}
                     "today"   {:key :today? :bool? true}
                     "limit"   {:key :limit :int? true}
                     "request" {:key :request}}}
    :leaves [{:path ["llm" "audit"] :section :llm
              :sum "AI run observability: the per-call trace over llm_call_log (newest first; --caller/--model/--errors/--since/--limit; <request-id> = full request+response)"}]}

   {:verb :llm-usage :home :llm
    :leaves [{:path ["llm" "usage"] :section :llm
              :sum "where LLMs are used across BOTH planes: batch (transform/function :language-model inputs) + interactive (agents) + embeddings"}]}

   {:verb :llm-next :home :llm
    :leaves [{:path ["llm" "next"] :section :llm
              :sum "WHEN model spend happens next: every schedule whose build scope contains a :language-model transform, with its next fire — and the paused ones that would start spending on unpause"}]}

   {:verb :eval :home :llm
    :args   {:no-flags #{:op :diff :run}
             :flags {"suite" {:key :suite}
                     "meta"  {:key :meta :repeat? true}
                     "grid"  {:key :grid :repeat? true}}}
    :leaves [{:path ["llm" "eval"] :section :llm :aliases ["eval"]
              :ops ["run" "suites" "list" "history" "save" "cases" "compare" "job"]
              :sum "score/compare/admin eval suites"}]}

   {:verb :agent-list :home :llm
    :leaves [{:path ["agent" "list"] :section :llm
              :sum "installed AI Platform agents (interactive LLM + Ontology tools)"}]}

   {:verb :agent-show :home :llm
    :args   {:pos [{:key :name :kw? true}]}
    :leaves [{:path ["agent" "show"] :section :llm :sum "one agent's model, tools, retrieval"}]}

   {:verb :agent-run :home :llm
    :args   {:pos   [{:key :name :kw? true}]
             :flags {"input"     {:key :input}
                     "max-turns" {:key :max-turns :int? true}}}
    :leaves [{:path ["agent" "run"] :section :llm
              :sum "invoke an agent's tool-calling loop: … run <name> --input \"…\" [--max-turns N]"}]}

   {:verb :access :home :access
    :leaves [{:path ["access" "show"] :section :access :aliases ["access"]
              :sum "the ACL snapshot"}]}

   {:verb :access-request :home :access
    :leaves [{:path ["access" "request"] :section :access
              :sum "ask for a project role or a group: role <project> --role <r> | group <group> [--for <user>] [--justification <why>]"}]}

   {:verb :access-decide :home :access
    :leaves [{:path ["access" "decide"] :section :access
              :sum "decide an access request: <id> [--reject] [--note <why>]"}]}

   {:verb :access-explain :home :access
    :leaves [{:path ["access" "explain"] :section :access
              :sum "why a principal may or may not do an operation on a project: <user> <op> <project> (admin)"}]}

   {:verb :access-role :home :access
    :args   {:pos [{:key :user}]}
    :leaves [{:path ["access" "role"] :section :access
              :sum "a principal's platform role: <username|user-id> [--set <role>] (admin)"}]}

   {:verb :org-guest :home :identity
    :args   {:pos   [{:key :op} {:key :organization} {:key :principal}]
             :flags {"stage" {:key :stage? :bool? true}}}
    :leaves [{:path ["org" "guest"] :section :access
              :sum (str "guest membership: add|remove <organization> <principal> [--stage]"
                        " — a guest is inside the organization without it being their primary")}]}

   {:verb :grant :home :identity
    :args   {:pos   [{:key :project :kw? true}]
             :flags {"to" {:key :to :id? true}}}
    :leaves [{:path ["access" "grant"] :section :access :sum "grant / revoke (immediate)"}]}

   {:verb :register
    :args   {:step  :principals
             :pos   [{:key :username}]
             :flags {"password"    {:key :password}
                     "user"        {:key :user-id :id? true}
                     "external"    {:key :external}
                     "auth-source" {:key :auth-source :kw? true}}
             :post  assemble/register}
    :leaves [{:path ["access" "register"] :section :access
              :sum "provision a principal (--external = SSO)"}]}

   {:verb :sso :home :identity
    :args   {:pos   [{:key :sub}]}
    :leaves [{:path ["access" "sso"] :section :access :ops ["status" "config"]
              :sum "Kerberos/SPNEGO admin"}]}

   {:verb :ldap :home :identity
    :args   {:pos   [{:key :sub}]
             :flags {"filter" {:key :user-filter}}}
    :leaves [{:path ["access" "ldap"] :section :access :ops ["status" "config" "sync"]
              :sum "AD directory sync"}]}

   {:verb :idp :home :identity
    :args   {:kv :settings}
    :leaves [{:path ["access" "idp"] :section :access
              :ops ["status" "settings" "config" "preview" "sync"]
              :sum "identity providers, as the server declares them"}]}

   {:verb :login :home :identity :status :file-only
    :leaves [{:path ["login"] :section :access :sum "authenticate the session"}]}

   {:verb :feedback :home :feedback
    :args   {:pos [{:key :id :optional true}]
             :flags {"benutzer" {:key :benutzer}
                     "erledigt" {:key :erledigt? :bool? true}
                     "offen"    {:key :offen? :bool? true}
                     "szenario" {:key :szenario? :bool? true}
                     "ereignis" {:key :ereignis}
                     "to"       {:key :to}}}
    :leaves [{:path ["access" "feedback"] :section :access :aliases ["feedback"]
              :sum "Feedback: was eine Nutzerin bei echter Arbeit tat und dazu meldete"}]}

   {:verb :login-audit :home :llm
    :args   {:pos [{:key :user :optional true}]
             :flags {"user"     {:key :user}
                     "outcome"  {:key :outcome}
                     "failed"   {:key :failed? :bool? true}
                     "provider" {:key :provider}
                     "limit"    {:key :limit :int? true}}}
    :leaves [{:path ["access" "login-audit"] :section :access :aliases ["login-audit"]
              :sum "the login-event trail (who signed in, when, success/failure)"}]}

   {:verb :branch :home :install
    :leaves [{:path ["branch" "create"] :section :branch :aliases ["branch"]
              :sum "new branch (--from)"}]}

   {:verb :checkout :home :checks
    :leaves [{:path ["branch" "checkout"] :section :branch :aliases ["checkout" "co"]
              :sum "switch branch"}]}

   {:verb :branches :home :branches
    :leaves [{:path ["branch" "list"] :section :branch :aliases ["branches"]
              :sum "every branch + its lifecycle status (--closed: with closed and merged; --machine: with staging/run/scenario)"}]}

   {:verb :protect :home :branches
    :args   {:kv :settings}
    :leaves [{:path ["branch" "protect"] :section :branch :aliases ["protect"]
              :sum "READ a branch's policy; --on requires proposals onto it (admin; `*` = the default for all; --auto-land = propose+approve+merge in one act)"}]}

   {:verb :branch-roles :home :branches
    :leaves [{:path ["branch" "roles"] :section :branch :aliases ["roles"]
              :sum "who owns a branch, and may land its proposals (--own / --revoke)"}]}

   {:verb :remove-resources :home :branches
    :args   {:pos      [{:key :name}]
             :variadic {:key :resources}}
    :leaves [{:path ["branch" "remove"] :section :branch
              :sum "take resources off a branch: <branch> <section/rid…> each return to what the target holds"}]}

   {:verb :sweep :home :branches
    :leaves [{:path ["branch" "sweep"] :section :branch :aliases ["sweep"]
              :sum "apply retention: inactive label, then close, then delete data after 7 days — and close open proposals whose source branch is gone (previews; --apply)"}]}

   {:verb :propose :home :branches
    :args   {:flags {"title" {:key :title}}}
    :leaves [{:path ["proposal" "create"] :section :branch :aliases ["propose"]
              :sum "propose merging your branch into a protected one ([--merge-mode squash|merge])"}]}

   {:verb :proposals :home :branches
    :leaves [{:path ["proposal" "list"] :section :branch :aliases ["proposals"]
              :sum "open proposals (--status all|merged|closed, --target/--source <branch>)"}]}

   {:verb :proposal :home :branches
    :leaves [{:path ["proposal" "show"] :section :branch :sum "one proposal + its approvals"}]}

   {:verb :protection :home :branches
    :leaves [{:path ["protection"] :section :branch
              :sum "what changes on main only through a proposal: [<section/name>] [--off] | --all — bare lists"}]}

   {:verb :ask-review :home :branches
    :leaves [{:path ["proposal" "ask-review"] :section :branch :aliases ["ask-review"]
              :sum "ask for review: Preparing → In Review (--reviewer a,b)"}]}

   {:verb :merge-proposal :home :branches
    :leaves [{:path ["proposal" "merge"] :section :branch :aliases ["merge-proposal"]
              :sum "land an approved proposal (branch owner or admin; --build all|modified|none)"}]}

   {:verb :trace :home :diagnose
    :args   {:flags {"on"          {:key :on?     :bool? true}
                     "off"         {:key :off?    :bool? true}
                     "follow"      {:key :follow? :bool? true}
                     "file"        {:key :file}
                     "egress-file" {:key :egress-file}
                     "no-params"   {:key :no-params? :bool? true}
                     "kill"        {:key :kill :int? true}
                     "kill-all"    {:key :kill-all? :bool? true}
                     "explain"     {:key :explain :int? true}
                     "kept"        {:key :kept? :bool? true}
                     "limit"       {:key :limit :int? true}
                     "fns"         {:key :fns}
                     "fns-off"     {:key :fns-off? :bool? true}
                     "fns-report"  {:key :fns-report? :bool? true}
                     "decisions"   {:key :decisions}
                     "decisions-off" {:key :decisions-off? :bool? true}
                     "decisions-report" {:key :decisions-report? :bool? true}
                     "decisions-save" {:key :decisions-save}
                     "shapes"      {:key :shapes? :bool? true}
                     "shapes-report" {:key :shapes-report? :bool? true}
                     "stuck"       {:key :stuck? :bool? true}
                     "slow-ms"     {:key :slow-ms :int? true}
                     "sort"        {:key :sort}}}
    :leaves [{:path ["trace"] :section :debug
              :ops ["stuck" "on" "off" "follow" "kill" "fns" "fns-off" "fns-report" "shapes"
                    "shapes-report" "decisions" "decisions-off" "decisions-report" "decisions-save"]
              :sum "--stuck: where every in-flight request is blocked right now (no arming); the live SQL tracer (--shapes: grouped by call path; --explain <seq>: the pg plan of a slow statement), and --fns the function tracer in every process, --decisions <kinds|all> records pure decisions (build plan, merge plan, build lane) for --decisions-save <file> to replay locally (admin)"}]}

   {:verb :profile :home :diagnose
    :args   {:flags {"on"       {:key :on?    :bool? true}
                     "off"      {:key :off?   :bool? true}
                     "save"     {:key :save}
                     "interval" {:key :interval :int? true}
                     "depth"    {:key :depth    :int? true}
                     "label"    {:key :label}
                     "limit"    {:key :limit    :int? true}}}
    :leaves [{:path ["profile"] :section :debug :ops ["on" "off" "save" "status"]
              :sum "the SAMPLING profiler: --on, reproduce, then bare for where the CPU went (by domain, by self frame) or --save <file> for a Gecko profile that opens in profiler.firefox.com (admin)"}]}

   {:verb :blob :home :platform
    :leaves [{:path ["blob" "get"] :section :debug :aliases ["blob"] :sum "fetch a CAS blob"}]}

   {:verb :help :status :meta
    :leaves [{:path ["help"] :section :repl :aliases ["?"] :sum "usage"}]}

   {:verb :status :status :meta
    :leaves [{:path ["status"] :section :repl :sum "session state (target, register)"}]}

   {:verb :sets :status :meta
    :leaves [{:path ["set" "list"] :section :repl :aliases ["sets"] :sum "the selection register"}]}

   {:verb :save-set :status :meta
    :leaves [{:path ["set" "save"] :section :repl :aliases ["save-set"]
              :sum "name the current selection"}]}

   {:verb :quit :status :meta
    :leaves [{:path ["quit"] :section :repl :aliases ["exit" "q"] :sum "leave"}]}

   {:verb :stream-show
    :args   {:pos [{:key :name :kw? true}]}}

   {:verb :stream-flush
    :args   {:pos [{:key :name :kw? true}]}}

   {:verb :drop-edits
    :args   {:pos [{:key :object-type :kw? true}]}}

   {:verb :drop-branch
    :args   {:flags {"yes" {:key :yes? :bool? true}}}}

   {:verb :automations :home :install :status :file-only}

   {:verb :confirm :home :read :status :file-only}

   {:verb :health-checks :home :install :status :file-only}

   {:verb :install :home :install :status :file-only}

   {:verb :principals :home :install :status :file-only}

   {:verb :schedules :home :install :status :file-only}])

(def client-verbs
  (into #{} (comp (filter #(and (= :client (:status %)) (:home %))) (map :verb)) verbs))

(def planned
  [{:path ["project" "ref"] :section :project :sum "project reference / ontology-import"}])

(def process-words
  {"bind"           {:run 'main-bind :offline? true}
   "instances"      {:run 'main-instances :offline? true}
   "clone"          {:run 'main-clone :code? true :checkout? true}
   "serve"          {:run 'main-serve :offline? true}
   "git-credential" {:run 'main-git-credential}
   "checks"         {:run 'main-checks :code? true}
   "push"           {:run 'main-retired-git :offline? true :code? true :checkout? true}
   "pull"           {:run 'main-retired-git :offline? true :code? true :checkout? true}
   "status"         {:run 'main-retired-git :offline? true :code? true :checkout? true}
   "export"         {:run 'main-export :code? true :checkout? true}
   "spec"           {:run 'main-spec :offline-sub #{"dump"}}
   "--help"         {:run 'main-help :offline? true}
   "-h"             {:run 'main-help :offline? true}
   "help"           {:run 'main-help :offline? true}})
