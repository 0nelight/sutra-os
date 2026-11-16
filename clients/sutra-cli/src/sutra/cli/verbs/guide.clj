(ns sutra.cli.verbs.guide
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [ok?]]
            [sutra.cli.out :refer [emit! grid mark paint render rpad start-here start-here-lines]]
            [sutra.i18n.core :as i18n]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.cli.hints :as hints]
            [sutra.cli.parse :as parse]
            [sutra.cli.tree :as tree]
            [sutra.cli.site :as site]
            [sutra.parser.authoring.workflow :as workflow]
            [sutra.cli.view :refer
             [row-class row-verb]]))

(def ^:private group-order  (mapv :key tree/sections))
(def ^:private group-label  (into {} (map (juxt :key :label)) tree/sections))

(defn- leaf-cmd
  [leaf]
  (str/join " " (:path leaf)))

(defn- leaf-caveat
  [leaf]
  (let [sum (str (:sum leaf))]
    (cond
      (str/starts-with? sum "DEPRECATED") "deprecated"
      (= :file-only (tree/status leaf))   "runbook-only — no one-shot line form"
      (= :planned (tree/status leaf))     "planned — not built"
      :else nil)))

(defn- guide-groups
  []
  (let [by (group-by :section tree/command-tree)]
    (for [g group-order
          :let [ls (sort-by :path (get by g))]
          :when (seq ls)]
      [g ls])))

(def ^:private first-principles
  [:cli/fp-1 :cli/fp-2 :cli/fp-3 :cli/fp-4 :cli/fp-5 :cli/fp-6 :cli/fp-7])

(def ^:private connect-help
  {:target
   (str "--base URL  >  --instance NAME (.sutra-site.edn :instances)  >  $SUTRA_HTTP_URL  >  "
        "$SUTRA_HTTP_PORT (dev)  >  `bind` (.sutra-site.edn "
        ":url here or above)  >  the checkout's git remote  >  "
        (or (some-> (site/prod-url) (str "  (default, from .sutra-site.edn)"))
            "the local sutra-solo, else http://localhost:8030  (no .sutra-site.edn)"))
   :prod    "sutra overview                     # prod is the DEFAULT — no prefix needed"
   :dev     ". bin/ports.sh && sutra overview       # local dev (exports $SUTRA_HTTP_PORT)"
   :auth    "auto-logs in as the default admin on the first server call, using stored creds (cached under .run/ by `login`) or SUTRA_ADMIN_PASSWORD; --creds PATH overrides"
   :instances "sutra instances · instances add <n> <url> · instances login <n>   # one creds file per host"
   :batch   "printf 'overview\\nprojects\\napps\\n' | sutra   # MANY verbs, ONE session — one login, one branch/staged state"
   :branch  "sutra --branch <name> <verb …>     # run this session on that branch (leading flag; refuses an unknown name, so nothing lands on main by accident). Also applies to a piped batch."})

(defn- connect-lines
  [connect]
  [(str "  " (paint :bold (i18n/t :cli/guide-connect)))
   (str "    " (paint :dim "target ") (:target connect))
   (str "    " (paint :dim "prod   ") (paint :cyan (:prod connect)))
   (str "    " (paint :dim "dev    ") (paint :cyan (:dev connect)))
   (str "    " (paint :dim "auth   ") (:auth connect))
   (str "    " (paint :dim "many   ") (paint :cyan (:instances connect)))
   (str "    " (paint :dim "batch  ") (paint :cyan (:batch connect)))
   (str "    " (paint :dim "branch ") (paint :cyan (:branch connect)))
   ""])

(def ^:private checkout-subcommands
  [{:cmd "change"
    :usage "change start \"<titel>\"  |  change push [-m \"…\"] [--alles] [--land]  |  change status  |  change sync  |  change check  |  change ask-review  |  change review --approve  |  change land"
    :purpose "SUPERSEDED — not the way to change anything any more. A `.ent` text (ontology or code repository, also .enta/.entp/.entt) goes through `projection pull` → edit → `projection push` → `commit` → `propose` (see `projection`); Python goes through `git push` while the branch's proposal is open. What follows describes the old client so an old checkout can still be read. The AUTHOR's client, and a separate binary (`bin/change`) — `bin/sutra` hands `change …` straight to it, because it reads THE CHECKOUT YOU ARE STANDING IN rather than the instance. `change start` creates the git branch AND the changeset in ONE act, which is the precondition for pushing at all: a push onto a branch with no open changeset is refused in `pre-receive`, so `git checkout -b` does not get you there. `change push` is the SAVE — the same gates run locally first, so a red verdict sends nothing; `--land` then walks the rest of the ladder (ask-review → approve → land) and stops at the first refusal, naming the rung that resumes. NONE of these verbs takes a proposal id: they read the changeset out of the checkout."
    :when "Never for a new change — `projection` and `propose` replace it. At most `change status` to read where an old checkout stands."
    :examples ["change status"]
    :see-also [:projection :propose :merge-proposal]}
   {:cmd "clone"
    :usage "clone ontology [dir]   |   clone <project>/<repo> [dir]   |   clone [project] [dir] [--only a,b] [--archive [--zip]]"
    :purpose "Materialise server state to disk. `clone ontology` names the ONTOLOGY REPOSITORY — the instance's TYPE layer (object types, links, interfaces, value types, shared properties, actions, agents) as an editable `ontology.ent`, its sources and datasets beside it in `transform.ent`, a real `git clone` served at the instance root, so plain `git clone http://<host>` gets the same thing. Every SOURCE REPOSITORY is then cloned BESIDE it (`<repo>/` per repository, credential-wired for you) — INDEPENDENT clones, not submodules: the ontology's history holds no pointer to a code repository's version, so each is committed and pushed on its own. The directory layout is a checkout convenience; `sutra pull` in the ontology pulls each of them too. `clone <project>/<repo>` names a SOURCE REPOSITORY and is a REAL `git clone` over /git/ — a working tree with git history, git branches and git's own three-way merge, authenticated by a 7-day repository-scoped token. Everything else is the legacy checkout: a working folder bound to the server's HEAD commit plus a `.sutra-checkout.edn` manifest (transforms/*.py + apps/*.cljs + export templates + uploaded files + a minimal .ent). With --archive, a re-installable NON-editable snapshot instead (a project → one .ent/.zip; NO project → the whole-instance backup as ONE .zip, --out mandatory)."
    :when "`clone ontology` is a READ copy of the type layer — never edit an `.ent` here and `git push` it: every `.ent` text, in the ontology and in a code repository, changes through `projection pull`/`projection push` on a branch, then commit → propose → approve → merge-proposal. git carries only Python: `clone <project>/<repo>` is a real `git clone` in which you commit and push `transforms/*.py`; an `.ent`/`.enta` in that repository still goes through the CLI. It falls back to the legacy protocol automatically when the git surface is off (SUTRA_GIT_REPOS), when `git` is not installed, or when you clone a whole PROJECT. `clone --archive --out b.zip` (no project) is the whole-instance backup — it must be named, because it is the one artifact that outlives the session. Sources and actions carry no credentials (`:secrets`/`:secret-cipher` is REFUSED) — arm them in-platform with `source secret <name>`, which is also the only thing --archive cannot reproduce."
    :examples ["clone ontology" "clone ontology ./types" "clone werft/etl" "clone werft/etl ./etl"
               "clone procurement" "clone werft ./wf"
               "clone --only parse_invoices,classify_events ./scoped"
               "clone --archive --out backup-prod.zip"]
    :see-also [:checks :status :push :pull]}
   {:cmd "slice"
    :usage "slice pull <node> … --to <dir> [--force]   |   slice push <dir> [--preview] [--mine|--theirs] [-m <message>]"
    :purpose "A checkout by node: the nodes you work on, wherever their texts lie, with the code they name (texts under <dir>/<projection>/, code under <dir>/repos/<repo>/), and ONE push for all of it. It merges the texts per element and commits them, opens a proposal if the branch has none and code came along, and commits the code through the checks of a git push. Texts are cut to the slice at the pull and spliced back at the push: a node you drop is deleted, a node you add stays in the text you add it to, placement never changes. It reads the slice's lineage neighbours out at the pull, to widen it by; at the push it says which inputs moved since the pull and what a changed contract feeds. A file in repos/ that moved since the pull and is changed here is a conflict (--mine / --theirs). A direct git push still works beside it."
    :when "For a change across several texts, or across `.ent` and Python. One text alone stays `projection`. Pass ABSOLUTE paths and name a branch: `bin/sutra --branch <b> slice pull …`. Texts land before code, and if the code fails its checks the texts stay on the branch and the same push can be sent again."
    :examples ["sutra --branch kp slice pull rechnung action/rechnung-berichtigen transform/shape-rechnung --to /abs/dir"
               "sutra --branch kp slice push /abs/dir --preview"
               "sutra --branch kp slice push /abs/dir -m \"Skonto je Rechnung\""]
    :see-also [:projection :propose :lineage]}
   {:cmd "projection"
    :usage "projection list   |   projection find <text> [--projection <p>]   |   projection where <name> … [--projection <p>]   |   projection pull <projection> [<text> …] [--project <p>] [--to <dir>] [--force] [--rehome]   |   projection push <text> … [--preview] [--mine|--theirs] [--land|--stage] [--adopt]"
    :purpose "The schema as texts, WITHOUT git. A projection is declared in the schema itself (`:projections`): which kinds of text it consists of, which nodes it shows, what a write may change. A CHECKOUT is the texts one pull names — one text (`pull ontologie controlling.objects.ent`), a project's texts (`--project controlling`), or every text — never a folder. `pull` writes them into `--to` (default: here; pass an ABSOLUTE path) with one sidecar named after what was pulled (`.controlling.objects.ent.sutra`, `.controlling.sutra`, `.ontologie.sutra`) that holds the BASE: a fingerprint of every element (node × shown field) of every node those texts hold. `push <text>` sends exactly that text's checkout. A text no pull wrote, lying in the checkout's folder where the projection's kinds would read it (a RENAMED or NEW text), is not sent unless named or `--adopt`ed — and while one is left out, a push that would DELETE nodes is refused as a likely rename (the old text gone, the new one never sent). `--adopt` carries them: what they hold moves there, what is new is created. A stray text that already holds nodes on the server is never adopted — pull it. The server merges PER ELEMENT onto what the projection shows now and STAGES the change: what only you changed is applied, what others changed since your pull is kept, both changed differently is a CONFLICT named element by element (`--mine` / `--theirs`). Nodes in texts the checkout does not carry are never touched; a node pasted in from such a text is refused (pull both texts to move it). Within a checkout, cut and paste MOVES a node (placement only, no schema change); `pull --rehome` lays every node out at its declared home. A text deleted from a checkout deletes the nodes it held — to RENAME a text, rename the file and `push --adopt` (or pull old and new name together)."
    :when (str "To edit the ontology — or the one text or project you are responsible for — as text. `projection list` names what this instance declares; `ontologie` is the whole type layer as authored. `push --preview` reports what a push would change and stages nothing. Preview and push also say what the nodes whose contract changed feed downstream, and which direct inputs outside the checkout changed their contract since its pull (a warning, never a refusal; the code of a transform is not part of a contract). A staged change is taken back with `discard`, not by editing it back: until commit the merge reads the committed branch. A change across projections (a function in a code repository's text, the action calling it in `ontology.ent`): push the DEFINING text first — references resolve against the branch as staged. With Python in the same repository the order is (a git push lands only while the branch's proposal is open; the committed `.ent` is minted onto the git branch, hence the rebase): "
               (workflow/sentence :python-and-ent {:branch "<b>" :trunk "main"}))
    :examples ["projection list"
               "projection find \"Selbst aufteilen\""
               "projection where grok_email_attached rechnung-erfassen"
               "sutra --branch kp projection pull ontologie controlling.objects.ent --to /abs/dir"
               "sutra --branch kp projection pull ontologie --project controlling --to /abs/dir"
               "projection push /abs/dir/controlling.objects.ent --preview"
               "projection push /abs/dir/controlling.objects.ent --theirs"
               "sutra --branch kp projection push /abs/dir/controlling.objects.ent --land"
               "sutra --branch split projection pull ontologie --rehome --to /abs/onto"]
    :see-also [:commit :propose :approve]}
   {:cmd "checks" :usage "checks [rev] [dir]"
    :purpose "The ontology's CHECK VERDICT for a commit you already pushed: scope · retire · lint · plan · test · ci/sutra-publish, each with its reason. `rev` defaults to HEAD and is resolved by git in the checkout, so a branch, a tag or a short sha all work. A commit whose checks failed is IN git and NOT in the ontology — that is the whole contract, and this is how you read it after the fact."
    :when "Whenever the push's own output is gone: a timeout killed the client while the remote hooks were still running, the terminal was closed, the scrollback rolled. Re-pushing does NOT bring it back — the refs are already where they belong, so git says `Everything up-to-date` and no hook runs. The verdicts are recorded rows; this reads them with the login you already have."
    :examples ["checks" "checks 46effb8" "checks origin/main" "checks HEAD~1 ./ontology"]
    :see-also [:clone]}
   {:cmd "status" :status :retired :usage "status — RETIRED → `git status`"
    :purpose "RETIRED. A clone is a real git checkout with a credential helper wired in, so `git status` is the whole of it — and it answers better (renames, untracked files, the ahead/behind count). The verb refuses with that pointer rather than wrapping git."
    :when "Don't — run `git status`."
    :examples ["git status"]
    :see-also [:clone :checks]}
   {:cmd "push" :status :retired :usage "push — RETIRED → `git push`"
    :purpose "RETIRED. `git push` IS the push: the commit lands in git either way, the server runs the checks and `ci/sutra-publish` is what makes the change take effect, and the verdict comes back over the push side-band as `remote: sutra: …` lines. So a push reporting `checks failed` means your change is in the repository and NOT in the ontology."
    :when "Don't — run `git push`. If its output is lost (a timeout, a closed terminal), read the same verdict back with `checks`; a second push only says `Everything up-to-date` and runs no hook."
    :examples ["git push" "checks"]
    :see-also [:checks :clone]}
   {:cmd "pull" :status :retired :usage "pull — RETIRED → `git pull --ff-only`"
    :purpose "RETIRED. `git pull --ff-only` is the whole of it — fast-forward ONLY, because the server's projection is the authority and a merge commit minted locally is a commit no push produced. The code repositories beside the ontology are independent clones — `git pull --ff-only` in each, or `sutra pull` in the ontology, which does that for you."
    :when "Don't — run `git pull --ff-only`. Pull BEFORE editing `ontology.ent`: that tree is authoritative for the whole type layer."
    :examples ["git pull --ff-only"]
    :see-also [:clone :checks]}
   {:cmd "export" :status :deprecated
    :usage
    "export <projekt> (DEPRECATED → clone --archive) · export list|show (the EXPORT resources)"
    :purpose "Two different things share this word. `export <projekt>` is the DEPRECATED alias for `clone --archive` — folded in so there's one 'materialise' concept, not four overlapping verbs; still works (prints a notice, redirects), removed in a later release. `export list` and `export show` read something else entirely: the declared EXPORTS, each one a resource at a source that writes a dataset's rows out to it in one of the six modes."
    :when "To download a project or a backup, don't — use `clone --archive --out <file>.zip` (whole instance) or `clone <project> --archive` (single shareable .ent). To see what leaves this instance for a foreign system, `export list`."
    :examples ["clone --archive --out backup-prod.zip" "clone werft ./out.ent --archive"
               "export list" "export show <name>"]
    :see-also [:clone :source]}])

(defn- subcommand-hint
  [topic]
  (let [t (clojure.core/name topic)]
    (some (fn [m] (clojure.core/when (= t (:cmd m)) m)) checkout-subcommands)))

(defn- commands-of
  [ls]
  (mapv (fn [l] {:cmd (leaf-cmd l) :sum (:sum l) :status (clojure.core/name (tree/status l))})
        ls))

(defn- render-guide-group!
  [ctx group]
  (let [g  (keyword group)
        ls (sort-by :path (get (group-by :section tree/command-tree) g))]
    (if (seq ls)
      (emit! ctx {:section  (clojure.core/name g)
                  :label    (group-label g)
                  :commands (mapv (fn [l c]
                                    (assoc c :purpose
                                           (or (some-> (hints/verb-hint (:verb l)) :purpose)
                                               (:sum l))))
                                  ls (commands-of ls))}
             ::group)
      (emit! ctx {:error "unknown section" :section (clojure.core/name g)
                  :sections (mapv clojure.core/name group-order)}
             ::unknown-group))))

(defmethod render ::group [_ {:keys [label commands]}]
  [(str "  " (paint :bold label))
   (for [{:keys [cmd status purpose]} commands
         :let [planned? (= "planned" status)]]
     (str "    " (paint (if planned? :dim :cyan) (rpad cmd 30)) " "
          (paint :dim (str purpose (when planned? "  [planned]")))))
   (str "  " (paint :dim (i18n/t :cli/guide-explain-pointer)))])

(defmethod render ::unknown-group [_ {:keys [section sections]}]
  (mark :yellow "⏸" (str (i18n/t :cli/no-such-group {:name (pr-str section)})
                         " — try: " (str/join " " sections))))

(defmethod step! :guide [ctx [_ {:keys [group]}]]
  (if group
    (render-guide-group! ctx group)
    (emit! ctx {:start-here  (mapv (fn [[v d]] {:verb v :purpose (i18n/t d)}) start-here)
                :first       (mapv i18n/t first-principles)
                :connect     connect-help
                :groups      (into {} (for [[g ls] (guide-groups)]
                                        [g (cond-> {:count    (count ls)
                                                    :commands (mapv leaf-cmd ls)}
                                             (seq (keep leaf-caveat ls))
                                             (assoc :not-typeable
                                                    (into {} (keep (fn [l]
                                                                     (when-let [c (leaf-caveat l)]
                                                                       [(leaf-cmd l) c])))
                                                          ls)))]))
                :concepts    (mapv (comp clojure.core/name key) (hints/concepts))
                :subcommands (mapv :cmd (remove :status checkout-subcommands))
                :subcommands-retired (into {} (for [c checkout-subcommands :when (:status c)]
                                                [(:cmd c) (clojure.core/name (:status c))]))
                :more        (i18n/t :cli/guide-more)}
           ::guide))
  nil)

(defmethod render ::guide [_ {:keys [first connect groups concepts subcommands]
                              retired :subcommands-retired}]
  [(start-here-lines false)
   (str "  " (paint :bold (i18n/t :cli/guide-first)))
   (for [[i p] (map-indexed vector first)]
     (str "    " (paint :cyan (str (inc i) ".")) " " (paint :dim p)))
   ""
   (connect-lines connect)
   (str "  " (paint :bold (i18n/t :cli/guide-groups)) "  "
        (paint :dim (str "(" (i18n/t :cli/guide-groups-sub) ")")))
   (for [g group-order :let [{n :count} (get groups g)] :when n]
     (str "    " (paint :cyan (rpad (clojure.core/name g) 11)) " "
          (paint :dim (str (i18n/t :cli/n-cmds {:n n}) " · " (group-label g)))))
   ""
   (str "  " (paint :bold (i18n/t :cli/guide-project))
        "  " (paint :dim (i18n/t :cli/guide-project-sub)))
   (str "    " (paint :dim (str/join "   " subcommands)))
   (when (seq retired)
     (str "    "
          (paint :dim (str (i18n/t :cli/retired) ": "
                           (str/join "   " (map (fn [[c s]] (str c " → " s)) retired))))))
   (when (seq concepts)
     [(str "  " (paint :bold (i18n/t :cli/guide-concepts)) "  "
           (paint :dim "(explain <concept>)"))
      (grid concepts 6 :dim)])
   (str "  " (paint :dim (i18n/t :cli/guide-footer)))])

(defmethod render ::hint [_ {:keys [kind topic theme purpose patterns anti-patterns examples
                                    see-also usage paths aliases]
                             wann :when}]
  (let [verb? (= "verb" (clojure.core/name kind))
        alts  (remove (set paths) aliases)]
    [(str "  " (paint :bold topic)
          "  " (paint :dim (str (clojure.core/name kind)
                                (when theme (str " · " (clojure.core/name theme))))))
     (str "    " purpose)
     (when (and verb? usage) (str "    " (paint :dim "usage: ") usage))
     (when (and verb? (seq paths))
       (str "    " (paint :dim (str (i18n/t :cli/hint-path) ": ")) (str/join " · " paths)
            (when (seq alts)
              (paint :dim (str "   " (i18n/t :cli/hint-aliases {:namen (str/join ", " alts)}))))))
     (when wann (str "    " (paint :dim (str (i18n/t :cli/hint-when) ": ")) wann))
     (when (seq patterns)
       [(str "    " (paint :green (i18n/t :cli/hint-patterns)))
        (for [p patterns] (str "      • " p))])
     (when (seq anti-patterns)
       [(str "    " (paint :red (i18n/t :cli/hint-anti-patterns)))
        (for [{:keys [rule why doc]} anti-patterns]
          [(str "      ✗ " rule)
           (when why (str "        " (paint :dim why)))
           (when doc (str "        " (paint :dim (str "docs: " doc))))])])
     (when (seq examples)
       [(str "    " (paint :bold (i18n/t :cli/hint-examples)))
        (for [e examples] (str "      " (paint :cyan e)))])
     (when (seq see-also)
       (str "    " (paint :dim (str "see also: "
                                    (str/join " " (map clojure.core/name see-also))))))]))

(defmethod render ::subcommand [_ {:keys [cmd usage purpose examples see-also] wann :when}]
  [(str "  " (paint :bold cmd) "  " (paint :dim "subcommand · project (server ⇄ disk)"))
   (str "    " purpose)
   (str "    " (paint :dim "usage: ") usage)
   (when wann (str "    " (paint :dim (str (i18n/t :cli/hint-when) ": ")) wann))
   (when (seq examples)
     [(str "    " (paint :bold (i18n/t :cli/hint-examples)))
      (for [e examples] (str "      " (paint :cyan e)))])
   (when (seq see-also)
     (str "    " (paint :dim (str "see also: "
                                  (str/join " " (map clojure.core/name see-also))))))])

(defn- render-namespace!
  [ctx topic ls]
  (emit! ctx {:namespace (clojure.core/name topic) :commands (commands-of ls)} ::namespace))

(defmethod render ::namespace [_ {:keys [namespace commands]}]
  [(str "  " (paint :bold namespace) "  " (paint :dim "namespace"))
   (for [{:keys [cmd sum status]} commands]
     (str "    " (paint (if (= "planned" status) :dim :cyan) (rpad cmd 30)) " " (paint :dim sum)))
   (str "  " (paint :dim "→ explain <verb> for one command in full"))])

(defn- namespace-leaves
  [topic]
  (seq (sort-by :path (filter #(= (clojure.core/name topic) (first (:path %)))
                              tree/command-tree))))

(defn- hint-topics
  []
  {:verbs       (sort (for [[_ pairs] (hints/verbs-by-theme) [v _] pairs]
                        (clojure.core/name v)))
   :concepts    (sort (map (comp clojure.core/name key) (hints/concepts)))
   :subcommands (mapv :cmd checkout-subcommands)})

(defn- tree-topic
  [topic]
  (let [toks (str/split (str/trim (clojure.core/name topic)) #"\s+")]
    (clojure.core/when (> (clojure.core/count toks) 1)
      (some-> (tree/match (vec toks)) :leaf :verb))))

(defmethod render ::topics [_ {:keys [verbs concepts subcommands]}]
  [(str "  " (paint :dim (str (i18n/t :cli/explain-pick-a-topic) ":")))
   (grid verbs 6 :cyan)
   (when (seq concepts) (grid concepts 6 :dim))
   (when (seq subcommands) (grid subcommands 6 :cyan))])

(defmethod render ::no-hint [_ {:keys [topic]}]
  (mark :yellow "⏸" (i18n/t :cli/no-hint-for {:name (pr-str topic)})))

(defmethod render ::widgets [_ {:keys [lines]}] lines)

(defmethod step! :explain [ctx [_ {:keys [topic item]}]]
  (let [{:keys [verbs concepts subcommands]} (hint-topics)]
    (cond
      (nil? topic)
      (emit! ctx {:verbs (vec verbs) :concepts (vec concepts) :subcommands (vec subcommands)}
             ::topics)

      (= "widget" (clojure.core/name topic))
      (let [answer (requiring-resolve 'sutra.cli.widgets/answer)]
        (emit! ctx (answer item (i18n/t :cli/widget-refs)) ::widgets))

      :else
      (let [canon (tree-topic topic)
            topic (or canon topic)]
        (if-let [[kind h] (hints/topic-hint topic)]
          (emit! ctx (cond-> (assoc h :topic (clojure.core/name topic) :kind kind)
                       (= kind :verb) (assoc :usage (parse/usage topic))
                       (= kind :verb) (merge (tree/names-for-verb (keyword topic))))
                 ::hint)
          (if-let [sc (subcommand-hint topic)]
            (emit! ctx (assoc sc :topic (clojure.core/name topic) :kind "subcommand") ::subcommand)
            (if-let [ls (namespace-leaves topic)]
              (render-namespace! ctx topic ls)
              (emit! ctx {:error "no hint" :topic (clojure.core/name topic)} ::no-hint)))))))
  nil)

(defn- staged-brief
  [staged]
  (->> staged
       (group-by (juxt row-verb row-class))
       (map (fn [[[v c] rows]] {:verb (str v) :class (str c) :n (clojure.core/count rows)}))
       (sort-by (juxt :class :verb))
       vec))

(defmethod step! :context [ctx [_ _]]
  (let [try*   (fn [f] (try (f) (catch Exception _ nil)))
        snap   (try* (fn []
                       (let [r (api/get! :snapshot)]    (clojure.core/when (ok? r) (:body r)))))
        who    (try* (fn [] (let [r (api/get! :me)]      (clojure.core/when (ok? r) (:body r)))))
        sr     (try* (fn [] (let [r (api/get! :staging)] (clojure.core/when (ok? r) (:body r)))))
        staged (:staged sr)
        txns   (:transactions sr)
        mine   (first (filter :mine? txns))
        branch (or (:branch snap) (some-> ctx :session deref :branch))
        brief  (staged-brief staged)
        summ   {:branch    branch
                :identity  (clojure.core/when who (select-keys who [:username :role]))
                :staged    (merge {:count (clojure.core/count staged) :by brief}
                                  (select-keys (or mine {})
                                               [:owner :mine? :origin :origin-ref :opened-at]))
                :others    (vec (remove :mine? txns))
                :counts    (:counts snap)
                :integrity (:integrity snap)}]
    (emit! ctx summ))
  nil)
