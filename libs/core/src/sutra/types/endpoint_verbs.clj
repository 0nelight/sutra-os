(ns sutra.types.endpoint-verbs
  (:require [sutra.types.paths :as paths]))

(set! *warn-on-reflection* true)

(def by-endpoint
  {:container-images
   {:get {:verb :images :path ["image" "list"] :section :pipeline
          :sum "the pushed images" :aliases ["images"]}}
   :branch
   {:delete {:verb :drop-branch :path ["branch" "delete"] :section :branch
             :sum (str "close a branch AND delete its data now (name,"
                       " status and proposals stay)")
             :confirm (str "CLOSE branch {name} and DELETE its data now"
                           " (every change that exists only on it)?"
                           "  Name, status and proposals stay; there is"
                           " no reopening.")}}
   :branch-status
   {:post [{:verb :close :path ["branch" "close"] :section :branch
            :sum (str "close a branch for good (terminal; its data is deleted"
                      " after 7 days)")
            :aliases ["close"] :send {:closed true}}
           {:verb :reactivate :path ["branch" "reactivate"] :section :branch
            :sum (str "take the Inactive label off a branch (owner; a closed"
                      " branch stays closed)")
            :aliases ["reactivate"] :send {:inactive false}}]}
   :retention-policies
   {:get {:verb :retention-policies :path ["retention" "policies"]
          :section :data
          :sum "the recommended and custom dataset retention policies"}}
   :retention-policy
   {:delete {:verb :retention-policy-delete
             :path ["retention" "policy" "delete"]
             :section :data :sum "remove a custom retention policy"}}
   :custom-app
   {:get {:verb :custom-app-show :path ["custom-app" "show"]
          :section :project
          :sum (str "one custom application as the server holds it — scope,"
                    " minted clients, hosting, metrics: … show <name>")}}
   :custom-app-metrics
   {:get {:verb :custom-app-metrics :path ["custom-app" "metrics"]
          :section :project
          :sum (str "calls, errors and latency per API namespace and"
                    " day: … metrics <name>")}}
   :custom-app-client
   {:delete {:verb :custom-app-client-revoke
             :path ["custom-app" "client" "revoke"]
             :section :project
             :sum (str "revoke a client credential: … revoke"
                       " <name> <client-id>")}
    :post {:verb :custom-app-client-rotate
           :path ["custom-app" "client" "rotate"]
           :section :project
           :sum (str "new secret for an existing client: …"
                     " rotate <name> <client-id>")
           :send {}}}
   :egress-policy-decide
   {:post [{:verb :egress-policy-approve
            :path ["source" "egress" "policy" "approve"]
            :section :source
            :sum "approve a policy (admin): … approve <host:port>"
            :pos [:id] :send {:decision :approve}}
           {:verb :egress-policy-revoke
            :path ["source" "egress" "policy" "revoke"]
            :section :source
            :sum "revoke a policy (admin): … revoke <host:port>"
            :pos [:id] :send {:decision :revoke}}]}
   :document-of
   {:delete {:verb :document-delete :path ["document" "delete"]
             :section :data :sum "delete a document"
             :aliases ["document-delete"]}}
   :widget-set
   {:get {:verb :widget-set-show :path ["widget-set" "show"]
          :section :project
          :sum "one widget set: its widgets, parameters, events and versions"}}
   :linter-rules
   {:get {:verb :linter-rules :path ["linter" "rules"]
          :section :operate
          :sum "the rules: why each exists and what it recommends"}}
   :linter-sweep
   {:post {:verb :linter-sweep :path ["linter" "sweep"]
           :section :operate
           :sum "sweep now (a sweep also runs by itself, daily)"}}
   :health-check-snooze
   {:delete {:verb :health-check-unsnooze
             :path ["health-check" "unsnooze"]
             :section :operate :sum "end a check's snooze"
             :resolve {:id {:from :health-checks
                            :rows :checks :by :name}}}}
   :health-check-group-of
   {:delete {:verb :health-check-group-delete
             :path ["health-check-group" "delete"]
             :section :operate
             :sum (str "remove a check group and its"
                       " subscriptions")
             :resolve {:id {:from :health-check-groups
                            :rows :groups :by :name}}}}
   :health-check-group-subscribe
   {:delete {:verb :health-check-group-unsubscribe
             :path ["health-check-group" "unsubscribe"]
             :section :operate
             :sum "stop the group's digest"
             :resolve {:id {:from :health-check-groups
                            :rows :groups :by :name}}}}
   :password
   {:post {:verb :password :path ["access" "password"] :section :access
           :sum (str "change your own password: --current <pw> --new <pw>"
                     " (ends every other session)")
           :body {:current :string :new :string}}}
   :organizations
   {:get {:verb :org-list :path ["org" "list"] :section :access
          :sum "the organizations you may discover: members, guests, spaces"
          :aliases ["orgs"]}}
   :organization
   {:get {:verb :org-show :path ["org" "show"] :section :access
          :sum (str "one organization: spaces, collaborators, discovery, who may apply it;"
                    " its members and guests where its users are discoverable")}}
   :spaces
   {:get {:verb :space-list :path ["space" "list"] :section :access
          :sum "the spaces you may see: organizations, ontology kind (private/shared), projects"
          :aliases ["spaces"]}}
   :space
   {:get {:verb :space-show :path ["space" "show"] :section :access
          :sum "one space (or `default`): organizations, ontology kind and its projects"}}
   :ontologies
   {:get {:verb :space-ontologies :path ["space" "ontologies"] :section :access
          :sum "one ontology per space: kind, organizations and resources per section"}}
   :ontology
   {:get {:verb :space-ontology :path ["space" "ontology"] :section :access
          :sum "the ontology of one space (or `default`): its resources by section"}}
   :federation-key
   {:get {:verb :trust-key :path ["trust" "key"] :section :access
          :sum "this instance's public key and fingerprint (another instance's trust names it)"}}
   :federation-trusts
   {:get  {:verb :trust-list :path ["trust" "list"] :section :access
           :sum "the instances this one trusts, each way" :aliases ["trusts"]}
    :post {:verb :trust-set :path ["trust" "set"] :section :access
           :sum (str "trust another instance: <name> <url> <public-key> [--inbound]"
                     " [--outbound] [--subjects remote=local,…]")
           :pos [:name :url :public-key]
           :as {:inbound :inbound? :outbound :outbound?}
           :body {:inbound :boolean :outbound :boolean :subjects :string}}}
   :federation-trust
   {:delete {:verb :trust-revoke :path ["trust" "revoke"] :section :access
             :sum "revoke a trust: its assertions are refused from now on"
             :confirm "REVOKE the trust {name}? Logins from it are refused at once."}}
   :resource-versions
   {:get {:verb :version-list :path ["version" "list"] :section :versions
          :sum (str "a resource's published versions and which one is served:"
                    " … list <section> <name> (section e.g. applications)")}}
   :resource-versions-publish
   {:post {:verb :version-publish :path ["version" "publish"] :section :versions
           :sum (str "publish a resource's saved head — what an app serves is its published"
                     " version, not the last push: … publish applications <name>"
                     " [--note …] [--version <n> to point back] [--preview]")
           :body {:note :string :version :string :preview :boolean}}}
   :object-type-drop-edits
   {:post {:verb :drop-edits :path ["object" "drop-edits"]
           :section :data :as {:object-type :type}
           :sum "discard edits, reconcile to source"}}
   :packages
   {:get {:verb :packages :path ["package" "list"] :section :project
          :sum (str "the ready-made packages: version offered, version"
                    " installed, whether an upgrade waits")
          :aliases ["packages"]}}
   :products
   {:get {:verb :products :path ["product" "list"] :section :project
          :sum (str "the products this instance publishes or took in: versions, channel,"
                    " state, installations")
          :aliases ["products"]}}
   :product
   {:get {:verb :product-describe :path ["product" "describe"] :section :project
          :sum (str "one product: its releases with inputs, projects and what they require,"
                    " and its installations: … describe <product>")}}
   :product-release
   {:post [{:verb :product-release :path ["product" "release"] :section :project
            :sum (str "release a drafted version onto a channel: … release <product>"
                      " <version> [--channel dev|test|stable]")
            :body {:channel :string} :send {:event :release}}
           {:verb :product-promote :path ["product" "promote"] :section :project
            :sum "move a released version one channel up (dev → test → stable)"
            :send {:event :promote}}
           {:verb :product-deprecate :path ["product" "deprecate"] :section :project
            :sum "deprecate a released version (installs still work, with a warning)"
            :send {:event :deprecate}}
           {:verb :product-withdraw :path ["product" "withdraw"] :section :project
            :sum "withdraw a version for good: it no longer installs or exports"
            :send {:event :withdraw}
            :confirm "WITHDRAW {product} {version} for good (no undo)?"}]}
   :product-installations
   {:get {:verb :product-installations :path ["product" "installations"] :section :project
          :sum (str "the product installations: branch, version, lock, channel, automatic"
                    " upgrade, the upgrade that waits")}}
   :product-installation
   {:post [{:verb :product-lock :path ["product" "lock"] :section :project
            :sum (str "lock an installation: its projects change only through upgrades:"
                      " … lock <installation-id>")
            :send {:locked true}}
           {:verb :product-unlock :path ["product" "unlock"] :section :project
            :sum (str "unlock an installation to edit (fork) what it installed; an upgrade"
                      " merges over your edits")
            :send {:locked false}}]}
   :product-trust
   {:get {:verb :product-issuer :path ["product" "issuer"] :section :project
          :sum (str "the name and Ed25519 key this instance signs its bundles with, and the"
                    " instances whose bundles it takes in (declared with trust set)")}}
   :peering-connections
   {:get  {:verb :peer-list :path ["peer" "list"] :section :source
           :sum "the peer connections: other instance, space, status, data types, health"}
    :post {:verb :peer-connect :path ["peer" "connect"] :section :source
           :sum (str "ask another instance to peer: <name> <trust> [--space S]"
                     " [--source-data export|import] [--action-data both|export|import]"
                     " [--media export|import] [--datasets export|import|both]"
                     " [--classification name:category:rank] [--markings a,b]"
                     " [--ingress cidr,…] — prints the invite code for its administrator")
           :pos [:name :trust]
           :body {:space :string :source-data :string :action-data :string :media :string
                  :datasets :string :classification :string :markings :string
                  :ingress :string}}}
   :peering-accept
   {:post {:verb :peer-accept :path ["peer" "accept"] :section :source
           :sum (str "take in another instance's invite code for a space here and approve it:"
                     " <invite> [--space S] [--classification …] [--markings …]")
           :pos [:invite]
           :body {:space :string :classification :string :markings :string :ingress :string}}}
   :peering-connection
   {:get    {:verb :peer-describe :path ["peer" "describe"] :section :source
             :sum "one peer connection: ceiling, pending change, mappings, queue, health"}
    :delete {:verb :peer-delete :path ["peer" "delete"] :section :source
             :sum "delete a peer connection here (the other side is told it is disabled)"
             :confirm "DELETE the peer connection {connection}? Its mappings and queue go."}}
   :peering-activate
   {:post {:verb :peer-activate :path ["peer" "activate"] :section :source :send {}
           :sum "activate a connection the other side approved (the asking side does)"}}
   :peering-status
   {:post [{:verb :peer-disable :path ["peer" "disable"] :section :source
            :send {:disabled true}
            :sum "disable a peer connection: no data peers until this side enables it"}
           {:verb :peer-enable :path ["peer" "enable"] :section :source
            :send {:disabled false}
            :sum "enable a peer connection this side disabled"}]}
   :peering-ceiling
   {:post [{:verb :peer-ceiling :path ["peer" "ceiling"] :section :source
            :sum (str "propose a new security ceiling: [--classification name:category:rank]"
                      " [--markings a,b] — in force once the other side approves")
            :body {:classification :string :markings :string}}
           {:verb :peer-ceiling-approve :path ["peer" "ceiling-approve"] :section :source
            :send {:approve true}
            :sum "approve the ceiling the other side proposed"}]}
   :peering-ingress
   {:post {:verb :peer-ingress :path ["peer" "ingress"] :section :source
           :sum "the addresses (CIDRs, a,b) the connection's data may come from; empty: any"
           :pos [:cidrs]}}
   :peering-markings
   {:post {:verb :peer-marking :path ["peer" "marking"] :section :source
           :sum (str "name the local marking a peer's marking stands for: <remote>"
                     " [--local name]; without --local the mapping goes")
           :pos [:remote] :body {:local :string}}}
   :peering-mappings
   {:get  {:verb :peer-mappings :path ["peer" "mappings"] :section :source
           :sum "the object types a connection peers, with their directions"}
    :post {:verb :peer-map :path ["peer" "map"] :section :source
           :sum (str "peer an object type: <connection> <local-type> <remote-type>"
                     " --properties a=b,c=d [--links l=r] [--source-data export|import]"
                     " [--action-data both|…]")
           :pos [:local-type :remote-type]
           :body {:properties :string :links :string :source-data :string
                  :action-data :string}}}
   :peering-mapping
   {:delete {:verb :peer-unmap :path ["peer" "unmap"] :section :source
             :sum "stop peering an object type over a connection"}}
   :peering-media
   {:get  {:verb :peer-media :path ["peer" "media" "list"] :section :source
           :sum "the media sets a connection peers, with their direction"}
    :post {:verb :peer-media-map :path ["peer" "media" "map"] :section :source
           :sum (str "peer a media set's items (artifacts) one way: <local-set>"
                     " [--remote-set S] --direction export|import — the connection's"
                     " --media must allow it")
           :pos [:local-set] :body {:remote-set :string :direction :string}}}
   :peering-media-set
   {:delete {:verb :peer-media-unmap :path ["peer" "media" "unmap"] :section :source
             :sum "stop peering a media set over a connection"}}
   :peering-shares
   {:get  {:verb :peer-shares :path ["peer" "share" "list"] :section :source
           :sum "the datasets and streams a connection shares for the other side to sync"}
    :post {:verb :peer-share :path ["peer" "share" "add"] :section :source
           :sum (str "share a dataset or stream over a connection: <dataset> — the other side"
                     " syncs it with a source of type sutra-peer; the"
                     " connection's --datasets must export and its ceiling admit its markings")
           :pos [:dataset]}}
   :peering-share
   {:delete {:verb :peer-unshare :path ["peer" "share" "remove"] :section :source
             :sum "stop sharing a dataset over a connection"}}
   :peering-jobs
   {:get {:verb :peer-jobs :path ["peer" "jobs"] :section :source
          :sum "a connection's peering jobs, newest first, its health and resend queue"}}
   :peering-run
   {:post {:verb :peer-run :path ["peer" "run"] :section :source :send {}
           :sum "one round of every peer connection now (the job runs every few seconds)"}}
   :peering-profiles
   {:get  {:verb :peer-profiles :path ["peer" "profile" "list"] :section :source
           :sum "the peer profiles: pair, side, space, the instances each applies to"}
    :post {:verb :peer-profile-create :path ["peer" "profile" "create"] :section :source
           :sum (str "a peer profile pair, this side's kept: <name> --space S [--side hub|spoke]"
                     " [--strategy all|mesh|instance:<trust>|labels:a,b] [--auto-connect true]"
                     " [--source-data …] [--action-data …] [--classification …] [--markings …]"
                     " --types '[{:local-type … :remote-type … :properties {a b}"
                     " :source-data :export}]' — prints the counterpart to install there")
           :pos [:name]
           :body {:space :string :side :string :strategy :string :auto-connect :string
                  :source-data :string :action-data :string :classification :string
                  :markings :string :types :string}}}
   :peering-profile-install
   {:post {:verb :peer-profile-install :path ["peer" "profile" "install"] :section :source
           :sum (str "install the other side's counterpart profile for a space here:"
                     " --profile '<counterpart EDN>' --space S [--strategy …]"
                     " [--auto-connect true]")
           :body {:profile :string :space :string :strategy :string :auto-connect :string}}}
   :peering-profile
   {:get    {:verb :peer-profile :path ["peer" "profile" "describe"] :section :source
             :sum "one peer profile: its targets, the connections it made, its counterpart"}
    :delete {:verb :peer-profile-delete :path ["peer" "profile" "delete"] :section :source
             :sum "delete a peer profile here (its connections stay)"
             :confirm "DELETE the peer profile {profile}? Its connections stay."}}
   :peering-mesh
   {:get  {:verb :peer-mesh :path ["peer" "mesh" "list"] :section :source
           :sum "the instances of the mesh and their labels"}
    :post {:verb :peer-mesh-set :path ["peer" "mesh" "set"] :section :source
           :sum (str "a trusted instance as a node of the mesh: <trust> [--labels a,b]"
                     " [--remove true]")
           :pos [:trust] :body {:labels :string :remove :string}}}
   :project-deps
   {:get {:verb :project-deps :path ["project" "deps"]
          :section :project
          :sum "measured project dependencies vs. declared :imports"
          :aliases ["project-deps"]}}
   :proposal
   {:delete {:verb :close-proposal :path ["proposal" "close"]
             :section :branch :sum "withdraw/reject without merging"
             :aliases ["close-proposal"]}}
   :proposal-approve
   {:post {:verb :approve :path ["proposal" "approve"]
           :section :branch
           :sum "approve someone else's proposal (--note why)"
           :aliases ["approve"]
           :body {:note :string :task :string} :send {}}}
   :proposal-reject
   {:post {:verb :reject :path ["proposal" "reject"] :section :branch
           :sum "block a proposal's merge without closing it (--note why)"
           :aliases ["reject"]
           :body {:note :string :task :string} :send {}}}
   :stream-archive
   {:post {:verb :stream-flush :path ["stream" "flush"]
           :section :pipeline :send {}
           :sum "index into the Ontology, then archive into the dataset — on demand; the loop does the same on its cadence"}}
   :stream-by
   {:get {:verb :stream-show :path ["stream" "show"] :section :pipeline
          :sum "one stream: its keys, end offsets, buffer depth and where each consumer stands"}}
   :object-counts
   {:get {:verb :overview :path ["overview"] :section :orient
          :sum "branch + per-type instance COUNTS"}}
   :markings
   {:get {:verb :markings :path ["marking" "list"] :section :access
          :sum "the declared markings: category, rank, disjunctive, who may remove, how many carry"
          :aliases ["markings"]}}
   :marking
   {:get {:verb :marking :path ["marking" "describe"] :section :access
          :sum "one marking: what carries it, what inherits it, which inputs stop it"}}
   :access-blocked
   {:get {:verb :access-blocked :path ["access" "why-blocked"] :section :access
          :sum "why a principal cannot reach a resource: missing markings, walls outside"}}
   :access-requests
   {:get {:verb :access-requests :path ["access" "requests"] :section :access
          :sum "open access requests; --inbox: those you may decide"
          :as {:inbox? :inbox}}}
   :egress-policies
   {:get {:verb :egress-policy :path ["source" "egress" "policy"] :section :source
          :sum "network egress policies: state, who uses them, what no policy covers"}}
   :dataset-retention
   {:get {:verb :retention-plan :path ["retention" "plan"] :section :data
          :sum (str "Retention over datasets: per dataset its horizon now, the one"
                    " the policies give, how many transactions that deletes (admin)")}}
   :document-versions
   {:get {:verb :document-versions :path ["document" "versions"] :section :orient
          :sum "a document's saved versions — named, endorsed, autosave"
          :aliases ["document-versions"]}}
   :documents-linked
   {:get {:verb :documents-linked :path ["document" "linked"] :section :orient
          :sum "the documents that reference one object" :aliases ["documents-linked"]}}
   :datasource-columns
   {:get {:verb :source-columns :path ["source" "columns"] :section :source
          :sum "introspect columns" :aliases ["source-columns"]}}
   :linter-list
   {:get {:verb :linter-list :path ["linter" "list"] :section :operate
          :sum (str "what the last sweep recommends (linter): [--mode"
                    " pipeline-resilience|resource-savings] [--rule R] [--project P]"
                    " [--assignee A] [--snoozed]")}}
   :linter-assign
   {:post {:verb :linter-assign :path ["linter" "assign"] :section :operate
           :sum "assign one for tracking: <rule> <resource> <assignee>"
           :pos [:rule :resource :assignee]}}
   :dataset-force-snapshot
   {:post {:verb :force-snapshot :path ["dataset" "snapshot"] :section :catalog
           :sum "run its next build as a snapshot" :aliases ["force-snapshot"]
           :body {:message :string} :send {}}}
   :widget-sets
   {:get {:verb :widget-sets :path ["widget-set"] :section :project
          :sum "widget sets: versioned ClojureScript shown in App Studio by a :custom-widget"
          :aliases ["widget-sets"]}}})

(def ^:private query-arg-type
  {:bool :boolean :kw :keyword :long :int})

(def ^:private url-arg-type
  [:or :keyword :string])

(defn- arg-for
  [as k]
  (or (some (fn [[a f]] (when (= f k) a)) as) k))

(defn- param-arg
  [{:keys [as resolve]} p]
  (or (get-in resolve [p :by]) (arg-for as p)))

(defn- arg-type [t]
  (get query-arg-type (if (vector? t) (first t) t) :string))

(defn- declared
  []
  (for [[id ms] by-endpoint
        [method cs] ms
        c (if (map? cs) [cs] cs)]
    [id method c]))

(let [bad (for [[id method] (declared)
                :when (not (contains? (paths/methods-of id) method))]
            [id method])]
  (assert (empty? bad) (str "a verb on a method its endpoint does not have: " (vec bad))))

(def cli-verbs
  (into (sorted-map)
        (for [[id method c] (declared)]
          [(:verb c) (assoc c :id id :method method)])))

(defn- resolve-problem
  [id rs]
  (some (fn [[p {:keys [from rows by]}]]
          (cond (not (some #{p} (paths/params-of (paths/url id)))) (str p " is no url param")
                (not (contains? (paths/methods-of from) :get)) (str from " has no GET")
                (not (and (keyword? rows) (keyword? by))) ":rows and :by are keywords"))
        rs))

(let [bad (for [[id method {:keys [verb path section sum pos as send confirm] rs :resolve}]
                (declared)
                :let [p (if (and (keyword? verb) (seq path) (keyword? section) (string? sum)
                                 (every? keyword? pos)
                                 (or (nil? as) (map? as))
                                 (or (nil? send) (map? send))
                                 (or (nil? confirm) (string? confirm)))
                          (resolve-problem id rs)
                          "needs :verb :path :section :sum (:pos keywords, :as and :send maps)")]
                :when p]
            [id method verb p])]
  (assert (empty? bad) (str "a verb of a method: " (vec bad)))
  (assert (= (count cli-verbs) (count (declared)))
          "a verb is declared twice"))

(defn cli-args
  [verb]
  (let [{:keys [id body pos as] :as c} (cli-verbs verb)
        ps (paths/params-of (paths/url id))]
    (into [:map {:closed true}]
          (concat (map-indexed (fn [i p] [(param-arg c p) {:pos i} url-arg-type]) ps)
                  (map-indexed (fn [i p] [p {:pos (+ (count ps) i)} :string]) pos)
                  (for [[k t] (sort-by key (paths/query id)) :when (not (some #{k} ps))]
                    [(arg-for as k) {:optional true} (arg-type t)])
                  (for [[k t] (sort-by key body)]
                    [k {:optional true} t])))))

(def cli-steps
  (vec (for [[verb {:keys [id pos]}] cli-verbs]
         [verb (if (seq (into (paths/params-of (paths/url id)) pos)) :req :opt)
          (cli-args verb)])))

(def cli-leaves
  (vec (for [[_ c] cli-verbs]
         (select-keys c [:path :verb :section :sum :aliases]))))

(defn cli-call
  [verb args]
  (let [{:keys [id method body pos as send] rs :resolve :as c} (cli-verbs verb)
        ps     (into (paths/params-of (paths/url id)) (keys (paths/query id)))
        fields (merge send (update-keys (select-keys args (concat pos (keys body)))
                                        #(get as % %)))]
    (cond-> {:method method :id id
             :params (into {} (keep (fn [p]
                                      (when-not (get rs p)
                                        (when-some [v (get args (param-arg c p))] [p v]))))
                           ps)
             :body   (if (contains? c :send) fields (not-empty fields))}
      rs (assoc :lookups (vec (for [[p r] rs]
                                (assoc r :param p :value (get args (:by r)))))))))
