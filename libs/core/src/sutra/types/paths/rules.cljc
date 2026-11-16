(ns sutra.types.paths.rules)

(def reads #{:read})
(def writes #{:read :write})

(def m-onto
  {:member (str "the ontology's definition: the frontend lives on the whole schema, so seeing a"
                " foreign type's definition stays open")})
(def m-counts {:member "counts and shell state; rows and links are read as the caller"})
(def m-own {:member "only what is the caller's own: session, staging, thread, temp set"})
(def m-ask {:member "anyone may ask for access; the owners decide"})
(def m-instance {:member "describes the instance, no resource of a project"})
(def m-names {:member "user names for display: authors, mentions and the launcher"})
(def m-blob {:member "addressed bytes: without a holding reference nobody reads them"})
(def m-branch
  {:member "branches and proposals are how everyone works; changes are judged where they land"})
(def c-run
  {:custom (str "a build's progress stays open; its inputs, errors and the names and jobs of"
                " projects the caller cannot read are cut (builds.http/run-for)")})
(def c-inbox
  {:custom "one access request: its author, its principal or who may decide it (bac/request)"})

(def c-action
  {:custom "the action is in the body: action/make-authorizer judges each operation"})
(def c-read
  {:custom "the types are in the body or the query: reads authorize them as the caller"})
(def c-app
  {:custom "the application's own grant decides (with-app), for consumers as well"})
(def c-app-preview
  {:custom "a draft's project is in the body: preview-rights-ok? judges it and the live app"})
(def c-workspace
  {:custom "a workspace frame's own rights decide (workspace-rights, resource-access)"})
(def c-schema
  {:custom "new name and project are in the body: mutation's guard judges the changed rows"})
(def c-stage
  {:custom "the nodes are in the body: the staging authorizer and the commit judge them"})
(def c-doc
  {:custom "a document's project is in its row: records/document reads it first"})
(def c-kompass
  {:custom "a node's project is in its row or the parent's: project-scope judges it"})
(def c-widget-set
  {:custom "a widget set's project is in its row: records/set-row! reads it first"})
(def c-branch
  {:custom "a branch's owners are in its role rows: governance/owner-block reads them"})
(def c-proposal
  {:custom "a proposal's owners and tasks are in its row: the governance verbs judge them"})
(def c-access
  {:custom "the deciding project is in the request or body: branching.access judges owners"})
(def c-repo
  {:custom "a repository's readers: repos/repo-readable? (ontology repo included)"})
(def c-agent
  {:custom "an agent's own grant decides, its name may be in the body (access/granted?)"})
(def c-blob
  {:custom "bytes are read through what holds them, named in the query (readable-through?)"})
(def c-exploration
  {:custom "a saved file's place is in its row: its owner, or the role in its project"})
(def c-function
  {:custom "the function is in the body: a stored one needs :read, ad-hoc code the global role"})
(def c-build
  {:custom "the jobs follow from body and plan: authorize-build! judges each job's project"})
(def c-media
  {:custom "the dataset is in the body: land-media needs :update on it"})
(def c-writeback
  {:custom "the object type is in the body: read to preview, :update to confirm"})
