(ns sutra.repos.api
  (:require [sutra.repos.access :as repo-access]
            [sutra.repos.checks :as repo-checks]
            [sutra.repos.objects :as g]
            [sutra.repos.projection :as projection]
            [sutra.repos.records :as records]
            [sutra.repos.riders :as riders]))

(set! *warn-on-reflection* true)

(defn advisories-for [findings authored] (repo-checks/advisories-for findings authored))

(defn authored-apps [pushed live] (repo-checks/authored-apps pushed live))

(defn changed-type-advisories [findings changed]
  (repo-checks/changed-type-advisories findings changed))

(defn checks-of
  ([store rows] (repo-checks/checks-of store rows))
  ([store rows known] (repo-checks/checks-of store rows known)))

(defn checks-of-ledger [store ledger] (repo-checks/checks-of-ledger store ledger))

(defn commit-ent [store repos repo-rid sha] (repo-checks/commit-ent store repos repo-rid sha))
(defn push-ent [live repos repo-rid sha] (repo-checks/push-ent live repos repo-rid sha))
(defn changed-texts
  [schema repos repo-rid old base-sha new]
  (repo-checks/changed-texts schema repos repo-rid old base-sha new))

(defn commit-ledger [store schema branches] (repo-checks/commit-ledger store schema branches))

(defn member-retire-verdict [opts] (repo-checks/member-retire-verdict opts))

(defn proposal-checks [store schema source] (repo-checks/proposal-checks store schema source))

(defn proposal-facts
  [store repos schema source target] (repo-checks/proposal-facts store repos schema source target))

(defn publish-only!
  [store repo-rid sha ent publish-fn]
  (repo-checks/publish-only! store repo-rid sha ent publish-fn))

(defn push-plan-opts [repo-rid schema] (repo-checks/push-plan-opts repo-rid schema))

(defn repo-settings [repos repo-rid base-branch]
  (repo-checks/repo-settings repos repo-rid base-branch))

(defn repository-rid [schema x] (repo-checks/repository-rid schema x))

(defn resolve-code-files
  ([schema repos body branch] (repo-checks/resolve-code-files schema repos body branch))
  ([schema repos body branch default-repo]
   (repo-checks/resolve-code-files schema repos body branch default-repo)))

(defn run-checks! [store repos repo-rid sha opts]
  (repo-checks/run-checks! store repos repo-rid sha opts))

(defn side-band-lines [opts] (repo-checks/side-band-lines opts))

(defn summary [store repo-rid sha] (repo-checks/summary store repo-rid sha))

(defn with-advisories [detail advisories] (repo-checks/with-advisories detail advisories))

(defn advertise [repos repo-rid service out] (g/advertise repos repo-rid service out))

(defn all-refs [repos repo-rid] (g/all-refs repos repo-rid))

(defn blob-bytes [repos repo-rid blob-sha] (g/blob-bytes repos repo-rid blob-sha))

(defn branch->ref [branch] (g/branch->ref branch))

(defn commit! [ins opts] (g/commit! ins opts))

(defn commit-info [repos repo-rid sha] (g/commit-info repos repo-rid sha))

(defn descends-from?
  [repos repo-rid new-sha old-sha] (g/descends-from? repos repo-rid new-sha old-sha))

(defn entry [bytes mode] (g/entry bytes mode))

(defn gc! [repos repo-rid] (g/gc! repos repo-rid))

(defn gitlink? [x] (g/gitlink? x))

(defn read-tree [repos repo-rid sha] (g/read-tree repos repo-rid sha))

(defn receive-pack [repos repo-rid in out opts] (g/receive-pack repos repo-rid in out opts))

(defn ref->branch [ref] (g/ref->branch ref))

(defn ref-sha [repos repo-rid ref] (g/ref-sha repos repo-rid ref))

(defn update-ref! [repos repo-rid ref sha opts] (g/update-ref! repos repo-rid ref sha opts))

(defn upload-pack [repos repo-rid in out opts] (g/upload-pack repos repo-rid in out opts))

(defmacro with-inserter [& args] `(g/with-inserter ~@args))

(defn write-tree! [ins entries] (g/write-tree! ins entries))

(defn adopt-push!
  [store repos blobs repo-rid commands]
  (projection/adopt-push! store repos blobs repo-rid commands))

(defn commit-files-to!
  [store repos blobs repo-rid branch files message]
  (projection/commit-files! store repos blobs repo-rid branch files message))

(defn rewind-push!
  [repos repo-rid ref old new]
  (projection/rewind-push! repos repo-rid ref old new))

(defn bind-published!
  [store repos repo-rid branch commit-sha seq]
  (projection/bind-published! store repos repo-rid branch commit-sha seq))

(defn forget-branch!
  [store repos repo-rid branch] (projection/forget-branch! store repos repo-rid branch))

(defn merge-git-half!
  [store repos blobs repo-rid source-branch target-branch opts]
  (projection/merge-git-half! store repos blobs repo-rid source-branch target-branch opts))

(defn normalize-followup!
  [store repos blobs repo-rid branch pushed-sha installed-schema]
  (projection/normalize-followup! store repos blobs repo-rid branch pushed-sha installed-schema))

(defn ontology-texts [store repos branch] (projection/ontology-texts store repos branch))

(defn platform-files [store repo-rid] (projection/platform-files store repo-rid))

(defn project-branch!
  [store repos blobs repo-rid branch]
  (projection/project-branch! store repos blobs repo-rid branch))

(defn commits-of [store repo-rid branch] (records/commits-of store repo-rid branch))

(defn files-of [store repo-rid commit-sha] (records/files-of store repo-rid commit-sha))

(defn record-tag!
  [store repo-rid tag commit-sha actor] (records/record-tag! store repo-rid tag commit-sha actor))

(defn repo-anchor [store repo-rid branch] (records/repo-anchor store repo-rid branch))

(defn repo-anchors [store] (records/repo-anchors store))

(defn set-check!
  [store repo-rid commit-sha check-name opts]
  (records/set-check! store repo-rid commit-sha check-name opts))

(defn apply! [store branch ent prior] (riders/apply! store branch ent prior))

(defn merge!
  ([store blobs repo-rid source target] (riders/merge! store blobs repo-rid source target))
  ([store blobs repo-rid source target opts]
   (riders/merge! store blobs repo-rid source target opts)))

(defn ontology-readable? [auth] (repo-access/ontology-readable? auth))

(defn repo-readable? [auth schema repo-rid] (repo-access/repo-readable? auth schema repo-rid))
