(ns sutra.repos.records
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.store.records :as r]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn record-commit! [store {:keys [source seq] :as row}]
  (ctl/upsert! store :git-commits (assoc row :source (name source) :seq (some-> seq long)))
  nil)

(defn commit-row [store repo-rid commit-sha]
  (ctl/get store :git-commits {:repo-rid repo-rid :commit-sha commit-sha}))
(defn commits-of [store repo-rid branch]
  (ctl/list store :git-commits {:repo-rid repo-rid :branch branch}))
(defn commits-of-branches
  [store repo-rid branches]
  (group-by #(str (:branch %))
            (ctl/list store :git-commits {:repo-rid repo-rid :branch (set (map str branches))})))
(defn branch-generation
  [store branch]
  (get-in (world/branch store branch) [:meta :gen]))

(defn raw-repo-anchor
  [store repo-rid branch]
  (ctl/get store :repo-anchor {:repo-rid repo-rid :branch branch}))

(defn repo-anchor
  [store repo-rid branch]
  (let [a (raw-repo-anchor store repo-rid branch)]
    (when (and a (= (:generation a) (branch-generation store branch))) a)))

(defn repo-anchors [store] (ctl/list store :repo-anchor {}))

(defn set-repo-anchor!
  [store repo-rid branch {:keys [watermark tip-sha tip-seq] :as a}]
  (let [gen (branch-generation store branch)]
    (r/plan! store
             [(fn [store at]
                (ctl/swapping store :repo-anchor {:repo-rid repo-rid :branch branch}
                          (fn [old]
                            (let [old (when (= gen (:generation old)) old)]
                              (when (and (contains? a :expect-tip)
                                         (not= (:expect-tip a) (:tip-sha old)))
                                (refusal/refuse!
                                 :refusal.repos/anchor-moved
                                 {:kind ::anchor-moved :repo repo-rid :branch branch
                                  :expected (:expect-tip a) :tip (:tip-sha old)}))
                              {:watermark
                                 (max (long (or watermark 0)) (long (or (:watermark old) 0)))
                               :tip-sha    (or tip-sha (:tip-sha old))
                               :tip-seq    (or (some-> tip-seq long) (:tip-seq old))
                               :generation gen}))
                          at))])
    nil))

(defn record-files! [store repo-rid commit-sha entries]
  (ctl/delete! store :repo-files {:repo-rid repo-rid :commit-sha commit-sha})
  (ctl/insert-many! store :repo-files (map (fn [{:keys [path blob-sha mode plane]}]
                                   {:repo-rid repo-rid :commit-sha commit-sha :path path
                                    :blob-sha blob-sha
                                    :mode (or mode "100644") :plane (name plane)})
                                 entries))
  nil)

(defn files-of
  [store repo-rid commit-sha]
  (let [chain (loop [sha commit-sha acc [] n 0]
                (if (or (nil? sha) (> n 256)) acc
                    (recur (:manifest-base (commit-row store repo-rid sha)) (conj acc sha)
                           (inc n))))
        by    (group-by :commit-sha
                        (when (seq chain)
                          (ctl/list store :repo-files
                                    {:repo-rid repo-rid :commit-sha (set chain)})))
        files (reduce (fn [m sha]
                        (reduce (fn [m f] (if (contains? m (:path f)) m (assoc m (:path f) f)))
                                m (by sha)))
                      {} chain)]
    (->> (vals files) (filter :blob-sha) (sort-by :path)
         (mapv #(select-keys % [:path :blob-sha :mode :plane])))))

(defn git-cas-hashes [store]
  (into #{} (filter string?)
        (concat (map :blob-sha (ctl/list store :repo-files {}))
                (map :object-blob (ctl/list store :git-commits {})))))

(defn set-check! [store repo-rid commit-sha check-name {:keys [status detail started? finished?]}]
  (ctl/upsert! store :repo-checks
               {:repo-rid repo-rid :commit-sha commit-sha :check-name (name check-name)
         :status (name status) :detail detail
         :started-at (when started? ctl/now) :finished-at (when finished? ctl/now)})
  nil)

(defn checks-of [store repo-rid commit-sha]
  (ctl/list store :repo-checks {:repo-rid repo-rid :commit-sha commit-sha}))

(defn checks-of-commits
  [store repo-rid shas]
  (group-by #(str (:commit-sha %))
            (ctl/list store :repo-checks {:repo-rid repo-rid :commit-sha (set (map str shas))})))

(defn record-tag! [store repo-rid tag commit-sha actor]
  (ctl/upsert! store :repo-tags {:repo-rid repo-rid :tag tag :commit-sha commit-sha :actor actor}
               {:conflict :nothing})
  nil)

(defn repo-branches [store repo-rid]
  (vec (sort (distinct (map :branch (ctl/list store :git-commits {:repo-rid repo-rid}))))))

(defn tags-of [store repo-rid] (ctl/list store :repo-tags {:repo-rid repo-rid}))
