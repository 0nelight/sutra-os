(ns sutra.branching.rebase
  (:require [sutra.actions.api :as actions]
            [sutra.authoring.api :as authoring]
            [sutra.governance.api :as governance]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- land-rebase!
  [store branch rebased parent rev]
  (authoring/install-world! store branch rebased
                               {:message  (str "rebase onto " parent)
                                :reparent {:branch parent :rev rev}
                                :fences   [[:branch-head parent rev]]}))

(defn- rebase!
  [store branch parent resolutions]
  (let [r       (:rev (world/head (world/at store parent)))
        {:keys [conflicts heads]} (governance/staging-merge-check store parent branch)
        decided (governance/resolver resolutions)
        table   #(str (name (:class %)) "/" (rid/->name (:key %)))
        open    (remove #(decided [(:class %) (rid/->name (:key %))]) conflicts)]
    (if (seq open)
      {:status :undecided :error "every conflict needs a decision"
       :undecided (mapv table open)}
      (do (land-rebase! store branch
                        (governance/rebase-schema (governance/heads-merge heads resolutions))
                        parent r)
          {:status :ok :rebased branch :onto parent :resolved (count conflicts)}))))

(defn rebase-staged!
  [store {:keys [parent staging live?]} resolutions]
  (if (or (nil? parent) (not live?))
    {:status :empty}
    (do (actions/decide! store :rebase nil :staged)
        (rebase! store staging parent resolutions))))

(defn rebase-branch!
  [store {:keys [staging live?]} branch parent resolutions]
  (if (and live? (seq (actions/list-staged (ctx/at store staging))))
    {:error "the branch holds staged changes — save or discard them first"}
    (rebase! store branch parent resolutions)))
