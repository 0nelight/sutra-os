(ns sutra.authoring.marketplace
  (:require [sutra.types.plan :as cp]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.access.api :as access]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.lifecycle :as lc]
            [sutra.foundation.ops :as op]
            [sutra.parser.product :as product]
            [sutra.parser.project-emit :as pemit]
            [sutra.catalog.branch-policy :as policy]
            [sutra.store.control :as ctl]
            [sutra.store.storage.instance-config :as icfg])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(def ^:private issuer-key "product_issuer")

(defn- derived-issuer [store fingerprint]
  (or (some-> (icfg/platform-title store)
              str/lower-case
              (str/replace #"[^a-z0-9]+" "-"))
      (str "sutra-" (subs (str/replace (str fingerprint) ":" "") 0 8))))

(defn issuer
  [store]
  (let [{:keys [public-key fingerprint]} (access/instance-identity store)]
    {:issuer      (or (icfg/get-cfg store issuer-key) (derived-issuer store fingerprint))
     :public-key  public-key
     :fingerprint fingerprint}))

(defn issuer!
  [store]
  (let [i (issuer store)]
    (when-not (icfg/get-cfg store issuer-key) (icfg/set-cfg! store issuer-key (:issuer i)))
    i))

(defn trusted
  [store]
  (vec (sort (keys (access/instance-trusts store)))))

(defn- trusted-as
  [store public-key]
  (if (= public-key (:public-key (access/instance-identity store)))
    "self"
    (some (fn [[nm t]] (when (= public-key (:public-key t)) nm))
          (access/instance-trusts store))))

(defn- sign-with [store] #(access/sign-payload store %))

(defn releases
  ([store] (ctl/list store :products {}))
  ([store p] (ctl/list store :products {:product (name p)})))

(defn release [store p v] (ctl/get store :products {:product (name p) :version (str v)}))

(defn installations
  ([store] (ctl/list store :product-installs {}))
  ([store where] (ctl/list store :product-installs where)))

(defn installation
  [store id]
  (or (ctl/get store :product-installs {:install-id (str id)})
      (refusal/refuse! :refusal.authoring/no-installation {:install-id id})))

(defn summary
  [r]
  (-> (select-keys r [:product :version :channel :state :description :digest :requires
                      :origin :published-by :published-at])
      (assoc :inputs (count (get-in r [:bundle :inputs])))))

(defn staged?
  [i]
  (= "staged" (some-> (:state i) name)))

(defn uninstalling?
  [i]
  (= "uninstalling" (some-> (:state i) name)))

(defn- project-names [schema]
  (into #{} (keep (comp #(some-> % name) :project-name val)) (:projects schema)))

(defn- standing?
  [schema i]
  (let [have (project-names schema)]
    (boolean (and (seq (:projects i)) (every? #(contains? have (str %)) (:projects i))))))

(defn effective
  [schema i]
  (cond-> i (and (staged? i) schema (standing? schema i)) (assoc :state :installed)))

(defn- branch-of [b] (if (str/blank? (str b)) policy/default-branch-name (str b)))

(defn locked-projects
  [store branch schema]
  (into #{}
        (comp (filter #(= (branch-of branch) (branch-of (:branch %))))
              (map #(effective schema %))
              (filter :locked) (remove staged?) (remove uninstalling?)
              (mapcat :projects) (map str))
        (installations store)))

(op/defop :authoring/locked-projects [{:keys [store]} {:keys [branch schema]}]
  (locked-projects store branch schema))

(defn- put! [store t rows]
  (ctl/commit! store [[:upsert t rows]])
  rows)

(defn- auto-requires
  [store branch schema inputs]
  (let [by-project (into {} (for [i (installations store {:branch (str branch)})
                                  p (:projects i)]
                              [(str p) i]))]
    (into {} (for [{:keys [rid]} inputs
                   :let [section (some #(when (contains? (get schema %) rid) %)
                                       [:object-types :link-types :datasets :actions :sources
                                        :interfaces :shared-properties :value-types
                                        :functions])
                         p       (some-> (get-in schema [section rid :project])
                                         (#(get-in schema [:projects % :project-name])) name)
                         i       (get by-project p)]
                   :when i]
               [(:product i) (str "^" (:version i))]))))

(defn publish!
  [store branch schema actor {:keys [product version projects channel description requires
                                     release? documents]}]
  (let [product  (some-> product name str/trim)
        projects (vec (distinct (map name projects)))
        unknown  (remove (project-names schema) projects)
        channel  (keyword (name (or channel :dev)))]
    (cond
      (not (re-matches #"[a-z0-9][a-z0-9-]*" (str product)))
      (refusal/refuse! :refusal.authoring/product-name {:product product})
      (nil? (product/parse-version version))
      (refusal/refuse! :refusal.authoring/product-version {:version version})
      (empty? projects)
      (refusal/refuse! :refusal.authoring/product-no-projects {})
      (seq unknown)
      (refusal/refuse! :refusal.authoring/product-unknown-projects
                       {:unknown unknown}
                       {:names (str/join ", " unknown)})
      (not (contains? (set product/channels) channel))
      (refusal/refuse! :refusal.authoring/product-channel
                       {:channel channel}
                       {:channel-name (name channel)
                        :known (str/join ", " (map name product/channels))})
      (release store product version)
      (refusal/refuse! :refusal.authoring/product-version-published
                       {:product product :version version}))
    (let [pkg     (product/package schema {:product product :version version
                                           :projects (map keyword projects)})
          inputs  (product/inputs-of schema pkg)
          links   (auto-requires store branch schema inputs)
          exports (for [p projects]
                    (assoc (pemit/export schema {:project (keyword p) :inline? true})
                           :project p))
          bundle  (product/bundle-of {:product product :version version
                                      :description description
                                      :exports exports
                                      :inputs (mapv #(dissoc % :rid) inputs)
                                      :requires (merge links requires)
                                      :documents documents})]
      (when-let [ps (seq (product/secret-paths (:bodies bundle)))]
        (refusal/refuse! :refusal.authoring/product-credentials {:paths (vec ps)}))
      (let [sealed (product/sealed bundle {:issuer (:issuer (issuer! store))
                                           :sign   (sign-with store)})]
        (first (put! store :products
                     [{:product product :version version :channel channel
                       :state (if release? :released :draft) :description description
                       :bundle sealed :digest (:digest sealed) :requires (:requires sealed)
                       :origin (:issuer (issuer! store)) :published-by actor}]))))))

(defn transition!
  [store product version event channel]
  (let [r     (or (release store product version)
                  (refusal/refuse! :refusal.authoring/no-release
                                   {:product product :version version}))
        event (keyword (name event))
        to    (lc/decide product/release-lifecycle (str product " " version) event
                         (:state r) r nil)
        ch    (case event
                :promote (product/next-channel r)
                :release (keyword (name (or channel (:channel r))))
                (:channel r))]
    (first (put! store :products [(assoc r :state to :channel ch)]))))

(defn import!
  [store bundle {:keys [channel require-signature?]}]
  (let [problems (product/bundle-problems bundle {:trusted-as #(trusted-as store %)
                                                  :valid?     access/signature-valid?
                                                  :require-signature? require-signature?})
        {p :product v :version} bundle]
    (cond
      (seq problems)
      (cp/check! (cp/of-findings (cp/problems :authoring/bundle problems))
                 {:code :refusal.authoring/bundle-not-taken})
      (some-> (release store p v) :digest (not= (:digest bundle)))
      (refusal/refuse! :refusal.authoring/product-version-differs {:product p :version v})
      :else
      (first (put! store :products
                   [{:product p :version v :channel (keyword (name (or channel :stable)))
                     :state :released :description (:description bundle) :bundle bundle
                     :digest (:digest bundle) :requires (:requires bundle)
                     :origin (or (get-in bundle [:signature :issuer]) "unsigned")}])))))

(def ^:private settings-keys [:locked :channel :auto-upgrade :window])

(defn record-install!
  [store row]
  (first (put! store :product-installs [row])))

(declare forget-install! vacate!)

(defn settle!
  [store branch schema]
  (let [here (installations store {:branch (branch-of branch)})
        done (vec (for [i here :when (and (staged? i) (standing? schema i))]
                    (assoc i :state :installed :last-error nil)))
        gone (for [i here
                   :when (and (uninstalling? i)
                              (not (product/sweep-standing? schema (product/uninstall-sweep i))))]
               i)]
    (when (seq done) (put! store :product-installs done))
    (doseq [i gone] (vacate! store i) (forget-install! store (:install-id i)))
    done))

(defn dependents
  [store i]
  (let [here (installations store {:branch (branch-of (:branch i))})
        mine (str (:install-id i))]
    (vec (for [o here
               :when (not= mine (str (:install-id o)))
               :let [linked? (some #(= mine (str (:install-id %))) (:linked o))
                     range   (get (:requires (release store (:product o) (:version o)))
                                  (:product i))
                     others  (filter #(and (= (:product i) (:product %))
                                           (not= mine (str (:install-id %)))
                                           (or (nil? range)
                                               (product/satisfies? (:version %) range)))
                                     here)]
               :when (or linked? (and range (empty? others)))]
           (select-keys o [:install-id :product :version])))))

(def ^:private vacated-key "product_vacated_projects")

(defn vacated
  [store]
  (or (some-> (icfg/get-cfg store vacated-key) edn/read-string) {}))

(defn- swap-vacated! [store f]
  (icfg/swap-cfg! store vacated-key
                  #(pr-str (f (or (some-> % edn/read-string) {})))))

(defn vacate!
  [store i]
  (when (seq (:projects i))
    (swap-vacated! store #(into % (for [p (:projects i)] [(str p) (str (:product i))])))))

(defn reclaim!
  [store projects]
  (when (seq projects) (swap-vacated! store #(apply dissoc % (map str projects)))))

(defn forget-install!
  [store id]
  (ctl/commit! store [[:delete :product-installs {:install-id (str id)}]]))

(defn configure!
  [store id settings]
  (let [i   (installation store id)
        s   (select-keys settings settings-keys)
        bad (concat (product/window-problems (:window s))
                    (when-let [c (:channel s)]
                      (when-not (contains? (set product/channels) (keyword (name c)))
                        [{:problem :unknown-channel :channel c}])))]
    (cp/check! (cp/of-findings (cp/problems :authoring/settings bad))
               {:code :refusal.authoring/settings-not-kept})
    (record-install! store (merge i (cond-> s (:channel s) (update :channel
                                                                   (comp keyword name)))))))

(defn new-install-id [] (str (UUID/randomUUID)))

(defn product-of-project
  [store p]
  (let [p (str p)]
    (or (some #(when (some #{p} (map str (:projects %))) (:product %)) (installations store))
        (some (fn [r] (when (some #(= p (str (:project %))) (get-in r [:bundle :bodies]))
                        (:product r)))
              (releases store)))))
