(ns sutra.authoring.http.products
  (:require [sutra.types.plan :as cp]
            [clojure.string :as str]
            [sutra.kit.anomaly :as anomaly]
            [sutra.authoring.ports :as ports]
            [sutra.authoring.marketplace :as mk]
            [sutra.access.api :as access]
            [sutra.parser.product :as product]
            [sutra.parser.project-emit :as pemit]
            [sutra.actions.api :as actions]
            [sutra.authoring.http.oms :as oms]
            [sutra.http.refusals :as refusals]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.project-scope :as project-scope]
            [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]
            [sutra.store.edge :as edge]
            [sutra.store.world :as world]
            [sutra.catalog.lookup :as lookup]))

(set! *warn-on-reflection* true)

(defn offers [store] (ports/products-offers store))

(def ^:private types-of
  (memoize (fn [offer] (vec (sort (keys (:objects ((:schema offer)))))))))

(defn- installed
  [store branch id]
  (ctl/get store :package-installs {:package (name id) :branch (str branch)}))

(defn list-offers
  [{schema :sutra/schema store :sutra/store :as req}]
  (let [os     (offers store)
        branch (actions/branch-of req)]
    (ok {:packages (product/listing os
                                    #(types-of (get os %))
                                    #(and schema (lookup/rid-of schema :object-types %))
                                    #(:version (installed store branch %)))})))

(defn- offer-of [store package]
  (let [os (offers store)]
    (when (or (string? package) (keyword? package))
      [(keyword (name package)) (get os (keyword (name package)))])))

(defn- unknown [store package]
  (let [known (mapv :id (product/listing (offers store) (constantly []) (constantly false)))]
    (refusal/refuse! :refusal.authoring/unknown-package {:package package :known known}
                     {:known-list (str/join ", " known)})))

(defn- staged-and-landed
  [{store :sutra/store :as req} id {:keys [version] :as offer} release fragment
   {:keys [preview? land?]}]
  (let [resp (oms/install-preset (assoc req
                                        :body-params
                                          {:schema fragment :preview? (boolean preview?)}))
        go?  (and (<= 200 (:status resp) 299) (not preview?) (actions/land? req land?))
        resp (cond->> resp go? (actions/land! req))]
    (when (get-in resp [:body :landed])
      (ctl/upsert! store :package-installs {:package (name id) :branch (str (actions/branch-of req))
                                         :version (or version "0") :release release
                                         :installed-by (policy/actor-name store)}))
    (update resp :body assoc :package (name id) :version (:version offer))))

(defn install-offer
  [{{:keys [package] :as body} :body-params :as req}]
  (let [[id offer] (offer-of (:sutra/store req) package)]
    (if-not offer
      (unknown (:sutra/store req) package)
      (let [release ((:schema offer))]
        (staged-and-landed req id offer release release body)))))

(defn upgrade-offer
  [{{:keys [package resolve] :as body} :body-params schema :sutra/schema store :sutra/store
    :as req}]
  (let [[id offer] (offer-of (:sutra/store req) package)]
    (if-not offer
      (unknown (:sutra/store req) package)
      (let [had     (installed store (actions/branch-of req) id)
            release ((:schema offer))
            current (get-in (pemit/export schema {:all-projects? true}) [:ent :schema])
            {merged :body :keys [conflicts]}
            (product/release-upgrade (:release had) release current
                                     (some-> resolve name keyword #{:mine :theirs}))]
        (cond
          (seq conflicts)
          (refusal/refuse! :refusal.authoring/release-conflicts-instance
                           {:conflicts conflicts :from (:version had) :to (:version offer)}
                           {:n (count conflicts)})

          (and had (= (:version had) (:version offer)))
          (ok {:package (name id) :version (:version offer) :unchanged true})

          :else
          (update (staged-and-landed req id offer release merged body) :body assoc
                  :from (:version had) :reinstall? (nil? had)))))))

(defn- refused
  [req f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (if-let [r (anomaly/of e)] (refusals/response req r) (throw e)))))

(defmacro ^:private refusing [req & body] `(refused ~req (fn [] ~@body)))

(defn- branch-name [req] (str (actions/branch-of req)))

(defn list-products
  [{store :sutra/store :as req}]
  (refusing req
           (let [installs (group-by :product (mk/installations store))]
              (ok {:issuer   (:issuer (mk/issuer store))
                   :products (vec (for [[p rs] (sort-by key (group-by :product
                                                                      (mk/releases store)))]
                                    {:product       p
                                     :versions      (mapv mk/summary
                                                          (sort-by :version
                                                                   product/compare-versions rs))
                                     :installations (count (get installs p))}))}))))

(defn- project-documents
  [store auth schema projects]
  (if-let [docs (ports/documents store)]
    (let [by-rid (into {} (keep (fn [p] (some-> (lookup/rid-of schema :projects (keyword p))
                                                (#(vector (str %) (name p))))))
                       projects)]
      (vec (for [d    ((:list docs) store auth {})
                 :let [p (get by-rid (str (:project d)))]
                 :when p
                 :let [full ((:get docs) store auth (:id d) :read)]
                 :when full]
             (assoc (select-keys full [:id :name :format :body :parameters :page])
                    :project p))))
    []))

(defn publish-product
  [{store :sutra/store schema :sutra/schema auth :sutra/auth body :body-params :as req}]
  (refusing req
           (ok {:published
                 (mk/summary (mk/publish! store (branch-name req) schema
                                          (policy/actor-name store)
                                          (assoc body :documents
                                                 (project-documents store auth schema
                                                                    (:projects body)))))})))

(defn describe-product
  [{{p :product} :path-params store :sutra/store :as req}]
  (refusing req
           (let [rs (sort-by :version product/compare-versions (mk/releases store p))]
              (if (empty? rs)
                (refusal/refuse! :refusal.authoring/no-product {:product p})
                (ok {:product       p
                     :releases      (vec (for [r rs]
                                           (assoc (mk/summary r)
                                                  :inputs   (get-in r [:bundle :inputs])
                                                  :projects (mapv :project
                                                                  (get-in r [:bundle :bodies])))))
                     :installations (mapv #(dissoc % :release)
                                          (mk/installations store {:product p}))})))))

(defn transition-release
  [{{:keys [product version]} :path-params {:keys [event channel]} :body-params
    store :sutra/store :as req}]
  (refusing req (ok {:release (mk/summary (mk/transition! store product version
                                                          (or event :release) channel))})))

(defn export-bundle
  [{{:keys [product version]} :path-params store :sutra/store :as req}]
  (refusing req
           (if-let [r (mk/release store product version)]
              (if (= :withdrawn (:state r))
                (refusal/refuse! :refusal.authoring/release-withdrawn
                                 {:product product :version version})
                (ok {:bundle (:bundle r)}))
              (refusal/refuse! :refusal.authoring/no-release {:product product :version version}))))

(defn import-bundle
  [{{:keys [bundle] :as body} :body-params store :sutra/store :as req}]
  (refusing req (ok {:imported (mk/summary (mk/import! store bundle body))})))

(defn product-trust
  [{store :sutra/store :as req}]
  (refusing req (ok (assoc (mk/issuer store) :trusted (mk/trusted store)))))

(defn- fresh
  [{store :sutra/store :as req}]
  (let [e (edge/read-edge store (branch-name req)
                          (grants/current-user-id (grants/actor store)) {:writes? true})]
    (assoc req :sutra/edge e :sutra/schema (:schema e))))

(defn- present? [schema]
  (fn [section nm] (some? (lookup/rid-of schema (keyword (name section)) (keyword (name nm))))))

(defn- stage-and-land
  [req body {:keys [preview? land?]}]
  (let [resp (oms/install-preset (assoc req :body-params {:schema body
                                                          :preview? (boolean preview?)}))
        go?  (and (<= 200 (:status resp) 299) (not preview?) (actions/land? req land?))]
    (cond->> resp go? (actions/land! req))))

(defn- installable
  [store p version channel]
  (let [r (if version
            (mk/release store p version)
            (->> (mk/releases store p)
                 (filter #(product/visible-on? % channel))
                 (sort-by :version product/compare-versions)
                 last))]
    (cond
      (nil? r)
      (refusal/refuse! :refusal.authoring/no-installable-release {:product p :version version}
                       {:which (if version (str " " version) (str " on channel " (name channel)))})
      (not (product/installable? r))
      (refusal/refuse! :refusal.authoring/release-not-released
                       {:product p :version (:version r) :state (:state r)}
                       {:state-name (name (:state r))})
      :else r)))

(defn- install-documents!
  [{store :sutra/store auth :sutra/auth :as req} i bundle]
  (when-let [docs (ports/documents store)]
    (let [schema (:sutra/schema (fresh req))]
      (vec (for [d    (product/installed-documents bundle (:install-id i) i)
                 :let [p (lookup/rid-of schema :projects (keyword (:project d)))]
                 :when p]
             (:id ((:save! docs) store auth
                                 (assoc d :project (str p)
                                        :label (str (:product i) " " (:version i))))))))))

(defn- settled!
  [{store :sutra/store :as req}]
  (doseq [i (mk/settle! store (branch-name req) (:sutra/schema (fresh req)))
          :let [r (mk/release store (:product i) (:version i))]
          :when r]
    (when-let [ids (seq (install-documents! req i (:bundle r)))]
      (mk/record-install! store (assoc i :documents (vec ids))))))

(declare install*)

(defn- linked!
  [req store bundle {:keys [with-linked? preview? land?]}]
  (let [links (product/resolve-requires (:requires bundle)
                                        (mk/installations store {:branch (branch-name req)})
                                        (mk/releases store))
        unmet (filterv #(= :unmet (:via %)) links)
        todo  (filterv #(= :install (:via %)) links)]
    (cond
      (seq unmet)
      (refusal/refuse! :refusal.authoring/linked-missing {:requires unmet})
      (and (seq todo) (not with-linked?))
      (refusal/refuse! :refusal.authoring/linked-first {:install todo})
      (and (seq todo) (not preview?))
      (mapv #(:body (install* req (:product %) {:version (:version %) :land? land?
                                                :with-linked? true}))
            todo)
      :else (mapv #(assoc % :planned true) todo))))

(defn- install*
  [req p {:keys [version channel suffix prefix bindings lock? auto-upgrade? window preview?]
          :as opts}]
  (settled! req)
  (let [store    (:sutra/store req)
        channel  (keyword (name (or channel :stable)))
        r        (installable store p version channel)
        bundle   (:bundle r)
        linked   (linked! req store bundle opts)
        req      (if (and (seq linked) (not preview?)) (fresh req) req)
        schema   (:sutra/schema req)
        shape    {:prefix prefix :suffix suffix :bindings bindings}
        projects (product/installed-projects bundle shape)
        owned    (into #{} (mapcat :projects) (mk/installations store {:branch (branch-name req)}))
        vacated  (mk/vacated store)
        back?    (fn [nm rid] (and (= (str p) (get vacated nm)) (not (owned nm))
                                   (project-scope/holds-nothing? schema rid)))
        taken    (filter #(when-let [rid (lookup/rid-of schema :projects (keyword %))]
                            (not (back? % rid)))
                         projects)
        problems (concat (product/input-problems (:inputs bundle) bindings (present? schema))
                         (product/window-problems window)
                         (for [t taken]
                           {:problem (if (owned t) :installed-already :name-taken)
                            :project t}))]
    (cp/check! (cp/of-findings (cp/problems :authoring/install-product problems))
               {:code   :refusal.authoring/product-not-installable
                :params {:product p :version (:version r)
                         :hint    (if (seq taken) " — install it again with --suffix" "")}})
    (let [body    (product/installed-body bundle shape)
          resp    (stage-and-land req body opts)
          id      (mk/new-install-id)
          landed? (boolean (get-in resp [:body :landed]))
          staged? (and (<= 200 (:status resp) 299) (not preview?) (not landed?))
          row     {:install-id id :product p :version (:version r)
                   :branch (branch-name req) :projects projects
                   :prefix prefix :suffix suffix :bindings bindings
                   :release body :locked (boolean lock?) :channel channel
                   :auto-upgrade (boolean auto-upgrade?) :window window
                   :state (if landed? :installed :staged)
                   :linked (mapv #(select-keys % [:product :version :install-id :planned])
                                 linked)
                   :installed-by (policy/actor-name store)}]
      (when (or landed? staged?)
        (mk/record-install! store row)
        (mk/reclaim! store projects))
      (when landed?
        (when-let [ids (seq (install-documents! req row bundle))]
          (mk/record-install! store (assoc row :documents (vec ids)))))
      (update resp :body assoc :product p :version (:version r) :projects projects
              :linked linked :state (when (or landed? staged?) (:state row))
              :install-id (when (or landed? staged?) id)))))

(defn install-product
  [{{p :product} :path-params body :body-params :as req}]
  (refusing req (install* req p body)))

(defn- upgrade*
  [req id {:keys [version resolve preview?] :as opts} auto?]
  (settled! req)
  (let [store (:sutra/store req)
        i     (mk/installation store id)
        _     (when (not= (:branch i) (branch-name req))
                (refusal/refuse! :refusal.authoring/installation-elsewhere {:branch (:branch i)}
                                 {:id id}))
        to    (if version
                (installable store (:product i) version (:channel i))
                (product/upgrade-target i (mk/releases store (:product i)) auto?))]
    (if (or (nil? to) (= (:version to) (:version i)))
      (ok {:install-id id :version (:version i) :unchanged true})
      (let [body    (product/installed-body (:bundle to) i)
            current (get-in (pemit/export (:sutra/schema req) {:all-projects? true})
                            [:ent :schema])
            {merged :body :keys [conflicts]}
            (product/release-upgrade (:release i) body current
                                     (some-> resolve name keyword #{:mine :theirs}))]
        (if (seq conflicts)
          (refusal/refuse! :refusal.authoring/release-conflicts-installation
                           {:conflicts conflicts :from (:version i) :to (:version to)}
                           {:n (count conflicts)})
          (let [resp (binding [project-scope/*unlocking* true]
                       (stage-and-land req merged opts))]
            (when (and (get-in resp [:body :landed]) (not preview?))
              (let [i' (assoc i :version (:version to) :release body :last-error nil)
                    ids (install-documents! req i' (:bundle to))]
                (mk/record-install! store (cond-> i' (seq ids) (assoc :documents (vec ids))))))
            (update resp :body assoc :install-id id :from (:version i)
                    :version (:version to))))))))

(defn- uninstall*
  [req id {:keys [preview? land?]}]
  (let [store (:sutra/store req)
        i     (mk/installation store id)
        _     (when (not= (:branch i) (branch-name req))
                (refusal/refuse! :refusal.authoring/installation-elsewhere {:branch (:branch i)}
                                 {:id id}))
        deps  (mk/dependents store i)
        _     (when (seq deps)
                (refusal/refuse! :refusal.authoring/installation-needed {:dependents deps}
                                 {:id id :products (str/join ", " (map :product deps))}))
        _     (settled! req)
        sweep (product/uninstall-sweep i)
        resp  (binding [project-scope/*unlocking* true]
                (let [r   (oms/stage-schema req {:schema {} :preview? (boolean preview?)}
                                            {:strict?          true
                                             :strict-sections  (set (keys sweep))
                                             :strict-projected (delay sweep)})
                      go? (and (<= 200 (:status r) 299) (not preview?) (actions/land? req land?))]
                  (cond->> r go? (actions/land! req))))
        ok?     (and (<= 200 (:status resp) 299) (not preview?))
        landed? (boolean (get-in resp [:body :landed]))]
    (cond
      (and ok? landed?)
      (do (when-let [docs (ports/documents store)]
            (doseq [d (:documents i)]
              (try ((:delete! docs) store (:sutra/auth req) d)
                   (catch clojure.lang.ExceptionInfo e
                     (println (str "[products] uninstall kept document " (pr-str d)
                                   ": " (ex-message e)))))))
          (mk/vacate! store i)
          (mk/forget-install! store id))
      ok? (mk/record-install! store (assoc i :state :uninstalling)))
    (update resp :body assoc :install-id id :product (:product i)
            :projects-stay (vec (:projects i))
            :state (cond landed? :uninstalled ok? :uninstalling))))

(defn uninstall-installation
  [{{id :id} :path-params body :body-params :as req}]
  (refusing req (uninstall* req id body)))

(defn upgrade-installation
  [{{id :id} :path-params body :body-params :as req}]
  (refusing req (upgrade* req id body false)))

(defn list-installations
  [{store :sutra/store schema :sutra/schema :as req}]
  (refusing req
            (ok {:installations
                 (mapv (fn [i]
                         (let [rs (mk/releases store (:product i))]
                           (-> (cond->> (dissoc i :release)
                                 (= (:branch i) (branch-name req)) (mk/effective schema))
                               (assoc :upgrade
                                      (:version (product/upgrade-target i rs false))))))
                       (mk/installations store))})))

(defn configure-installation
  [{{id :id} :path-params body :body-params store :sutra/store :as req}]
  (refusing req (ok {:installation (dissoc (mk/configure! store id body) :release)})))

(defn- installer-request
  [store b username]
  (when-let [auth (access/of-principal store b username)]
    (let [st (world/viewing (ctx/store b auth))
          e  (edge/read-edge st b (grants/current-user-id auth) {:writes? true})]
      {:sutra/store st :sutra/auth auth :sutra/schema (:schema e) :sutra/branch b
       :sutra/edge e})))

(defn settle-all!
  [store]
  (doseq [[b is] (group-by :branch (filter #(or (mk/staged? %) (mk/uninstalling? %))
                                           (mk/installations store)))
          :let [req (installer-request store b (:installed-by (first is)))]
          :when req]
    (try (settled! req)
         (catch InterruptedException e (throw e))
         (catch Exception e (println "[products] settling on" b "failed:" (ex-message e))))))

(defn auto-upgrade!
  [store]
  (settle-all! store)
  (doseq [i (mk/installations store)
          :when (and (:auto-upgrade i) (not (mk/staged? i)) (not (mk/uninstalling? i)))
          :let  [to (product/upgrade-target i (mk/releases store (:product i)) true)]
          :when (and to (product/in-window? (:window i) (ctl/instant store)))]
    (let [req (installer-request store (:branch i) (:installed-by i))
          why (try (if-not req
                     (str "the installer " (:installed-by i) " is no principal any more")
                     (let [resp (upgrade* req (:install-id i)
                                          {:version (:version to) :land? true} true)]
                       (when-not (get-in resp [:body :landed])
                         (pr-str (select-keys (:body resp) [:error :land-refused :data])))))
                   (catch InterruptedException e (throw e))
                   (catch Exception e (ex-message e)))]
      (when why
        (println "[products] auto-upgrade of" (:install-id i) "failed:" why)
        (mk/record-install! store (assoc (mk/installation store (:install-id i))
                                         :last-error why))))))

(def module
  {:name        :products
   :wants       #{:documents/records}
   :jobs        [{:name :product-auto-upgrade :every "SUTRA_PRODUCT_UPGRADE_MS"
                  :tick auto-upgrade! :default 300000}]
   :contributes {:http/routes
                 {:packages                     {:get list-offers}
                  :package-install              {:post install-offer}
                  :package-upgrade              {:post upgrade-offer}
                  :products                     {:get list-products :post publish-product}
                  :product                      {:get describe-product}
                  :product-release              {:post transition-release}
                  :product-bundle               {:get export-bundle}
                  :product-imports              {:post import-bundle}
                  :product-trust                {:get product-trust}
                  :product-install              {:post install-product}
                  :product-installations        {:get list-installations}
                  :product-installation         {:post configure-installation}
                  :product-installation-upgrade {:post upgrade-installation}
                  :product-installation-uninstall {:post uninstall-installation}}}})
