(ns ^:unit ontology.object-type-delete-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.world :as onto-install]
            [sutra.types.rid :as rid]
            [support.in-project :refer [in-project]]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.kernel :as kernel]
            [sutra.ref.install :as ri]))

(defn- ix [W] (live/world-index W))

(defn- prepare
  [sch opts]
  (onto-install/prepare-schema (in-project :t sch) opts))

(defn- prep [sch] (:ok (prepare sch {})))

(defn- rid-of [schema kinds-key nm]
  (rid/resolve-name->rid (rid/name->rid-index schema) #{kinds-key} nm))

(def ^:private one-type
  (prep {:object-types {:scratch {:attribute-list [{:attribute-name :x :attribute-type :string}]
                                  :primary-key :x :plural "Scratches"}}}))

(def ^:private two-types+link
  (prep {:object-types
           {:rechnung   {:attribute-list [{:attribute-name :rnr :attribute-type :string}
                                          {:attribute-name :bnr :attribute-type :string}]
                         :primary-key :rnr :plural "Rechnungen"}
            :bestellung {:attribute-list [{:attribute-name :bnr :attribute-type :string}]
                         :primary-key :bnr :plural "Bestellungen"}}
         :link-types  {:rechnung-of {:from-object-type :rechnung :to-object-type :bestellung
                                     :cardinality :many-to-one}}
         :transforms  {:link-rechnung-of
                       {:input  {:kind :object-type :object-type :rechnung}
                        :output {:kind :link-type :link-type :rechnung-of :fk-attribute :bnr}}}}))

(deftest a-types-own-crud-does-not-block-its-delete
  (testing "no CRUD Action Type exists for a type — neither stored nor derived"
    (is (not (contains? (:actions one-type) :create-scratch))
        "the type carries no stored create-<type> action")
    (is (nil? (actions/resolve-action one-type :create-scratch))
        "and nothing derives one either — the platform derives no Action Types"))
  (testing "deleting a type is not blocked by CRUD that does not exist"
    (is (empty? (retirement/deletion-blockers one-type :object-types :scratch
                                              false (ix one-type))))))

(deftest a-delete-leaves-no-residue
  (let [ot-rid (first (keys (:object-types one-type)))
        after  (ri/ensure-actions (update one-type :object-types dissoc ot-rid))]
    (testing "there was no per-type action to dangle in the first place"
      (is (not (contains? (:actions after) :create-scratch)))
      (is (not (contains? (:actions after) :update-scratch)))
      (is (not (contains? (:actions after) :delete-scratch)))
      (is (nil? (actions/resolve-action after :create-scratch))))
    (testing "so the post-delete schema is dangling-free and installs clean"
      (is (kernel/accept? after)))))

(deftest a-genuine-external-referrer-still-blocks
  (testing "a link-type pointing at the object-type blocks its delete"
    (is (= #{:link-types :transforms}
           (set (mapv :in (retirement/deletion-blockers two-types+link :object-types :rechnung
                                                        false (ix two-types+link)))))
        "the delete is refused and names the link and its producer, not the type's own CRUD"))
  (testing "the link-type itself deletes freely — nothing references it"
    (is (empty? (retirement/deletion-blockers two-types+link :link-types :rechnung-of
                                              false (ix two-types+link))))))

(deftest cascade-removes-associated-link-types
  (let [lt-rid (rid-of two-types+link :link-types :rechnung-of)]
    (testing "the associated link (which would block) is identified"
      (is (= [lt-rid]
             (mapv :entity (retirement/associated-link-types two-types+link :object-types :rechnung
                                                             (ix two-types+link))))))
    (testing "cascade removes it, so deleting the type no longer blocks"
      (let [i    (ix two-types+link)
            casc (-> two-types+link
                     (retirement/remove-owned-link-producers :object-types :rechnung true i)
                     (retirement/cascade-associated-links :object-types :rechnung i))]
        (is (not (contains? (:link-types casc) lt-rid)))
        (is (empty? (retirement/deletion-blockers casc :object-types :rechnung false (ix casc)))
            "post-cascade the type is freely deletable")
        (is (= [:bestellung]
               (mapv (comp :object-type-name val)
                     (dissoc (:object-types casc) (rid-of two-types+link :object-types :rechnung))))
            "the OTHER endpoint's object-type is untouched — only the link cascades")))))

(def ^:private stale-baked-create-llm-model
  {:rules [{:op :set :path [:llm-models "x"]
            :value {:name [:get :arg0 :name]
                    :in   [:get :arg0 :in]
                    :out  [:get :arg0 :out]}}]})

(deftest a-stale-baked-schema-action-never-shadows-the-platform-verb
  (let [fresh  (get actions/platform-actions :create-llm-model)
        schema {:actions {:create-llm-model stale-baked-create-llm-model
                          :assign-employee  {:rules [{:op :noop}]}}}]
    (testing "the platform's create-llm-model forwards the CURRENT field vector"
      (is (some? fresh))
      (is (every? (set (mapcat keys (:rules fresh))) [:currency :deprecated])
          ":currency and :deprecated survive the platform passthrough"))
    (testing "resolve-action returns the PLATFORM spec, not the stale stored one"
      (is (= fresh (actions/resolve-action schema :create-llm-model)))
      (is (not= stale-baked-create-llm-model (actions/resolve-action schema :create-llm-model))))
    (testing "a real authored Action Type still resolves from storage"
      (is (some? (actions/resolve-action schema :assign-employee))))
    (testing "and no per-type CRUD is invented for anything"
      (is (nil? (actions/resolve-action schema :create-rechnung)))
      (is (nil? (actions/resolve-action two-types+link :connect-rechnung-of))))))

(def ^:private statused
  (prep {:object-types {:legacy {:attribute-list [{:attribute-name :x :attribute-type :string}]
                                 :primary-key :x :status :active :plural "Alte"}
                        :endorsed-t {:attribute-list [{:attribute-name :x :attribute-type :string}]
                                     :primary-key :x :status :endorsed :plural "Bestätigte"}
                        :draft {:attribute-list [{:attribute-name :x :attribute-type :string}]
                                :primary-key :x :status :experimental :plural "Entwürfe"}
                        :legacy-nil {:attribute-list [{:attribute-name :x :attribute-type :string}]
                                     :primary-key :x :plural "Alte ohne"}}}))

(deftest active-and-endorsed-types-cannot-be-deleted
  (testing ":active is blocked with a deprecate-first message"
    (let [b (retirement/deletion-status-block statused :object-types :legacy)]
      (is (= :active (:status b)))
      (is (re-find #"experimental|deprecated" (:message b)))))
  (testing ":endorsed is blocked too"
    (is (= :endorsed (:status (retirement/deletion-status-block statused :object-types :endorsed-t)))))
  (testing ":experimental and UNSET (≡ experimental) are freely deletable"
    (is (nil? (retirement/deletion-status-block statused :object-types :draft)))
    (is (nil? (retirement/deletion-status-block statused :object-types :legacy-nil)))
    (is (nil? (retirement/status-of statused :object-types :legacy-nil))
        "an unstamped type stores no status (nil) — no drift, guard reads it as experimental")))

(deftest active-and-endorsed-types-cannot-be-RENAMED-either
  (testing "only an experimental type may change its API name. A rename destroys
            no data, which is exactly why it needs saying — the RID survives a
            rename, so from inside the ontology nothing looks broken while every
            consumer holding the NAME breaks."
    (let [b (retirement/rename-status-block statused :object-types :legacy)]
      (is (= :active (:status b)))
      (is (re-find #"experimental" (:message b))
          "the way out is named")
      (is (re-find #":replacement" (:message b))
          "deprecate, naming the successor"))
    (is (= :endorsed (:status (retirement/rename-status-block statused :object-types :endorsed-t))))
    (is (nil? (retirement/rename-status-block statused :object-types :draft)))
    (is (nil? (retirement/rename-status-block statused :object-types :legacy-nil))
        "UNSET ≡ :experimental, the same default the delete guard applies — a
         type is protected only once somebody promoted it")))

(defn- with-soft-referrers
  []
  (let [rrid    (rid-of two-types+link :object-types :rechnung)
        app-rid (rid/mint-for :applications (random-uuid))
        grp-rid (rid/mint-for :object-type-groups (random-uuid))]
    [(-> two-types+link
         (assoc-in [:applications app-rid]
                   {:application-name :rechnung-app :id app-rid :object-type rrid})
         (assoc-in [:object-type-groups grp-rid]
                   {:object-type-group-name :belege :id grp-rid :members [rrid]}))
     app-rid grp-rid]))

(deftest set-status-patches-in-place-and-drives-the-guard
  (let [ot-rid (rid-of one-type :object-types :scratch)
        dep    {:reason "superseded" :deadline "2027-01-01" :replacement :scratch-v2}
        active (retirement/set-status one-type :object-types :scratch :active nil)
        deprd  (retirement/set-status one-type :object-types :scratch :deprecated dep)]
    (testing "status is set WITHOUT dropping :attributes (unlike a full update)"
      (is (= :active (get-in active [:object-types ot-rid :status])))
      (is (= (get-in one-type [:object-types ot-rid :attribute-list])
             (get-in active [:object-types ot-rid :attribute-list]))
          ":x survives the patch"))
    (testing "the deprecation sidecar is carried when supplied"
      (is (= dep (get-in deprd [:object-types ot-rid :deprecation]))))
    (testing "promoting to :active CLEARS a prior deprecation sidecar"
      (is (nil? (get-in (retirement/set-status deprd :object-types :scratch :active nil)
                        [:object-types ot-rid :deprecation]))))
    (testing "and the new status drives the delete guard"
      (is (some? (retirement/deletion-status-block active :object-types :scratch)))
      (is (nil?  (retirement/deletion-status-block deprd  :object-types :scratch))
          ":deprecated is deletable; :active is not"))))

(deftest soft-referrers-warn-and-unbind-they-never-block
  (let [[schema app-rid grp-rid] (with-soft-referrers)]
    (testing "the link (HARD) still blocks; the app + group (SOFT) do not appear as blockers"
      (is (= #{:link-types :transforms}
             (set (mapv :in (retirement/deletion-blockers schema :object-types :rechnung
                                                          false (ix schema)))))))
    (testing "the app + group surface as WARNINGS instead"
      (is (= #{:applications :object-type-groups}
             (set (mapv :in (retirement/deletion-warnings schema :object-types :rechnung
                                                          (ix schema)))))))
    (testing "unbind-soft-referrers clears the app binding and drops the group member"
      (let [u (retirement/unbind-soft-referrers schema :object-types :rechnung)]
        (is (nil? (get-in u [:applications app-rid :object-type])))
        (is (= [] (get-in u [:object-type-groups grp-rid :members])))
        (is (empty? (retirement/deletion-warnings u :object-types :rechnung (ix u)))
            "after unbinding, deleting rechnung introduces no soft dangling ref")))))

(def ^:private type+backing+output
  (prep {:object-types {:gadget {:attribute-list [{:attribute-name :pk :attribute-type :string}]
                                 :primary-key :pk :plural "Gadgets"}}
         :datasets {:seed_g {:kind :standalone
                             :columns [{:attribute-name :pk :attribute-type :string}]}}
         :transforms {:mk-gadget {:language :pg-sql :query "SELECT pk FROM s"
                                  :inputs {:s {:kind :dataset :dataset :seed_g}}
                                  :primary-key :pk
                                  :output {:kind :object-type :object-type :gadget}}}}))

(defn- ds-kinds [schema]
  (into {} (map (fn [[_ d]] [(:dataset-name d) (:kind d)])) (:datasets schema)))

(deftest deleting-a-type-removes-its-object-backing-storage
  (testing "the fixture really carries both kinds of dataset"
    (is (= :object-backing (get (ds-kinds type+backing+output) :gadget)))
    (is (= :transform-output (get (ds-kinds type+backing+output) :mk-gadget))))
  (let [after (retirement/remove-own-object-backing type+backing+output :object-types :gadget
                                                    (ix type+backing+output))
        kinds (ds-kinds after)]
    (is (nil? (get kinds :gadget))
        "the type's own storage went with the delete")
    (is (= :transform-output (get kinds :mk-gadget))
        "the PRODUCER's output dataset stayed")
    (is (= :standalone (get kinds :seed_g)) "and an unrelated dataset is untouched")))

(deftest removing-the-backing-is-a-no-op-where-there-is-none
  (testing "a link-type delete has no object storage of its own"
    (is (= two-types+link
           (retirement/remove-own-object-backing two-types+link :link-types :rechnung-of
                                                 (ix two-types+link)))))
  (testing "and an unknown type leaves the schema exactly as it was"
    (is (= one-type (retirement/remove-own-object-backing one-type :object-types :nope
                                                          (ix one-type))))))
