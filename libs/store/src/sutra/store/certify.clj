(ns sutra.store.certify
  (:require [sutra.certify :as c :refer [pick rnd]]
            [sutra.store.api :as api]
            [sutra.store.conformance :as store-conf]
            [sutra.store.core.engine :as core-engine]
            [sutra.store.guard :as guard]
            [sutra.store.model :as model]
            [sutra.store.model-engine :as model-engine]
            [sutra.store.spi :as spi]))

(set! *warn-on-reflection* true)

(def ^:private T1 "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000ce171")
(def ^:private T2 "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000ce172")
(def ^:private L1 "ri.sutra.main.link-type.00000000-0000-0000-0000-0000000ce1a1")
(def ^:private kinds {:objects [T1 T2] :links [L1]})

(def sig
  {:types      {T1 {:attrs [{:name :n :type :integer} {:name :s :type :string}]}
                T2 {:attrs [{:name :n :type :integer} {:name :s :type :string}]}}
   :interfaces {}
   :links      {L1 {:from T1 :to T2 :cardinality :many-to-many}}})

(def ^:private empty-sig {:types {} :interfaces {} :links {}})

(def ^:private kv-space :sutra.certify/kv)

(defn- id-of [n] (format "00000000-0000-0000-0000-%012d" n))

(defn- text [r]
  (pick r ["a" "b" "ab" "Müller" "z" "😀" "" "A" "" "a b"]))

(defn- values [r]
  (cond-> {:n (- (rnd r 2000) 1000)}
    (zero? (rnd r 3)) (assoc :s nil)
    (pos? (rnd r 3))  (assoc :s (text r))))

(defn- gen-op [r]
  (case (long (rnd r 6))
    (0 1) [:put :objects (pick r (:objects kinds)) (id-of (rnd r 12)) (values r)]
    2     [:del :objects (pick r (:objects kinds)) (id-of (rnd r 12))]
    3     [:put :links L1 (id-of (+ 100 (rnd r 8))) (id-of (rnd r 12)) (id-of (rnd r 12))]
    4     [:del :links L1 (id-of (+ 100 (rnd r 8)))]
    5     [:kv (str "k" (text r) (rnd r 5)) (when (pos? (rnd r 4)) (rnd r 100))]))

(defn- gen-command [r {:keys [namespaces]}]
  (let [nss (vec (sort namespaces)) forks (remove #{"main"} nss)]
    (case (long (rnd r 10))
      (0 1 2 3 4) [:tx (pick r nss) (vec (repeatedly (inc (rnd r 4)) #(gen-op r))) (pos? (rnd r 5))]
      5 [:fork (pick r nss) (str "b" (rnd r 4))]
      6 (if (seq forks) [:drop (pick r forks)] [:tx "main" [(gen-op r)] true])
      7 (if (seq forks)
          [:detach (pick r forks) (pick r [[:objects T1] [:objects T2] [:links L1]])]
          [:tx "main" [(gen-op r)] true])
      8 [:clear (pick r nss) (pick r (:objects kinds)) false]
      9 [:tx (pick r nss) [(gen-op r) (gen-op r)] false])))

(defn- admit [{:keys [namespaces held]} [k ns x :as cmd]]
  (when (case k
          :fork               (and (namespaces ns) (not (namespaces x)))
          :detach             (and (namespaces ns) (not (held [ns (second x)])))
          (:drop :clear :tx)  (contains? namespaces ns)
          true)
    (cond-> cmd (and (= :clear k) (not= "main" ns) (not (held [ns x]))) (assoc 3 true))))

(defn- committed? [[k _ _ commit?]] (or (not= :tx k) commit?))

(defn- advance [state [k ns x flag :as cmd] [[rev] [mrev]]]
  (cond-> (-> state
              (update :held (fn [s]
                              (case k
                                :detach (conj s [ns (second x)])
                                :clear  (cond-> s flag (conj [ns x]))
                                :fork   (into s (for [[n K] s :when (and (= n ns) (not= "main" ns))]
                                                  [x K]))
                                :drop   (into #{} (remove #(= ns (first %))) s)
                                s)))
              (update :namespaces #(case k :fork (conj % x) :drop (disj % ns) %))
              (update :history #(case k
                                  :tx   (cond-> % flag (update ns (fnil conj [])
                                                               {:driver rev :reference mrev}))
                                  :fork (assoc % x [])
                                  :drop (dissoc % ns)
                                  %)))
    (committed? cmd) (update :head #(merge-with max % {:driver rev :reference mrev}))))

(defn- same-reads? [a b]
  (and (= (keys (:rows a)) (keys (:rows b)))
       (every? #(model/same-rows? (get-in a [:rows %]) (get-in b [:rows %])) (keys (:rows a)))
       (model/same-rows? (:some a) (:some b))
       (every? #(model/same-rows? (get-in a [:links %]) (get-in b [:links %])) (keys (:links a)))
       (= (:kv a) (:kv b))))

(defn- judge [_ _ a b]
  (let [differ (fn [duty n as-of a b]
                 {:duty duty :msg "the engine answered otherwise than the model"
                  :data {:ns n :as-of as-of :engine a :model b}})]
    (or (some (fn [[n x]] (when (not= x (get-in b [:exists n]))
                            (differ :duty/ns n nil x (get-in b [:exists n]))))
              (:exists a))
        (some (fn [[n x]] (let [y (get-in b [:reads n])]
                            (when-not (same-reads? x y) (differ :duty/rows n nil x y))))
              (:reads a))
        (some (fn [[n xs]] (some (fn [[[rv x] [_ y]]]
                                   (when-not (same-reads? x y) (differ :duty/history n rv x y)))
                                 (map vector xs (get-in b [:history n]))))
              (:history a)))))

(defn- write-cx [ns rev]
  {:ns ns :sig sig :rev rev :time (java.util.Date.) :actor {:principal "certify"}})

(defn- in-tx! [e ns commit? f]
  (let [h (spi/-begin e :write nil)]
    (try (spi/-lock! e h [[:certify ns]])
         (let [rev (spi/-next-rev e h) x (f h rev)]
           (if commit? (spi/-commit! e h) (spi/-rollback! e h))
           [rev x])
         (catch Throwable x (spi/-rollback! e h) (throw x)))))

(defn- apply-op! [e h cx [k sec K id a b]]
  (case k
    :put (if (= :objects sec)
           (spi/-write-rows! e h cx K [{:id id :values a}])
           (spi/-write-links! e h cx K [{:id id :from a :to b}]))
    :del (if (= :objects sec)
           (spi/-delete-rows! e h cx K [id])
           (spi/-delete-links! e h cx K [id]))
    :kv  (spi/-kv-write! e h {[kv-space "p" sec] K})))

(defn- layout! [e h ns rev change]
  (spi/-layout! e h (write-cx ns rev) (merge {:old sig :new sig :steps []} change)))

(defn- run-command! [e [k ns x flag]]
  (case k
    :tx     (in-tx! e ns flag (fn [h rev] (doseq [op x] (apply-op! e h (write-cx ns rev) op))))
    :fork   (in-tx! e x true (fn [h rev] (spi/-ns-fork! e h ns x {:sig sig :rev rev})
                               (layout! e h x rev {})))
    :drop   (in-tx! e ns true (fn [h _] (spi/-ns-drop! e h ns)))
    :detach (in-tx! e ns true (fn [h rev]
                                (layout! e h ns rev {:steps [[(if (= :links (first x))
                                                                :detach-link-type
                                                                :detach-type)
                                                              (second x)]]})))
    :clear  (in-tx! e ns true (fn [h rev]
                                (layout! e h ns rev {:steps (cond->> [[:clear-attribute x :s]]
                                                              flag (cons [:detach-type x])
                                                              :always vec)})))
    :layout (in-tx! e ns true (fn [h rev]
                                (layout! e h ns rev {:old empty-sig :steps [[:relayout]]})))))

(defn- reads [e ns basis as-of]
  (let [h  (spi/-begin e :read nil)
        cx (cond-> {:ns ns :sig sig :basis (or as-of basis)} as-of (assoc :as-of as-of))]
    (try
      {:rows  (into {} (for [T (:objects kinds)] [T (spi/-rows e h cx T nil)]))
       :some  (spi/-rows e h cx T1 [(id-of 1) (id-of 3) (id-of 99)])
       :links (into {} (for [w [nil {:from [(id-of 1) (id-of 2)]} {:to [(id-of 3)]}
                                {:ends [(id-of 4)]} {:ids [(id-of 101) (id-of 102)]}]]
                         [w (spi/-links e h cx L1 w)]))
       :kv    (when-not as-of
                {:all   (spi/-kv-scan e h kv-space "p" {})
                 :page  (spi/-kv-scan e h kv-space "p" {:after "ka" :limit 3})
                 :rev   (spi/-kv-scan e h kv-space "p" {:upto "kz" :reverse? true :limit 4})
                 :keys  (spi/-kv-scan e h kv-space "p" {:keys-only? true})
                 :parts (spi/-kv-parts e h kv-space)})}
      (finally (spi/-rollback! e h)))))

(defn- exists [e ns]
  (let [h (spi/-begin e :read nil)]
    (try (boolean (spi/-ns-exists? e h ns)) (finally (spi/-rollback! e h)))))

(defn- observe [side e {:keys [namespaces history head]} _ _]
  (let [basis {:rev (side head)}]
    {:exists  (into {} (for [n (into namespaces (map #(str "b" %) (range 4)))] [n (exists e n)]))
     :reads   (into {} (for [n namespaces] [n (reads e n basis nil)]))
     :history (into {} (for [n namespaces]
                         [n (vec (for [revs (take-last 3 (get history n))
                                       :let [rv (side revs)]]
                                   [rv (reads e n basis {:rev rv :root-rev rv})]))]))}))

(defn model-check
  [{:keys [make steps seed] :or {steps 120 seed 1}}]
  (let [x {:driver (make) :reference (model-engine/engine)}
        heads (update-vals x #(first (run-command! % [:layout "main"])))]
    (c/program {:duty :duty/rows :shape :driver/store-command :steps steps :seed seed
                :init {:namespaces #{"main"} :held #{} :history {"main" []} :head heads}
                :gen gen-command :admit admit :advance advance
                :run (fn [_ e _ cmd] (run-command! e cmd))
                :observe observe :judge judge}
               x)))

(defn- threads [n f]
  (let [fs (doall (for [i (range n)] (future (f i))))] (mapv deref fs)))

(defn- kv-get [e k]
  (let [h (spi/-begin e :read nil)]
    (try (get (spi/-kv-read e h [[kv-space "c" k]]) [kv-space "c" k])
         (finally (spi/-rollback! e h)))))

(defn- head! [e] {:rev (first (in-tx! e "main" true (fn [_ _])))})

(defn- clock [e _]
  (let [revs (threads 4 (fn [_] (vec (for [_ (range 10)]
                                       (first (in-tx! e "main" false (fn [_ _])))))))]
    (cond (not= (count (distinct (apply concat revs))) 40)
          (c/fail :duty/clock "two handles got the same revision" {:revs revs})
          (not (every? #(apply < %) revs))
          (c/fail :duty/clock "revisions of one thread did not rise" {:revs revs}))))

(defn- lock [e _]
  (threads 4 (fn [_] (dotimes [_ 10]
                       (in-tx! e "main" true
                               (fn [h _]
                                 (spi/-lock! e h [[:certify "counter"]])
                                 (let [k [kv-space "c" "n"] v (get (spi/-kv-read e h [k]) k 0)]
                                   (spi/-kv-write! e h {k (inc v)})))))))
  (when-not (= 40 (kv-get e "n"))
    (c/fail :duty/lock "an increment under a lock was lost" {:counter (kv-get e "n")})))

(defn- read-stable [e _]
  (let [k  [kv-space "c" "stable"]
        _  (in-tx! e "main" true (fn [h _] (spi/-kv-write! e h {k 1})))
        h  (spi/-begin e :read nil)
        v1 (get (spi/-kv-read e h [k]) k)
        _  @(future (in-tx! e "main" true (fn [h2 _] (spi/-kv-write! e h2 {k 2}))))
        v2 (try (get (spi/-kv-read e h [k]) k) (finally (spi/-rollback! e h)))]
    (cond (not= v1 v2) (c/fail :duty/read-stable "a read handle saw a later commit"
                               {:before v1 :after v2})
          (not= 2 (kv-get e "stable"))
          (c/fail :duty/read-stable "a new handle did not see the commit" {}))))

(defn- atomic [e _]
  (let [k [kv-space "c" "rolled"]]
    (in-tx! e "main" false (fn [h rev]
                             (spi/-kv-write! e h {k 1})
                             (spi/-write-rows! e h (write-cx "main" rev) T1
                                               [{:id (id-of 77) :values {:n 1}}])))
    (let [r (reads e "main" (head! e) nil)]
      (when (or (some? (kv-get e "rolled")) (seq (:some r))
                (seq (filter #(= (id-of 77) (:id %)) (get-in r [:rows T1]))))
        (c/fail :duty/atomic "a rolled back handle left a write" {})))))

(defn- no-ambient [e make]
  (let [other (make)]
    (run-command! other [:layout "main"])
    (in-tx! e "main" true (fn [h _] (spi/-kv-write! e h {[kv-space "c" "mine"] 1})))
    (when (some? (kv-get other "mine"))
      (c/fail :duty/no-ambient "two engines shared state" {}))))

(defn- concurrency-check [{:keys [make]}]
  (let [e (make)]
    (run-command! e [:layout "main"])
    (or (some #(% e make) [clock lock read-stable atomic no-ambient]) c/proven)))

(defn- records-run [colls]
  (let [cx {:ns "" :sig sig :rev 0 :time (java.util.Date.) :actor {:principal "certify"}}]
    (fn [_ e _ [k role x commit?]]
      (let [h (spi/-begin e :write nil) coll (colls role)]
        (try (let [v (case k
                       :put  (spi/-rec-put! e h (assoc cx :rev (spi/-next-rev e h)) coll x)
                       :get  (spi/-rec-get e h cx coll x)
                       :scan (sort-by first (spi/-rec-scan e h cx coll)))]
               (if (and (= :put k) (not commit?)) (spi/-rollback! e h) (spi/-commit! e h))
               v)
             (catch Throwable t (spi/-rollback! e h) (throw t)))))))

(def ^:private keyed-script
  [[:put :keyed [["a" {:k "a" :v 1}] ["b" {:k "b" :v 2}] ["a" nil]] true]
   [:get :keyed "b"]
   [:scan :keyed]
   [:put :keyed [["c" {:k "c" :v 3}] ["b" nil]] false]
   [:scan :keyed]
   [:put :keyed [["b" {:k "b" :v 4}] ["c" {:k "c" :v 5}]] true]
   [:get :keyed "a"]
   [:scan :keyed]])

(def ^:private auto-script
  [[:put :auto [[api/auto {:v 7}]] true]
   [:put :auto [[api/auto {:v 8}] [api/auto {:v 9}]] false]
   [:put :auto [[api/auto {:v 10}] [api/auto {:v 11}]] true]])

(defn- auto-keys [state side [k role] answer]
  (when (and (= :put k) (= :auto role))
    (reductions (fn [[_ prior] key] [prior key]) [nil (get-in state [:auto side])] answer)))

(defn- records-judge [state [k role :as cmd] a b]
  (if (and (= :put k) (= :auto role))
    (some (fn [[side answer]]
            (some (fn [[prior key]]
                    (when-not (and (int? key) (or (nil? prior) (> key prior)))
                      {:duty :duty/record-auto
                       :msg  "an assigned key not above every one assigned before"
                       :data {:side side :key key :before prior}}))
                  (rest (auto-keys state side cmd answer))))
          (map vector c/sides [a b]))
    (when (not= a b)
      {:duty :duty/records :msg "records answered otherwise than the model"
       :data {:engine a :model b}})))

(defn- records-advance [state [k role] answers]
  (cond-> state
    (and (= :put k) (= :auto role))
    (assoc :auto (zipmap c/sides (map #(reduce max (or %2 Long/MIN_VALUE) %1) answers
                                      (map #(get-in state [:auto %]) c/sides))))))

(defn- records-check [{:keys [make records auto-records]}]
  (if (empty? records)
    {:status :proven :records :none}
    (c/program {:duty :duty/records :shape :driver/records-command
                :script (cond-> keyed-script auto-records (into auto-script))
                :run (records-run {:keyed (first records) :auto auto-records})
                :advance records-advance :judge records-judge}
               {:driver (make)
                :reference (model-engine/engine (cond-> (set records)
                                                  auto-records (conj auto-records)))})))

(defn- store-check [{:keys [make db]}]
  (c/suite :duty/store "the Db over the engine broke the store contract"
           #(binding [core-engine/*verify* true
                      core-engine/*one-handle-per-lane* false
                      guard/*on* true]
              (store-conf/check-store {:engine make :db db}))))

(def proves
  {:certify/model       #{:duty/rows :duty/links :duty/history :duty/fork :duty/ns :duty/layout
                          :duty/kv-read :duty/kv-write :duty/kv-scan :duty/kv-parts
                          :duty/write-own :duty/failures}
   :certify/concurrency #{:duty/clock :duty/lock :duty/read-stable :duty/atomic :duty/no-ambient}
   :certify/records     #{:duty/records :duty/record-auto}
   :certify/store       #{:duty/store :duty/pushdown :duty/layout :duty/shapes :duty/snapshot
                          :duty/begin :duty/profile}})

(def checks
  {:certify/model       model-check
   :certify/concurrency concurrency-check
   :certify/records     records-check
   :certify/store       store-check})

(defn claims [profile results]
  (let [ok? #(c/proven? (results %))]
    (cond-> {}
      (and (:history? profile) (ok? :certify/model)) (assoc :history? true)
      (and (:consistency profile) (ok? :certify/concurrency))
      (assoc :consistency (:consistency profile))
      (and (ok? :certify/store) (:scan profile)) (assoc :scan (:scan profile))
      (and (ok? :certify/store) (:plan profile)) (assoc :plan (:plan profile))
      (and (ok? :certify/store) (:explain? profile)) (assoc :explain? true))))

(defn certify
  [driver]
  (binding [core-engine/*require-certificate* false]
    (let [{:keys [results] :as r} (c/certification checks driver)]
      (assoc r :claims (claims (spi/-profile ((:make driver))) results)))))
