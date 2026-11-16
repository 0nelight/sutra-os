(ns sutra.governance.merge-plan
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.plan :as cp]
            [sutra.parser.checkout :as checkout]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.signature :as signature]))

(set! *warn-on-reflection* true)

(defn transform-outputs
  [w spec]
  (into {} (for [out  (outputs/outputs-of spec)
                 :when (= :dataset (:kind out))
                 :let  [dsr (:dataset out)]]
             [dsr (get-in w [:datasets dsr])])))

(defn transform-key
  [t]
  (if (keyword? t) (name t) (str t)))

(defn owed-resets
  [w kept base]
  (vec (for [[t spec] (:transforms w)
             :let [v    (get spec :semantic-version 1)
                   k    (get kept (transform-key t))
                   was  (or k (when base (get-in base [:transforms t :semantic-version] 1)))]
             :when (and (integer? v) (not= k v))]
         [t v (when (and (integer? was) (> v was)) (transform-outputs w spec))])))

(defn link-backing-sig
  [schema]
  (into {}
        (for [[tk t] (:transforms schema)
              out    (outputs/outputs-of t)
              :when  (= :link-type (:kind out))]
          [(:link-type out)
           {:producer tk :strategy (:strategy t) :language (:language t)
            :output (dissoc out :dataset)}])))

(defn link-key
  [l]
  (if (keyword? l) (name l) (str l)))

(defn link-backing-fingerprints
  [w]
  (let [sig (link-backing-sig w)]
    (into {} (for [l (keys (:link-types w))] [l (checkout/fingerprint (get sig l))]))))

(def ^:private unproduced
  (checkout/fingerprint nil))

(defn owed-reregisters
  [w kept base]
  (let [was-in (when base (link-backing-fingerprints base))]
    (vec (for [[l fp] (link-backing-fingerprints w)
               :let [k   (get kept (link-key l))
                     was (or k (get was-in l))]
               :when (not= k fp)]
           [l fp (cond (or (nil? was) (= was fp)) nil
                       (= was unproduced)         :adopt
                       :else                      :retract)]))))

(defn after-commit
  [{:keys [head at kept-links kept-versions]} base]
  (-> []
      (into (for [[l fp owed] (owed-reregisters head kept-links base)]
              (if owed [:reregister l fp owed] [:keep-link l fp])))
      (into (for [[t v outs] (owed-resets head kept-versions base)]
              (if outs [:reset t v outs] [:keep-version t v])))
      (cond-> at (conj [:record-llm-rates]))))

(defn refuse-unclean!
  [{:keys [status]} parent staging]
  (when-not (= :clean status)
    (refusal/refuse! :refusal.governance/branch-behind-parent
                     {:status status :branch staging} {:parent parent})))

(defn refuse-invalid!
  [{:keys [error schema findings]}]
  (when error
    (cp/check! (cp/of-findings (or (seq findings)
                                   [(cp/problem :governance/merged-schema-invalid
                                                (if (string? error) error (pr-str error)))]))
               {:code :refusal.governance/merged-schema-invalid
                :data {:reasons error :schema schema}})))

(defn plan
  [{:keys [parent old full staged-rows applied retired actor]}]
  (let [{:keys [ops log records]} applied]
    {:b    parent
     :old  old
     :full full
     :tx   (signature/installing
            {:ops     ops
             :log     log
             :records (into records (when (seq staged-rows) retired))
             :message "commit staged"
             :actor   {:principal (some-> actor str)}}
            full)}))
