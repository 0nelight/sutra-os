(ns sutra.cli.verbs.constructs
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [live-schema]]
            [sutra.cli.out :refer [emit! mark paint prompt-yes? render say! show-lines warn!]]
            [sutra.i18n.core :as i18n]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.types.languages :as langs]
            [sutra.types.shape :as tshape]
            [sutra.wire.api :as api]
            [sutra.cli.view :refer
             [kw-name rid->name-index]]))

(defn- sugar-raise-name-sugar   [& a]
  (apply @(requiring-resolve 'sutra.parser.sugar/raise-name-sugar) a))
(defn- pe-emit-transform        [& a]
  (apply @(requiring-resolve 'sutra.parser.project-emit/emit-transform) a))
(defn- pe-names-resolved        [& a]
  (apply @(requiring-resolve 'sutra.parser.project-emit/names-resolved) a))

(defn- spec-by-name
  [body section name-key nm]
  (some (fn [[rid s]] (when (= nm (some-> (get s name-key) clojure.core/name)) [rid s]))
        (get body section)))

(defn- named-refs
  [body section rid spec]
  (let [idx (update-vals (rid->name-index body) keyword)]
    (get (pe-names-resolved idx section {rid spec}) rid)))

(defn- object-type-author
  [spec]
  (let [attrs (mapv (fn [{:keys [attribute-name attribute-type opts]}]
                      (cond-> [attribute-name attribute-type] (seq opts) (conj opts)))
                    (:attribute-list spec))]
    (sugar-raise-name-sugar
     :object-types
     (tshape/->author :object-types spec
                      (cond-> {:attributes attrs}
                        (seq (:implements spec)) (assoc :implements (vec (:implements spec))))))))

(defn- editable-entry
  [body kind nm]
  (case kind
    :transform
    (when-let [[rid spec] (spec-by-name body :transforms :transform-name nm)]
      (case (langs/body-field (:language spec))
        :code
        (let [{[_ terse] :entry} (pe-emit-transform (named-refs body :transforms rid spec)
                                                    {:inline? true})
              fname (some-> (:function-name spec) kw-name)]
          {:text   (or (:code spec) "") :ext (langs/file-ext (:language spec))
           :writer (fn [t] (let [entry (assoc terse :code t)]
                             (if fname
                               {:functions  {(keyword fname) entry}}
                               {:transforms {(keyword nm) entry}})))})
        :query {:text (or (:query spec) "") :ext (langs/file-ext (:language spec)) :writer nil}
        (when-let [q (get-in spec [:source :query])]
          (let [idx (rid->name-index body)
                rn  (fn [r] (when r (or (get idx (kw-name r)) (kw-name r))))
                ds  (rn (get-in spec [:output :dataset]))
                ot  (rn (get-in spec [:output :object-type]))
                es  (cond-> {:strategy :simple
                             :datasource (get-in spec [:input :datasource])}
                      (:primary-key spec) (assoc :primary-key (:primary-key spec)))]
            {:text q :ext "sql"
             :writer (cond
                       ds (fn [t] {:datasets {(keyword ds) {:external-source (assoc es :query t)}}})
                       ot (fn [t] {:objects  {(keyword ot) {:external-source (assoc es :query t)}}})
                       :else nil)}))))
    :application
    (when-let [[_ spec] (spec-by-name body :applications :application-name nm)]
      (when (contains? spec :source)
        {:text (or (:source spec) "") :ext "cljs" :writer nil}))
    :object-type
    (when-let [[rid spec] (spec-by-name body :object-types :object-type-name nm)]
      {:text
       (with-out-str
         (pprint/pprint (object-type-author (named-refs body :object-types rid spec))))
       :ext    "edn"
       :writer (when-not (:external-source spec)
                 (fn [t] {:objects {(keyword nm) (edn/read-string t)}}))})
    :function
    (if-let [[rid spec] (spec-by-name body :functions :function-name nm)]
      {:text
       (with-out-str
         (pprint/pprint (tshape/->author :functions (named-refs body :functions rid spec) {})))
       :ext    "edn"
       :writer (fn [t] {:functions {(keyword nm) (edn/read-string t)}})}
      (when-let [[_ spec] (spec-by-name body :transforms :function-name nm)]
        (editable-entry body :transform (clojure.core/name (:transform-name spec)))))
    nil))

(defn- run-editor!
  [text ext]
  (let [editor (or (not-empty (System/getenv "EDITOR"))
                   (not-empty (System/getenv "VISUAL")) "vi")
        tmp    (java.io.File/createTempFile "sutra-edit-" (str "." ext))]
    (try
      (spit tmp text)
      (let [cmd  (conj (vec (str/split editor #"\s+")) (.getPath tmp))
            proc (.start (.inheritIO (ProcessBuilder. ^java.util.List cmd)))]
        (when (zero? (.waitFor proc)) (slurp tmp)))
      (catch Exception e
        (warn! (mark :yellow "⏸"
                     (i18n/t :cli/editor-launch-failed {:editor editor :grund (.getMessage e)})))
        nil)
      (finally (.delete tmp)))))

(defmethod step! :edit [ctx [_ {:keys [kind name print? from]}]]
  (let [nm  (clojure.core/name name)
        tgt (editable-entry (live-schema) kind nm)]
    (cond
      (nil? tgt)
      (say! ctx (mark :yellow "⏸"
                      (str "can't edit " (clojure.core/name kind) " " (paint :bold nm)
                           " — not found, or it has no editable body "
                           "(transforms, object-types, functions, app views)")))
      print?
      (emit! ctx {:kind (clojure.core/name kind) :name nm :text (:text tgt)} ::text)
      (nil? (:writer tgt))
      (say! ctx (mark :yellow "⏸"
                      (i18n/t :cli/in-place-edit-unsupported)))
      :else
      (let [new-text (if from (slurp from) (run-editor! (:text tgt) (:ext tgt)))]
        (cond
          (nil? new-text) nil
          (= (str/trimr new-text) (str/trimr (:text tgt)))
          (say! ctx (mark :cyan "=" (i18n/t :cli/unchanged-nothing-staged)))
          :else
          (do
            (step! ctx [:install {:schema ((:writer tgt) new-text)}])
            (let [{:keys [staged]} (req! "staged" (api/get! :staging))]
              (say! ctx (mark :green "✓"
                              (i18n/t :cli/staged-edit
                                      {:art (clojure.core/name kind) :name (paint :bold nm)
                                       :n (count staged)}))))
            (if (prompt-yes? ctx
                             (i18n/t :cli/commit-this-edit
                                     {:art (clojure.core/name kind) :name nm}))
              (step! ctx [:commit {}])
              (say! ctx (mark :yellow "⏸" (i18n/t :cli/left-staged)))))))))
  nil)

(defmethod render ::text [_ {:keys [text]}]
  (str/replace (str text) #"\n\z" ""))

(defmethod step! :trash [ctx [_ _]]
  (let [{:keys [trash] :as r} (req! "trash" (api/get! :trash))]
    (emit! ctx (merge {:trash (vec trash)} (select-keys r [:complete? :reaches-back-to])) ::trash))
  nil)

(defmethod render ::trash [_ {:keys [trash complete? reaches-back-to]}]
  [(show-lines {:trash trash})
   (when (false? complete?)
     (mark :yellow "ℹ" (i18n/t :cli/trash-reaches-back {:rev reaches-back-to})))])

(defmethod step! :restore [ctx [_ {:keys [kind name]}]]
  (let [k    (clojure.core/name kind)
        nm   (clojure.core/name name)
        resp (req! (str "restore " k " " nm) (api/post! :trash-restore {:kind k :name nm}))
        c    (get-in resp [:restored :from-commit])]
    (emit! ctx {:kind k :name nm :from-commit c} ::restored))
  nil)

(defmethod render ::restored [_ {:keys [kind name from-commit]}]
  (let [c (str from-commit)]
    (mark :green "✓"
          (str (i18n/t :cli/restored {:art kind :name (paint :bold name)})
               (when from-commit
                 (paint :dim (str " (from " (subs c 0 (min 8 (count c))) ")")))))))

(defmethod step! :delete-construct [ctx [_ {:keys [kind name cascade?]}]]
  (let [k     (keyword kind)
        nm    (clojure.core/name name)
        casc? (boolean cascade?)
        f     (case k
                :object-type api/delete-object-type
                :link-type   api/delete-conn-type
                :transform   api/delete-transform
                :function    api/delete-function
                :dataset     api/delete-dataset
                :action-type api/delete-action-type
                (throw (ex-info (str "delete: unsupported kind " (pr-str k)
                                     " — one of object-type / link-type / transform / function"
                                     " / dataset / action-type")
                                {:kind k})))
        resp  (req! (str "delete " (clojure.core/name k) " " nm)
                    (if (#{:object-type :link-type :dataset} k) (f nm casc?) (f nm)))]
    (emit! ctx (merge {:deleted (str (clojure.core/name k) " " nm)} resp))
    nil))

(defmethod step! :set-status [ctx [_ {:keys [kind name status deprecation]}]]
  (let [k    (clojure.core/name (keyword kind))
        nm   (clojure.core/name name)
        body (cond-> {:status (keyword status)}
               (seq deprecation) (assoc :deprecation deprecation))]
    (req! (str "set " k " " nm " status " (clojure.core/name status))
          (api/post! :set-status {:kind k :name nm} body))
    (emit! ctx (assoc body :kind k :name nm))
    nil))

(defmethod step! :rename [ctx [_ {:keys [from to kind]}]]
  (let [k (or kind :project)]
    (req! "rename" (api/rename k from to))
    (emit! ctx {:renamed k :from from :to to})
    nil))
