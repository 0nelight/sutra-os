(ns platform-sdk.wire
  (:require [clojure.string :as str]
            [platform-sdk.client :as client]
            [platform-sdk.errors :as err]
            [sutra.types.paths :as paths]
            [sutra.types.payloads :as pl]
            #?(:clj [sutra.wire.transport :as tx])))

(defn query
  [c spec] (client/request c :post :ontology-query nil spec))

(defn object-type-spec [c type-kw] (client/request c :get :object-type-by {:name type-kw}))

(defn object-sets [c] (client/request c :get :object-sets))

(defn create-object-set! [c body] (client/request c :post :object-sets-temp nil body))

(defn delete-object-set! [c rid] (client/request c :delete :object-set-temp-by {:rid rid}))

(defn- check-args!
  [kind nm args {:keys [required params]}]
  (when-not (map? args)
    (err/raise! :usage (str (name kind) " " (pr-str nm) " takes a map of arguments, got "
                            (pr-str (type args)))
                {kind nm :got args}))
  (when-let [missing (seq (remove (partial contains? args) required))]
    (err/raise! :usage (str (name kind) " " (pr-str nm) " is missing required "
                            (str/join ", " (map pr-str missing)))
                {kind nm :missing (vec missing) :declared (vec params)}))
  (when-let [unknown (and (seq params) (seq (remove (set params) (keys args))))]
    (err/raise! :usage (str (name kind) " " (pr-str nm) " has no parameter "
                            (str/join ", " (map pr-str unknown))
                            " — declared: " (str/join ", " (map pr-str params)))
                {kind nm :unknown (vec unknown) :declared (vec params)}))
  args)

(defn- action-query
  [{:keys [publish? stage? dry-run?]}]
  (cond dry-run? {:dry-run true}
        publish? {:publish true}
        stage?   {:stage true}))

(defn action!
  ([c action args] (action! c action args nil nil))
  ([c action args opts] (action! c action args opts nil))
  ([c action args opts sig]
   (client/check! c (str "action " action))
   (check-args! :action action args sig)
   (client/request c :post :actions (action-query opts) (pl/action-call action args))))

(defn function!
  ([c fn-name inputs] (function! c fn-name inputs nil))
  ([c fn-name inputs sig]
   (client/check! c (str "function " fn-name))
   (check-args! :function fn-name inputs sig)
   (client/request c :post :function-run nil {:function-name fn-name :inputs inputs})))

(defn objects-at [c commit type-kw]
  (client/request c :get :at-objects {:commit commit :type type-kw}))

(defn object-type-at [c commit type-kw]
  (client/request c :get :at-object-type {:commit commit :type type-kw}))

(defn around
  [c ids types]
  (client/request c :post :instances-around nil {:ids (vec ids) :types (vec types)}))

#?(:clj
   (defn blob
     [c content-hash]
     (let [{:keys [status bytes content-type]}
           (tx/get-bytes-from! (client/target c) (paths/address :blob {:hash content-hash}))]
       (if (<= 200 (or status 0) 299)
         {:bytes bytes :content-type content-type}
         (err/raise! (err/status->kind status) (str "blob " content-hash ": HTTP " status)
                     {:status status :hash content-hash})))))

#?(:clj
   (defn upload-blob!
     [c ^bytes data]
     (err/check! (tx/post-bytes-to! (client/target c) (paths/address :blob-upload) data)
                 {:endpoint :blob-upload})))
