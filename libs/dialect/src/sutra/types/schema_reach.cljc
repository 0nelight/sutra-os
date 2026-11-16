(ns sutra.types.schema-reach)

(def body-endpoint
  :schema-by-rid)

(def common-bodies
  #{:releases :release-notes})

(def bodies
  {:applications #{:layout}
   :templates    #{:source}
   :transforms   #{:code :query :source}})

(def elsewhere-key
  :sutra/elsewhere)

(def body-version-key
  :sutra/body-version)

(defn bodies-of
  [section]
  (into common-bodies (get bodies section)))

(defn- carries?
  [spec field]
  (let [v (get spec field)]
    (cond
      (nil? v)    false
      (string? v) (not= "" v)
      (coll? v)   (boolean (seq v))
      :else       true)))

(defn- catalog-entry
  [bodies spec]
  (if-not (map? spec)
    spec
    (let [held (into #{} (filter (partial carries? spec)) bodies)]
      (cond-> (apply dissoc spec bodies)
        (seq held) (assoc elsewhere-key held
                          body-version-key (hash (select-keys spec held)))))))

(defn catalog
  [schema]
  (reduce-kv (fn [acc section entries]
               (assoc acc section
                      (if (map? entries)
                        (update-vals entries #(catalog-entry (bodies-of section) %))
                        entries)))
             {} schema))

(defn with-bodies
  [section spec fetched]
  (-> (merge spec (select-keys fetched (bodies-of section)))
      (dissoc elsewhere-key body-version-key)
      (vary-meta assoc ::held-version (get spec body-version-key))))

(defn- kept-entry
  [section old spec]
  (let [version (get spec body-version-key)]
    (if (and (some? version) (= version (::held-version (meta old))))
      (with-bodies section spec old)
      spec)))

(defn keep-bodies
  [old new]
  (reduce-kv (fn [acc section entries]
               (assoc acc section
                      (if (map? entries)
                        (reduce-kv (fn [m k spec]
                                     (assoc m k (kept-entry section (get-in old [section k])
                                                            spec)))
                                   {} entries)
                        entries)))
             {} new))

(defn elsewhere?
  [spec field]
  (contains? (get spec elsewhere-key) field))
